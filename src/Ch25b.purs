module Ch25b where
  
import Prelude

import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Parallel (parSequence)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), decodeJson, (.:))
import Data.Argonaut.Decode.Decoders (decodeJObject)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Foreign.Generic (class Encode, defaultOptions, encodeJSON, genericEncode)
import Type.Proxy (Proxy(..))

test :: Effect Unit
test = launchAff_ do
  result <- bimap Affjax.printError _.body <$> Affjax.post ResponseFormat.json echoURL (Just $ RequestBody.String $ encodeJSON testTeacher)
  log $ processAjaxResult (Proxy :: _ Teacher) result
  pure unit

test' :: Effect Unit
test' = launchAff_ do
  results <- parSequence $ (\json -> Affjax.post ResponseFormat.json echoURL $ Just $ RequestBody.String json)
    <$> [
      encodeJSON testTeacher
    , encodeJSON testStudent
    ]
  log $ case map (_.body) <$> sequence results of
    Left err -> Affjax.printError err
    Right [teacherJson, studentJson] -> show (decodeJson teacherJson :: _ Teacher) <> "\n\n"
      <> show (decodeJson studentJson :: _ Student)
    Right _ -> "The number of Ajax calls is different than what's being processed."
  pure unit

echoURL :: String
echoURL = "http://localhost:3000/"

newtype Centimeters = Centimeters Number
derive instance genericCentimeters :: Generic Centimeters _
derive newtype instance encodeCentimeters :: Encode Centimeters
derive newtype instance decodeCentimeters :: DecodeJson Centimeters

instance Show Centimeters where
  show = genericShow

newtype Kilograms = Kilograms Number
derive instance genericKilograms :: Generic Kilograms _
derive newtype instance encodeKilograms :: Encode Kilograms
derive newtype instance decodeKilograms :: DecodeJson Kilograms

instance Show Kilograms where
  show = genericShow

newtype Years = Years Int
derive instance genericYears :: Generic Years _
derive newtype instance encodeYears :: Encode Years
derive newtype instance decodeYears :: DecodeJson Years

instance Show Years where
  show = genericShow

newtype Personal = Personal
  { height :: Centimeters
  , weight :: Kilograms
  , age :: Years
  }

derive instance genericPersonal :: Generic Personal _
instance Encode Personal where
  encode = genericEncode defaultOptions

instance Show Personal where
  show = genericShow

instance DecodeJson Personal where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    if tag == "Personal" then do
      c <- o .: "stnetnoc"
      height <- c .: "thgieh"
      weight <- c .: "thgiew"
      age <- c .: "ega"
      pure $ Personal { height, weight, age }
    else Left $ AtKey "tag" $ UnexpectedValue json

newtype GPA = GPA Number
derive instance genericGPA :: Generic GPA _
derive newtype instance encodeGPA :: Encode GPA
derive newtype instance decodeGPA :: DecodeJson GPA

instance Show GPA where
  show = genericShow

data Grade = Preschool | Kindergarten | Grade Int | High Int | College Int
derive instance genericGrade :: Generic Grade _

instance Encode Grade where
  encode = genericEncode defaultOptions

instance DecodeJson Grade where
  decodeJson json = do
    o <- decodeJObject json 
    tag <- o .: "gat"
    let contents = o .: "stnetnoc"
    case tag of
      "Preschool" -> pure Preschool
      "Kindergarten" -> pure Kindergarten
      "Grade" -> Grade <$> contents
      "High" -> High <$> contents
      "College" -> College <$> contents
      _ -> Left $ AtKey "tag" $ UnexpectedValue json

instance Show Grade where
  show = genericShow

newtype Student = Student
  { grade :: Grade
  , teacher :: Teacher
  , gpa :: GPA
  , personal :: Personal
  }

derive instance genericStudent :: Generic Student _
instance Encode Student where
  encode = genericEncode defaultOptions

instance Show Student where
  show = genericShow

instance DecodeJson Student where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    if tag == "Student" then do
      c <- o .: "stnetnoc"
      grade <- c .: "edarg"
      teacher <- c .: "rehcaet"
      gpa <- c .: "apg"
      personal <- c .: "lanosrep"
      pure $ Student { grade, teacher, gpa, personal }
    else Left $ AtKey "tag" $ UnexpectedValue json

data TeachingStatus = StudentTeacher | Probationary | NonTenured | Tenured
derive instance genericTeachingStatus :: Generic TeachingStatus _

instance Encode TeachingStatus where
  encode = genericEncode defaultOptions

instance DecodeJson TeachingStatus where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    case tag of
      "StudentTeacher" -> pure StudentTeacher
      "Probationary" -> pure Probationary
      "NonTenured" -> pure NonTenured
      "Tenured" -> pure Tenured
      _ -> Left $ AtKey "tag" $ UnexpectedValue json

instance Show TeachingStatus where
  show = genericShow

newtype Teacher = Teacher
  { grades :: Array Grade
  , numberOfStudents :: Int
  , personal :: Personal
  , status :: TeachingStatus
  }

derive instance genericTeacher :: Generic Teacher _

instance Encode Teacher where
  encode = genericEncode defaultOptions

instance Show Teacher where
  show = genericShow

instance DecodeJson Teacher where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    if tag == "Teacher" then do
      c <- o .: "stnetnoc"
      grades <- c .: "sedarg"
      numberOfStudents <- c .: "stnedutSfOrebmun"
      personal <- c .: "lanosrep"
      status <- c .: "sutats"
      pure $ Teacher { grades, numberOfStudents, personal, status }
    else Left $ AtKey "tag" $ UnexpectedValue json

testTeacher :: Teacher
testTeacher = Teacher
  { grades: [ Preschool, Kindergarten, Grade 1 ]
  , numberOfStudents: 23
  , personal: Personal
    { height: Centimeters 162.56
    , weight: Kilograms 63.5
    , age: Years 31
    }
  , status: NonTenured
  }

processAjaxResult :: ∀ a. DecodeJson a => Show a => Proxy a -> Either String Json -> String
processAjaxResult _ = case _ of
  Left e -> e
  Right res ->
    case decodeJson res :: _ a of
      Left e' -> show e'
      Right res' -> show res'

testStudent :: Student
testStudent = Student
  { grade: Grade 1
  , teacher: testTeacher
  , gpa: GPA 3.2
  , personal: Personal
    { height: Centimeters 107.9
    , weight: Kilograms 17.9
    , age: Years 5
    }
  }