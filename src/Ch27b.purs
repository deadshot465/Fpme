module Ch27b where
  
import Prelude

import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Control.Parallel (parSequence)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Foreign (MultipleErrors)
import Foreign.Generic (class Decode, class Encode, Foreign, decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Foreign.JSON (parseJSON)
import Type.Proxy (Proxy(..))

test :: Effect Unit
test = launchAff_ do
  result <- bimap Affjax.printError _.body <$> Affjax.post ResponseFormat.string echoURL (Just $ RequestBody.String $ encodeJSON testTeacher)
  log $ processAjaxResult (Proxy :: _ Teacher) result
  pure unit

test' :: Effect Unit
test' = launchAff_ do
  results <- parSequence $ (\json -> Affjax.post ResponseFormat.string echoURL $ Just $ RequestBody.String json)
    <$> [
      encodeJSON testTeacher
    , encodeJSON testStudent
    ]
  log $ case map (_.body) <$> sequence results of
    Left err -> Affjax.printError err
    Right [teacher, student] -> show (processJson teacher :: _ Teacher) <> "\n\n"
      <> show (processJson student :: _ Student)
    Right _ -> "The number of Ajax calls is different than what's being processed."
  where
    processJson :: ∀ a. Decode a => String -> Either MultipleErrors a
    processJson json = runExcept do
      o <- parseJSON json
      decodeJSON $ _reverseKeys o

echoURL :: String
echoURL = "http://localhost:3000/"

newtype Centimeters = Centimeters Number
derive instance genericCentimeters :: Generic Centimeters _
derive newtype instance encodeCentimeters :: Encode Centimeters
derive newtype instance decodeCentimeters :: Decode Centimeters

instance Show Centimeters where
  show = genericShow

newtype Kilograms = Kilograms Number
derive instance genericKilograms :: Generic Kilograms _
derive newtype instance encodeKilograms :: Encode Kilograms
derive newtype instance decodeKilograms :: Decode Kilograms

instance Show Kilograms where
  show = genericShow

newtype Years = Years Int
derive instance genericYears :: Generic Years _
derive newtype instance encodeYears :: Encode Years
derive newtype instance decodeYears :: Decode Years

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

instance Decode Personal where
  decode = genericDecode defaultOptions

newtype GPA = GPA Number
derive instance genericGPA :: Generic GPA _
derive newtype instance encodeGPA :: Encode GPA
derive newtype instance decodeGPA :: Decode GPA

instance Show GPA where
  show = genericShow

data Grade = Preschool | Kindergarten | Grade Int | High Int | College Int
derive instance genericGrade :: Generic Grade _

instance Encode Grade where
  encode = genericEncode defaultOptions

instance Decode Grade where
  decode = genericDecode defaultOptions

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

instance Decode Student where
  decode = genericDecode defaultOptions

data TeachingStatus = StudentTeacher | Probationary | NonTenured | Tenured
derive instance genericTeachingStatus :: Generic TeachingStatus _

instance Encode TeachingStatus where
  encode = genericEncode defaultOptions

instance Decode TeachingStatus where
  decode = genericDecode defaultOptions

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

instance Decode Teacher where
  decode = genericDecode defaultOptions

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

processAjaxResult :: ∀ a. Decode a => Show a => Proxy a -> Either String String -> String
processAjaxResult _ = case _ of
  Left e -> e
  Right res ->
    case runExcept (decodeJSON res :: _ a) of
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

foreign import _reverseKeys :: Foreign -> String