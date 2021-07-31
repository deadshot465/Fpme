module Ch25a where
  
import Prelude

import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Foreign.Generic (class Decode, class Encode, Options, SumEncoding(..), decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Type.Proxy (Proxy(..))

test :: Effect Unit
test = launchAff_ do
  result <- bimap Affjax.printError _.body <$> Affjax.post ResponseFormat.string echoURL (Just $ RequestBody.String $ encodeJSON teacher)
  log $ processAjaxResult (Proxy :: _ ReversedTeacher) result

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

type Personal =
  { height :: Centimeters
  , weight :: Kilograms
  , age :: Years
  }

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
  decode = genericDecode decodeOptions

instance Show Grade where
  show = genericShow

type Student =
  { grade :: Grade
  , teacher :: Teacher
  , gpa :: GPA
  , personal :: Personal
  }

data TeachingStatus = Student | Probationary | NonTenured | Tenured
derive instance genericTeachingStatus :: Generic TeachingStatus _

instance Encode TeachingStatus where
  encode = genericEncode defaultOptions

instance Decode TeachingStatus where
  decode = genericDecode decodeOptions

instance Show TeachingStatus where
  show = genericShow

type Teacher =
  { grades :: Array Grade
  , numberOfStudents :: Int
  , personal :: Personal
  , status :: TeachingStatus
  }

teacher :: Teacher
teacher = 
  { grades: [ Preschool, Kindergarten, Grade 1 ]
  , numberOfStudents: 23
  , personal: 
    { height: Centimeters 162.56
    , weight: Kilograms 63.5
    , age: Years 31
    }
  , status: NonTenured
  }

type ReversedPersonal =
  { thgieh :: Centimeters
  , thgiew :: Kilograms
  , ega :: Years
  }

type ReversedStudent =
  { edarg :: Grade
  , rehcaet :: Teacher
  , apg :: GPA
  , lanosrep :: ReversedPersonal
  }

type ReversedTeacher =
  { sedarg :: Array Grade
  , stnedutSfOrebmun :: Int
  , lanosrep :: ReversedPersonal
  , sutats :: TeachingStatus
  }

decodeOptions :: Options
decodeOptions = defaultOptions
  { sumEncoding = TaggedObject
    { tagFieldName: "gat"
    , contentsFieldName: "stnetnoc"
    , constructorTagTransform: identity
    }
  }

processAjaxResult :: ∀ a. Decode a => Show a => Proxy a -> Either String String -> String
processAjaxResult _ = case _ of
  Left e -> e
  Right res ->
    case runExcept $ (decodeJSON res :: _ a) of
      Left e' -> show e'
      Right res' -> show res'