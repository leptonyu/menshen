{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module:      Data.Salak
-- Copyright:   (c) 2019 Daniel YU
-- License:     BSD3
-- Maintainer:  Daniel YU <leptonyu@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Data Validation inspired by JSR305
--
module Data.Menshen(
  -- * How to use this library
  -- $use

  -- * Definition
    HasValid(..)
  , Validator
  , ValidationException(..)
  , HasI18n(..)
  , ValidatorErr(..)
  , VerifyResult(..)
  -- * Validation Functions
  , HasValidSize(..)
  , notNull
  , assertNull
  , assertTrue
  , assertFalse
  , positive
  , positiveOrZero
  , negative
  , negativeOrZero
  , minInt
  , maxInt
  , minDecimal
  , maxDecimal
  , pattern
  , email
  -- * Validation Operations
  , (?)
  , verify
  , (?:)
  , vcvt
  -- * Reexport Functions
  , (=~)
  ) where

import           Control.Exception    (Exception (..), SomeException)
import           Control.Monad.Catch
import           Data.Scientific
import qualified Data.Text            as TS
import qualified Data.Text.Lazy       as TL
import           Data.Word
import           Text.Regex.TDFA
import           Text.Regex.TDFA.Text ()
#if __GLASGOW_HASKELL__ > 708
import           Data.Function        ((&))
#else
infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x
#endif

-- | apply record validation to the value
infixl 5 ?
(?) :: HasValid m => m a -> Validator a -> m a
(?) = (&)

-- | lift value a to validation context and check if it is valid.
--
infixl 5 ?:
(?:) :: HasValid m => a -> Validator a -> m a
(?:) = verify

-- | Plan for i18n translate, now just for english.
class Exception e => HasI18n e where
  toI18n :: e -> String
  toErr :: String -> e -> ValidatorErr
  toErr field e =
    let message   = toI18n e
        exception = toException e
    in ValidatorErr{..}

-- | Validation Error Message
data ValidationException
  = ShouldBeFalse
  | ShouldBeTrue
  | ShouldNull
  | ShouldNotNull
  | InvalidSize Word64 Word64
  | InvalidPositive
  | InvalidPositiveOrZero
  | InvalidNegative
  | InvalidNegativeOrZero
  | InvalidMax Integer
  | InvalidMin Integer
  | InvalidEmail
  | InvalidNotBlank
  | InvalidNotEmpty
  | InvalidPast
  | InvalidFuture
  | InvalidPastOrPresent
  | InvalidFutureOrPresent
  | InvalidDecimalMax Scientific
  | InvalidDecimalMin Scientific
  | InvalidDigits Word8 Word8
  | InvalidPattern String
  deriving Show

instance Exception ValidationException

instance HasI18n ValidationException where
  toI18n ShouldBeTrue           = "must be true"
  toI18n ShouldBeFalse          = "must be false"
  toI18n ShouldNull             = "must be null"
  toI18n ShouldNotNull          = "must not be null"
  toI18n (InvalidSize a b)      = "size must be between " ++ show a ++ " and " ++ show b
  toI18n InvalidPositive        = "must be greater than 0"
  toI18n InvalidPositiveOrZero  = "must be greater than or equal to 0"
  toI18n InvalidNegative        = "must be less than 0"
  toI18n InvalidNegativeOrZero  = "must be less than or equal to 0"
  toI18n InvalidEmail           = "must be a well-formed email address"
  toI18n InvalidNotBlank        = "must not be blank"
  toI18n InvalidNotEmpty        = "must not be empty"
  toI18n InvalidPast            = "must be a past date"
  toI18n InvalidFuture          = "must be a future date"
  toI18n InvalidPastOrPresent   = "must be a date in the past or in the present"
  toI18n InvalidFutureOrPresent = "must be a date in the present or in the future"
  toI18n (InvalidMax n)         = "must be less than or equal to " ++ show n
  toI18n (InvalidMin n)         = "must be greater than or equal to " ++ show n
  toI18n (InvalidDecimalMax d)  = "must be less than " ++ show d
  toI18n (InvalidDecimalMin d)  = "must be greater than " ++ show d
  toI18n (InvalidDigits i f)    = "numeric value out of bounds (<" ++ show i ++ " digits>.<" ++ show f ++ " digits> expected)"
  toI18n (InvalidPattern r)     = "must match " ++ r


data ValidatorErr = ValidatorErr
  { exception :: SomeException
  , message   :: String
  , field     :: String
  } deriving Show

instance Exception ValidatorErr

-- | Define how invalid infomation passed to upper layer.
class Monad m => HasValid m where
  invalid  :: HasI18n a => a -> m b
  invalid = error . toI18n
  mark :: String -> m a -> m a
  mark _ = id

instance HasValid (Either String) where
  invalid = Left . toI18n

instance {-# OVERLAPPABLE #-} (Monad m, MonadThrow m) => HasValid m where
  invalid = throwM

data VerifyResult a = Invalid [ValidatorErr] | Valid a deriving (Show, Functor)

instance Applicative VerifyResult where
  pure = Valid
  (Invalid a) <*> (Invalid b) = Invalid (a ++ b)
  (Invalid a) <*> _ = Invalid a
  _ <*> (Invalid b) = Invalid b
  (Valid f) <*> (Valid b) = Valid (f b)

instance Monad VerifyResult where
  return = Valid
  (Valid   a) >>= f = f a
  (Invalid a) >>= _ = Invalid a

instance HasValid VerifyResult where
  invalid e = Invalid [toErr "" e]
  mark name ma =
    let go err = if null (field err) then err { field = name } else err { field = field err ++ "." ++ name }
    in case ma of
        (Invalid x) -> Invalid $ go <$> x
        v           -> v

-- | Validator, use to define detailed validation check.
type Validator a  = forall m. HasValid m => m a -> m a
type Validator' a = forall m. HasValid m => a -> m a

vcvt :: Validator' a -> Validator a
vcvt f = (>>= f)

-- | Length checker bundle
class HasValidSize a where
  -- | Size validation
  size :: (Word64, Word64) -> Validator a
  size (x,y) = vcvt $ \a -> do
    let la = getLength a
    if la < x || la > y
      then invalid $ InvalidSize x y
      else return a
  -- | Assert not empty
  notEmpty :: Validator a
  notEmpty = vcvt $ \a -> do
    if getLength a == 0
      then invalid InvalidNotEmpty
      else return a
  -- | Assert not blank
  notBlank :: Validator a
  notBlank = vcvt $ \a -> do
    if getLength a == 0
      then invalid InvalidNotBlank
      else return a
  -- | calculate length from value
  getLength :: a -> Word64
  {-# MINIMAL getLength #-}

instance HasValidSize TS.Text where
  getLength = fromIntegral . TS.length

instance HasValidSize TL.Text where
  getLength = fromIntegral . TL.length

instance HasValidSize [a] where
  getLength = fromIntegral . length

-- | Regular expression validation
pattern :: RegexLike Regex a => String -> Validator a
pattern p = vcvt $ \a -> do
  if a =~ p then return a
    else invalid $ InvalidPattern p

emailPattern :: String
emailPattern = "^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,64}$"

-- | Email validation
email :: RegexLike Regex a => Validator a
email = vcvt $ \a -> do
  if a =~ emailPattern then return a
    else invalid InvalidEmail

-- | Positive validation
positive :: (Eq a, Num a) => Validator a
positive = vcvt $ \a -> do
  if a /= 0 && abs a - a == 0
    then return a
    else invalid InvalidPositive

-- | Positive or zero validation
positiveOrZero :: (Eq a, Num a) => Validator a
positiveOrZero = vcvt $ \a -> do
  if abs a - a == 0
    then return a
    else invalid InvalidPositiveOrZero

-- | Negative validation
negative :: (Eq a, Num a) => Validator a
negative = vcvt $ \a -> do
  if a /= 0 && abs a + a == 0
    then return a
    else invalid InvalidNegative

-- | Negative or zero validation
negativeOrZero :: (Eq a, Num a) => Validator a
negativeOrZero = vcvt $ \a -> do
  if abs a + a == 0
    then return a
    else invalid InvalidNegativeOrZero

-- | Assert true
assertTrue :: Validator Bool
assertTrue = vcvt $ \a -> do
  if a then return a
    else invalid ShouldBeTrue

-- | Assert false
assertFalse :: Validator Bool
assertFalse = vcvt $ \a -> do
  if not a then return a
    else invalid ShouldBeFalse

-- | Assert not null
notNull :: Validator (Maybe a)
notNull = vcvt $ \a -> do
  case a of
    Just _ -> return a
    _      -> invalid ShouldNotNull

-- | Assert null
assertNull :: Validator (Maybe a)
assertNull = vcvt $ \a -> do
  case a of
    Just _ -> invalid ShouldNull
    _      -> return a

-- | Maximum int validation
maxInt :: Integral a => a -> Validator a
maxInt m = vcvt $ \a -> do
  if a > m
    then invalid (InvalidMax $ toInteger m)
    else return a

-- | Minimum int validation
minInt :: Integral a => a -> Validator a
minInt m = vcvt $ \a -> do
  if a < m
    then invalid (InvalidMin $ toInteger m)
    else return a

-- | Maximum decimal validation
maxDecimal :: RealFloat a => a -> Validator a
maxDecimal m = vcvt $ \a -> do
  if a > m
    then invalid (InvalidDecimalMax $ fromFloatDigits m)
    else return a

-- | Minimum int validation
minDecimal :: RealFloat a => a -> Validator a
minDecimal m = vcvt $ \a -> do
  if a < m
    then invalid (InvalidDecimalMin $ fromFloatDigits m)
    else return a

-- | lift value a to validation context and check if it is valid.
verify :: HasValid m => a -> Validator a -> m a
verify a f = f (return a)

-- $use
--
-- This library is designed for validate haskell records, such as in configurations or web request parameters.
--
-- Usage:
--
-- > {-# LANGUAGE RecordWildCards #-}
-- > module Main where
-- > import Data.Menshen
-- > data Body = Body
-- >   { name :: String
-- >   , age  :: Int
-- >   } deriving Show
-- >
-- > verifyBody :: Validator Body
-- > verifyBody = vcvt $ \Body{..} -> Body
-- >   <$> name ?: mark "name" . pattern "^[a-z]{3,6}$"
-- >   <*> age  ?: mark "age"  . minInt 1 . maxInt 150
-- >
-- > makeBody :: String -> Int -> Either String Body
-- > makeBody name age = Body{..} ?: verifyBody
-- >
-- > main = do
-- >   print $ makeBody "daniel" 15
-- >
--
-- Useage in Record parsing process:
--
-- > instance HasValid Parser where
-- >   invalid = fail . toI18n
-- >
-- > instance FromJSON Body where
-- >   parseJSON = withObject "Body" $ \v -> Body
-- >     <$> v .: "name" ? pattern "^[a-z]{3,6}$"
-- >     <*> v .: "age"  ? minInt 1 . maxInt 150
-- >
