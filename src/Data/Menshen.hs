{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
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
  , valify
  , (?:)
  -- * Reexport Functions
  , (=~)
  ) where

import           Data.Scientific
import qualified Data.Text       as TS
import qualified Data.Text.Lazy  as TL
import           Data.Word
import           Text.Regex.TDFA
#if __GLASGOW_HASKELL__ > 708
import           Data.Function   ((&))
#else
infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x
#endif

-- | apply record validation to the value
infixl 5 ?
(?) = (&)

-- | lift value a to validation context and check if it is valid.
--
infixl 5 ?:
(?:) :: HasValid m => a -> Validator a -> m a
(?:) = valify

-- | Plan for i18n translate, now just for english.
class HasI18n a where
  toI18n :: a -> String

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

-- | Define how invalid infomation passed to upper layer.
class Monad m => HasValid m where
  invalid  :: HasI18n a => a -> m b
  invalid = error . toI18n

instance HasValid (Either String) where
  invalid = Left . toI18n

-- | Validator, use to define detailed validation check.
type Validator a = forall m. HasValid m => m a -> m a

-- | Length checker bundle
class HasValidSize a where
  -- | Size validation
  size :: (Word64, Word64) -> Validator a
  size (x,y) = \ma -> do
    a <- ma
    let la = getLength a
    if la < x || la > y
      then invalid $ InvalidSize x y
      else return a
  -- | Assert not empty
  notEmpty :: Validator a
  notEmpty = \ma -> do
    a <- ma
    if getLength a == 0
      then invalid InvalidNotEmpty
      else return a
  -- | Assert not blank
  notBlank :: Validator a
  notBlank = \ma -> do
    a <- ma
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
pattern p = \ma -> do
  a <- ma
  if a =~ p then return a
    else invalid $ InvalidPattern p

emailPattern :: String
emailPattern = "^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,64}$"

-- | Email validation
email :: RegexLike Regex a => Validator a
email = \ma -> do
  a <- ma
  if a =~ emailPattern then return a
    else invalid InvalidEmail

-- | Positive validation
positive :: (Eq a, Num a) => Validator a
positive = \ma -> do
  a <- ma
  if a /= 0 && abs a - a == 0
    then return a
    else invalid InvalidPositive

-- | Positive or zero validation
positiveOrZero :: (Eq a, Num a) => Validator a
positiveOrZero = \ma -> do
  a <- ma
  if abs a - a == 0
    then return a
    else invalid InvalidPositiveOrZero

-- | Negative validation
negative :: (Eq a, Num a) => Validator a
negative = \ma -> do
  a <- ma
  if a /= 0 && abs a + a == 0
    then return a
    else invalid InvalidNegative

-- | Negative or zero validation
negativeOrZero :: (Eq a, Num a) => Validator a
negativeOrZero = \ma -> do
  a <- ma
  if abs a + a == 0
    then return a
    else invalid InvalidNegativeOrZero

-- | Assert true
assertTrue :: Validator Bool
assertTrue = \ma -> do
  a <- ma
  if a then return a
    else invalid ShouldBeTrue

-- | Assert false
assertFalse :: Validator Bool
assertFalse = \ma -> do
  a <- ma
  if not a then return a
    else invalid ShouldBeFalse

-- | Assert not null
notNull :: Validator (Maybe a)
notNull = \ma -> do
  a <- ma
  case a of
    Just _ -> return a
    _      -> invalid ShouldNotNull

-- | Assert null
assertNull :: Validator (Maybe a)
assertNull = \ma -> do
  a <- ma
  case a of
    Just _ -> invalid ShouldNull
    _      -> return a

-- | Maximum int validation
maxInt :: Integral a => a -> Validator a
maxInt m = \ma -> do
  a <- ma
  if a > m
    then invalid (InvalidMax $ toInteger m)
    else return a

-- | Minimum int validation
minInt :: Integral a => a -> Validator a
minInt m = \ma -> do
  a <- ma
  if a < m
    then invalid (InvalidMin $ toInteger m)
    else return a

-- | Maximum decimal validation
maxDecimal :: RealFloat a => a -> Validator a
maxDecimal m = \ma -> do
  a <- ma
  if a > m
    then invalid (InvalidDecimalMax $ fromFloatDigits m)
    else return a

-- | Minimum int validation
minDecimal :: RealFloat a => a -> Validator a
minDecimal m = \ma -> do
  a <- ma
  if a < m
    then invalid (InvalidDecimalMin $ fromFloatDigits m)
    else return a

-- | lift value a to validation context and check if it is valid.
valify :: HasValid m => a -> Validator a -> m a
valify a f = return a ? f

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
-- > valifyBody :: Validator Body
-- > valifyBody = \ma -> do
-- >   Body{..} <- ma
-- >   Body
-- >     <$> name ?: pattern "^[a-z]{3,6}$"
-- >     <*> age  ?: minInt 1 . maxInt 150
-- >
-- > makeBody :: String -> Int -> Either String Body
-- > makeBody name age = Body{..} ?: valifyBody
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
