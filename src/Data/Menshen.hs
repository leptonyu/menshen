{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.Menshen(
    HasValid(..)
  , Validator
  , HasValidSize
  , size
  , notEmpty
  , notBlank
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
  , (&)
  , (=~)
  , ValidationException(..)
  , HasI18n(..)
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


class HasI18n a where
  toI18n :: a -> String

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

class Monad m => HasValid m where
  invalid  :: HasI18n a => a -> m b
  invalid = error . toI18n

instance HasValid IO

instance HasValid (Either String) where
  invalid = Left . toI18n

type Validator a = forall m. HasValid m => m a -> m a

class HasValidSize a where
  size :: (Word64, Word64) -> Validator a
  size (x,y) = \ma -> do
    a <- ma
    let la = getLength a
    if la < x || la > y
      then invalid $ InvalidSize x y
      else return a
  notEmpty :: Validator a
  notEmpty = \ma -> do
    a <- ma
    if getLength a == 0
      then invalid InvalidNotEmpty
      else return a
  notBlank :: Validator a
  notBlank = \ma -> do
    a <- ma
    if getLength a == 0
      then invalid InvalidNotBlank
      else return a
  getLength :: a -> Word64
  {-# MINIMAL getLength #-}

instance HasValidSize TS.Text where
  getLength = fromIntegral . TS.length

instance HasValidSize TL.Text where
  getLength = fromIntegral . TL.length

instance HasValidSize [a] where
  getLength = fromIntegral . length

pattern :: RegexLike Regex a => String -> Validator a
pattern p = \ma -> do
  a <- ma
  if a =~ p then return a
    else invalid $ InvalidPattern p

emailPattern :: String
emailPattern = "^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,64}$"

email :: RegexLike Regex a => Validator a
email = \ma -> do
  a <- ma
  if a =~ emailPattern then return a
    else invalid InvalidEmail

positive :: (Eq a, Num a) => Validator a
positive = \ma -> do
  a <- ma
  if a /= 0 && abs a - a == 0
    then return a
    else invalid InvalidPositive

positiveOrZero :: (Eq a, Num a) => Validator a
positiveOrZero = \ma -> do
  a <- ma
  if abs a - a == 0
    then return a
    else invalid InvalidPositiveOrZero

negative :: (Eq a, Num a) => Validator a
negative = \ma -> do
  a <- ma
  if a /= 0 && abs a + a == 0
    then return a
    else invalid InvalidNegative

negativeOrZero :: (Eq a, Num a) => Validator a
negativeOrZero = \ma -> do
  a <- ma
  if abs a + a == 0
    then return a
    else invalid InvalidNegativeOrZero

assertTrue :: Validator Bool
assertTrue = \ma -> do
  a <- ma
  if a then return a
    else invalid ShouldBeTrue

assertFalse :: Validator Bool
assertFalse = \ma -> do
  a <- ma
  if not a then return a
    else invalid ShouldBeFalse

notNull :: Validator (Maybe a)
notNull = \ma -> do
  a <- ma
  case a of
    Just _ -> return a
    _      -> invalid ShouldNotNull

assertNull :: Validator (Maybe a)
assertNull = \ma -> do
  a <- ma
  case a of
    Just _ -> invalid ShouldNull
    _      -> return a

maxInt :: (Ord a, Integral a) => a -> Validator a
maxInt m = \ma -> do
  a <- ma
  if a > m
    then invalid (InvalidMax $ toInteger m)
    else return a

minInt :: (Ord a, Integral a) => a -> Validator a
minInt m = \ma -> do
  a <- ma
  if a < m
    then invalid (InvalidMin $ toInteger m)
    else return a

maxDecimal :: (Ord a, RealFloat a) => a -> Validator a
maxDecimal m = \ma -> do
  a <- ma
  if a > m
    then invalid (InvalidDecimalMax $ fromFloatDigits m)
    else return a

minDecimal :: (Ord a, RealFloat a) => a -> Validator a
minDecimal m = \ma -> do
  a <- ma
  if a < m
    then invalid (InvalidDecimalMin $ fromFloatDigits m)
    else return a
