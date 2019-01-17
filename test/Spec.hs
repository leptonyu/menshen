{-# LANGUAGE CPP               #-}
{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Either
import           Data.Int
import           Data.Maybe
import           Data.Menshen
import           Data.Word
import           Test.Hspec
import           Test.QuickCheck
#if __GLASGOW_HASKELL__ <= 708
import           Control.Applicative
#endif

main = hspec spec

spec :: Spec
spec = do
  describe "Data.Menshen" specProperty

intValue :: Either String Int
intValue = Right 10

strValue :: Either String String
strValue = Right "x@xxx.xx"

true :: Either String Bool
true = Right True

false :: Either String Bool
false = Right False

nullValue :: Either String (Maybe Int)
nullValue = Right Nothing

notNullValue :: Either String (Maybe Int)
notNullValue = Right (Just 1)

data Body = Body
  { name :: String
  , age  :: Int
  } deriving (Eq, Show)

instance HasValid Parser where
  invalid = fail . toI18n

instance FromJSON Body where
  parseJSON = withObject "Body" $ \v -> Body
    <$> v .: "name" ? pattern "^[a-z]{3,6}$"
    <*> v .: "age"  ? minInt 1 . maxInt 150

valifyBody :: Validator Body
valifyBody = \ma -> do
  Body{..} <- ma
  Body
    <$> name ?: pattern "^[a-z]{3,6}$"
    <*> age  ?: minInt 1 . maxInt 150

makeBody :: String -> Int -> Either String Body
makeBody name age = Body{..} ?: valifyBody

specProperty = do
  context "bool" $ do
    it "true" $ do
      (true  ? assertTrue)  `shouldBe`      true
      (true  ? assertFalse) `shouldSatisfy` isLeft
    it "false" $ do
      (false ? assertFalse) `shouldBe`      false
      (false ? assertTrue)  `shouldSatisfy` isLeft
  context "int" $ do
    it "minInt" $ do
      (intValue ? minInt 10) `shouldBe`      intValue
      (intValue ? minInt 11) `shouldSatisfy` isLeft
    it "maxInt" $ do
      (intValue ? maxInt 10) `shouldBe`      intValue
      (intValue ? maxInt  9) `shouldSatisfy` isLeft
    it "positive" $ do
      (intValue ? positive)       `shouldBe`      intValue
      (intValue ? positiveOrZero) `shouldBe`      intValue
      (intValue ? negative)       `shouldSatisfy` isLeft
      (intValue ? negativeOrZero) `shouldSatisfy` isLeft
    it "randomWord" $ property $ \a ->
      let b = Right a :: Either String Word8
      in (b ? minInt 0 ? maxInt 255 ? positiveOrZero) == b
    it "randomInt-negative" $ property $ \a ->
      let b = Right (negate $ abs a) :: Either String Int8
      in (b ? minInt (-128) ? maxInt 0 ? negativeOrZero) == b
    it "randomInt-positive" $ property $ \a ->
        let b = Right (if a == -128 then 0 else abs a) :: Either String Int8
        in (b ? minInt 0 ? maxInt 127 ? positiveOrZero) == b
  context "string" $ do
    it "size" $ do
      (strValue ? size (0,10))  `shouldBe`      strValue
      (strValue ? size (10,0))  `shouldSatisfy` isLeft
      (strValue ? size (10,30)) `shouldSatisfy` isLeft
    it "pattern" $ do
      (strValue ? pattern "^xx$") `shouldSatisfy` isLeft
      (strValue ? pattern "xx")   `shouldBe`      strValue
      (strValue ? email)          `shouldBe`      strValue
    it "notBlank" $ do
      (strValue ? notBlank)       `shouldBe`      strValue
      (strValue ? notEmpty)       `shouldBe`      strValue
  context "null" $ do
    it "null" $ do
      (nullValue ? assertNull)    `shouldBe`      nullValue
      (nullValue ? notNull)       `shouldSatisfy` isLeft
    it "notNull" $ do
      (notNullValue ? notNull)    `shouldBe`      notNullValue
      (notNullValue ? assertNull) `shouldSatisfy` isLeft
  context "valify" $ do
    it "makeBody" $ do
      makeBody "daniel"  5   `shouldSatisfy` isRight
      makeBody "daniel"  200 `shouldSatisfy` isLeft
      makeBody "danielx" 5   `shouldSatisfy` isLeft
    it "makeJsonBody" $ do
      (decode "{\"name\":\"daniel\",\"age\":15}" :: Maybe Body) `shouldSatisfy` isJust
      (decode "{\"name\":\"daniel\",\"age\":-1}" :: Maybe Body) `shouldBe` Nothing



