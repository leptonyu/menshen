# menshen

[![Hackage](https://img.shields.io/hackage/v/menshen.svg)](https://hackage.haskell.org/package/menshen)
[![Build Status](https://travis-ci.org/leptonyu/menshen.svg?branch=master)](https://travis-ci.org/leptonyu/menshen)


```Haskell

{-# LANGUAGE RecordWildCards #-}
module Main where
import Data.Menshen
data Body = Body
  { name :: String
  , age  :: Int
  } deriving Show

verifyBody :: Validator Body
verifyBody = vcvt $ Body{..} -> Body
  <$> name ?: mark "name" . pattern "^[a-z]{3,6}$"
  <*> age  ?: mark "age"  . minInt 1 . maxInt 150

makeBody :: String -> Int -> Either String Body
makeBody name age = Body{..} ?: verifyBody

main = do
  print $ makeBody "daniel" 15


```

