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

valifyBody :: Validator Body
valifyBody = \ma -> do
  Body{..} <- ma
  Body
    <$> name ?: pattern "^[a-z]{3,6}$"
    <*> age  ?: minInt 1 . maxInt 150

makeBody :: String -> Int -> Either String Body
makeBody name age = Body{..} ?: valifyBody

main = do
  print $ makeBody "daniel" 15


```

