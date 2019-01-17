# menshen

[![Build Status](https://travis-ci.org/leptonyu/menshen.svg?branch=master)](https://travis-ci.org/leptonyu/menshen)


```Haskell

λ> let value :: Int -> Either String Int; value = Right
λ> value 10 & maxInt 8
Left "must be less than or equal to 8"
λ> value 10 & maxInt 10
Right 10
λ> value 10 & maxInt 10 & minInt 8
Right 10
λ> value 9 & maxInt 10 & minInt 8
Right 9
λ> value 7 & maxInt 10 & minInt 8
Left "must be greater than or equal to 8"
λ> let str :: String -> Either String String; str = Right
λ> str "xxxx@gmail.com" & email
Right "xxxx@gmail.com"
λ> str "xxxx@gmail" & email
Left "must be a well-formed email address"
λ> str "123456789" & size (6,32)
Right "123456789"
λ> str "12345" & size (6,32)
Left "size must be between 6 and 32"


```

