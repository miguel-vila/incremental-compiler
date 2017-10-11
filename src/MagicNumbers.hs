module MagicNumbers where

intShift :: Int
intShift = 2

intTag :: Int
intTag = 3

boolMask :: Integer
boolMask = 63 -- 00111111

falseValue :: Integer
falseValue = 47  -- 00101111

trueValue :: Integer
trueValue = 111  -- 01101111

charMask :: Integer
charMask = 255

charTag :: Int
charTag = 15 -- 00001111

charShift :: Int
charShift = 8

nilValue :: Integer
nilValue = 63 -- 00111111

pairTag :: Int
pairTag = 1

pairMask :: Integer
pairMask = 7 -- 00000111

carOffset :: Integer
carOffset = -4

cdrOffset :: Integer
cdrOffset = - 8

vectorTag :: Int
vectorTag = 5 -- 00000101

vectorMask :: Integer
vectorMask = 7 -- 00000111

vectorLengthOffset :: Integer
vectorLengthOffset = 4

vectorContentOffset :: Integer
vectorContentOffset = 8
