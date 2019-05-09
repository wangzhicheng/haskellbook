module Chapter11.Phone where

data DaPhone = DaPhone

-- validButtons = "123456789*#"
type Digit = Char

-- valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone
            -> Char
            -> [(Digit, Presses)]
reverseTaps = undefined

cellPhoneDead :: DaPhone
              -> String
              -> [(Digit, Presses)]
cellPhoneDead = undefined
