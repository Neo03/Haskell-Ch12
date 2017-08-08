module EqCaseGuard where

data PersonInvalid = NameEmpty
                    | AgeToLow

-- Compiles fine without Eq

toString :: PersonInvalid -> String
toString NameEmpty = "Name is Empty"
toString AgeToLow = "Age to low"

instance Show PersonInvalid where
  show = toString

-- do not compiles without Eq
{--
blah :: PersonInvalid -> String
blah pi
  | pi == NameEmpty = "NameEmpty"
  | pi == AgeToLow = "AgeToLow"
  | otherwise = "???"
--}
