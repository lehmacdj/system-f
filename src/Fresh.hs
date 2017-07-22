module Fresh where

varStream :: [String]
varStream = letters ++ (varStream >>= appendEachLetter)
    where appendEachLetter s = map (s++) letters
          letters = map (:[]) ['a'..'z']

fresh :: [String] -> String
fresh = head . flip filter varStream . flip ((not.) . elem)
