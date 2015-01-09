module LSystem.GeneralDataTypes (
    Name,
    readsName,
    readsNames,
    nameFromString
) where

import Data.List
import Data.Char
import LSystem.ShowReadUtils


----------------------------------------------------------------------------------------------------
-- | Name of parameter or variable. First char of name have to be alphabetic, rest can contain any
-- alphanumeric chars.
newtype Name = Name String
    deriving (Eq)

instance Show Name where
    showsPrec _ (Name s) = (s++)

instance Read Name where
    readsPrec _ = readsName

showsNames :: [Name] -> ShowS
showsNames = showsCommaList

readsName :: ReadS Name
readsName str = [(Name name, rest) | (name, rest) <- myLex str, isNameValid (Name name)]

readsNames :: ReadS [Name]
readsNames = readsCommaList readsName

nameFromString :: String -> Maybe Name
nameFromString str = if isNameValid $ Name str then Just $ Name str else Nothing

isNameValid :: Name -> Bool
isNameValid (Name []) = False
isNameValid (Name str) = isAlpha (head str) && foldl testCharAcc True str where
    testCharAcc acc c = acc && isAlphaNum c


























