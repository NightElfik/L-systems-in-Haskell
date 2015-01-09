module LSystem.ShowReadUtils (
    myLex,
    readSafeCustom,
    readListSafe,
    showAll,
    showsCustAll,
    showsList,
    showsCustomList,
    showsCommaList,
    showsCustomCommaList,
    readAll,
    readAllSafe,
    readsComma,
    readsCommaList
) where

import Data.Char
import LSystem.Utils

-- http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html
-- type ShowS = String -> String
-- type ReadS a = String -> [(a, String)]


----------------------------------------------------------------------------------------------------
-- | More clever impllementation of Prelude.lex. It can lex unicode charaters and special-char
-- sequencel will split to every possible part. For example, we want lex part "^-2" of expression
-- "2^-2". Standard lex will produce:
--
-- > lex "^-2" = [("^-","2")]
--
-- but this improve lex will produce:
--
-- > myLex "^-2" = [("^","-2"), ("^-","2")]
--
myLex :: ReadS String
myLex str =
    let trimmedStr = dropWhile isSeparator str
        lexOutput = lex trimmedStr in
    if null lexOutput then
        -- str is not "" (other wise lex would return [("","")]), so we have some special char first
        [([head trimmedStr], tail trimmedStr)] -- so return it
    else
        -- split special sequences which stndard lex keeps together
        lexOutput ++ [(spl1, spl2 ++ rest) |
            (xs, rest) <- lexOutput,
            all (\c -> c `elem` specChars) xs,
            (spl1, spl2) <- splitNonEmpty xs] where
                specChars = "!@#$%&*+./<=>?\\^|:-~" -- from lex source

-- | Reads a string by given read function and if it failes, given default value is returned.
readSafeCustom :: a -> (ReadS a) -> ReadS a
readSafeCustom defVal readFunc str =
    let x = readFunc str in
    if null x then
        [(defVal, str)]
    else
        x

-- | Reads lis by given read function and if it failes, epty list and original string is returned.
readListSafe :: (ReadS [a]) -> ReadS [a]
readListSafe readFunc str =
    let x = readFunc str in
    if null x then
        [([], str)]
    else
        x

-- | Shows whole list.
showAll :: Show a => [a] -> ShowS
showAll [] = id
showAll (x:xs) = shows x . showAll xs

-- | Shows whole list by cust shows function nad end function (can be eg. (""++)).
showsCustAll :: (a -> ShowS) -> ShowS -> [a] -> ShowS
showsCustAll showsFunc endFunc xs = foldr (\x  acc -> showsFunc x . acc) endFunc xs

-- | Shows list by default shows function separated by given separator.
showsList :: Show a => String -> [a] -> ShowS
showsList sep = showsCustomList sep shows

-- | Shows list by given function separated by given separator.
showsCustomList :: Show a => String -> (a -> ShowS) -> [a] -> ShowS
showsCustomList _ _ [] = id
showsCustomList sep showsFunc (x:xs) = showsFunc x . showsCommaList sep showsFunc xs where
    showsCommaList _ _ [] = id
    showsCommaList sep showsFunc (x:xs) = (sep++) . showsFunc x . showsCommaList sep showsFunc xs

-- | Shows list by default shows function separated by comma.
showsCommaList :: Show a => [a] -> ShowS
showsCommaList = showsList ", "

-- | Shows list by given function separated by comma.
showsCustomCommaList :: Show a => (a -> ShowS) -> [a] -> ShowS
showsCustomCommaList = showsCustomList ", "

-- | Reads given string by given read function until it failes. Returns result in array together
-- with reminder of string or [] if nothing is readed.
readAll :: (ReadS a) -> ReadS [a]
readAll readFunc str = [(p:ps, rest) |
    (p, r) <- readFunc str,
    (ps, rest) <- readAllSafe readFunc r]

-- | Reads given string by given "unsafe" function until it failes (returns []). Returns reminder of
-- string which is unprocessed even if nothing is readed.
readAllSafe :: (ReadS a) -> ReadS [a]
readAllSafe readFunc str =
    let x = readFunc str in
    if null x then
        [([], str)]
    else
        [(y:ys, rest) | (y, r) <- x, (ys, rest) <- readAllSafe readFunc r]

-- | Reads comma and rest bygiven read func. Returns [] if failed.
readsComma :: (ReadS a) -> ReadS a
readsComma readFunc str = [x | (",", rest) <- myLex str, x <- readFunc rest]

-- | Reads comma separated list. Returns [] if failed.
readsCommaList :: (ReadS a) -> ReadS [a]
readsCommaList readFunc str = [(x:xs, rest) | (x, r) <- readFunc str, (xs, rest) <- readAllSafe (readsComma readFunc) r]




















