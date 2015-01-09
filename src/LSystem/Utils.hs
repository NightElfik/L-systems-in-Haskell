module LSystem.Utils (
    (/.),
    maybeHead,
    parseInt,
    toMaybeCond,
    readToBlankLine,
    splitNonEmpty,
    parse,
    degToRad,
    round2,
    filterNonEmpty
) where

import Data.List
import Data.Function


----------------------------------------------------------------------------------------------------
-- | Helper operator for division.
(/.) = (/) `on` fromIntegral

maybeHead :: [a] -> Maybe a
maybeHead xs =
    let x = take 1 xs in
    if length x == 1 then Just $ head x else Nothing


-- | Reads string by given func and returns first result which has empty remainder (whole given
-- string was readed by reads func)
parse :: ReadS a -> String -> Maybe a
parse readsFunc str =
    let fullyParsed = filter (\ (_, rest) -> null rest) $ readsFunc str in
    if null fullyParsed then
        Nothing
    else
        Just $ fst $ head fullyParsed

parseInt :: String -> Maybe Int
parseInt = parse reads

toMaybeCond :: (a -> Bool) -> a -> Maybe a
toMaybeCond condFunc x = if condFunc x then Just x else Nothing

readToBlankLine :: [String] -> ([String], [String])
readToBlankLine strLines =
    case break null strLines of
        ([], rest) -> ([], rest)
        (lines, rest) -> (lines, drop 1 rest) -- drop empty line


-- | Returns all possible splits of list, left nor right part of split is epty.
splitNonEmpty :: [a] -> [([a], [a])]
splitNonEmpty xs = [splitAt n xs | n <- [1..(length xs - 1)]]

-- | Converts angle in dergees to radians.
degToRad :: Double -> Double
degToRad x = x * (pi / 180.0)

-- | Rounds double to precision of 2 digits after deciaml point.
round2 :: Double -> Double
round2 n = round (n * 100) /. 100

filterNonEmpty :: [[a]] -> [[a]]
filterNonEmpty = filter (\x -> not $ null x)













