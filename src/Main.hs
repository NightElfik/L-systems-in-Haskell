module Main (
    main
) where

import Data.Map
import Data.List
import Data.Char
import Data.Maybe
import System.IO
import System.FilePath
import System.Environment -- for getArgs
import LSystem
import LSystem.Utils
import LSystem.DataTypes
import LSystem.Interprets


----------------------------------------------------------------------------------------------------
-- | Entry point of the application.
main = do
    args <- getArgs
    if length args == 3 then
        loadAndInterpretLSystem (args !! 0) (args !! 1) (args !! 2)
    else
        putStrLn $ "Error: Excpected 1st arguemnt as path to L-system definition"
            ++ ", 2nd argument as path to interpret definition"
            ++ " and 3rd argument as path to interpret commands."


-- | Loads L-sysytem definition and interpret definition from files and starts processisng stdIn.
loadAndInterpretLSystem :: FilePath -> FilePath -> FilePath -> IO ()
loadAndInterpretLSystem  lsysFilePath iDefFilePath iCmdsFilePath = do
    lsysDefFile <- readUtf8File lsysFilePath
    let maybeLSys = parseLSystem $ lines lsysDefFile
    if isJust maybeLSys then do
        iDefFile <- readUtf8File iDefFilePath
        let maybeIMap = parseIMap $ lines iDefFile
        if isJust maybeIMap then do
            iCmds <- readUtf8File iCmdsFilePath
            mapM_ saveStringToFile
                $ processLSystem (fromJust maybeLSys) (fromJust maybeIMap) (lines iCmds)
        else
            putStrLn "Error: Failed to load Interpret definition from file."
    else
        putStrLn "Error: Failed to load L-system from file."

-- | Saves given string to file at given path and prints info to stdOut. If given string is empty,
-- no file is saved and error is printed into console. If path equals "-", given string is printed
-- into stdOut delimited by delimiter.
saveStringToFile :: (FilePath, String) -> IO ()
saveStringToFile (fPath, str) =
    if Data.List.null str then do
        putStrLn $ ("No output (interpretation probbly filed), file \""++)
            . (fPath++) $ "\" not saved."
    else
        if dropExtension (takeFileName fPath) == "-" then do
            putStrLn outputDelimiter
            putStrLn str
            putStrLn outputDelimiter
        else do
            writeFile fPath str
            putStrLn $ ("File \""++) . (fPath++) $ "\" succesfully saved."

-- | Delimiter of output to stdOut, to be able detect output from info text or
outputDelimiter :: String
outputDelimiter = "<==============))((((o))))((==============>" -- double bladed light-saber

-- | Interpret data are: number of iterations, outputFile, input data (symbols), rest of input.
type InterpretData = (InterpretFunc, FilePath, Int, [String], [String])

-- | Reads interpret data from given input (axiom, out file path, iterations), iterates with them
-- given L-system and then interprets result by given interpret function. Raturns filePath from
-- interpret data and result string from interpret.
processLSystem :: LSystem -> InstructionMap -> [String] -> [(FilePath, String)]
processLSystem _ _ [] = []
processLSystem lSys iMap lines =
    case readInterpretData lines of
        Just (iFunc, fp, iters, axiomStrLines, rest) ->
            (fp, iFunc iMap $ interpretLSystem lSys iters axiomStrLines)
                : processLSystem lSys iMap rest
        Nothing -> processLSystem lSys iMap $ tail lines -- try start at next line


-- | Tries to read interpret data. If succeded with reading all data, returns them with rest of
-- unreaded input lines.
readInterpretData :: [String] -> Maybe InterpretData
readInterpretData lines = do
    filePathStr <- maybeHead lines
    filePath <- toMaybeCond isValid filePathStr
    -- extensions in avibleInterprets are lowercase
    let extStr = Prelude.map toLower $ takeExtension filePath
        ext = case extStr of ('.':x) -> x; x -> x
    interpret <- Data.Map.lookup ext avibleInterprets
    iStr <- maybeHead $ tail lines
    i <- parseInt iStr
    nonNegI <- toMaybeCond (>=0) i
    let (axiomStrLines, rest) = readToBlankLine $ drop 2 lines
    Just (interpret, filePath, nonNegI, axiomStrLines, rest)


----------------------------------------------------------------------------------------------------
readUtf8File :: FilePath -> IO String
readUtf8File filePath = do
    handle <- openFile filePath ReadMode
    _ <- hSetEncoding handle utf8
    hGetContents handle


















