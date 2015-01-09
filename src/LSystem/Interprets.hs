module LSystem.Interprets (
    InterpretFunc,
    InterpretFuncMapped,
    avibleInterprets,
    InterpretInstruction(..),
    InstructionMap,
    parseIMap,
    interpretLSystemRaw
) where

import Data.Map
import Data.Char
import Data.Maybe
import LSystem.Utils
import LSystem.DataTypes
import LSystem.Expressions (isTrueInDoubleLogic)
import LSystem.ShowReadUtils


----------------------------------------------------------------------------------------------------
-- | Type of interpret function.
type InterpretFunc = InstructionMap -> [SymbolDouble] -> String

-- | Type of interpret function with mapped instructions.
type InterpretFuncMapped = [SymbolDouble] -> String

-- | Map of file extensions to aviable interprets. Key (extension) is lowercase.
avibleInterprets :: Map String InterpretFunc
avibleInterprets = fromList [
        ("txt", interpretLSystemRaw),
        ("svg", interpretLSystemToSvg)
    ]




----------------------------------------------------------------------------------------------------
-- | Type for map symbol to instruction.
data InterpretInstruction =
    DoNothing |
    DrawLineForward |
    MoveForward |
    TurnLeft |
    TurnRight |
    StartBranch |
    CompleteBranch |
    SpecialInstruction String
    deriving (Show, Read, Eq, Ord)

-- type ReadS a = String -> [(a, String)]

-- | Reads InterpretInstruction from given string. If nothing was readed by default read function,
-- whole string is considered as name of special instruction (if not empty).
readJustInterpretInstruction :: ReadS InterpretInstruction
readJustInterpretInstruction str =
    if Prelude.null str then
        []
    else
        let defRead = reads str in
        if Prelude.null defRead then
            [(SpecialInstruction name, "") | (name, "") <- lex str]
        else
            defRead

type InstructionMap = Map Symbol InterpretInstruction

-- | Tries to parse instruction association to symbol from each given element of list.
-- If even one association filed to parse, nothing is returned.
parseIMap :: [String] -> Maybe InstructionMap
parseIMap strLines = parseIAssocAcc empty $ filterNonEmpty strLines

parseIAssocAcc :: InstructionMap -> [String] -> Maybe InstructionMap
parseIAssocAcc acc [] = Just acc
parseIAssocAcc acc strLines = do
    (symbol, ii) <- parseIAssoc $ head strLines
    parseIAssocAcc (insert symbol ii acc) (tail strLines)

-- | Tries to parse interpret instruction assoc to symbol.
parseIAssoc :: String -> Maybe (Symbol, InterpretInstruction)
parseIAssoc str = maybeHead [(symbol, instr) |
        (symbol, rest) <- reads str,
        (instr, "") <- readJustInterpretInstruction rest]




----------------------------------------------------------------------------------------------------
-- | Type synonim for interpret inner function which takes old iState, new iState, its state and
-- symbol and produces tuple with newest interpret state (which will be same as new iState if no
-- special instructions are recognised) and its new state.
type InterpretInnerFunc a = InstructionMap -> InterpretState -> InterpretState -> a -> SymbolDouble
    -> Maybe (InterpretState, a)

data InterpretState = InterpretState {
    is_x :: Double,
    is_y :: Double,
    is_angle :: Double,
    is_previous :: Maybe InterpretState,
    -- | min x, min y, max x, max y
    is_extremes :: (Double, Double, Double, Double)
}

-- | Default instance of InterpretState (everything zero).
emptyIState :: InterpretState
emptyIState = InterpretState 0 0 0 Nothing (0, 0, 0, 0)

-- | Main interpret loop, counts new iState and gives all data to user func.
interpretSymbols :: InstructionMap -> (InterpretState, a) -> InterpretInnerFunc a-> [SymbolDouble]
    -> Maybe (InterpretState, a)
interpretSymbols _ oldState _ [] = Just oldState
interpretSymbols iMap (oldIState, oldCustState) interpretFunc (s:ss) = do
    newIState <- interpretBasicInstr iMap oldIState s
    newState <- interpretFunc iMap oldIState newIState oldCustState s
    interpretSymbols iMap newState interpretFunc ss

-- | Interprets known instructions in common way. Real interprets do not have to do this dirty job.
interpretBasicInstr :: InstructionMap -> InterpretState -> SymbolDouble -> Maybe InterpretState
interpretBasicInstr iMap is (SymbolDouble symbol params) =
    case Data.Map.lookup symbol iMap of
        Just instruction ->
            let InterpretState x y angle prevState ex = is
                paramsLen = length params in
            case instruction of
                DrawLineForward ->
                    if paramsLen >= 1 then
                        let newX = x + cos (degToRad angle) * head params
                            newY = y + sin (degToRad angle) * head params in
                        Just $ InterpretState newX newY angle prevState $ countExtremes newX newY ex
                    else
                        Nothing

                MoveForward ->
                    if paramsLen >= 1 then
                        let newX = x + cos (degToRad angle) * head params
                            newY = y + sin (degToRad angle) * head params in
                        Just $ InterpretState newX newY angle prevState $ countExtremes newX newY ex
                    else
                        Nothing

                TurnLeft ->
                    if paramsLen >= 1 then
                        Just $ InterpretState x y (angle + head params) prevState ex
                    else
                        Nothing

                TurnRight ->
                    if paramsLen >= 1 then
                        Just $ InterpretState x y (angle - head params) prevState ex
                    else
                        Nothing

                StartBranch -> Just $ InterpretState x y angle (Just is) ex

                CompleteBranch ->
                    if isJust prevState then
                        let InterpretState px py pa ps _= fromJust prevState in
                        Just $ InterpretState px py pa ps ex -- copy new extrems to old instance
                    else
                        Nothing

                _ -> Just is
        Nothing -> Just is

-- | Takes new X and new Y with extremes and counts new extremes.
countExtremes :: Double -> Double -> (Double, Double, Double, Double)
    -> (Double, Double, Double, Double)
countExtremes x y (minX, minY, maxX, maxY) = (min minX x, min minY y, max maxX x, max maxY y)


----------------------------------------------------------------------------------------------------
-- | The most primitive L-system interpret, it just convert given symbols to string.
interpretLSystemRaw :: InterpretFunc
interpretLSystemRaw _ [] = ""
interpretLSystemRaw iMap (s:ss) = show s ++ interpretLSystemRaw iMap ss



----------------------------------------------------------------------------------------------------
-- | Interprets given symbols with respect to given instrunction mapping as SVG image.
interpretLSystemToSvg :: InterpretFunc
interpretLSystemToSvg _ [] = ""
interpretLSystemToSvg iMap ss =
    case interpretSymbols iMap (emptyIState, emptySvgState) svgInterpretFunc ss of
        Just (iState, svgState) -> getSvgData iState $ closeSvgPolyline svgState
        Nothing -> []

svgInterpretFunc :: InterpretInnerFunc SvgState
svgInterpretFunc iMap oldIState newIState svgState (SymbolDouble symbol params) =
    case Data.Map.lookup symbol iMap of
        Just instruction ->
            let InterpretState oldX oldY _ _ _ = oldIState
                InterpretState newX newY _ _ _ = newIState in
            case instruction of
                DrawLineForward ->
                    let newSvgState =
                            if isLineOpened svgState then
                                svgState
                            else
                                addPointToSvgPolyline svgState (oldX, oldY)
                        in
                    Just (newIState, addPointToSvgPolyline newSvgState (newX, newY))
                MoveForward -> Just (newIState, closeSvgPolyline svgState)
                CompleteBranch -> Just (newIState, closeSvgPolyline svgState)
                SpecialInstruction instr -> svgInterpretSpecial newIState svgState params instr
                _ -> Just (newIState, svgState) -- unhandled instructions, do nothing by default

        Nothing -> Just (newIState, svgState) -- unknown symbol, do nothing

-- | Interprets svg special instruction.
svgInterpretSpecial :: InterpretState -> SvgState -> [Double] -> String
    -> Maybe (InterpretState, SvgState)
svgInterpretSpecial iState svgState params instr =
    let InterpretState x y _ _ _ = iState in
    case instr of
        "StartPolyline" -> Just (iState, openSpecSvgPolyline svgState)
        "RecordPolylineVertex" -> Just (iState, addSpecPointToSvgPolyline svgState (x, y))
        "EndPolyline" -> do
            newSvgState <- closeSpecSvgPolyline svgState
                $ checkNthParam isTrueInDoubleLogic 1 params
            Just (iState, newSvgState)
        _ -> Just (iState, svgState) -- unknown instruction, do nothing

-- | Calls given function on n-th param. If count of params is less than n, False is returned.
checkNthParam :: (Double -> Bool) -> Int -> [Double] -> Bool
checkNthParam func n params =
    let ps = take n params in
    if length ps == n then
        func $ last ps
    else
        False

-- | State of SVG interpret.
data SvgState = SvgState {
    svgs_finishedLines :: [PolylineD],
    svgs_openedLine :: PolylineD,
    svgs_openedLineSpec :: PolylineD,
    svgs_lineSpecStack :: [PolylineD]
}

type PointD = (Double, Double)
type PolylineD = [PointD]

-- | Empty instance of SvgState.
emptySvgState :: SvgState
emptySvgState = SvgState [] [] [] []

isLineOpened :: SvgState -> Bool
isLineOpened (SvgState {svgs_openedLine = opened}) = not $ Prelude.null opened

addPointToSvgPolyline :: SvgState -> PointD -> SvgState
addPointToSvgPolyline (SvgState finLines curLine specLine specStack) p =
    SvgState finLines (p:curLine) specLine specStack

-- | Closes polyline only if it is opened (can be called on state with alredy closed polyline).
closeSvgPolyline :: SvgState -> SvgState
closeSvgPolyline (SvgState finLines curLine specLine specStack) =
    if Prelude.null curLine then
        SvgState finLines [] specLine specStack
    else
        SvgState (curLine:finLines) [] specLine specStack

openSpecSvgPolyline :: SvgState -> SvgState
openSpecSvgPolyline (SvgState finLines curLine specLine specStack) =
    SvgState finLines curLine [] (specLine:specStack)

addSpecPointToSvgPolyline :: SvgState -> PointD -> SvgState
addSpecPointToSvgPolyline (SvgState finLines curLine specLine specStack) p =
    SvgState finLines curLine (p:specLine) specStack

-- | Ends current special polyline and if second parametr is True, polyline is also closed (first
-- and last points are connected).
closeSpecSvgPolyline :: SvgState -> Bool -> Maybe SvgState
closeSpecSvgPolyline (SvgState finLines curLine specLine specStack) close =
    if Prelude.null specStack then
        Nothing
    else
        if Prelude.null specLine then
            Just $ SvgState finLines curLine (head specStack) (tail specStack)
        else
            let newLine = if close then last specLine : specLine else specLine in
            Just $ SvgState (newLine:finLines) curLine (head specStack) (tail specStack)


getSvgData :: InterpretState -> SvgState -> String
getSvgData _ (SvgState [] _ _ _) = ""
getSvgData (InterpretState _ _ _ _ extremes) (SvgState finLines _ _ _) =
    showsSvgHeader
        . showsBeginOfSvg extremes
        . showsCustAll showsSvgPoliline (endOfSvg++) finLines
        $ ""

showsSvgPoliline :: PolylineD -> ShowS
showsSvgPoliline line =
    ("<polyline points=\""++) . foldr (\ pt acc -> showsPointPair pt . acc) ("\" />\n"++) line

showsPointPair :: PointD -> ShowS
showsPointPair (x, y) = shows (round2 x) . (',':) . shows (round2 y) . (' ':)

-- | Header of SVG file (DOCTYPE, ...)
showsSvgHeader :: ShowS
showsSvgHeader = ("<?xml version=\"1.0\" standalone=\"no\"?>\n"++)
    . ("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""++)
    . (" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n"++)

-- | Takes extremes and returns SVG start tag with viewBox attribute.
showsBeginOfSvg :: (Double, Double, Double, Double) -> ShowS
showsBeginOfSvg (minX, minY, maxX, maxY) = ("<svg viewBox=\""++)
    . shows (round2 $ minX - 1) . (' ':) . shows (round2 $ minY - 1) . (' ':)
    . shows (round2 $ maxX - minX + 2) . (' ':) . shows (round2 $ maxY - minY + 2)
    . ("\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n"++)
    . ("<g fill=\"none\" stroke=\"#000000\" stroke-width=\"2\" stroke-linejoin=\"bevel\">\n"++)

-- | SVG end tag
endOfSvg :: String
endOfSvg = "</g>\n</svg>"































