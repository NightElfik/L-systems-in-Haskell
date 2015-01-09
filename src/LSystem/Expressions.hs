module LSystem.Expressions (
    Operator(..),
    getOpSyntax,
    getOpPriotiry,
    getOpActivePriotiry,
    allOperators,
    operators,
    allUnaryOperators,
    unaryOperatorsSpec,
    unaryOperators,
    allBinaryOperators,
    binaryOperatorsSpec,
    binaryOperators,
    doubleCmpToDoubleLogic,
    boolCmpToDoubleLogic,
    isTrueInDoubleLogic,
    readsOperator,
    isLeftParenthesis,
    isRightParenthesis,
    Term(..),
    readsTerm,
    VariableValue(..),
    evalVariable,
    PostfixExpression(..),
    emptyPe,
    isPeEmpty,
    peFromNumber,
    showsPeAsInfix,
    readsPostfixExpression,
    readsPeFromInfix,
    evalPe,
    evalPeAsBool,
    evalPeVarAsBool,
    evalPeVar,
    evalPeNan,
    evalPeVarNan,
    evalPeListNan,
    evalPeListVarNan
) where

import Data.List
import Data.Maybe
import LSystem.ShowReadUtils
import LSystem.GeneralDataTypes


----------------------------------------------------------------------------------------------------
-- | Arithmetic operator.
data Operator =
    UnaryOperator {
        uo_syntax :: String,
        uo_priority :: Int,
        uo_activePriority :: Int,
        uo_evalFunc :: Double -> Double
    } |
    BinaryOperator {
        bo_syntax :: String,
        bo_priority :: Int,
        bo_activePriority :: Int,
        bo_evalFunc :: Double -> Double -> Double
    } |
    SpecialOperator {
        so_syntax :: String,
        so_priority :: Int,
        so_activePriority :: Int
    }

instance Show Operator where
    showsPrec _ = showsOperator

instance Eq Operator where
   (==) = cmpOpPriority (==)
   (/=) = cmpOpPriority (/=)

instance Ord Operator where
   (<) = cmpOpPriority (<)
   (<=) = cmpOpPriority (<=)
   (>) = cmpOpPriority (>)
   (>=) = cmpOpPriority (>=)

getOpSyntax :: Operator -> String
getOpSyntax o =
    case o of
        UnaryOperator {uo_syntax = s} -> s
        BinaryOperator {bo_syntax = s} -> s
        SpecialOperator {so_syntax = s} -> s

getOpPriotiry :: Operator -> Int
getOpPriotiry o =
    case o of
        UnaryOperator {uo_priority = p} -> p
        BinaryOperator {bo_priority = p} -> p
        SpecialOperator {so_priority = p} -> p

getOpActivePriotiry :: Operator -> Int
getOpActivePriotiry o =
    case o of
        UnaryOperator {uo_activePriority = p} -> p
        BinaryOperator {bo_activePriority = p} -> p
        SpecialOperator {so_activePriority = p} -> p

-- | Compares priorities of given operators by given compare function and returns its result.
cmpOpPriority :: (Int -> Int -> a) -> Operator -> Operator -> a
cmpOpPriority cmp x y = cmp (getOpPriotiry x) (getOpPriotiry y)

-- | Returns all operators.
allOperators :: [Operator]
allOperators = allBinaryOperators ++ allUnaryOperators

-- | Returns all basic operators.
operators :: [Operator]
operators = binaryOperators ++ unaryOperators

-- | Returns all unary operators.
allUnaryOperators = unaryOperatorsSpec ++ unaryOperators

-- | Returns all special uary operators.
unaryOperatorsSpec :: [Operator]
unaryOperatorsSpec = [
    SpecialOperator "(" 99 0]

-- | Returns all basic uary operators.
unaryOperators :: [Operator]
unaryOperators = [
    UnaryOperator "√" 2 3 sqrt,
    UnaryOperator "+" 5 2 (0+),
    UnaryOperator "-" 5 2 (0-)]

-- | Returns all binary operators.
allBinaryOperators :: [Operator]
allBinaryOperators = binaryOperatorsSpec ++ binaryOperators

-- | Returns all special binary operators.
binaryOperatorsSpec :: [Operator]
binaryOperatorsSpec = [
    SpecialOperator "(" 99 0,
    SpecialOperator ")" 0 99]

-- | Returns all basic binary operators.
binaryOperators :: [Operator]
binaryOperators = [
    BinaryOperator "^" 3 3 (**),

    BinaryOperator "*" 7 8 (*),
    BinaryOperator "/" 7 8 (/),

    BinaryOperator "+" 9 10 (+),
    BinaryOperator "-" 9 10 (-),

    BinaryOperator "==" 15 16 $ doubleCmpToDoubleLogic (==),
    BinaryOperator "!=" 15 16 $ doubleCmpToDoubleLogic (/=),
    BinaryOperator "<" 15 16 $ doubleCmpToDoubleLogic (<),
    BinaryOperator "<=" 15 16 $ doubleCmpToDoubleLogic (<=),
    BinaryOperator ">" 15 16 $ doubleCmpToDoubleLogic (>),
    BinaryOperator ">=" 15 16 $ doubleCmpToDoubleLogic (>=),

    BinaryOperator "||" 17 18 $ boolCmpToDoubleLogic (||),
    BinaryOperator "&&" 17 18 $ boolCmpToDoubleLogic (&&)]

-- | Converts function which takes two Doubles and returns bool to function which returns Double.
doubleCmpToDoubleLogic :: (Double -> Double -> Bool) -> Double -> Double -> Double
doubleCmpToDoubleLogic d x y = if d x y then 1.0 else 0.0

isTrueInDoubleLogic :: Double -> Bool
isTrueInDoubleLogic d = abs d > 0.0001

-- | Converts function which takes two bools and returns bool to function
-- which takes and returns Doubles.
boolCmpToDoubleLogic :: (Bool -> Bool -> Bool) -> Double -> Double -> Double
boolCmpToDoubleLogic f x y = if f (x /= 0.0) (y /= 0.0) then 1.0 else 0.0

showsOperator :: Operator -> ShowS
showsOperator o = ((getOpSyntax o)++)

readsOperator :: [Operator] -> ReadS Operator
readsOperator ops str = [(o, rest) | (opStr, rest) <- myLex str, Just o <- [findOperator ops opStr]]

-- | Tries to find operator in given list witch has same syntax as given string.
findOperator :: [Operator] -> String -> Maybe Operator
findOperator ops str = find (\ o -> getOpSyntax o == str ) ops

-- | Returns True if given operator is right parenthesis.
isLeftParenthesis :: Operator -> Bool
isLeftParenthesis o =
    case o of
        SpecialOperator {so_syntax = "("} -> True
        _ -> False

-- | Returns True if given operator is left parenthesis.
isRightParenthesis :: Operator -> Bool
isRightParenthesis o =
    case o of
        SpecialOperator {so_syntax = ")"} -> True
        _ -> False




----------------------------------------------------------------------------------------------------
-- | Term is token of expression. It can be constant number, variable or operator.
data Term
    = Constant Double
    | Variable Name
    | TOperator Operator

instance Show Term where
    showsPrec _ = showsTerm

showsTerm :: Term -> ShowS
showsTerm t =
    case t of
        Constant c -> shows c
        Variable v -> shows v
        TOperator o -> shows o

-- | Reads one term from given string. Returns all possible combintions how to parse. For example:
--
-- > readsTerm allUnaryOperators "-2" = [(-2.0,""), (-,"2")]
--
readsTerm :: [Operator] -> ReadS Term
readsTerm ops str = [(Constant f, rest) | (f, rest) <- reads str]
        ++ [(TOperator o, rest) | (o, rest) <- readsOperator ops str]
        ++ [(Variable n, rest) | (n, rest) <- readsName str]



----------------------------------------------------------------------------------------------------
-- | Variable with its Double value.
type VariableValue = (Name, Double)

evalVariable :: [VariableValue] -> Name -> Maybe Double
evalVariable [] _ = Nothing
evalVariable ((name, value):vs) n =
    if n == name then
        Just value
    else
        evalVariable vs n




----------------------------------------------------------------------------------------------------
-- | Postfix expression is list of terms. It does not have instance of Read class because of
--   disambiguity in read operation. Reading from infix notation is not disambiguous.
newtype PostfixExpression = PostfixExpression [Term]

instance Show PostfixExpression where
    showsPrec _ = showsPostfixExpression

-- | Returns empty postfix expression.
emptyPe :: PostfixExpression
emptyPe = PostfixExpression []

-- | Returns True if given postfix expression is empty.
isPeEmpty :: PostfixExpression -> Bool
isPeEmpty (PostfixExpression ts) = null ts

peFromNumber :: Double -> PostfixExpression
peFromNumber n = PostfixExpression [(Constant n)]

showsPostfixExpression :: PostfixExpression -> ShowS
showsPostfixExpression (PostfixExpression ts) = showsList " " ts

-- TODO: now just shows expression as postfix, only needed for debug reasons, user never get
-- evaluated expression
showsPeAsInfix :: PostfixExpression -> ShowS
showsPeAsInfix (PostfixExpression ts) = showsList " " ts

-- | Reads postfix expression from string representation. Same string representation of some
-- operators can be problem (eg. unary or binary minus). Binary operators have higher priority.
readsPostfixExpression :: ReadS PostfixExpression
readsPostfixExpression str =
    [(PostfixExpression ts, rest) | (ts, rest) <- readAll (readsTerm operators) str]

-- | Reads postfix expression from infix repreentation in string.
readsPeFromInfix :: ReadS PostfixExpression
readsPeFromInfix str =
    [(PostfixExpression ((reverse ps) ++ [TOperator o | o <- os]), rest) |
        ((ps, os), rest) <- readsPeOpeandFromInfix [] [] str,
        isNothing (findOperator os "(")]

-- | Starts to read postfix expression with operand (or unary operator). Returns [] if failed.
readsPeOpeandFromInfix :: [Term] -> [Operator] -> ReadS ([Term], [Operator])
readsPeOpeandFromInfix postfix opStack str =
    let t = readsTerm allUnaryOperators str in
    if null t then
        []
    else
        [x | (term, rest) <- t, x <- processReadedOperand postfix opStack term rest]

-- | Process given unary term and tries to parse rest of given string.
processReadedOperand :: [Term] -> [Operator] -> Term -> ReadS ([Term], [Operator])
processReadedOperand postfix opStack t str =
    case t of
        cc@(Constant c) -> readsPeOperatorFromInfix (cc:postfix) opStack str
        vv@(Variable v) -> readsPeOperatorFromInfix (vv:postfix) opStack str
        TOperator o ->
            let p = pushOperator postfix opStack o in
            case p of
                Just (ts, os) -> readsPeOpeandFromInfix ts os str
                _ -> [] -- failed to push operator -> fail

-- | Starts to read postfix expression with operator. Returns reminder of unreaded string even if
-- nothing is readed.
readsPeOperatorFromInfix :: [Term] -> [Operator] -> ReadS ([Term], [Operator])
readsPeOperatorFromInfix postfix opStack str =
    let os = readsOperator allBinaryOperators str in
    if null os then
        [((postfix, opStack), str)]
    else
        let p = [x |
                (o, rest) <- os,
                Just (newPostfix, newOpStack) <- [pushOperator postfix opStack o],
                x <-
                    if isRightParenthesis o then
                        readsPeOperatorFromInfix newPostfix newOpStack rest
                    else
                        readsPeOpeandFromInfix newPostfix newOpStack rest
                ] in
        if null p then
            [((postfix, opStack), str)]
        else
            p

-- | Pushes operator on operatos stack. This operation can move some operators fom operator stack
-- to postfix expression.
pushOperator :: [Term] -> [Operator] -> Operator -> Maybe ([Term], [Operator])
pushOperator postfix [] o =
    if isRightParenthesis o then
        Nothing
    else
        Just (postfix, [o])
pushOperator postfix (oStack@(oTop:oRest)) oActive =
    if (getOpActivePriotiry oActive) > (getOpPriotiry oTop) then
        pushOperator ((TOperator oTop):postfix) oRest oActive
    else
        if isRightParenthesis oActive then
            if isLeftParenthesis oTop then
                Just (postfix, oRest)
            else
                Nothing -- missing left parenthesis
        else
            Just (postfix, (oActive:oStack))

-- | Double constant of NaN (not a number)
nan :: Double
nan = 0.0 / 0.0 -- how to get nan?

-- | Tries to evaluate given postfix expression.
evalPe :: PostfixExpression -> Maybe Double
evalPe = evalPeVar []

-- | Evaluates given postfix expression. If evaluation fails, NaN value is returned.
evalPeNan :: PostfixExpression -> Double
evalPeNan = evalPeVarNan []

-- | Evaluates list of given postfix expression. NaN is used if evaluation of expression fails.
evalPeListNan :: [PostfixExpression] -> [Double]
evalPeListNan = map evalPeNan

-- | Evaluates list of given postfix expression with given list of variable values. NaN is used if
-- evaluation of expression fails.
evalPeListVarNan :: [VariableValue] -> [PostfixExpression] -> [Double]
evalPeListVarNan vs = map (evalPeVarNan vs)

-- | Tries to evaluate given postfix expression. Result is returned in bool logic (0 == False).
evalPeAsBool :: PostfixExpression -> Maybe Bool
evalPeAsBool = evalPeVarAsBool []

-- | Tries to evaluate given postfix expression with given list of variable values.
evalPeVar :: [VariableValue] -> PostfixExpression -> Maybe Double
evalPeVar _ (PostfixExpression []) = Nothing
evalPeVar vs (PostfixExpression ts) = evalPeStack vs [] ts

-- | Evaluates given postfix expression with given list of variable values. If evaluation fails,
-- NaN value is returned.
evalPeVarNan :: [VariableValue] -> PostfixExpression -> Double
evalPeVarNan _ (PostfixExpression []) = nan
evalPeVarNan vs (PostfixExpression ts) =
    case evalPeStack vs [] ts of
        Just n -> n
        _ -> nan

-- | Evaluates postfix expression with given stack of alredy evaluated numbers.
evalPeStack :: [VariableValue] -> [Double] -> [Term] -> Maybe Double
evalPeStack _ numStack [] =
    if length numStack == 1 then
        Just (head numStack)
    else
        Nothing
evalPeStack vs numStack (t:ts) =
    case t of
        Constant c -> evalPeStack vs (c:numStack) ts
        Variable v ->
            case evalVariable vs v of
                Just x -> evalPeStack vs (x:numStack) ts
                _ -> Nothing
        TOperator o ->
            case o of
                UnaryOperator {uo_evalFunc = f} ->
                    let x = take 1 numStack in
                    if length x == 1 then
                        evalPeStack vs ((f (head x)):(drop 1 numStack)) ts
                    else
                        Nothing
                BinaryOperator {bo_evalFunc = f} ->
                    let x = take 2 numStack in
                    if length x == 2 then
                        evalPeStack vs ((f (last x) (head x)):(drop 2 numStack)) ts
                    else
                        Nothing
                _ -> Nothing


-- | Tries to evaluate given postfix expression with given list of variable values. Result is
-- returned in bool logic (0 == False).
evalPeVarAsBool :: [VariableValue] -> PostfixExpression -> Maybe Bool
evalPeVarAsBool vs e = let x = evalPeVar vs e in
    case x of
        Just n -> Just (if abs n < 0.0001 then False else True)
        _-> Nothing
























