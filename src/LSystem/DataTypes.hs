module LSystem.DataTypes (
    Symbol(..),
    ParameterDouble(..),
    ParameterExpr(..),
    SymbolDouble(..),
    SymbolExpr(..),
    readsSymbolExpr,
    readsSymbolsExprs,
    evalSymbol,
    evalSymbols,
    evalSymbolVar,
    evalSymbolsVar,
    RewriteRule(..),
    readsRewriteRule,
    getRrPattern
) where

import Data.Char
import Data.List
import LSystem.Expressions
import LSystem.ShowReadUtils
import LSystem.GeneralDataTypes


----------------------------------------------------------------------------------------------------
-- | Type definition for L-system symbol. Symbol can be any char with some exceptions, which are
-- listed in nonSymbolChars list.
newtype Symbol = Symbol Char
    deriving (Eq, Ord)

instance Show Symbol where
    showsPrec _ (Symbol c) = (c:)

instance Read Symbol where
    readsPrec _ = readsSymbol

readsSymbol :: ReadS Symbol
readsSymbol str =
    let l = myLex str in
    if null l then
        []
    else
        [(Symbol c, cx ++ rest) | (c:cx, rest) <- l, notElem c nonSymbolChars]

-- | List of chars which can not be symbols.
nonSymbolChars :: [Char]
nonSymbolChars = ['(', ')']




----------------------------------------------------------------------------------------------------
-- | Parameter of L-system symbol which is represented by postfix Double number
type ParameterDouble = Double




----------------------------------------------------------------------------------------------------
-- | Parameter of L-system symbol which is represented by postfix expression.
type ParameterExpr = PostfixExpression

showsParamsExpr :: [ParameterExpr] -> ShowS
showsParamsExpr = showsCustomCommaList showsPeAsInfix

readsParamExpr :: ReadS ParameterExpr
readsParamExpr = readsPeFromInfix

-- | Reads comma separated list of parameters as infix expressions . Returns [] if failed.
readsParamsExpr :: ReadS [ParameterExpr]
readsParamsExpr = readsCommaList readsPeFromInfix




----------------------------------------------------------------------------------------------------
-- | L-system symbol with parameters as Doubles.
data SymbolDouble = SymbolDouble {
    sf_symbol :: Symbol,
    sf_params :: [Double]
}

instance Show SymbolDouble where
    showsPrec _ = showsSymbolDouble

showsSymbolDouble :: SymbolDouble -> ShowS
showsSymbolDouble (SymbolDouble s []) = shows s
showsSymbolDouble (SymbolDouble s fs) = shows s . showParen True (showsCommaList fs)




----------------------------------------------------------------------------------------------------
-- | L-system symbol with parameters as postfix expressions.
data SymbolExpr = SymbolExpr {
    se_symbol :: Symbol,
    se_params :: [ParameterExpr]
}

instance Show SymbolExpr where
    showsPrec _ = showsSymbolExpr

instance Read SymbolExpr where
    readsPrec _ = readsSymbolExpr

getSeSymbol :: SymbolExpr -> Symbol
getSeSymbol (SymbolExpr {se_symbol = s}) = s

showsSymbolExpr :: SymbolExpr -> ShowS
showsSymbolExpr (SymbolExpr s []) = shows s
showsSymbolExpr (SymbolExpr s ps) = shows s . showParen True (showsParamsExpr ps)

-- | Reads one symbol with postfix expression params. Returns [] if failed.
readsSymbolExpr :: ReadS SymbolExpr
readsSymbolExpr str =
    let sp = [(SymbolExpr s ps, rest) |
            (s, r) <- readsSymbol str,
            (ps, rest) <- readParen True readsParamsExpr r] in
    if null sp then
        [(SymbolExpr s [], rest) | (s, rest) <- readsSymbol str]
    else
        sp

-- | Reads all symbols with postfix expression params. Returns [] if failed.
readsSymbolsExprs :: ReadS [SymbolExpr]
readsSymbolsExprs = readAll readsSymbolExpr

evalSymbol :: SymbolExpr -> SymbolDouble
evalSymbol = evalSymbolVar []

evalSymbols :: [SymbolExpr] -> [SymbolDouble]
evalSymbols = evalSymbolsVar []

evalSymbolVar :: [VariableValue] -> SymbolExpr -> SymbolDouble
evalSymbolVar vs (SymbolExpr s es) = SymbolDouble s (evalPeListVarNan vs es)

evalSymbolsVar :: [VariableValue] -> [SymbolExpr] -> [SymbolDouble]
evalSymbolsVar vs ss = map (evalSymbolVar vs) ss


----------------------------------------------------------------------------------------------------
-- | Rewrite rule of L-system.
data RewriteRule = RewriteRule {
    rr_pattern :: Symbol,
    rr_paramNames :: [Name],
    rr_condition :: PostfixExpression,
    rr_replacment :: [SymbolExpr]
}

instance Show RewriteRule where
    showsPrec _ = showsRewriteRule

instance Read RewriteRule where
    readsPrec _ = readsRewriteRule

getRrPattern :: RewriteRule -> Symbol
getRrPattern (RewriteRule {rr_pattern = p}) = p

showsRewriteRule :: RewriteRule -> ShowS
showsRewriteRule (RewriteRule pat names cond replacs) =
    shows pat . showsRrParamNames names . showsRrCondition cond . (" -> "++) . showAll replacs


showsRrParamNames :: [Name] -> ShowS
showsRrParamNames ns =
    if null ns then
        id
    else
        showParen True (showsCommaList ns)

showsRrCondition :: PostfixExpression -> ShowS
showsRrCondition (p@(PostfixExpression ts)) =
    if null ts then
        id
    else
        (" ?"++) . showParen True (showsPeAsInfix p)

-- | Reads one RewriteRule. Returns [] if failed.
readsRewriteRule :: ReadS RewriteRule
readsRewriteRule str =
    [(RewriteRule s ns c rs, rest) |
        (s, r1) <- readsSymbol str,
        (ns, r2) <- readListSafe readsRrParamNames r1,
        (c, r3) <- readSafeCustom emptyPe readsRrCondition r2,
        ("->", r4) <- myLex r3,
        (rs, rest) <- readAllSafe readsSymbolExpr r4]

readsRrParamNames :: ReadS [Name]
readsRrParamNames = readParen True readsNames

readsRrCondition :: ReadS PostfixExpression
readsRrCondition str = [(e, rest) |
        ("?", r) <- myLex str,
        (e, rest) <- readParen True readsPeFromInfix r]
































