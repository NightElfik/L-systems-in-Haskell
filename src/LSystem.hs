module LSystem (
    LSystem(..),
    interpretLSystem,
    parseLSystem,
    parseAndEvalSymbols
) where

import Data.List
import Data.Maybe
import LSystem.Utils
import LSystem.DataTypes
import LSystem.Expressions
import LSystem.ShowReadUtils



----------------------------------------------------------------------------------------------------
iterateLSystem :: Int -> LSystem -> [SymbolDouble] -> [SymbolDouble]
iterateLSystem 0 _ axiom = axiom
iterateLSystem i lsys@(LSystem rrs) axiom =  
    let xs = rewriteSymbols rrs axiom
    in iterateLSystem (i-1) lsys xs


rewriteSymbols :: [RewriteRule] -> [SymbolDouble] -> [SymbolDouble]
rewriteSymbols rrs ss = foldr rewrite [] ss where
    rewrite s acc = (rewriteSymbol rrs s) ++ acc

-- | Rewrites given symbol with appropriate rewrite rule from given list.
rewriteSymbol :: [RewriteRule] -> SymbolDouble -> [SymbolDouble]
rewriteSymbol rrs s@(SymbolDouble _ params) =
    case pickRr (findRrsForSymbol s rrs) of
        Just (RewriteRule _ names _  replac) -> evalSymbolsVar (zip names params) replac
        Nothing -> [s]

-- | Picks randomly one rewrite rule from given list. Randomness is based on probability of each
-- rewrite rule. Returns Nothing if input list is empty.
-- TODO: implement stochastism
pickRr :: [RewriteRule] -> Maybe RewriteRule
pickRr [] = Nothing
pickRr rrs = Just (head rrs) -- TODO 

-- | Finds all rewrite rules from given list which can rewrite given symbol. Filtres by symbol and
-- condition.
findRrsForSymbol :: SymbolDouble -> [RewriteRule] -> [RewriteRule]
findRrsForSymbol (SymbolDouble symbol params) rrs =
    let symbolMatching = filter (\ rr -> getRrPattern rr == symbol ) rrs in
    filter checkRrCondition symbolMatching where
        checkRrCondition :: RewriteRule -> Bool
        checkRrCondition (RewriteRule _ names cond _) =
            if isPeEmpty cond then
                True
            else
                let vs = zip names params in
                case evalPeVarAsBool vs cond of
                    Just b -> b
                    Nothing -> False

parseRewriteRule :: String -> Maybe RewriteRule
parseRewriteRule = parse readsRewriteRule

parseAndEvalSymbols :: [String] -> Maybe [SymbolDouble]
parseAndEvalSymbols [] = Just []
parseAndEvalSymbols ([]:ss) = parseAndEvalSymbols ss
parseAndEvalSymbols (s:ss) = do
    symbols <- parse readsSymbolsExprs s
    sDouble <- parseAndEvalSymbols ss
    Just $ evalSymbols symbols ++ sDouble

----------------------------------------------------------------------------------------------------
-- | L-system is list of rewrite rules.
newtype LSystem = LSystem [RewriteRule]
    deriving (Show)


-- | Interprets given string of symbols by given L-system.
interpretLSystem :: LSystem -> Int -> [String] -> [SymbolDouble]
interpretLSystem lsys iters lines =
    case parseAndEvalSymbols lines of
        Just a -> iterateLSystem iters lsys a
        Nothing -> []

parseLSystem :: [String] -> Maybe LSystem
parseLSystem lines =
    let maybeRrs = map parseRewriteRule $ filterNonEmpty lines in
    if all isJust maybeRrs then
        Just $ LSystem $ map fromJust maybeRrs
    else
        Nothing

























