{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Simpl
    ( simplify
    ) where

import Parser

import Data.List
import Text.Megaparsec (parseMaybe)
import Text.Regex.PCRE.Heavy (sub, gsub, re)

type CTable = [(String, String)]
type RTable = [(String, ([String], String))] -- Rule table. Name => (params, body)

-- Creates a constants table containing the global constants
cTable :: BakeProgram -> CTable
cTable prog = [(name, val) | Constant name val <- prog]

-- Creates a rule table containing all the rules of the bakefile
ruleTable :: BakeProgram -> RTable
ruleTable prog = [(name, (consts, cmds)) | Rule name consts cmds <- prog]


-- Substitutes constants into the body of a build
substConst :: CTable -> BakeItem -> BakeItem
substConst ct (Build n ts ds False cs) =
    Build n ts ds False (map f cs) where
        input = intercalate " " ds
        output = intercalate " " ts
        ct' = ("output", output):("input", input):ct
        f = gsub [re|@(\w+)|] (\(_:m) ->
            (case lookup m ct' of
               Just s -> s
               Nothing -> "@" ++ m))

-- A bit of a hack for expanding the forall modifier.
substConst ct (Build n ts ds True cs) =
    Build n ts ds False (concat $ map f (zip ts ds)) where
        f (t, d) = let ct' = ("output", t):("input", d):ct
                       f' = gsub [re|@(\w+)|] (\(_:m) ->
                        (case lookup m ct' of
                           Just s -> s
                           Nothing -> "@" ++ m))
                    in map f' cs

-- Substitutes constants into the body of a rule
-- It is hygienic, which means that parameters shadow
-- whatever is in the constants table.
substConst ct (Rule name consts c) =
    Rule name consts (f c) where
        f = gsub  [re|@(\w+)|] (\(_:m) ->
            if m `elem` consts
               then "@" ++ m
               else (case lookup m ct of
                       Just s -> s
                       Nothing -> "@" ++ m))

-- Constants do not contain Constants.
substConst vt const = const

-- Substitution used in rule expansion
substConstsInBody :: CTable -> String -> String
substConstsInBody vt cmd =
        gsub [re|@(\w+)|] (\(_:m) ->
            (case lookup m vt of
               Just s -> s
               Nothing -> "@" ++ m)) cmd

-- Expands the rule applications. Creates a constants table
-- for the formal parameters to the rule.
expandRule :: RTable -> String -> String
expandRule rt s =
    case parseMaybe parseCall s of
        Nothing -> s
        Just (name, vars) ->
            case lookup name rt of
              Nothing -> s
              Just (vars', cmd) ->
                  substConstsInBody (zip vars' vars) cmd


-- Expands all rule applications in a build.
expandRulesInBuild :: RTable -> BakeItem -> BakeItem
expandRulesInBuild rt (Build n t ds forall cs) =
    Build n t ds forall (map f cs) where
        f = gsub [re|@\w+\(.+\)|]
            (\g -> expandRule rt g)

expandRulesInBuild rt other = other


-- Substitutes all global variables.
removeConsts :: BakeProgram -> BakeProgram
removeConsts prog =
    let fs = substConst $ cTable prog
     in map fs prog

-- Substitutes variables and expands rules.
simplify :: BakeProgram -> BakeProgram
simplify prog =
    let prog' = removeConsts prog
        fs = expandRulesInBuild $ ruleTable prog'
     in map fs prog'
