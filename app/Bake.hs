{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Parser
import Deps
import qualified Data.Map as M
import Data.List

import Text.Regex.PCRE.Heavy (sub, gsub, re)

import Text.Megaparsec (parseMaybe, parseErrorPretty)

import System.IO
import System.Environment
import System.Directory
import System.Process
import System.Exit

import Control.Monad
import Control.Monad.Except

-- Error monad that contains the error and the code.
type CmdMonad = ExceptT (String, ExitCode) IO

type CTable = [(String, String)]
type RTable = [(String, ([String], String))] -- Rule table. Name => (params, body)
type DepMap = M.Map BuildStep [String]
type CmdMap = M.Map BuildStep [String]

deps :: BakeProgram -> DepMap
deps prog = M.fromList [(n, ds) | Build n _ ds _ _ <- prog]

cmds :: BakeProgram -> CmdMap
cmds prog = M.fromList [(n, cs) | Build n _ _ _ cs <- prog]
--
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

-- Executes one command. Can fail.
execCmds :: String -> [String] -> CmdMonad ()
execCmds _ [] = return ()
execCmds name (cmd:cmds) = do
    liftIO $ putStrLn $ "Step '" ++ name ++ "' running: " ++ cmd
    handle <- liftIO $ runCommand cmd
    code <- liftIO $ waitForProcess handle
    case code of
      ExitSuccess -> do
          execCmds name cmds
      ExitFailure n -> do
          throwError $ ( "Step '" ++ name ++ "' failed with exit code: " ++ (show n)
                       , code
                       )


-- Executes an entire build step. Checks that the dependencies exist as well.
execStep :: DepMap -> CmdMap -> BuildStep -> CmdMonad ()
execStep dm cmds name =
    let deps = case M.lookup name dm of
                 Nothing -> []
                 Just ds -> ds
    in do
        b <- liftIO $ mapM doesFileExist deps
        if not $ all id b
           then throwError $ ( "Step '" ++ name ++ "' failed, dependencies not found: " ++ show deps
                             , ExitFailure 1
                             )
                             else execCmds name (cmds M.! name)


-- Executes a sequence of build steps. Stops if one fails.
execPlan :: DepMap -> CmdMap -> [BuildStep] -> IO ()
execPlan _ _ [] = return ()
execPlan ds cmds (step:steps) = do
    res <- runExceptT (execStep ds cmds step)
    case res of
      Left (e, c) -> putStrLn e >> exitWith c
      Right () -> execPlan ds cmds steps


-- Executes the entire bakefile.
execBakefile :: BakeProgram -> Maybe BuildStep -> IO ()
execBakefile prog target =
    let prog' = simplify prog
        plan = buildPlan prog' target
     in (execPlan (deps prog') (cmds prog')) plan

main :: IO ()
main = do
    args <- getArgs
    let (bakefile, target) = case args of
                                [] -> ("Bakefile", Nothing)
                                [bakefile] -> (bakefile, Nothing)
                                (bakefile:target:_) -> (bakefile, Just target)
    prog <- parseFromFile parser bakefile
    case prog of
      Left e -> do
          putStrLn $ parseErrorPretty e
          exitWith (ExitFailure 2)
      Right p -> do
          execBakefile p target
