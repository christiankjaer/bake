{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Lib
import qualified Data.Graph as G
import qualified Data.Map as M
import Data.List as L

import Text.Regex.PCRE.Heavy (sub, gsub, re)

import Text.Megaparsec (parseMaybe, parseErrorPretty)

import System.IO
import System.Environment
import System.Directory
import System.Process
import System.Exit

import Control.Monad
import Control.Monad.Except

type CmdMonad = ExceptT String IO

type VTable = [(String, String)]
type RTable = [(String, ([String], String))] -- Rule table. Name => (params, body)
type BuildStep = (String, [String], [String]) -- Name, dependencies, cmds

targets :: BakeProgram -> [String]
targets t =
    let ts = [targets | Build n targets d c <- t]
     in nub (concat ts)

depGraph :: [BakeItem] -> (G.Graph, G.Vertex ->
    ((String, [String]), String, [String]),
                          String -> Maybe G.Vertex)
depGraph bs =
    let nodes = [[((n, c), t, ds) | t <- ts] | Build n ts ds c <- bs]
     in G.graphFromEdges (nub . concat $ nodes)

-- Creates a variable table containing the global variables.
vTable :: BakeProgram -> VTable
vTable prog = [(name, val) | Variable name val <- prog]

-- Creates a rule table containing all the rules of the bakefile
ruleTable :: BakeProgram -> RTable
ruleTable prog = [(name, (vars, cmds)) | Rule name vars cmds <- prog]

-- Substitutes variables into the body of a build
substVar :: VTable -> BakeItem -> BakeItem
substVar vt (Build n t ds cs) =
    Build n t ds (map f cs) where
        input = intercalate " " ds
        output = intercalate " " t
        vt' = ("output", output):("input", input):vt
        f = gsub [re|@(\w+)|] (\(_:m) ->
            (case lookup m vt' of
               Just s -> s
               Nothing -> "@" ++ m))

-- Substitutes variables into the body of a rule
-- It is hygienic, which means that parameters shadow
-- whatever is in the variable table.
substVar vt (Rule name vars c) =
    Rule name vars (f c) where
        f = gsub  [re|@(\w+)|] (\(_:m) ->
            if m `elem` vars
               then "@" ++ m
               else (case lookup m vt of
                       Just s -> s
                       Nothing -> "@" ++ m))

-- Variables does not contain variables.
substVar vt var = var

-- Substitution used in rule expansion
substVarsInBody :: VTable -> String -> String
substVarsInBody vt cmd =
        gsub [re|@(\w+)|] (\(_:m) ->
            (case lookup m vt of
               Just s -> s
               Nothing -> "@" ++ m)) cmd

-- Expands the rule applications. Creates a variable table
-- for the formal parameters to the rule.
expandRule :: RTable -> String -> String
expandRule rt s =
    case parseMaybe parseCall s of
        Nothing -> s
        Just (name, vars) ->
            case lookup name rt of
              Nothing -> s
              Just (vars', cmd) ->
                  substVarsInBody (zip vars' vars) cmd


-- Expands all rule applications in a build.
expandRulesInBuild :: RTable -> BakeItem -> BakeItem
expandRulesInBuild rt (Build n t ds cs) =
    Build n t ds (map f cs) where
        f = gsub [re|@\w+\(.+\)|]
            (\g -> expandRule rt g)

expandRulesInBuild rt other = other


-- Substitutes all global variables.
removeVars :: BakeProgram -> BakeProgram
removeVars prog =
    let fs = substVar $ vTable prog
     in map fs prog

-- Substitutes variables and expands rules.
simplify :: BakeProgram -> BakeProgram
simplify prog =
    let prog' = removeVars prog
        fs = expandRulesInBuild $ ruleTable prog'
     in map fs prog'


-- Calculates the correct order of dependencies, and
-- makes a plan for the build.
buildPlan :: BakeProgram -> [BuildStep]
buildPlan bs =
    let (g, f1, f2) = depGraph bs
        sorted = reverse $ G.topSort g
     in map ((\((a, a'),b,c) -> (a, a', c)) . f1) sorted

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
          throwError $ "Step '" ++ name ++ "' failed with exit code: " ++ (show n)


-- Executes an entire build step. Checks that the dependencies exist as well.
execStep :: BuildStep -> CmdMonad ()
execStep (name, cmds, deps) = do
    b <- liftIO $ mapM doesFileExist deps
    if not $ all id b
       then throwError $ "Step '" ++ name ++ "' failed, dependencies not found: " ++ show deps
       else execCmds name cmds


-- Executes a sequence of build steps. Stops if one fails.
execPlan :: [BuildStep] -> IO ()
execPlan [] = return ()
execPlan (step:steps) = do
    res <- runExceptT (execStep step)
    case res of
      Left e -> putStrLn e >> exitWith (ExitFailure 1)
      Right () -> execPlan steps


-- Executes the entire bakefile.
execBakefile :: BakeProgram -> IO ()
execBakefile prog = execPlan . buildPlan . simplify $ prog


main :: IO ()
main = do
    (bakefile : rest) <- getArgs
    prog <- parseFromFile parser bakefile
    case prog of
      Left e -> do
          putStrLn $ parseErrorPretty e
          exitWith (ExitFailure 2)
      Right p -> do
          execBakefile p
