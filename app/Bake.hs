module Main where

import Parser
import Deps
import Simpl

import qualified Data.Map as M

import Text.Megaparsec (parseErrorPretty)

import System.IO
import System.Environment
import System.Directory
import System.Process
import System.Exit

import Control.Monad
import Control.Monad.Except

-- Error monad that contains the error and the code.
type CmdMonad = ExceptT (String, ExitCode) IO

type DepMap = M.Map BuildStep [String]
type CmdMap = M.Map BuildStep [String]

deps :: BakeProgram -> DepMap
deps prog = M.fromList [(n, ds) | Build n _ ds _ _ <- prog]

cmds :: BakeProgram -> CmdMap
cmds prog = M.fromList [(n, cs) | Build n _ _ _ cs <- prog]

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
