module Deps
    ( buildPlan
    ) where

import Parser

import qualified Data.Graph as G
import qualified Data.Tree as T
import qualified Data.Map as M
import Data.List

-- Uses Data.Graph to build a dependency graph
-- It also returns lookup functions from vertices
-- to the actual buildsteps.
depGraph :: BakeProgram ->
    (G.Graph, G.Vertex -> (BuildStep, BuildStep, [BuildStep])
    , BuildStep -> Maybe G.Vertex)
depGraph bs =
    let names = foldr (\(k,v) -> M.insertWith (++) k [v])
                      M.empty
                      [(t, n) | Build n ts _ _ _ <- bs,
                                t <- ts]
        f = map (\d -> case M.lookup d names of
                         Nothing -> []
                         Just l -> l)
        nodes = [(n, n, concat (f ds)) | Build n _ ds _ _ <- bs]
     in G.graphFromEdges nodes

-- Uses the graph library to check for cycles.
hasCycles :: G.Graph -> Bool
hasCycles g = any id [length sf /= 0 | T.Node _ sf <- G.scc g]

-- Calculates the correct order of dependencies, and
-- makes a plan for the build.
buildPlan :: BakeProgram -> Maybe BuildStep -> Maybe [BuildStep]
buildPlan bs target =
    let (g, f1, f2) = depGraph bs
        sorted = reverse $ G.topSort g
        goal = case target of
                 Nothing -> sorted
                 Just t ->
                     case f2 t of
                       Just vertex -> G.reachable g vertex
                       Nothing -> sorted
     in if hasCycles g
          then Nothing
          else Just $ map ((\(a,b,c) -> a) . f1) (sorted `intersect` goal)
