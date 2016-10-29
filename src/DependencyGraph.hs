module DependencyGraph (
  getUnused,
  Node
) where

import Control.Arrow (first)
import Data.Graph
import Data.Array (elems, indices)
import Java
import Java.Autowired (Autowiring(AutowireAll, Autowiring))
import qualified Data.Map.Strict as Map
import Config (getBlacklistedAnnotations, blacklistedClassSuffixes)
import Data.List (isSuffixOf)

type Node = (Result, String, [String])
type NodeWithInDegree = (Node, Int)

getUnused :: [Result] -> [Node]
getUnused xs = removeUsed (buildAutowiredMap xs) xs

removeUsed :: Map.Map String Autowiring -> [Result] -> [Node]
removeUsed autowiredMap = removeOnEmpty . removeBlacklisted . getWithoutInEdges
  where removeOnAnnotations = filter blacklistedAnnotations
        removeOnClassSuffix = filter removeBlacklistedClassSuffixes
        removeOnEmpty = filter nodeHasClassName
        removeOnAutowired = filter (inAutowired autowiredMap)
        transformAllToEdges = map transformToEdges
        removeBlacklisted = removeOnAnnotations . removeOnAutowired . removeOnClassSuffix
        getWithoutInEdges = noInEdges . graphFromEdges' . transformAllToEdges

inAutowired :: Map.Map String Autowiring -> Node -> Bool
inAutowired aMap node = not $ any (`Map.member` aMap) candidates
  where candidates = fileName (fst' node): implements (fst' node)

buildAutowiredMap :: [Result] -> Map.Map String Autowiring
buildAutowiredMap xs = Map.fromList $ concatMap autowiredToTuple xs
  where autowiredToTuple = map toAutowiredTuple . autowired

toAutowiredTuple :: Autowiring -> (String, Autowiring)
toAutowiredTuple autowiring@(AutowireAll type') = (type', autowiring)
toAutowiredTuple autowiring@(Autowiring type' _) = (type', autowiring)

fst' :: (a,b,c) -> a
fst' (a, _, _) = a

noInEdges :: (Graph, Vertex -> Node) -> [Node]
noInEdges (graph, vmap) = (getName . indegreeIsZero . addNodeName vmap) (withIndegree graph)

addNodeName :: (Vertex -> Node) -> [(Vertex, Int)] -> [NodeWithInDegree]
addNodeName vmap = map (first vmap)

getName :: [(Node, d)] -> [Node]
getName = map fst

indegreeIsZero :: [(a, Int)] -> [(a, Int)]
indegreeIsZero = filter ((==0).snd)

withIndegree :: Graph -> [(Vertex,Int)]
withIndegree g = zip (indices g) (elems (indegree g))

transformToEdges :: Result -> Node
transformToEdges r = (r, fileName r, references r ++ imports r ++ implements r)

blacklistedAnnotations :: Node -> Bool
blacklistedAnnotations r = not $ any (`elem` annotations) getBlacklistedAnnotations
  where annotations = topLevelAnnotations (fst' r) ++ methodAnnotations (fst' r)

removeBlacklistedClassSuffixes :: Node -> Bool
removeBlacklistedClassSuffixes = isBlacklisted . fileName . fst3
  where isBlacklisted s = not.or $ map (`isSuffixOf` s) blacklistedClassSuffixes

nodeHasClassName :: Node -> Bool
nodeHasClassName = (/= "") . fileName . fst3

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
