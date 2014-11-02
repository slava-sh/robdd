module Data.ROBDD.GraphViz (showDot) where

import Text.Dot (Dot)
import qualified Text.Dot as Dot

import Data.ROBDD.Internal

showDot :: Robdd p -> String
showDot = Dot.showDot . toDot

toDot :: Robdd p -> Dot ()
toDot (Leaf False)                         = node 0 "0"
toDot (Leaf True)                          = node 1 "1"
toDot (Branch Ref _ _ _ _)                 = return ()
toDot x@(Branch Orig (Id i) (Var v) lo hi) = do
  node i ("x" ++ show v)
  edgeLo x lo
  edgeHi x hi
  toDot lo
  toDot hi

node :: Int -> String -> Dot ()
node i s = Dot.userNode (Dot.userNodeId i) [("label", s)]

edgeLo :: Robdd p -> Robdd p -> Dot ()
edgeLo a b = Dot.edge (nodeId a) (nodeId b)
  [ ("minlen",   "2")
  , ("tailport", "sw")
  , ("style",    "dashed")]

edgeHi :: Robdd p -> Robdd p -> Dot ()
edgeHi a b = Dot.edge (nodeId a) (nodeId b)
  [ ("minlen",   "2")
  , ("tailport", "se")
  ]

nodeId :: Robdd p -> Dot.NodeId
nodeId x = case getId x of
  (Id i) -> Dot.userNodeId i
