import Control.Concurrent.STM
import Data.Map

data Graph e v = Graph (TVar (Map v (Node e v)))
data Node e v = Node { nodeLabel :: v
                     , nodeEdges :: (TVar (Map v e))
                     }

insertNode :: (Ord v) => v -> Graph e v -> STM ()
insertNode v (Graph m) = do
  newneighs <- newTVar empty
  modifyTVar' m . insert v $ Node v newneighs

insertEdge :: (Ord v) => v -> v -> e -> Graph e v -> STM ()
insertEdge v w e (Graph m) = do
  m' <- (readTVar m)
  modifyTVar (nodeEdges (m' ! v)) (insert w e)
