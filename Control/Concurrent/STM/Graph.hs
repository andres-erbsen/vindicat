module Control.Concurrent.STM.Graph (
    Graph
  , newGraph
  , insertNode
  , deleteNode
  , insertEdge
  , deleteEdge
  , deleteEdgesBetween
) where

import System.IO.Unsafe

import Control.Mutable
import Control.Loop
import Control.Monad
import Control.Monad.ST
import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TSkipList (TSkipList)
import Control.Concurrent.STM.TArray    (TArray)
import qualified Control.Concurrent.STM.TSkipList as TM

import Data.Maybe
import Data.STM.LinkedList (LinkedList)
import Data.Maybe (fromJust)
import qualified Data.STM.LinkedList as TL
import qualified Data.Array.MArray as MA
import Data.Array.ST
import Data.Map (Map)
import qualified Data.Map as M
import Data.Heap.Finger (Heap, OrdSeq)
import qualified Data.Heap.Finger as FH

import Debug.Trace

type TList = LinkedList
type TMap = TSkipList
newTM = TM.empty

data Graph e v = Graph { nodeMap    :: TMap v Int
                       , graphNodes :: TArray Int (Maybe v)
                       , outEdges   :: TArray Int (TMap Int (TList e))
                       ,  inEdges   :: TArray Int (TMap Int (TList e))
                       , nextNode   :: TVar Int
                       }

type Path e v = [(v,e)]

newGraph maxn = liftM5 Graph newTM nodeA neighA neighA (newTVar 0) where
  nodeA = MA.newArray (0,maxn) Nothing
  neighA = do
    a <- MA.newArray_ (0,maxn)
    forM_ [0..maxn] (\i -> do
      m <- newTM
      MA.writeArray a i m)
    return a

insertNode :: (Ord v) => Graph e v -> v -> STM ()
insertNode g v = do
  n <- val $ nextNode g -- todo: reuse numbers of deleted nodes
  nextNode g #= n+1
  nodeWithThisLabel <- TM.lookup v (nodeMap g)
  case nodeWithThisLabel of
    Nothing -> do
      TM.insert v n (nodeMap  g)
      MA.writeArray (graphNodes g) n (Just v)
    Just _ -> error "Duplicate nodes not allowed"

nodes' :: Graph e v -> STM [Int]
nodes' g = do
  n <- val $ nextNode g
  filterM (liftM isJust . (nodeLabel g)) [0..n]

nodes :: Graph e v -> STM [v]
nodes g = do
  n <- val $ nextNode g
  liftM catMaybes $ mapM (nodeLabel g) [0..n]

toList' :: Graph e v -> STM [(Int,[(Int,e)])]
toList' g = do
  ns <- nodes' g
  es <- mapM (edgesFrom' g) ns
  return $ zip ns es

toList :: (Ord v) => Graph e v -> STM [(v,[(v,e)])]
toList g = do
  ns <- nodes g
  es <- mapM (edgesFrom g) ns
  return $ zip ns es

toListAndLabels :: Graph e v -> STM ([(Int, [(Int,e)] )]
                                    ,[(Int, v         )])
toListAndLabels g = do
  adjlist <- toList' g
  labels <- MA.getAssocs $ graphNodes g
  let labels' = map (\(a,b) -> (a,fromJust b)) $ filter (isJust . snd) labels
  return (adjlist, labels')

fromList :: (Ord v) => Int -> [(v,[(v,e)])] -> STM (Graph e v)
fromList n graphdump = do
  g <- newGraph n :: STM (Graph e v)
  forM_ graphdump (insertNode g . fst)
  forM_ graphdump (\(v,outedges) -> do
    forM_ outedges (\(w,e) -> insertEdge g v w e))
  return g

instance (Show e, Show v, Ord v) => Show (Graph e v) where
  show = ("fromList " ++) . show . unsafePerformIO . atomically . toList

nodeNumber :: (Ord v) => Graph e v -> v -> STM (Maybe Int)
nodeNumber g v = TM.lookup v (nodeMap g)

nodeNumber' :: (Ord v) => Graph e v -> v -> STM Int
nodeNumber' g v = fromJust <$> nodeNumber g v

nodeLabel :: Graph e v -> Int -> STM (Maybe v)
nodeLabel g = MA.readArray (graphNodes g)

nodeLabel' :: Graph e v -> Int -> STM v
nodeLabel' g a = fromJust <$> nodeLabel g a

-- this can be optimized
deleteNode :: (Ord v, Eq e) => Graph e v -> v -> STM ()
deleteNode g v = do
  mapM_ (\(u,_) -> deleteEdgesFromTo g u v) =<< edgesTo   g v
  mapM_ (\(w,_) -> deleteEdgesFromTo g v w) =<< edgesFrom g v
  n <- fromJust <$> nodeNumber g v
  MA.writeArray (graphNodes g) n Nothing
  TM.delete v (nodeMap  g)

-- insertEdge :: (Ord v) => Graph e v -> v -> v -> e -> STM ()
insertEdge g v w e = do
  a <- nodeNumber' g v
  b <- nodeNumber' g w
  insertEdge' g a b e

insertEdge' g a b e = do
  from_a <- MA.readArray (outEdges g) a
  into_b <- MA.readArray ( inEdges g) b
  a_to_b <- TM.lookup b from_a -- out-edges a-->b
  case a_to_b of
    Just ab -> do
      ba <- fromJust <$> TM.lookup a into_b -- in-edges to b from a
      TL.append e ab
      TL.append e ba
      return ()
    Nothing -> do
      ab <- TL.empty
      ba <- TL.empty
      TL.append e ab
      TL.append e ba
      TM.insert b ab from_a
      TM.insert a ba into_b


deleteEdge :: (Ord v, Eq e) => Graph e v -> v -> v -> e -> STM ()
deleteEdge g v w e = do
  a <- nodeNumber' g v
  b <- nodeNumber' g w
  deleteEdge' g a b e

deleteEdge' g a b e = do
  from_a <- MA.readArray (outEdges g) a
  into_b <- MA.readArray ( inEdges g) b
  a_to_b <- TM.lookup b from_a
  case a_to_b of
    Nothing -> return ()
    Just ab -> do
      ba <- fromJust <$> TM.lookup a into_b
      tldel e =<< TL.start ab
      tldel e =<< TL.start ba
      where
      tldel e mn = case mn of
        Just n  -> do
          if TL.value n == e
            then TL.delete n
            else tldel e =<< TL.next n
        Nothing -> return ()


-- deleteEdgesBetween :: (Ord v) => Graph e v -> v -> v -> STM ()
deleteEdgesBetween g v w = do
  a <- nodeNumber' g v
  b <- nodeNumber' g w
  deleteEdgesFromTo' g a b
  deleteEdgesFromTo' g b a

deleteEdgesFromTo g v w = do
  a <- nodeNumber' g v
  b <- nodeNumber' g w
  deleteEdgesFromTo' g a b

deleteEdgesFromTo' g a b = do
  from_a <- MA.readArray (outEdges g) a
  into_b <- MA.readArray ( inEdges g) b
  TM.delete b from_a
  TM.delete a into_b


edgesFromTo :: (Ord v) => Graph e v -> v -> v -> STM [e]
edgesFromTo g v w = do
  a <- nodeNumber' g v
  b <- nodeNumber' g w
  edgesFromTo' g a b

edgesFromTo' g a b = do
  from_a <- MA.readArray (outEdges g) a
  edges_v_w <- TM.lookup b from_a
  case edges_v_w of
    Just vw -> TL.toList vw
    Nothing -> return []

wrapEdges marr a = do
  neighmap <- MA.readArray marr a
  neighs <- TM.toRevList neighmap -- (other node, [links to him])
  ll <- forM (reverse neighs) $ \(b,el) -> do
    ab <- TL.toList el
    return $ map (\e -> (b,e)) ab
  return $ concat ll

edgesFrom' :: Graph e v -> Int -> STM [(Int,e)]
edgesFrom' = wrapEdges . outEdges

edgesTo' :: Graph e v -> Int -> STM [(Int,e)]
edgesTo' = wrapEdges . inEdges

labelEdges :: Graph e v -> [(Int, e)] -> STM [(v,e)]
labelEdges g es =
  forM es $ \(i,e) -> do
    v <- nodeLabel' g i
    return (v,e)

edgesFrom :: (Ord v) => Graph e v -> v -> STM [(v,e)]
edgesFrom g v = labelEdges g =<< edgesFrom' g =<< nodeNumber' g v

edgesTo   :: (Ord v) => Graph e v -> v -> STM [(v,e)]
edgesTo   g v = labelEdges g =<< edgesTo'   g =<< nodeNumber' g v

-- shortestPathsFrom    :: (Ord v, Ord e, Num e) => Graph e v -> v -> STM [(Path e v, e)]
-- shortestPathsFrom = shortestPathsByFrom id
shortestPathsByFrom'  :: (Ord b, Num b) =>
     Graph e v
  -> (e -> b)
  -> Int
  -> IO [( Int, (Path e Int, b))]
shortestPathsByFrom' g measure source = do
  lg <- atomically $ toList' g
  -- neighbour lookup map: v -> [e,v']
  let mg = M.fromList lg
  return $! runST $ dijkstraST measure mg source

dijkstraST :: (Num b, Ord b) => (e -> b) -> Map Int [(Int, e)] -> Int -> ST s [( Int, (Path e Int, b))]
dijkstraST measure mg source = do
  -- Maximal node (id) number
  let n = maximum $ M.keys mg
  -- array[v] is mminumum distance from source to node v
  reached <- MA.newArray (0,n) False :: ST s (STArray s Int Bool)
  -- array[v] is mminumum distance from source to node v
  distances <- MA.newArray_ (0,n) :: ST s (STArray s Int b)
  -- array[v] is the last edge on shortest path from source to v
  parents   <- MA.newArray_ (0,n) :: ST s (STArray s Int Int)
  -- frontier queue
  q <- ref $ FH.insert 0 source $ FH.empty
  writeArray reached     source True
  writeArray distances   source 0
  while (not . FH.null # q) $ do
    (dst_v, v, q') <- FH.extractMin # q
    q #= q'
    let maybe_v_w = M.lookup v mg
    case maybe_v_w of
      Nothing -> pass
      Just vw -> forM_ vw $ \(w,e) -> do
        isreached <- readArray reached w
        dst_w   <- readArray distances w
        let dst_w' = dst_v + measure e
        if not isreached || dst_w' < dst_w
        then do
          q .= FH.insert dst_w' w
          writeArray distances w dst_w'
          writeArray parents   w v
        else pass
  return []

main = do
  g <- atomically $ newGraph 10 :: IO (Graph Float String)
  atomically $ insertNode g "K"
  atomically $ insertNode g "L"
  atomically $ insertEdge g "K" "L" 10.5
  print =<< (atomically $ edgesFrom g "K")
  print =<< (atomically $ edgesFromTo g "K" "L")
  
  print =<< (atomically $ edgesTo g "L")
  atomically $ deleteEdgesBetween g "K" "L"
  putStrLn "deleted"
  print =<< (atomically $ edgesFrom g "K")
  print =<< (atomically $ edgesFromTo g "K" "L")
  print =<< (atomically $ edgesTo g "L")
  atomically $ insertNode g "KALA"
  putStrLn "added"
  atomically $ insertEdge g "KALA" "L" 42
  print =<< (atomically $ edgesFrom g "KALA")
  print =<< (atomically $ edgesFromTo g "KALA" "L")
  print =<< (atomically $ edgesTo g "L")
  atomically $ deleteEdge g "KALA" "L" 0
  putStrLn "still here"
  print =<< (atomically $ edgesFrom g "KALA")
  print =<< (atomically $ edgesFromTo g "KALA" "L")
  print =<< (atomically $ edgesTo g "L")
  atomically $ deleteEdge g "KALA" "L" 42
  putStrLn "deleted"
  print =<< (atomically $ edgesFrom g "K")
  print =<< (atomically $ edgesFromTo g "KALA" "L")
  print =<< (atomically $ edgesTo g "L")
  putStrLn "added"
  atomically $ insertEdge g "KALA" "L" 42
  print =<< (atomically $ edgesFrom g "KALA")
  print =<< (atomically $ edgesFromTo g "KALA" "L")
  print =<< (atomically $ edgesTo g "L")
  atomically $ deleteEdge g "KALA" "L" 0
  putStrLn "added"
  atomically $ insertEdge g "KALA" "L" 42
  print =<< (atomically $ edgesFrom g "KALA")
  print =<< (atomically $ edgesFromTo g "KALA" "L")
  print =<< (atomically $ edgesTo g "L")
  atomically $ deleteEdge g "KALA" "L" 0
  putStrLn "added LEHM"
  atomically $ insertNode g "LEHM"
  atomically $ insertEdge g "LEHM" "L" 99
  print =<< (atomically $ edgesFrom g "LEHM")
  print =<< (atomically $ edgesFromTo g "LEHM" "L")
  print =<< (atomically $ edgesTo g "L")
  putStrLn "Dumpíng graph:"
  print =<< (atomically $ toList' g)
  print =<< (atomically $ toList g)
  putStrLn "deleted KALA"
  atomically $ deleteNode g "KALA"
  print =<< (atomically $ edgesTo g "L")

  let n = 50
  let k = 10
  putStrLn "Creating..."
  g <- atomically $ newGraph n :: IO (Graph Int Int)
  putStrLn "Inserting nodes..."
  forM_ [0..n] (atomically . insertNode g)
  putStrLn "Inserting edges..."
  forM_ [1..n-k] $ \i ->
    forM_ [0..k] $ \j ->
      atomically $ insertEdge g i (i+j) (i^j)
  putStrLn "Done!"

