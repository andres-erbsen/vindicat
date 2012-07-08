module Atlas (
    Atlas(..)
  , mkAtlas
  , insertDevice
  , insertLink
  , getDevice
  , getPathTo
) where

import Data.Maybe
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Graph.Inductive (Gr, Path, LPath)
import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.SP as G
import Data.Graph.Inductive.Internal.RootPath as G

import Vindicat

data Atlas = Atlas 
  { atlasDevs :: Map PubKey Device
  , atlasGraph :: Gr Device Link
  , nextNode :: Int
  }

mkAtlas ourDev = Atlas M.empty (G.insNode (0,ourDev) G.empty) 1

insertDevice :: Device -> Atlas -> (Atlas,Bool)
insertDevice dev atlas@(Atlas m gr n) = case match of
  Nothing -> (Atlas m' gr' n', True) -- this device is not in our graph yet
    where
    m' = foldr (\k -> M.insert k dev') m newkeys
    gr' = G.insNode (n, dev') gr
    n'  = n+1
    dev' = dev {deviceGraphId = Just n}
  (Just old_dev) -> (atlas, False) -- TODO: update info
  where
  match = find isJust $ map devByKey keys
  newkeys = filter (isNothing . devByKey) keys
  keys = deviceKeys dev
  devByKey k = M.lookup k m

headJust :: [Maybe a] -> Maybe a
headJust = listToMaybe . catMaybes

getDevice :: Atlas -> PubKey -> Maybe Device
getDevice atlas k = M.lookup k (atlasDevs atlas)

insertLink   :: Link -> Atlas -> (Atlas,Bool)
insertLink link atlas = (atlas, False) `fromMaybe` do
  ldev <- headJust . map (getDevice atlas) . deviceKeys . linkLeftEnd  $ link
  rdev <- headJust . map (getDevice atlas) . deviceKeys . linkRightEnd $ link
  let (l,r) = (fromJust $ deviceGraphId ldev, fromJust $ deviceGraphId rdev)
  let gr' = if linkDead link then G.delEdge (l,r)      gr
                             else G.insEdge (l,r,link) gr
  return $ (atlas {atlasGraph = gr'}, True)
  where gr = atlasGraph atlas

getPathTo :: Atlas -> PubKey -> [Device]
getPathTo atlas k = [] `fromMaybe` do
  ddev <- M.lookup k (atlasDevs atlas)
  d <- deviceGraphId ddev
  return .  map (fromJust . G.lab gr) . G.sp 0 d . G.emap measureEdge $ gr
  where gr = atlasGraph atlas

measureEdge :: Num a => Link -> a
measureEdge _ = 1 -- number of hops metric.
