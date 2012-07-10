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

import Vindicat

data Atlas = Atlas 
  { atlasDevs  :: Map PubKey Device
  , atlasGraph :: Gr Device Link
  , nextNode   :: Int
  }

mkAtlas ourDev = Atlas M.empty (G.insNode (0,ourDev) G.empty) 1

insertDevice :: Device -> Atlas -> (Atlas,Bool)
insertDevice dev atl@(Atlas m gr n) = case match of
  Nothing -> (Atlas m' gr' n', True) -- this device is not in our graph yet
    where
    m' = foldr (\k -> M.insert k dev') m newkeys
    gr' = G.insNode (n, dev') gr
    n'  = n+1
    dev' = dev {deviceGraphId = Just n}
  (Just old_dev) -> (atl, False) -- TODO: update info
  where
  match = find isJust $ map devByKey keys
  newkeys = filter (isNothing . devByKey) keys
  keys = deviceKeys dev
  devByKey k = M.lookup k m

headJust :: [Maybe a] -> Maybe a
headJust = listToMaybe . catMaybes

getDevice :: Atlas -> PubKey -> Maybe Device
getDevice atl k = M.lookup k (atlasDevs atl)

insertLink   :: Link -> Atlas -> (Atlas,Bool)
insertLink link atl = (atl, False) `fromMaybe` do
  ldev <- headJust . map (getDevice atl) . deviceKeys . linkLeftEnd  $ link
  rdev <- headJust . map (getDevice atl) . deviceKeys . linkRightEnd $ link
  l <- deviceGraphId ldev
  r <- deviceGraphId rdev
  let gr' = if linkDead link then G.delEdge (l,r)      gr
                             else G.insEdge (l,r,link) gr
  return $ (atl {atlasGraph = gr'}, True)
  where gr = atlasGraph atl

getPathTo :: Atlas -> PubKey -> [Device]
getPathTo atl k = [] `fromMaybe` do
  ddev <- M.lookup k (atlasDevs atl)
  d <- deviceGraphId ddev
  return . map (fromJust . G.lab gr) . G.sp 0 d . G.emap measureEdge $ gr
  where gr = atlasGraph atl

measureEdge :: Num a => Link -> a
measureEdge _ = 1 -- number of hops metric.
