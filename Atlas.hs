module Atlas (
    Atlas(..)
  , mkAtlas
  , insertDevice
  , insertLink
  , getDevice
  , getPathTo
) where

import Data.Maybe
import Control.Applicative
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
  , ourDev     :: Device
  }

mkAtlas ourDev = Atlas M.empty (G.insNode (0,ourDev) G.empty) 1 ourDev

insertDevice :: Device -> Atlas -> (Atlas,Bool)
insertDevice dev atl@(Atlas m gr n od) = case match of
  Nothing -> (Atlas m' gr' n' od, True) -- this device is not in our graph yet
    where
    m' = foldr (\k -> M.insert k dev') m newkeys
    gr' = G.insNode (n, dev') gr
    n'  = n+1
    dev' = dev {deviceGraphId = Just n}
  (Just old_dev) -> (atl, False) -- TODO: update info
  where
  match = find isJust $ map devByKey keys
  devByKey k = M.lookup k m
  newkeys = filter (isNothing . devByKey) keys
  keys = deviceKeys dev


getDevice :: Atlas -> PubKey -> Maybe Device
getDevice atl k = M.lookup k (atlasDevs atl)

insertLink :: Link -> Atlas -> (Atlas,Bool)
insertLink link atl
 = if (haveleft || haveright) && (not haveleft || not haveright || dead)
     then if dead
       then (atl {atlasGraph = G.delEdge (l,r)      gr}, True)
       else (atl {atlasGraph = G.insEdge (l,r,link) gr}, True)
     else (atl,False)
  where
  dead = linkDead link
  haveleft  = not . null $ leftdevs
  haveright = not . null $ rightdevs
  l = fromJust <$> deviceGraphId $ head leftdevs
  r = fromJust <$> deviceGraphId $ head rightdevs
  leftdevs  = catMaybes . map (getDevice atl) $ leftKeys 
  rightdevs = catMaybes . map (getDevice atl) $ leftKeys 
  leftKeys  = deviceKeys . linkLeftEnd   $ link
  rightKeys = deviceKeys . linkRightEnd  $ link
  gr = atlasGraph atl

getPathTo :: Atlas -> PubKey -> [Device]
getPathTo atl k = [] `fromMaybe` do
  ddev <- M.lookup k (atlasDevs atl)
  d <- deviceGraphId ddev
  return . map (fromJust . G.lab gr) . G.sp 0 d . G.emap measureEdge $ gr
  where gr = atlasGraph atl

measureEdge :: Num a => Link -> a
measureEdge _ = 1 -- number of hops metric.
