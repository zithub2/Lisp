module Env
( env0
, get, getV
, set
) where

import qualified L
import qualified Dict as D


get name env = case D.get name env of
  (Just f, e) -> (f,e)
  (Nothing, _) -> error $ concat [L.无, L.缘, L.sep7, name]

getV name env = fst $ get name env


set kv@(k,v) dicts@(d:ds) = case lookup k d of
  Nothing -> D.set kv dicts
  Just _ -> error $ concat [L.有, L.缘, L.sep7, k]

env0 = [[]]

