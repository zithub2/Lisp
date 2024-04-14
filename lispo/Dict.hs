module Dict
( get, getV
, set
) where

-- dict: [[(String, F)]]
--type KV = (String, F)
--type Dict = [KV]
--type Dicts = [Dict]

-- get val and the corresponding env
get name env@(d:ds) = case lookup name d of
  Nothing -> get name ds
  j -> (j,env)
get name [] = (Nothing, [])

getV name env = fst $ get name env -- get val, disgard env

-- bind k to v in the first dict
set kv dicts@(d:ds) = (kv:d) : ds
set kv [] = set kv [[kv]]

