module F
( F(N,C)
, fei, shi, doi, nil
) where

import qualified L

data F = N String | C F F deriving (Eq, Show)
fei = N L.非
shi = N L.是
doi = N L.指
nil = N L.坤

