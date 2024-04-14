module FRead
( read1, read3
, readIndent
) where

import Data.Bifunctor
import Data.Char (isPrint, isSpace)
import qualified L
import F

isC (C _ _) = True
isC (N _) = False

(.>) = flip (.)

readIndent = lines .> fmap readIndentLine .> filter (snd .> isC) .> indent2F

readIndentLine = span isSpace .> bimap length (fst . read3)

indent2F [] = nil
indent2F ((i,f):ifs) =
  let (subs,rest) = span (fst .> (>i)) ifs
  in C (indentBlock f subs) $ indent2F rest

indentBlock (C f (N _)) [] = f
indentBlock f [] = f
indentBlock f subs = concatF f $ indent2F subs

concatF (N _) f = f
concatF (C x y) f = C x (concatF y f)

read1 "" = (nil, "")
read1 str@(c:cs)
  | c==L.lg = let (e,rest)=read3 cs in (e, rest)
  | c==L.rg = (nil, cs)
  | isInk c = let (name,rest)=span atomChar str in (N name, rest)
  | otherwise = read1 cs

read3 "" = (nil,"")
read3 str@(c:cs)
  | c==L.rg = (nil,cs)
  | isInk c = -- atomChar or lg
      let (f,rest) = read1 str
          (fs,rest') = read3 rest
      in (C f fs, rest')
  | otherwise = read3 cs

isInk c = isPrint c && not (isSpace c)
atomChar c = isInk c && notElem c [L.lg,L.rg]

