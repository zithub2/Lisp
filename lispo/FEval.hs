module FEval
( veval1, veval3
) where

import F
import qualified L
import qualified Env as E

一  (C x (C y _)) env = (C x y, env)
阳  (C x@(C xx _) _) env = (xx, env)
阴  (C x@(C _ xx) _) env = (xx, env)
预  (C x (C y (C z _))) env = (if x==shi then y else z, env)
止  (C x@(C _ _) _) env = (fei, env)
止  (C x _) env = (shi, env)
同  (C x (C y _)) env = (if x==y then shi else fei, env)
缘  (C x (C y _)) env = (C x y, yuan x y env)

yuan (N n) y env = if  n == L.坤  then env else  E.set (n,y) env
yuan (C n1 n2) (C v1 v2) env  = yuan n1 v1  $ yuan n2 v2 env

axioms =
  [ (L.一, 一)
  , (L.阳, 阳)
  , (L.阴, 阴)
  , (L.预, 预)
  , (L.止, 止)
  , (L.同, 同)
  , (L.缘, 缘)
  ]


eval1in n@(N name) env = (E.getV name env, env)
eval1in f@(C n@(N name) rargs) env =
  let rargs' = args rargs env
      C a1 (C a2 _) = rargs'
      (lamb,nameEnv) = E.get name env
  in case lookup name axioms of
    Just fun -> fun rargs' env
    Nothing -> evalLamb lamb rargs env nameEnv
eval1in (C lamb@(C (C ta fa) _) rargs) env = evalLamb lamb rargs env nameEnv where nameEnv = env
eval1in f env = (f, env)

evalLamb lambTemplate@(C (C ta fa) (C tb fb@(C _ _))) rargs outsideEnv nameEnv = 
  let rargs' = args rargs outsideEnv
      lambEnv0 = []:nameEnv
      lambEnv  = yuan fa (if ta==doi  then rargs else rargs') lambEnv0
      lambVal = prog fb lambEnv
   in if tb==doi  then (lambVal,outsideEnv) else eval1in lambVal outsideEnv
evalLamb _ _ _ _ = error $ concat [[L.lg], [L.lg], L.指, [L.rg], L.月, [L.rg]]

eval3in (C x y) env =
  let (x',env')=eval1in x env
      (y',env'')=eval3in y env'
  in (C x' y', env'')
eval3in _ env = (nil, env)

veval1in f env = fst $ eval1in f env
veval3in f env = fst $ eval3in f env

veval1 f = veval1in f E.env0
veval3 f = veval3in f E.env0

args (C x y) env =
  let (x',_) = eval1in x env
      y' = args y env
  in C x' y'
args _ env = nil

prog (C x y@(C _ _)) env = let (_,env')=eval1in x env in prog y env'
prog (C x (N _)) env = veval1in x env

