{-
Copyright (C) 2012 Michael Fogus <me -at- fogus -dot- me>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Eval (evil, eval, evalStr, apply, bind, load, evalfuns)
where
import Control.Monad.Error
import Data.IORef (readIORef, newIORef)
import Helpers
import K (nullK, cps, cpsArgs)
import Macros (macroexpand)
import Parser (read_, read')
import Refs
import Cps
import Types
import Debug.Trace

evil :: Env -> LispVal -> IOThrowsError LispVal
evil env src = keval env (nullK env) src

keval :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
keval env k code = kfun env k code eval
kapply :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
kapply env k code = kfun env k code prepareApply
kfun :: Env -> LispVal -> LispVal -> (Env -> LispVal -> LispVal -> IOThrowsError LispVal) -> IOThrowsError LispVal
kfun env k code fun = do
  macroexpand env code >>= (fun env k)

-- # eval

evalStr :: Env -> String -> IO String
evalStr env expr = runio $ liftM show $ (liftThrows $ read' expr) >>= keval env (nullK env)

eval :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
eval env k (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env (nullK env))

eval env k val@(String _) = continue env k val
eval env k val@(Float _) = continue env k val
eval env k val@(Complex _) = continue env k val
eval env k val@(Ratio _) = continue env k val
eval env k val@(Number _) = continue env k val
eval env k val@(Bool _) = continue env k val
eval env k val@(Vector _) = continue env k val
eval env k (Atom id) = continue env k =<< getVar env id
eval env _ (List [Atom "env"]) = return $ Environment env
eval env k (List [Atom "quote", val]) = continue env k val

eval env k form@(List [Atom "if", pred, conseq, alt]) = do
  bound <- liftIO $ isBound env "if"
  if bound
    then prepareApply env k form
    else keval env (cps env k rest) pred
  where rest :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        rest e c result _ =
            case (result) of
              Bool False -> keval e c alt
              _ -> keval e c conseq

-- TODO: set! should take a fun
eval env k args@(List [Atom "set!", Atom var, form]) = do
  bound <- liftIO $ isBound env "set!"
  if bound
    then prepareApply env k args
    else keval env (cps env k rest) form
  where rest :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        rest e c result _ = setVar e var result >>= continue e c

eval env k args@(List [Atom "define", Atom var, form]) = do
  bound <- liftIO $ isBound env "define"
  if bound
    then prepareApply env k args
    else keval env (cps env k rest) form
  where rest :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        rest e c result _ = defineVar e var result >>= continue e c

eval env k args@(List (Atom "λ" : List params : body)) = do
  bound <- liftIO $ isBound env "λ"
  if bound
    then prepareApply env k args
    else do result <- fun0 env params body
            continue env k result

eval env k form@(List (_ : _)) = kapply env k form
eval _ _ bad = throwError $ BadSpecialForm "Programmer is insufficiently polite regarding " bad

bind :: Env -> [(String, LispVal)] -> IO Env
bind envRef bindings = readIORef envRef >>= extend bindings >>= newIORef
    where extend bindings env = liftM (++ env) (mapM bind' bindings)
          bind' (var, value) = do ref <- newIORef value
                                  return (var, ref)

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . read_

makeFun args env params body = return $ Func (map showVal params) args body env

fun0 = makeFun Nothing

-- # apply

apply :: LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply _ cont@(Continuation env k next _ wind) args = do
 case wind of
    Just ([DynamicWinders before _]) -> apply (cps env cont restApply) before []
    _ -> doit env cont
 where
   restApply :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
   restApply e c _ _ = doit e c
   doit e c =
      case (toInteger $ length args) of
        0 -> throwError $ NumArgs 1 []
        1 -> continue e c $ head args
        _ -> continue e (Continuation env k next (Just $ tail args) wind) $ head args

apply k (PrimitiveFunc func) args = do
  res <- liftThrows $ (func args)
  case k of
    Continuation env _ _ _ _ -> continue env k res
    _ -> return res

apply k (IOFunc func) args = do
  res <- func args
  case k of
    Continuation env _ _ _ _ -> continue env k res
    _ -> return res

apply k (KFunc func) args = do
  func (k : args)

-- eliminate varargs
apply k f@(Func params varargs body context) args =
    if num params /= num args && varargs == Nothing
    then if num args < num params
            then return $ Partial f args (num args) ((num params) - (num args))
            else throwError $ NumArgs (num params) args
    else (liftIO $ bind context $ zip params args) >>= bind' varargs >>= eval''
    where xargs = drop (length params) args
          num = toInteger . length
          eval'' env = case k of
            Continuation _
                         (Just (CodeBody xbody))
                         (Just k')
                         _
                         wind -> if length xbody == 0
                                    then continue' env (body) k' wind
                                    else continue' env (body) k wind
            Continuation _
                         _
                         _
                         _
                         wind -> continue' env (body) k wind
            _ -> continue' env (body) k Nothing
          continue' kenv kbody nekt kwind =
            continue kenv (Continuation kenv
                                        (Just (CodeBody kbody))
                                        (Just nekt)
                                        Nothing
                                        kwind) $ String ""
          bind' arg env = case arg of
              Just name -> liftIO $ bind env [(name, List $ xargs)]
              Nothing -> return env
apply _ head _ = throwError $ NotFunction "Not a function " (show head)

-- # Cps

prepareApply :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
prepareApply env cont (List (function : functionArgs)) = do
  eval env (cpsArgs env cont prepArgs $ functionArgs) function
 where prepArgs :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       prepArgs e c func (Just args) =
          case args of
            [] -> apply c func []
            [a] -> keval env (cpsArgs e c evalArgs $ [func, List [], List []]) a
            (a : as) -> keval env (cpsArgs e c evalArgs $ [func, List [], List as]) a
       prepArgs _ _ _ Nothing = throwError $ Default "Unexpected error in function application (1)"
       evalArgs :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       evalArgs e c arg (Just [func, List args', List restArgs]) =
          case restArgs of
            [] -> apply c func (args' ++ [arg])
            [a] -> keval e (cpsArgs e c evalArgs $ [func, List (args' ++ [arg]), List []]) a
            (a : as) -> keval e (cpsArgs e c evalArgs $ [func, List (args' ++ [arg]), List as]) a
       evalArgs _ _ _ (Just _) = throwError $ Default "Unexpected error in function application (1)"
       evalArgs _ _ _ Nothing = throwError $ Default "Unexpected error in function application (2)"
prepareApply _ _ _ = throwError $ Default "Unexpected error in prepareApply"


continue :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
continue _
         (Continuation env (Just (HostBody fun args))
                           (Just (Continuation context
                                               cc
                                               next
                                               _
                                               wind))
                           xargs
                           _)
         val = fun env (Continuation context cc next xargs wind) val args

continue _
        (Continuation env
                      (Just (CodeBody code))
                      (Just k)
                      xargs
                      wind) val = do
    case code of
        [] -> do
          case k of
            Continuation kenv kode nekt _ kwind ->
              continue kenv
                       (Continuation kenv kode nekt xargs kwind)
                       val
            _ -> return (val)
        [lv] -> keval env
                      (Continuation env
                                    (Just (CodeBody []))
                                    (Just k)
                                    Nothing
                                    wind)
                      lv
        (lv : lvs) -> keval env
                            (Continuation env
                                          (Just (CodeBody lvs))
                                          (Just k)
                                          Nothing
                                          wind)
                            lv

continue _ (Continuation env Nothing (Just k) _ _) val = continue env k val
continue _ (Continuation _ Nothing Nothing _ _) val = return val
continue _ _ _ = throwError $ Default "Error following continuation!"

-- Eval functions

apply' :: [LispVal] -> IOThrowsError LispVal
apply' [k@(Continuation _ _ _ _ _), fun, List args] = apply k fun args
apply' (_ : args) = throwError $ NumArgs 2 args
apply' _ = throwError $ NumArgs 2 []

evil' :: [LispVal] -> IOThrowsError LispVal
evil' [k@(Continuation env _ _ _ _), form] = keval env k form
evil' [k, (Environment env), form] = keval env k form          -- TODO: Get this working
evil' (_ : args) = throwError $ NumArgs 1 args
evil' _ = throwError $ NumArgs 1 []

callcc :: [LispVal] -> IOThrowsError LispVal
callcc [k@(Continuation _ _ _ _ _), fun] = do
   case fun of
     PrimitiveFunc f -> do
       result <- liftThrows $ f [k]
       case k of
         Continuation env _ _ _ _ -> continue env k result
         _ -> return result
     Func args _ _ _ ->
       if (toInteger $ length args) == 1
         then apply k fun [k]
         else throwError $ NumArgs (toInteger $ length args) [k]
     other -> throwError $ TypeMismatch "function" other
callcc (_ : bad) = throwError $ CallCCBadArgs bad
callcc _ = throwError $ CallCCNoArgs "nothing at all"

evalfuns :: [(String, [LispVal] -> IOThrowsError LispVal)]
evalfuns = [("call-with-current-continuation", callcc)
           ,("apply", apply')
           ,("eval",  evil')]

