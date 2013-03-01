{-
Copyright (C) 2013 Michael Fogus <me -at- fogus -dot- me>

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

module Main where
import Types
import Helpers
import Eval (bind, evil, evalStr)

import Primitives (globals)
import System.Environment (getArgs)
import System.IO (hFlush, stderr, stdout, hPutStrLn)
import Control.Monad.Error (runErrorT)
import Control.Monad (liftM)

import Paths_li (version)
import Data.Version (showVersion)

-- # REPL

flush :: String -> IO ()
flush str = putStr str >> hFlush stdout

prompt :: String -> IO String
prompt prefix = flush prefix >> getLine

printeval :: Env -> String -> IO ()
printeval env expr =  evalStr env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

oneshot :: [String] -> IO ()
oneshot args = do
    env <- globals >>= flip bind [("args", List $ map String $ drop 1 args)]
    (runio $ liftM show $ evil env (List [Atom "load", String (args !! 0)]))
         >>= hPutStrLn stderr

repl :: IO ()
repl = globals >>= until_ (== "quit") (prompt "r7rs> ") . printeval

-- # Main

main :: IO ()
main = do args <- getArgs
          case args of
               ["--version"] -> putStrLn ("Li " ++ showVersion version)
               [] -> repl
               _  -> oneshot $ args

