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

module Io (iofuns) 
where
import Types
import Eval (apply, load)
import Parser (read')
import qualified Control.Monad.Error as M
import System.IO 


-- # I/O

iofuns :: [(String, [LispVal] -> IOThrowsError LispVal)]
iofuns = [("open-input-file!", fun_make_port ReadMode),
          ("open-output-file!", fun_make_port WriteMode),
          ("close-input-port!", fun_close_port),
          ("close-output-port!", fun_close_port),
          ("in!", fun_read_port),
          ("out!", fun_write_port),
          ("slurp!", fun_slurp_port),
          ("read-forms!", fun_read_forms)]

fun_read_forms :: [LispVal] -> IOThrowsError LispVal
fun_read_forms [String filename] = M.liftM List $ load filename

fun_make_port :: IOMode -> [LispVal] -> IOThrowsError LispVal
fun_make_port mode [String filename] = M.liftM Port $ M.liftIO $ openFile filename mode

fun_close_port :: [LispVal] -> IOThrowsError LispVal
fun_close_port [Port port] = M.liftIO $ hClose port >> (return $ Bool True)
fun_close_port _ = return $ Bool False

fun_read_port :: [LispVal] -> IOThrowsError LispVal
fun_read_port [] = fun_read_port [Port stdin]
fun_read_port [Port port] = (M.liftIO $ hGetLine port) >>= liftThrows . read'

fun_write_port :: [LispVal] -> IOThrowsError LispVal
fun_write_port [obj] = fun_write_port [obj, Port stdout]
fun_write_port [obj, Port port] = M.liftIO $ hPrint port obj >> (return $ Bool True)

fun_slurp_port :: [LispVal] -> IOThrowsError LispVal
fun_slurp_port [String filename] = M.liftM String $ M.liftIO $ readFile filename

