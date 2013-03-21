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

module Helpers
where
import Types
import Control.Monad.Error

trap action = catchError action (return . show)

runio :: IOThrowsError String -> IO String
runio action = runErrorT (trap action) >>= return . extricate

slice :: [a] -> Integer -> Integer -> [a]
slice l s e = do
    let start = fromIntegral s
    let end   = fromIntegral e
    (take start $ drop end $ l)

-- unpackers

pull :: LispVal -> [LispVal]
pull (List ary) = ary

extricate :: ThrowsError a -> a
extricate (Right val) = val

unpackchar :: LispVal -> ThrowsError Char
unpackchar (Character c) = return c
unpackchar notChar = throwError $ TypeMismatch "char" notChar

unpackstr :: LispVal -> ThrowsError String
unpackstr (String s) = return s
unpackstr notString = throwError $ TypeMismatch "string" notString

unpackbool :: LispVal -> ThrowsError Bool
unpackbool (Bool b) = return b
unpackbool notBool = throwError $ TypeMismatch "boolean" notBool

unpacknum :: LispVal -> ThrowsError Integer
unpacknum (Number n) = return n
unpacknum (List [n]) = unpacknum n
unpacknum notNum = throwError $ TypeMismatch "number" notNum

-- function type builders

unary :: (a -> b) -> [a] -> b
unary f [v] = f v

boolop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

comparator = boolop unpacknum
str = boolop unpackstr
bool = boolop unpackbool
