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

module Primitives (globals)
where
import Types
import Numerics
import Helpers
import Egal
import Eval (bind, eval, evalfuns)
import Io (iofuns)
import Control.Monad.Error (throwError, liftIO)
import System.IO
import Data.IORef

-- # Primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("<", comparator (<)),
              (">", comparator (>)),
              (">=", comparator (>=)),
              ("<=", comparator (<=)),
              ("&&", bool (&&)),
              ("||", bool (||)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("make-list", makeList),
              ("append", append),
              ("length", len),
              ("reverse", rev),
              ("not", unary fun_not)]

stringFun :: [(String, [LispVal] -> ThrowsError LispVal)]
stringFun = [("string=?", str (==)),
             ("string<?", str (<)),
             ("string>?", str (>)),
             ("string<=?", str (<=)),
             ("string>=?", str (>=)),
             ("string-length", strLen),
             ("string", string),
             ("string-ref", stringRef),
             ("substring", stringSlice),
             ("string-copy", stringCopy),
             ("string-append", stringCat),
             ("make-string", makeString)]

predicates :: [(String, [LispVal] -> ThrowsError LispVal)]
predicates = [("symbol?",    unary fun_symbolp),
              ("string?",    unary fun_stringp),
              ("char?",      unary fun_charp),
              ("number?",    unary fun_numberp),
              ("boolean?",   unary fun_boolp),
              ("list?",      unary fun_listp),
              ("vector?",    unary fun_vectorp),
              ("pair?",      unary fun_dottedp),
              ("procedure?", unary fun_funp),
              ("zero?",      unary fun_zerop),
              ("null?",      unary fun_emptyp)]

convertors :: [(String, [LispVal] -> ThrowsError LispVal)]
convertors = [("symbol->string", symbolToString),
              ("string->symbol", stringToSymbol),
              ("string->list", stringToList),
              ("list->string", listToString),
              ("vector->list", vectorToList),
              ("list->vector", listToVector)]

vectorFun :: [(String, [LispVal] -> ThrowsError LispVal)]
vectorFun =  [("make-vector", makeVector),
              ("vector-length", vectorLen),
              ("vector-ref", vectorRef)]


globals :: IO Env
globals = nullEnv >>= (flip bind $ map (funAs IOFunc) iofuns
                                        ++ map (funAs PrimitiveFunc) primitives
                                        ++ map (funAs PrimitiveFunc) numerics
                                        ++ map (funAs PrimitiveFunc) stringFun
                                        ++ map (funAs PrimitiveFunc) vectorFun
                                        ++ map (funAs PrimitiveFunc) convertors
                                        ++ map (funAs KFunc) evalfuns
                                        ++ map (funAs PrimitiveFunc) predicates)
    where funAs constructor (var, func) = (var, constructor func)


-- # not

fun_not :: LispVal -> ThrowsError LispVal
fun_not   (Bool v)         = return $ Bool (not v)
fun_not   _                = return $ Bool False

-- # convertors

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom s] = return $ String s
symbolToString badArgList = throwError $ NumArgs 1 badArgList

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [String s] = return $ Atom s
stringToSymbol badArgList = throwError $ NumArgs 1 badArgList

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [] = return $ List []
stringToList [String chars] = return $ List (map Character chars)
stringToList [String chars, Number i] = return $ List (map Character (slice chars i (toInteger (length chars))))
stringToList [String chars, Number s, Number e] = return $ List (map Character (slice chars s e))
stringToList args@[Number _, Number _, String _] = throwError $ BadArg "Argument order error, should be (str [num num])" (List args)
stringToList args@[Number _, String _, Number _] = throwError $ BadArg "Argument order error, should be (str [num num])" (List args)
stringToList args@[_, _, _] = throwError $ BadArg "Bad arguments, should be (str [num num])" (List args)
stringToList args = throwError $ BadArg "Bad arguments, should be (str [num num])" (List args)

listToString :: [LispVal] -> ThrowsError LispVal
listToString [] = return $ String ""
listToString [(List [Character c])] = return $ String [c]
listToString [List chars] = mapM unpackchar chars >>= return . String
listToString args = throwError $ BadArg "Bad arguments, should be ([char]*)" (List args)

listToVector :: [LispVal] -> ThrowsError LispVal
listToVector [] = return $ Vector []
listToVector [List vals] = return $ Vector vals
listToVector badArgs = throwError $ BadArg "Bad arguments, should be (a-list)" (List badArgs)

vectorToList :: [LispVal] -> ThrowsError LispVal
vectorToList [] = return $ List []
vectorToList [Vector vals] = return $ List vals
vectorToList badArgs = throwError $ BadArg "Bad arguments, should be (a-vector)" (List badArgs)


-- # predicates

fun_emptyp, fun_symbolp, fun_vectorp, fun_numberp, fun_stringp, fun_boolp, fun_listp, fun_dottedp :: LispVal -> ThrowsError LispVal
fun_listp   (List _)         = return $ Bool True
fun_listp   _                = return $ Bool False
fun_vectorp   (Vector _)     = return $ Bool True
fun_vectorp   _              = return $ Bool False
fun_dottedp (Dotted _ _)     = return $ Bool True
fun_dottedp (List [])        = return $ Bool False
fun_dottedp (List _)         = return $ Bool True
fun_dottedp _                = return $ Bool False
fun_symbolp (Atom _)         = return $ Bool True
fun_symbolp _                = return $ Bool False
fun_numberp (Number _)       = return $ Bool True
fun_numberp _                = return $ Bool False
fun_stringp (String _)       = return $ Bool True
fun_stringp _                = return $ Bool False
fun_charp (Character _)      = return $ Bool True
fun_charp _                  = return $ Bool False
fun_boolp   (Bool _)         = return $ Bool True
fun_boolp   _                = return $ Bool False
fun_emptyp  (List [])        = return $ Bool True
fun_emptyp  (List _)         = return $ Bool False
fun_emptyp  _                = return $ Bool False
fun_funp    (Func _ _ _ _)   = return $ Bool True
fun_funp    _                = return $ Bool False
fun_zerop  (Number n)        = return $ Bool (n == 0)
fun_zerop   _                = return $ Bool False

-- # car / cdr / cons

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [Dotted [l] _] = return l
car [empty@(List [])] = throwError $ TypeMismatch "list with one or more elements" empty
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [Dotted _ r] = return r
cdr [empty@(List [])] = throwError $ TypeMismatch "list with one or more elements" empty
cdr [badArg] = throwError $ TypeMismatch "list" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [l, r] = return $ Dotted [l] r
cons badArgList = throwError $ NumArgs 2 badArgList

makeList :: [LispVal] -> ThrowsError LispVal
makeList [Number size, fill] = return $ List (take (fromIntegral size) (repeat fill))
makeList [Number size] = return $ List (take (fromIntegral size) (repeat (Number 0)))
makeList [] = throwError $ NumArgs 1 []
makeList badArgs = throwError $ TypeMismatch "list" (List badArgs)

append :: [LispVal] -> ThrowsError LispVal
append [List a, List []] = return $ List a
append [List [], List b] = return $ List b
append [List as, List bs] = return $ List (as ++ bs)
append [List as, (Dotted l r)] = return $ Dotted (as ++ l) r
append [List [], r] = return $ r
append badArgList = throwError $ NumArgs 2 badArgList

len :: [LispVal] -> ThrowsError LispVal
len [List []] = return $ Number 0
len [List l] = return $ Number (fromIntegral (length l))
len [badArg] = throwError $ TypeMismatch "list" badArg

rev :: [LispVal] -> ThrowsError LispVal
rev [l@(List [])] = return l
rev [List l] = return $ List (reverse l)
rev badArgList = throwError $ NumArgs 1 badArgList

-- ## String functions

makeString :: [LispVal] -> ThrowsError LispVal
makeString [Number size, Character fill] = return $ String (take (fromIntegral size) (repeat fill))
makeString [Number size] = return $ String (take (fromIntegral size) (repeat 'z'))
makeString [] = throwError $ NumArgs 1 []
makeString badArgs = throwError $ TypeMismatch "char" (List badArgs)

strLen :: [LispVal] -> ThrowsError LispVal
strLen [String ""] = return $ Number 0
strLen [String s] = return $ Number (fromIntegral (length s))
strLen [badArg] = throwError $ TypeMismatch "list" badArg

string :: [LispVal] -> ThrowsError LispVal
string [Character c] = return $ String [c]
string chars = mapM unpackchar chars >>= return . String

stringSlice :: [LispVal] -> ThrowsError LispVal
stringSlice [String s, Number start, Number end] = return $ String (slice s start end)
stringSlice args@[Number _, Number _, String _] = throwError $ BadArg "Argument order error, should be (str num num)" (List args)
stringSlice args@[Number _, String _, Number _] = throwError $ BadArg "Argument order error, should be (str num num)" (List args)
stringSlice args@[_, _, _] = throwError $ BadArg "Bad arguments, should be (str num num)" (List args)
stringSlice args = throwError $ BadArg "Bad arguments, should be (str num num)" (List args)

stringCopy :: [LispVal] -> ThrowsError LispVal
stringCopy [String s] = return $ String s
stringCopy [String s, Number start] = return $ String (slice s start (toInteger (length s)))
stringCopy [String s, Number start, Number end] = return $ String (slice s start end)
stringCopy args@[Number _, Number _, String _] = throwError $ BadArg "Argument order error, should be (str num num)" (List args)
stringCopy args@[Number _, String _, Number _] = throwError $ BadArg "Argument order error, should be (str num num)" (List args)
stringCopy args@[_, _, _] = throwError $ BadArg "Bad arguments, should be (str num num)" (List args)
stringCopy args = throwError $ BadArg "Bad arguments, should be (str num num)" (List args)

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [s@(String ""), _] = throwError $ BadArg "Cannot take from an empty string" s
stringRef [String s, idx@(Number i)] = if ((fromIntegral i) > length s)
                                          then throwError $ BadArg "String index out of bounds" idx
                                          else return $ Character (s!!(fromIntegral i))

stringCat :: [LispVal] -> ThrowsError LispVal
stringCat [] = return $ String ""
stringCat [s@(String _)] = return s
stringCat [(String l), (String r)] = return $ String (l ++ r)
stringCat strs = (mapM unpackstr strs) >>= return . String . concat

-- # vectors

makeVector :: [LispVal] -> ThrowsError LispVal
makeVector [Number size, fill] = return $ Vector (take (fromIntegral size) (repeat fill))
makeVector [Number size] = return $ Vector (take (fromIntegral size) (repeat (Number 0)))
makeVector [] = throwError $ NumArgs 1 []
makeVector badArgs = throwError $ TypeMismatch "list" (Vector badArgs)

vectorLen :: [LispVal] -> ThrowsError LispVal
vectorLen [Vector []] = return $ Number 0
vectorLen [Vector v] = return $ Number (fromIntegral (length v))
vectorLen [badArg] = throwError $ TypeMismatch "vector" badArg

vectorRef :: [LispVal] -> ThrowsError LispVal
vectorRef [v@(Vector []), _] = throwError $ BadArg "Cannot take from an empty vector" v
vectorRef [Vector s, idx@(Number i)] = if ((fromIntegral i) > length s)
                                          then throwError $ BadArg "Vector index out of bounds" idx
                                          else return (s!!(fromIntegral i))

