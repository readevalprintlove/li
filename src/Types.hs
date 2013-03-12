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

module Types
    ( Env
    , nullEnv
    , showVal
    , liftThrows
    , LispVal(..)
    , DeferredCode(..)
    , LispError(..)
    , DynamicWinders(..)
    , ThrowsError
    , IOThrowsError)
where
import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Error (Error, ErrorT, noMsg, strMsg, throwError)
import System.IO
import Data.IORef
import Data.Array (Array (..))
import Data.Ratio
import Data.Complex

-- # Environment

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

-- Scheme-like data types

data DeferredCode = CodeBody [LispVal]
                  | HostBody { k :: (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal),
                                  kargs :: (Maybe [LispVal])}

data DynamicWinders = DynamicWinders { before :: LispVal,
                                       after :: LispVal}

data LispVal = Atom String
             | List [LispVal]
             | Dotted [LispVal] LispVal
             | Vector (Array Int LispVal)
             | Number Integer
             | Environment Env
             | Ratio Rational
             | Complex (Complex Double)
             | Float Double
             | String String
             | Character Char
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params  :: [String],
                      vararg  :: (Maybe String),
                      body    :: [LispVal],
                      context :: Env}
             | Partial { fun  :: LispVal,
                         args :: [LispVal],
                         has  :: Integer,
                         need :: Integer}
             | Continuation { context :: Env,
                              cc :: (Maybe DeferredCode),
                              next :: (Maybe LispVal),
                              ret :: (Maybe [LispVal]),
                              wind :: (Maybe [DynamicWinders])}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | KFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

-- # Print reprs

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Character char) = "\"" ++ [char] ++ "\""
showVal (Atom name) = name
showVal (Float contents) = show contents
showVal (Complex contents) = show contents
showVal (Ratio contents) = show (numerator contents) ++ "/" ++ show (denominator contents)
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Environment env) = "<environment>"
showVal (Vector contents) = "[" ++ (show contents) ++ "]"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (Dotted h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, context = env}) =
  "(lambda (" ++ unwords (map show args) ++
     (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Partial _ _ _ need) = "<partial functional needing " ++ show need ++ " more args>"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
showVal (KFunc _) = "<continuation function>"

-- hook into Haskell's show typeclass
instance Show LispVal where show = showVal
instance Show LispError where show = showError


data LispError = NumArgs Integer [LispVal]
               | CallCCBadArgs [LispVal]
               | CallCCNoArgs String
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message funcName) = message ++ ": " ++ funcName
showError (NumArgs expected got) = "Expected " ++ show expected ++ " args; got " ++ unwordsList got
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (CallCCBadArgs got) = "Bad call to call/cc, expected 1 argument but got [" ++ unwordsList got ++ "]"
showError (CallCCNoArgs got) = "Incomplete call to call/cc, expected 1 argument but got " ++ got

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val
