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

module Numerics
where
import Types
import Control.Monad.Error
import Helpers


math :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
math op singleVal@[_] = throwError $ NumArgs 2 singleVal
math op [l, r] = return (Number $ op (extricate (unpacknum l)) (extricate (unpacknum r)))
math op args = throwError $ NumArgs 2 args

numerics :: [(String, [LispVal] -> ThrowsError LispVal)]
numerics = [("+", math (+)),
            ("-", math (-)),
            ("*", math (*)),
            ("/", math div),
            ("mod", math mod),
            ("quot", math quot),
            ("rem", math rem)]

