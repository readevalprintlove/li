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

module Egal
where
import Types
import Helpers
import Control.Monad.Error (throwError, catchError, liftM)

-- For any type that is an instance of Eq, you can define an Unpacker that 
-- takes a function from LispVal to that type. May throw an error.
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackeq :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackeq l r (AnyUnpacker unpacker) = 
             do unpackedL <- unpacker l
                unpackedR <- unpacker r
                return $ unpackedL == unpackedR
        `catchError` (const $ return False)


-- equality support

eqSeq :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqSeq eqFun [(List l), (List r)] = return $ Bool $ (length l == length r) && 
                                                         (all eqPair $ zip l r)
      where eqPair (x1, x2) = case eqFun [x1, x2] of
                                    Left err -> False
                                    Right (Bool val) -> val

egal :: [LispVal] -> ThrowsError LispVal
egal [Atom(l), Atom(r)] = Right $ Bool (l == r)
egal [l@(List _), r@(List _)] = eqSeq egal [l, r]
egal [l, r] = do
    let unpackers = [AnyUnpacker unpacknum, AnyUnpacker unpackstr, AnyUnpacker unpackbool] 
    eq <- liftM or $ mapM (unpackeq l r) unpackers
    return $ Bool $ eq
egal bad = throwError $ NumArgs 2 bad
