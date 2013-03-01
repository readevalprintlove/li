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

module K (nullK, cps, cpsArgs)
where
import Types


nullK :: Env -> LispVal
nullK env = Continuation env Nothing Nothing Nothing Nothing


cps :: Env -> LispVal -> (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal) -> LispVal
cps env cont@(Continuation _ _ _ _ dynWind) cps =
    Continuation env
                 (Just (HostBody cps Nothing))
                 (Just cont)
                 Nothing
                 dynWind
cps env cont cps =
    Continuation env
                 (Just (HostBody cps Nothing))
                 (Just cont)
                 Nothing
                 Nothing


cpsArgs :: Env -> LispVal -> (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal) -> [LispVal] -> LispVal
cpsArgs env cont@(Continuation _ _ _ _ dynWind) cps args =
        Continuation env
                     (Just (HostBody cps (Just args)))
                     (Just cont)
                     Nothing
                     dynWind
cpsArgs env cont cps args =
        Continuation env
                     (Just (HostBody cps (Just args)))
                     (Just cont)
                     Nothing
                     Nothing

