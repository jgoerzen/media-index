{- 
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

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

module Scanutils(initMagic, getMimeType) where
import Magic
import Data.List

initMagic :: IO Magic
initMagic = do m <- magicOpen [MagicMime]
               magicLoadDefault m
               return m

getMimeType :: Magic -> String -> IO String
getMimeType m fn = 
    do t <- magicFile m fn
       return $ takeWhile ((/=) ';') t