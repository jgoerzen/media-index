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

module Config (searchdir, dbdir, mknmzArgs)
where

import MissingH.ConfigParser
import MissingH.Either
import MissingH.Str
import System.Directory

basecp = forceEither $ set emptyCP "DEFAULT" "mknmzargs" "[]"

getcp = do hd <- getHomeDirectory
           val <- readfile basecp (hd ++ "/.media-indexrc")
           return $ forceEither val

genericget :: String -> IO String
genericget opt = do cp <- getcp
                    return $ forceEither $ get cp "DEFAULT" opt

searchdir = genericget "searchdir"
dbdir = genericget "dbdir"

mknmzArgs :: IO [String]
mknmzArgs = do a <- genericget "mknmzargs"
               return $ read a
