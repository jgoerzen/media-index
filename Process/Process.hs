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

module Process.Process where

import Config
import Data.Char
import Scan.Scan
import Types
import FileDB.DB
import System.IO

filtertype :: [String] -> [FileRec] -> [FileRec]
filtertype tl l = filter (\rec -> elem (map toLower (frname rec)) tll) l
                  where tll = map (map toLower) tl

procfuncs = [(["application/x-zip"], proczip)]

processit conn base fsdir num title files =
    do putStrLn " *** Adding files to DB..."
       hFlush stdout
       mapM_ (addFileRec conn num) files
       mapM_ (runproc files) procfuncs
    where runproc files (typelist, procfunc) =
              let filestoproc = filtertype typelist files
                  in if length filestoproc > 0
                     then procfunc filestoproc
                     else return ()


          
proczip files = return ()