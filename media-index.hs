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

module Main where

import Config
import System.Environment
import Scan.Scan
import MissingH.IO
import FileDB.DB
import Database.HSQL
import Process

syntax = do
         putStrLn "Syntax:"
         putStrLn "media-index /media/dir number title"

main = do
       args <- getArgs
       optimizeForBatch
       case args of
          [dir, num, title] -> process dir num title
          _ -> syntax

process dir num title = 
    do c <- initdb
       files <- scan "" dir num title
       process c "" dir num title
       putStrLn " *** Adding files to DB..."
       hFlush stdout
       setFilesRec c num files
       putStrLn " *** Noting disc in DB..."
       hFlush stdout
       addDisc c num title
       putStrLn " *** Cleaning up..."
       hFlush stdout
       disconnect c
       return ()
       --indexscan dir num title
       --index dir num title
       
