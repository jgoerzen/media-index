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

module Scan(scan, index) where
import Config
import MissingH.Path
import MissingH.Cmd
import System.Posix.Files
import MissingH.IO.HVFS
import Control.Monad

scan dir num title = 
    do files <- bracketCWD dir (recurseDirStat SystemFS ".")
       writefiles dir num title (map convfile files)
    where convfile (fn, fs) = (drop 2 fn, fs)

writefiles dir num title files =
    do id <- fileDir
       res <- foldM writeit (1,[]) files
       writeFile (id ++ "/" ++ num ++ ".idx.txt") (unlines (snd res))
    where writeit (counter,accum) (fn,fs) =
              do if counter `mod` 10 == 0
                    then putStrLn $ "Processed " ++ num ++ " files"
                    else return ()
                 let filesize = withStat fs vFileSize
                 let entry = fn ++ "\t" ++ (show filesize)
                 return (counter + 1,
                         entry : accum)

                 

index dir num title = brackettmpdir "/tmp/media-index.XXXXXX" (\td ->
   do createSymbolicLink dir (td ++ "/" ++ num)
      nd <- namazuDir
      safeSystem "mknmz" ["-O", nd,
                          "--replace=s#" ++ td ++ "##",
                          (td ++ "/" ++ num)]
                                                             )
