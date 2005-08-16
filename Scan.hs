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

module Scan(scan, indexscan, index) where
import Config
import MissingH.Path
import MissingH.Cmd
import System.Posix.Files
import MissingH.IO.HVFS
import Control.Monad

scan dir num title = bracketCWD dir $
    do files <- recurseDirStat SystemFS "."
       writefiles dir num title (map convfile files)
    where convfile (fn, fs) = (num ++ tail fn, fs)

writefiles dir num title files =
    do id <- fileDir
       res <- foldM writeit (1,[]) files
       writeFile (id ++ "/" ++ num ++ ".idx.txt") (unlines (snd res))
    where writeit (counter,accum) (fn,fs) =
              do if counter `mod` 100 == 0
                    then putStr $ "Processed " ++ (show counter) ++ " files\r"
                    else return ()
                 let filesize = withStat fs vFileSize
                 let entry = fn ++ "\t" ++ (show filesize)
                 return (counter + 1,
                         entry : accum)

index dir num title = brackettmpdir "/tmp/media-index.XXXXXX" (\td ->
   do createSymbolicLink dir (td ++ "/" ++ num)
      args <- mknmzArgs
      nd <- namazuDir
      safeSystem "mknmz" $
                     args ++ ["-Y", "-f", "mknmzrc", "-O", nd,
                              "--replace=s#" ++ td ++ "#/CONTENT#",
                              (td ++ "/" ++ num)]
                                                             )

indexscan dir num title = brackettmpdir "/tmp/media-indexis.XXXXXX" (\td ->
    do nd <- namazuDir
       id <- fileDir
       args <- mknmzArgs
       let fn = (td ++ "/" ++ num ++ ".idx.txt")
       createSymbolicLink (id ++ "/" ++ num ++ ".idx.txt") fn
       safeSystem "mknmz" $
                      args ++ ["-Y", "-f", "mknmzrc", "-O", nd,
                           "--replace=s#" ++ fn ++ "#/INDEX/" ++ num ++ "#",
                           "--media-type=text/plain", fn]
                                                                    )