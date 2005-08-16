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
import System.IO
import Control.Monad

scan dir num title = bracketCWD dir $
    do files <- recurseDirStat SystemFS "."
       writefiles dir num title (map (convfile num) files)
       putStrLn "Adding files to index..."
       index dir num title files

convfile num (fn, fs) = (num ++ tail fn, fs)
nodirs = filter (\(fn, fs) -> not $ withStat fs vIsDirectory)

writefiles dir num title files =
    do putStrLn "Scanning input directory..."
       id <- fileDir
       res <- foldM writeit (1,[]) files
       writeFile (id ++ "/" ++ num ++ ".idx.txt") (unlines (snd res))
    where writeit (counter,accum) (fn,fs) =
              do if counter `mod` 10 == 0
                    then do putStr $ "Processed " ++ (show counter) ++ " files\r"
                            hFlush stdout
                    else return ()
                 let filesize = withStat fs vFileSize
                 let entry = fn ++ "\t" ++ (show filesize)
                 return (counter + 1,
                         entry : accum)

index dir num title files = 
    runhe [] filelist
    where filelist = map mkfilen files
          mkfilen :: (String,a) -> (String, String)
          mkfilen (fn,fs) = (fn, "file:///CONTENTS/" ++ (fst $ convfile num (fn,fs)))

indexscan dir num title = 
    do id <- fileDir
       runhe ["-ft"] 
                 [(id ++ "/" ++ num ++ ".idx.txt",
                   "file:///INDEX/" ++ num)]

runhe :: [String] -> [(String, String)] -> IO ()
runhe extraargs idxfiles =
    -- idxfiles is a list of [(osfilename, heurl)]
    do nm <- namazuDir
       res <- pipeTo "estcmd"
              (extraargs ++
               ["gather", "-cl", "-sd", "-cm", "-px", "@uri",
                "-fx", ".doc,.DOC,.xls,.XLS,.ppt,.PPT", "H@estfxmsotohtml",
                "-fx", ".pdf,.PDF", "H@estfxpdftohtml", nm, "-"
               ]
              ) idxstring
       forceSuccess res
    where idxstring = unlines . map (\(osfn, url) -> osfn ++ "\t" ++ url) $ idxfiles



