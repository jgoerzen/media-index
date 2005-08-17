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
import Scanutils

mb = 1048576

counter :: (Int -> IO ()) -> Int -> [a] -> IO [a]
counter dispf interval inplist =
    do res <- foldM writeit (0,[]) inplist
       return $ snd res
    where writeit (count, accum) x =
              do if count `mod` interval == 0
                    then do dispf count
                            hFlush stdout
                    else return ()
                 return (count + 1, x : accum)

scan dir num title = bracketCWD dir $
    do putStrLn " *** Scanning source directory..."
       items <- (recurseDirStat SystemFS "." >>= dispCount)
       putStr $ "Found " ++ (show $ length items) ++ " total items.  "
       let files = nodirs items
       let size = sum . map (\(_, fs) -> withStat fs vFileSize) $ files
       putStrLn $ (show (length files)) ++ " regular files, totaling " ++
                show (size `div` mb) ++ "MB"
       putStrLn " *** Determining MD5 sums and MIME types for files."
       m <- initMagic
       xfiles <- addMeta m files
       putStrLn $ show (length xfiles)
    where 
    dispCount = counter (\i -> putStr $ "Found " ++ (show i) ++ " items\r")
                        10

addMeta _ [] = return []
addMeta m ((fn, fs):xs) =
    catch (do ftype <- getMimeType m fn
              --putStrLn $ fn ++ " " ++ ftype
              --fmd5 <- getMD5 fn
              next <- addMeta m xs
              return $ (fn, fs, ftype, ""):next
          ) (\e -> do putStrLn $ "WARNING " ++ fn ++ ": " ++ (show e)
                      addMeta m xs)

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
              (["gather"] ++ extraargs ++
               ["-cl", "-sd", "-cm", "-px", "@uri",
                "-fx", ".doc,.DOC,.xls,.XLS,.ppt,.PPT", "H@" ++ fp ++ "estfxmsotohtml",
                "-fx", ".pdf,.PDF", "H@" ++ fp ++ "estfxpdftohtml", nm, "-"
               ]
              ) idxstring
       forceSuccess res
    where idxstring = unlines . map (\(osfn, url) -> osfn ++ "\t" ++ url) $ idxfiles
          fp = "/usr/share/hyperestraier/filter/" -- FIXME



