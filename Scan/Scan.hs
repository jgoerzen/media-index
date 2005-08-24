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

module Scan.Scan(scan) where
import Magic
import Data.Maybe
import Config
import Control.Monad
import MissingH.Path
import MissingH.Cmd
import MissingH.IO
import System.Posix.Files
import MissingH.IO.HVFS
import System.IO
import Control.Monad
import Scan.Scanutils
import Utils
import Types

scan fsroot dir num title = 
    do m <- initMagic
       runscan m fsroot dir num title

runscan m base dir num title = bracketCWD dir $
    do putStrLn " *** Scanning source " ++ dir ++ "..."
       items <- (recurseDirStat SystemFS "." >>= dispCount)
       let files = map (\(fn, fs) -> (base ++ "/" ++ fn, fs)) items
       putStrLn $ "Found " ++ (show $ length files) ++ " total items.  "
       let size = sum . map (\(_, fs) -> withStat fs vFileSize) $ files
       putStrLn $ (show (length files)) ++ " non-directories, totaling " ++
                show (size `div` mb) ++ "MB"
       putStrLn " *** Determining MD5 sums and MIME types for files."
       xfiles <- (addMeta m files >>= md5progress (length files))
       putStrLn ""
       putStrLn $ show (length xfiles) ++ " remain to process."
       return xfiles
    where 
    dispCount = counter (\i -> putStr $ "Found " ++ (show i) ++ " items\r")
                        10
    md5progress fc =
        counter (\i -> putStr $ "File " ++ (show i) ++ " of " ++ (show fc) ++
                                " (" ++ show (i * 100 `div` fc) ++ "%)\r")
                10

addMeta :: Magic -> [(FilePath, HVFSStatEncap)] -> IO [FileRec]
addMeta m inp =
    do c <- lazyMapM conv inp
       return $ mapMaybe id c
    where special (fn, fs) t = return $ Just $ 
                               FileRec {frname = fn,
                                        frsize = fromIntegral $ withStat fs vFileSize,
                                        frmd5 = "",
                                        frmime = t}
          conv (fn,fs) = 
              if withStat fs vIsDirectory then
                 special (fn, fs) "inode/directory"
              else if withStat fs vIsBlockDevice then
                   special (fn, fs) "inode/blockdevice"
              else if withStat fs vIsCharacterDevice then
                   special (fn, fs) "inode/chardevice"
              else if withStat fs vIsNamedPipe then
                   special (fn, fs) "inode/fifo"
              else if withStat fs vIsSocket then
                   special (fn, fs) "inode/socket"
              else if withStat fs vIsSymbolicLink then
                   special (fn, fs) "x-inode/symbolic-link"
              else catch
                         (do let fsize = withStat fs vFileSize
                             ftype <- getMimeType m fn
                             fmd5 <- getMD5Sum fn (withStat fs vFileSize)
                             --putStrLn $ fn ++ " " ++ ftype ++ " " ++ fmd5
                             return $ Just $ FileRec {frname = fn,
                                                      frsize = fromIntegral fsize,
                                                      frmd5 = fmd5,
                                                      frmime = ftype}
                         ) (\e -> do putStrLn $ 
                                      "WARNING " ++ fn ++ ": " ++ (show e)
                                     return Nothing
                           )

convfile num (fn, fs) = (num ++ tail fn, fs)

-- FIXME: should probably select just files and symlinks

nodirs = filter (\(fn, fs) -> not $ withStat fs vIsDirectory)
{-

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



-}