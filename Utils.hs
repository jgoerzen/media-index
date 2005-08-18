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

module Utils where
import Control.Monad
import System.IO

counter :: (Int -> IO ()) -> Int -> [a] -> IO [a]
counter dispf interval inplist =
    zipWithM writeit inplist [0..]
    where writeit item count =
              do if count `mod` interval == 0
                    then do dispf count
                            hFlush stdout
                    else return ()
                 return item

mb :: Num a => a
mb = 1048576

