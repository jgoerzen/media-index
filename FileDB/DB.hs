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

module FileDB.DB where

import Config
import Database.HSQL
import Database.HSQL.SQLite3

init :: IO Connection

init = 
    do putStrLn " *** Initializing database system..."
       dbpath <- dbdir
       handleSqlError $
         do c <- connect (dbpath ++ "/media-index-main") ReadWriteMode
            initTables c
            return c

initTables conn =
    do t <- tables conn
       let t2 = map (map toUpper) t
       if not (elem "MISTORE" t2)
          then do execute conn "CREATE TABLE mistore (api TEXT)"
                  execute conn "INSERT INTO mistore VALUES ('media-index1')"
          else return ()
       if not (elem "MIFILES" tw)
          then do execute conn "CREATE TABLE mifiles (" ++
                   "discid TEXT, filename TEXT, filesize INTEGER, " ++
                   "md5 TEXT, mimetype TEXT"
                  execute conn "CREATE UNIQUE INDEX mifilespri ON mifiles " ++
                          "(discid, filename)"
                  execute conn "CREATE INDEX mifilesmd5 ON mifiles " ++
                          "(md5)"
                  execute conn "CREATE INDEX mifilesfiles ON mifiles (filename)"
          else return ()
       if not (elem "MIDISCS" tw)
          then execute conn "CREATE TABLE midiscs (discid TEXT, discdescrip TEXT)"
               execute conn "CREATE UNIQUE INDEX midiscspri ON midiscs (discid)"
          else return ()


{- | Propogate SQL exceptions to IO monad. -}
handleSqlError :: IO a -> IO a
handleSqlError action =
    catchSql action handler
    where handler e = fail ("SQL error: " ++ show e)
