{-# LANGUAGE OverloadedStrings #-}

import Database.Persist.Sqlite

main = withSqlConn "C:\\Users\\mind-s\\Documents\\GitHub\\LR2\\LR2beta3MIND.R\\LR2files\\Database\\tag.db" $ runSqlConn $ return ()
