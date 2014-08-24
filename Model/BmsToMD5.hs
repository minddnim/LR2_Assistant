{-# LANGUAGE OverloadedStrings #-}

import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 as B hiding (map, zip, writeFile, concatMap)
import System.Environment(getArgs)
import Data.Map as M hiding (map)
import Network.HTTP
import Data.Text as T hiding (concatMap, map, zip)

musicURLBase :: String
musicURLBase = "http://www.dream-pro.info/~lavalse/LR2IR/search.cgi?mode=ranking&bmsmd5="

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

main :: IO()
main = do
  let tableFile = "bmsToMD5Table.txt"
  tableList <- B.readFile tableFile
  args <- getArgs
  readFilePaths <- mapM B.readFile args
  let bmsMd5 = map md5 readFilePaths
      tbl = zip (map (B.pack.show) bmsMd5) (map B.pack args) ++ map keyDivVal (B.lines tableList)
      md5s = map B.unpack $ (keys. M.fromList) tbl
      urlMD5s = map (musicURLBase ++) md5s
  contents <- mapM openURL urlMD5s
  let bmsIdMD5Tbl = zip (map T.pack md5s) (map (getBmsId . T.pack) contents)
  print $ T.unpack $ T.unlines $ map keyDivValInv bmsIdMD5Tbl
  writeFile ("new_" ++ tableFile) $ T.unpack $ T.unlines $ map keyDivValInv bmsIdMD5Tbl
  _ <- getLine
  return()

createTblMap :: ByteString -> Map ByteString ByteString
createTblMap "" = M.empty
createTblMap txt = fromList $ map keyDivVal (B.lines txt)

keyDivVal :: ByteString -> (ByteString, ByteString)
keyDivVal str = (key, B.tail val)
  where (key, val) = B.span (== ',') str

keyDivValInv :: (Text, Text) -> Text
keyDivValInv (k, v) = k `T.append` T.pack "," `T.append` v

getBmsId :: Text -> Text
getBmsId str = str
--getBmsId str = T.concat $ map (convText . stripPrefix endStr . convText . stripPrefix beginStr) (T.lines str)
  where beginStr = T.pack "<h3>情報 <a href=\"search.cgi?mode=edit&bmsid="
        endStr = T.pack "\">[編集]</a></h3>"

convText :: Maybe Text -> Text
convText (Just a) = a
convText Nothing = T.pack "" 