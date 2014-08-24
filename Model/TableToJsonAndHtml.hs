{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Attoparsec as AP (parseOnly)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import qualified Data.Text as T (pack, unpack, unlines)
import qualified Data.Text.IO as T (writeFile)
import Codec.Binary.UTF8.String
import Network.HTTP
import Data.List as L
import System.Environment(getArgs)
import MusicDataParser
import ConvDataToHTML(defVarMNameStr)


data BmsRecode = BmsRecode {
  btColor :: String,
  btLevel :: String,
  btTitle :: String,
  btLr2BmsId :: Int,
  btArtist :: String,
  btUrl :: String,
  btNameDiff :: String,
  btUrlDiff :: String,
  btComment :: String,
  btMd5 :: String
} deriving (Show, Eq)

instance FromJSON BmsRecode where
  parseJSON (Object v) = BmsRecode
                         <$> v .:? "color"
                         <*> v .:  "level"
                         <*> v .:? "title"
                         <*> v .:? "lr2_bmsid"
                         <*> v .:? "artist"
                         <*> v .:? "url"
                         <*> v .:? "name_diff"
                         <*> v .:? "url_diff"
                         <*> v .:? "comment"
                         <*> v .:  "md5"
  parseJSON _ = mzero

convBmsRecodeToCsv :: BmsRecode -> String
convBmsRecodeToCsv r = intercalate "\t" [level, title, show lr2BmsId, artist, url, nameDiff, urlDiff, comment, md5]
  where comment = btComment r
        title = btTitle r
        url = btUrl r
        artist = btArtist r
        nameDiff = btNameDiff r
        urlDiff = btUrlDiff r
        level = btLevel r
        lr2BmsId = btLr2BmsId r
        md5 = btMd5 r

parseBmsRecodeToJson :: Value -> Result BmsRecode
parseBmsRecodeToJson = fromJSON

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ "./" ++ "defHtmlFileName.txt"
  let outFileName = head $ lines contents
      outHtmlFileName = outFileName ++ ".html"
  musicContents <- readFile $ head args
  headerContents <- readFile $ "./" ++ "tableHeader.html"
  footerContents <- readFile $ "./" ++ "tableFooter.html"
  let outputSrc = T.concat [T.pack headerContents,defData,forStr,ifStr musicDatas,T.pack footerContents]
      musicDataTsv = T.pack $ unlines $ tail $ lines musicContents
      musicDatas = getMusicDataSrc musicDataTsv
      defData = defVarMNameStr musicDatas
  T.writeFile outHtmlFileName $ (T.pack . encodeString . T.unpack) outputSrc

  htmlSrc <- openURL "http://minddnim.web.fc2.com/bms_sara/sara_1/data.json"
  let jsons = convStrToJson ((encodeString .tail) htmlSrc) []
      bmsRecode = mapM (AP.parseOnly json . C.pack) jsons
      columnName = intercalate "\t" ["level", "title", "lr2_bmsid", "artist", "url", "name_diff", "url_diff", "comment", "md5"] :: String
  case bmsRecode of
    Left _ -> error "json parse error."
    Right records -> 
      T.writeFile "data.tsv" $ T.pack . unlines $ columnName : map (decodeString . convBmsRecodeToCsv . forceSuccess . parseBmsRecodeToJson) records

convStrToJson :: String -> [String] -> [String]
convStrToJson [] ret = reverse ret
convStrToJson str ret = convStrToJson remstr [record ++ "}"] ++ ret
  where (record, _:_:remstr) = break (== '}') str

forceSuccess :: Result BmsRecode -> BmsRecode
forceSuccess record =
  case record of
    Success a -> a
    Error str -> error str
