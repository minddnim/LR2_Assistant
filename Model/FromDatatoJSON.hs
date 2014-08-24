-- OverloadedStrings 言語拡張を使うとダブルクオートで囲んだ文字列を、
-- Text、ByteString リテラルとして扱ってくれるようになります。 
{-# LANGUAGE OverloadedStrings #-} 

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List as L
import System.Environment(getArgs)
import MusicDataParser

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ "./" ++ "defHtmlFileName.txt"
  let outFileName = head $ lines contents
      outJsonFileName = outFileName ++ ".json"
  musicContents <- readFile $ head args
  let musicDataTsv = T.pack $ unlines $ tail $ lines musicContents
      musicDatas = getMusicDataSrc musicDataTsv
      defData = defVarMNameStr musicDatas
  T.writeFile outJsonFileName defData

defVarMNameStr :: [MusicData] -> T.Text
defVarMNameStr datas = T.concat varMName
  where mNames = getVarMNames datas
        varMName = ["[{",T.concat mNames,"}]"]

getVarMNames :: [MusicData] -> [T.Text] 
getVarMNames datas = L.intersperse "},{" $ map getVarMName datas

getVarMName :: MusicData -> T.Text
getVarMName x = T.concat dataDef
  where level = T.concat ["\"level\":", (convData.dLevel) x]
        musicTitle = T.concat ["\"title\":", (convData.dMusicTitle) x]
        bmsID = T.concat ["\"lr2_bmsid\":", dBmsID x]
        orgArtist = T.concat ["\"artist\":", (convData.dOrgArtist) x]
        orgArtistURL = T.concat ["\"url\":", (convData.dOrgArtistURL) x]
        scoreSite = T.concat ["\"name_diff\":", (convData.dScoreSite) x]
        scoreSiteURL = T.concat ["\"url_diff\":", (convData.dScoreSiteURL) x]
        comment = T.concat ["\"comment\":", (convData.dComment) x]
        dataDef = L.intersperse "," [level,musicTitle,bmsID,orgArtist,orgArtistURL,scoreSite,scoreSiteURL,comment]

convData :: T.Text -> T.Text
convData str = T.concat ["\"", str, "\""]
