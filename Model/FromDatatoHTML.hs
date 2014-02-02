-- OverloadedStrings 言語拡張を使うとダブルクオートで囲んだ文字列を、
-- Text、ByteString リテラルとして扱ってくれるようになります。 
{-# LANGUAGE OverloadedStrings #-} 

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List as L
import System.Environment(getArgs)
import System.Argv0
import MusicDataParser

main :: IO()
main = do
  args <- getArgs
  exeFileName <- getArgv0
  contents <- readFile $ getDirPath (show exeFileName) ++ "outHtmlSrc.txt"
  let outFileName = head $ lines contents
      outHtmlFileName = outFileName ++ ".html"
  musicContents <- readFile $ head args
  headerContents <- readFile $ getDirPath (show exeFileName) ++ "tableHeader.html"
  footerContents <- readFile $ getDirPath (show exeFileName) ++ "tableFooter.html"
  let outputSrc = T.concat [T.pack headerContents,defData,forStr,ifStr musicDatas,T.pack footerContents]
      musicDataTsv = T.pack $ unlines $ tail $ lines musicContents
      musicDatas = getMusicDataSrc musicDataTsv
      defData = defVarMNameStr musicDatas
  T.writeFile　outHtmlFileName outputSrc

getDirPath :: String -> String
getDirPath str = tail $ reverse $ dropWhile ('\\' /=) $ reverse $ dropWhile ('"' /=) str

derimita :: T.Text
derimita = "\t"

--defineMusicDataStart :: [MusicData] -> T.Text
--defineMusicDataStart x = T.pack "///////////////////曲のデータ(" `T.append` dataNumStr `T.append` T.pack "番まで使ってるよ！)///////////////////\n"
--  where dataNumStr = T.pack $ show $ length x

defVarMNameStr :: [MusicData] -> T.Text
defVarMNameStr datas = T.concat varMName
  where mNames = getVarMNames datas
        varMName = ["var mname = [",T.concat mNames,"];\n"]

getVarMNames :: [MusicData] -> [T.Text] 
--getVarMNames datas = L.intersperse "," $ map getVarMName $ zip [1..] datas 
getVarMNames datas = L.intersperse "," $ zipWith (curry getVarMName) [1 ..] datas

getVarMName :: (Int, MusicData) -> T.Text
getVarMName (n,x) = T.concat dataDef
  where level = dLevel x
        musicTitle = dMusicTitle x
        bmsID = dBmsID x
        orgArtist = dOrgArtist x
        orgArtistURL = dOrgArtistURL x
        orgStr | orgArtist /= "" && orgArtistURL /= "" = "<a href='" `T.append` orgArtistURL `T.append`"'>" `T.append` orgArtist `T.append` "</a>"
               | orgArtist /= "" = orgArtist
               | orgArtistURL /= "" = "<a href='" `T.append` orgArtistURL `T.append`"'>" `T.append` "___________" `T.append` "</a>"
               | otherwise = ""
        scoreSite = dScoreSite x
        scoreSiteURL = dScoreSiteURL x
        scoreStr | scoreSite /= "" && scoreSiteURL /= "" = "<a href='" `T.append` scoreSiteURL `T.append` "'>" `T.append` scoreSite `T.append` "</a>"
                 | scoreSite /= "" = scoreSite
                 | scoreSiteURL /= "" = "<a href='" `T.append` scoreSiteURL `T.append`"'>" `T.append` "___________" `T.append` "</a>"
                 | otherwise = ""
        comment = dComment x
        dataDef = ["\n[",T.pack (show n),",\"",level,p,musicTitle,p,bmsID,p,orgStr,p,scoreStr,p,comment,"\",\n]"]
        p = "\",\n\""

forStr :: T.Text
forStr = "  for (var i=0;i<mname.length;i++) {\n"

ifStr :: [MusicData] -> T.Text
ifStr datas = "if(" `T.append` condStr `T.append` ")\n"
  where condStr = getCondStr datas

getCondStr :: [MusicData] -> T.Text
getCondStr datas = T.concat $ L.intersperse "||" condIdListText 
  where (x:xs) = zip [1..] datas
        condIdList = map fst $ remCond xs [x] 
        condIdListStr = map show condIdList
        condIdListText = map (("mname[i][0]==" `T.append`) . T.pack) condIdListStr



remCond :: [(Int, MusicData)] -> [(Int, MusicData)] -> [(Int, MusicData)]
remCond [] ret = reverse ret
remCond [x] ret | dLevel (snd x) /= dLevel (snd (head ret)) = remCond [] (x:ret)
                | otherwise = remCond [] ret
remCond (x:y:xs) ret | dLevel (snd x) /= dLevel (snd y) = remCond (y:xs) (y:ret)
                     | otherwise = remCond (y:xs) ret
