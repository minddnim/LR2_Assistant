import Network.HTTP
import Data.List

main :: IO()
main = do
	htmlSrc <- openURL "http://flowermaster.web.fc2.com/lrnanido_sara.html"
	let musicDatas = defMusicSrc $ lines htmlSrc
	writeFile "musicData.txt" $ unlines musicDatas

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest　x)

defMusicSrc :: [String] -> [String]
defMusicSrc = searchMusicDefEnd . searchMusicDefStart

searchMusicDefStart :: [String] -> [String]
searchMusicDefStart (x:xs) | "var mname" `isInfixOf` x = x:xs
													 | otherwise = searchMusicDefStart xs

searchMusicDefEnd :: [String] -> [String]
searchMusicDefEnd src = searchMusicDefEnd' src [""]
  where searchMusicDefEnd' (x:xs) ret | "];" `isInfixOf` x = reverse ret
																      | otherwise = searchMusicDefEnd' xs (x:ret)
