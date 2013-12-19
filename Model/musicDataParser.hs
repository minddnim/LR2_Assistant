import Data.Attoparsec.ByteString.Char8 as AP
import Data.ByteString.Char8 as B

data MusicData = MusicData {
  dID :: ByteString,
  dLevel :: ByteString,
  dMusicTitle :: ByteString,
  dBmsID :: ByteString,
  dOrgArtist :: ByteString,
--  orgArtistURL :: ByteString,
  dScoreSite :: ByteString,
--  scoreSiteURL :: ByteString,
  dComment :: ByteString
} deriving (Show)

pMusicData :: Parser MusicData
pMusicData = do
  char '['
  iD <- pID
  char ','
  level <- pLevel
  char ','
  musicTitle <- pMusicTitle
  char ','
  bmsID <- pBmsID
  char ','
  orgArtist <- pOrgArtist
  char ','
  scoreSite <- pScoreSite
  char ','
  comment <- pComment
  char ','
  char ']'
  return $ MusicData iD level musicTitle bmsID orgArtist scoreSite comment
--  return $ MusicData iD level musicTitle orgArtist orgArtistURL scoreSite scoreSiteURL comment

pID :: Parser ByteString
pID = AP.takeWhile $ notInClass ","

pLevel :: Parser ByteString
pLevel = do
  char '"'
  level <- AP.takeWhile $ notInClass "\""
  char '"'
  return level

pMusicTitle :: Parser ByteString
pMusicTitle = do
  char '"'
  musicTitle <- AP.takeWhile $ notInClass "\""
  char '"'
  return musicTitle

pBmsID :: Parser ByteString
pBmsID = do
  char '"'
  bmsID <- AP.takeWhile $ notInClass "\""
  char '"'
  return bmsID

pOrgArtist :: Parser ByteString
pOrgArtist = do
  char '"'
  orgArtist <- AP.takeWhile $ notInClass "\""
  char '"'
  return orgArtist

pScoreSite :: Parser ByteString
pScoreSite = do
  char '"'
  scoreSite <- AP.takeWhile $ notInClass "\""
  char '"'
  return scoreSite

pComment :: Parser ByteString
pComment = do
  char '"'
  comment <- AP.takeWhile $ notInClass "\""
  char '"'
  return comment


main :: IO ()
main = print $ parseOnly pMusicData (B.pack "[61,\"<font color='red'>◎1</font>\",\"reikon girl@Genso-kyo (NORMAL7)\",\"120135\",\"\",\"<a href='http://cid-578814acc2bd5a74.skydrive.live.com/browse.aspx/.Public/BMS'>sun3</a>\",\"\",],")