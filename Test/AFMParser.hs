module Main where
    
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Data.Char(toUpper)
import System.Environment
import Data.Maybe(isJust,fromJust,mapMaybe)
import qualified Data.IntMap as IM
import Data.List(intersperse)
import qualified Data.Map as M

capitalize :: String -> String
capitalize [] = []
capitalize (h:t) = toUpper h : t

line :: Parser ()
line = do string "\r\n"
          return ()

toEndOfLine :: Parser ()
toEndOfLine = do many (noneOf "\r\n")
                 line
                 return ()
                 
getString :: String -> Parser String
getString s = do 
               string s
               spaces
               c <- many1 (alphaNum <|> oneOf "-+")
               line
               return c
               
getName :: String -> Parser String
getName s = do 
              string s
              spaces
              c <- alphaNum >> many (alphaNum <|> oneOf " -+")
              line
              return c
               
getInt :: String -> Parser Int
getInt s = do c <- getString s
              return $ read c
              
getFloat :: String -> Parser Double
getFloat s = do string s
                spaces
                c <- many1 (alphaNum <|> oneOf ".-+")
                line
                return $ read c
              
getBool :: String -> Parser Bool
getBool s = do c <- getString s
               return $ read (capitalize c)
               
data CharacterSet = ExtendedRoman
                  | Special
                  deriving(Eq,Read,Show)
    
data Weight = Medium
            | Bold
            | Roman
            deriving(Eq,Read,Show)
               
getCharacterSet :: String -> Parser CharacterSet
getCharacterSet s = do c <- getString s
                       return $ read c
                       
getWeigth :: String -> Parser Weight
getWeigth s = do c <- getString s
                 return $ read c
                 
  
array :: Parser [String]  
array = sepEndBy (many1 (oneOf "-+0123456789")) (many1 (oneOf " "))
                 
getArray :: String -> Parser [Double]
getArray s = do string s
                spaces
                c <- array
                line
                return . map read $ c
                 
comment :: String -> Parser ()
comment s = do string s
               toEndOfLine
             
comments :: Parser ()
comments = do many1 (comment "Comment")
              return ()
              
data EncodingScheme = AdobeStandardEncoding 
                    | FontSpecific
                    deriving(Eq,Read,Show)
                
getEncoding :: String -> Parser EncodingScheme
getEncoding s = do c <- getString s
                   return $ read c
                                           
number :: Parser Int
number  = do c <- many1 (oneOf "-+0123456789")
             return $ read c
         
data Elem = C Int
          | WX Int
          | N String
          | B [Double]
          | L
          deriving(Eq,Read,Show)    
               
metricElem :: Parser Elem
metricElem  = do char 'C'
                 spaces
                 c <- number 
                 return $ C c
              <|>
              do string "WX"
                 spaces
                 c <- number   
                 return $ WX c
              <|> 
              do char 'N'
                 spaces
                 c <- many1 alphaNum
                 return $ N c
              <|>
              do char 'B'
                 spaces
                 c <- array
                 return . B . map read $ c   
              <|> 
              do char 'L'
                 spaces
                 many1 letter
                 spaces
                 many1 letter
                 return L
                 
data Metric = Metric { charCode :: Int
                     , width :: Int
                     , name :: String
                     , bounds :: [Double]
                     }
                     deriving(Eq,Show)
                     
data Font = Font { metrics :: [Metric]
                 , underlinePosition :: Int
                 , underlineThickness :: Int
                 , ascent :: Int
                 , descent :: Int
                 , kernData :: Maybe [KX]
                 }
                 deriving(Eq,Show)
                        
isEncoded :: Metric -> Bool
isEncoded (Metric c _ _ _) = c /= (-1)                  
                        
mkMetric :: [Elem] -> Metric
mkMetric l = foldr addElem (Metric (-1) 0 "" []) l     
 where
     addElem  (C c) m = m {charCode=c}
     addElem  (WX c) m = m {width=c}
     addElem  (N s) m = m {name=s}
     addElem  (B l) m = m {bounds=l}
     addElem  _ m = m         
                          
charMetric :: Parser Metric
charMetric = do
       l <- sepEndBy metricElem (many1 (oneOf "; ")) 
       line 
       return . mkMetric $ l
       
data KX = KX String String Int  
        deriving(Eq,Ord,Show)  
       
kernPair :: Parser KX
kernPair = do string "KPX"
              spaces
              namea <- many1 letter
              spaces
              nameb <- many1 letter
              spaces
              nb <- many1 (oneOf "-+0123456789")
              line
              return $ KX namea nameb (read nb)
                       
getKernData :: Parser (Maybe [KX])
getKernData = do 
            { comment "StartKernData"
            ; nbKerns <- getInt "StartKernPairs" 
            ; k <- many1 kernPair
            ; comment "EndKernPairs"
            ; comment "EndKernData"
            ; return $ Just k
            }
                            
afm :: Parser Font
afm = do { comment "StartFontMetrics"
         ; comments
         ; fontName <- getString "FontName"
         ; fullName <- getName "FullName"
         ; familyName <- getString "FamilyName"
         ; weight <- getWeigth "Weight"
         ; italicAngle <- getFloat "ItalicAngle"
         ; isFixedPitch <- getBool "IsFixedPitch"
         ; characterSet <- getCharacterSet "CharacterSet"
         ; bbox <- getArray "FontBBox"
         ; underlinePosition <- getInt "UnderlinePosition"
         ; underlineThickness <- getInt "UnderlineThickness"
         ; comment "Version"
         ; comment "Notice"
         ; encodingScheme <- getEncoding "EncodingScheme"
         ; capHeight <- option 0 $ getInt "CapHeight"
         ; xHeight <- option 0 $ getInt "XHeight"
         ; ascender <- option 0 $ getInt "Ascender"
         ; descender <- option 0 $ getInt "Descender" 
         ; stdHW <- getInt "StdHW"
         ; stdVW <- getInt "StdVW"
         ; startCharMetrics <- getInt "StartCharMetrics"
         ; charMetrics <- many1 charMetric
         ; comment "EndCharMetrics"
         ; kerns <- option Nothing getKernData
         ; comment "EndFontMetrics"
         ; return $ Font (filter isEncoded charMetrics) underlinePosition underlineThickness ascender descender kerns
         }
     
allFonts :: [String]                          
allFonts = [ "Helvetica.afm"
           , "Helvetica-Bold.afm"
           , "Helvetica-Oblique.afm"
           , "Helvetica-BoldOblique.afm"
           , "Times-Roman.afm"
           , "Times-Bold.afm"
           , "Times-Italic.afm"
           , "Times-BoldItalic.afm"
           , "Courier.afm"
           , "Courier-Bold.afm"
           , "Courier-Oblique.afm"
           , "Courier-BoldOblique.afm"
           , "Symbol.afm"
           , "ZapfDingbats.afm"
           ]
           
createFontList :: Font -> [(Int,Int)]
createFontList (Font m up ut asc desc k)  = zip codes (map getWidth [32..255])
 where
     codes = [32..255]
     getWidth c = IM.findWithDefault spaceWidth c chars
     spaceWidth = IM.findWithDefault 0 32 chars
     chars = IM.fromList . map zipMetric $ m
     zipMetric m@(Metric c w _ _) = (c,w)
         
parseFont :: String -> IO (Maybe Font)
parseFont s = do
    r <- parseFromFile afm $ "../Core14_AFMs/" ++ s
    case r of
      Left e -> error (show e)
      Right r -> return $ Just r

fontData :: Font -> [String]
fontData (Font _ up ut asc desc k) = [show up,show ut,show asc,show desc,maybe "0" (const "1") k]

kernForFont :: (Int,Font) -> M.Map (Int,Int,Int) Int
kernForFont (i,(Font m _ _ _ _ Nothing)) = M.empty
kernForFont (i,(Font m _ _ _ _ (Just k))) = do
    let names = M.fromList . map (\x -> (name x, charCode x)) $ m
        k' = mapMaybe kernToList k
        kernToList (KX na nb v) =
            let ca = M.lookup na names
                cb = M.lookup nb names in
            case (ca,cb) of
                (Just a,Just b) -> Just $ ((i,a,b),v)
                (_,_) -> Nothing
    M.fromList k'



createKern :: [Font] -> IO ()
createKern f = do
    let allmaps = map kernForFont $ (zip [0..] f)
        endkern = M.fromList $ [((i,j,0),0) | i <- [0..13],j <- [32..255]]
        result = M.unions $ allmaps
    putStrLn $ "-- #hide"
    putStrLn $ "module Graphics.PDF.LowLevel.Kern(kerns) where"
    putStrLn ""
    putStrLn $ "import qualified Data.Map as M"
    putStrLn $ "import Data.Word"
    putStrLn $ "kerns :: M.Map (Int,Word8,Word8) Int"
    putStrLn $ "kerns = M." ++ (show result)

main :: IO ()
main = do
    r <- getArgs
    maybeFonts <- mapM parseFont allFonts 
    let fonts = map fromJust . filter isJust $ maybeFonts
    if null r
      then do
        let carray = map createFontList fonts
        putStrLn "#ifndef _METRICS_H_"
        putStrLn "#define _METRICS_H_"
        putStrLn "static const unsigned long gmetric[]={"
        mapM_ putStr . intersperse "\n," . map (\(c,w) -> show w) . concat $ carray
        putStrLn "};"
        putStrLn "static const long fmetric[]={"
        mapM_ putStr . intersperse "\n," . concatMap fontData $ fonts
        putStrLn "};"
        putStrLn "#endif"
      else do
        createKern fonts
    
    