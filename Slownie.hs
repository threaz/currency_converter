module Slownie (Rodzaj(..), Waluta(..), slownie) where
import Data.Ix
import Data.List
import Data.Char
import qualified Data.Map as Map

data Rodzaj   = Meski | Zenski | Nijaki deriving (Show, Read, Eq)

data Waluta = Waluta {
  mianownik_poj  :: String,
  mianownik_mn   :: String,
  dopelniacz_mn  :: String,
  rodzaj         :: Rodzaj
  } deriving (Show, Read)

slownie :: Waluta -> Integer -> String

slownie cur num
  | abs(num) >= 10 ^ 6000 = "mnóstwo"
  | num < 0   = "minus " ++ res (-num)
  | otherwise = res num
  where
    res num  = translateTuplesToString (integerToListOfTuples num)
      cur ++ " " ++ (findEnding num cur)

data SpecialCase = BelowThousand | Thousands | Milions | ReallyBigNumbers  deriving Show

numbersMap :: Map.Map Integer String
numbersMap =  Map.fromList  
  [(0, "zero"), (1, "jeden"), (2, "dwa"), (3, "trzy"), (4, "cztery"), (5, "pięć"), (6, "sześć"),
   (7, "siedem"), (8, "osiem"), (9, "dziewięć"),(10, "dziesięć"),
   (11, "jedenaście"), (12, "dwanaście"), (13, "trzynaście"), (14, "czternaście"), (15, "piętnascie"),
   (16, "szesnaście"), (17, "siedemnaście"), (18, "osiemnaście"), (19, "dziewiętnaście"),
   (20, "dwadzieścia"), (30, "trzydzieści"), (40, "czterdzieści"), (50, "pięćdziesiąt"),
   (60, "sześćdziesiąt"), (70, "siedemdziesiąt"), (80, "osiemdziesiąt"), (90, "dziewięćdziesiąt"),
   (100, "sto"), (200, "dwieście"), (300, "trzysta"), (400, "czterysta"), (500, "pięćset"),
   (600, "sześćset"), (700, "siedemset"), (800, "osiemset"), (900, "dziewięćset")]

-- konwertuje liczbę do następującego formatu:
-- 123 = [100, 20, 3]
-- natomiast w przypadku liczb od 11 do 19: 111 = [100, 11]
belowThousandToList :: Integer -> [Integer]
belowThousandToList n = if betweenElevenNineteen n then bTTL1 else bTTL where
  bTTL  = if n == 0 then [0] else concat $ hundreds : tens : ones : []
  bTTL1 = concat $ hundreds : teens : []
  hundreds = if n `div` 100 `mod` 10  == 0 then [] else [n `div` 100 `mod` 10 * 100]
  tens     = if n `div` 10  `mod` 10  == 0 then [] else [n `div` 10 `mod` 10 * 10]
  ones     = if n `mod` 10  == 0 then [] else [n `mod` 10]
  teens    = [n `div` 10 `mod` 10 * 10 + n `mod` 10]

-- sprawdza czy "końcówka" danej liczby jest z przedziału [11, 19]
betweenElevenNineteen :: Integer -> Bool
betweenElevenNineteen n = n `div` 10 `mod` 10 == 1 && inRange (1, 9) (n `mod` 10) 
                          
toIntegersListBelowThousand :: Integer -> [Integer] -> [Integer]
toIntegersListBelowThousand act acc
  | act == 0    = acc
  | otherwise = toIntegersListBelowThousand (act `div` 1000) (x : acc)
  where x = act `mod` 1000

-- tworzy listę trójek [(nr < 1000, k, i)]
integerToListOfTuples :: Integer -> [(Integer, Integer, Integer)]
integerToListOfTuples n = makeTuples $ reverse $ toIntegersListBelowThousand n [] where
  makeTuples :: [Integer] -> [(Integer, Integer, Integer)]
  makeTuples xs = makeTuplesR xs 0 0 []
    
  makeTuplesR :: [Integer] -> Integer -> Integer -> [(Integer, Integer, Integer)]
                -> [(Integer, Integer, Integer)]
  makeTuplesR [] _ _  acc = acc
  makeTuplesR (x : xs) k i acc 
    | i == 0 = makeTuplesR xs k (i + 1) ((x, k, i) : acc)
    | otherwise = makeTuplesR xs (k + 1) 0 ((x, k, i) : acc)
  

translateTuplesToString :: [(Integer, Integer, Integer)] -> Waluta -> String
translateTuplesToString [] _ = "zero"
translateTuplesToString t_xs cur = intercalate " " $ foldr f [] t_xs where
  f (num, k, i) acc
    | num == 0         = acc 
    | k == 0 && i == 0 = (belowThousandToString (num, k, i) cur BelowThousand) : acc
    | k == 0 && i == 1 = (belowThousandToString (num, k, i) cur Thousands) : acc
    | k < 10           = (belowThousandToString (num, k, i) cur Milions) : acc
    | otherwise        = (belowThousandToString (num, k, i) cur ReallyBigNumbers) : acc
  
belowThousandToString :: (Integer, Integer, Integer) -> Waluta -> SpecialCase -> String
belowThousandToString (n, _, _) _ Thousands 
  | n == 1                       = "tysiąc"
  | betweenElevenNineteen n      = f ++ " tysięcy"
  | (inRange(2, 4) $ n `mod` 10) = f ++ " tysiące"
  | otherwise                    = f ++ " tysięcy"
    where
      f = intercalate " " $ foldr (\x acc -> (numbersMap Map.! x) : acc) [] $ belowThousandToList n

belowThousandToString (n, _, _) cur BelowThousand 
  | betweenElevenNineteen n = f n
  | (n == 1 && rodzaj cur == Zenski) = "jedna"
  | (n == 2 && rodzaj cur == Zenski) = "dwie"    
  | (n == 1 && rodzaj cur == Nijaki) = "jedno"
  | (n `mod` 10 == 2 && rodzaj cur == Zenski) =
      f (n `div` 10 * 10) ++ " dwie"
  | otherwise = f n
      where
        f = (intercalate " ") .
          (foldr (\x acc -> (numbersMap Map.! x) : acc) []) . belowThousandToList

-- kiedy k < 10
belowThousandToString (n, k, i) _ Milions
  | n == 1                     = prefix ++ suffix
  | betweenElevenNineteen n    = f ++ (space ++ prefix ++ suffix ++ "ów")
  | inRange(2, 4) $ n `mod` 10 = f ++ (space ++ prefix ++ suffix ++ "y")
  | otherwise                  = f ++ (space ++ prefix ++ suffix ++ "ów")
    where
      space  = " "
      prefix = prefixMap Map.! k
      suffix = if even i then "lion" else "liard"
      prefixPairsList = [(1, "mi"), (2, "bi"), (3, "try"), (4, "kwadry"), (5, "kwinty"),
                         (6, "seksty"), (7, "septy"), (8, "okty"), (9, "noni")]
      prefixMap = Map.fromList prefixPairsList
      f = intercalate " " $ foldr (\x acc -> (numbersMap Map.! x) : acc) [] $ belowThousandToList n

-- kiedy k >= 10
belowThousandToString (n, k, i) _ ReallyBigNumbers
  | n == 1                     = prefix ++ suffix
  | betweenElevenNineteen n    = f ++ (space ++ prefix ++ suffix ++ "ów")
  | inRange(2, 4) $ n `mod` 10 = f ++ (space ++ prefix ++ suffix ++ "y")
  | otherwise                  = f ++ (space ++ prefix ++ suffix ++ "ów")
  where
    space  = " "
    a = k `mod` 10
    b = k `div` 10 `mod` 10
    c = k `div` 100 `mod` 10

    prefix = concat $ pa : pb : pc : []
    pa = prefixA Map.! a
    pb = prefixB Map.! b
    pc = prefixC Map.! c
    
    suffix = if even i then "lion" else "liard"
    
    prefixA = Map.fromList [(0, ""), (1, "un"), (2, "do"), (3, "tri"), (4, "kwatuor"), (5, "kwin"),
                               (6, "seks"), (7, "septen"), (8, "okto"), (9, "nowem")]
    prefixB = Map.fromList [(0, ""), (1, "decy"), (2, "wicy"), (3, "trycy"), (4, "kwadragi"),
                                (5, "kwintagi"), (6, "seksginty"), (7, "septagi"),
                                (8, "oktagi"), (9, "nonagi")]
    prefixC = Map.fromList [(0, ""), (1, "centy"), (2, "ducenty"), (3, "trycenty"),
                                (4, "kwadryge"), (5, "kwinge"), (6, "sescenty"),
                                (7, "septynge"), (8, "oktynge"), (9, "nonge")]
    f = intercalate " " $ foldr (\x acc -> (numbersMap Map.! x) : acc) [] $ belowThousandToList n

findEnding :: Integer -> Waluta -> String
findEnding n cur
  | n == 1                     = mianownik_poj cur
  | betweenElevenNineteen n    = dopelniacz_mn cur
  | inRange(2, 4) $ n `mod` 10 = mianownik_mn cur
  | otherwise                  = dopelniacz_mn cur
  
