import Slownie

import System.Environment
import Prelude hiding (putStrLn)
import Data.ByteString.Char8 (putStrLn)
import Data.ByteString.UTF8 (fromString)

import qualified Data.Map as Map

main :: IO ()
main = do
  (number : currency : _) <- getArgs
  let cur = currencyMap Map.! currency
      num = (read number :: Integer)
  putStrLn $ fromString (slownie cur num)

 
currencyMap = Map.fromList [ ("AUD" , Waluta "dolar australijski" "dolary australijskie" "dolarów australijskich" Meski),
  ("BGN", Waluta "lew" "lewy" "lewów" Meski),
  ("BRL", Waluta "real" "reale" "realów" Meski),
  ("BYR", Waluta "rubel białoruski" "ruble białoruskie" "rubli białoruskich" Meski),
  ("CAD", Waluta "dolar kanadyjski" "dolary kanadyjskie" "dolarów kanadyjskich" Meski),
  ("CHF", Waluta "frank szwajcarski" "franki szwajcarskie" "franków szwajcarskich" Meski),
  ("CNY", Waluta "yuan" "yuany" "yuanów" Meski),
  ("CZK", Waluta "korona czeska" "korony czeskie" "koron czeskich" Zenski),
  ("DKK", Waluta "korona duńska" "korony duńskie" "koron duńskich" Zenski),
  ("EUR", Waluta "euro" "euro" "euro" Nijaki),
  ("GBP", Waluta "funt brytyjski" "funty brytyjskie" "funtów brytyjskich" Meski),
  ("HKD", Waluta "dolar hongkoński" "dolary hongkońskie" "dolarów hongkońskich" Meski),
  ("HRK", Waluta "kuna" "kuny" "kun" Zenski),
  ("HUF", Waluta "forint" "forinty" "forintów" Meski),
  ("IDR", Waluta "rupia" "rupie" "rupii" Zenski),
  ("ISK", Waluta "kronur" "kronury" "kronurów" Meski),
  ("JPY", Waluta "jen" "jeny" "jenów" Meski),
  ("KRW", Waluta "won" "wony" "wonów" Meski),
  ("MXN", Waluta "peso meksykańskie" "peso meksykańskie" "peso meksykańskich" Nijaki),
  ("MYR", Waluta "ringgit" "ringgity" "ringgitów" Meski),
  ("NOK", Waluta "korona norweska" "korony norweskie" "koron norweskich" Zenski),
  ("NZD", Waluta "dolar nowozelandzki" "dolary nowozelandzkie" "dolarów nowozelandzkich" Meski),
  ("PHP", Waluta "peso filipińskie" "peso filipińskie" "peso filipińskich" Nijaki),
  ("PLN", Waluta "złoty" "złote" "złotych" Meski),
  ("RON", Waluta "lej" "leje" "lei" Meski),
  ("RUB", Waluta "rubel rosyjski" "ruble rosyjskie" "rubli rosyjskich" Meski),
  ("SDR", Waluta "specjalne prawo ciągnienia" "specjalne prawa ciągnienia" "specjalnych praw ciągnienia" Nijaki),
  ("SEK", Waluta "korona szwedzka" "korony szwedzkie" "koron szwedzkich" Zenski),
  ("SGD", Waluta "dolar singapurski" "dolary singapurskie" "dolarów singapurskich" Meski),
  ("THB", Waluta "baht" "bahty" "bahtów" Meski),
  ("TRY", Waluta "lira" "liry" "lir" Zenski),
  ("UAH", Waluta "hrywna" "hrywny" "hrywien" Zenski),
  ("USD", Waluta "dolar amerykańki" "dolary amerykańskie" "dolarów amerykańskich" Meski),
  ("ZAR", Waluta "rand" "randy" "randów" Meski)]

