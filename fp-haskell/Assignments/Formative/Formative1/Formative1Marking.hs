import qualified Formative1                      as Form1
import qualified Formative1Solutions             as Solutions

import           Formative1TestCases

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Data.Maybe
import           System.Timeout
import           Test.QuickCheck.Counterexamples
import           Text.Printf

-- timeout in microseconds
time :: Int
time = 400000

-- "correct" will be printed in green
correctString :: String
correctString = " is " ++ "\x1b[32m" ++ "correct" ++ "\x1b[0m" ++ " :)"

-- "wrong" will be printed in red
wrongString :: String
wrongString = " is " ++ "\x1b[31m" ++ "wrong" ++ "\x1b[0m" ++ " :("

type Cell = (Int,Int)
type Grid = [Cell]

prop_translate :: (Int, Int) -> Grid -> Bool
prop_translate (a,b) g = Form1.translate (a,b) g == Solutions.translate (a,b) g

prop_hasPeriod :: TestCase -> Int -> IO Bool
prop_hasPeriod c k = do
  let g = grid c
  let b = Form1.hasPeriod g k == Solutions.hasPeriod g k
  if not b
    then do
      putStrLn "\tIn your case:"
      putStrLn $ printf "\t%s %s %d = %s" "hasPeriod"
        (name c) k (show $ Form1.hasPeriod g k)
      putStrLn $ printf "\tCorrect answer is %s\n"
        (show $ Solutions.hasPeriod g k)
      return b
    else return b

data InputArgs a = InputArgs {
  test       :: TestCase,
  funcName   :: String,
  funcTheirs :: Grid -> Maybe a,
  funcOurs   :: Grid -> Maybe a,
  showFunc   :: Maybe a -> String
  }

boolToMaybe :: Bool -> Maybe ()
boolToMaybe True  = Just ()
boolToMaybe False = Nothing

prop_terminate :: (NFData a, Eq a, Show a) => InputArgs a -> IO Bool
prop_terminate input = do
  x <- timeout time $ do
    let a = funcTheirs input (grid . test $ input)
    deepseq a (return ())
    return a
  y <- timeout time $ do
    let a = funcOurs input (grid . test $ input)
    deepseq a (return ())
    return a
  case (x,y) of
    (Just x , Just y) ->
      if x == y
      then return True
      else do
        putStrLn "\tIn your case:"
        putStrLn $ printf "\t%s %s = %s"
          (funcName input) (name . test $ input) (showFunc input x)
        putStrLn $ printf "\tCorrect answer is %s\n" (showFunc input y)
        return False
    (Nothing, Just (Just y)) -> do
      putStrLn $ printf "\t%s %s did not terminate when it should have returned %s"
        (funcName input) (name . test $ input) (showFunc input $ Just y)
      return False
    (Just (Just x), Nothing) -> do
      putStrLn $ printf "\t%s %s returned %s when it should have returned %s or not terminated"
        (funcName input) (name . test $ input) (showFunc input $ Just x) (showFunc input Nothing)
      return False
    _ -> return True

resultPrinter :: [Bool] -> String -> IO ()
resultPrinter bs s = if deepseq bs (and bs)
                     then putStrLn $ "\t" ++ s ++ correctString
                     else putStrLn $ "\t" ++ s ++ wrongString

quickCheckArgs = Args {
  replay = replay stdArgs,
  maxSuccess = maxSuccess stdArgs,
  maxDiscardRatio = maxDiscardRatio stdArgs,
  maxSize = maxSize stdArgs,
  chatty = False,
  maxShrinks = maxShrinks stdArgs
}

catchUndefined :: String -> IO () -> IO ()
catchUndefined name action = catch action
  (\(SomeException e) -> putStrLn $
    printf "\t%s is \x1b[31mnot implemented\x1b[0m :(" name)

main :: IO ()
main = do
  putStrLn "Testing translate:"

  catchUndefined "translate" $ do
    (x, _) <- quickCheckWithResult quickCheckArgs prop_translate
    case x of
      Just ((a,b) :&: g :&: ()) -> do
        putStrLn "\n\tIn your case:"
        putStrLn $ printf "\ttranslate %s %s = %s"
          (show (a,b))
          (show g)
          (show $ Form1.translate (a,b) g)
        putStrLn $ printf "\tCorrect answer is %s\n"
          (show $ Solutions.translate (a,b) g)
        putStrLn $ "\t" ++ "translate" ++ wrongString
      Nothing -> putStrLn $ "\t" ++ "translate" ++ correctString

  putStrLn "\nTesting isMortal:"
  catchUndefined "isMortal" $ do
    success <- forM cases $ \c -> prop_terminate $ InputArgs c
                                           "isMortal"
                                           (boolToMaybe . Form1.isMortal)
                                           (boolToMaybe . Solutions.isMortal)
                                           (show . isJust)
    resultPrinter success "isMortal"

  putStrLn "\nTesting lifetime:"
  catchUndefined "lifetime" $ do
    success <- forM cases $ \c -> prop_terminate $ InputArgs c
                                           "lifetime"
                                           Form1.lifetime
                                           Solutions.lifetime
                                           show
    resultPrinter success "lifetime"

  putStrLn "\nTesting hasPeriod:"
  catchUndefined "hasPeriod" $ do
    success <- forM [(c,k) | c <- cases, k <- [1..20]]
             $ uncurry prop_hasPeriod
    resultPrinter success "hasPeriod"

  putStrLn "\nTesting eventuallyPeriodic"
  catchUndefined "eventuallyPeriodic" $ do
    success <- forM cases $ \c -> prop_terminate $ InputArgs c
                                           "eventuallyPeriodic"
                                           (boolToMaybe . Form1.eventuallyPeriodic)
                                           (boolToMaybe . Solutions.eventuallyPeriodic)
                                           (show . isJust)
    resultPrinter success "eventuallyPeriodic"

  putStrLn "\nTesting findEventualPeriod"
  catchUndefined "findEventualPeriod" $ do
    success <- forM cases $ \c -> prop_terminate $ InputArgs c
                                           "findEventualPeriod"
                                           Form1.findEventualPeriod
                                           Solutions.findEventualPeriod
                                           show
    resultPrinter success "findEventualPeriod"
