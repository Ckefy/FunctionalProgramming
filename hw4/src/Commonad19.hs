module Commonad19 where

import Control.Comonad
import System.Random
import Data.List
import System.Console.ANSI
import Control.Monad (forM_, forM)
import System.Process     (callCommand)
import Control.Concurrent (threadDelay)

-- |----------------------------|--
-- |-----------CONFIG-----------|--
-- |----------------------------|--

-- | StdGen - each person should has differend random
-- | Healthcare - current status of his illness
-- | Int - how much to stay in this healthcate
data Person = Person
     StdGen
     Healthcare
     Int

-- | Good - no illness, hidden - incubation period
-- | Ill - have ilness, protected - immune
data Healthcare = Good
                | Hidden
                | Ill
                | Protected

instance Show Person where
  show (Person _ Good _) = "1"
  show (Person _ Hidden _) = "?"
  show (Person _ Ill _) = "#"
  show (Person _ Protected _) = "@"


-- |----------------------------|--
-- |-----------ZIPPER-----------|--
-- |----------------------------|--

--LiztZipper instance was copied from slides from lecture
data ListZipper a = LZ [a] a [a]

listLeft :: ListZipper a -> ListZipper a
listLeft (LZ (l : ls) cur r) = LZ ls l (cur : r)
listLeft lz = lz

listRight :: ListZipper a -> ListZipper a
listRight (LZ l cur (r : rs)) = LZ (cur : l) r rs
listRight lz = lz

instance Functor ListZipper where
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
  extract (LZ _ x _) = x
  duplicate = mkZipper listLeft listRight

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

mkZipper :: (v -> v) -> (v -> v) -> v -> ListZipper v
mkZipper genLeft genRight e =
   LZ (iterateTail genLeft e) e (iterateTail genRight e)

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ l _ r) = LZ l x r


-- |----------------------------|--
-- |------------GRID------------|--
-- |----------------------------|--

--Grid instance was copied from slides from lecture
--external LZ is zipper for rows and inner LZ for columns
newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) } --2D

up :: Grid a -> Grid a
up (Grid g) = Grid $ listLeft g

down :: Grid a -> Grid a
down (Grid g) = Grid $ listRight g

left :: Grid a -> Grid a
left (Grid g) = Grid $ fmap listLeft g

right :: Grid a -> Grid a
right (Grid g) = Grid $ fmap listRight g

gridRead :: Grid a -> a
gridRead (Grid g) = extract (extract g)

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid (listWrite newLine g)
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal :: Grid a -> ListZipper (Grid a)
horizontal = mkZipper left right

vertical :: Grid a -> ListZipper (Grid a)
vertical   = mkZipper up down

instance Functor Grid where
  fmap f (Grid a) = Grid ((f <$>) <$> a)

instance Comonad Grid where
  extract = gridRead
  duplicate = Grid . fmap horizontal . vertical


-- |----------------------------|--
-- |------------GAME------------|--
-- |----------------------------|--

--next 4 functions from slides

-- | Number of ill out of given array
illCount :: [Person] -> Int
illCount = length . filter checkIll

-- | Array of ill out of given array
illPersons :: [Person] -> [Person]
illPersons = filter checkIll

checkIll :: Person -> Bool
checkIll (Person _ Good _) = False
checkIll (Person _ Hidden _) = True
checkIll (Person _ Ill _) = True
checkIll (Person _  Protected _) = False

-- | 4 neighbours
neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals
  where horizontals = [left, right]
        verticals   = [up, down]

-- | Number of ill Neighbours
illNeighboursCount :: Grid Person -> Int
illNeighboursCount g = illCount $ map (\direction -> extract $ direction g) neighbours

-- | Array of ill Neighbours
illNeighboursArr :: Grid Person -> [Person]
illNeighboursArr g = illPersons $ map (\direction -> extract $ direction g) neighbours

-- | Random generator generate Float and next generator
getRand :: StdGen -> (Float, StdGen)
getRand = randomR (0 :: Float, 1)

-- | Make Person ill with certain probability
maybeBecomeIll :: Person -> [Person]
     -> Float -> Int -> Person
maybeBecomeIll person@(Person gen _ _) illPersons p daysHidden = do
    let result = map (\curNeigh@(Person curGen _ _) -> do
          let curP = fst(getRand curGen)
          curP <= p
         ) illPersons
    if True `elem` result then
       Person gen Hidden daysHidden
    else
       person

-- | Rule of the game - if person hasn`t illness and immune
-- | Than maybe they can become ill (depend on their neighbours)
-- | If not, just decrease the number of day to go to another status
-- | If day == 1 -> change status
rule :: Float -> Int -> Int -> Int -> Grid Person -> Person
rule p daysHidden daysIll daysProtected g = do
 let (Person oldGen _ _) = extract g
 let newGen = snd(getRand oldGen)
 case illNeighboursCount g of
   0 -> case extract g of
          Person gen Good n -> Person newGen Good n
          Person gen Hidden 1 -> Person newGen Ill daysIll
          Person gen Hidden n -> Person newGen Hidden (n - 1)
          Person gen Ill 1 -> Person newGen Protected daysProtected
          Person gen Ill n -> Person newGen Ill (n - 1)
          Person gen Protected 1 -> Person newGen Good 0
          Person gen Protected n -> Person newGen Protected (n - 1)
   _ -> case extract g of
          Person gen Hidden 1 -> Person newGen Ill daysIll
          Person gen Hidden n -> Person newGen Hidden (n - 1)
          Person gen Ill 1 -> Person newGen Protected daysProtected
          Person gen Ill n -> Person newGen Ill (n - 1)
          Person gen Protected 1 -> Person newGen Good 0
          Person gen Protected n -> Person newGen Protected (n - 1)
          Person gen Good n -> maybeBecomeIll (Person newGen Good n) (illNeighboursArr g)
                                     p daysHidden

-- | Make 1 step of the game
evolve :: Float -> Int -> Int -> Int -> Grid Person -> Grid Person
evolve p daysHidden daysIll daysProtected = extend (rule p
                                daysHidden daysIll daysProtected)

-- | True means middle zipper
-- | Generate random ListZipper with persons
initLZ :: Bool -> Int -> Int -> IO(ListZipper Person)
initLZ flag daysHidden n = do
  let halfN = div (n - 1) 2
  leftPersons <- forM [1..halfN] (\x -> do
                                     gCur <- newStdGen
                                     return (Person gCur Good 0))
  gMid <- newStdGen
  let middle = Person gMid Good 0
  rightPersons <- forM [1..halfN] (\x -> do
                                     gCur <- newStdGen
                                     return (Person gCur Good 0))
  if flag then
    return (LZ leftPersons (Person gMid Hidden daysHidden) rightPersons)
  else
    return (LZ leftPersons middle rightPersons)

-- | Generate map with focus on central Person
initializeMap :: Int -> Int -> IO (Grid Person)
initializeMap daysHidden n = do
  let halfN = div (n - 1) 2
  topPersons <- forM [1..halfN] (\x -> initLZ False daysHidden n)
  middle <- initLZ True daysHidden n
  botPersons <- forM [1..halfN] (\x -> initLZ False daysHidden n)
  let grid = Grid (LZ topPersons middle botPersons)
  return grid

-- | Recursive function to make game
-- | Steps - how many steps we want to do
-- | Size - size of 1 edge (only odd numbers available)
infect :: Float -> Int -> Int -> Int -> Int -> Int -> Grid Person -> Int -> IO ()
infect p daysHidden daysIll daysProtected steps size grid speedLevel = if steps == 0 then putStrLn ""
  else do
    threadDelay (100000 * speedLevel)
    callCommand "tput reset"
    showGrid size grid
    infect p daysHidden daysIll daysProtected (steps - 1) size (
            evolve p daysHidden daysIll daysProtected grid) speedLevel

-- | Make list out of LZ
makeList1D :: Int -> ListZipper a -> [a]
makeList1D n (LZ lefts x rights) = do
   let leftSide = reverse (take n lefts)
   let middle = [x]
   let rightSide = take n rights
   leftSide ++ middle ++ rightSide

-- | Make 2D list out of Grid
makeList2D :: Int -> Grid a -> [[a]]
makeList2D n = fmap (makeList1D n) .
                makeList1D n .
                   unGrid

-- | Print given grid
showGrid :: Show a => Int -> Grid a -> IO ()
showGrid n g = do
  let arr = map (map show) (makeList2D (div (n - 1) 2) g)
  setSGR [SetColor Foreground Dull Magenta]
  putStrLn "LEGEND:"
  setSGR [SetColor Foreground Dull Green]
  putStrLn "1 - doesn't have illness"
  setSGR [SetColor Foreground Dull Yellow]
  putStrLn "? - incubation period"
  setSGR [SetColor Foreground Vivid Red]
  putStrLn "# - has illness"
  setSGR [SetColor Foreground Dull Blue]
  putStrLn "@ - has immunity\n"
  setSGR [Reset]
  forM_ arr (\curList -> do
                  printStr curList
                  putStrLn "")

printStr arr = forM_ arr (\x -> do
                          if x == "1" then
                             setSGR [SetColor Foreground Dull Green]
                          else if x == "?" then
                             setSGR [SetColor Foreground Dull Yellow]
                          else if x == "#" then
                             setSGR [SetColor Foreground Vivid Red]
                          else
                             setSGR [SetColor Foreground Dull Blue]
                          putStr (x ++ " ")
                          setSGR [Reset])

-- | Run our game
-- | If user make even size, decrease it
runGame :: Float -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
runGame prob daysHidden daysIll daysProtected steps size speed =
  if mod size 2 > 0 then do
      startGrid <- initializeMap daysHidden (size - 1)
      infect prob daysHidden daysIll daysProtected steps (size - 1) startGrid speed
  else do
     startGrid <- initializeMap daysHidden size
     infect prob daysHidden daysIll daysProtected steps size startGrid speed

-- | Width only available odd numbers - as we want to have central of Grid
-- | So we will have the grid with size <width x width>
-- | Speed level - with the increasing of level one step becomes longer
-- | Speed level 0 = no pauses at all
exampleOfGame :: IO()
exampleOfGame = do
    let probabilityIll = 0.4
    let daysIncubationPeriod = 5
    let daysIllness = 2
    let daysProtected = 1
    let countSteps = 50
    let width = 25
    let speedLevel = 2
    runGame probabilityIll daysIncubationPeriod daysIllness daysProtected
                     countSteps width speedLevel

