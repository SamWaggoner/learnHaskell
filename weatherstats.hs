module WeatherStats where

{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable
import qualified System.Exit as Exit

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

import Data.Csv
  ( DefaultOrdered(headerOrder)
  , FromField(parseField)
  , FromNamedRecord(parseNamedRecord)
  , Header
  , ToField(toField)
  , ToNamedRecord(toNamedRecord)
  , (.:)
  , (.=)
  )
import qualified Data.Csv as Cassava

import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Data.Vector (Vector)
import qualified Data.Vector as Vector

data Observation =
   Observation
     { day :: Text
     , hourOfDay :: Text
     , temperature :: Float
     , precipitation :: Float
     , windspeed :: Float
     }
   deriving (Eq, Show)

date (Observation d _ _ _ _) = d
hour (Observation _ h _ _ _) = h
temp (Observation _ _ t _ _) = t
precip (Observation _ _ _ p _) = p
wind (Observation _ _ _ _ w) = w

instance FromNamedRecord Observation where
   parseNamedRecord m =
     Observation
       <$> m.: "Date"
       <*> m.: "Time"
       <*> m.: "Temp"
       <*> m.: "Precip"
       <*> m.: "Wind"

decodeItems :: ByteString -> Either String (Vector Observation)
decodeItems = fmap snd . Cassava.decodeByName

decodeItemsFromFile :: FilePath -> IO (Either String (Vector Observation))

decodeItemsFromFile filePath = catchShowIO (ByteString.readFile filePath) >>= return . either Left decodeItems

catchShowIO :: IO a -> IO (Either String a)

catchShowIO action = fmap Right action `Exception.catch` handleIOException
  where
    handleIOException :: IOException -> IO (Either String a)
    handleIOException = return . Left . show

-- Function to simply read the data. Note that any errors during reading of the csv file will stop the program. To debug file I/O, call dcodeItemsFromFile directly
readData :: FilePath -> IO [Observation]
readData filename = do
  x <- decodeItemsFromFile filename
  case x of
     Left reason -> return []
     Right obsData -> return (Vector.toList obsData)

--------------------------------------------
-- DO NOT CHANGE ANYTHING ABOVE THIS LINE --
--------------------------------------------
-- Name: Sam Waggoner
-- Partner: n/a
--------------------------------------------

-- RUN INSTRUCTIONS:
-- cd OneDrive\Desktop\projects\a4_301.hs
-- ghci
-- :set -XOverloadedStrings
-- :set -XRecordWildCards
-- :l weatherstats.hs
-- obsData <- readData "2069-2021.csv"
-- daySummary <day_number> obsData


-- Question 1a: Finds average of array
average :: [Float] -> Float
average xs = foldl1 (+) xs / fromIntegral (length xs)

-- Question 1b: Finds the difference between the maximum and minimum values in an array
maxDiff :: [Float] -> Float
maxDiff xs = maximum xs - minimum xs

-- -- Question 1c
-- computes the average and maxDiff for the temperature and the windSpeed on a particular day of the year from the data provided
-- by the readData function (for simplicity, the day is given as Integer). The output should be: (day, average
-- temperature, max temperature differential, average windspeed, max windspeed differential

daySummary day obsData = [fromIntegral day,
  getAvgTemp (getDay day obsData),
    getMaxDiffTemp (getDay day obsData),
      getAvgWind (getDay day obsData),
        getMaxDiffWind (getDay day obsData)]

--- any other functions you need for 1a-1c go here
getDay day obsData = drop (24*(day-1)) (take (24*day) obsData)
getAvgTemp days = average (map temp days)
getMaxDiffTemp days = maxDiff (map temp days)
getAvgWind days = average (map wind days)
getMaxDiffWind days = maxDiff (map wind days)


-- Question 2a 
-- Write a function chunkby n l that takes a list l and an integer n and produces a list of lists of
-- length n. Make sure you include a type declaration. Use the provided function dailyTemperatureStat as
-- inspiration. You will also need to use the “:” function to append things to lists.

chunkby :: Int -> [a] -> [[a]]
chunkby _ [] = []
chunkby n l = take n l : chunkby n (drop n l)

-- Question 2b
-- Define a partial function chunkByDays l that relies on chunkby and chunks the list of hourly
-- observations (as provided by the readData function) into a list of lists of observations for each day

chunkByDays :: [Observation] -> [[Observation]]
chunkByDays obsData = chunkby 24 obsData


-- 3a: 
{-
dailyTemperatureStat takes a function, a day, and a list of Observations
It finds the 24 temperature measurements associated with that day. Then, it performs
the given function on the list of temperatures.
Example: dailyTemperatureStat average 1 obsData       results in -3.4250002
Example: dailyTemperatureStat maxDiff 3 obsData       results in 4.1000004

How it does it:
First, it calculates h, which will be the number of list entries that will be dropped from obsData.
You want to drop everything up to (not including) the day number. Since each day 
consists of 24 measurements, h = 24 * (day-1).
If you wanted day 4, for example, you would have gone from 
[1,2,3,4,5,6]    ->    [4,5,6]
(pretending each number is 24 measurements)
Now, the day that you want is at the beginning of the list, so you take the first
24 measurements and you have your day data as dayList.
Next, you map the function temp over your dayList, applying temp to each observation
that day, resulting in a list of temperatures for the given day.
Finally, the given function is applied to the list of day temperatures. Depending
on what your function was, the results will differ.
-}
dailyTemperatureStat :: ([Float] -> t) -> Int -> [Observation] -> t
dailyTemperatureStat f day obsData = f (map temp dayList)
  where
    h = 24*(day-1)
    dayList = (take 24 (drop h obsData))

-- Example function for 3b: Computes the minimum temperature for Jan 3 based on the 24 hourly measurements
{-
Use the function dailyTemperatureStat filename to define a new function allMinimumTemps
that produces a list of tuples consisting of the day of the year and the minimum temperature on that day. You
will need to use map and function composition (using the “.” operation), but be careful because we’re operating
on a list of lists.
-}
jan3Minimum filename = do
  obsData <- readData filename 
  let result = minimum (map temp (take 24 (drop 48 obsData)))
  putStr "minimum temperature on January 3 = "
  print result

-- creates a list of tuples (day, minimimDayTemp)
chunkby2 :: Int -> [a] -> [[a]]
chunkby2 _ [] = []
chunkby2 n l = take n l : chunkby n (drop n l)

getDay2 day obsData = drop (24*(day-1)) (take (24*day) obsData)

{-
for every day, apply minimum to the day's temperatures
-}
-- Question 3b
-- makeTuple 366 obsData = [(2.0,2.0)]
-- makeTuple day obsData = (day, minimum (take 24 obsData)) : makeTuple (day+1) obsData
-- allMinimumTemp filename = do
--   obsData <- readData filename
--   -- let result = map (dailyTemperatureStat minimum day obsData) obsData
--   let result = map (dailyTemperatureStat minimum dayNum obsData) [1..365]
--   let result = makeTuple 1 obsData
--   putStr "Minimum temperature for each day of the year:"
--   print result

-- -- Question 3c
-- highDifferentialDays filename = ...