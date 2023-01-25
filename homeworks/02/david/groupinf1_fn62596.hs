{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use &&" #-}

import GHC.Natural (Natural)
import Data.List (find, nub)
import Data.Function (on)
import Data.Maybe (mapMaybe)

data Status = Upcoming | Canceled | InProgress | Finished
  deriving (Eq, Show, Ord)

data Activity where
  Activity ::
    { activityName :: String
    , duration :: Natural
    } -> Activity
  deriving (Eq, Show, Ord)

data RefundPolicy = Flexible | Moderate | Strict
  deriving (Eq, Show, Ord)

data Trip where
  Trip ::
    { destination :: String
    , price :: Double
    , status :: Status
    , policy :: RefundPolicy
    , activities :: [Activity]
    } -> Trip

instance Eq Trip where
  (==) :: Trip -> Trip -> Bool
  (==) t1 t2 =
    and
      [ eq destination
      , eq activities
      , eq price
      ]
    where
      eq :: (Eq a) => (Trip -> a) -> Bool
      eq field = ((==) `on` field) t1 t2

data Customer where
  Customer ::
    { cName :: String
    , trips :: [Trip]
    , loyaltyPoints :: Natural
    } -> Customer

newtype Agency where
  Agency ::
    { customers :: [Customer]
    } -> Agency

------------
---- a) ----
------------

data Thresholds where
  Thresholds ::
    { moneyThreshold :: Double
    , durationThreshold :: Natural
    , loyaltyPointsThreshold :: Natural
    } -> Thresholds

freeTripEligibleCustomers :: Agency -> Thresholds -> [Customer]
freeTripEligibleCustomers
  Agency{customers}
  Thresholds{moneyThreshold, durationThreshold, loyaltyPointsThreshold}
  = filter isEligible customers
    where
      isEligible Customer{trips, loyaltyPoints} =
        and
          [ moneyThreshold         < totalSpendings -- at least that much spent
          , durationThreshold * 60 < totalTime      -- at least that much time in trips (notice that the threshold is in hours)
          , loyaltyPointsThreshold < loyaltyPoints  -- at least that many loyalty points
          , all ((/= Canceled) . status) trips      -- never canceled a trip
          , hasNoDuplicates $ map destination trips -- never visited a destination twice
          ]
        where
          total :: (Num a) => (b -> a) -> [b] -> a
          total getter = sum . map getter

          totalSpendings = total price trips
          totalTime = total (total duration . activities) trips
          -- -- originally, like that:
          -- hasNoDuplicates xs = ((==) `on` length) xs (nub xs)
          -- -- or like that :P
          -- hasNoDuplicates xs = ((==) `on` (length . ($ xs))) id nub
          -- or most beutifully, like that :D
          hasNoDuplicates = ((==) `on` length) =<< nub

------------
---- b) ----
------------

-- HELPERS
getProportion :: (a -> Bool) -> [a] -> Double
getProportion pred xs = ((/) `on` lenFiltered) pred (const True)
  where
    lenFiltered p = fromIntegral $ length $ filter p xs

avg :: [Double] -> Double
avg xs = sum xs / fromIntegral (length xs)
-- avg xs = ((/) `on` ($ xs)) sum (fromIntegral . length)

isCanceled :: Trip -> Bool
isCanceled = (== Canceled) . status

-- ESSENCE OF SOLUTION
getRefund :: Agency -> String -> String -> Maybe Double
getRefund Agency{customers} customerName tripDestination = do
  currentCustomerTrips <- trips <$> find ((== customerName) . cName) customers
  currentTrip <- find ((== tripDestination) . destination) currentCustomerTrips
  case status currentTrip of
    InProgress -> Just 0.0
    Upcoming -> do

      let c = case policy currentTrip of
            Flexible -> 0.7
            Moderate -> 0.4
            Strict -> 0.1

      let k = getProportion (not . isCanceled) currentCustomerTrips

      let quotient = getProportion isCanceled $ mapMaybe (find (== currentTrip) . trips) customers

      let canceledAvg =
            avg $
            map price $
            -- --alternatively :D
            -- flip filter currentCustomerTrips $ liftA2 (&&) isCanceled (/= currentTrip)
            filter isCanceled $
            filter (/= currentTrip)
            currentCustomerTrips

      let fee = quotient * canceledAvg

      Just $ max 0 $ k * c * price currentTrip - fee

    _ -> Nothing