{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))

---- Optics

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = lmap f . rmap g

  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id

  rmap :: (b -> c) -> p a b -> p a c
  rmap = dimap id

  {-# MINIMAL dimap | (lmap, rmap) #-}

instance Profunctor (->) where
  dimap :: (a -> b) -> (c -> d) -> (b -> c) -> (a -> d)
  -- dimap fab fcd fbc = fcd . fbc . fab
  dimap = flip ((.) . (.)) . flip (.)

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

-- type Lens = Iso @(->)

--- Lens
type Lens s t a b = forall f. (Functor f) => (a -> f b) -> (s -> f t)
type Lens' s a = Lens s s a a

--- Generator
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter f s = setter s <$> f (getter s)

--- Getter
view :: Lens s t a b -> s -> a
view l s = getConst $ l Const s

--- ^. operator (view)
infixr 9 ^.
(^.) :: s -> Lens s t a b -> a
s ^. l = getConst (l Const s)

--- Setter
set :: Lens s t a b -> b -> s -> t
set l b = over l $ const b

--- Getter + Setter
over :: Lens s t a b -> (a -> b) -> s -> t
over l f s = runIdentity $ l (Identity . f) s

--- Traversals
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

type Traversal' s a = Traversal s s a a

--- ^.. operator (toListOf)
infixr 9 ^..
(^..) :: s -> Traversal s t a b -> [a]
s ^.. l = toListOf l s

toListOf :: Traversal s t a b -> s -> [a]
toListOf t = foldMapOf t pure

foldMapOf :: Monoid m => Traversal s t a b -> (a -> m) -> s -> m
foldMapOf t f = getConst . t (Const . f)

filtered :: Applicative f => (a -> Bool) -> (a -> f a) -> a -> f a
filtered p f s = if p s then f s else pure s

---- Data

newtype Agency where
  MkAgency ::
    { _customers :: [Customer]
    } ->
    Agency

-- Lens for accessing the customers field in the Agency data type
customers :: Lens' Agency [Customer]
customers = lens _customers (\s c -> s {_customers = c})

customersTraversal :: Traversal' Agency Customer
customersTraversal = customers . traverse

data Customer where
  MkCustomer ::
    { _cName :: String
    , _trips :: [Trip]
    , _loyaltyPoints :: Integer
    } ->
    Customer

-- Lens for accessing the cName field in the Customer data type
cName :: Lens' Customer String
cName = lens _cName (\s n -> s {_cName = n})

-- Lens for accessing the trips field in the Customer data type
trips :: Lens' Customer [Trip]
trips = lens _trips (\s t -> s {_trips = t})

tripsTraversal :: Traversal' Customer Trip
tripsTraversal = trips . traverse

-- Lens for accessing the loyaltyPoints field in the Customer data type
loyaltyPoints :: Lens' Customer Integer
loyaltyPoints = lens _loyaltyPoints (\s t -> s {_loyaltyPoints = t})

data Trip where
  MkTrip ::
    { _destination :: String
    , _price :: Double
    , _status :: Status
    , _activities :: [Activity]
    , _policy :: Policy
    } ->
    Trip

instance Eq Trip where
  (==) :: Trip -> Trip -> Bool
  (==) t1 t2 =
    and
      [ t1 ^. destination == t2 ^. destination
      , t1 ^. price == t2 ^. price
      , t1 ^. activities == t2 ^. activities
      ]

-- Lens for accessing the destination field in the Trip data type
destination :: Lens' Trip String
destination = lens _destination (\s d -> s {_destination = d})

-- Lens for accessing the price field in the Trip data type
price :: Lens' Trip Double
price = lens _price (\s p -> s {_price = p})

-- Lens for accessing the status field in the Trip data type
status :: Lens' Trip Status
status = lens _status (\s st -> s {_status = st})

-- Lens for accessing the activities field in the Trip data type
activities :: Lens' Trip [Activity]
activities = lens _activities (\s a -> s {_activities = a})

activitiesTraversal :: Traversal' Trip Activity
activitiesTraversal = activities . traverse

-- Lens for accessing the policy field in the Trip data type
policy :: Lens' Trip Policy
policy = lens _policy (\s a -> s {_policy = a})

data Status where
  Upcoming :: Status
  Cancelled :: Status
  InProgress :: Status
  Finished :: Status
  deriving (Eq)

data Activity where
  MkActivity ::
    { _aName :: String
    , _duration :: Integer
    } ->
    Activity
  deriving (Eq)

-- Lens for accessing the aName field in the Activity data type
aName :: Lens' Activity String
aName = lens _aName (\s n -> s {_aName = n})

-- Lens for accessing the duration field in the Activity data type
duration :: Lens' Activity Integer
duration = lens _duration (\s d -> s {_duration = d})

data Policy where
  Flexible :: Policy
  Moderate :: Policy
  Strict :: Policy
  deriving (Eq)

data Thresholds where
  Thresholds ::
    { moneyThreshold :: Double
    , durationThreshold :: Integer
    , loyaltyPointsThreshold :: Integer
    } ->
    Thresholds

---- Part a

freeTripEligibleCustomers :: Agency -> Thresholds -> [Customer]
freeTripEligibleCustomers agency Thresholds {moneyThreshold, durationThreshold, loyaltyPointsThreshold} =
  agency ^.. customersTraversal . filtered eligible
  where
    eligible customer =
      and
        [ sum (customer ^.. tripsTraversal . price) >= moneyThreshold
        , sum (customer ^.. tripsTraversal . activitiesTraversal . duration) >= durationThreshold
        , (customer ^. loyaltyPoints) >= loyaltyPointsThreshold
        , Cancelled `notElem` (customer ^.. tripsTraversal . status)
        , not (any ((> 1) . length . nub) (customer ^.. tripsTraversal . destination))
        ]

    nub :: (Eq a) => [a] -> [a]
    nub [] = []
    nub (x : xs) = x : nub (filter (x /=) xs)

---- Part b

getRefund :: Agency -> String -> String -> Maybe Double
getRefund agency customerName tripDestination = do
  let getCustomer :: Maybe Customer
      getCustomer =
        listToMaybe $
          agency
            ^.. customersTraversal
              . filtered
                ( \customer ->
                    customer ^. cName == customerName
                )

  customer <- getCustomer

  let getTrip :: Maybe Trip
      getTrip =
        listToMaybe $
          customer
            ^.. tripsTraversal
              . filtered
                ( \trip ->
                    trip ^. destination == tripDestination
                )

  trip <- getTrip

  let k :: Double
      k =
        (/)
          (tripsThat (\t -> t ^. status /= Cancelled))
          (tripsThat (const True))
        where
          tripsThat p =
            fromIntegral . length $
              customer
                ^.. tripsTraversal
                  . filtered p

  let c :: Double
      c = case trip ^. policy of
        Flexible -> 0.7
        Moderate -> 0.4
        Strict -> 0.1

  let canceledAvg :: Double
      canceledAvg =
        (/)
          (sum $ cancelledTrips ^.. traverse . price)
          (fromIntegral $ length cancelledTrips)
        where
          cancelledTrips :: [Trip]
          cancelledTrips =
            customer
              ^.. tripsTraversal
                . filtered
                  ( \t ->
                      t ^. status == Cancelled
                  )

  let quotient :: Double
      quotient =
        (/)
          (succ $ peopleWhoHaveATripThat (\t -> t ^. status == Cancelled && t == trip))
          (peopleWhoHaveATripThat (\t -> t == trip))
        where
          peopleWhoHaveATripThat p =
            fromIntegral . length $
              ( agency
                  ^.. customersTraversal
                    . filtered
                      ( \c ->
                          not . null $
                            c
                              ^.. tripsTraversal
                                . filtered p
                      )
              )

  let fee :: Double
      fee = quotient * canceledAvg

  case trip ^. status of
    Upcoming -> Just $ max 0 (k * c * (trip ^. price) - fee)
    InProgress -> Just 0
    _ -> Nothing
  where
    listToMaybe :: [a] -> Maybe a
    listToMaybe (x : _) = Just x
    listToMaybe [] = Nothing
