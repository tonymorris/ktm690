data Who =
  Rob
  | Tony
  deriving (Eq, Ord, Show)

exchange ::
  Floating a
  => a
exchange =
  307920 / 339890

data Item =
  Item
    String
    Integer
    Who
  deriving (Eq, Ord, Show)

items ::
  [Item]
items =
  [
    Item "Tail tidy 21-690" 79950 Tony
  , Item "Rear flush mount indicators 22-100c" 44980 Tony
  , Item "Rear flush mount indicators 22-100c" 44980 Rob
  , Item "Turn signal resistor kit 22-690" 30960 Tony
  , Item "Turn signal resistor kit 22-690" 30960 Rob
  , Item "KTM Black Turn Signal Hand Guard Deflectors 22-206c" 59980 Rob
  , Item "Shipping" 8055 Rob
  , Item "Shipping" 8055 Tony
  ]

sumCost ::
  ([Item] -> [Item])
  -> Integer
sumCost f =
  sum . map (\(Item _ v _) -> v) . f $ items

usdTotal ::
  Integer
usdTotal =
  sumCost id

isRob ::
  Item
  -> Bool
isRob (Item _ _ w) =
  w == Rob
  
isTony ::
  Item
  -> Bool
isTony (Item _ _ w) =
  w == Tony

robsUsdTotal ::
  Integer
robsUsdTotal =
  sumCost (filter isRob)

tonysUsdTotal ::
  Integer
tonysUsdTotal =
  sumCost (filter isTony)

robsPortion ::
  Floating a =>
  a
robsPortion =
  fromIntegral robsUsdTotal / fromIntegral usdTotal

tonysPortion ::
  Floating a =>
  a
tonysPortion =
  fromIntegral tonysUsdTotal / fromIntegral usdTotal

audTotal ::
  Floating a =>
  a
audTotal =
  fromIntegral usdTotal * (1 / exchange)

robsAudTotal ::
  Floating a =>
  a
robsAudTotal =
  robsPortion * audTotal

tonysAudTotal ::
  Floating a =>
  a
tonysAudTotal =
  tonysPortion * audTotal
