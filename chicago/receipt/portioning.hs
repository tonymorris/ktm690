data Who =
  Rob
  | Tony
  deriving (Eq, Ord, Show)

audTotal ::
  Integer
audTotal =
  1746170

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
    Item "KTM 690 1190 R Offroad Bad Fuel Dongle 60312953000" 109990 Tony
  , Item "Oxford KTM Sport Heated Grips OXF.OF692Z" 84990 Tony
  , Item "Oxford KTM Sport Heated Grips OXF.OF692Z" 84990 Rob
  , Item "UNI KTM 690 High Flow Air Filter" 49990 Tony
  , Item "R&G KTM 690 Enduro SMC Fork Sliders FP0085BK" 51950 Tony
  , Item "R&G KTM 690 Enduro SMC Fork Sliders FP0085BK" 51950 Rob
  , Item "KTM 690 Enduro SMC Service Repair Manual DVD" 12595 Tony
  , Item "KTM 690 Enduro SMC Service Repair Manual DVD" 12595 Rob
  , Item "Cycra KTM Pro Bend Handguards" 129950 Rob
  , Item "KTM 690 Enduro SMC X2 Halogen Lighting Kit 76514901044" 264000 Rob
  , Item "Scotts KTM 690 Enduro SMC Rubber Mounted Damper Kit 2012+" 620000 Tony
  , Item "Shipping" 53560 Tony
  , Item "Shipping" 53560 Rob
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

robsAudTotal ::
  Floating a =>
  a
robsAudTotal =
  robsPortion * fromIntegral audTotal

tonysAudTotal ::
  Floating a =>
  a
tonysAudTotal =
  tonysPortion * fromIntegral audTotal

