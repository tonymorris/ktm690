data Who =
  Rob
  | Tony
  deriving (Eq, Ord, Show)

data Vendor =
  KtmTwins
  | Sicass
  deriving (Eq, Ord, Show)

data Item =
  Item
    String
    Integer
    Vendor
    Who
  deriving (Eq, Ord, Show)

showVendor ::
  Vendor
  -> String
showVendor KtmTwins =
  "ktmtwins.com"
showVendor Sicass =
  "sicassracing.com"

exchange ::
  Fractional a =>
  Vendor
  -> a
exchange KtmTwins =
  1746170 / 1580120
exchange Sicass =
  339890 / 307920

aud ::
  Fractional a =>
  Vendor
  -> a
  -> a
aud v x =
  x * exchange v

items ::
  [Item]
items =
  [
    Item "KTM 690 1190 R Offroad Bad Fuel Dongle 60312953000" 109990 KtmTwins Tony
  , Item "Oxford KTM Sport Heated Grips OXF.OF692Z" 84990 KtmTwins Tony
  , Item "Oxford KTM Sport Heated Grips OXF.OF692Z" 84990 KtmTwins Rob
  , Item "UNI KTM 690 High Flow Air Filter" 49990 KtmTwins Tony
  , Item "R&G KTM 690 Enduro SMC Fork Sliders FP0085BK" 51950 KtmTwins Tony
  , Item "R&G KTM 690 Enduro SMC Fork Sliders FP0085BK" 51950 KtmTwins Rob
  , Item "KTM 690 Enduro SMC Service Repair Manual DVD" 12595 KtmTwins Tony
  , Item "KTM 690 Enduro SMC Service Repair Manual DVD" 12595 KtmTwins Rob
  , Item "Cycra KTM Pro Bend Handguards" 129950 KtmTwins Rob
  , Item "KTM 690 Enduro SMC X2 Halogen Lighting Kit 76514901044" 164000 KtmTwins Rob
  , Item "Scotts KTM 690 Enduro SMC Rubber Mounted Damper Kit 2012+" 620000 KtmTwins Tony
  , Item "Scotts KTM 690 Enduro SMC Rubber Mounted Damper Kit 2012+ (discount)" (-31000) KtmTwins Tony
  , Item "Shipping" 53560 KtmTwins Tony
  , Item "Shipping" 53560 KtmTwins Rob
  , Item "Tail tidy 21-690" 79950 Sicass Tony
  , Item "Rear flush mount indicators 22-100c" 44980 Sicass Tony
  , Item "Rear flush mount indicators 22-100c" 44980 Sicass Rob
  , Item "Turn signal resistor kit 22-690" 30960 Sicass Tony
  , Item "Turn signal resistor kit 22-690" 30960 Sicass Rob
  , Item "KTM Black Turn Signal Hand Guard Deflectors 22-206c" 59980 Sicass Rob
  , Item "Shipping" 8055 Sicass Rob
  , Item "Shipping" 8055 Sicass Tony
  ]

sumCost ::
  ([Item] -> [Item])
  -> Integer
sumCost f =
  sum . map (\(Item _ v _ _) -> v) . f $ items

rob ::
  [Item]
  -> [Item]
rob =
  filter (\(Item _ _ _ w) -> w == Rob)

tony ::
  [Item]
  -> [Item]
tony =
  filter (\(Item _ _ _ w) -> w == Rob)

ktmtwins ::
  [Item]
  -> [Item]
ktmtwins =
  filter (\(Item _ _ v _) -> v == KtmTwins)

sicass ::
  [Item]
  -> [Item]
sicass =
  filter (\(Item _ _ v _) -> v == Sicass)
