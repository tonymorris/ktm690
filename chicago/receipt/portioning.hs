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

class Aud a where
  toAud ::
    Fractional x =>
    a
    -> x

instance Aud Item where
  toAud (Item _ a v _) =
    aud v (fromIntegral a)

instance Aud a => Aud [a] where
  toAud =
    sum . map toAud

class Usd a where
  toUsd ::
    Fractional x =>
    a
    -> x

instance Usd Item where
  toUsd (Item _ a _ _) =
    fromIntegral a

instance Usd a => Usd [a] where
  toUsd =
    sum . map toUsd

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

-- hack
showAmount ::
  (Show x, Fractional x) =>
  x
  -> String
showAmount x =
  let r = show (x / 1000)
      (d, c) = break (== '.') r
  in d ++ '.' : (take 3 . drop 1 $ c)

report =
  let r = rob items
      kr = ktmtwins r
      sr = sicass r
  in "# Costs\n\n\
      \### Rob\n\n\
      \##### ktmtwins.com\n\n\
      \" ++ (kr >>= \i@(Item n x _ _) -> "----\n\n" ++ n ++ "\n\n  **USD" ++ showAmount (fromIntegral x) ++ "** \n\n  **AUD" ++ showAmount (toAud i) ++ "**\n\n") ++
      "##### siccassracing.com\n\n\
      \" ++ (sr >>= \i@(Item n x _ _) -> "----\n\n" ++ n ++ "\n\n  **USD" ++ showAmount (fromIntegral x) ++ "** \n\n  **AUD" ++ showAmount (toAud i) ++ "**\n\n") ++
      "----\n\n\
      \## TOTAL\n\n\
      \**USD" ++ showAmount (toUsd r) ++ "**\n\n\
      \**AUD" ++ showAmount (toAud r) ++ "**" 


{-
Report

Rob
  ktmwins
    each item
    subtotal
  sicass
    each item
    subtotal

Tony
  ktmwins
    each item
    subtotal
  sicass
    each item
    subtotal

-}