data Part =
  Part
    String -- name
    String -- brand
    String -- vendor
    String -- URL
    Price
    
data Price =
  Price
    Int -- cents
    Currency

data Currency =
  USD
  | AUD
