module Fuzzy
  ( Value,
    bot,
    top,
    clamp,
  )
where

-- | fuzzy truth value
type Value = Double

-- | falsity constant
bot :: Value
bot = 0.0

-- | truth constant
top :: Value
top = 1.0

-- | clamps fuzzy values to unit interval
clamp :: Value -> Value
clamp x = max bot $ min top x
