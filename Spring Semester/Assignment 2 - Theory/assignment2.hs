-- Question 1
class Distinguished a where
  special :: a

data Nat = Zero | Succ Nat deriving (Eq, Show)
data Stream a = None | Append (Stream a) a deriving (Eq, Show)

sPop :: Stream a -> Stream a
sPop s = case s of
  None          -> None                -- [sPop.0]
  Append None _ -> None                -- [sPop.1]
  Append s1 x   -> Append (sPop s1) x  -- [sPop.n]

sTop :: Distinguished a => Stream a -> a
sTop s = case s of
  None          -> special             -- [sTop.0]
  Append None x -> x                   -- [sTop.1]
  Append s1 _   -> sTop s1             -- [sTop.n]

sRev :: Distinguished a => Stream a -> Stream a
sRev s = case s of
  None          -> None                -- [sRev.0]
  Append None _ -> s                   -- [sRev.1]
  _             -> Append (sRev s1) x1 -- [sRev.n]
    where
      s1 = sPop s                      -- [sRev.s’]
      x1 = sTop s                      -- [sRev.x’]

sLen :: Stream a -> Nat
sLen s = case s of
  None        -> Zero                  -- [sLen.0]
  Append s1 _ -> Succ (sLen s1)        -- [sLen.n]

-- Question 2
f :: Num a => Either Bool a -> b
g :: c -> Either c Int

f = f
g = g
h = \x -> x f g
