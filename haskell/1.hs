
-- Returns the list of integers <1000 that are multiples of 3 or 5.
-- I wish I could evaluate the compiler's output to know if it ever
-- actually constructed the list in memory (there is no need to).
--
-- Obviously this is much easier with list comprehensions, but that
-- requires "mod" arithmetic (slow!).
getlist :: Integer -> Integer -> [Integer]
getlist next3mul next5mul
  | next3mul >= 1000 && next5mul >= 1000 = []
  | next3mul < next5mul = next3mul : getlist (next3mul + 3) next5mul
  | next3mul > next5mul = next5mul : getlist next3mul (next5mul + 5)
  | next3mul == next5mul = next3mul : getlist (next3mul + 3) (next5mul + 5)

main = print (show (sum (getlist 3 5)))
