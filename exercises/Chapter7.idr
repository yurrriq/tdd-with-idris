module Chapter7

import Chapter4 -- For Shape

%access public export


-- Exercise 7.1.6.1

Eq Shape where
  (==) (Triangle  b1 h1) (Triangle  b2 h2) = b1 == b2 && h1 == h2
  (==) (Triangle  _  _)   _                = False
  (==) (Rectangle w1 h1) (Rectangle w2 h2) = w1 == w2 && h1 == h2
  (==) (Rectangle _  _)   _                = False
  (==) (Circle    r1)    (Circle r2)       = r1 == r2
  (==) (Circle    _)      _                = False


-- Exercise 7.1.6.2

Ord Shape where
 compare x y = compare (area x) (area y)
