-- ----------------------------------------------------------- [ DataTypes.idr ]
-- Module      : Chapter.DataTypes
-- Description : Definitions from Chapter 4 of Edwin Brady's book,
--               "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Chapter.DataTypes

%access export

-- ------------------------------------------------------- [ 4.1.2 Union Types ]

public export
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

%name Shape shape, shape1, shape2

namespace Shape

  ||| Calculate the area of a shape.
  area : Shape -> Double
  area (Triangle base  height)  = 0.5 * base * height
  area (Rectangle width height) = width * height
  area (Circle radius)          = pi * radius * radius

-- --------------------------------------------------- [ 4.1.3 Recursive Types ]

public export
data Picture = ||| A primitive shape.
               Primitive Shape
             | ||| A combination of two other pictures.
               Combine Picture Picture
             | ||| A picture rotated through an angle.
             Rotate Double Picture
             | ||| A picture translated to a different location.
             Translate Double Double Picture

%name Picture pic, pic1, pic2

namespace Picture

  area : Picture -> Double
  area (Primitive shape)   = area shape
  area (Combine pic pic1)  = area pic + area pic1
  area (Rotate _ pic)      = area pic
  area (Translate _ _ pic) = area pic

-- ------------------------------------------------ [ 4.1.4 Generic Data Types ]

||| A binary search tree.
public export
data Tree elem = ||| A tree with no data.
                 Empty
               | ||| A node with a left subtree, a value, and a right subtree.
                 Node (Tree elem) elem (Tree elem)
               
%name Tree tree, tree1

||| Insert a value into a binary search tree.
||| @ x a value to insert
||| @ tree a binary search tree to insert into
insert : Ord elem => (x : elem) -> (tree : Tree elem) -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right)
       = case compare x val of
              LT => Node (insert x left) val right
              EQ => orig
              GT => Node left val (insert x right)

-- --------------------------------------------------------------------- [ EOF ]
