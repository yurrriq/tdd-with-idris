||| An abstract Shape data type (from Listing 10.11).
module Shape

%access  export
-- %default total

||| A shape is a triangle, rectangle or circle.
data Shape = Triangle  Double Double
           | Rectangle Double Double
           | Circle    Double

--------------------------------------------------------------- [ Constructors ]
-- Export functions for building shapes, rather than their constructors.

triangle : (base, height : Double) -> Shape
triangle = Triangle

rectangle : (width, height : Double) -> Shape
rectangle = Rectangle

circle : (radius : Double) -> Shape
circle = Circle

---------------------------------------------------------- [ Exercise 10.3.4.2 ]

public export
data ShapeView  : (shape : Shape) -> Type where
     STriangle  : ShapeView shape
     SRectangle : ShapeView shape
     SCircle    : ShapeView shape

shapeView : (shape : Shape) -> ShapeView shape
shapeView (Triangle  _ _) = STriangle
shapeView (Rectangle _ _) = SRectangle
shapeView (Circle    _)   = SCircle

area : (shape : Shape) -> Double
area shape with (shapeView shape)
  area (Triangle  base  height) | STriangle  = 0.5 * base * height
  area (Rectangle width height) | SRectangle = width * height
  area (Circle    radius)       | SCircle    = pi * pow radius 2

------------------------------------------------------------------------ [ EOF ]
