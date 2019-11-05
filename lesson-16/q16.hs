type FirstName = String
type LastName = String
type MiddleName = String

data Contact = AuthorConcat Author | Organisation String

data Creator = AuthorCreator Author | ArtistCreator Artist

data Author = Author Name

data Artist = Person Name | Band String

data Name = Name FirstName LastName
 | NameWithMiddle FirstName MiddleName LastName
 | TwoInitialsWithLast Char Char LastName
 | FirstNameWithTwoInits FirstName Char Char

data Book = Book {
 author      :: Creator
 , isbn      :: String
 , bookTitle :: String
 , bookYear  :: Int
 , bookPrice :: Double
 }

data VinylRecord = VinylRecord {
 artist        :: Creator
 , recordTitle :: String
 , recordYear  :: Int
 , recordPrice :: Double
 }

data CollectibleToy = CollectibleToy {
 name         :: String
 , descrption :: String
 , toyPrice   :: Double
 }

data Pamphlet = Pamphlet {
 title                :: String
 , pamphletDescrption :: String
 , contact            :: Contact
 }

data StoreItem = BookItem Book
 | RecordItem VinylRecord
 | ToyItem CollectibleToy
 | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book)     = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy)       = toyPrice toy
price (PamphletItem p)    = 0

----------------------------------------------------------------------------------------------------
	--
	--
----------------------------------------------------------------------------------------------------
type Radius = Double
type Size = Double
type Height = Size
type Width = Size

data Shape = CircleShape Circle
 | SquareShape Square
 | RectangleShape Rectangle

perimeter :: Shape -> Double
perimeter (CircleShape c)    = 2 * pi * radius c
perimeter (SquareShape  s)   = 4 * size s
perimeter (RectangleShape r) = 2 * ( w r + h r)
area :: Shape -> Double
area (CircleShape  c)    = pi * (radius c)^2
area (SquareShape  s)    = (size s)^2
area (RectangleShape  r) = (w r * h r)

data Circle = Circle {
 radius :: Radius
 }

data Square = Square {
 size :: Size
 }

data Rectangle = Rectangle {
 h   :: Height
 , w :: Width
 }
