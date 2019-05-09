module Chapter11.Normalform where

-- data Fiction = Fiction deriving Show

-- data Nonfiction = Nonfiction deriving Show

-- data BookType = FictionBook Fiction
--               | NonfictionBook Nonfiction
--               deriving Show

type AuthorName = String

-- data Author = Author (AuthorName, BookType)

data Author = Fiction AuthorName
            | Nonfiction AuthorName
            deriving (Eq, Show)

-- Normal form
data Expr = Number Int
      | Add Expr Expr
      | Minus Expr
      | Mult Expr Expr
      | Divide Expr Expr

-- Stricter interpretation of normal form of the `Expr` datatype
type Number = Int
type Add = (Expr, Expr)
type Minus = Expr
type Mult = (Expr, Expr)
type Divide = (Expr, Expr)

type ExprNF = Either Number
                (Either Add
                    (Either Minus
                        (Either Mult Divide)))

-------------------------------------------------------
-- exercise 11.12
-- data FlowerType = Gardenia
--                 | Daisy
--                 | Rose
--                 | Lilac
--                 deriving Show

type Gardener = String

-- data Garden = Garden Gardener FlowerType deriving Show

data Garden = GardeniaGardener Gardener
            | DaisyGardener Gardener
            | RoseGardener Gardener
            | LilacGardener Gardener
            deriving Show
