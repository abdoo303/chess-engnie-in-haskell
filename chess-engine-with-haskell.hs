type Location = (Char, Int)

data Player = White | Black deriving (Show, Eq)

data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)
data PieceType = WhitePiece | BlackPiece deriving (Show, Eq)

data Direction = U | D | L | RR| UL | UR | DL | DR deriving (Show, Eq)

type Board = (Player, [Piece], [Piece])


getPieceLocation :: Piece -> Location
getPieceLocation (P loc) = loc
getPieceLocation (N loc) = loc
getPieceLocation (K loc) = loc
getPieceLocation (Q loc) = loc
getPieceLocation (R loc) = loc
getPieceLocation (B loc) = loc

updatePieceLocation::Piece -> Location -> Piece
updatePieceLocation (P _ ) dest = (P dest)
updatePieceLocation (N _ ) dest = (N dest)
updatePieceLocation (K _ ) dest = (K dest)
updatePieceLocation (Q _ ) dest = (Q dest)
updatePieceLocation (R _ ) dest = (R dest)
updatePieceLocation (B _ ) dest = (B dest)

findType:: Location-> [Piece]->PieceType
findType p []= BlackPiece
findType p (x:xs) = if p==getPieceLocation x then WhitePiece else findType p xs

inverse:: Player -> Player
inverse Black= White
inverse White = Black
setBoard :: Board
setBoard = (White, [R ('h',1),N ('g',1),B ('f',1),K ('e',1),
                    Q ('d',1),B ('c',1),N ('b',1),R ('a',1),
                    P ('h',2),P ('g',2),P ('f',2),P ('e',2),
                    P ('d',2),P ('c',2),P ('b',2),P ('a',2)],
                    [R ('h',8),N ('g',8),B ('f',8),K ('e',8),
                    Q ('d',8),B ('c',8),N ('b',8),R ('a',8),
                    P ('h',7),P ('g',7),P ('f',7),P ('e',7),
                    P ('d',7),P ('c',7),P ('b',7),P ('a',7)])
getAllLocations:: [Location]
getAllLocations =[('a', 1), ('a', 2), ('a', 3), ('a', 4),
					('a', 5), ('a', 6), ('a', 7), ('a', 8),
					('b', 1), ('b', 2), ('b', 3), ('b', 4),
					('b', 5), ('b', 6), ('b', 7), ('b', 8),
					('c', 1), ('c', 2), ('c', 3), ('c', 4),
					('c', 5), ('c', 6), ('c', 7), ('c', 8),
					('d', 1), ('d', 2), ('d', 3), ('d', 4),
					('d', 5), ('d', 6), ('d', 7), ('d', 8),
					('e', 1), ('e', 2), ('e', 3), ('e', 4),
					('e', 5), ('e', 6), ('e', 7), ('e', 8),
					('f', 1), ('f', 2), ('f', 3), ('f', 4),
					('f', 5), ('f', 6), ('f', 7), ('f', 8),
					('g', 1), ('g', 2), ('g', 3), ('g', 4),
					('g', 5), ('g', 6), ('g', 7), ('g', 8),
					('h', 1), ('h', 2), ('h', 3), ('h', 4),
					('h', 5), ('h', 6), ('h', 7), ('h', 8)]

find :: [Piece] -> Location -> String

find [] _ = "  "
find ((P loc):xs) iloc
  | loc == iloc = "P"
  | otherwise = find xs iloc
find ((N loc):xs) iloc
  | loc == iloc = "N"
  | otherwise = find xs iloc
find ((K loc):xs) iloc
  | loc == iloc = "K"
  | otherwise = find xs iloc
find ((Q loc):xs) iloc
  | loc == iloc = "Q"
  | otherwise = find xs iloc
find ((R loc):xs) iloc
  | loc == iloc = "R"
  | otherwise = find xs iloc
find ((B loc):xs) iloc
  | loc == iloc = "B"
  | otherwise = find xs iloc
  

findRow :: Int -> [Char] -> [Piece] -> [Piece] -> String
findRow _ [] _ _ = "\n"
findRow n (c:col) white black =
  if find white (c, n) /= "  "
    then find white (c, n) ++ "W | " ++ findRow n col white black
    else if find black (c, n) /= "  "
           then find black (c, n) ++ "B | " ++ findRow n col white black
           else "   | " ++ findRow n col white black

visualizeHelper :: Board -> Int -> String
visualizeHelper (player, _, _) 0 = "\nTurn: " ++ show player
visualizeHelper (player, whites, blacks) n =
  show n ++ " | " ++ findRow n ['a'..'h'] whites blacks ++
  visualizeHelper (player, whites, blacks) (n-1)

visualizeBoard :: Board -> String
visualizeBoard (player, whites, blacks) = 
  "    a    b    c    d    e    f    g    h  \n" ++
  visualizeHelper (player, whites, blacks) 8



isEmpty:: Location -> [Piece] -> [Piece] -> Bool
isEmpty l white black = find white l == "  " && find black l == "  " 

findInc :: Piece -> [Piece] -> Int
findInc _ [] = -1
findInc (P loc) (x:xs)
  | (P loc) == x = 1
  | otherwise = findInc (P loc) xs
findInc (P loc) (_:xs) = findInc (P loc) xs

prev :: Char -> [Char] -> Char	
prev ' ' _ = ' '
prev 'a' _ = ' '
prev z (x:y:xs)
  | z == y = x
  | otherwise = prev z (y:xs)

next :: Char -> [Char] -> Char	
next ' ' _ = ' '
next 'h' _ = ' '
next z (x:y:xs)
  | z == x = y
  | otherwise = next z (y:xs)	
nxt::Char->Char
nxt c = next c ['a'..'h']
prv:: Char->Char
prv c = prev c ['a'..'h']

lastTargetOrEmpty :: PieceType -> Location -> [Piece] -> [Piece] -> Bool
lastTargetOrEmpty typ l white black = isEmpty l white black || findType l white /= typ

checkPath:: PieceType ->Direction ->Location -> Location-> Board-> Bool

checkPath _ _ (' ',_) _ _=False
checkPath _ _ (_,9) _ _ =False
checkPath _ _ (_,0) _ _ =False
checkPath  typ U (c,n) dest (player,white, black) = if (c,n+1) == dest then lastTargetOrEmpty typ dest white black
													else isEmpty (c,n+1) white black  
														&& checkPath typ U (c,n+1) dest (player,white,black)
checkPath  typ D (c,n) dest (player,white, black) = if (c,n-1) == dest then lastTargetOrEmpty typ dest white black
													else isEmpty (c,n-1) white black  
														&& checkPath typ D (c,n-1) dest (player,white,black)
checkPath  typ L (c,n) dest (player,white, black) = if (prv c,n) == dest then lastTargetOrEmpty typ dest white black
													else isEmpty (prv c,n) white black 
														&& checkPath typ L (prv c,n) dest (player,white,black) 
checkPath  typ RR (c,n) dest (player,white, black) = if (nxt c,n) == dest then lastTargetOrEmpty typ dest white black
													else isEmpty (nxt c,n) white black  
														&& checkPath typ RR (nxt c,n) dest (player,white,black)
														

checkPath  typ UR (c,n) dest (player,white, black) = if (nxt c,n+1) == dest then lastTargetOrEmpty typ dest white black
													else isEmpty (nxt c,n+1) white black  
													&& checkPath typ UR (nxt c,n+1) dest (player,white,black)
checkPath  typ UL (c,n) dest (player,white, black) = if (prv c,n+1) == dest then lastTargetOrEmpty typ dest white black
													else isEmpty (prv c,n+1) white black  
													&& checkPath typ UL (prv c,n+1) dest (player,white,black)
checkPath  typ  DR (c,n) dest (player,white, black) = if (nxt c,n-1) == dest then lastTargetOrEmpty typ dest white black
													else isEmpty (nxt c,n-1) white black  
													&& checkPath typ DR (nxt c,n-1) dest (player,white,black)
checkPath  typ DL (c,n) dest (player,white, black) = if (prv c,n-1) == dest then lastTargetOrEmpty typ dest white black
													else isEmpty (prv c,n-1) white black  
													&& checkPath typ DL (prv c,n-1) dest (player,white,black)
													


isLegal :: Piece -> Board -> Location -> Bool
isLegal (P (c,n)) (player, white, black) (cc,nn) = 
  if (c == cc && findInc (P (c,n)) white == nn - n) && isEmpty (cc,nn) white black then True
  else if c == cc && (2*findInc (P (c,n)) white == (nn - n)) && isEmpty (cc,nn) white black &&
    isEmpty (cc, n + findInc(P (c,n)) white) white black && ((findInc(P (c,n)) white==1 && n == 2) || ( findInc(P (c,n)) white ==(-1)&& n == 7)) then True  
  else if nn - n == (-1) && (prv c == cc || nxt c == cc) &&
    find white (cc, nn) /= "  " then True	 
  else if nn - n == 1 && (prv c == cc || nxt c== cc) &&
    find black (cc, nn) /= "  " then True
  else False
isLegal (R src) (player, white, black) dest = 
	 checkPath typ U src dest (player,white,black) ||
	 checkPath typ L src dest (player,white,black)||
	 checkPath typ D src dest (player,white,black)||
	 checkPath typ RR src dest (player,white,black)
	 where typ = findType src white 
	 
isLegal (B src) (player, white, black) dest = 
	 checkPath typ UL src dest (player,white,black) ||
	 checkPath typ UR src dest (player,white,black)||
	 checkPath typ DR src dest (player,white,black)||
	 checkPath typ DL src dest (player,white,black)
	 where typ = findType src white 
	 
isLegal (Q src) (player, white, black) dest = 
	 checkPath typ UL src dest (player,white,black) ||
	 checkPath typ UR src dest (player,white,black)||
	 checkPath typ DR src dest (player,white,black)||
	 checkPath typ DL src dest (player,white,black)||
	 checkPath typ U src dest (player,white,black) ||
	 checkPath typ L src dest (player,white,black)||
	 checkPath typ D src dest (player,white,black)||
	 checkPath typ RR src dest (player,white,black)
	 where typ = findType src white 


isLegal  (N (c,n)) (player, white, black) dest=
		 ( (nxt (nxt c),n+1)==dest && lastTargetOrEmpty typ dest white black )||
		 ( (nxt (nxt c),n-1)==dest && lastTargetOrEmpty typ dest white black )||
		 ( (prv (prv c),n+1)==dest && lastTargetOrEmpty typ dest white black )||
		 ( (prv (prv c),n-1)==dest && lastTargetOrEmpty typ dest white black )||
		 ( (nxt c,n+2)==dest && lastTargetOrEmpty typ dest white black )||
		 ( (nxt c,n-2)==dest && lastTargetOrEmpty typ dest white black )||
		 ( (prv c,n+2)==dest && lastTargetOrEmpty typ dest white black )||
		 ( (prv c,n-2)==dest && lastTargetOrEmpty typ dest white black )
		 where typ = findType (c,n) white 
isLegal (K (c,n)) (player, white, black) dest=
		 ( (nxt c,n+1)==dest && lastTargetOrEmpty typ dest white black )||
		 ( (nxt c,n-1)==dest && lastTargetOrEmpty typ dest white black )||
		 ( (prv c,n+1)==dest && lastTargetOrEmpty typ dest white black )||
		 ( (prv c,n-1)==dest && lastTargetOrEmpty typ dest white black )||
		 ( (nxt c,n)==dest && lastTargetOrEmpty typ dest white black )||
		 ( (c,n+1)==dest && lastTargetOrEmpty typ dest white black )||
		 ( (prv c,n)==dest && lastTargetOrEmpty typ dest white black )||
		 ( (c,n-1)==dest && lastTargetOrEmpty typ dest white black )
		 where typ = findType (c,n) white 


helperSuggest:: [Location]-> Piece -> Board ->[Location]
helperSuggest [] _ _ =[]
helperSuggest (x:xs) p  board= 
					if isLegal p board x then [x]++ helperSuggest xs p board
					else helperSuggest xs p board


suggestMove:: Piece -> Board -> [Location]
suggestMove p board = helperSuggest getAllLocations p board	

hisTurn:: Piece ->Board ->Bool 
hisTurn p (White ,white,black)=findType(getPieceLocation p ) white ==WhitePiece
hisTurn p (Black ,white,black)=findType(getPieceLocation p ) white ==BlackPiece

remove:: Location->[Piece]->[Piece]
remove _ []= []
remove loc (x:xs) = if getPieceLocation x ==loc then xs
						else x:(remove loc xs)
						
update:: Piece->Location->[Piece]->[Piece]
update _ _ []= []
update p loc (x:xs) = if x == p then ((updatePieceLocation p loc):xs) 
						else x:(update p loc xs)

updateBoard:: Piece -> Location -> Board -> Board
updateBoard  p dest (White,white,black) = (Black , (update p dest white) , (remove dest black)) 
updateBoard  p dest (Black,white,black) = (White ,(remove dest white), (update p dest black) ) 


move:: Piece -> Location -> Board -> Board 
move p loc (player ,white, black) = if hisTurn p (player,white,black) then
		( if   (isLegal p (player,white,black) loc) then updateBoard p loc (player,white,black) 
			else error ("Illegal move for piece "++ show  p))
					else error ("This is "++ (show player)++ " player's turn, " ++(show (inverse player))++" can't move.")
		
