--Gerardo Muela
--Carlos Herrera
-- CS3360
-- Project 3: Omok Haskell


module Board where
--Make rows
rowMaker x = take x (repeat 0)
--Column maker 
colMaker n x = if n > 0 then rowMaker x:colMaker(n-1) x
          else []
--Get column
col x bd s = if s <= (size bd) then (row s bd !! (x-1)):col x bd (s+1)
             else []
--Make the board 
mkBoard n = if n > 0 then rowMaker n:colMaker (n-1) n
            else []
--Make Player: 1
mkPlayer = 1
--Make the Opponent: 2
mkOpponent = 2
--Return size of board
size bd = length bd
--Return a row of board
row y bd = bd !! (y-1)
--Return a column of board
column x bd = (row 1 bd !! (x-1)):col x bd 2
--Mark place in board
mark x y bd p = take (y-1) bd++[take (x-1) (row y bd)++[p]++drop x (row y bd)]++drop y bd
--Check if spot is empty
isEmpty x y bd = ((row y bd) !! (x-1) == 0)
--Check if spot is marked
isMarked x y bd = ((row y bd) !! (x-1) /= 0)
--Check who the spot is marked by
isMarkedBy x y bd p = ((row y bd) !! (x-1) == p)
--Mark a spot on the board
marker x y board = (row y board) !! (x-1)
--Check to see if all places on board are marked
isFull bd = product [minimum x | x <-bd] /= 0
--Check if 5 consecutive stones have been placed from all directions (Horiz,Vert, RightDiag, LeftDiag)
isWonBy bd p x y = (((checkHorizontalRight bd p (x+1) y 0)+(checkHorizontalLeft bd p (x-1) y 0)+1) >= 5)
                   || (((checkVerticalRight bd p x (y+1) 0)+(checkVerticalLeft bd p x (y-1) 0)+1) >= 5)
		   || (((checkDiagonalRRight bd p (x+1) (y+1) 0)+(checkDiagonalRLeft bd p (x-1) (y-1) 0)+1) >= 5)
		   || (((checkDiagonalLRight bd p (x+1) (y-1) 0)+(checkDiagonalLLeft bd p (x-1) (y+1) 0)+1) >= 5)
--Check if game is a Draw
isDraw bd = (isFull bd) -- && (not(isWonBy bd mkPlayer) || not(isWonBy bd mkOpponent))
--Check if Game is Over
isGameOver bd = (isDraw bd) -- || (isWonBy bd mkPlayer) || (isWonBy bd mkOpponent)
--Print the board
boardToStr playerToChar bd = iterateBoard playerToChar 1 1 bd
--Iterate through the board
iterateBoard playerToChar y x bd = if x <= (size bd) then
                                      if (marker x y bd) /= 0 then (playerToChar (marker x y bd))++(iterateBoard playerToChar y (x+1) bd)
				      else " . "++iterateBoard playerToChar y (x+1) bd
                                   else if y < size bd then "\n"++iterateBoard playerToChar (y+1) 1 bd
				   else ""
--Count how many consecutive stones have been placed on horizontal right half
checkHorizontalRight bd p x y c = if x > 0 && x <= (size bd)
		                  then
		                   if isMarkedBy x y bd p
		                   then checkHorizontalRight bd p (x+1) y (c+1)
		                   else c
				  else c
--Count how many consecutive stones have been placed on horizontal left half
checkHorizontalLeft bd p x y c = if x > 0 && x <= (size bd)
		                 then
		                  if isMarkedBy x y bd p
		                  then checkHorizontalLeft bd p (x-1) y (c+1)
		                  else c
				 else c
--Count how many consecutive stones have been placed on Vertical right half
checkVerticalRight bd p x y c = if y > 0 && y <= (size bd)
		                then
		                 if isMarkedBy x y bd p
		                 then checkVerticalRight bd p x (y+1) (c+1)
		                 else c
				else c
--Count how many consecutive stones have been placed on Vertical left half
checkVerticalLeft bd p x y c = if y > 0 && y <= (size bd)
		               then
		                if isMarkedBy x y bd p
		                then checkVerticalLeft bd p x (y-1) (c+1)
		                else c
			       else c
--Count how many consecutive stones have been placed on Diagonal Right right half
checkDiagonalRRight bd p x y c = if y > 0 && y <= (size bd) && x > 0 && x <= (size bd)
		                 then
		                  if isMarkedBy x y bd p
		                  then checkDiagonalRRight bd p (x+1) (y+1) (c+1)
		                  else c
				 else c
--Count how many consecutive stones have been placed on Diagonal Right left half
checkDiagonalRLeft bd p x y c = if y > 0 && y <= (size bd) && x > 0 && x <= (size bd)
		                then
		                 if isMarkedBy x y bd p
		                 then checkDiagonalRLeft bd p (x-1) (y-1) (c+1)
		                 else c
			        else c
--Count how many consecutive stones have been placed on Diagonal Left right half
checkDiagonalLRight bd p x y c = if y > 0 && y <= (size bd) && x > 0 && x <= (size bd)
		                 then
		                  if isMarkedBy x y bd p
		                  then checkDiagonalLRight bd p (x+1) (y-1) (c+1)
		                  else c
				 else c
--Count how many consecutive stones have been placed on Diagonal Left left half
checkDiagonalLLeft bd p x y c = if y > 0 && y <= (size bd) && x > 0 && x <= (size bd)
		                then
		                 if isMarkedBy x y bd p
		                 then checkDiagonalLLeft bd p (x-1) (y+1) (c+1)
		                 else c
			        else c