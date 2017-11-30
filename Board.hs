module Board where

rowMaker x = take x (repeat 0)

colMaker n x = if n > 0 then rowMaker x:colMaker(n-1) x
          else []

col x bd s = if s <= (size bd) then (row s bd !! (x-1)):col x bd (s+1)
             else []

mkBoard n = if n > 0 then rowMaker n:colMaker (n-1) n
            else []

mkPlayer = 1

mkOpponent = 2

size bd = length bd

row y bd = bd !! (y-1)

column x bd = (row 1 bd !! (x-1)):col x bd 2

mark x y bd p = take (y-1) bd++[take (x-1) (row y bd)++[p]++drop x (row y bd)]++drop y bd

isEmpty x y bd = ((row y bd) !! (x-1) == 0)

isMarked x y bd = ((row y bd) !! (x-1) /= 0)

isMarkedBy x y bd p = ((row y bd) !! (x-1) == p)

marker x y board = (row y board) !! (x-1)

isFull bd = product [minimum x | x <-bd] /= 0

isWonBy bd p x y = (((checkHorizontalRight bd p (x+1) y 1)+(checkHorizontalLeft bd p (x-1) y 1)) >= 5)

isDraw bd = (isFull bd) -- && (not(isWonBy bd mkPlayer) || not(isWonBy bd mkOpponent))

isGameOver bd = (isDraw bd) -- || (isWonBy bd mkPlayer) || (isWonBy bd mkOpponent)

boardToStr playerToChar bd = iterateBoard playerToChar 1 1 bd

iterateBoard playerToChar y x bd = if x <= (size bd) then
                                      if (marker x y bd) /= 0 then (playerToChar (marker x y bd))++(iterateBoard playerToChar y (x+1) bd)
				      else " . "++iterateBoard playerToChar y (x+1) bd
                                   else if y < size bd then "\n"++iterateBoard playerToChar (y+1) 1 bd
				   else ""

checkHorizontalRight bd p x y c = if x <= 0 && x > (size bd)
		                  then
		                   if (isMarkedBy x y bd p) == p
		                   then checkHorizontalRight bd p (x+1) y (c+1)
		                   else c
				  else c

checkHorizontalLeft bd p x y c = if x <= 0 && x > (size bd)
		                 then
		                  if (isMarkedBy x y bd p) == p
		                  then checkHorizontalLeft bd p (x-1) y (c+1)
		                  else c
				 else c