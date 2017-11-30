import Board

playerToChar p = if p == 1 then " O "
                 else " X "

readXY bd p = do
              putStrLn ((playerToChar p)++"'s turn: enter x (1-15) or -1 to quit")
              line <- getLine
              let parsed = reads line :: [(Integer, String)] in
	       if length parsed == 0
	       then readXY'
	       else let (x, _) = head parsed in
	       if (fromIntegral x) > 0 && (fromIntegral x) <= size bd
	       then do
	       putStrLn ((playerToChar p)++"'s turn: enter y (1-15) or -1 to quit")
	       line <- getLine
	       let parsed = reads line :: [(Integer, String)] in
	        if length parsed == 0
	        then readXY'
	        else let (y, _) = head parsed in
	        if (fromIntegral y) > 0 && (fromIntegral y) <= size bd
	        then do
	        let board = mark (fromIntegral x) (fromIntegral y) bd p
	        putStrLn (boardToStr playerToChar board)
	        if p == 1
	        then readXY board mkOpponent
	        else readXY board mkPlayer
	        else if (fromIntegral y) == -1
	        then do
	        putStrLn "Bye!"
	        else readXY'
               else if (fromIntegral x) == -1
	       then do
               putStrLn "Bye!"
	       else readXY'
               where
               readXY' = do
	       putStrLn "Invalid input!"
               readXY bd p

main = do
       putStrLn (boardToStr playerToChar (mkBoard 15))
       readXY (mkBoard 15) mkPlayer