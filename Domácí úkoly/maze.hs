{-
Josef Micak (MIC0378)
DÚ č.1 PP ZS 2021/2022
Zadání č. 3 z roku 2020/2021 - Maze
-}

sampleInput = ["*********",
               "*s*   * *",
               "* * * * *",
               "* * * * *",
               "*   *   *",
               "******* *",
               "        *",
               "*********"]

type Result = [String]
pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))

maze :: Result -> String -> [String]--Hlavní funkce
maze [] _ = [] 
maze (inputString:inputStrings) (move:moves) = mazeAddDot (inputString:inputStrings) (getAllCoordinates (findStart sampleInput 0) (findStart sampleInput 0) ((move:moves) ++ getLastCharacter(move:moves))) 0

getLastCharacter :: String -> String--Funkce získá poslední znak ze sekvence pohybů - zajistí provedení úplně posledního pohybu
getLastCharacter (move:moves)
    | length(moves) == 0 = [move]
    | otherwise = getLastCharacter moves

mazeAddDot :: Result -> [(Int,Int)] -> Int -> Result--Přidá tečky na všechny příslušné souřadnice
mazeAddDot [] _ _ = []
mazeAddDot (inputString:inputStrings) ((coordinateCol,coordinateRow):rowsAndCols) stringId = mazeAddDotHelper inputString ((coordinateCol,coordinateRow):rowsAndCols) 0 stringId : mazeAddDot inputStrings ((coordinateCol,coordinateRow):rowsAndCols) (stringId+1)

mazeAddDotHelper :: String -> [(Int,Int)] -> Int -> Int -> String--Pomocná funkce pro mazeAddDot
mazeAddDotHelper [] _ _ _ = []
mazeAddDotHelper (inputChar:inputChars) ((coordinateCol,coordinateRow):rowsAndCols) charId stringId
    --K přidání tečky dojde pouze pokud A) dané souřadnice se nacházejí v uloženém seznamu souřadnic; B) na daných souřadnicích je prádzné místo (=>nepřekryjeme tečkou zeď, ani startovní pole)
    | isCoordinateInList charId stringId ((coordinateCol,coordinateRow):rowsAndCols) == True && inputChar == ' ' = "." ++ mazeAddDotHelper inputChars ((coordinateCol,coordinateRow):rowsAndCols) (charId+1) stringId
    | otherwise = inputChar : mazeAddDotHelper inputChars ((coordinateCol,coordinateRow):rowsAndCols) (charId+1) stringId

findStart :: Result -> Int -> (Int, Int)--Zjistíme souřadnici startovního pole
findStart (inputString:inputStrings) stringId
    | findStartCharacter 's' inputString 0 /= -1 = (stringId, findStartCharacter 's' inputString 0)
    | otherwise = findStart inputStrings (stringId+1)

findStartCharacter :: Char -> String -> Int -> Int--Pomocná funkce pro findStart
findStartCharacter _ [] _ = -1--Číslo -1 je zde používáno jako "placeholder" - indikuje, že v daném inputStringu nebyl startCharacter nalezen
findStartCharacter startCharacter (inputChar:inputChars) charId
    | startCharacter == inputChar = charId
    | otherwise = findStartCharacter startCharacter inputChars (charId+1)

getAllCoordinates :: (Int,Int) -> (Int,Int) -> String -> [(Int,Int)]--Získáme seznam všech souřadnic, na které se v poli během běhu programu přesuneme
getAllCoordinates (startCol, startRow) (coordinateCol,coordinateRow) [] = []
getAllCoordinates (startCol, startRow) (coordinateCol,coordinateRow) (move:moves)
    --Zjišťujeme, že se jedná o souřadnice startovního pole - tyto souřadnice nejsou do seznamu přidány
    | (coordinateCol,coordinateRow) == (startCol, startRow) && move == 'd' = getAllCoordinates (startCol, startRow) (coordinateCol,coordinateRow+1) moves
    | (coordinateCol,coordinateRow) == (startCol, startRow) && move == 'u' = getAllCoordinates (startCol, startRow) (coordinateCol,coordinateRow-1) moves
    | (coordinateCol,coordinateRow) == (startCol, startRow) && move == 'l' = getAllCoordinates (startCol, startRow) (coordinateCol-1,coordinateRow) moves
    | (coordinateCol,coordinateRow) == (startCol, startRow) && move == 'r' = getAllCoordinates (startCol, startRow) (coordinateCol+1,coordinateRow) moves
    --Zde se již nejedná o souřadnice startovního pole, tudíž jsou souřadnice do seznamu přidány
    | move == 'd' = (coordinateCol,coordinateRow) : getAllCoordinates (startCol, startRow) (coordinateCol,coordinateRow+1) moves
    | move == 'u' = (coordinateCol,coordinateRow) : getAllCoordinates (startCol, startRow) (coordinateCol,coordinateRow-1) moves
    | move == 'l' = (coordinateCol,coordinateRow) : getAllCoordinates (startCol, startRow) (coordinateCol-1,coordinateRow) moves
    | move == 'r' = (coordinateCol,coordinateRow) : getAllCoordinates (startCol, startRow) (coordinateCol+1,coordinateRow) moves

getFirstCoordinate :: (Int,Int) -> Char -> (Int,Int)--Získá první set souřadnic (při absenci této funkce jsou do seznamu souřadnic chybně přidány souřadnice startovního pole)
getFirstCoordinate (startCol, startRow) move
    | move == 'd' = (startCol, startRow+1)
    | move == 'u' = (startCol, startRow-1)
    | move == 'l' = (startCol-1, startRow)
    | move == 'r' = (startCol+1, startRow)

isCoordinateInList :: Int -> Int -> [(Int,Int)] -> Bool--Zjistíme, jestli se na dané pole během běhu programu přesuneme - tzn. jestli jsou testované souřadnice v uloženém seznamu souřadnic
isCoordinateInList _ _ [] = False
isCoordinateInList charId stringId ((coordinateCol,coordinateRow):rowsAndCols)
    | (charId == coordinateCol && stringId == coordinateRow) = True 
    | otherwise = isCoordinateInList charId stringId rowsAndCols