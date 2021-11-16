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

maze :: Result -> String -> Result--Hlavní funkce
maze [] _ = [] 
maze (inputString:inputStrings) (move:moves) = mazeAddDot (inputString:inputStrings) (getAllCoordinates sampleInput (findStart sampleInput 0) (findStart sampleInput 0) ((move:moves) ++ getLastCharacter(move:moves))) 0

getLastCharacter :: String -> String--Funkce získá poslední znak ze sekvence pohybů - zajistí provedení úplně posledního pohybu
getLastCharacter (move:moves)
    | length(moves) == 0 = [move]
    | otherwise = getLastCharacter moves

mazeAddDot :: Result -> [(Int, Int)] -> Int -> Result--Přidá tečky na všechny příslušné souřadnice
mazeAddDot [] _ _ = []
mazeAddDot (inputString:inputStrings) ((coordinateCol, coordinateRow):rowsAndCols) stringId = mazeAddDotHelper inputString ((coordinateCol, coordinateRow):rowsAndCols) 0 stringId : mazeAddDot inputStrings ((coordinateCol, coordinateRow):rowsAndCols) (stringId+1)

mazeAddDotHelper :: String -> [(Int, Int)] -> Int -> Int -> String--Pomocná funkce pro mazeAddDot
mazeAddDotHelper [] _ _ _ = []
mazeAddDotHelper (inputChar:inputChars) ((coordinateCol, coordinateRow):rowsAndCols) charId stringId
    --K přidání tečky dojde pouze pokud A) dané souřadnice se nacházejí v uloženém seznamu souřadnic; B) na daných souřadnicích je prádzné místo (=> nepřekryjeme tečkou startovní pole)
    | isCoordinateInList charId stringId ((coordinateCol, coordinateRow):rowsAndCols) == True && inputChar == ' ' = "." ++ mazeAddDotHelper inputChars ((coordinateCol, coordinateRow):rowsAndCols) (charId+1) stringId
    | otherwise = inputChar : mazeAddDotHelper inputChars ((coordinateCol, coordinateRow):rowsAndCols) (charId+1) stringId

findStart :: Result -> Int -> (Int, Int)--Zjistíme souřadnici startovního pole
findStart (inputString:inputStrings) stringId
    | findStartCharacter 's' inputString 0 /= -1 = (stringId, findStartCharacter 's' inputString 0)
    | otherwise = findStart inputStrings (stringId+1)

findStartCharacter :: Char -> String -> Int -> Int--Pomocná funkce pro findStart
findStartCharacter _ [] _ = -1--Číslo -1 je zde používáno jako "placeholder" - indikuje, že v daném inputStringu nebyl startCharacter nalezen
findStartCharacter startCharacter (inputChar:inputChars) charId
    | startCharacter == inputChar = charId
    | otherwise = findStartCharacter startCharacter inputChars (charId+1)

getAllCoordinates :: Result -> (Int, Int) -> (Int, Int) -> String -> [(Int, Int)]--Získáme seznam všech souřadnic, na které se v poli během běhu programu přesuneme
getAllCoordinates (inputString:inputStrings) (startCol, startRow) (coordinateCol,coordinateRow) [] = []
getAllCoordinates (inputString:inputStrings) (startCol, startRow) (coordinateCol,coordinateRow) (move:moves)
    --Zjišťujeme, že bychom se daným pohybem dostali na souřadnice zdi - takový pohyb tedy nebereme v potaz
    | move == 'd' && length(moves) > 1 && isWallOnCoordinates (inputString:inputStrings) (coordinateCol, coordinateRow+1) 0 = getAllCoordinates (inputString:inputStrings) (startCol, startRow) (coordinateCol, coordinateRow) moves
    | move == 'u' && length(moves) > 1 && isWallOnCoordinates (inputString:inputStrings) (coordinateCol, coordinateRow-1) 0 = getAllCoordinates (inputString:inputStrings) (startCol, startRow) (coordinateCol, coordinateRow) moves
    | move == 'l' && length(moves) > 1 && isWallOnCoordinates (inputString:inputStrings) (coordinateCol-1, coordinateRow) 0 = getAllCoordinates (inputString:inputStrings) (startCol, startRow) (coordinateCol, coordinateRow) moves
    | move == 'r' && length(moves) > 1 && isWallOnCoordinates (inputString:inputStrings) (coordinateCol+1, coordinateRow) 0 = getAllCoordinates (inputString:inputStrings) (startCol, startRow) (coordinateCol, coordinateRow) moves
    --Zjišťujeme, že se jedná o souřadnice startovního pole - tyto souřadnice nejsou do seznamu přidány
    | move == 'd' && (coordinateCol, coordinateRow) == (startCol, startRow) = getAllCoordinates (inputString:inputStrings) (startCol, startRow) (coordinateCol, coordinateRow+1) moves
    | move == 'u' && (coordinateCol, coordinateRow) == (startCol, startRow) = getAllCoordinates (inputString:inputStrings) (startCol, startRow) (coordinateCol, coordinateRow-1) moves
    | move == 'l' && (coordinateCol, coordinateRow) == (startCol, startRow) = getAllCoordinates (inputString:inputStrings) (startCol, startRow) (coordinateCol-1, coordinateRow) moves
    | move == 'r' && (coordinateCol, coordinateRow) == (startCol, startRow) = getAllCoordinates (inputString:inputStrings) (startCol, startRow) (coordinateCol+1, coordinateRow) moves
    --Zde se již nejedná o souřadnice startovního pole, a na souřadnicích se nenachází zeď, tudíž jsou souřadnice do seznamu přidány
    | move == 'd' = (coordinateCol, coordinateRow) : getAllCoordinates (inputString:inputStrings) (startCol, startRow) (coordinateCol, coordinateRow+1) moves
    | move == 'u' = (coordinateCol, coordinateRow) : getAllCoordinates (inputString:inputStrings) (startCol, startRow) (coordinateCol, coordinateRow-1) moves
    | move == 'l' = (coordinateCol, coordinateRow) : getAllCoordinates (inputString:inputStrings) (startCol, startRow) (coordinateCol-1, coordinateRow) moves
    | move == 'r' = (coordinateCol, coordinateRow) : getAllCoordinates (inputString:inputStrings) (startCol, startRow) (coordinateCol+1, coordinateRow) moves

isCoordinateInList :: Int -> Int -> [(Int, Int)] -> Bool--Zjistíme, jestli se na dané pole během běhu programu přesuneme - tzn. jestli jsou testované souřadnice v uloženém seznamu souřadnic
isCoordinateInList _ _ [] = False
isCoordinateInList charId stringId ((coordinateCol, coordinateRow):rowsAndCols)
    | (charId == coordinateCol && stringId == coordinateRow) = True 
    | otherwise = isCoordinateInList charId stringId rowsAndCols

isWallOnCoordinates :: Result -> (Int, Int) -> Int -> Bool--Funkce zjistí, jestli se na daných souřadnicích nenachází zeď (pokud ano, souřadnice se do seznamu nepřidají)
isWallOnCoordinates [] _ _ = False
isWallOnCoordinates (inputString:inputStrings) (coordinateCol, coordinateRow) stringId
    | coordinateRow /= stringId = isWallOnCoordinates inputStrings (coordinateCol, coordinateRow) (stringId+1)
    | isWallOnCoordinatesHelper inputString coordinateCol 0 == True = True
    | otherwise = False

isWallOnCoordinatesHelper :: String -> Int -> Int -> Bool--Pomocná funkce pro isWallOnCoordinates
isWallOnCoordinatesHelper [] _ _ = False
isWallOnCoordinatesHelper (inputChar:inputChars) coordinateCol charId 
    | coordinateCol /= charId = isWallOnCoordinatesHelper inputChars coordinateCol (charId+1)
    | inputChar == '*' = True 
    | otherwise = False