import Data.List 

-- Point x y z
data Point = Point Int Int Int deriving (Eq, Ord, Show)
data Cube = Cube {start::Point, size::Int }  deriving (Eq, Show)

sampleInput :: [Cube]
sampleInput = [Cube { start = Point 0 0 0, size = 5},
               Cube { start = Point 4 4 4, size = 5},
               Cube { start = Point 8 8 8, size = 4}, 
               Cube { start = Point 12 12 12, size = 2},
               Cube { start = Point 13 13 13, size = 2},
               Cube { start = Point 10 10 0, size = 2},
               Cube { start = Point 9 9 0, size = 4}]

printIt :: [[Cube]] -> IO ()--Funkce pro výpis výsledků
printIt components = putStr (concat [show component ++ "\n" |component<-components])

components :: [Cube] -> [[Cube]]--Hlavní funkce programu - vytvoří komponenty
components [] = []
components cubes = componentsAddedCubes cubes []

componentsAddedCubes :: [Cube] -> [Cube] -> [[Cube]]--"Pomocná funkce" - Přidá parametr alreadyAddedCubes, což je seznam krychlí které jsou již obsaženy v některém ze seznamů kompoment
componentsAddedCubes [] _ = []
componentsAddedCubes (cube:cubes) alreadyAddedCubes = (componentsNewComponent (cube:cubes) alreadyAddedCubes) ++ componentsAddedCubes cubes (concat(alreadyAddedCubes:componentsNewComponent (cube:cubes) alreadyAddedCubes))

componentsNewComponent :: [Cube] -> [Cube] -> [[Cube]]--Při splnění podmínek přidá novou komponentu
componentsNewComponent [] _ = []
componentsNewComponent [x] _ = []
componentsNewComponent (firstCube:secondCube:cubes) (alreadyAddedCubes)
    | (doTwoCubesOverlap firstCube secondCube == True && isCubeInAlreadyAddedCubes firstCube alreadyAddedCubes == False && isCubeInAlreadyAddedCubes secondCube alreadyAddedCubes == False) = ([firstCube,secondCube] ++ (componentsExtendComponent secondCube cubes (firstCube:secondCube:alreadyAddedCubes))) : []
    | otherwise = componentsNewComponent (cubes) (alreadyAddedCubes)

componentsExtendComponent :: Cube -> [Cube] -> [Cube] -> [Cube]--Při splnění podmínek přidá do existující komponenty další krychle
componentsExtendComponent _ [] _ = []
componentsExtendComponent secondCube (thirdCube:cubes) alreadyAddedCubes
    | (doTwoCubesOverlap secondCube thirdCube == True && isCubeInAlreadyAddedCubes thirdCube alreadyAddedCubes == False) = thirdCube : (componentsExtendComponent thirdCube cubes alreadyAddedCubes)
    | otherwise = componentsExtendComponent secondCube cubes alreadyAddedCubes

isCubeInAlreadyAddedCubes :: Cube -> [Cube] -> Bool--Ověří, jestli už nebyla daná krychle přidána do některého seznamu komponentů
isCubeInAlreadyAddedCubes _ [] = False
isCubeInAlreadyAddedCubes thirdCube (firstAlreadyAddedCube:alreadyAddedCubes)
    | thirdCube == firstAlreadyAddedCube = True 
    | otherwise = isCubeInAlreadyAddedCubes thirdCube alreadyAddedCubes

doTwoCubesOverlap :: Cube -> Cube -> Bool--Ověří, jestli je mezi 2 danými krychlemi průnik
doTwoCubesOverlap Cube {start = Point firstCubeX firstCubeY firstCubeZ, size = firstCubeSize} Cube {start = Point secondCubeX secondCubeY secondCubeZ, size = secondCubeSize}
    | (firstCubeX + firstCubeSize) > (secondCubeX) && (firstCubeY + firstCubeSize) > (secondCubeY) && (firstCubeZ < secondCubeZ && (firstCubeZ + firstCubeSize) > (secondCubeZ) || firstCubeZ >= secondCubeZ && (firstCubeZ) <= (secondCubeZ + secondCubeSize)) = True 
    | otherwise = False