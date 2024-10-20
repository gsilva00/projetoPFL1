import qualified Data.List
import qualified Data.Array -- Definir hash tables (map) -> Representação 2 enunciado
import qualified Data.Bits -- Usar bit masks para cidades visitadas (bastante mais rápido)

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String -- Vertex
type Path = [City]
type Distance = Int

type Edge = (City,City,Distance) -- Edge (Added by us)

type RoadMap = [Edge] -- List of Edges

-- Description: nub é O(E^2) no pior dos casos
-- !! NOTA: Dar sort e depois remover os duplicados é mais rápido -> O(listComp + concat + sort + rmDup) O(E + E + 2E log 2E + E) -> O(E log E) -> VERSÃO MAIS RECENTE
-- NOTA: Solução com lista auxiliar que é minimamente mais eficaz - O(E*2*m), onde E é o numero de edges, e m é o numero de cidades encontradas até então (<=2E)
cities :: RoadMap -> [City]
cities graph = rmDup . Data.List.sort $ concat [[c1,c2] | (c1,c2,_) <- graph]
  where
    rmDup :: (Eq a) => [a] -> [a]
    rmDup [] = []
    rmDup (x:xs) = x : rmDup (dropWhile (== x) xs)

-- Description: O(E) - traversal of the any function - stops at first True (lazy)
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent graph v1 v2 = any (\(c1,c2,_) -> (v1 == c1 && v2 == c2) || (v1 == c2 && v2 == c1)) graph

-- Description: O(E) - traversal of find function - stops at first element found
distance :: RoadMap -> City -> City -> Maybe Distance
distance graph v1 v2 = case Data.List.find (\(c1,c2,_) -> (v1 == c1 && v2 == c2) || (v1 == c2 && v2 == c1)) graph of
                       Nothing -> Nothing
                       Just (_,_,dist) -> Just dist

-- Description: O(E) - traversal
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent graph v = [(if v == c1 then c2 else c1, d) | (c1,c2,d) <- graph, v == c1 || v == c2]

-- Description: zip, drop -> O(n) -> O(2n); foldl is O(n-1) pairs in path of n cities in path; distance is called for each pair -> O(E); Total: O(E*(n-1)); At worst (path thru all the vertices) -> O(E*V)
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance graph path = foldl accPath (Just 0) (zip path (drop 1 path))
  where
    accPath :: Maybe Distance -> (City, City) -> Maybe Distance -- Helper function
    accPath Nothing _ = Nothing -- if any of the edges doesn't exist (distance call returns Nothing) -> the path returns Nothing
    accPath (Just total) (v1,v2) = case distance graph v1 v2 of
                                   Nothing -> Nothing
                                   Just currDist -> Just (total + currDist)

-- Description: [res] -> O(V); cI -> O(E); cC -> O(E^2) for cities function, O(E) or O(2E) for length and filter for each city -> O(V*E); sCC -> O(V*log V)
rome :: RoadMap -> [City]
rome graph = [res | (res,_) <- sortedCityCounts]
  where
    cityInstances = concat [[c1,c2] | (c1,c2,_) <- graph] -- Get all city occurences in edges (represents the number of edges connected to each city)
    cityCounts = [(city, length $ filter (== city) cityInstances) | city <- cities graph] -- Count the number of occurences for each city
    sortedCityCounts = Data.List.sortBy (\(_,count1) (_,count2) -> compare count2 count1) cityCounts -- Order by the city with most edges

-- Description:
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected [] = False
isStronglyConnected graph@((x,_,_):xs) = allVisited (dfs graph (zip allCities [0..]) x 0)
  where
    allCities = cities graph
    allVisited :: Int -> Bool
    allVisited visited = visited == (2^length allCities - 1)

dfs :: RoadMap -> [(City, Int)] -> City -> Int -> Int
dfs graph allCities currC visited
  | Data.Bits.testBit visited i = visited -- City already visited
  | otherwise = foldl (\updatedVisited edge -> dfsVisit graph currC edge allCities updatedVisited) (Data.Bits.setBit visited i) graph
  where
    i = case Data.List.find (\(x,_) -> x == currC) allCities of
        Just (_,index) -> index
        Nothing -> error "City not found" -- Should never happen
    -- Handle undirected edges (by interpreting them as 2 edges in opposite directions)
    dfsVisit :: RoadMap -> City -> Edge -> [(City, Int)] -> Int -> Int
    dfsVisit graph currC (c1, c2, _) allCities visited
      | c1 == currC = dfs graph allCities c2 visited
      | c2 == currC = dfs graph allCities c1 visited
      | otherwise = visited

-- Description: Dijkstra (lista de adjacencias)
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath graph v1 v2 = undefined

-- Description: definir um grafo com estrutura transformada com o where. No livro referido no enunciado O(n^2*2^n -> dynamic programming -> tabela -> nos +infinito pode-se usar nothing
travelSales :: RoadMap -> Path
travelSales graph = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

-- Tests added by us
gTest4 :: RoadMap
gTest4 = [("0","2",4),("0","4",4),("0","5",4),("1","4",4),("1","5",4),("2","3",4),("2","4",4),("4","5",4)]
