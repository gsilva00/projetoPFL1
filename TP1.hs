import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String -- Vertex
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)] -- Edge

-- Description: nub é O(n^2) no pior dos casos, há uma solução com lista auxiliar que é minimamente mais eficaz - O(n*(2*m)), onde n é o numero de edges, e m é o numero de cidades encontradas (<=2n)
cities :: RoadMap -> [City]
cities graph = Data.List.nub $ concat [[c1,c2] | (c1, c2, _) <- graph]

-- Description: O(n) - traversal
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent graph v1 v2 = any (\(c1, c2, _) -> (v1 == c1 && v2 == c2) || (v1 == c2 && v2 == c1)) graph

-- Description:
distance :: RoadMap -> City -> City -> Maybe Distance
distance graph v1 v2 = case Data.List.find (\(c1, c2, _) -> (v1 == c1 && v2 == c2) || (v1 == c2 && v2 == c1)) graph of
  Just (_, _, dist) -> Just dist
  Nothing -> Nothing

-- Description:
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent = undefined

-- Description:
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

-- Description:
rome :: RoadMap -> [City]
rome = undefined

-- Description:
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

-- Description:
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

-- Description:
travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
