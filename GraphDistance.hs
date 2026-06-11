-- Using Dykstra's algorithm to calculate the shortest distance between graph nodes

import Data.List (sortBy)
import qualified Data.Set as Set
import System.IO(readFile)

type Node = String

type Distance = Integer

data Connection = Connection Node Node Distance

type Graph = [Connection]

type AdjDistances = [(Node, Distance)]

type CalcDistance = (Node, Maybe Distance)

getAdjDistances :: Graph -> Node -> AdjDistances
getAdjDistances graph node = concatMap getAdjDistance graph
    where getAdjDistance (Connection node1 node2 distance)
            | node == node1 = [(node2, distance)]
            | node == node2 = [(node1, distance)]
            | otherwise = []

updateCalcDistance :: Maybe Distance -> AdjDistances -> CalcDistance -> CalcDistance
updateCalcDistance currDistance adjDistances (node, calcDistance) =
    let adjForNode = filter (\x -> fst x == node) adjDistances
    in
        if null adjForNode
            then (node, calcDistance)
        else
            let totalDistance = (+ snd (head adjForNode)) <$> currDistance
            in
                if null calcDistance then (node, totalDistance) else
                    (node, min <$> calcDistance <*> totalDistance)

sortCompare :: CalcDistance -> CalcDistance -> Ordering
sortCompare (_, dist1) (_, dist2)
    | null dist1 = GT
    | null dist2 = LT
    | otherwise = compare dist1 dist2

calculateDistance :: Graph -> Node -> Node -> Maybe Distance -> [CalcDistance] -> Maybe Distance
calculateDistance graph currentNode targetNode currentDistance calcDistances
    | currentNode == targetNode = currentDistance
    | null calcDistances = Nothing
    | otherwise =
        let adjDistances = getAdjDistances graph currentNode
            newCalcDistances = map (updateCalcDistance currentDistance adjDistances) calcDistances
            sortedDistances = sortBy sortCompare newCalcDistances
            (nextNode, nextDistance) = head sortedDistances
        in calculateDistance graph nextNode targetNode nextDistance (tail sortedDistances)

getAllNodes :: Graph -> [Node]
getAllNodes graph =
    let nodes1 = Set.fromList [node | (Connection node _ _) <- graph]
        nodes2 = Set.fromList  [node | (Connection _ node _) <- graph]
    in Set.toList $ Set.union nodes1 nodes2

getMinDistance :: Graph -> Node -> Node -> Maybe Distance
getMinDistance graph startNode endNode =
    let otherNodes = filter (/= startNode) $ getAllNodes graph
        calcDistances = [(node, Nothing) | node <- otherNodes]
    in calculateDistance graph startNode endNode (Just 0) calcDistances

getConnection :: String -> Connection
getConnection fileLine = 
    let [node1, node2, distanceStr] = words fileLine
        distance = read distanceStr
    in Connection node1 node2 distance

main :: IO ()
main = do

    putStrLn ""

    contents <- readFile "graph.txt"
    let graph = map getConnection (lines contents)

    putStrLn ("Distance between A and B: " ++ show (getMinDistance graph "A" "B"))
    putStrLn ("Distance between C and C: " ++ show (getMinDistance graph "C" "C"))
    putStrLn ("Distance between A and D: " ++ show (getMinDistance graph "A" "D"))
    putStrLn ("Distance between A and X: " ++ show (getMinDistance graph "A" "X"))
    putStrLn ("Distance between D and B: " ++ show (getMinDistance graph "D" "B"))
    putStrLn ("Distance between F and E: " ++ show (getMinDistance graph "F" "E"))
    putStrLn ("Distance between H and A: " ++ show (getMinDistance graph "H" "A"))
    putStrLn ("Distance between A and I: " ++ show (getMinDistance graph "A" "I"))
