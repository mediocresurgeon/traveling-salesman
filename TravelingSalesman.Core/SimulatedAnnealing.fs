namespace TravelingSalesman.Core.TspAlgorithms

open System.Collections.Generic
open TravelingSalesman.Core.DTO
open TravelingSalesman.Core.Geodesics

module SimulatedAnnealing = 
    let internal swapElementsAtIndex (locations:Location[]) (index1:int) (index2:int) = 
        let temp = locations.[index1]
        locations.[index1] <- locations.[index2]
        locations.[index2] <- temp
        locations

    let toTuple (location:Location) =
        location.LatitudeRadians, location.LongitudeRadians

    let GetAngle (previousResults:Dictionary<(Location * Location), float>) (location1:Location) (location2:Location) =
        // 1) Sort the two coordinates into a key
        // 2) Look in the dictionary to see if a result has already been calculated
        // 2a) If a result was already calculated, return that result
        // 2b) Otherwise, calculate the result, save it to the dictionary and return the result
        
        let sortedLocations = [| location1; location2; |] |> Seq.sortBy(fun l -> l.Name)
        let firstLoc = Seq.head sortedLocations
        let secondLoc = Seq.tail sortedLocations |> Seq.head
        let dicKey = (firstLoc, secondLoc)

        match previousResults.TryGetValue dicKey with
                         | true, value -> value
                         | _           -> let p1 = toTuple location1
                                          let p2 = toTuple location2
                                          let angle = HaversineFormula.GetAngle p1 p2
                                          previousResults.Add(dicKey, angle)
                                          angle