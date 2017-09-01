namespace TravelingSalesman.Core.TspAlgorithms


open System
open System.Collections.Generic
open TravelingSalesman.Core.Common
open TravelingSalesman.Core.Geodesics


module SimulatedAnnealing = 


    /// <summary>
    /// Returns the next highest index of an array.
    /// If the input index is the final array element, index zero is returned.
    /// </summary>
    /// <param name="index">The known index.</param>
    /// <param name="arrayLength">The index count of the array.</param>
    /// <returns>The adjacent index.</returns>
    let internal getAdjacentIndex (index:int) (arrayLength:int) = 
        (index + 1) % arrayLength

            
    /// <summary>
    /// Returns a shallow copy of an array, swapping the elements at the index1 and index2.
    /// </summary>
    /// <param name="array">The array to read from.</param>
    /// <param name="index1">The index of the first element to swap.</param>
    /// <param name="index2">The index of the second element to swap.</param>
    /// <returns>A new array.</returns>
    let internal swapElementsAtIndex (array:array<'T>) (index1:int) (index2:int) = 
        let arrayCopy = Array.copy array
        let element1 = arrayCopy.[index1]
        arrayCopy.[index1] <- arrayCopy.[index2]
        arrayCopy.[index2] <- element1
        arrayCopy


    /// <summary>
    /// Returns a shallow copy of an array with elements in a random order.
    /// </summary>
    /// <param name="random">The random number generator to use.</param>
    /// <param name="array">The array to read from.</param>
    /// <returns>A new array.</returns>
    let internal shuffleArray (random:Random) (array:array<'T>) = 
        // Fisher–Yates shuffle
        // https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_modern_algorithm
        let mutable newArray = array
        for i in (array.Length-1)..1 do
            let randomIndex = random.Next(0, i+1) // first argument is inclusive; second argument is exclusive
            if i <> randomIndex then
                newArray <- swapElementsAtIndex newArray i randomIndex
        newArray


    /// <summary>
    /// Returns a shallow copy of an array, where the new array has swapped the elements in two adjacent indexes.
    /// </summary>
    /// <param name="random">The random number generator to use.</param>
    /// <param name="array">The array to read from.</param>
    /// <returns>A new array.</returns>
    let internal swapAdjacentRandom (random:Random) (array:array<'T>) = 
        let randomIndex = random.Next(array.Length)
        let adjIndex = getAdjacentIndex randomIndex array.Length
        swapElementsAtIndex array randomIndex adjIndex


    /// <summary>
    /// Converts an IRadialCoordinate to a tuple of (Latitude, Longitude) in radians..
    /// </summary>
    /// <param name="location">The Location object to convert.</param>
    /// <returns>A tuple of (Latitude, Longitude) in radians.</returns>
    let internal toTuple (location:IRadialCoordinate) =
        location.LatitudeRadians, location.LongitudeRadians


    /// <summary>
    /// Determines the angle between two points on the surface of a sphere.
    /// </summary>
    /// <remarks>Looking up a known result in a dictionary is faster than performing the same calculation over and over again.</remarks>
    /// <param name="previousResults">A dictionary of the results of previous calculations.  This eliminates computational redundancy.</param>
    /// <param name="location1">The first location to compare.</param>
    /// <param name="location2">The second location to compare.</param>
    /// <returns>The angle (in radians) between location1 and location2.</returns>
    let internal GetAngle (previousResults:Dictionary<(Location * Location), float>) (location1:Location) (location2:Location) =
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