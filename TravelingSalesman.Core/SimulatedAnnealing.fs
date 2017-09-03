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
    let internal GetAdjacentIndex (index:int) (arrayLength:int) = 
        (index + 1) % arrayLength

            
    /// <summary>
    /// Returns a shallow copy of an array, swapping the elements at the index1 and index2.
    /// </summary>
    /// <param name="array">The array to read from.</param>
    /// <param name="index1">The index of the first element to swap.</param>
    /// <param name="index2">The index of the second element to swap.</param>
    /// <returns>A new array.</returns>
    let internal SwapElementsAtIndex (array:array<'T>) (index1:int) (index2:int) = 
        let arrayCopy = Array.copy array
        let element1 = Array.get arrayCopy index1
        let element2 = Array.get arrayCopy index2
        Array.set arrayCopy index1 element2
        Array.set arrayCopy index2 element1
        arrayCopy


    /// <summary>
    /// Returns a shallow copy of an array with elements in a random order.
    /// </summary>
    /// <param name="random">The random number generator to use.</param>
    /// <param name="array">The array to read from.</param>
    /// <returns>A new array.</returns>
    let public ShuffleArray (random:Random) (array:array<'T>) = 
        // Fisher–Yates shuffle
        // https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_modern_algorithm
        let mutable newArray = array
        for i in 0..(array.Length-2) do
            let randomIndex = random.Next(i, array.Length) // first argument is inclusive; second argument is exclusive
            if i <> randomIndex then
                newArray <- SwapElementsAtIndex newArray i randomIndex
        newArray


    /// <summary>
    /// Returns a shallow copy of an array, where the new array has swapped the elements in two adjacent indexes.
    /// </summary>
    /// <param name="random">The random number generator to use.</param>
    /// <param name="array">The array to read from.</param>
    /// <returns>A new array.</returns>
    let internal SwapAdjacentRandom (random:Random) (array:array<'T>) = 
        let randomIndex = random.Next(array.Length)
        let adjIndex = GetAdjacentIndex randomIndex array.Length
        SwapElementsAtIndex array randomIndex adjIndex


    /// <summary>
    /// Converts an IRadialCoordinate to a tuple of (Latitude, Longitude) in radians..
    /// </summary>
    /// <param name="location">The Location object to convert.</param>
    /// <returns>A tuple of (Latitude, Longitude) in radians.</returns>
    let internal ToTuple (location:IRadialCoordinate) =
        location.LatitudeRadians, location.LongitudeRadians


    /// <summary>
    /// Determines the angle between two points on the surface of a sphere.
    /// </summary>
    /// <remarks>
    /// Looking up a known result in a dictionary is faster than performing the same calculation over and over again.
    /// </remarks>
    /// <param name="previousResults">A dictionary of the results of previous calculations.  This eliminates computational redundancy.</param>
    /// <param name="location1">The first location to compare.</param>
    /// <param name="location2">The second location to compare.</param>
    /// <returns>The angle (in radians) between location1 and location2.</returns>
    let internal GetAngle (previousResults:Dictionary<(Location * Location), float>) (location1:Location) (location2:Location) =
        // 1) Sort the two coordinates into a key
        // 2) Look in the dictionary to see if a result has already been calculated
        // 2a) If a result was already calculated, return that result
        // 2b) Otherwise, calculate the result, save it to the dictionary and return the result
        
        let sortedLocations = [| location1; location2; |]
                              |> Seq.sortBy(fun l -> l.Name)
        let firstLoc = Seq.head sortedLocations
        let secondLoc = Seq.tail sortedLocations
                        |> Seq.head
        let dicKey = (firstLoc, secondLoc)

        match previousResults.TryGetValue dicKey with
        | true, value -> value
        | _           -> let p1 = ToTuple location1
                         let p2 = ToTuple location2
                         let angle = HaversineFormula.GetAngle p1 p2
                         previousResults.Add(dicKey, angle)
                         angle

    
    /// <summary>
    /// Determines the relative distance of a collection of locations on the surface of a perfect sphere.
    /// </summary>
    let public GetRelativeDistance (knownDistances:Dictionary<(Location * Location), float>) (locations:array<Location>) =
        let mutable runningTotal = 0.0
        let arrayLength = locations.Length
        for index in 0..(arrayLength-1) do
            let adjacentIndex = GetAdjacentIndex index arrayLength
            let location1 = Array.get locations index
            let location2 = Array.get locations adjacentIndex
            let angle = GetAngle knownDistances location1 location2
            runningTotal <- runningTotal + angle
        runningTotal
             

    /// <summary>
    /// Determines the probability that a given solution will be accepted.
    /// </summary>
    /// <remarks>
    /// This keeps the Simulated Annealing algorithm from getting stuck in a local minima.
    /// </remarks>
    let internal GetAcceptanceProbability (currentEnergy:float) (newEnergy:float) (temperature:float) =
        if (currentEnergy > newEnergy) then
            1.0
        else
            Math.Exp((currentEnergy - newEnergy) / temperature)


    /// <summary>
    /// Calculates an efficient circuit through all locations.
    /// Distances are compared using the Haversine Formula.
    /// </summary>
    let public OptimizePathHaversine (startingTemperature:float, finalTemperature:float, coolingRate:float) (random:Random) (locations:list<Location>) = 
        // 1) Begin with a random collection of locations.
        // 2) Loop until computation budget is spent:
        // 2.1) Swap two adjacent elements
        // 2.2) Record the new result
        // 2.3) Compare the new distance to the previous best distance.
        // 2.3.1) If the new distance is better, keep the new best result and loop again.
        // 2.3.2) If the new distance is worse...
        // 2.3.2.1) Determine the acceptance probability
        // 2.3.2.2) Randomly determine whether the new value is accepted as a best result
        // 2.3.2.2.1) If the new value is accepted, replace keep the best result and loop again.

        let distances = new Dictionary<(Location * Location), float>()

        let mutable bestSolution = List.toArray locations
                                   //|> (ShuffleArray random)
        let arrayLength = Array.length bestSolution

        /// <summary>
        /// Determines the weight of a given distance solution.
        /// </summary>
        let getSolutionEnergy (distance:float) = 
            distance / float arrayLength

        let mutable bestSolutionEnergy = GetRelativeDistance distances bestSolution
                                         |> getSolutionEnergy
        
        let budgetMultiplier = 1.0 - coolingRate
        let mutable currentResult = Array.copy bestSolution
        let mutable computationBudget = startingTemperature
        while computationBudget > finalTemperature do
            currentResult <- SwapAdjacentRandom random currentResult
            let currentSolutionEnergy = GetRelativeDistance distances currentResult
                                        |> getSolutionEnergy
            if currentSolutionEnergy < bestSolutionEnergy then
                bestSolution <- currentResult
                bestSolutionEnergy <- currentSolutionEnergy
            else
                let acceptanceprobability = GetAcceptanceProbability bestSolutionEnergy currentSolutionEnergy computationBudget
                if acceptanceprobability > random.NextDouble() then
                    bestSolution <- currentResult
                    bestSolutionEnergy <- currentSolutionEnergy
            computationBudget <- computationBudget * budgetMultiplier

        Array.toList bestSolution