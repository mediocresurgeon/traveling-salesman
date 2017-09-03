// Learn more about F# at http://fsharp.org

open System
open System.Collections.Generic
open System.Diagnostics
open System.Threading
open TravelingSalesman.Core.Common
open TravelingSalesman.Core.TspAlgorithms.SimulatedAnnealing


/// <summary>
/// Runs a task, but cancels it if it takes too long.
/// </summary>
/// <param name="timeout">In milliseconds.</param>
/// <param name="action">The task to execute.</param>
/// <returns>The result of the task.</returns>
let withTimeout timeout action = 
    // https://stackoverflow.com/a/32744262
    async {
        let! child = Async.StartChild(action, timeout)
        return! child
    }


/// <summary>
/// Tries to find a travelling salesman solution.
/// Will time out.
/// </summary>
/// <param name="timeout">In milliseconds.</param>
/// <param name="random">A random number generator (RNG).</param>
/// <param name="startingTemp">The starting temperature.</param>
/// <param name="finalTemp">The desired final temperature.</param>
/// <param name="coolingRate">The cooling rate.</param>
/// <param name="knownDistances">A dictionary of known distances.</param>
/// <param name="locations">The collection of locations to optimize.</param>
/// <returns>A solution to the travelling salesman problem.</returns>
let getSolutionWithTimeout timeout random startingTemp finalTemp coolingRate knownDistances locations =
    async {
        let finder =
            async {
                let getSolution = OptimizePathHaversine (startingTemp, finalTemp, coolingRate) random
                let bestRoute = getSolution locations
                let bestDistance = List.toArray bestRoute
                                   |> GetRelativeDistance knownDistances
                return bestRoute
            }
        return! withTimeout timeout finder    
    }





[<EntryPoint>]
let main argv = 
    let locations = [
        new Location("The Atrium - Massey University", -36.7331<degree>, 174.7011<degree>);
        new Location("Aperture Memorial", -36.7328<degree>, 174.7006<degree>);
        new Location("Massey IMS Building", -36.7338<degree>, 174.7012<degree>);
        new Location("Sir Neil Water Lecture Hall", -36.7338<degree>, 174.7019<degree>);
        new Location("Massey University Quadrangle Building", -36.7324<degree>, 174.7017<degree>);
        new Location("Matauranga Stained Window", -36.7324<degree>, 174.7013<degree>);
        new Location("Massey University Lost Sign", -36.7319<degree>, 174.7014<degree>);
        new Location("Study Centre Opening Plaque", -36.7316<degree>, 174.7004<degree>);
        new Location("Massey University Study Centre Courtyard", -36.7311<degree>, 174.7006<degree>);
        new Location("Albany Presbyterian Church", -36.730<degree>, 174.6977<degree>);
        new Location("Albany Village Cemetery", -36.7298<degree>, 174.6972<degree>);
        new Location("Albany Community Playground", -36.7286<degree>, 174.6982<degree>);
        new Location("Albany Community House", -36.7284<degree>, 174.6988<degree>);
        new Location("King George V Coronation Hall", -36.727<degree>, 174.6971<degree>);
        new Location("Albany Memorial Library", -36.7264<degree>, 174.6968<degree>);
        new Location("Holy Cross Anglican Church", -36.7253<degree>, 174.6961<degree>);
        new Location("Tranquil Space", -36.7253<degree>, 174.695<degree>);
        new Location("\"Balance\" Bronze Sculpture", -36.7254<degree>, 174.6949<degree>);
        new Location("Kell Park Tree House", -36.72525<degree>, 174.6941<degree>);
        new Location("Kell Park Heritage Trail", -36.725<degree>, 174.6942<degree>);
        new Location("Albany Village Community Anchor", -36.7246<degree>, 174.6944<degree>);
        new Location("Lucas Landing", -36.72345<degree>, 174.6929<degree>);
        new Location("Kell Park Phillips Property", -36.7242<degree>, 174.693<degree>);
        new Location("Art in the Park", -36.7242<degree>, 174.6933<degree>);
        new Location("Albany Orchards", -36.7248<degree>, 174.6934<degree>);
        new Location("The Oldtimer - Daniel Lucas", -36.7254<degree>, 174.6933<degree>);
        new Location("Kell Park Bridge Entrance", -36.7259<degree>, 174.6936<degree>);
        new Location("North Harbour Stadium Sculpture", -36.7269<degree>, 174.7028<degree>);
        new Location("Albany Stadium", -36.7278<degree>, 174.7023<degree>);
        new Location("Don Munri Plaque", -36.7261<degree>, 174.7026<degree>);
        new Location("Harbour Sports Sports House", -36.7261<degree>, 174.703<degree>);
        new Location("Marist North Harbour Rugby Club", -36.7259<degree>, 174.7035<degree>);
        new Location("North Harbour RC Club Race Track", -36.7258<degree>, 174.7043<degree>);    
        new Location("North Harbour Stadium Archway", -36.7248<degree>, 174.705<degree>);
        new Location("Iron Beanstalk", -36.7229<degree>, 174.7052<degree>);   
        new Location("Bronze Stars", -36.7228<degree>, 174.7047<degree>);
        new Location("Hooton Reserve", -36.7227<degree>, 174.7039<degree>);   
        new Location("Hooton Reserve Park Map", -36.722<degree>, 174.7052<degree>);
        new Location("Hooton Reserve Skate Bowl", -36.7212<degree>, 174.7053<degree>);
        new Location("Hooton Reserve Playground", -36.7211<degree>, 174.7056<degree>);
        new Location("Kawai Purapura", -36.7211<degree>, 174.7044<degree>);
    ]
    let random = new Random()

    let knownDistances = new Dictionary<(Location * Location), float>()

    (*
    // Correct magnitudes!
    let startingTemp = 0.000001
    let finalTemp = 0.0000000000000001
    let coolingRate = 0.01
    *)

    let startingTemp = 0.0000005
    let finalTemp = 0.0000000000000001
    let coolingRate = 0.01

    //let timeOutInMilliseconds = 1000
    //let initialStartingTemp   = 10000000000.0
    //let initialFinalTemp      = 10000000000.0
    //let coolingRate           = 0.001

    //let mutable bestResult = 1000.0
    //let mutable bestCombination = initialStartingTemp, initialFinalTemp, coolingRate
    //for startingMult in 0..20 do
    //    let startingTemp = 10.0 ** -(float startingMult)
    //    for finalMult in 0..20 do
    //        let finalTemp = 10.0 ** -(float finalMult)
    //        try
    //            let calc =
    //                getSolutionWithTimeout timeOutInMilliseconds random startingTemp finalTemp coolingRate
    //            let mutable solutions:List<float> = new List<float>()
    //            for attempts in 0..100 do
    //                calc knownDistances locations
    //                |> Async.RunSynchronously
    //                |> List.toArray
    //                |> GetRelativeDistance knownDistances
    //                |> solutions.Add
    //                |> ignore
    //            let average = solutions.ToArray()
    //                          |> Array.toList
    //                          |> List.sum
    //                          |> (fun total -> total / float solutions.Count)
    //            if average < bestResult then
    //                bestResult <- average
    //                bestCombination <- startingTemp, finalTemp, coolingRate
    //                printfn "New best!\nstartingTemp: %e\nfinalTemp: %e\ncoolingRate: %e" startingTemp finalTemp coolingRate
    //        with
    //        | _ -> ()
    
    //let s, f, c = bestCombination
    //printfn "startingTemp: %e\n finalTemp: %e\n coolingRate: %e" s f c 
    //printfn "average: %g" bestResult
    let originalDistance = List.toArray locations
                           |> GetRelativeDistance knownDistances
    
    printfn "Original distance: %A" originalDistance

    let mutable newRoute = List.toArray locations
                           |> ShuffleArray random
                           |> Array.toList
                           |> OptimizePathHaversine (startingTemp, finalTemp, coolingRate) random
    let mutable newDistance = List.toArray newRoute
                              |> GetRelativeDistance knownDistances
    let mutable attempts = 1

    while originalDistance <= newDistance do
        attempts <- attempts + 1
        let candidateRoute = OptimizePathHaversine (startingTemp, finalTemp, coolingRate) random newRoute
        let candidateDistance = List.toArray candidateRoute
                                |> GetRelativeDistance knownDistances
        printfn "Attempt result:    %A" candidateDistance
        if candidateDistance < newDistance then
            newRoute    <- candidateRoute
            newDistance <- candidateDistance

    for coordinate in newRoute do
        printfn "%s: %f, %f" coordinate.Name coordinate.LatitudeDegrees coordinate.LongitudeDegrees

    //if newDistance <= originalDistance then
        //for coordinate in newRoute do
        //printfn "%s: %f, %f" coordinate.Name coordinate.LatitudeDegrees coordinate.LongitudeDegrees
    
    printfn "Final distance:    %A" newDistance
    printfn "Attempts:          %i" attempts
    0 // return an integer exit code
