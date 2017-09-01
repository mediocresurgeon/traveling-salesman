namespace TravelingSalesman.Core


open System


module Common =

    
    /// <summary>
    /// The result of an operation.
    /// </summary>
    /// <remarks>
    /// Taken from http://fsharpforfunandprofit.com/posts/recipe-part1
    /// <remarks>
    type public Result<'TSuccess,'TFailure> = 
        | Success of 'TSuccess
        | Failure of 'TFailure


    /// <summary>
    /// Returns data from a Success.
    /// Raises an exception when the input is a Failure.
    /// </summary>
    let internal Unwrap (result:Result<'TSuccess, 'TFailure>) =
        match result with
        | Success(r) -> r
        | _          -> failwith "Unable to unwrap a Failure."


    /// <summary>
    /// Transforms a function with one one-track argument into
    /// a function with one two-track argument.
    /// </summary>
    /// <remarks>
    /// Taken from http://fsharpforfunandprofit.com/posts/recipe-part2
    /// <remarks>
    let internal Bind1 (functionWithOneOneTrackArgument) = 
        function
        | Failure f -> Failure f
        | Success s -> functionWithOneOneTrackArgument s


    /// <summary>
    /// Transforms a function with two one-track arguments into
    /// a function with one two-track argument and one one-track argument.
    /// </summary>
    let internal Bind21 (functionWithTwoOneTrackArguments) = 
        fun twoTrackInput singleTrackInput ->
            match twoTrackInput with
            | Failure f -> Failure f
            | Success s -> functionWithTwoOneTrackArguments s singleTrackInput


    /// <summary>
    /// Transforms a function with two one-track arguments into
    /// a function with two two-track arguments.
    /// </summary>
    let internal Bind22 (functionWithTwoOneTrackArguments) = 
        fun twoTrackInput1 twoTrackInput2 ->
            match twoTrackInput1, twoTrackInput2 with
            | Failure f, _ -> Failure f
            | _, Failure f -> Failure f
            | Success s1, Success s2 -> functionWithTwoOneTrackArguments s1 s2


    /// <summary>
    /// Transforms a function with four one-track arguments into 
    /// a function with three two-track arguments and one one-track argument.
    /// </summary>
    let internal Bind2221 (functionWithFourOneTrackArguments) = 
        fun twoTrackInput1 twoTrackInput2 twoTrackInput3 oneTrackInput ->
            match twoTrackInput1, twoTrackInput2, twoTrackInput3 with
            | Failure f, _, _                    -> Failure f
            | _, Failure f, _                    -> Failure f
            | _, _, Failure f                    -> Failure f
            | Success s1, Success s2, Success s3 -> functionWithFourOneTrackArguments s1 s2 s3 oneTrackInput


    /// <summary>
    /// Transforms a function with four one-track arguments into 
    /// a function with four two-track arguments.
    /// </summary>
    let internal Bind2222 (functionWithFourOneTrackArguments) = 
        fun twoTrackInput1 twoTrackInput2 twoTrackInput3 twoTrackInput4 ->
            match twoTrackInput1, twoTrackInput2, twoTrackInput3, twoTrackInput4 with
            | Failure f, _, _, _                             -> Failure f
            | _, Failure f, _, _                             -> Failure f
            | _, _, Failure f, _                             -> Failure f
            | _, _, _, Failure f                             -> Failure f
            | Success s1, Success s2, Success s3, Success s4 -> functionWithFourOneTrackArguments s1 s2 s3 s4


    /// <summary>
    /// Transforms a function with one one-track argument into
    /// a function with one two-track argument.
    /// <summary>
    /// <remarks>
    /// Taken from http://fsharpforfunandprofit.com/posts/recipe-part2
    /// <remarks>
    let internal Map1 (oneTrackFunction) = 
        function
        | Failure f -> Failure f
        | Success s -> oneTrackFunction s
                       |> Success


    /// <summary>
    /// Transforms a function with two one-track arguments into
    /// a function with two two-track arguments.
    /// </summary>
    let internal Map22 (functionWithTwoOneTrackArguments) = 
        fun twoTrackInput1 twoTrackInput2 ->
            match twoTrackInput1, twoTrackInput2 with
            | Failure f, _           -> Failure f
            | _, Failure f           -> Failure f
            | Success s1, Success s2 -> functionWithTwoOneTrackArguments s1 s2
                                        |> Success


    /// <summary>
    /// Transforms a function with five one-track arguments into
    /// a function with four two-track arguments and one one-track argument.
    /// </summary>
    let internal Map22221 (functionWithFiveOneTrackArguments) = 
        fun twoTrackInput1 twoTrackInput2 twoTrackInput3 twoTrackInput4 oneTrackInput1 ->
            match twoTrackInput1, twoTrackInput2, twoTrackInput3, twoTrackInput4 with
            | Failure f, _, _, _                             -> Failure f
            | _, Failure f, _, _                             -> Failure f
            | _, _, Failure f, _                             -> Failure f
            | _, _, _, Failure f                             -> Failure f
            | Success s1, Success s2, Success s3, Success s4 -> functionWithFiveOneTrackArguments s1 s2 s3 s4 oneTrackInput1
                                                                |> Success


    /// <summary>
    /// Transforms a function with six one-track arguments into
    /// a function with four two-track arguments and two one-track arguments.
    /// </summary>
    let internal Map222211 (functionWithSixOneTrackArguments) = 
        fun twoTrackInput1 twoTrackInput2 twoTrackInput3 twoTrackInput4 oneTrackInput1 oneTrackInput2 ->
            match twoTrackInput1, twoTrackInput2, twoTrackInput3, twoTrackInput4 with
            | Failure f, _, _, _                             -> Failure f
            | _, Failure f, _, _                             -> Failure f
            | _, _, Failure f, _                             -> Failure f
            | _, _, _, Failure f                             -> Failure f
            | Success s1, Success s2, Success s3, Success s4 -> functionWithSixOneTrackArguments s1 s2 s3 s4 oneTrackInput1 oneTrackInput2
                                                                |> Success


    /// <summary>
    /// Transforms a function with eight one-track arguments into
    /// a function with seven two-track arguments and one one-track argument.
    /// </summary>
    let internal Map22222221 (functionWithEightOneTrackArguments) = 
        fun twoTrackInput1 twoTrackInput2 twoTrackInput3 twoTrackInput4 twoTrackInput5 twoTrackInput6 twoTrackInput7 oneTrackInput ->
            match twoTrackInput1, twoTrackInput2, twoTrackInput3, twoTrackInput4, twoTrackInput5, twoTrackInput6, twoTrackInput7 with
            | Failure f, _, _, _, _, _, _                                                        -> Failure f
            | _, Failure f, _, _, _, _, _                                                        -> Failure f
            | _, _, Failure f, _, _, _, _                                                        -> Failure f
            | _, _, _, Failure f, _, _, _                                                        -> Failure f
            | _, _, _, _, Failure f, _, _                                                        -> Failure f
            | _, _, _, _, _, Failure f, _                                                        -> Failure f
            | _, _, _, _, _, _, Failure f                                                        -> Failure f
            | Success s1, Success s2, Success s3, Success s4, Success s5, Success s6, Success s7 -> functionWithEightOneTrackArguments s1 s2 s3 s4 s5 s6 s7 oneTrackInput
                                                                                                    |> Success


    /// <summary>
    /// Measures the size of an angle.
    /// In general, degrees are no smaller than 0 and no greater than 360.
    /// </summary>
    [<Measure>] type public degree


    /// <summary>
    /// An object which defines a location on the surface of a sphere in degrees.
    /// </summary>
    [<Interface>]
    type public IDecimalCoordinate = 
        /// <summary>
        /// Returns the latitude in degrees.
        /// </summary>
        abstract member LatitudeDegrees : float<degree>
        /// <summary>
        /// Returns the longitude in degrees.
        /// </summary>
        abstract member LongitudeDegrees : float<degree>


    /// <summary>
    /// An object which defines a location on the surface of a sphere in radians.
    /// </summary>
    [<Interface>]
    type public IRadialCoordinate = 
        /// <summary>
        /// Returns the latitude in radians.
        /// </summary>
        abstract member LatitudeRadians : float
        /// <summary>
        /// Returns the longitude in radians.
        /// </summary>
        abstract member LongitudeRadians : float


    /// <summary>
    /// Converts degrees to radians.
    /// </summary>
    let internal DegreesToRadians (degrees:float<degree>) =
        Math.PI / 180.0<degree> * degrees


    /// <summary>
    /// Reduces a degrees mesurement to its simplest form.
    /// </summary>
    /// <returns>A value d where 0.0 <= d < 360.0</returns>
    let internal SimplifyDegrees (degrees:float<degree>) =
        let rec makePositive n =
            if n < 0.0<degree> then
                makePositive (n + 360.0<degree>)
            else
                n
        makePositive degrees
            |> (fun n -> n % 360.0<degree>)


    /// <summary>
    /// An object which specifies a named location on the surface of a sphere.
    /// </summary>
    [<StructuredFormatDisplay("{Name}: {LatitudeDegrees}, {LongitudeDegrees")>]
    type public Location(name:string, latitude:float<degree>, longitude:float<degree>) =
        member this.Name = name
        member this.LatitudeDegrees = SimplifyDegrees latitude
        member this.LatitudeRadians = DegreesToRadians this.LatitudeDegrees
        member this.LongitudeDegrees = SimplifyDegrees longitude
        member this.LongitudeRadians = DegreesToRadians this.LongitudeDegrees
        interface IDecimalCoordinate with
            member this.LatitudeDegrees = this.LatitudeDegrees
            member this.LongitudeDegrees = this.LongitudeDegrees
        interface IRadialCoordinate with
            member this.LongitudeRadians = this.LongitudeRadians
            member this.LatitudeRadians = this.LatitudeRadians
    

    /// <summary>
    /// An object which specifies a named location on the surface of a sphere at a specific point in time.
    /// </summary>
    [<StructuredFormatDisplay("{Name}: {LatitudeDegrees}, {LongitudeDegrees} @ {Timestamp}")>]
    type public Waypoint(name:string, latitude:float<degree>, longitude:float<degree>, timestamp:DateTime) = 
        inherit Location(name, latitude, longitude)
        member this.Timestamp = timestamp