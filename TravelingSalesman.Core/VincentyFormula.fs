namespace TravelingSalesman.Core.Geodesics


open System
open TravelingSalesman.Core.Common


/// <summary>
/// An algorithm for determining the distance between two points on an ellipsoid.
/// This can have an error as small as +/- 1mm on Earth.
/// Discovered by Thadeus Vincenty in 1975.
/// Unfortunately, there is a small set of values which cannot be computed.
/// </summary>
module VincentyFormula = 


    /// <summary>
    /// Vincenty's formula can encounter errors in specific circumstances.
    /// This represents a non-exceptional error encountered by the algorithm.
    /// </summary>
    type public VincentyError = 
        /// <summary>
        /// Unable to find a real solution.
        /// </summary>
        | NoSolution of string
        /// <summary>
        /// Unable to find a solution in the allotted computation budget.
        /// </summary>
        | FailedToConverge


    [<NoEquality;NoComparison>]
    type private FinalData =
        {
            sinλ   : float
            cosλ   : float
            sinSqσ : float
            sinσ   : float
            cosσ   : float
            σ      : float
            sinα   : float
            cosSqα : float
            cos2σM : float
            C      : float
            λʹ     : float
        }

    [<NoEquality;NoComparison>]
    type private IntermediateData =
        {
            sinλ   : float
            cosλ   : float
            sinSqσ : Result<float,VincentyError>
            sinσ   : Result<float,VincentyError>
            cosσ   : Result<float,VincentyError>
            σ      : Result<float,VincentyError>
            sinα   : Result<float,VincentyError>
            cosSqα : Result<float,VincentyError>
            cos2σM : Result<float,VincentyError>
            C      : Result<float,VincentyError>
            λʹ     : Result<float,VincentyError>
        }
    
    /// <summary>
    /// Returns the reciprocal of a number.
    /// Zero has no reciprocal.
    /// </summary>
    /// <returns>The reciprocal of the input (or nothing, if the input was zero).</returns>
    let private GetReciprocal (n:float<'u>) = 
        match n with 
        | 0.0<_> -> NoSolution "Zero does not have a reciprocal."
                    |> Failure
        | _      -> 1.0 / n
                    |> Success
    

    /// <summary>
    /// Flattens an ellipsoid.
    /// </summary>
    /// <param name="semiMajorAxis">The measurement of the major axis of the ellipsoid.</param>
    /// <param name="semiMinorAxis">The measurement of the minor axis of the ellipsoid.</param>
    /// <returns>The flattened ellipsoid.</returns>
    let private TryFlattenEllipsoid (semiMajorAxis:float<'u>, semiMinorAxis:float<'u>) = 
        // f = 1 - (minorAxis/majorAxis) gives a less accurate result than
        // f = (majorAxis - minorAxis)/majorAxis
        match semiMajorAxis with
        | 0.0<_> -> NoSolution "An ellipsoid cannot have a semi-major axis of zero."
                    |> Failure
        | _      -> (semiMajorAxis - semiMinorAxis)/semiMajorAxis
                    |> Success


    /// <summary>
    /// Given a flattened ellipsoid and an angle, returns the tangent of the angle.
    /// </summary>
    /// <param name="f">The flattened ellipsoid.</param>
    /// <param name="θ">The angle (in radians).</param>
    /// <returns>The tangent of the angle.</returns>
    let private GetTan (f:float) (θ:float) = 
        (1.0 - f) * tan θ
        |> Success
    

    /// <summary>
    /// Given the value of the tangent of an angle, returns the cosine of the angle.
    /// </summary>
    /// <remarks>This operation is more performant than calling cos θ directly.</remarks>
    /// <param name="tanθ">The value of the tangent of an angle.</param>
    /// <returns>The cosine of the angle.</returns>
    let private GetCos (tanθ:float) = 
        (tanθ ** 2.0) + 1.0                      // This value is guaranteed to be positive, since adding 1 to the square of any number is at least 1
        |> sqrt                                  // Never fails, since the input is always positive
        |> GetReciprocal                         // Will never fail (input is always positive)
        |> Unwrap                                // Will never fail (input is always Success)

   
    /// <summary>
    /// Given the tangent of an angle and the cosine of an angle, returns the sine of the angle.
    /// </summary>
    /// <remarks>This operation is more performant than calling sin θ directly.</remarks>
    /// <param name="tanθ">The value of the tangent of an angle.</param>
    /// <param name="cosθ">The value of the cosine of an angle.</param>
    /// <returns>The sine of the angle.</returns>
    let private GetSin (tanθ:float) (cosθ:float) = 
        tanθ * cosθ


    let private GetSinα (cosU1:float) (cosU2:float) (sinσ:float) (sinλ:float) = 
        match sinσ with 
        | 0.0 -> NoSolution "Unable to calculate intermediate value sinα (divide by zero)"
                 |> Failure
        | _   -> cosU1 * cosU2 * sinλ / sinσ
                 |> Success


    let private GetSinSqσ (cosU1:float) (cosU2:float) (sinU1:float) (sinU2:float) (cosλ:float) (sinλ:float) = 
        (cosU2 * sinλ) ** 2.0 + (cosU1 * sinU2 - sinU1 * cosU2 * cosλ) ** 2.0


    let private GetCosσ (cosU1:float) (cosU2:float) (sinU1:float) (sinU2:float) (cosλ:float) = 
        sinU1 * sinU2 + cosU1 * cosU2 * cosλ


    let private GetC (f:float) (cosSqα:float) =
        let denominator = 16.0 * cosSqα * (4.0 + f * (4.0 - 3.0 * cosSqα))
        match denominator with
        | 0.0 -> NoSolution "Unable to calculate intermediate value C (divide by zero)"
                 |> Failure
        | _   -> f / denominator
                 |> Success
    
    
    let private GetCosSqα (sinα:float) =
        1.0 - (sinα ** 2.0)


    let private GetCos2σM (sinU1:float) (sinU2:float) (cosσ:float) (cosSqα:float) = 
        match cosSqα with
        | 0.0 -> NoSolution "Unable to calculate intermediate value cos2σM (divide by zero)"
                 |> Failure
        | _ ->   cosσ - 2.0 * sinU1 * sinU2 / cosSqα
                 |> Success


    let private GetSqrt (n:float) =
        match n with
        | value when 0.0 > value -> String.Format("Cannot find square root of {0}", [value])
                                    |> NoSolution 
                                    |> Failure
        | _                      -> n
                                    |> sqrt
                                    |> Success


    let Getσ (sinσ:float) (cosσ:float) =
        atan2 sinσ cosσ

    
    let Getλ (C:float) (f:float) (sinα:float) (σ:float) (sinσ:float) (cosσ:float) (cos2σM:float) (L:float) =
        L + (1.0 - C) * f * sinα * (σ + C * sinσ * (cos2σM + C * cosσ * (-1.0 + 2.0 * cos2σM ** 2.0)))
    

    let private UnwrapResult (result:IntermediateData) =
        {
            FinalData.sinλ   = result.sinλ
            FinalData.cosλ   = result.cosλ
            FinalData.sinSqσ = Unwrap result.sinSqσ
            FinalData.sinσ   = Unwrap result.sinσ
            FinalData.sinα   = Unwrap result.sinα
            FinalData.cosσ   = Unwrap result.cosσ
            FinalData.σ      = Unwrap result.σ
            FinalData.cosSqα = Unwrap result.cosSqα
            FinalData.cos2σM = Unwrap result.cos2σM
            FinalData.C      = Unwrap result.C
            FinalData.λʹ     = Unwrap result.λʹ
        }
                                                                                        
    /// <summary>
    /// Calculates the distance between two points on an ellipsoid.
    /// The accuracy is +/- 1mm when the ellispoid is Earth.
    /// </summary>
    /// <remarks>
    /// Certain distance values cannot be calculated
    /// (such as when both points are on the equator, or when both points are on the poles).
    /// </remarks>
    /// <param name="semiMajorAxis">The radius of the ellipsoid at the equator.</param>
    /// <param name="semiMinorAxis">The radius of the ellipsoid at the poles.</param>
    /// <param name="coordinate1">The first location on the surface of the ellipsoid.</param>
    /// <param name="coordinate2">The first location on the surface of the ellipsoid.</param>
    /// <returns>The distance between the two points.</returns>
    let public GetDistance (semiMajorAxis:float<'u>, semiMinorAxis:float<'u>) (coordinate1:IRadialCoordinate) (coordinate2:IRadialCoordinate) =
        // F# translation of "Direct and Inverse Solutions of Geodesics
        // on the Ellipsoid with Application of Nested Functions" by
        // Thadeus Vincenty (1975).
        // https://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf

        /// <summary>
        /// Just like GetTan, except the f value can be a Result instead of a float.
        /// </summary>
        let tryGetTan = Bind21 GetTan

        /// <summary>
        /// Just like GetCos, except the tanθ value can be a Result instead of a float.
        /// </summary>
        let tryGetCos = Map1 GetCos

        /// <summary>
        /// Just like GetSin, except the tanθ and cosθ values can be a Results instead of a floats.
        /// </summary>
        let tryGetSin = Map22 GetSin

        let a     = semiMajorAxis
        let b     = semiMinorAxis
        let φ1    = coordinate1.LatitudeRadians
        let λ1    = coordinate1.LongitudeRadians
        let φ2    = coordinate2.LatitudeRadians
        let λ2    = coordinate2.LongitudeRadians
        let f     = TryFlattenEllipsoid (a,b)
        let L     = λ2 - λ1
        let tanU1 = tryGetTan f φ1
        let cosU1 = tryGetCos tanU1
        let sinU1 = tryGetSin tanU1 cosU1
        let tanU2 = tryGetTan f φ2
        let cosU2 = tryGetCos tanU2
        let sinU2 = tryGetSin tanU2 cosU2
       
        let tryGetIntermediateResults (λ:float) = 
            /// <summary>
            /// Just like GetSinα except the cosU1, cosU2, and sinσ arguments are Results instead of floats.
            /// (The sinλ argument is still a float.)
            /// <summary>
            let tryGetSinα = Bind2221 GetSinα

            /// <summary>
            /// Just like GetSinSqσ except the cosU1, cosU2, sinU1, sinU2 arguments are Results instead of floats.
            /// (The sinσ and sinλ arguments are still floats.)
            /// <summary>
            let tryGetSinSqσ = Map222211 GetSinSqσ

            /// <summary>
            /// Just like GetSqrt except the argument is a Result instead of a float.
            /// </summary>
            let tryGetSqσ = Bind1 GetSqrt

            /// <summary>
            /// Just like GetCosσ except the cosU1 cosU2 sinU1 sinU2 arguments are Results instead of floats.
            /// (The cosλ argument is still a float.)
            /// </summary>
            let tryGetCosσ = Map22221 GetCosσ

            /// <summary>
            /// Just like GetC except that the f and cosSqα arguments are Results instead of floats.
            /// </summary>
            let tryGetC = Bind22 GetC

            /// <summary>
            /// Just like GetCosSqα except that sinα argument is a Result instead of a float.
            /// </summary>
            let tryGetCosSqα = Map1 GetCosSqα

            /// <summary>
            /// Just like GetCos2σM except the sinU1, sinU2, cosσ, and cosSqα arguments are Results instead of floats.
            /// </summary>
            let tryGetCos2σM = Bind2222 GetCos2σM

            /// <summary>
            /// Just like Getσ except the sinσ and cosσ arguments are Results instead of floats.
            /// </summary>
            let tryGetσ = Map22 Getσ

            /// <summary>
            /// Just like Getλ except the C, f, sinα, σ, sinσ, cosσ, and cos2σM arguments are Results instead of floats.
            /// /// (The L argument is still a float.)
            /// </summary>
            let tryGetλ = Map22222221 Getλ

            let cosλ   = cos λ
            let sinλ   = sin λ
            let sinSqσ = tryGetSinSqσ cosU1 cosU2 sinU1 sinU2 cosλ sinλ
            let sinσ   = tryGetSqσ sinSqσ
            let sinα   = tryGetSinα cosU1 cosU2 sinσ sinλ
            let cosσ   = tryGetCosσ cosU1 cosU2 sinU1 sinU2 cosλ
            let σ      = tryGetσ sinσ cosσ
            let cosSqα = tryGetCosSqα sinα
            let cos2σM = tryGetCos2σM sinU1 sinU2 cosσ cosSqα
            let C      = tryGetC f cosSqα
            {
                sinλ   = sinλ
                cosλ   = cosλ
                sinSqσ = sinSqσ
                sinσ   = sinσ
                sinα   = sinα
                cosσ   = cosσ
                σ      = σ
                cosSqα = cosSqα
                cos2σM = cos2σM
                C      = C
                λʹ     = tryGetλ C f sinα σ sinσ cosσ cos2σM L
            }

        let rec getFinalResults (λ:float) (accumulator:int) =
            let result = tryGetIntermediateResults λ
            match result.λʹ with
            | _ when accumulator > 200                            -> Failure FailedToConverge
            | Failure(msg)                                        -> Failure msg
            | Success(value) when λ-value |> abs > 0.000000000001 -> getFinalResults value (accumulator+1)
            | Success(value)                                      -> UnwrapResult result
                                                                     |> Success

        let finalResults = getFinalResults L 0
        match finalResults with
        | Failure(msg)    -> Failure msg
        | Success(result) -> let uSq = result.cosSqα * (a*a - b*b) / (b*b)
                             let A = 1.0 + uSq / 16384.0 * (4096.0 + uSq * (-768.0 + uSq * (320.0 - 175.0 * uSq)))
                             let B = uSq / 1024.0 * (256.0 + uSq * (-128.0 + uSq * (74.0 - 47.0 * uSq)))
                             let Δσ = B * result.sinσ * (result.cos2σM + B / 4.0 * (result.cosσ * (-1.0 + 2.0 * result.cos2σM * result.cos2σM) - B / 6.0 * result.cos2σM * (-3.0 + 4.0 * result.sinσ * result.sinσ) * (-3.0 + 4.0 * result.cos2σM * result.cos2σM)))
                             Success (b*A*(result.σ-Δσ))