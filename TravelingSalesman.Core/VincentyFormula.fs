namespace TravelingSalesman.Core.Geodesics

open TravelingSalesman.Core.DTO

/// <summary>
/// An algorithm for determining the distance between two points on an ellipsoid.
/// This can have an error as small as +/- 1mm on Earth.
/// Discovered by Thadeus Vincenty in 1975.
/// Unfortunately, there is a small set of values which cannot be computed.
/// </summary>
module VincentyFormula = 


    [<NoEquality;NoComparison>]
    type private IntermediateData =
        {
            sinλ   : float;
            cosλ   : float;
            sinSqσ : float;
            sinσ   : float;
            cosσ   : float;
            σ      : float;
            sinα   : float;
            cosSqα : float;
            cos2σM : float;
            C      : float;
        }
    

    /// <summary>
    /// Returns the reciprocal of a number.
    /// Zero has no reciprocal.
    /// </summary>
    /// <returns>The reciprocal of the input (or nothing, if the input was zero).</returns>
    let private GetReciprocal (n:float<'u>) = 
        match n with 
        | 0.0<_> -> None
        | _      -> Some(1.0 / n)
    

    /// <summary>
    /// Flattens an ellipsoid.
    /// </summary>
    /// <param name="semiMajorAxis">The measurement of the major axis of the ellipsoid.</param>
    /// <param name="semiMinorAxis">The measurement of the minor axis of the ellipsoid.</param>
    /// <returns>The flattened ellipsoid.</returns>
    let private FlattenEllipsoid (semiMajorAxis:float<'u>, semiMinorAxis:float<'u>) = 
        // f = 1 - (minorAxis/majorAxis) gives a less accurate result than
        // f = (majorAxis - minorAxis)/majorAxis
        match semiMajorAxis with
        | 0.0<_> -> None // Divide by zero
        | _      -> Some((semiMajorAxis - semiMinorAxis)/semiMajorAxis)


    /// <summary>
    /// Given a flattened ellipsoid and an angle, returns the tangent of the angle.
    /// </summary>
    /// <param name="f">The flattened ellipsoid.</param>
    /// <param name="θ">The angle (in radians).</param>
    /// <returns>The tangent of the angle.</returns>
    let private ToTan (f:float) (θ:float) = 
        (1.0 - f) * tan θ
    

    /// <summary>
    /// Given the value of the tangent of an angle, returns the cosine of the angle.
    /// </summary>
    /// <remarks>This operation is more performant than calling cos θ directly.</remarks>
    /// <param name="tanθ">The value of the tangent of an angle.</param>
    /// <returns>The cosine of the angle.</returns>
    let private ToCos (tanθ:float) = 
        (tanθ ** 2.0) + 1.0 // This value is guaranteed to be positive, since adding 1 to the square of any number is at least 1
            |> sqrt
            |> GetReciprocal
            |> (fun o -> o.Value) // This is always safe, since the argument to GetReciprocal is never zero
    

    /// <summary>
    /// Given the value of the tangent of an angle and the cosine of an angle, returns the sine of the angle.
    /// </summary>
    /// <remarks>This operation is more performant than calling sin θ directly.</remarks>
    /// <param name="tanθ">The value of the tangent of an angle.</param>
    /// <param name="cosθ">The value of the cosine of an angle.</param>
    /// <returns>The sine of the angle.</returns>
    let private ToSin (tanθ:float) (cosθ:float) = 
        tanθ * cosθ
       

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
        let a = semiMajorAxis
        let b = semiMinorAxis
        let φ1 = coordinate1.LatitudeRadians
        let λ1 = coordinate1.LongitudeRadians
        let φ2 = coordinate2.LatitudeRadians
        let λ2 = coordinate2.LongitudeRadians
        let f = FlattenEllipsoid (a,b)
        // unwrap f
        match f with
        | None    -> None // If the ellipsoid cannot be flattened, return early
        | Some(f) ->
        let L = λ2 - λ1
        let tanU1 = ToTan f φ1
        let cosU1 = ToCos tanU1
        let sinU1 = ToSin tanU1 cosU1
        let tanU2 = ToTan f φ2
        let cosU2 = ToCos tanU2
        let sinU2 = ToSin tanU2 cosU2

        let mutable λ = L
        let mutable λʹ = λ
        let mutable loopCount = 0

        let rec getIntermediateResults() = 
            loopCount <- loopCount + 1
            let sinλ = sin λ
            let cosλ = cos λ
            let sinSqσ = (cosU2 * sinλ) * (cosU2 * sinλ) + (cosU1 * sinU2 - sinU1 * cosU2 * cosλ) * (cosU1 * sinU2 - sinU1 * cosU2 * cosλ)
            let sinσ = sqrt sinSqσ
            if sinσ = 0.0 then
                None // This will give us a divide by zero error on the next line
            else
            let sinα = cosU1 * cosU2 * sinλ / sinσ
            let cosσ = sinU1 * sinU2 + cosU1 * cosU2 * cosλ
            let σ = atan2 sinσ cosσ
            let cosSqα = 1.0 - (sinα ** 2.0)
            if cosSqα = 0.0 then
                None // This will give us a divide by zero error on the next line
            else 
            let cos2σM = cosσ - 2.0 * sinU1 * sinU2 / cosSqα
            let cDenom = 16.0 * cosSqα * (4.0 + f * (4.0 - 3.0 * cosSqα))
            if cDenom = 0.0 then
                None // This will give us a divide by zero error on the next line
            else
            let C = f / cDenom
            λʹ <- λ
            λ <- L + (1.0-C) * f * sinα * (σ + C*sinσ*(cos2σM+C*cosσ*(-1.0+2.0*cos2σM**2.0)))
            if (loopCount >= 1000) then
                None                            // failed to converge - quit wasting CPU!
            elif (λ-λʹ |> abs > 0.000000000001) then
                getIntermediateResults()        // continue trying to converge
            else                                // return the converged result
                Some({
                        sinλ = sinλ
                        cosλ = cosλ
                        sinSqσ = sinSqσ
                        sinσ = sinσ
                        cosσ = cosσ
                        σ = σ
                        sinα = sinα
                        cosSqα = cosSqα
                        cos2σM = cos2σM
                        C = C
                })

        let calcs = getIntermediateResults()
        // Unwrap calcs
        match calcs with
        | None        -> None
        | Some(calcs) ->
                
        let uSq = calcs.cosSqα * (a*a - b*b) / (b*b)
        let A = 1.0 + uSq / 16384.0 * (4096.0 + uSq * (-768.0 + uSq * (320.0 - 175.0 * uSq)))
        let B = uSq / 1024.0 * (256.0 + uSq * (-128.0 + uSq * (74.0 - 47.0 * uSq)))
        let Δσ = B * calcs.sinσ * (calcs.cos2σM + B / 4.0 * (calcs.cosσ * (-1.0 + 2.0 * calcs.cos2σM * calcs.cos2σM) - B / 6.0 * calcs.cos2σM * (-3.0 + 4.0 * calcs.sinσ * calcs.sinσ) * (-3.0 + 4.0 * calcs.cos2σM * calcs.cos2σM)))

        Some(b*A*(calcs.σ-Δσ))