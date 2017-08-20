namespace TravelingSalesman.Core.Geodesics

open TravelingSalesman.Core.DTO

/// <summary>
/// An algorithm for determining the distance between two points on an ellipsoid.
/// This can have an error as small as +/- 1mm on Earth.
/// Discovered by Thadeus Vincenty in 1975.
/// </summary>
module VincentyFormula = 
    [<NoEquality;NoComparison>]
    type private IntermediateData =
        {
            sinλ : float;
            cosλ : float;
            sinSqσ : float;
            sinσ : float;
            cosσ : float;
            σ : float;
            sinα : float;
            cosSqα : float;
            cos2σM : float;
            C : float;
        }

    let private reciprocal (n:float) = 
        1.0 / n
        
    let private flatten (a:float<'u>) (b:float<'u>) = 
        (a - b)/a

    let private reduceLatitude (f:float) (φ:float) = 
        (1.0 - f) * tan φ
     
    let private toCos (tanθ:float) = 
        (tanθ ** 2.0) + 1.0
            |> sqrt
            |> reciprocal

    let private toSin (tanθ:float) (cosθ:float) = 
        tanθ * cosθ
       


    let public GetDistance (semiMajorAxis:float<'u>, semiMinorAxis:float<'u>) (location1:IRadialCoordinate) (location2:IRadialCoordinate) =
        // F# translation of "Direct and Inverse Solutions of Geodesics
        // on the Ellipsoid with Application of Nested Functions" by
        // Thadeus Vincenty (1975).
        // https://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf
        let a = semiMajorAxis
        let b = semiMinorAxis
        let φ1 = location1.LatitudeRadians
        let λ1 = location1.LongitudeRadians
        let φ2 = location2.LatitudeRadians
        let λ2 = location2.LongitudeRadians
        let f = flatten a b
        let L = λ2 - λ1
        let tanU1 = reduceLatitude f φ1
        let cosU1 = toCos tanU1
        let sinU1 = toSin tanU1 cosU1
        let tanU2 = reduceLatitude f φ2
        let cosU2 = toCos tanU2
        let sinU2 = toSin tanU2 cosU2

        let mutable λ = L
        let mutable λʹ = λ
        let mutable loopCount = 0

        let rec getIntermediateResults() = 
            loopCount <- loopCount + 1
            let sinλ = sin λ
            let cosλ = cos λ
            let sinSqσ = (cosU2 * sinλ) * (cosU2 * sinλ) + (cosU1 * sinU2 - sinU1 * cosU2 * cosλ) * (cosU1 * sinU2 - sinU1 * cosU2 * cosλ)
            let sinσ = sqrt sinSqσ
            let cosσ = sinU1 * sinU2 + cosU1 * cosU2 * cosλ
            let σ = atan2 sinσ cosσ
            let sinα = cosU1 * cosU2 * sinλ / sinσ
            let cosSqα = 1.0 - (sinα ** 2.0)
            let cos2σM = cosσ - 2.0 * sinU1 * sinU2 / cosSqα
            let C = f / 16.0 * cosSqα * (4.0 + f * (4.0 - 3.0 * cosSqα))
            λʹ <- λ
            λ <- L + (1.0-C) * f * sinα * (σ + C*sinσ*(cos2σM+C*cosσ*(-1.0+2.0*cos2σM**2.0)))
            if (λ-λʹ |> abs > 0.000000000001 && loopCount < 200) then
                getIntermediateResults()
            else
                {
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
                }

        let calcs = getIntermediateResults()
                
        let uSq = calcs.cosSqα * (a*a - b*b) / (b*b)
        let A = 1.0 + uSq / 16384.0 * (4096.0 + uSq * (-768.0 + uSq * (320.0 - 175.0 * uSq)))
        let B = uSq / 1024.0 * (256.0 + uSq * (-128.0 + uSq * (74.0 - 47.0 * uSq)))
        let Δσ = B * calcs.sinσ * (calcs.cos2σM + B / 4.0 * (calcs.cosσ * (-1.0 + 2.0 * calcs.cos2σM * calcs.cos2σM) - B / 6.0 * calcs.cos2σM * (-3.0 + 4.0 * calcs.sinσ * calcs.sinσ) * (-3.0 + 4.0 * calcs.cos2σM * calcs.cos2σM)))

        b*A*(calcs.σ-Δσ)