namespace TravelingSalesman.Core.Geodesics

open TravelingSalesman.Core.DTO

/// <summary>
/// An algorithm for determining the distance between two points on an ellipsoid.
/// This can have an error as small as +/- 1mm on Earth.
/// Discovered by Thadeus Vincenty in 1975.
/// </summary>
module VincentyFormula = 
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

        let eq1 (sinλ:float) (cosλ:float) =
            (cosU2 * sinλ) * (cosU2 * sinλ) + (cosU1 * sinU2 - sinU1 * cosU2 * cosλ) * (cosU1 * sinU2 - sinU1 * cosU2 * cosλ)

        let eq2 (cosλ:float) = 
            sinU1 * sinU2 + cosU1 * cosU2 * cosλ

        let eq3 (sinλ:float) (sinσ:float) = 
            cosU1 * cosU2 * sinλ / sinσ

        let eq4 (sinα:float) = 
            1.0 - (sinα ** 2.0)

        let eq5 (cosσ:float) (cosSqα:float) = 
            cosσ - 2.0 * sinU1 * sinU2 / cosSqα

        let eq6 (cosSqα:float) = 
            f / 16.0 * cosSqα * (4.0 + f * (4.0 - 3.0 * cosSqα))

        let mutable λ = L
        let mutable λʹ = λ
        let mutable sinλ = sin λ
        let mutable cosλ = cos λ
        let mutable sinSqσ = eq1 sinλ cosλ
        let mutable sinσ = sqrt sinSqσ
        let mutable cosσ = eq2 cosλ
        let mutable σ = atan2 sinσ cosσ
        let mutable sinα = eq3 sinλ sinσ
        let mutable cosSqα = eq4 sinα
        let mutable cos2σM = eq5 cosσ cosSqα
        let mutable C =  eq6 cosSqα

        let looper = 
            sinλ <- sin λ
            cosλ <- cos λ
            sinSqσ <- eq1 sinλ cosλ
            sinσ <- sqrt sinSqσ
            cosσ <- eq2 cosλ
            σ <- atan2 sinσ cosσ
            sinα <- eq3 sinλ sinσ
            cosSqα <- eq4 sinα
            cos2σM <- eq5 cosσ cosSqα
            C <-  eq6 cosSqα
            λʹ <- λ
            λ <- L + (1.0-C) * f * sinα * (σ + C*sinσ*(cos2σM+C*cosσ*(-1.0+2.0*cos2σM**2.0)))

        looper
        let mutable loopCount = 0
        while (λ-λʹ |> abs > 0.000000000001 && loopCount < 200) do
            looper
            loopCount <- loopCount + 1
        
        let uSq = cosSqα * (a*a - b*b) / (b*b)
        let A = 1.0 + uSq/16384.0*(4096.0+uSq*(-768.0+uSq*(320.0-175.0*uSq)))
        let B = uSq/1024.0 * (256.0+uSq*(-128.0+uSq*(74.0-47.0*uSq)))
        let Δσ = B*sinσ*(cos2σM+B/4.0*(cosσ*(-1.0+2.0*cos2σM*cos2σM)-B/6.0*cos2σM*(-3.0+4.0*sinσ*sinσ)*(-3.0+4.0*cos2σM*cos2σM)))

        b*A*(σ-Δσ)