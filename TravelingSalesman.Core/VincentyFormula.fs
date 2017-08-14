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

    let public GetDistance (majorAxis:float<'u>, minorAxis:float<'u>) (location1:IRadialCoordinate) (location2:IRadialCoordinate) =
        // F# translation of "Direct and Inverse Solutions of Geodesics
        // on the Ellipsoid with Application of Nested Functions" by
        // Thadeus Vincenty (1975).
        // https://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf
        let a = majorAxis
        let b = minorAxis
        let φ1 = location1.LatitudeRadians
        let φ2 = location2.LongitudeRadians
        let f = flatten a b
        let L = location2.LongitudeRadians - location1.LongitudeRadians
        let tanU1 = reduceLatitude f φ1
        let cosU1 = toCos tanU1
        let sinU1 = toSin tanU1 cosU1
        let tanU2 = reduceLatitude f φ2
        let cosU2 = toCos tanU2
        let sinU2 = toSin tanU2 cosU2
        L

