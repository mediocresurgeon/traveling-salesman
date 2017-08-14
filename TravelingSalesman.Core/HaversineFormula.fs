namespace TravelingSalesman.Core.Geodesics

open TravelingSalesman.Core.DTO

/// <summary>
/// An algorithm for determining the great circle distance between two points on a sphere.
/// This can have an error as large as 0.3% (22km) on Earth, since Earth is not a sphere.
/// Discovered by Don Josef de Mendoza y Rios in 1796.
/// </summary>
module HaversineFormula = 
    /// <summary>
    /// A little-used trigonometric function.
    /// </summary>
    /// <remarks>"Haversine" is short for "half a versine".</remarks>
    /// <param name="θ">The angle (in radians).</param>
    /// <returns>The haversine of the angle (in radians).</returns>
    let private hav (θ:float) =
        // we avoid the 1 - cos θ definition as
        // it creates significant rounding errors
        // when cos θ is very close to 0
        
        // hav(θ) = sin^2(θ/2)
        // http://mathworld.wolfram.com/Haversine.html
        θ / 2.0                     // Divide the angle by two
            |> sin                  // Get the sine of the angle
            |> (fun n -> n ** 2.0)  // Return the square of the sine

                
    /// <summary>
    /// Returns the angle between two points on a sphere.
    /// </summary>
    /// <param name="φ1">The latitude of the first coordinate (in radians).</param>
    /// <param name="λ1">The longitude of the first coordinate (in radians).</param>
    /// <param name="φ2">The latitude of the second coordinate (in radians).</param>
    /// <param name="λ2">The longitude of the second coordinate (in radians).</param>
    /// <returns>The angle between the two location (in radians).</returns>
    let public GetAngle (φ1:float, λ1:float) (φ2:float, λ2:float) = 
        let havΔLat = φ2 - φ1 |> hav
        let cosφ1 = cos φ1
        let cosφ2 = cos φ2
        let havΔLon = λ2 - λ1 |> hav

        // Pure formula:
        // distance = 2 radius arcsin( sqrt( hav(φ2 - φ1) + cos(φ1) * cos(φ2) * hav(λ2 - λ1) ) )
        // https://en.wikipedia.org/wiki/Haversine_formula#The_haversine_formula

        //(cosφ1 * cosφ2 * havΔLong) + havΔLat                        // determine radicand
            //|> sqrt                                                 // determine ratio
            //|> asin                                                 // determine principal value
            //|> (fun principalValue -> 2.0 * principalValue)         // determine angle

        // Computers a better at calculating small distances if atan2 is used instead of asin sqrt
        // This requires intermediate steps and cannot be written as cleanly.
        // http://mathforum.org/library/drmath/view/51879.html

        let a = havΔLat + (cosφ1 * cosφ2 * havΔLon)    // Determine intermediate result
        let y = sqrt a                                  // Determine y component of atan2 function
        let x = 1.0 - a |> sqrt                         // Determine x component of atan2 function
        2.0 * atan2 y x


    /// <summary>
    /// Calculates the great circle distance between two points on the surface of a sphere.
    /// <summary>
    /// <param name="location1">The first location on the surface of a sphere.</param>
    /// <param name="location2">The second location on the surface of a sphere.</param>
    /// <returns>The great circle distance between the two locations.</returns>
    let GetDistance (sphereRadius:float<_>) (location1:IRadialCoordinate) (location2:IRadialCoordinate) = 
        let toTuple (loc:IRadialCoordinate) =                       // Define how to convert a IRadialCoordinate into a tuple
            (loc.LatitudeRadians, loc.LongitudeRadians)
        let θ = GetAngle <| toTuple location1 <| toTuple location2  // Convert the two locations into tuples and feed them to GetAngle
        sphereRadius * θ                                            // multiple the angle by the radius and return the result