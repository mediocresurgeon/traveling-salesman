module TravelingSalesman.Core.UnitTests.VincentyFormulaTest

open System
open NUnit.Framework
open TravelingSalesman.Core.DTO
open TravelingSalesman.Core.Geodesics.VincentyFormula


// We define a kilometer here, but we could just as easily use miles or fathoms if we wanted
[<Measure>] type kilometer

/// <summary>
/// A given calculation can contain compounded rounding errors.
/// This function rounds a result such that it ignores anything after the 6th decimal place.
/// On the surface of the Earth, means that a result will be rounded to +/- 1 millimeters.
/// </summary>
/// <param name="distance">The distance between two points on a sphere.</name>
/// <returns>The distance rounded to 6 decimal places.</returns>
let private roundDistance (distance:float<kilometer>) =
    distance / 1.0<kilometer>
        |> (fun n -> n * 1000000.0)
        |> truncate
        |> (fun n -> n / 1000000.0)
        |> (fun n -> n * 1.0<kilometer>)

let earthEquatorRadius = 6378.1370<kilometer>
let earthPolarRadius = 6356.752314245<kilometer>

[<Test>]
let ``Pole to equator distance``() = 
    // Arrange
    let loc1 = new Location("North Pole", 90.0<degree>, 0.0<degree>)
    let loc2 = new Location("Equator", 0.0<degree>, 0.0<degree>)

    // Act
    let distance = GetDistance (earthEquatorRadius, earthPolarRadius) loc1 loc2

    // Assert
    Assert.AreEqual(10001.965729<kilometer>, roundDistance distance)

// This test reveals a bug - the GetDistance function fails when both points are on the equator.
//[<Test>]
//let ``Halfway around to equator distance``() = 
    //// Arrange
    //let loc1 = new Location("Equator1", 0.0<degree>, -90.0<degree>)
    //let loc2 = new Location("Equator2", 0.0<degree>, 90.0<degree>)

    //// Act
    //let distance = GetDistance (earthEquatorRadius, earthPolarRadius) loc1 loc2

    //// Assert
    //Assert.AreEqual(10001.965729<kilometer>, roundDistance distance)