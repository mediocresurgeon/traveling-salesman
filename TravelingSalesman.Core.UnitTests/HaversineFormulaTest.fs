module TravelingSalesman.Core.UnitTests.HaversineTest


open System
open NUnit.Framework
open TravelingSalesman.Core.Common
open TravelingSalesman.Core.Geodesics.HaversineFormula


// We define a kilometer here, but we could just as easily use miles or fathoms if we wanted
[<Measure>] type kilometer


// The size of Earth
let earthRadius = 6371.0<kilometer>


/// <summary>
/// A given calculation can contain compounded rounding errors.
/// This function rounds a result such that it ignores anything after the 10th decimal place.
/// On the surface of the Earth, means that a result will be rounded to +/- 2 millimeters.
/// </summary>
/// <param name="angle">The central angle between two points on a sphere (in radians).</name>
/// <returns>The angle rounded to 10 decimal places.</returns>
let private roundAngle (angle:float) = 
    angle * 10000000000.0
        |> truncate
        |> (fun n -> n / 10000000000.0)


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


[<Test>]
let ``Pole to pole angle``() = 
    // Arrange
    let loc1 = ((Math.PI / 2.0), 0.0)  // North pole
    let loc2 = (-(Math.PI / 2.0), 0.0) // South pole

    // Act
    let result = GetAngle loc1 loc2

    // Assert
    Assert.AreEqual(Math.PI, result)


[<Test>]
let ``Pole to pole distance``() = 
    // Arrange
    let loc1 = new Location("North Pole", 90.0<degree>, 0.0<degree>)
    let loc2 = new Location("South Pole", -90.0<degree>, 0.0<degree>)

    // Act
    let result = GetDistance earthRadius loc1 loc2 |> roundDistance

    // Assert
    let expectedResult = earthRadius * Math.PI |> roundDistance
    Assert.AreEqual(expectedResult, result)


[<Test>]
let ``Pole to equator angle``() = 
    // Arrange
    let loc1 = ((Math.PI / 2.0), 0.0)   // North pole
    let loc2 = (0.0, 0.0)               // Equator

    // Act
    let result = GetAngle loc1 loc2 |> roundAngle

    // Assert
    let expectedResult = Math.PI / 2.0 |> roundAngle
    Assert.AreEqual(expectedResult, result)


[<Test>]
let ``Pole to equator distance``() = 
    // Arrange
    let loc1 = new Location("North Pole", 90.0<degree>, 0.0<degree>)
    let loc2 = new Location("Equator", 0.0<degree>, 0.0<degree>)

    // Act
    let result = GetDistance earthRadius loc1 loc2 |> roundDistance

    // Assert
    let expectedResult = earthRadius * Math.PI / 2.0 |> roundDistance
    Assert.AreEqual(expectedResult, result)