module TravelingSalesman.Core.UnitTests.CommonTest

open System
open NUnit.Framework
open TravelingSalesman.Core.Common


[<Test>]
let ``DegreesToRadians 0``() = 
    // Arrange
    let testValue = 0.0<degree>

    // Act
    let result = DegreesToRadians testValue

    // Assert
    let expectedValue = 0.0
    Assert.AreEqual(expectedValue, result)


[<Test>]
let ``DegreesToRadians 90``() = 
    // Arrange
    let testValue = 90.0<degree>

    // Act
    let result = DegreesToRadians testValue

    // Assert
    let expectedValue = 0.5 * Math.PI
    Assert.AreEqual(expectedValue, result)


[<Test>]
let ``DegreesToRadians 180``() = 
    // Arrange
    let testValue = 180.0<degree>

    // Act
    let result = DegreesToRadians testValue

    // Assert
    let expectedValue = 1.0 * Math.PI
    Assert.AreEqual(expectedValue, result)


[<Test>]
let ``DegreesToRadians 270``() = 
    // Arrange
    let testValue = 270.0<degree>

    // Act
    let result = DegreesToRadians testValue

    // Assert
    let expectedValue = 1.5 * Math.PI
    Assert.AreEqual(expectedValue, result)


[<Test>]
let ``DegreesToRadians 360``() = 
    // Arrange
    let testValue = 360.0<degree>

    // Act
    let result = DegreesToRadians testValue

    // Assert
    let expectedValue = 2.0 * Math.PI
    Assert.AreEqual(expectedValue, result)


[<Test>]
let ``SimplifyDegrees 0``() = 
    // Arrange
    let testValue = 0.0<degree>

    // Act
    let result = SimplifyDegrees testValue

    // Assert
    let expectedValue = 0.0<degree>
    Assert.AreEqual(expectedValue, result)


[<Test>]
let ``SimplifyDegrees 180``() = 
    // Arrange
    let testValue = 180.0<degree>

    // Act
    let result = SimplifyDegrees testValue

    // Assert
    let expectedValue = 180.0<degree>
    Assert.AreEqual(expectedValue, result)


[<Test>]
let ``SimplifyDegrees 360``() = 
    // Arrange
    let testValue = 360.0<degree>

    // Act
    let result = SimplifyDegrees testValue

    // Assert
    let expectedValue = 0.0<degree>
    Assert.AreEqual(expectedValue, result)


[<Test>]
let ``SimplifyDegrees 361``() = 
    // Arrange
    let testValue = 361.0<degree>

    // Act
    let result = SimplifyDegrees testValue

    // Assert
    let expectedValue = 1.0<degree>
    Assert.AreEqual(expectedValue, result)


[<Test>]
let ``SimplifyDegrees -90``() = 
    // Arrange
    let testValue = -90.0<degree>

    // Act
    let result = SimplifyDegrees testValue

    // Assert
    let expectedValue = 270.0<degree>
    Assert.AreEqual(expectedValue, result)


[<Test>]
let ``SimplifyDegrees -180``() = 
    // Arrange
    let testValue = -180.0<degree>

    // Act
    let result = SimplifyDegrees testValue

    // Assert
    let expectedValue = 180.0<degree>
    Assert.AreEqual(expectedValue, result)