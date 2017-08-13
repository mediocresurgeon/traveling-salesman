module TravelingSalesman.Core.UnitTests.DTOTest

open System
open NUnit.Framework
open TravelingSalesman.Core.DTO


[<Test>]
let ``degreesToRadians 0``() = 
    // Arrange
    let testValue = 0.0<degree>

    // Act
    let result = degreesToRadians testValue

    // Assert
    let expectedValue = 0.0
    Assert.AreEqual(expectedValue, result)


[<Test>]
let ``degreesToRadians 90``() = 
    // Arrange
    let testValue = 90.0<degree>

    // Act
    let result = degreesToRadians testValue

    // Assert
    let expectedValue = 0.5 * Math.PI
    Assert.AreEqual(expectedValue, result)


[<Test>]
let ``degreesToRadians 180``() = 
    // Arrange
    let testValue = 180.0<degree>

    // Act
    let result = degreesToRadians testValue

    // Assert
    let expectedValue = 1.0 * Math.PI
    Assert.AreEqual(expectedValue, result)


[<Test>]
let ``degreesToRadians 270``() = 
    // Arrange
    let testValue = 270.0<degree>

    // Act
    let result = degreesToRadians testValue

    // Assert
    let expectedValue = 1.5 * Math.PI
    Assert.AreEqual(expectedValue, result)


[<Test>]
let ``degreesToRadians 360``() = 
    // Arrange
    let testValue = 360.0<degree>

    // Act
    let result = degreesToRadians testValue

    // Assert
    let expectedValue = 2.0 * Math.PI
    Assert.AreEqual(expectedValue, result)