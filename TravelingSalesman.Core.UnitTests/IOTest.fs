module TravelingSalesman.Core.UnitTests.IOTest

open System.Reflection
open NUnit.Framework
open TravelingSalesman.Core.DTO
open TravelingSalesman.Core.IO

// https://stackoverflow.com/questions/1175056/value-of-the-last-element-of-a-list#answer-1175123
let rec last = function
    | hd :: [] -> hd
    | hd :: tl -> last tl
    | _ -> failwith "Empty list."

[<Test>]
let ``Deserialize Example.gpx``() = 
    // Arrange
    let assembly = Assembly.GetEntryAssembly()
    use stream = assembly.GetManifestResourceStream("Example.gpx")
    // Example.gpx is an embedded resource

    // Act
    let coordinates = ReadLocationsFromGpxStream stream

    // Assert
    Assert.IsNotEmpty(coordinates)

    let firstCoordinate = coordinates.Head
    Assert.AreEqual("Hagatna Pillbox", firstCoordinate.Name)
    Assert.AreEqual(13.47852, firstCoordinate.LatitudeDegrees)
    Assert.AreEqual(144.7516, firstCoordinate.LongitudeDegrees)

    let finalCoordinate = last coordinates
    Assert.AreEqual("Chief Quipuha Statue", finalCoordinate.Name)
    Assert.AreEqual(13.47725, finalCoordinate.LatitudeDegrees)
    Assert.AreEqual(144.7538, finalCoordinate.LongitudeDegrees)