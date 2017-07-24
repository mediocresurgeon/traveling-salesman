module TravelingSalesman.Core.UnitTests.SimpleTest

open NUnit.Framework
open TravelingSalesman.Core

[<Test>]
let ``sample test``() = 
    let result = Say.getNumber
    Assert.AreEqual(result, 1)