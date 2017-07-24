module TravelingSalesman.Core.UnitTests.MyTest

open NUnit.Framework
open TravelingSalesman.Core

[<Test>]
let ``Swap two adjacent elements``() = 
    Assert.AreEqual(Say.getNumber, 1)