module TravelingSalesman.Core.UnitTests.Program

// Learn more about F# at http://fsharp.org
open Reflection
open System
open NUnitLite

[<EntryPoint>]
let main argv = 
      let typeInfo = typedefof<Program>
      let info = typeInfo.GetTypeInfo();
      let ar = AutoRun(typeInfo.Assembly)
      ar.Execute(args);
    printfn "Hello World from F#!"
    0 // return an integer exit code
