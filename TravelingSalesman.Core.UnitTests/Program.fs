module TravelingSalesman.Core.UnitTests.Program

open System.Reflection
open NUnitLite

// F# doesn't have typeof(), and typedefof() does not work on modules.
// We get around this by declaring a junk interface,
// then finding the type that declared it (this module)
type internal Marker = interface end

[<EntryPoint>]
let main argv = 
    let program = typeof<Marker>.DeclaringType
    let typeInfo = program.GetTypeInfo()
    let runner = new AutoRun(typeInfo.Assembly)
    runner.Execute argv