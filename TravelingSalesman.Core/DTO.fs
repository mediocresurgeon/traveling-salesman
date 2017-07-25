namespace TravelingSalesman.Core

open System

module DTO = 
    [<Measure>] type public degree
    [<Measure>] type public radian

    [<Interface>]
    type public ICoordinate = 
        abstract member LatitudeDegrees : float<degree>
        abstract member LatitudeRadians : float<radian>
        abstract member LongitudeDegrees : float<degree>
        abstract member LongitudeRadians : float<radian>

    let internal degreesToRadians (degrees:float<degree>) = Math.PI * degrees / 180.0<degree/radian>

    [<StructuredFormatDisplay("{Name}: {LatitudeDegrees}, {LongitudeDegrees")>]
    type public Location(name:string, latitude:float<degree>, longitude:float<degree>) =
        member this.Name = name
        member this.LatitudeDegrees = latitude
        member this.LatitudeRadians = degreesToRadians this.LatitudeDegrees
        member this.LongitudeDegrees = longitude
        member this.LongitudeRadians = degreesToRadians this.LongitudeDegrees
        interface ICoordinate with
            member this.LatitudeDegrees = this.LatitudeDegrees
            member this.LatitudeRadians = this.LatitudeRadians
            member this.LongitudeDegrees = this.LongitudeDegrees
            member this.LongitudeRadians = this.LongitudeRadians
    
    [<StructuredFormatDisplay("{Name}: {LatitudeDegrees}, {LongitudeDegrees @ {Timestamp}")>]
    type public Waypoint(name:string, latitude:float<degree>, longitude:float<degree>, timestamp:DateTime) = 
        inherit Location(name, latitude, longitude)
        member this.Timestamp = timestamp