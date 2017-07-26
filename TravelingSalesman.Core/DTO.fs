namespace TravelingSalesman.Core

open System

module DTO = 
    /// <summary>
    /// Measures the size of an angle.
    /// In general, degrees are no smaller than 0 and no greater than 360.
    /// Degrees can be converted into radians.
    /// </summary>
    [<Measure>] type public degree

    /// <summary>
    /// Measures the size of an angle.
    /// In general, radians are no smaller than 0 and no greater than 2π.
    /// Radians can be converted into degrees.
    /// </summary>
    [<Measure>] type public radian

    /// <summary>
    /// An object which defines a location on the surface of a sphere.
    /// </summary>
    [<Interface>]
    type public ICoordinate = 
        abstract member LatitudeDegrees : float<degree>
        abstract member LatitudeRadians : float<radian>
        abstract member LongitudeDegrees : float<degree>
        abstract member LongitudeRadians : float<radian>

    /// <summary>
    /// Converts degrees to radians.
    /// </summary>
    let internal degreesToRadians (degrees:float<degree>) = Math.PI * degrees / 180.0<degree/radian>

    /// <summary>
    /// An object which specifies a named location on the surface of a sphere.
    /// </summary>
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
    
    /// <summary>
    /// An object which specifies a named location on the surface of a sphere at a specific point in time.
    /// </summary>
    [<StructuredFormatDisplay("{Name}: {LatitudeDegrees}, {LongitudeDegrees} @ {Timestamp}")>]
    type public Waypoint(name:string, latitude:float<degree>, longitude:float<degree>, timestamp:DateTime) = 
        inherit Location(name, latitude, longitude)
        member this.Timestamp = timestamp