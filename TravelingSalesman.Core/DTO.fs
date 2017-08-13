namespace TravelingSalesman.Core

open System

module DTO = 
    /// <summary>
    /// Measures the size of an angle.
    /// In general, degrees are no smaller than 0 and no greater than 360.
    /// </summary>
    [<Measure>] type public degree
    
    /// <summary>
    /// An object which defines a location on the surface of a sphere in degrees.
    /// </summary>
    [<Interface>]
    type public IDecimalCoordinate = 
        /// <summary>
        /// Returns the latitude in degrees.
        /// </summary>
        abstract member LatitudeDegrees : float<degree>
        /// <summary>
        /// Returns the longitude in degrees.
        /// </summary>
        abstract member LongitudeDegrees : float<degree>

    /// <summary>
    /// An object which defines a location on the surface of a sphere in radians.
    /// </summary>
    [<Interface>]
    type public IRadialCoordinate = 
        /// <summary>
        /// Returns the latitude in radians.
        /// </summary>
        abstract member LatitudeRadians : float
        /// <summary>
        /// Returns the longitude in radians.
        /// </summary>
        abstract member LongitudeRadians : float

    /// <summary>
    /// Converts degrees to radians.
    /// </summary>
    let internal degreesToRadians (degrees:float<degree>) = Math.PI / 180.0<degree> * degrees

    /// <summary>
    /// Reduces a degrees mesurement to its simplest form.
    /// </summary>
    let internal simplifyDegrees (degrees:float<degree>) = degrees % 360.0<degree>

    /// <summary>
    /// An object which specifies a named location on the surface of a sphere.
    /// </summary>
    [<StructuredFormatDisplay("{Name}: {LatitudeDegrees}, {LongitudeDegrees")>]
    type public Location(name:string, latitude:float<degree>, longitude:float<degree>) =
        member this.Name = name
        member this.LatitudeDegrees = simplifyDegrees latitude
        member this.LatitudeRadians = degreesToRadians this.LatitudeDegrees
        member this.LongitudeDegrees = simplifyDegrees longitude
        member this.LongitudeRadians = degreesToRadians this.LongitudeDegrees
        interface IDecimalCoordinate with
            member this.LatitudeDegrees = this.LatitudeDegrees
            member this.LongitudeDegrees = this.LongitudeDegrees
        interface IRadialCoordinate with
            member this.LongitudeRadians = this.LongitudeRadians
            member this.LatitudeRadians = this.LatitudeRadians
    
    /// <summary>
    /// An object which specifies a named location on the surface of a sphere at a specific point in time.
    /// </summary>
    [<StructuredFormatDisplay("{Name}: {LatitudeDegrees}, {LongitudeDegrees} @ {Timestamp}")>]
    type public Waypoint(name:string, latitude:float<degree>, longitude:float<degree>, timestamp:DateTime) = 
        inherit Location(name, latitude, longitude)
        member this.Timestamp = timestamp