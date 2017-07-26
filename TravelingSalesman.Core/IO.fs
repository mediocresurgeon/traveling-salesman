﻿namespace TravelingSalesman.Core

open System
open System.ComponentModel
open System.IO
open System.Runtime.Serialization
open System.Xml.Serialization
open TravelingSalesman.Core.DTO

module IO = 

// Some of these items are marked with [<EditorBrowsable(EditorBrowsableState.Never)>]
// This prevents them from showing up in Intellisense.
// These items should have been marked Private or Internal,
// but had to be marked Public in order for the XmlSerializer to do work on them.
// Hiding them from intellisense should make it clear that these items are not intended for public use.

    /// <summary>
    /// DTO used for deserializing GPX nodes.
    /// Not intended for public use.
    /// This object should not show up in Intellisense.
    /// </summary>
    [<CLIMutable>]
    [<DataContract>]
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type public WaypointInput = {
        [<XmlAttribute("lat")>]
        Latitude : float<degree>
        [<XmlAttribute("lon")>]
        Longitude : float<degree>
        [<XmlElement("name")>]
        Name : string
    }

    /// <summary>
    /// DTO used for serializing GPX nodes.
    /// Not intended for public use.
    /// This object should not show up in Intellisense.
    /// </summary>
    [<DataContract>]
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type public WaypointOutput = {
        [<XmlAttribute("lat")>]
        Latitude : float<degree>
        [<XmlAttribute("lon")>]
        Longitude : float<degree>
        [<XmlElement("name")>]
        Name : string
        [<XmlElement("time")>]
        Timestamp: DateTime
    }

    /// <summary>
    /// DTO used for deserializing GPX nodes.
    /// Not intended for public use.
    /// This object should not show up in Intellisense.
    /// </summary>
    [<CLIMutable>]
    [<DataContract>]
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    [<XmlRoot(ElementName = "gpx")>]
    type public GpxInput = {
        [<XmlElement("wpt")>] 
        Waypoints : WaypointInput array
    }

    /// <summary>
    /// DTO used for serializing GPX nodes.
    /// Not intended for public use.
    /// This object should not show up in Intellisense.
    /// </summary>
    [<DataContract>]
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    [<XmlRoot(ElementName = "gpx")>]
    type public GpxOutput = {
        [<XmlElement("wpt")>]
        Waypoints: WaypointOutput array
    }

    let private ConvertWaypointInputToLocation (waypoint:WaypointInput) =
        new Location(waypoint.Name, waypoint.Latitude, waypoint.Longitude)
    
    let private ConvertWaypointToWaypointOutput (waypoint:Waypoint) = 
        {
            Latitude = waypoint.LatitudeDegrees;
            Longitude = waypoint.LongitudeDegrees;
            Name = waypoint.Name;
            Timestamp = waypoint.Timestamp
        }
    
    /// <summary>
    /// Deserializes a Stream with GPX formatting into a collection of ordered Location objects.
    /// </summary>
    let public ReadLocationsFromGpxStream (gpxFile:Stream) =
        let xmlSerializer = new XmlSerializer(typedefof<GpxInput>)
        let dataContractResult = xmlSerializer.Deserialize(gpxFile) :?> GpxInput // http://stackoverflow.com/questions/31616761/f-casting-operators
        dataContractResult.Waypoints
            |> Array.toList
            |> List.map (fun wpt -> ConvertWaypointInputToLocation wpt)

    /// <summary>
    /// Serializes a collection of ordered Waypoint objects into a Stream with GPX formatting.
    /// </summary>
    let public WriteWaypointsToGpxStream (waypoints:List<Waypoint>, gpxFile:Stream) = 
        let xmlSerializer = new XmlSerializer(typedefof<GpxOutput>)
        let gpx = { Waypoints = waypoints |> List.map (fun wpt -> ConvertWaypointToWaypointOutput wpt) |> List.toArray }
        xmlSerializer.Serialize(gpxFile, gpx)