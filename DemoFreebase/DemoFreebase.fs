module DemoFreebase

#if INTERACTIVE
    #r @"..\packages\FSharp.Data.1.1.5\lib\net40\FSharp.Data.dll"
#endif 

open System
open FSharp.Data

// Put your Google API key here:
// https://code.google.com/apis/console/
type FreeBaseKeyed = FSharp.Data.FreebaseDataProvider<Key=""> 

let dc = FreeBaseKeyed.GetDataContext()

let NullableToStr (fl : Nullable<'T>) =
    if fl.HasValue then sprintf "%A" fl.Value
    else ""

let ListAFewAirports() =
    dc.Transportation.Aviation.Airports
    |> Seq.take 100
    |> Seq.iter (fun airport -> printfn "%A" airport)

let ListAFewAirportLocations() =
    dc.Transportation.Aviation.Airports
    |> Seq.take 100
    |> Seq.iter (fun airport -> printfn "%s %s %s" airport.Name (NullableToStr(airport.Geolocation.Latitude)) (NullableToStr(airport.Geolocation.Longitude)))

type AirportPos = { Name: string; Lat: float; Long: float }

/// Gets airports between a specified start and end index (to facilitate paged access).
let GetAirportsPaged startIndex pageSize = 
    query {
        for airport in dc.Transportation.Aviation.Airports do
        where (    airport.Geolocation <> null
                && airport.Geolocation.Latitude.HasValue
                && airport.Geolocation.Longitude.HasValue
              )
        skip startIndex
        take pageSize
        select (airport.Name, 
                airport.Geolocation.Latitude.Value, 
                airport.Geolocation.Longitude.Value)
    }
    |> Array.ofSeq
    |> Array.map (fun (name, lat, long) -> { Name = name; Lat = lat; Long = long })

/// Gets all airports from Freebase which have a defined location.
let GetAllAirports pageSize =
    let rec getPage startIndex acc =
        let page = GetAirportsPaged startIndex pageSize
        if page.Length > 0 then
            Array.append acc (getPage (startIndex+pageSize) page)
        else
            acc

    getPage 0 [||]

/// Calculates the straight-line distance between two Latitude/Longitude positions.
let DistanceBetween lat1 long1 lat2 long2 =
    let toRadians deg = deg * Math.PI / 180.
    let lat1r = lat1 |> toRadians
    let lat2r = lat2 |> toRadians
    let earthRadius = 6371. // km
    let deltaLat = (lat2 - lat1) |> toRadians
    let deltaLong = (long2 - long1) |> toRadians

    let a = Math.Sin(deltaLat/2.) ** 2. +
            (Math.Sin(deltaLong/2.) ** 2. * Math.Cos(lat1r) * Math.Cos(lat2r))

    let c = 2. * Math.Atan2(Math.Sqrt(a), Math.Sqrt(1.-a))

    earthRadius * c

/// Gets the closest n airports to the given airport.
let GetClosest target count airportList =
    airportList
    |> Array.map (fun airport -> airport, (DistanceBetween (airport.Lat) (airport.Long) (target.Lat) (target.Long)))
    |> Array.sortBy (fun (airport, distance) -> distance)
    |> Seq.truncate count
    |> Array.ofSeq

let airports = GetAllAirports 2000

/// Gets airports near an airport specified by name.
let GetAirportsNear name airportList =

    let target = airportList 
                 |> Array.tryFind (fun airport -> airport.Name.Contains(name))

    if target.IsSome then
        airportList
        |> GetClosest target.Value 20
        |> Array.iter (fun (airport, distance) -> printfn "%s - %f km" airport.Name distance)

    else
        printfn "Could not find %s" name