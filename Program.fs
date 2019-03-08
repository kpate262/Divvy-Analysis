
//
// F# program to analyze Divvy daily ride data.
//
// << YOUR NAME HERE >>
// U. of Illinois, Chicago
// CS 341, Spring 2019
// Project #04
//

#light

module project04

//
// ParseLine and ParseInput
//
// Given a sequence of strings representing Divvy data, 
// parses the strings and returns a list of lists.  Each
// sub-list denotes one bike ride.  Example:
//
//   [ [176;74;1252;21;595;1986;1]; ... ]
//
// The values are station id (from), station id (to), bike
// id, starting hour (0..23), trip duration (secs), birth
// year (0=>not specified), and gender (0=>not specified, 
// 1=>identifies as male, 2=>identifies as female).
//


let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints


let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides


let rec printstars n = 
    match n with
    | 0 -> 0
    | 1 -> printf "*"
           printstars (n-1)
    | _ -> printf "*"
           printstars (n-1)



let rec eachRider list counter gender age z sec hour = 
    match list with
    | [] -> 0
    | e::tail when (counter = 6 && e = gender && z = -1 && hour = -1) ->  1
    | e::tail when (counter = 5 && age > 0 && e > 0 && z = -1 && hour = -1) ->  (System.DateTime.Now.Year - e) 
    | e::tail when (counter = 5 && age > 0 && e = 0 && z = 1 && hour = -1) -> 1
    | e::tail when (counter = 4 && sec = 30 && (e >= 0 && e <= (30*60)) && hour = -1) -> 1
    | e::tail when (counter = 4 && sec = 60 && (e > (30*60) && e <= (60*60) )&& hour = -1) -> 1
    | e::tail when (counter = 4 && sec = 120 && (e > (60*60) && e <= (120*60))) -> 1
    | e::tail when (counter = 4 && sec = 180 && (e > (120*60) && e <= (24*60*60*60))&& hour = -1) -> 1
    | e::tail when (counter = 3 && e = hour) -> 1
    | e::tail -> eachRider tail (counter+1) gender age z sec hour



let rec outterList list gender age z sec hour =
    match list with
    | [] -> 0
    | e::tail -> (eachRider e 0 gender age z sec hour) + outterList tail gender age z sec hour



let rec hourlyRides list i =
    if i = 24 then 0
    else let counter = outterList list -1 -1 -1 -1 i
         printf "%A: " i
         printstars (counter/10)
         printfn "%A" counter
         hourlyRides list (i+1)
         
    
[<EntryPoint>]
let main argv =
  //
  // input file name, then input divvy ride data and build
  // a list of lists:
  //
  printf "filename> "
  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents
  let numOfMales = outterList ridedata 1 -1 -1 -1 -1
  let numOfFemales = outterList ridedata 2 -1 -1 -1 -1
  let totalge = outterList ridedata -1 1 -1 -1 -1
  let zeros = outterList ridedata -1 1 1 -1 -1
  let halfHourRides = outterList ridedata -1 -1 -1 30 -1
  let halfToHourRides = outterList ridedata -1 -1 -1 60 -1
  let hourToTwoRides = outterList ridedata -1 -1 -1 120 -1
  let moreThenTwoHourRides = outterList ridedata -1 -1 -1 180 -1
  
  
  //printfn "%A" ridedata
  let N = List.length ridedata
  printfn ""
  printfn "# of riders: %A" N
  printfn ""
  printfn "%% of riders indentifying as male: %A (%A%%)" numOfMales ((float (numOfMales*100))/float N)
  printfn "%% of riders indentifying as female: %A (%A%%)" numOfFemales ((float (numOfFemales*100))/float N)
  printfn ""
  printfn "Average age: %A" ( (float (totalge) )/float (N-zeros))
  printfn "** Ride Durations:" 
  printfn " 0..30 mins: %A (%A%%)" halfHourRides ((float (halfHourRides*100))/float N)
  printfn " 30..60 mins: %A (%A%%)" halfToHourRides ((float (halfToHourRides*100))/float N)
  printfn " 60..120 mins: %A (%A%%)" hourToTwoRides ((float (hourToTwoRides*100))/float N)
  printfn " > 2 hours: %A (%A%%)" moreThenTwoHourRides ((float (moreThenTwoHourRides*100))/float N)
  printfn ""
  printfn "** Ride Start Time Histogram:"
  hourlyRides ridedata 0
  0 
