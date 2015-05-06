module Data

open FSharp.Data

let private STATES_FILENAME = System.Environment.GetCommandLineArgs().[1]
let private EDGES_FILENAME = System.Environment.GetCommandLineArgs().[2]

let NUM_GENES = (CsvFile.Load(STATES_FILENAME).Headers |> Option.get |> Seq.length) - 1
let NUM_STATES = CsvFile.Load(STATES_FILENAME).Rows |> Seq.length

type Gene = string

[<CustomEquality; CustomComparison>]
type State =
  { Name : string;
    Values : Map<Gene, bool> }

    override x.Equals(yobj) =
        match yobj with
        | :? State as y -> (x.Name = y.Name)
        | _ -> invalidArg "yobj" "cannot test for equality of a State with a non-State type"

    override x.GetHashCode() = hash x.Name
    interface System.IComparable with
      member x.CompareTo yobj =
          match yobj with
          | :? State as y -> compare x.Name y.Name
          | _ -> invalidArg "yobj" "cannot compare a State with a non-State type"

let readLines filePath = System.IO.File.ReadAllLines(filePath)
