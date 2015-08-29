#r "FSharp.Data.dll"
#r "FSharpx.Core.dll"

open System.Collections
open System.Collections.Generic
open FSharp.Data
open FSharpx.Collections

if Array.length fsi.CommandLineArgs <> 6 then
    failwith "Incorrect number of arguments.\
              Correct format: fsi.exe --exec constructSTG.fsx input.csv 25 outputStates.csv outputEdges.csv output.sif"

let loadCsv (filename : string) =
    let csv = CsvFile.Load(filename)
    if csv.NumberOfColumns - 1 > 64 then
        failwith "constructSTG.fsx does not support datasets with more than 64 genes"

    let parseRow (r : CsvRow) =
        (r.GetColumn 0,  [| for i in 1 .. csv.NumberOfColumns - 1 do
                                yield System.Double.Parse (r.GetColumn i) |])
    
    let header = (Option.get csv.Headers).[1..]
    let rows = Seq.map parseRow csv.Rows
    (header, rows)

let inputCsvFilename = fsi.CommandLineArgs.[1]
let threshold = fsi.CommandLineArgs.[2] |> System.Double.Parse
let outputStatesCsvFilename = fsi.CommandLineArgs.[3]
let outputEdgesCsvFilename = fsi.CommandLineArgs.[4]
let outputSifFilename = fsi.CommandLineArgs.[5]

let discretise rows =
    let discretise x = x < threshold
    Seq.map (fun (id, a) -> (id, Array.map discretise a)) rows

let boolArrayToUint32 (a : bool []) =
    let a = BitArray (Array.rev a)
    let u = [| 0 |]
    a.CopyTo(u, 0)
    uint32 (u.[0])

let boolArrayToUint64 a =
    if Array.length a <= 32 then
        uint64 (boolArrayToUint32 a)
    else
        let high = boolArrayToUint32 a.[.. 31]
        let low = boolArrayToUint32 a.[32 ..]
        (uint64 high <<< (Array.length a.[32 ..])) ||| uint64 low

let toUniqueBitvectors rows =
    let seen = HashSet<uint64>()
    Map.ofList [ for (id : string, array) in rows do
                     let bitVec = boolArrayToUint64 array
                     if not <| seen.Contains bitVec then
                         yield (bitVec, id) ]

let genes, rows = loadCsv inputCsvFilename
let states = discretise rows
let map = toUniqueBitvectors states
let numGenes = Array.length genes

let allOnes = System.UInt64.MaxValue >>> (64 - numGenes)

let possibleOneNeighbours state =
    let flipRightmost0 i = i ||| i + 1UL
    let flip = ref state
    seq { while !flip <> allOnes do
              let newFlip = flipRightmost0 !flip
              yield (newFlip ^^^ !flip) ||| state
              flip := newFlip }

let geneChange (state : uint64) state' =
    let index = numGenes - (int <| System.Math.Log(float <| state ^^^ state', 2.0)) - 1
    genes.[index]

let uniqueStates =
    let uniqueIds = Set.ofSeq <| Map.values map
    seq { for (id, a) in states do
           if Set.contains id uniqueIds then
             yield (id, a) }

let edges =
    seq { for state in Map.keys map do
              for state' in possibleOneNeighbours state do
                  if Map.containsKey state' map then
                      yield (Map.find state map, Map.find state' map, geneChange state state') }
    
let writeBoolArray a =
    Array.map (sprintf ",%b") a |> Array.reduce (+)

System.IO.File.WriteAllLines(outputStatesCsvFilename, let header = genes |> Array.map (sprintf ",%s") |> Array.reduce (+)
                                                      in let rows = uniqueStates |> Seq.map (fun (id, a) -> sprintf "%s%s" id (writeBoolArray a))
                                                         in Seq.append [header] rows)
System.IO.File.WriteAllLines(outputEdgesCsvFilename, let header = "Gene,StateA,StateB"
                                                      in let rows = edges |> Seq.map (fun (x, y, g) -> sprintf "%s,%s,%s" g x y)
                                                         in Seq.append [header] rows)
System.IO.File.WriteAllLines(outputSifFilename, edges |> Seq.map (fun (x, y, _) -> sprintf "%s -> %s" x y))
