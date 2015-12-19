module Program

open Data
open FSharp.Data
open FSharpx.Collections

let synth (statesFilename : string) (edgesFilename : string) (parametersFilename : string) initialStatesFilename targetStatesFilename outputDir
          oneSolution =
    let geneParameters = CsvFile.Load(parametersFilename).Rows
                       |> Seq.map (fun (row : CsvRow) -> row.GetColumn 0, (System.Int32.Parse <| row.GetColumn 1,
                                                                           System.Int32.Parse <| row.GetColumn 2,
                                                                           System.Int32.Parse <| row.GetColumn 3))
                       |> Map.ofSeq

    let statesCsv = CsvFile.Load(statesFilename)
    let edgesCsv = CsvFile.Load(edgesFilename)
    let geneNames = statesCsv.Headers |> Option.get |> Seq.skip 1 |> Array.ofSeq

    let states = statesCsv.Rows
              |> Seq.map (fun (row : CsvRow) -> let name = row.GetColumn 0
                                                let values = Array.map (System.Boolean.Parse) row.Columns.[1..]
                                                (name, { Name = name
                                                         Values = Array.zip geneNames values |> Map.ofArray }))
              |> Map.ofSeq

    let edges = edgesCsv.Rows
             |> Seq.map (fun (row : CsvRow) -> let a, b = row.GetColumn 1, row.GetColumn 2
                                               row.GetColumn 0, (min a b, max a b)) |> Set.ofSeq

    let statesWithGeneTransitions =
      seq { for g in geneNames do
              yield g, set [ for (g', (a, b)) in edges do
                               if g = g' then yield (Map.find a states, Map.find b states) ] } |> Map.ofSeq

    let statesWithoutGeneTransitions =
      statesWithGeneTransitions |> Map.map (fun _ e -> Set.difference (Set.ofSeq <| Map.values states)
                                                                      (Set.map (fun (a, b) -> set [a; b]) e |> Set.unionMany))

    let initialStates = readLines initialStatesFilename |> Set.ofArray
    let targetStates = readLines targetStatesFilename |> Set.ofArray
    let initialStates = Map.values states |> Seq.filter (fun (s : State) -> Set.contains s.Name initialStates) |> Array.ofSeq
    let targetStates = Map.values states |> Seq.filter (fun (s : State) -> Set.contains s.Name targetStates) |> Array.ofSeq

    Synthesis.synthesise geneParameters statesWithGeneTransitions statesWithoutGeneTransitions initialStates targetStates outputDir oneSolution
    
[<EntryPoint>]
let main args =
    if Array.length args <> 6 && Array.length args <> 7  then
        failwith "Incorrect number of arguments. Correct format:\n\
                  SynthesisEngine.exe cmpStates.csv cmpEdges.csv cmpParameters.csv \
                  cmp_initial_states.txt cmp_target_states.txt <output_directory> [-oneSolution]"
    
    synth args.[0] args.[1] args.[2] args.[3] args.[4] args.[5] (Array.length args = 7)

    0
