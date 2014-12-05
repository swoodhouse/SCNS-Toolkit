module Program

open DataLoading
open FSharp.Data

let synth (statesFilename : string) edgesFilename (parametersFilename : string) initialStatesFilename targetStatesFilename nonTransitionsNodesFilename outputDir =
    let geneNames = CsvFile.Load(statesFilename).Headers |> Option.get |> Seq.skip 1 |> Array.ofSeq

    let f n = 2 + (Seq.findIndex ((=) n) geneNames)
    let geneIds = geneNames |> Seq.map f |> Set.ofSeq

    let initialStates = readLines initialStatesFilename
    let targetStates = readLines targetStatesFilename
    let nonTransitionEnforcedStates = readLines nonTransitionsNodesFilename |> Set.ofArray

    let geneParameters = CsvFile.Load(parametersFilename).Rows
                      |> Seq.map (fun (row : CsvRow) -> row.GetColumn 0, (System.Int32.Parse <| row.GetColumn 1, System.Int32.Parse <| row.GetColumn 2))
                      |> Map.ofSeq

    Synthesis.synthesise geneIds geneNames geneParameters statesFilename initialStates targetStates nonTransitionEnforcedStates outputDir
    
[<EntryPoint>]
let main args =
    synth args.[0] args.[1] args.[2] args.[3] args.[4] args.[5] args.[6]

    0