module Program

open DataLoading
open FSharp.Data

let synth (statesFilename : string) edgesFilename initialStatesFilename targetStatesFilename nonTransitionsNodesFilename outputDir =
    let geneNames = CsvFile.Load(statesFilename).Headers |> Option.get |> Seq.skip 1 |> Array.ofSeq

    let f n = 2 + (Seq.findIndex ((=) n) geneNames)
    let geneIds = geneNames |> Seq.map f |> Set.ofSeq

    let initialStates = readLines initialStatesFilename
    let targetStates = readLines targetStatesFilename
    let nonTransitionEnforcedStates = readLines nonTransitionsNodesFilename |> Set.ofArray

    let geneParameters = Map.ofList ["Gata2", (1, 3)
                                     "Gata1", (3, 1)
                                     "Fog1", (1, 0)
                                     "EKLF", (1, 1)
                                     "Fli1", (1, 1)
                                     "Scl", (1, 1)
                                     "Cebpa", (1, 3)
                                     "Pu.1", (2, 2)
                                     "cJun", (1, 1)
                                     "EgrNab", (2, 1)
                                     "Gfi1", (1, 1)]

    Synthesis.synthesise geneIds geneNames geneParameters statesFilename initialStates targetStates nonTransitionEnforcedStates outputDir
    
[<EntryPoint>]
let main args =
    synth args.[0] args.[1] args.[2] args.[3] args.[4] args.[5]

    0