module Synthesis

open FSharp.Data
open Microsoft.Z3
open Microsoft.Z3.FSharp.Common
open Microsoft.Z3.FSharp.Bool
open DataLoading
open FunctionEncoding
open ShortestPaths

type NumNonTransitionsEnforced = All | Num of int | DropFraction of int

let private constraintsBitVec ctor (m : Model) (d : FuncDecl) =
    let x = System.Int32.Parse(m.[d].ToString())
    (ctor (d.Name.ToString())) <>. x

let private addConstraintsCircuitVar (solver : Solver) (m : Model) (ds : FuncDecl []) =
    let constraints = Or <| Array.map (constraintsBitVec makeCircuitVar m) ds
    solver.Add(constraints)

let private buildGraph edges =
    let mutable adjacency = Map.empty
    for (u, v) in edges do
        let something = if Map.containsKey u adjacency then v :: Map.find u adjacency else [v]
        adjacency <- Map.add u something adjacency
    adjacency

let private askNonTransition gene aVars rVars =
    let counter = ref 0
        
    fun (profile : bool []) ->
        let nonTransitionEnforced = makeEnforcedVar (sprintf "enforced_%i" !counter)
        counter := !counter + 1

        let encoding, same = circuitEvaluatesToSame gene aVars rVars profile
        (encoding &&. If (same, nonTransitionEnforced =. 1, nonTransitionEnforced =. 0),
            nonTransitionEnforced)

let private manyNonTransitionsEnforced gene aVars rVars expressionProfilesWithoutGeneTransitions numNonTransitionsEnforced =
    if numNonTransitionsEnforced = 0 then True
    else
        let askNonTransitions, enforceVars = Array.unzip << Array.ofSeq <| Seq.map (askNonTransition gene aVars rVars) expressionProfilesWithoutGeneTransitions
        let askNonTransitions = And askNonTransitions
        let manyEnforced = Array.reduce (+) enforceVars >=. numNonTransitionsEnforced

        askNonTransitions &&. manyEnforced

let private findAllowedEdges (solver : Solver) gene genes (geneNames : string []) maxActivators maxRepressors numNonTransitionsEnforced
                             (expressionProfilesWithGeneTransitions : Runtime.CsvFile<CsvRow>) (expressionProfilesWithoutGeneTransitions : Runtime.CsvFile<CsvRow>) =
    let seenEdges = System.Collections.Generic.HashSet<string * string>()

    let circuitEncoding, aVars, rVars = encodeUpdateFunction gene genes maxActivators maxRepressors
    let expressionProfilesWithoutGeneTransitions = Seq.map rowToArray expressionProfilesWithoutGeneTransitions.Rows

    let numNonTransitionsEnforced =
        match numNonTransitionsEnforced with
        | All -> expressionProfilesWithoutGeneTransitions |> Seq.length
        | Num i -> i
        | DropFraction i -> let max = expressionProfilesWithoutGeneTransitions |> Seq.length
                            max - max / i

    let undirectedEdges = geneTransitions geneNames.[gene - 2] |> Set.ofArray
    let manyNonTransitionsEnforced = manyNonTransitionsEnforced gene aVars rVars expressionProfilesWithoutGeneTransitions numNonTransitionsEnforced

    let encodeTransition stateA =
        let profile s = expressionProfilesWithGeneTransitions.Filter(fun row -> row.Columns.[0] = s).Rows |> Seq.head |> rowToArray
        let differentA = (let e, v = circuitEvaluatesToDifferent gene aVars rVars (profile stateA) in e &&. v)

        differentA

    let checkEdge (a, b) =
        if seenEdges.Contains (a, b) then true
        else
            solver.Reset()
            solver.Add (circuitEncoding,
                        manyNonTransitionsEnforced,
                        encodeTransition a)

            if solver.Check() = Status.SATISFIABLE then
                let m = solver.Model

                let activatorDecls = Array.filter (fun (d : FuncDecl) -> Set.contains (d.Name.ToString()) activatorVars) m.ConstDecls |> Array.sortBy (fun d -> d.Name.ToString().Remove(0,1).AsInteger())
                let repressorDecls = Array.filter (fun (d : FuncDecl) -> Set.contains (d.Name.ToString()) repressorVars) m.ConstDecls |> Array.sortBy (fun d -> d.Name.ToString().Remove(0,1).AsInteger())
                let activatorAssignment =
                    activatorDecls |> Seq.map (fun d -> System.Int32.Parse(m.[d].ToString()))
                let repressorAssignment = repressorDecls |> Seq.map (fun d -> System.Int32.Parse(m.[d].ToString()))

                let circuit = solutionToCircuit geneNames activatorAssignment repressorAssignment
                                
                let profile state =
                    expressionProfilesWithGeneTransitions.Filter(fun row -> row.Columns.[0] = state).Rows |> Seq.head |> rowToArray
                    
                let arrayToStateMap (a : bool []) =
                    Map.ofArray <| Array.zip geneNames a

                for (a, b) in undirectedEdges do
                    let stateA = profile a
                    if Circuit.evaluate circuit (arrayToStateMap stateA) <> stateA.[gene - 2] then
                        seenEdges.Add (a, b) |> ignore
                    let stateB = profile b
                    if Circuit.evaluate circuit (arrayToStateMap stateB) <> stateB.[gene - 2] then
                        seenEdges.Add (b, a) |> ignore

                true
            else
                false
    set [ for (a, b) in undirectedEdges do
              if checkEdge (a, b) then yield (a, b)
              if checkEdge (b, a) then yield (b, a) ]

let private findFunctions (solver : Solver) gene genes (geneNames : string []) maxActivators maxRepressors numNonTransitionsEnforced shortestPaths
                          (expressionProfilesWithGeneTransitions : Runtime.CsvFile<CsvRow>) (expressionProfilesWithoutGeneTransitions : Runtime.CsvFile<CsvRow>) =
    let circuitEncoding, aVars, rVars = encodeUpdateFunction gene genes maxActivators maxRepressors
    let expressionProfilesWithoutGeneTransitions = Seq.map rowToArray expressionProfilesWithoutGeneTransitions.Rows
    let undirectedEdges = geneTransitions geneNames.[gene - 2] |> Set.ofArray
    
    let numNonTransitionsEnforced =
        match numNonTransitionsEnforced with
        | All -> expressionProfilesWithoutGeneTransitions |> Seq.length
        | Num i -> i
        | DropFraction i -> let max = expressionProfilesWithoutGeneTransitions |> Seq.length
                            max - max / i

    let encodeTransition (stateA, stateB) =
        if not (Set.contains (stateA, stateB) undirectedEdges || Set.contains (stateB, stateA) undirectedEdges)
        then
            True
        else
        let profile s = expressionProfilesWithGeneTransitions.Filter(fun row -> row.Columns.[0] = s).Rows |> Seq.head |> rowToArray
        let differentA = (let e, v = circuitEvaluatesToDifferent gene aVars rVars (profile stateA) in e &&. v)

        differentA

    let encodePath (path : string list) =
        let f (formula, u) v = (And [| formula; encodeTransition (u, v) |], v)
        List.fold f (True, List.head path) (List.tail path) |> fst
    
    let pathsEncoding = if Seq.isEmpty shortestPaths then True else
                        And [| for paths in shortestPaths do
                                   yield Or [| for path in paths do
                                                   yield encodePath path |] |]

    solver.Reset()
    solver.Add (circuitEncoding,
                pathsEncoding,
                manyNonTransitionsEnforced gene aVars rVars expressionProfilesWithoutGeneTransitions numNonTransitionsEnforced)

    let intToName i = if i = AND then "And"
                      elif i = OR then "Or"
                      elif i = NOTHING then "Nothing"
                      else geneNames.[i - 2]

    set [ while solver.Check() = Status.SATISFIABLE do
                let m = solver.Model

                let circuitDecls = Array.filter (fun (d : FuncDecl) -> Set.contains (d.Name.ToString()) circuitVars) m.ConstDecls
                addConstraintsCircuitVar solver m circuitDecls

                let enforceDecls = Array.filter (fun (d : FuncDecl) -> d.Name.ToString().StartsWith "enforced") m.ConstDecls
                let numEnforced = List.sum <| [ for d in enforceDecls do yield System.Int32.Parse (string m.[d]) ]

                yield ("numEnforced", string numEnforced) :: [ for d in circuitDecls do
                                                                    let value = System.Int32.Parse(m.[d].ToString())
                                                                    if value <> NOTHING then
                                                                        yield (sprintf "%O" d.Name, intToName value) ] ]

let synthesise geneIds geneNames geneParameters statesFilename initialStates targetStates nonTransitionEnforcedStates outputDir =
    let f n = 2 + (Seq.findIndex ((=) n) geneNames)

    let getExpressionProfiles = getExpressionProfiles statesFilename nonTransitionEnforcedStates geneNames
    let solver = Solver()

    let allowedEdges = geneNames |> Array.map (fun g -> let a, r = Map.find g geneParameters
                                                        let expressionProfilesWithGeneTransitions, expressionProfilesWithoutGeneTransitions = getExpressionProfiles (f g)
                                                        findAllowedEdges solver (f g) geneIds geneNames a r All expressionProfilesWithGeneTransitions expressionProfilesWithoutGeneTransitions)
                                 |> Set.unionMany

    let reducedStateGraph = buildGraph allowedEdges

    let shortestPaths = initialStates |> Array.map (fun initial ->
        let targetStates = targetStates |> Set.ofArray |> Set.remove initial
        shortestPathMultiSink reducedStateGraph initial targetStates) 

    let invertedPaths = [| for i in 0 .. Array.length targetStates - 1 do
                               yield [ for j in 0 .. Array.length initialStates - 1 do
                                           for path in shortestPaths.[j] do
                                               match path with
                                               | [] -> ()
                                               | l -> if List.nth l (List.length l - 1) = targetStates.[i] then yield l ] |]

    geneNames |> Array.iter (fun gene -> let numAct, numRep = Map.find gene geneParameters
                                         let expressionProfilesWithGeneTransitions, expressionProfilesWithoutGeneTransitions = getExpressionProfiles (f gene)
                                         let circuits = findFunctions solver (f gene) geneIds geneNames numAct numRep All invertedPaths expressionProfilesWithGeneTransitions expressionProfilesWithoutGeneTransitions
                                         System.IO.File.WriteAllLines (outputDir + "/" + gene + ".txt", Seq.map (sprintf "%A") circuits))
