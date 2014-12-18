module FunctionEncoding

open FSharp.Data
open DataLoading
open Microsoft.Z3.FSharp.Common
open Microsoft.Z3.FSharp.Bool
open Microsoft.Z3.FSharp.BitVec

let toBool b = if b then True else False

let rec private delete x = function
  | [] -> []
  | h :: t when x = h -> t
  | h :: t -> h :: delete x t

let private numGenes = let statesFilename = System.Environment.GetCommandLineArgs().[1]
                       (CsvFile.Load(statesFilename).Headers |> Option.get |> Seq.length) - 1

let private numNonTransitionEnforcedStates = let nonTransitionEnforcedStatesFilename = System.Environment.GetCommandLineArgs().[5]
                                             readLines nonTransitionEnforcedStatesFilename |> Array.length

let [<Literal>] AND = 0
let [<Literal>] OR = 1
let NOTHING = numGenes + 2

let makeCircuitVar = let numBits = (uint32 << ceil <| System.Math.Log(float numGenes, 2.0)) + 1u
                     fun name -> BitVec (name, numBits)
                     
let makeEnforcedVar = let numBits = (uint32 << ceil <| System.Math.Log(float numNonTransitionEnforcedStates, 2.0)) + 1u
                      fun name -> BitVec (name, numBits)

let private variableDomains var lowerBound upperBound =
    (var >=. lowerBound) &&. (var <=. upperBound)

let private parentsOfNothingArentGates (a : BitVec []) (r : BitVec []) =
    let f c1 c2 p = ((c1 =. NOTHING) ||. (c2 =. NOTHING)) =>. ((p <>. AND) &&. (p <>. OR))

    let aParents = And [| Implies ((a.[1] =. NOTHING) ||. (a.[2] =. NOTHING),  And [| a.[0] <>. AND; a.[0] <>. OR; a.[0] <>. NOTHING |])
                          f a.[3] a.[4] a.[1]
                          f a.[5] a.[6] a.[2] |]

    let rParents = And [| f r.[1] r.[2] r.[0]
                          f r.[3] r.[4] r.[1]
                          f r.[5] r.[6] r.[2] |]
                              
    aParents &&. rParents

let private parentsOfRestAreGates (a : BitVec []) (r : BitVec []) =
    let f c1 c2 p = ((c1 <>. NOTHING) ||. (c2 <>. NOTHING)) =>.  ((p =. AND) ||. (p =. OR))

    let aParents = And [| f a.[1] a.[2] a.[0]
                          f a.[3] a.[4] a.[1]
                          f a.[5] a.[6] a.[2] |]

    let rParents = And [| f r.[1] r.[2] r.[0]
                          f r.[3] r.[4] r.[1]
                          f r.[5] r.[6] r.[2] |]

    aParents &&. rParents

let private variablesDoNotAppearMoreThanOnce symVars =
    let isVar (v : BitVec) = And [| v <>. NOTHING; v <>. AND; v <>. OR |]
    let notEqual v vars = List.map ((<>.) v) vars |> Array.ofList |> And
    let doesNotAppearMoreThanOnce (v : BitVec) = 
        (isVar v) =>. (notEqual v (delete v symVars))
    List.map doesNotAppearMoreThanOnce symVars |> Array.ofList |> And

let private enforceSiblingLexigraphicalOrdering (v1 : BitVec) (v2 : BitVec) =
    v1 <=. v2

let private enforceLexigraphicalOrderingBetweenBranches (p1 : BitVec) (p2 : BitVec) (c1 : BitVec) (c2 : BitVec) =
    (p1 =. p2) =>. (c1 <=. c2)

let private enforceLexigraphicalOrderingNaryGate (vars : BitVec []) =
    (vars.[0] =. vars.[1]) =>. (vars.[2] <=. vars.[3])

let activatorVars = set [ for i in 1..7 do yield "a" + string i ]
let repressorVars = set [ for i in 1..7 do yield "r" + string i ]
let circuitVars = activatorVars + repressorVars

let private fixMaxInputs v max =
    match max with
    | 0 -> makeCircuitVar (v + "1") =. NOTHING
    | 1 -> makeCircuitVar (v + "2") =. NOTHING
    | 2 -> makeCircuitVar (v + "4") =. NOTHING
    | 3 -> makeCircuitVar (v + "6") =. NOTHING
    | _ -> True

let private fixMaxActivators = fixMaxInputs "a"
let private fixMaxRepressors = fixMaxInputs "r"

let encodeUpdateFunction gene genes maxActivators maxRepressors =
    if not (Set.contains gene genes && maxActivators > 0 && maxActivators <= 4 && maxRepressors >= 0 && maxRepressors <= 4) then
        failwith "Incorrect arguments to encodeForUpdateFunction"

    let a = [| for i in 1..7 -> makeCircuitVar (sprintf "a%i" i) |]
    let r = [| for i in 1..7 -> makeCircuitVar (sprintf "r%i" i) |]

    let circuitEncoding = And <| [| variableDomains a.[0] 0 (NOTHING - 1)
                                    variableDomains r.[0] 0 NOTHING

                                    variableDomains a.[1] 0 NOTHING; variableDomains a.[2] 0 NOTHING              
                                    variableDomains r.[1] 0 NOTHING; variableDomains r.[2] 0 NOTHING

                                    variableDomains a.[3] 2 NOTHING; variableDomains a.[4] 2 NOTHING; variableDomains a.[4] 2 NOTHING; variableDomains a.[5] 2 NOTHING
                                    variableDomains a.[6] 2 NOTHING
                                    variableDomains r.[3] 2 NOTHING; variableDomains r.[4] 2 NOTHING; variableDomains r.[4] 2 NOTHING; variableDomains r.[5] 2 NOTHING
                                    variableDomains r.[6] 2 NOTHING

                                    parentsOfNothingArentGates a r
                                    parentsOfRestAreGates a r
                                    variablesDoNotAppearMoreThanOnce (List.ofSeq <| a ++ r)
                                    
                                    And [| for i in 1 .. 2 .. 5 -> enforceSiblingLexigraphicalOrdering a.[i] a.[i + 1] |]
                                    And [| for i in 1 .. 2 .. 5 -> enforceSiblingLexigraphicalOrdering r.[i] r.[i + 1] |]
                                    
                                    enforceLexigraphicalOrderingBetweenBranches a.[1] a.[2] a.[3] a.[5]
                                    enforceLexigraphicalOrderingBetweenBranches r.[1] r.[2] r.[3] r.[5]

                                    enforceLexigraphicalOrderingNaryGate a
                                    enforceLexigraphicalOrderingNaryGate r
                                    
                                    fixMaxActivators maxActivators
                                    fixMaxRepressors maxRepressors |]
    (circuitEncoding, a, r)
 
let private evaluateUpdateFunction = 
    let counter = ref 0
    
    fun (aVars : BitVec []) (rVars : BitVec []) (geneValues : bool []) ->
        let i = !counter
        counter := i + 1

        let geneValues = Array.map toBool geneValues
        
        let intermediateValueVariablesA = [| for j in 1 .. 7 ->  Bool <| sprintf "va%i_%i" j i |]
        let intermediateValueVariablesR = [| for j in 1 .. 7 ->  Bool <| sprintf "vr%i_%i" j i |]

        let andConstraints (symVars : BitVec []) (variables : Bool []) pi c1i c2i =
            Implies (symVars.[pi] =. AND, variables.[pi] =. (variables.[c1i] &&. variables.[c2i]))

        let orConstraints (symVars : BitVec []) (variables : Bool []) pi c1i c2i =
            Implies (symVars.[pi] =. OR, variables.[pi] =. (variables.[c1i] ||. variables.[c2i]))
        
        let variableConstraints (symVars : BitVec []) (intermediateVars : Bool []) =
            let f i symVar =
                [| for v in 2 .. (NOTHING - 1) do
                       yield Implies (symVar =. v, intermediateVars.[i] =. geneValues.[v - 2])
                |] |> And

            Array.mapi f symVars |> And

        let circuitValue =
            let noRepressors = rVars.[0] =. NOTHING
            If (noRepressors,
                intermediateValueVariablesA.[0],
                intermediateValueVariablesA.[0] &&. Not intermediateValueVariablesR.[0])

        let circuitVal = Bool <| sprintf "circuit_%i" i
                        
        (And [| variableConstraints aVars intermediateValueVariablesA
                variableConstraints rVars intermediateValueVariablesR
                andConstraints aVars intermediateValueVariablesA 0 1 2
                andConstraints aVars intermediateValueVariablesA 1 3 4
                andConstraints aVars intermediateValueVariablesA 2 5 6
                andConstraints rVars intermediateValueVariablesR 0 1 2
                andConstraints rVars intermediateValueVariablesR 1 3 4
                andConstraints rVars intermediateValueVariablesR 2 5 6
                orConstraints aVars intermediateValueVariablesA 0 1 2
                orConstraints aVars intermediateValueVariablesA 1 3 4
                orConstraints aVars intermediateValueVariablesA 2 5 6
                orConstraints rVars intermediateValueVariablesR 0 1 2
                orConstraints rVars intermediateValueVariablesR 1 3 4
                orConstraints rVars intermediateValueVariablesR 2 5 6

                circuitVal =. circuitValue|], circuitVal)

let circuitEvaluatesToSame gene aVars rVars (profile : bool []) =
    let b = toBool profile.[gene - 2]
    let evaluationEncoding, circuitVal = evaluateUpdateFunction aVars rVars profile
    (evaluationEncoding, circuitVal =. b)
            
let circuitEvaluatesToDifferent gene aVars rVars (profile : bool []) =
    let b = toBool profile.[gene - 2]
    let evaluationEncoding, circuitVal = evaluateUpdateFunction aVars rVars profile
    (evaluationEncoding, circuitVal =. Not b)

let solutionToCircuit (geneNames : string []) (activatorAssignment : seq<int>) (repressorAssignment : seq<int>) =
    let leftChild i = ((i + 1) * 2) - 1
    let rightChild i = leftChild i + 1
    let toCircuit assignment =
        let rec toCircuit i =
            match Seq.nth i assignment with
            | AND -> Circuit.And (toCircuit <| leftChild i, toCircuit <| rightChild i)
            | OR -> Circuit.Or (toCircuit <| leftChild i, toCircuit <| rightChild i)
            | n when n = NOTHING -> failwith "solutionToCircuit pattern match error"
            | var -> Circuit.Node (geneNames.[var - 2])            
        toCircuit 0

    if Seq.head repressorAssignment = NOTHING then
        toCircuit activatorAssignment
    else
        toCircuit activatorAssignment |> Circuit.inhibition (toCircuit repressorAssignment)
