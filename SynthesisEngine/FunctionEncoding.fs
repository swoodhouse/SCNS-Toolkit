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

let [<Literal>] AND = 0
let [<Literal>] OR = 1
let NOTHING = NUM_GENES + 2

let makeCircuitVar = let numBits = (uint32 << ceil <| System.Math.Log(float NUM_GENES, 2.0)) + 1u
                     fun name -> BitVec (name, numBits)

let makeEnforcedVar = let numBits = (uint32 << ceil <| System.Math.Log(float NUM_NON_TRANSITIONS_ENFORCED_STATES, 2.0)) + 1u
                      fun name -> BitVec (name, numBits)

let private variableDomains lowerBound upperBound var =
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

    let circuitEncoding = And [| variableDomains 0 (NOTHING - 1) a.[0]
                                 Array.map (variableDomains 0 NOTHING) a.[1..2] |> And
                                 Array.map (variableDomains 0 NOTHING) r.[0..2] |> And
                                 Array.map (variableDomains 2 NOTHING) a.[3..6] |> And
                                 Array.map (variableDomains 2 NOTHING) r.[3..6] |> And

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

let private leftChild i = ((i + 1) * 2) - 1
let private rightChild i = leftChild i + 1

let private evaluateUpdateFunction = 
    let counter = ref 0
    
    fun (aVars : BitVec []) (rVars : BitVec []) (geneValues : bool []) ->
        let i = !counter
        counter := i + 1

        let geneValues = Array.map toBool geneValues
        
        let intermediateValueVariablesA = [| for j in 1 .. 7 -> Bool <| sprintf "va%i_%i" j i |]
        let intermediateValueVariablesR = [| for j in 1 .. 7 -> Bool <| sprintf "vr%i_%i" j i |]

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

                [| for i in 0 .. 2 -> andConstraints aVars intermediateValueVariablesA i (leftChild i) (rightChild i) |] |> And
                [| for i in 0 .. 2 -> andConstraints rVars intermediateValueVariablesR i (leftChild i) (rightChild i) |] |> And
                [| for i in 0 .. 2 -> orConstraints aVars intermediateValueVariablesA i (leftChild i) (rightChild i) |] |> And
                [| for i in 0 .. 2 -> orConstraints rVars intermediateValueVariablesR i (leftChild i) (rightChild i) |] |> And

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
