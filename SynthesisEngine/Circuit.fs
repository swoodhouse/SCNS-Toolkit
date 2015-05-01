module Circuit

type Circuit = Value of bool | Node of string | And of Circuit * Circuit | Or of Circuit * Circuit | Not of Circuit

let inhibition pre activ = And (activ, Not pre)

let rec evaluate circuit nodeValues =
    match circuit with
    | Value b -> b
    | And (c1, c2) -> evaluate c1 nodeValues && evaluate c2 nodeValues
    | Or (c1, c2) -> evaluate c1 nodeValues || evaluate c2 nodeValues
    | Not c -> not (evaluate c nodeValues)
    | Node name -> Map.find name nodeValues

let rec printCircuit c =
    match c with
    | Value b -> string b
    | And (c1, c2) -> sprintf "And(%s, %s)" (printCircuit c1) (printCircuit c2)
    | Or (c1, c2) -> sprintf "Or(%s, %s)" (printCircuit c1) (printCircuit c2)
    | Not c -> sprintf "Not %s" (printCircuit c)
    | Node name -> name
