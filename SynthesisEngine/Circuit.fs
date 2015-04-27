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