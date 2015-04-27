module ShortestPaths

open System.Collections.Generic

type private Cursor = {
    Focus: string
    Previous: Map<string, string> }

let private neighbours graph visited cursor =
    if not <| Map.containsKey cursor.Focus graph then Seq.empty
    else
        seq { for neighbour in Map.find cursor.Focus graph do
                if not <| Set.contains neighbour visited then
                  yield { Focus = neighbour; Previous = Map.add neighbour cursor.Focus cursor.Previous } }

let private previousToPath target previous =
    let rec previousToPath target path =
        if not <| Map.containsKey target previous then
            path
        else
            previousToPath (Map.find target previous) (target :: path)

    previousToPath target []

let shortestPathMultiSink graph source sinks =
  let q = Queue<Cursor>([{Focus = source; Previous = Map.empty}])
  let visited = HashSet<string>()
  
  [ while q.Count > 0 && not <| visited.IsSupersetOf sinks do
      let u = q.Dequeue()
      for v in neighbours graph (Set.ofSeq visited) u do
          if not <| visited.Contains v.Focus then
              visited.Add(v.Focus) |> ignore
              q.Enqueue(v)
              if Set.contains v.Focus sinks then yield source :: previousToPath v.Focus v.Previous ]
