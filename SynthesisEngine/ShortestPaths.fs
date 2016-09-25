module ShortestPaths

open System.Collections.Generic

type private Cursor<'a when 'a : comparison> = {
   Focus: 'a
   Previous: 'a list }

let private neighbours graph visited cursor =
    if not <| Map.containsKey cursor.Focus graph then Seq.empty
    else
        seq { for neighbour in Map.find cursor.Focus graph do
                if not <| Set.contains neighbour visited then
                  yield { Focus = neighbour; Previous = cursor.Focus :: cursor.Previous } }

let shortestPathMultiSink graph source sinks =
  let q = Queue<Cursor<'a>>([{Focus = source; Previous = []}])
  let visited = HashSet<'a>()

  [ while q.Count > 0 && not <| visited.IsSupersetOf sinks do
      let u = q.Dequeue()
      for v in neighbours graph (Set.ofSeq visited) u do
          if not <| visited.Contains v.Focus then
              visited.Add(v.Focus) |> ignore
              q.Enqueue(v)
              if Set.contains v.Focus sinks then yield List.rev (v.Focus :: v.Previous) ]
