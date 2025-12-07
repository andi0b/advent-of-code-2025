module aoc25.Day07

let findStart input =
    input |> Array.item 0 |> Seq.findIndex ((=) 'S')

let findSplitterSets =
    Array.map (Seq.indexed >> Seq.filter (snd >> ((=) '^')) >> Seq.map fst >> Set)

let part1 input =
    ((Set [ findStart input ], 0), findSplitterSets input)
    ||> Array.fold (fun (tachyons, splits) splitter ->
        let activeSplitters = splitter |> Set.intersect tachyons
        let newTachyons = activeSplitters |> Seq.collect (fun s -> [ s - 1; s + 1 ]) |> Set
        Set.difference tachyons activeSplitters |> Set.union newTachyons, splits + activeSplitters.Count)
    |> snd

let part2 input =
    ([ findStart input, 1L ], findSplitterSets input)
    ||> Array.fold (fun tachyons splitters ->
        [ for pos, count in tachyons do
              match splitters |> Set.contains pos with
              | true -> yield! [ pos - 1, count; pos + 1, count ]
              | false -> yield pos, count ]
        |> List.groupBy fst
        |> List.map (TupleEx.mapSnd <| List.sumBy snd))
    |> List.sumBy snd

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [| ".......S......."
           "..............."
           ".......^......."
           "..............."
           "......^.^......"
           "..............."
           ".....^.^.^....."
           "..............."
           "....^.^...^...."
           "..............."
           "...^.^...^.^..."
           "..............."
           "..^...^.....^.."
           "..............."
           ".^.^.^.^.^...^."
           "..............." |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 21

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 40
