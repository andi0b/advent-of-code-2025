module aoc25.Day07

let parse (input: string array) =
    let lines = input |> Array.map _.ToCharArray()
    let start = lines[0] |> Array.findIndex ((=) 'S')
    lines, start

let findSplitters (line: char array) =
    line |> Seq.indexed |> Seq.filter (snd >> ((=) '^')) |> Seq.map fst |> Set

let part1 input =
    let lines, start = parse input

    ((Set [ start ], 0), lines)
    ||> Array.fold (fun (tachyons, splits) line ->
        let splitters = line |> findSplitters
        let activeSplitters = Set.intersect tachyons splitters

        let splitTachyons =
            activeSplitters |> Seq.collect (fun s -> [ s - 1; s + 1 ]) |> Set

        let newTachyons = Set.difference tachyons activeSplitters |> Set.union splitTachyons
        newTachyons, splits + activeSplitters.Count)
    |> snd

let part2 input =
    let lines, start = parse input

    ([ (start, 1L) ], lines)
    ||> Array.fold (fun tachyons line ->
        let splitters = line |> findSplitters

        tachyons
        |> List.collect (fun (pos, count) ->
            if splitters |> Set.contains pos then
                [ (pos - 1, count); (pos + 1, count) ]
            else
                [ (pos, count) ])
        |> List.groupBy fst
        |> List.map (fun (pos, groups) -> pos, groups |> List.sumBy snd))
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
