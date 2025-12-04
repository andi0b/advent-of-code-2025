module aoc25.Day04

type Point = (struct (int * int))

module Point =
    let inline add ((a, b): Point) ((c, d): Point) = struct (a + c, b + d)

    let allDirections =
        let directions = [ -1; 0; 1 ]

        Seq.allPairs directions directions
        |> Seq.except [ (0, 0) ]
        |> Seq.map (fun (a, b) -> Point(a, b))
        |> Seq.toArray

    let inline adjacent (p: Point) = allDirections |> Array.map (add p)

module Grid =
    let parse =
        let parseLine (line: string) =
            line.ToCharArray()
            |> Array.map (function
                | '@' -> 1uy
                | _ -> 0uy)

        Array.map parseLine

    let inline at (struct (x, y)) (grid: byte array array) =
        if y >= 0 && y < grid.Length && x >= 0 && x < grid[y].Length then
            grid[y][x]
        else
            0uy

    let inline set value (struct (x, y)) (grid: byte array array) =
        if y >= 0 && y < grid.Length && x >= 0 && x < grid[y].Length then
            grid[y][x] <- value
        else
            failwith "out of bounds"


    let allPos (grid: byte array array) =
        seq {
            for y = 0 to grid.Length - 1 do
                for x = 0 to grid[y].Length - 1 do
                    yield Point(x, y)
        }

let part1 lines =
    let grid = lines |> Grid.parse

    grid
    |> Grid.allPos
    |> Seq.filter (fun pos -> (grid |> Grid.at pos) = 1uy)
    |> Seq.filter (fun pos ->
        let adjPapers =
            pos
            |> Point.adjacent
            |> Seq.map (fun adj -> grid |> Grid.at adj)
            |> Seq.filter ((=) 1uy)
            |> Seq.length

        adjPapers < 4)
    |> Seq.length

let part2 lines =
    let grid = lines |> Grid.parse

    let findReachable () =
        [| for y = 0 to grid.Length - 1 do
               for x = 0 to grid[y].Length - 1 do
                   let pos = Point(x, y)

                   let isMatch =
                       grid |> Grid.at pos = 1uy
                       && (pos |> Point.adjacent |> Array.fold (fun sum adj -> sum + Grid.at adj grid) 0uy) < 4uy

                   if isMatch then
                       yield pos |]

    let removableByGeneration =
        List.unfold
            (fun _ ->
                match findReachable () with
                | [||] -> None
                | reachable ->
                    for p in reachable do
                        grid |> Grid.set 0uy p

                    Some(reachable.Length, ()))
            ()

    removableByGeneration |> List.sum

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [| "..@@.@@@@."
           "@@@.@.@.@@"
           "@@@@@.@.@@"
           "@.@@@@..@."
           "@@.@@@@.@@"
           ".@@@@@@@.@"
           ".@.@.@.@@@"
           "@.@@@.@@@@"
           ".@@@@@@@@."
           "@.@.@@@.@." |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 13

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 43
