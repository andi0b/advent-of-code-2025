module aoc25.Day11

let parse =
    Array.map (StringEx.splitS ": " >> fun r -> r[0], r[1] |> StringEx.splitS " ")

let paths input =
    let map = parse input |> Map |> Map.add "out" [||]

    let rec paths' (from, to') =
        if from = to' then
            1L
        else
            map[from] |> Array.sumBy (fun n -> paths (n, to'))

    and paths = memorize paths'
    paths

let part1 input = paths input ("you", "out")

let part2 input =
    let combinePaths = List.pairwise >> List.map (paths input) >> List.reduce (*)

    combinePaths [ "svr"; "fft"; "dac"; "out" ]
    + combinePaths [ "svr"; "dac"; "fft"; "out" ]

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example1 =
        [| "aaa: you hhh"
           "you: bbb ccc"
           "bbb: ddd eee"
           "ccc: ddd eee fff"
           "ddd: ggg"
           "eee: out"
           "fff: out"
           "ggg: out"
           "hhh: ccc fff iii"
           "iii: out" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! 5

    let example2 =
        [| "svr: aaa bbb"
           "aaa: fft"
           "fft: ccc"
           "bbb: tty"
           "tty: ccc"
           "ccc: ddd eee"
           "ddd: hub"
           "hub: fff"
           "eee: dac"
           "dac: fff"
           "fff: ggg hhh"
           "ggg: out"
           "hhh: out" |]

    [<Fact>]
    let ``Part 2 example`` () = part2 example2 =! 2
