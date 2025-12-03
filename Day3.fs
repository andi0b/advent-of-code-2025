module aoc25.Day3

let parse (input: string array) =
    input |> Array.map (Seq.map (fun chr -> chr - '0' |> int) >> Seq.toArray)

let maxJoltage2 (pack: int array) =
    let firstMax = pack |> Seq.take (pack.Length - 1) |> Seq.max
    let firstMaxPos = pack |> Array.findIndex ((=) firstMax)
    let secondMax = pack |> Seq.skip (firstMaxPos + 1) |> Seq.max
    firstMax * 10 + secondMax |> int64

let maxJoltage12 (pack: int array) =
    let findMax offset remaining =
        let slice = pack[offset .. pack.Length - remaining]
        let max = slice |> Array.max
        let idx = slice |> Array.findIndex ((=) max)
        int64 max, offset + idx + 1

    seq { 12..-1..1 }
    |> Seq.mapFold findMax 0
    |> fst
    |> Seq.rev
    |> Seq.mapi (fun i v -> v * pown 10L i)
    |> Seq.sum

let solve joltageFunc = parse >> Seq.map joltageFunc >> Seq.sum
let part1 = solve maxJoltage2
let part2 = solve maxJoltage12

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [| "987654321111111"; "811111111111119"; "234234234234278"; "818181911112111" |]

    [<Fact>]
    let ``parse test`` () =
        let parsed = example |> parse
        parsed[0] =! [| 9; 8; 7; 6; 5; 4; 3; 2; 1; 1; 1; 1; 1; 1; 1 |]

    [<Fact>]
    let ``joltage out of 2`` () =
        let parsed = parse example
        parsed[0] |> maxJoltage2 =! 98
        parsed[1] |> maxJoltage2 =! 89
        parsed[2] |> maxJoltage2 =! 78
        parsed[3] |> maxJoltage2 =! 92

    [<Fact>]
    let ``joltage out of 12`` () =
        let parsed = parse example
        parsed[0] |> maxJoltage12 =! 987654321111L
        parsed[1] |> maxJoltage12 =! 811111111119L
        parsed[2] |> maxJoltage12 =! 434234234278L
        parsed[3] |> maxJoltage12 =! 888911112111L

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 357

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 3121910778619L
