module aoc25.Day02

open System

let parseRange = StringEx.splitC '-' >> (fun r -> (r[0], r[1])) >> TupleEx.map int64
let parse = StringEx.splitC ',' >> Array.map parseRange

let isTwoRepetitions (num: int64) =
    let str = num.ToString().AsSpan()

    if str.Length % 2 = 1 then
        false
    else
        let halfLen = str.Length / 2
        let p1 = str.Slice(0, halfLen)
        let p2 = str.Slice(halfLen, halfLen)
        MemoryExtensions.Equals(p1, p2, StringComparison.OrdinalIgnoreCase)

let possibleRepetitionLengths =
    let inner digitCount =
        [ for i in 1 .. (digitCount / 2) do
              if digitCount % i = 0 then
                  i ]
        |> List.rev

    memorizeThreadLocal inner

let isRepeating (num: int64) =
    let str = num.ToString()

    let rec inner: int list -> bool =
        function
        | [] -> false
        | len :: remaining ->
            let reps = str.Length / len

            let part = str.AsSpan(0, len)
            let mutable m = true

            for i in 1 .. reps - 1 do
                let comp = str.AsSpan(i * len, len)
                m <- m && MemoryExtensions.Equals(part, comp, StringComparison.OrdinalIgnoreCase)

            if m then true else inner remaining

    inner (possibleRepetitionLengths str.Length)

let solve filter input =
    input
    |> parse
    |> Array.Parallel.map (fun (s, e) -> seq { s..e } |> Seq.filter filter |> Seq.fold (+) 0L)
    |> Array.sum

let part1 = solve isTwoRepetitions
let part2 = solve isRepeating

let run = runReadAllText part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

    [<Fact>]
    let ``parse`` () =
        let parsed = parse example
        parsed[0] =! (11, 22)
        parsed[1] =! (95, 115)
        parsed |> Array.last =! (2121212118, 2121212124)
        0

    [<Theory>]
    [<InlineData(1122, false)>]
    [<InlineData(111, false)>]
    [<InlineData(123123, true)>]
    [<InlineData(9999, true)>]
    [<InlineData(567567, true)>]
    let ``check two repetitions`` (number, expected) = number |> isTwoRepetitions =! expected

    [<Theory>]
    [<InlineData(1122, false)>]
    [<InlineData(111, true)>]
    [<InlineData(123123, true)>]
    [<InlineData(9999, true)>]
    [<InlineData(567567, true)>]
    [<InlineData(567567567, true)>]
    [<InlineData(567567568, false)>]
    let ``check any repetitions`` (number, expected) = number |> isRepeating =! expected

    [<Fact>]
    let ``possible repetition lengths`` () =
        possibleRepetitionLengths 1 =! []
        possibleRepetitionLengths 2 =! [ 1 ]
        possibleRepetitionLengths 3 =! [ 1 ]
        possibleRepetitionLengths 4 =! [ 2; 1 ]
        possibleRepetitionLengths 10 =! [ 5; 2; 1 ]
        possibleRepetitionLengths 9 =! [ 3; 1 ]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 1227775554L

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 4174379265L
