module aoc25.Day02

let parseRange = StringEx.splitC '-' >> (fun r -> (r[0], r[1])) >> TupleEx.map int64
let parse = StringEx.splitC ',' >> Array.map parseRange
let digitCount (num: int64) = (num |> double |> log10 |> int) + 1

let isTwoRepetitions (num: int64) =
    let digits = digitCount num

    if digits % 2 = 1 then
        false
    else
        let factor = pown 10L (digits / 2)
        let shifted = num / factor
        (num - shifted) = shifted * factor

let possibleRepetitionLengths =
    let inner digitCount =
        [ for i in 1.. (digitCount / 2)  do
              if digitCount % i = 0 then
                  i ]
        |> List.rev

    memorize inner

let isRepeating (num: int64) =
    let digits = digitCount num

    let rec inner: int list -> bool =
        function
        | [] -> false
        | len :: remaining ->
            let reps = digits / len
            let factor = pown 10L len
            let repVal = num / pown factor (reps - 1)

            let reconstructed =
                seq { 0 .. reps - 1 } |> Seq.map (fun i -> repVal * (pown factor i)) |> Seq.sum

            if reconstructed = num then true else inner remaining

    inner (possibleRepetitionLengths digits)


let solve filter input =
    let invalids =
        input
        |> parse
        |> Seq.collect (fun (s, e) -> seq { s..e } |> Seq.filter filter)
        |> Seq.map int64
        |> Seq.toList

    invalids |> Seq.sum






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
    [<InlineData(9, 1)>]
    [<InlineData(111, 3)>]
    [<InlineData(123456789, 9)>]
    let ``count digits`` (number, expected) = number |> digitCount =! expected

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
