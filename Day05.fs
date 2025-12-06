module aoc25.Day05

let parse =
    let parseRange = StringEx.splitC '-' >> (fun s -> int64 s[0], int64 s[1])

    let mapLines mapper =
        StringEx.splitSs [| "\n"; "\r\n" |]
        >> Array.filter ((<>) "")
        >> Array.map mapper

    StringEx.splitSs [| "\n\n"; "\r\n\r\n" |]
    >> (fun s -> mapLines parseRange s[0], mapLines int64 s[1])

let part1 input =
    let ranges, nums = parse input

    let inRange ranges num =
        ranges |> Array.tryFind (fun (s, e) -> num >= s && num <= e) |> Option.isSome

    nums |> Array.filter (inRange ranges) |> Array.length

let part2 input =
    let sortedRanges = input |> parse |> fst |> Seq.sortBy fst |> Seq.toList

    let rec mergeNext cur =
        function
        | (s, e) :: remaining when s <= snd cur -> mergeNext (fst cur, max (snd cur) e) remaining
        | remaining -> cur, remaining

    sortedRanges
    |> List.unfold (function
        | cur :: remaining -> mergeNext cur remaining |> Some
        | _ -> None)
    |> List.sumBy (fun (s, e) -> e - s + 1L)


let run = runReadAllText part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        """3-5
10-14
16-20
12-18

1
5
8
11
17
32
""" // the input has an empty line at the end

    [<Fact>]
    let ``parse input`` () =
        let ranges, ingredients = parse example
        ranges =! [| (3, 5); (10, 14); (16, 20); (12, 18) |]
        ingredients =! [| 1; 5; 8; 11; 17; 32 |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 3

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 14
