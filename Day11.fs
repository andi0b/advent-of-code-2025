module aoc25.Day11


let parse =
    Array.map
    <| (StringEx.splitS ": " >> fun r -> r[0], r[1] |> StringEx.splitS " ")


let part1 input =
    let nextNodes (cur: string list, map: Map<string, string array>) =
        let head = cur |> List.head
        let nodes = map |> Map.tryFind head |> Option.defaultValue Array.empty

        [ for n in nodes do
              n :: cur, map |> Map.remove head ]

    let paths from to' map =
        [ [ from ], map ]
        |> List.unfold (fun items ->
            let outs, remaining =
                items
                |> List.partition (function
                    | head :: _, _ when head = to' -> true
                    | _ -> false)

            let next = remaining |> List.collect nextNodes

            match outs, next with
            | [], [] -> None
            | _ -> Some(outs, next))
        |> List.collect id

    parse input |> Map |> paths "you" "out" |> List.length

let part2 input =
    let parsed = parse input |> Array.append [| "out", [||] |]
    let name2index = parsed |> Array.mapi (fun i (key, _) -> key, i |> int16) |> Map
    let getIndex n = name2index[n]
    let map = parsed |> Array.map (fun (_, value) -> value |> Array.map getIndex)

    let paths from to' =
        let rec inner' node =
            if node = to' then
                1L
            else
                match map[node |> int] with
                | [||] -> 0L
                | next -> next |> Array.sumBy inner

        and inner = memorize inner'
        inner from

    let combinePaths =
        List.map getIndex
        >> List.pairwise
        >> List.map (fun x -> x ||> paths)
        >> List.reduce (*)

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
