module aoc25.Day08

open System.Numerics

let allCombinations (r: 'a array) =
    seq {
        for i in 0 .. r.Length - 1 do
            for j in i + 1 .. r.Length - 1 do
                r[i], r[j]
    }

let parse (lines: string array) =
    lines |> Array.map (fun s -> s.Split(',') |> Array.map float32 |> Vector3)

let part1 (items: int) input =
    let distances =
        input
        |> parse
        |> Array.indexed
        |> allCombinations
        |> Seq.map (fun ((i, a), (j, b)) -> (i, j), Vector3.DistanceSquared(a, b))
        |> Seq.sortBy snd
        |> Seq.take items
        |> Seq.toArray

    let findConnections item =
        distances
        |> Seq.filter (fun ((a,b), _) -> a = item || b = item)
        |> Seq.map (fun ((a,b), _) -> if item = a then b else a)

    let connectedBoxes =
        distances
        |> Seq.collect (fst >> (fun (a, b) -> [ a; b ]))
        |> Seq.distinct
        |> Set.ofSeq

    let circuits = connectedBoxes |> List.unfold (fun set ->
            let rec traverse added remaining =
                let newboxes =  added  |> Seq.collect findConnections |> Set |> Set.intersect remaining
                if newboxes |> Set.isEmpty then
                    added, remaining
                else
                    traverse (added + newboxes) (remaining - newboxes)

            if set |> Set.isEmpty then
                None
            else
                let min = set |> Set.minElement
                let circuit, remaining = traverse ([min] |> Set) (set |> Set.remove min)

                Some (circuit, remaining)
        )

    circuits |> List.map _.Count |> List.sortDescending |> List.take 3 |> List.reduce (*)

let part2 input =
    let distances =
        input
        |> parse
        |> Array.indexed
        |> allCombinations
        |> Seq.map (fun ((i, a), (j, b)) -> (i, j), Vector3.DistanceSquared(a, b))
        |> Seq.sortBy snd
        |> Seq.toList


    let boxes = Set [0.. (input |> parse |> Array.length) - 1]

    let rec traverse (distances: ((int*int) * float32) list) unconnectedBoxes =
        match distances with
        | [] -> failwith "duh!"
        | ((a,b) , _) :: remaining ->
            let newUnconnectedBoxes = unconnectedBoxes |> Set.remove a |> Set.remove b
            if newUnconnectedBoxes.IsEmpty then
                (a,b)
            else
                traverse remaining newUnconnectedBoxes

    let (a,b) = traverse distances boxes

    let boxes =  input |> parse

    (boxes[a].X |> int64) * ( boxes[b].X |> int64)



let run = runReadAllLines (part1 1000) part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [| "162,817,812"
           "57,618,57"
           "906,360,560"
           "592,479,940"
           "352,342,300"
           "466,668,158"
           "542,29,236"
           "431,825,988"
           "739,650,466"
           "52,470,668"
           "216,146,977"
           "819,987,18"
           "117,168,530"
           "805,96,715"
           "346,949,466"
           "970,615,88"
           "941,993,340"
           "862,61,35"
           "984,92,344"
           "425,690,689" |]


    [<Fact>]
    let ``Part 1 example`` () = part1 10 example =! 40

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 25272L
