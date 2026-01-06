module aoc25.Day09

open System.Numerics

let parse = Array.map (StringEx.splitC ',' >> Array.map float32 >> Vector2)

let allCombinations (r: 'a array) =
    seq {
        for i in 0 .. r.Length - 1 do
            for j in i + 1 .. r.Length - 1 do
                r[i], r[j]
    }

let calcArea (a: Vector2, b: Vector2) =
    let diff = b - a |> Vector2.Abs
    (int64 diff.X + 1L) * (int64 diff.Y + 1L)

let part1 input =
    input |> parse |> allCombinations |> Seq.map calcArea |> Seq.max

// lazy AI implementation
let part2 input =
    let tiles =
        input |> Array.map (StringEx.splitC ',' >> fun a -> (int a[0], int a[1]))

    let n = tiles.Length

    let hEdges, vEdges =
        [| for i in 0 .. n - 1 -> (tiles[i], tiles[(i + 1) % n]) |]
        |> Array.partition (fun ((_, y1), (_, y2)) -> y1 = y2)

    let isInside px py =
        vEdges
        |> Array.sumBy (fun ((x1, y1), (_, y2)) ->
            let lo, hi = min y1 y2, max y1 y2
            if x1 <= px && py >= lo && py < hi then 1 else 0)
        |> fun c -> c % 2 = 1

    let hCuts rx1 ry1 rx2 ry2 ((x1, y1), (x2, _)) =
        y1 > ry1 && y1 < ry2 && max x1 x2 > rx1 && min x1 x2 < rx2

    let vCuts rx1 ry1 rx2 ry2 ((x1, y1), (_, y2)) =
        x1 > rx1 && x1 < rx2 && max y1 y2 > ry1 && min y1 y2 < ry2

    let area (x1, y1) (x2, y2) =
        (int64 (abs (x2 - x1)) + 1L) * (int64 (abs (y2 - y1)) + 1L)

    let isValid ((x1, y1), (x2, y2)) =
        let rx1, rx2 = min x1 x2, max x1 x2
        let ry1, ry2 = min y1 y2, max y1 y2

        isInside rx1 ry1
        && not (hEdges |> Array.exists (hCuts rx1 ry1 rx2 ry2))
        && not (vEdges |> Array.exists (vCuts rx1 ry1 rx2 ry2))

    tiles
    |> allCombinations
    |> Seq.sortByDescending (fun (a, b) -> area a b)
    |> Seq.find isValid
    ||> area

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example1 = [| "7,1"; "11,1"; "11,7"; "9,7"; "9,5"; "2,5"; "2,3"; "7,3" |]

    [<Fact>]
    let ``calc area`` () =
        calcArea (Vector2(7f, 1f), Vector2(11f, 7f)) =! 35L
        calcArea (Vector2(11f, 7f), Vector2(7f, 1f)) =! 35L

    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! 50

    [<Fact>]
    let ``Part 2 example`` () = part2 example1 =! 24L
