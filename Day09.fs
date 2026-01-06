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
    let diff = b - a
    let abs = Vector2.Abs diff
    (int64 abs.X + 1L) * (int64 abs.Y + 1L)

let part1 input =
    input |> parse |> allCombinations |> Seq.map calcArea |> Seq.max

let part2 = (fun _ -> 0)

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

    let example2 = null

    [<Fact(Skip = "missing")>]
    let ``Part 2 example`` () = part2 example2 =! -1
