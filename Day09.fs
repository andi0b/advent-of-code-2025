module aoc25.Day09

[<Struct>]
type Point = { x: int; y: int }

[<Struct>]
type Rect = { x1: int; x2: int; y1: int; y2: int }

module Rect =
    let create a b =
        { x1 = min a.x b.x
          x2 = max a.x b.x
          y1 = min a.y b.y
          y2 = max a.y b.y }

    let corners r =
        [| { x = r.x1; y = r.y1 }
           { x = r.x1; y = r.y2 }
           { x = r.x2; y = r.y1 }
           { x = r.x2; y = r.y2 } |]

    let area r =
        (int64 (r.x2 - r.x1) + 1L) * (int64 (r.y2 - r.y1) + 1L)

[<Struct>]
type Segment =
    | Horizontal of y: int * x1: int * x2: int
    | Vertical of x: int * y1: int * y2: int

module Segment =
    let ofPoints a b =
        if a.y = b.y then Horizontal(a.y, min a.x b.x, max a.x b.x)
        elif a.x = b.x then Vertical(a.x, min a.y b.y, max a.y b.y)
        else failwith "Edges must be axis-aligned"

    let contains point =
        function
        | Horizontal(y, x1, x2) -> point.y = y && point.x >= x1 && point.x <= x2
        | Vertical(x, y1, y2) -> point.x = x && point.y >= y1 && point.y <= y2

    let crossesRect rect =
        function
        | Horizontal(y, x1, x2) -> y > rect.y1 && y < rect.y2 && max x1 rect.x1 < min x2 rect.x2
        | Vertical(x, y1, y2) -> x > rect.x1 && x < rect.x2 && max y1 rect.y1 < min y2 rect.y2

let allCombinations (r: 'a array) =
    seq {
        for i in 0 .. r.Length - 1 do
            for j in i + 1 .. r.Length - 1 do
                r[i], r[j]
    }

let parsePoints =
    let parsePoint s =
        let parts = StringEx.splitC ',' s
        { x = int parts[0]; y = int parts[1] }

    Array.map parsePoint

let part1 input =
    input
    |> parsePoints
    |> allCombinations
    |> Seq.map (fun (a, b) -> Rect.create a b |> Rect.area)
    |> Seq.max

let part2 input =
    let tiles = input |> parsePoints

    let edges =
        Array.init tiles.Length (fun i -> Segment.ofPoints tiles[i] tiles[(i + 1) % tiles.Length])

    let vEdges = edges |> Array.filter _.IsVertical

    let isInside point =
        let onBoundary = edges |> Array.exists (Segment.contains point)

        let rightwardCrossings =
            vEdges
            |> Array.sumBy (function
                | Vertical(x, y1, y2) when x <= point.x && point.y >= y1 && point.y < y2 -> 1
                | _ -> 0)

        onBoundary || (rightwardCrossings % 2 = 1)

    let isValid rect =
        Rect.corners rect |> Array.forall isInside
        && not (edges |> Array.exists (Segment.crossesRect rect))

    tiles
    |> allCombinations
    |> Seq.fold
        (fun best (a, b) ->
            let rect = Rect.create a b
            let area = Rect.area rect
            if area > best && isValid rect then area else best)
        0L

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example1 = [| "7,1"; "11,1"; "11,7"; "9,7"; "9,5"; "2,5"; "2,3"; "7,3" |]

    [<Fact>]
    let ``rect area`` () =
        test <@ Rect.create { x = 7; y = 1 } { x = 11; y = 7 } |> Rect.area = 35L @>
        test <@ Rect.create { x = 11; y = 7 } { x = 7; y = 1 } |> Rect.area = 35L @>

    [<Fact>]
    let ``rect create orders bounds`` () =
        test <@ Rect.create { x = 5; y = 7 } { x = 2; y = 3 } = { x1 = 2; x2 = 5; y1 = 3; y2 = 7 } @>

    [<Fact>]
    let ``segment ofPoints axis aligned`` () =
        test <@ Segment.ofPoints { x = 1; y = 2 } { x = 4; y = 2 } = Horizontal(2, 1, 4) @>
        test <@ Segment.ofPoints { x = 3; y = 1 } { x = 3; y = 5 } = Vertical(3, 1, 5) @>

    [<Fact>]
    let ``segment crosses rect when spanning`` () =
        let rect = Rect.create { x = 1; y = 1 } { x = 4; y = 4 }
        test <@ Segment.crossesRect rect (Vertical(2, 0, 5)) = true @>
        test <@ Segment.crossesRect rect (Horizontal(0, 0, 5)) = false @>

    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! 50L

    [<Fact>]
    let ``Part 2 example`` () = part2 example1 =! 24L
