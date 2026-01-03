module aoc25.Day12

open FSharp.Text.RegexExtensions
open FSharp.Text.RegexProvider

type ShapeRegex = Regex< @"[0-9]:([\r\n]+[#.]+)+" >

let parseShapes text =
    text
    |> ShapeRegex().TypedMatches
    |> Seq.map (fun m -> m.Value |> Seq.filter ((=) '#') |> Seq.length)
    |> Seq.toArray

type ProblemRegex = Regex< @"(?<x>[0-9]+)x(?<y>[0-9]+):(?<spec> [0-9]+)+" >

let parseProblems text =
    text
    |> ProblemRegex().TypedMatches
    |> Seq.map (fun m -> m.x.AsInt, m.y.AsInt, m.spec.Captures |> Seq.map (fun c -> int c.Value) |> Seq.toArray)
    |> Seq.toArray

type Classification =
    | Fit
    | MightFit
    | DoesntFit

let part1 input =
    let shapes = parseShapes input
    let problems = parseProblems input

    let classify (x, y, counts) =
        let area = x * y
        let parcels = Array.zip shapes counts |> Array.sumBy (fun (a, b) -> a * b)

        let shapeCount = Array.sum counts
        let thirdsArea = (x / 3) * (y / 3)

        if (parcels > area) then DoesntFit
        else if (shapeCount <= thirdsArea) then Fit
        else MightFit

    let classes =
        problems
        |> Array.groupBy classify
        |> Array.map (fun (c, p) -> c, p |> Array.length)

    let find c =
        classes |> Array.tryFind (fst >> ((=) c)) |> Option.defaultValue (c, 0) |> snd

    $"Fit: {find Fit}; MightFit: {find MightFit}; DoesntFit: {find DoesntFit}"

let part2 = (fun _ -> "🎄")

let run = runReadAllText part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        """0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2
"""

    [<Fact>]
    let ``parse shapes`` () =
        parseShapes example =! [| 7; 7; 7; 7; 7; 7 |]

    [<Fact>]
    let ``parse problems`` () =
        parseProblems example
        =! [| (4, 4, [| 0; 0; 0; 0; 2; 0 |])
              (12, 5, [| 1; 0; 1; 0; 2; 2 |])
              (12, 5, [| 1; 0; 1; 0; 3; 2 |]) |]

    [<Fact>]
    let ``Example`` () = part1 example =! "Fit: 0; MightFit: 3; DoesntFit: 0" // lol
