module aoc25.Day10

open System.Collections.Generic
open System.Numerics
open System.Runtime.Intrinsics
open System.Runtime.Intrinsics.X86
open FSharp.HashCollections
open FSharp.Text.RegexProvider

type LineRegex = Regex< @"\[(?<ind>.*)\] (?<but>.*) \{(?<jolt>.*)}" >


type Machine =
    { buttons: int array array
      indicator: int
      jolts: int32 array }

let parse =
    let parseLine line =
        let matches = LineRegex().TypedMatch(line)

        let indicator =
            let binaryString =
                matches.ind.Value.Replace('.', '0').Replace('#', '1')
                |> Seq.rev
                |> Seq.toArray
                |> System.String

            System.Convert.ToInt32(binaryString, 2)

        let parseButton (input: string) =
            input.Trim('(', ')', ' ') |> StringEx.splitC ',' |> Array.map int32

        { indicator = indicator
          buttons = matches.but.Value |> StringEx.splitC ' ' |> Array.map parseButton
          jolts = matches.jolt.Value.Trim('{', '}') |> StringEx.splitC ',' |> Array.map int32 }

    Array.map parseLine

let inline allCombinations list =
    let rec comb k =
        function
        | _ when k = 0 -> [ [] ]
        | [] -> []
        | x :: xs ->
            let withX = comb (k - 1) xs |> List.map (fun ys -> x :: ys)
            let withoutX = comb k xs
            withX @ withoutX

    seq {
        for i = 1 to list |> List.length do
            yield! comb i list
    }

let part1 input =
    let xorButtons = Seq.reduce (fun state button -> state ^^^ button)
    let binaryValue = Array.fold (fun state value -> (1 <<< value) ||| state) 0

    let minButtonPresses machine =
        machine.buttons
        |> Array.map binaryValue
        |> Array.toList
        |> allCombinations
        |> Seq.find (fun buttons -> xorButtons buttons = machine.indicator)
        |> List.length

    parse input |> Array.sumBy minButtonPresses

let toRightAlignedVector (input: 'T[]) =
    let n = Vector<'T>.Count

    if input.Length > n then
        failwith $"Vector<{typeof<'T>.Name}> doesn't support more than {n} elements but {input.Length} were provided"

    let tmp = Array.zeroCreate<'T> n
    let offset = n - input.Length

    for i = 0 to input.Length - 1 do
        tmp.[offset + i] <- input.[i]

    Vector<'T>(tmp)

let getPattern (vec: inref<Vector<uint16>>) =
    let vec' = vec.AsVector256()
    let sign = Avx2.ShiftLeftLogical(vec', 15uy)
    sign.AsSByte() |> Avx2.MoveMask

let part2 =
    let minButtonPresses machine =

        let offset = Vector<uint16>.Count - machine.jolts.Length

        let patterns =
            machine.buttons
            |> Seq.map (fun b ->
                let tmp = Array.zeroCreate<uint16> Vector<uint16>.Count
                for i in b do
                     tmp[offset + i] <- 1us

                Vector<uint16>(tmp))
            |> Seq.toList
            |> allCombinations
            |> Seq.map (fun buttons ->
                let mutable sum = Vector<uint16>.Zero
                for b in buttons do
                    sum <- sum + b

                let count = buttons |> List.length
                let pattern = getPattern &sum
                pattern, (sum, count))
            |> Seq.append (seq { 0, (Vector<uint16>.Zero, 0) })
            |> Seq.groupBy fst
            |> Seq.map (fun (k, v) -> KeyValuePair.Create(k, v |> Seq.map snd |> Seq.toArray))
            |> HashMap.ofSeq

        let rec solveStep target =
            if target = Vector<uint16>.Zero then
                ValueSome 0
            else
                match patterns |> HashMap.tryFind (getPattern &target) with
                | ValueSome pattern ->
                    seq {
                        for sum, count in pattern do
                            if Vector.GreaterThanOrEqualAll(target, sum) then
                                match (target - sum) / Vector<uint16>(2us) |> solveStep with
                                | ValueSome x -> yield x * 2 + count
                                | _ -> ()
                    }
                    |> Seq.fold
                        (fun s n ->
                            match s with
                            | ValueNone -> ValueSome n
                            | ValueSome sm -> min sm n |> ValueSome)
                        ValueNone
                | ValueNone -> ValueNone

        let target = machine.jolts |> Array.map uint16 |> toRightAlignedVector
        solveStep target |> ValueOption.get

    parse >> Array.sumBy minButtonPresses

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [| "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
           "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
           "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 7


    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 33
