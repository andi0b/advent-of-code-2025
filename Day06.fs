module aoc25.Day06

open System

let getColumns (lines: string array) =
    lines
    |> Array.map _.Split(' ', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.transpose

let part1 =
    let sumColumn (col: string array) =
        let op =
            match col[col.Length - 1] with
            | "*" -> (*)
            | "+" -> (+)
            | _ -> failwith "huh"

        col[.. col.Length - 2] |> Array.map int64 |> Array.reduce op

    getColumns >> Array.sumBy sumColumn

let part2 (lines: string array) =
    let transposed = lines |> Array.map _.ToCharArray() |> Array.transpose

    let chunks =
        transposed
        |> Array.unfold (function
            | [||] -> None
            | remaining ->
                match remaining |> Array.tryFindIndex (Array.forall ((=) ' ')) with
                | Some idx -> Some(remaining[.. idx - 1], remaining[idx + 1 ..])
                | None -> Some(remaining, [||]))

    chunks
    |> Array.sumBy (fun chunk ->
        let op =
            match chunk[0] |> Array.last with
            | '*' -> (*)
            | '+' -> (+)
            | c -> failwith $"invalid op '{c}'"

        chunk
        |> Array.map (fun chars -> chars[.. chars.Length - 2] |> String |> int64)
        |> Array.reduce op)

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [| //
           "123 328  51 64 "
           " 45 64  387 23 "
           "  6 98  215 314"
           "*   +   *   +  " |]

    [<Fact>]
    let ``get columns`` () =
        let columns = getColumns example
        columns[0] =! [| "123"; "45"; "6"; "*" |]
        columns[3] =! [| "64"; "23"; "314"; "+" |]


    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 4277556

    let example2 = null

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 3263827
