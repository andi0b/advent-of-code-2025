module aoc25.Day01

open System

let parseCommand (command: string) =
    command[1..] |> int |> (if command[0] = 'L' then (~-) else id)

let turnDial start command = (100 + start + command % 100) % 100

let part1 commands =
    let intermediates = commands |> Array.map parseCommand |> Array.scan turnDial 50
    intermediates |> Seq.filter (fun i -> i = 0) |> Seq.length

let part2 commands =
    ((0, 50), commands |> Array.map parseCommand)
    ||> Array.fold (fun (zeros, position) command ->
        let fullRotations = command / 100 |> Math.Abs

        let overflow =
            match position, position + (command % 100) with
            | 0, _ -> 0
            | _, p when p <= 0 || p >= 100 -> 1
            | _ -> 0

        zeros + fullRotations + overflow, turnDial position command)
    |> fst

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [| "L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 3

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 6
