open System
open System.Diagnostics
open System.IO
open System.Net.Http
open System.Reflection
open FSharp.Text.RegexProvider
open Spectre.Console
open aoc25

[<assembly: Xunit.CaptureTrace>]
[<assembly: Xunit.CaptureConsole>]
do ()

let year = "2025"
let inputDirectory = "inputs"

let inputPath day =
    Path.Combine(inputDirectory, $"day%02u{day}.txt")

module Con =
    type MyChristmasSpinner() =
        inherit Spinner()
        override this.Frames = [ "⣷ 🌲"; "⣯ 🌲"; "⣟ 🌲"; "⡿ 🌲"; "⢿ 🎄"; "⣻ 🎄"; "⣽ 🎄"; "⣾ 🎄" ]
        override this.Interval = TimeSpan.FromMilliseconds 100L
        override this.IsUnicode = true

    let printHeader title =
        AnsiConsole.MarkupLine ""
        AnsiConsole.MarkupLine ""
        FigletText($"Advent of Code {year}").Color(Color.Yellow) |> AnsiConsole.Write
        Rule("🎄 " + title).LeftJustified().RuleStyle("red") |> AnsiConsole.Write

    let printResultTable day (part1Result, part1Ms) (part2Result, part2Ms) =
        let table =
            Table()
                .Title($"[bold red]D[/][bold gold1]a[/][bold green]y[/] [bold gold1]{day}[/]")
                .RoundedBorder()
                .HideHeaders()
                .AddColumn(TableColumn("").Width(12))
                .AddColumn(TableColumn("Result").Width(60))
                .AddColumn(TableColumn("Time").RightAligned().Width(12))
                .AddRow("[bold silver]Part 1[/]", part1Result, part1Ms |> sprintf "%d [dim]ms[/]")
                .AddRow("[bold gold1]Part 2[/]", part2Result, part2Ms |> sprintf "%d [dim]ms[/]")

        AnsiConsole.Write table

let downloadAllDays maxDay =

    let dotEnv path =
        if File.Exists path then
            let vars =
                File.ReadAllLines(path)
                |> Array.map (StringEx.splitC '=')
                |> Array.choose (function
                    | [| key; value |] -> Some(key, value)
                    | _ -> None)

            for key, value in vars do
                Environment.SetEnvironmentVariable(key, value)

    dotEnv ".env"
    Directory.CreateDirectory(inputDirectory) |> ignore

    let httpClient =
        let handler = new HttpClientHandler(UseCookies = false)
        let client = new HttpClient(handler)
        let sessionKey = Environment.GetEnvironmentVariable("AOC_SESSION")
        client.DefaultRequestHeaders.Add("Cookie", $"session={sessionKey}")
        client

    let downloadInput day =
        task {
            let! response = httpClient.GetAsync($"https://adventofcode.com/{year}/day/{day}/input")
            response.EnsureSuccessStatusCode |> ignore
            use stream = response.Content.ReadAsStream()
            use fileStream = File.OpenWrite(inputPath day)
            stream.CopyTo(fileStream)
        }

    AnsiConsole
        .Status()
        .StartAsync(
            "preparing inputs...",
            fun ctx ->
                task {
                    if Environment.GetEnvironmentVariable("AOC_SESSION") |> String.IsNullOrWhiteSpace then
                        AnsiConsole.MarkupLine "Skipping input download, because AOC_SESSION is unset"
                    else
                        for day = 1 to maxDay do
                            if not (File.Exists(inputPath day)) then
                                ctx.Status <- $"Downloading input for Day {day}"
                                do! downloadInput day
                                AnsiConsole.MarkupLineInterpolated $"Input for Day {day} [green]downloaded[/]"

                        AnsiConsole.MarkupLine ""
                }
        )

type DayRegex = Regex< @"Day(?<num>\d+)" >

let days =

    let dayModules =
        Assembly.GetExecutingAssembly().GetTypes()
        |> Array.filter (fun t ->
            t.IsClass
            && t.IsAbstract
            && t.IsSealed
            && t.IsPublic
            && (t.DeclaringType = null)) // Ensure it's a top-level module
        |> Array.choose (fun t ->
            DayRegex().TryTypedMatch(t.Name)
            |> Option.map (fun m -> (m.num.Value |> int, t)))
        |> Map

    let createDay i =
        dayModules
        |> Map.tryFind i
        |> Option.map (fun t ->

            let propertyMethodInfo =
                t.GetMethod("get_run", BindingFlags.Public ||| BindingFlags.Static)

            let functionMethodInfo =
                t.GetMethod("run", BindingFlags.Public ||| BindingFlags.Static)

            if (propertyMethodInfo = null && functionMethodInfo = null) then
                failwith $"Type {t.Name} doesn't have a method 'run'"

            if (propertyMethodInfo <> null) then
                (fun () ->
                    let run =
                        propertyMethodInfo.Invoke(null, [||]) :?> (string -> ((unit -> string) * (unit -> string)))

                    run (inputPath i))
            else
                (fun () ->
                    functionMethodInfo.Invoke(null, [| inputPath i |]) :?> ((unit -> string) * (unit -> string))))

        |> Option.defaultValue (fun () -> (fun () -> "skipped"), (fun () -> "skipped"))

    let maxDay = dayModules |> Map.keys |> Seq.max
    [ 1..maxDay ] |> List.map createDay


let runDays days =
    let runPart partImpl =
        let sw = Stopwatch.StartNew()
        let result = partImpl ()
        sw.Stop()
        result, sw.ElapsedMilliseconds

    AnsiConsole
        .Status()
        .Spinner(Con.MyChristmasSpinner())
        .Start(
            "running",
            fun ctx ->
                for day, impl in days do

                    AnsiConsole.WriteLine ""
                    AnsiConsole.WriteLine ""
                    AnsiConsole.WriteLine ""
                    ctx.Status <- $"[bold white]Running Day[/] [bold gold1]{day}[/] [bold silver]Part 1[/]"

                    let part1Impl, part2Impl = impl ()
                    let part1 = runPart part1Impl

                    ctx.Status <- $"[bold white]Running Day[/] [bold gold1]{day}[/] [bold gold1]Part 2[/]"
                    let part2 = runPart part2Impl

                    Con.printResultTable day part1 part2
        )

let runAll () =
    days
    |> List.indexed
    |> List.map (fun (day, impl) -> day + 1, impl)
    |> List.rev
    |> runDays

open TryParser

[<EntryPoint>]
let Main args =
    (downloadAllDays days.Length).Wait()

    match args with
    | [| Int day |] when day > 0 && day <= 25 ->

        match days |> List.tryItem (day - 1) with
        | Some implementation ->
            Con.printHeader $"[gold1]Running[/] [gold1 bold]day {day}[/]"
            runDays [ (day, implementation) ]
            0

        | None ->
            Con.printHeader $"[red bold]Could not find an implementation for day {day}[/]"
            1

    | [| "latest" |] when days.Length > 0 ->
        Con.printHeader $"[gold1]Running[/] [gold1 bold]latest[/] [gold1](day {days.Length})[/]"
        runDays [ (days.Length, days |> List.last) ]
        0

    | [||] ->
        Con.printHeader $"[gold1]Running[/] [gold1 bold]all days[/] [gold1](1 - {days.Length})[/]"
        runAll ()
        0

    | _ ->
        Con.printHeader $"[red bold]Expect either a number, 'latest' or no parameters[/]"
        1
