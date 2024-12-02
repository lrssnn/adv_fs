module adv_fs.Day2

open System.IO
open FSharpx.String

type Day2() =
    let testInput =
        "7 6 4 2 1\n\
         1 2 7 8 9\n\
         9 7 6 2 1\n\
         1 3 2 4 5\n\
         8 6 4 4 1\n\
         1 3 6 7 9"

    let input = trim (File.ReadAllText "input/input02.txt")

    let reports input =
        input
        |> splitChar [| '\n' |]
        |> Array.map (splitChar [| ' ' |])
        |> Array.map (Array.map int)

    // TODO Figure out why this isn't enough
    // let rec differencesWithin grace min max (report: int array) =
    //     let parity = sign (report[1] - report[0])
    //
    //     report
    //     |> Array.indexed
    //     |> Array.pairwise
    //     |> Array.forall (fun ((i, a), (j, b)) ->
    //         let diff = b - a
    //         let valid = sign diff = parity && min <= abs diff && abs diff <= max
    //
    //         valid
    //         || (grace
    //             && (differencesWithin false min max (Array.removeAt i report)
    //                 || differencesWithin false min max (Array.removeAt j report))))

    let rec differencesWithin min max (report: int array) =
        let parity = sign (report[1] - report[0])

        report
        |> Array.indexed
        |> Array.pairwise
        |> Array.forall (fun ((i, a), (j, b)) ->
            let diff = b - a
            let valid = sign diff = parity && min <= abs diff && abs diff <= max

            valid)

    let isSafe grace (report: int array) =
        differencesWithin 1 3 report
        || (grace
            && [ 0 .. report.Length - 1 ]
               |> List.map (fun remove -> differencesWithin 1 3 (Array.removeAt remove report))
               |> List.exists id)

    let solve grace (reports: int array array) =
        reports |> Array.filter (isSafe grace) |> Array.length

    interface Day with
        member this.DayName = "02"
        member this.answer1 = "279"
        member this.answer2 = "343"

        member this.part1() = string (solve false (reports input))

        member this.part2() = string (solve true (reports input))
