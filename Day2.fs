module adv_fs.Day2

open Puz.Input

type Day2() =
    let reports = readLinesAsInts "input/input02.txt" ' '

    // Validity rule on a report (a sequence of ints)
    // All differences must be between 1 and 3, in one direction for the whole list
    let satisfyTest (report: int list) =
        let parity = sign (report[1] - report[0])

        report
        |> List.pairwise
        |> List.forall (fun (a, b) ->
            let diff = b - a
            sign diff = parity && 1 <= abs diff && abs diff <= 3)

    // Check if the report is deemed safe. If there is grace (true), then we must try removing single item at
    // a time, if one of those is safe, then the report itself is safe
    let isSafe grace (report: int list) =
        satisfyTest report
        || (grace
            && [ 0 .. report.Length - 1 ]
               |> List.exists (fun remove -> satisfyTest (List.removeAt remove report)))

    let solve grace (reports: int list list) =
        reports |> List.filter (isSafe grace) |> List.length

    interface Day with
        member this.DayName = "02"
        member this.answer1 = "279"
        member this.answer2 = "343"

        member this.part1() = string (solve false reports)

        member this.part2() = string (solve true reports)
