module adv_fs.Day2

open System.IO
open System.Linq


type Day2() =
    let input = File.ReadAllText "input/input02"
    //let input = "5\t1\t9\t5\n7\t5\t3\n2\t4\t6\t8"
    //let input = "5\t9\t2\t8\n9\t4\t7\t3\n3\t8\t6\t5"
    let numList = Input.readLinesAsInts input '\t'

    let solveLine (line: int list) = line.Max() - line.Min()

    // Some(x / y) if it is a whole number, otherwise None
    let tryWholeDivide x y = if x % y = 0 then Some(x / y) else None

    let solveTwo (line: int list) =
        // get a list of all the possible combinations of the list elements
        List.allPairs line line
        // exclude the pairs where the item has been paired with itself
        |> List.filter (fun (x, y) -> x <> y)
        // find the first item which results in a whole division, correcting for ordering
        |> List.pick (fun (x, y) -> if y > x then tryWholeDivide y x else tryWholeDivide x y)

    let solve (nums: int list list) lineSolver = List.sumBy lineSolver nums

    interface Day with
        member this.DayName = "02"
        member this.answer1 = "42299"
        member this.answer2 = "277"

        member this.part1() = string (solve numList solveLine)
        member this.part2() = string (solve numList solveTwo)
