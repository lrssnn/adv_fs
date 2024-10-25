module adv_fs.Day1

open System.IO

type Day1() =
    let input = File.ReadAllText "input/input01"
    //let input = "91212129"

    let numList =
        Seq.map (fun c -> int (System.Char.GetNumericValue c)) (input.ToCharArray())
        |> Seq.toList

    let indices length offset =
        List.map (fun ind -> (ind, (ind + offset) % length)) [ 0 .. (length - 1) ]

    let solve (nums: int list) offset =
        List.map (fun (a, b) -> (nums[a], nums[b])) (indices nums.Length offset)
        |> List.filter (fun (a, b) -> a = b)
        |> List.sumBy fst

    interface Day with
        member this.DayName = "01"
        member this.answer1 = "1171"
        member this.answer2 = "1024"

        member this.part1 = string (solve numList 1)
        member this.part2 = string (solve numList (numList.Length / 2))
