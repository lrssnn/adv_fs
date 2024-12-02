module adv_fs.Day1

open System.IO

type Day1() =
    let input = File.ReadAllText "input/input01"
    //let input = "91212129"

    let numList = Input.readCharsToNums input

    /// Generate a list of index pairs, comparing each item with the item 'offset' away
    /// treating the list as circular
    let indices length offset =
        List.map (fun ind -> (ind, (ind + offset) % length)) [ 0 .. (length - 1) ]

    let solve (nums: int list) offset =
        // Use the indices list and use those indices to get a list of pairs of list items we want to compare
        List.map (fun (a, b) -> (nums[a], nums[b])) (indices nums.Length offset)
        // Only keep the pairs where the two elements match
        |> List.filter (fun (a, b) -> a = b)
        // Sum by the element (the two elements are the same, just take the first)
        |> List.sumBy fst

    interface Day with
        member this.DayName = "01"
        member this.answer1 = "1171"
        member this.answer2 = "1024"

        member this.part1() = string (solve numList 1)

        member this.part2() =
            string (solve numList (numList.Length / 2))
