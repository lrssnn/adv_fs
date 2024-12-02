module adv_fs.Day3


type Day3() =
    let input = 289326

    let ringNum index =
        index, (if index = 1 then 0 else (index / 8) + 1)

    interface Day with
        member this.DayName = "03"
        member this.answer1 = "?"
        member this.answer2 = "?"

        member this.part1() =
            Seq.init 10 (fun index -> index + 1)
            |> Seq.map ringNum
            |> Seq.map (fun (index, ring) -> printfn "%i: %i" index ring)
            |> Seq.map string
            |> String.concat ","

        member this.part2() = string (1)
