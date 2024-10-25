open adv_fs
open Day1

let validate1 (day: Day) : char =
    if day.part1.Equals(day.answer1) then '✓' else 'x'

let validate2 (day: Day) : char =
    if day.part2.Equals(day.answer2) then '✓' else 'x'

let doDay (day: Day) : unit =
    printfn $"{day.DayName}: {validate1 day} {validate2 day} {day.part1} {day.part2}"

let d1 = new Day1()
doDay d1
