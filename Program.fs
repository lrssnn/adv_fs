module adv

open System.Diagnostics
open adv_fs
open Day19

let validate expected actual : char = if expected = actual then '✓' else 'x'

let timedSolve f =
    let start = Stopwatch.StartNew()
    let ans = f ()
    (ans, start.Elapsed.TotalMilliseconds)

let formatOutput expected ans solveTime =
    $" {validate expected ans} %11.2f{solveTime} |"

let doDay (day: Day) : unit =
    printf $"|  {day.DayName} |"

    let (ans1, time1) = timedSolve day.part1
    printf $"{(formatOutput ans1 day.answer1 time1)}"

    let (ans2, time2) = timedSolve day.part2
    printf $"{(formatOutput ans2 day.answer2 time2)}"
    printfn $" 1: {ans1} 2: {ans2}"

let days: Day list =
    // [ new Day1()
    //   new Day2()
    //   new Day3()
    //   new Day4()
    //   new Day5()
    //   new Day6()
    //   new Day7()
    //   new Day8()
    //   new Day9()
    [ new Day19() ]


printfn "+-----+---------------+---------------+"
printfn "| Day |       1       |       2       |"
printfn "+-----+---------------+---------------+"

for day in days do
    doDay day

printfn "+-----+---------------+---------------+"
