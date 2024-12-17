module adv_fs.Day14

open Puz.Input
open Puz.Convenience
open FSharpx.String

type Vec2 =
    { mutable x: int
      mutable y: int }

    static member OfString input =
        let (left, right) = splitOnceStr "," input
        { x = int left; y = int right }

    static member wrappedAdd pos vel limits turns =
        let mutable x = (pos.x + (turns * vel.x)) % limits.x
        let mutable y = (pos.y + (turns * vel.y)) % limits.y

        if x < 0 then
            x <- x + (((-x / limits.x) + 1) * limits.x)

        if y < 0 then
            y <- y + (((-y / limits.y) + 1) * limits.y)

        { x = x; y = y }

    member v.ToString = $"({v.x},{v.y})"

type Guard =
    { mutable pos: Vec2
      vel: Vec2 }

    static member OfString input =
        let (left, right) = splitOnceStr " v=" input

        { pos = Vec2.OfString left[2..]
          vel = Vec2.OfString right }

    member g.ToString = $"p: {g.pos.ToString} | v: {g.vel.ToString}"
    member g.isAt(x, y) = g.pos.x = x && g.pos.y = y

    member g.Update limits turns =
        g.pos <- Vec2.wrappedAdd g.pos g.vel limits turns

type Day14() =
    let input = readText "input/input14.txt"

    let testInput =
        "p=0,4 v=3,-3\n\
        p=6,3 v=-1,-3\n\
        p=10,3 v=-1,2\n\
        p=2,0 v=2,-1\n\
        p=0,0 v=1,3\n\
        p=3,0 v=-2,-2\n\
        p=7,6 v=-1,-3\n\
        p=3,0 v=-1,-2\n\
        p=9,3 v=2,3\n\
        p=7,3 v=-1,2\n\
        p=2,4 v=2,-3\n\
        p=9,5 v=-3,-3"

    let stateToStr limits (guards: Guard array) =
        [ 0 .. (limits.y - 1) ]
        |> List.map (fun y ->
            [ 0 .. (limits.x - 1) ]
            |> List.map (fun x ->
                let g = guards |> Array.filter (fun guard -> guard.isAt (x, y)) |> Array.length
                if g = 0 then "." else string g)
            |> String.concat "")
        |> String.concat "\n"

    let evaluate limits guards =
        let xMid = limits.x / 2
        let yMid = limits.y / 2

        guards
        |> Array.countBy (fun g ->
            let isLeft = g.pos.x <= xMid
            let isRight = g.pos.x > xMid + 1
            let isUp = g.pos.y <= yMid
            let isDown = g.pos.y > yMid + 1

            match (isLeft, isRight, isUp, isDown) with
            | (true, false, true, false) -> 0 // Up Left
            | (false, true, true, false) -> 1 // Up Right
            | (true, false, false, true) -> 2 // Down Left
            | (false, true, false, true) -> 3 // Down Right
            | _ -> 4 // not in a quadrant
        )
        |> Array.filter (fun (group, _) -> not (group = 4))
        |> Array.map snd
        |> Array.fold (fun acc e -> acc * e) 1

    interface Day with
        member this.DayName = "14"
        member this.answer1 = "?"
        member this.answer2 = "?"

        member this.part1() =
            let guards = input |> splitChar [| '\n' |] |> Array.map Guard.OfString
            //let lim = { x = 11; y = 7 }
            let lim = { x = 101; y = 103 }

            // [ 1..100 ]
            // |> List.iter (fun _turn -> guards |> Array.iter (fun g -> g.Update lim))

            guards |> Array.iter (fun g -> g.Update lim 100)

            printfn "\n%s" (stateToStr lim guards)

            evaluate lim guards |> string
            // Should be > 220983048


        member this.part2() = "!"
