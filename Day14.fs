module adv_fs.Day14

open Puz.Input
open Puz.Convenience
open FSharpx.String

type Vec2 =
    { x: int
      y: int }

    static member OfString input =
        let left, right = splitOnceStr "," input
        { x = int left; y = int right }

    member v.ToString = $"({v.x},{v.y})"

type Guard =
    { pos: Vec2
      vel: Vec2 }

    static member OfString input =
        let left, right = splitOnceStr " v=" input

        { pos = Vec2.OfString left[2..]
          vel = Vec2.OfString right }

    member g.ToString = $"p: {g.pos.ToString} | v: {g.vel.ToString}"
    member g.isAt(x, y) = g.pos.x = x && g.pos.y = y

type Day14() =
    let input = readText "input/input14.txt"

    let categorise position dimensions =
        let xMid = dimensions.x / 2
        let yMid = dimensions.y / 2

        let isLeft = position.x < xMid
        let isRight = position.x > xMid
        let isUp = position.y < yMid
        let isDown = position.y > yMid

        match (isLeft, isRight, isUp, isDown) with
        | true, false, true, false -> 0 // Up Left
        | false, true, true, false -> 1 // Up Right
        | true, false, false, true -> 2 // Down Left
        | false, true, false, true -> 3 // Down Right
        | _ -> 4 // not in a quadrant

    let stateToStr limits (guards: Guard array) =
        [ 0 .. limits.y ]
        |> List.map (fun y ->
            [ 0 .. limits.x ]
            |> List.map (fun x ->
                let g = guards |> Array.filter (fun guard -> guard.isAt (x, y)) |> Array.length
                if g = 0 then "." else string g)
            |> String.concat "")
        |> String.concat "\n"

    let hasEmptiesX limits guards threshold =
        [ 0 .. limits.x ]
        |> List.filter (fun x -> guards |> Array.forall (fun g -> g.pos.x <> x))
        |> List.length > threshold

    let hasEmptiesY limits guards threshold =
        [ 0 .. limits.y ]
        |> List.filter (fun y -> guards |> Array.forall (fun g -> g.pos.y <> y))
        |> List.length > threshold

    let evaluate limits guards =
        guards
        |> Array.countBy (fun g -> categorise g.pos limits)
        |> Array.filter (fun (group, _) -> not (group = 4))
        |> Array.map snd
        |> Array.fold (fun acc e -> acc * e) 1

    // Stolen :)
    let wrappedAdd v delta maxValue =
        let m = maxValue + 1
        let v2 = v + delta
        let v3 = v2 + (1 - v2 / m) * m
        v3 % m

    let moveGuard guard limits turns =
        { guard with
            pos =
                { x = wrappedAdd guard.pos.x (guard.vel.x * turns) limits.x
                  y = wrappedAdd guard.pos.y (guard.vel.y * turns) limits.y } }

    interface Day with
        member this.DayName = "14"
        member this.answer1 = "230436441"
        member this.answer2 = "8270"

        member this.part1() =
            let guards = input |> splitChar [| '\n' |] |> Array.map Guard.OfString
            let dimensions = { x = 101; y = 103 }

            let lim =
                { dimensions with
                    x = dimensions.x - 1
                    y = dimensions.y - 1 }

            guards |> Array.map (fun g -> moveGuard g lim 100) |> evaluate lim |> string

        member this.part2() =
            let guards = input |> splitChar [| '\n' |] |> Array.map Guard.OfString
            let dimensions = { x = 101; y = 103 }

            let lim =
                { dimensions with
                    x = dimensions.x - 1
                    y = dimensions.y - 1 }

            let mutable result = -1

            // TODO there must be more functional way to find the first time we satisfy the
            // condition, while folding...
            [ 1..10000 ]
            |> List.fold
                (fun guards turn ->
                    let newGuards = guards |> Array.map (fun g -> moveGuard g lim 1)
                    let emptyx = hasEmptiesX lim newGuards 5
                    let emptyy = hasEmptiesY lim newGuards 5

                    if emptyx && emptyy then
                        result <- turn

                    newGuards)
                guards
            |> ignore

            result |> string
