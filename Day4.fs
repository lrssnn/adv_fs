module adv_fs.Day4

open Puz.Convenience
open Puz.Input

type Day4() =
    let input = readAsCharA2D (readText "input/input04.txt")

    // Look for an XMAS starting at (x,y) and moving in the direction defined by (dx, dy). Returns 1 if found, 0 if not
    let tryScore grid x y dX dY =
        if
            boundedCheck grid (x + 1 * dX) (y + 1 * dY) 'M'
            && boundedCheck grid (x + 2 * dX) (y + 2 * dY) 'A'
            && boundedCheck grid (x + 3 * dX) (y + 3 * dY) 'S'
        then
            1
        else
            0

    /// Assuming that grid[x,y] is an X, looks for XMAS in any direction, including diagonals starting from that X
    let score grid (x, y) =
        tryScore grid x y -1 1 // Diag up right
        + tryScore grid x y -1 0 // Straight up
        + tryScore grid x y -1 -1 // Diag up left
        +

        tryScore grid x y 0 1 // Straight right
        + tryScore grid x y 0 -1 // Straight left
        +

        tryScore grid x y 1 1 // Diag down right
        + tryScore grid x y 1 0 // Straight down
        + tryScore grid x y 1 -1 // Diag down left

    // TODO: Both these strategies could be expressed as a kind of convolution construct which might be useful to
    // have in the library. i.e. for this one you would say does it satisfy
    // [M;*;*]    [S;*;*]     [*;*;S]    [*;*;M]
    // [*;A;*] or [*;A;*] and [*;A;*] or [*;A;*]
    // [*;*;S]    [*;*;M]     [M;*;*]    [S;*;*]
    /// Assuming that grid[x,y] is 'A', looks for an X-shaped construct of MAS or SAM on each diagonal
    let score2 grid (x, y) : int =
        if
            ((boundedCheck grid (x - 1) (y - 1) 'S' && boundedCheck grid (x + 1) (y + 1) 'M')
             || (boundedCheck grid (x + 1) (y + 1) 'S' && boundedCheck grid (x - 1) (y - 1) 'M'))
            && ((boundedCheck grid (x - 1) (y + 1) 'S' && boundedCheck grid (x + 1) (y - 1) 'M')
                || (boundedCheck grid (x + 1) (y - 1) 'S' && boundedCheck grid (x - 1) (y + 1) 'M'))
        then
            1
        else
            0

    /// Find all instances of anchorChar in the grid, and then sum up the scores of each one according to the
    /// given strategy
    let solve grid anchorChar scoreStrategy =
        grid |> findAll anchorChar |> Seq.sumBy (scoreStrategy grid)

    interface Day with
        member this.DayName = "04"
        member this.answer1 = "2297"
        member this.answer2 = "1745"

        member this.part1() = string (solve input 'X' score)

        member this.part2() = string (solve input 'A' score2)
