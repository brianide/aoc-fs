module Year2020.Day11

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

let parse path =
    File.ReadAllLines path
    |> Array.map Seq.toArray
    |> array2D

let nextGrid neighborFn diePop grid =
    let mutable changed = 0
    let initer r c =
        let oldValue = Array2D.get grid r c
        let newValue =
            match oldValue, neighborFn grid r c with
            | 'L', n when n = 0 -> '#'
            | '#', n when n >= diePop -> 'L'
            | s, _ -> s
        if newValue <> oldValue then
            changed <- changed + 1
        newValue

    Array2D.init (Array2D.length1 grid) (Array2D.length2 grid) initer
    |> fun g -> g, changed

let formatGrid (grid: char[,]) =
    let sb = System.Text.StringBuilder()
    for r in 0 .. Array2D.length1 grid - 1 do
        for c in 0 .. Array2D.length2 grid - 1 do
            sb.Append(grid[r,c]) |> ignore
        sb.AppendLine() |> ignore
    sb.ToString()

module Silver =
    let neighbors grid r c =
        let rows, cols = Array2D.length1 grid, Array2D.length2 grid
        seq {
            for rc in -1..1 do
                for cc in -1..1 do
                    if rc + rc + cc <> 0 then
                        let kr, kc = r + rc, c + cc
                        if kr >= 0 && kr < rows && kc >= 0 && kc < cols then
                            yield kr, kc
        }
        |> Seq.filter (fun (kr, kc) -> grid[kr,kc] = '#')
        |> Seq.length    

    let solve input =
        let rec recur grid =
            match nextGrid neighbors 4 grid with
            | grid, 0 -> Array2D.fold (fun acc c -> if c = '#' then acc + 1 else acc) 0 grid
            | grid, _ -> recur grid
        input
        |> recur
        |> sprintf "%i"

module Gold =
    let neighbors (grid: char[,]) r c =
        let rows, cols = Array2D.length1 grid, Array2D.length2 grid
        let mutable count = 0
        for rc in -1..1 do
            for cc in -1..1 do
                if rc + rc + cc <> 0 then
                    let mutable kr, kc = r + rc, c + cc
                    let mutable found = false
                    while not found && kr >= 0 && kr < rows && kc >= 0 && kc < cols do
                        if grid[kr,kc] = '#' then
                            count <- count + 1
                            found <- true
                        else if grid[kr,kc] = 'L' then
                            found <- true
                        else
                            kr <- kr + rc
                            kc <- kc + cc
        count

    let solve input =
        let rec recur grid =
            match nextGrid neighbors 5 grid with
            | grid, 0 -> Array2D.fold (fun acc c -> if c = '#' then acc + 1 else acc) 0 grid
            | grid, _ -> recur grid
        input
        |> recur
        |> sprintf "%i"

[<Solution("2020", "11", "Seating System")>]
let Solver = chainFileHandler parse Silver.solve Gold.solve