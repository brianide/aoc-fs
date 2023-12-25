module Year2023.Day24

open System.IO
open FParsec
open Scaffold.Parsec
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions

let parse =
    let vec = tuple3 (pfloat .>> skipString "," .>> spaces) (pfloat .>> skipString "," .>> spaces) pfloat
    let stone = tuple2 (vec .>> skipString " @" .>> spaces) vec .>> newline

    File.ReadAllText
    >> getParsed (many1 stone)

let inline vec2 (x, y, _) = x, y
let inline vadd (x, y) (a, b) = x + a, y + b
let inline vmul t (x, y) = x * t, y * t
let isFinite = System.Double.IsFinite

let solveSilver input =
    let lo, hi = 200000000000000.0, 400000000000000.0
    let inside (x, y) = lo <= x && x <= hi && lo <= y && y <= hi

    let intersect (((x1, y1), vel1), ((x3, y3), vel3)) =
        let x2, y2 = vadd (x1, y1) vel1
        let x4, y4 = vadd (x3, y3) vel3
        let t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4))
        let u = ((x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2)) / ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4))
        if t > 0.0 && u > 0.0 && isFinite t && isFinite u then
            vmul t vel1
            |> vadd (x1, y1)
            |> function
            | (x, y) when inside (x, y) -> Some (x, y)
            | _ -> None
        else
            None
    
    let input2d = List.map (fun (pos, vel) -> vec2 pos, vec2 vel) input
    [ for list in Seq.tails input2d do
        for item in Seq.tail list do
            Seq.head list, item ]
    |> List.choose intersect
    |> List.length
    |> sprintf "%A"

let solveGold input =
    let intDec = sprintf "(declare-const %s Int)"
    let tDec = sprintf "(declare-const t%d Int)" 
    [ for lbl in  ["pxi"; "pyi"; "pzi"; "vxi"; "vyi"; "vzi"] do yield intDec lbl
      for ind in [0 .. List.length input - 1] do yield tDec ind
      for (ind, ((px, py, pz), (vx, vy, vz))) in List.indexed input do
        let tv = sprintf "t%d" ind
        let x, y, z = int64 (px + vx), int64 (py + vy), int64 (pz + vz)
        yield sprintf "(assert (= (* %s %d) (* %s (+ %s %s))))" tv x tv "pxi" "vxi"
        yield sprintf "(assert (= (* %s %d) (* %s (+ %s %s))))" tv y tv "pyi" "vyi"
        yield sprintf "(assert (= (* %s %d) (* %s (+ %s %s))))" tv z tv "pzi" "vzi"
      yield "(check-sat)"
      yield "(get-model)" ]
    |> String.concat "\n"

[<Solution("2023", "24", "Never Tell Me The Odds")>]
let Solver = chainFileHandler parse solveSilver solveGold