module Year2020.Day14

open System.IO
open System.Numerics
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions
open Scaffold.Util.Patterns

type Instruction =
| SetMask of uint64 * uint64
| Write of uint64 * uint64

module Silver =
    let parse path =
        let rec parseMask mask value = function
        | 'X' :: rest -> parseMask (mask <<< 1) (value <<< 1) rest
        | '1' :: rest -> parseMask (mask <<< 1 ||| 1UL) (value <<< 1 ||| 1UL) rest
        | '0' :: rest -> parseMask (mask <<< 1 ||| 1UL) (value <<< 1) rest
        | [] -> SetMask (mask, value)
        | c :: _ -> failwithf "Invalid character: %A" c

        let parseLine = function
        | RegGroups @"^mask = ([01X]+)$" [mask] -> Seq.toList mask |> parseMask 0UL 0UL
        | RegGroups @"^mem\[(\d+)] = (\d+)$" [TryParse uint64 addr; TryParse uint64 value] -> Write (addr, value)
        | x -> failwithf "Invalid line: %s" x

        File.ReadAllLines path
        |> Seq.map parseLine
        |> Seq.toList

    let solve path =
        let folder ((mask, value), mem) = function
        | SetMask (mask, value) -> ((mask, value), mem)
        | Write (addr, v) -> ((mask, value), Map.add addr ((mask &&& value) ||| (~~~mask &&& v)) mem)
        
        parse path
        |> List.fold folder ((0UL, 0UL), Map.empty)
        |> snd
        |> Map.values
        |> Seq.reduce (+)
        |> sprintf "%i"

module Gold =
    let parse path =
        let rec parseMask owrt flot  = function
        | 'X' :: rest -> parseMask (owrt <<< 1) (flot <<< 1 ||| 1UL) rest
        | '1' :: rest -> parseMask (owrt <<< 1 ||| 1UL) (flot <<< 1) rest
        | '0' :: rest -> parseMask (owrt <<< 1) (flot <<< 1) rest
        | [] -> SetMask (owrt, flot)
        | c :: _ -> failwithf "Invalid character: %A" c

        let parseLine = function
        | RegGroups @"^mask = ([01X]+)$" [mask] -> Seq.toList mask |> parseMask 0UL 0UL
        | RegGroups @"^mem\[(\d+)] = (\d+)$" [TryParse uint64 addr; TryParse uint64 value] -> Write (addr, value)
        | x -> failwithf "Invalid line: %s" x

        File.ReadAllLines path
        |> Seq.map parseLine
        |> Seq.toList

    let rec genAddrs (baseAddr: uint64) (mask: uint64) prev =
        let count = BitOperations.TrailingZeroCount mask
        if (count > 36) then
            Seq.singleton baseAddr
        else
            let nextMask = mask >>> (count + 1)
            let off = count + prev
            Seq.concat [
                genAddrs baseAddr nextMask (off + 1)
                genAddrs (baseAddr ^^^ (1UL <<< (off + prev))) nextMask (off + 1)
            ]
    
    let solve path =
        let folder ((owrt, flot), mem) = function
        | SetMask (owrt, flot) ->
            ((owrt, flot), mem)
        | Write (addr, v) ->
            let mem =
                genAddrs (owrt ||| addr) flot 0
                |> Seq.fold (fun acc m -> Map.add m v acc) mem
            ((owrt, flot), mem)
        
        parse path
        |> List.fold folder ((0UL, 0UL), Map.empty)
        |> snd
        |> Map.values
        |> Seq.reduce (+)
        |> sprintf "%i"

[<Solution("2020", "14", "Docking Data")>]
let Solver = simpleFileHandler Silver.solve Gold.solve