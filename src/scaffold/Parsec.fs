module Scaffold.Parsec

open FParsec

let space: Parser<unit, unit> = skipMany (skipChar ' ')

let getParsed spec =
    run spec
    >> function
    | Success (res, _, _) -> res
    | Failure (msg, _, _) -> failwith msg