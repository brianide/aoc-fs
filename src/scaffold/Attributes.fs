module Scaffold.Attributes

open System

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type SolutionAttribute(year: string, id: string, name: string) =
    inherit Attribute()
    member val Year = year
    member val Id = id
    member val Name = name