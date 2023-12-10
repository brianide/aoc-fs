module Scaffold.Image

open System.IO
open System.Text

/// Splits an integer into three byte values.
let intToRGB num =
    let red = (num >>> 16) &&& 0xFF |> byte
    let green = (num >>> 8) &&& 0xFF |> byte
    let blue = num &&& 0xFF |> byte
    (red, green, blue)

/// <summary>Render a grayscale PGM frame.</summary>
/// <param name="width">The width of the generated image.</param>
/// <param name="height">The height of the generated image.</param>
/// <param name="path">The output file path.</param>
/// <param name="byteFn">A function that accepts a row and column index and returns a one-byte grayscale value.</param>
let saveToPGM width height path byteFn =
    let header = $"P5 {width} {height} 255 "
    
    let body = Array.zeroCreate<byte> (ASCIIEncoding.ASCII.GetByteCount header + width * height)
    let offset = ASCIIEncoding.ASCII.GetBytes(header, body)

    for i in 0 .. width * height - 1 do
        let r, c = i / width, i % width
        body[i + offset] <- byteFn r c

    File.WriteAllBytes(path, body)

/// <summary>Render a color PPM frame.</summary>
/// <param name="width">The width of the generated image.</param>
/// <param name="height">The height of the generated image.</param>
/// <param name="path">The output file path.</param>
/// <param name="byteFn">A function that accepts a row and column index and returns a three-byte color value.</param>
let saveToPPM width height path byteFn =
    let header = $"P6 {width} {height} 255 "
    
    let body = Array.zeroCreate<byte> (ASCIIEncoding.ASCII.GetByteCount header + width * height * 3)
    let offset = ASCIIEncoding.ASCII.GetBytes(header, body)

    for i in 0 .. width * height - 1 do
        let r, c = i / width, i % width
        let (red, green, blue) = byteFn r c
        body[i * 3 + offset] <- red
        body[i * 3 + offset + 1] <- green
        body[i * 3 + offset + 2] <- blue

    File.WriteAllBytes(path, body)