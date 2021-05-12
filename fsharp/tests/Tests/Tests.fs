// Copyright 2021 Lassi Kortela
// Copyright 2021 Oskar Gewalli
// SPDX-License-Identifier: ISC
module Tests

open System
open Xunit
open Pose


[<Theory;
  InlineData("(symbol \"value\")", "(symbol value)");
  InlineData("; Foo", "");
  InlineData("  ; Bar", "");
  InlineData("( 1 2  (|asdo\|aisdj| \"dfdosi dsi\"))", "(1 2 (asdo|aisdj dfdosi dsi))");
  InlineData("()", "()");
 >]
let ``Can parse and stringify`` (sample:string, expected:string) =
  let read = 
            use stream = new IO.MemoryStream()
            use rd = new IO.BinaryReader (stream)
            use w = new IO.StreamWriter (stream)
            w.Write sample
            w.Flush ()
            stream.Seek (0L, IO.SeekOrigin.Begin) |> ignore
            readAll rd
  let written =
            use stream = new IO.MemoryStream()
            use w = new IO.StreamWriter (stream)
            for e in read do write w e
            w.Flush ()
            stream.Seek (0L, IO.SeekOrigin.Begin) |> ignore
            use rd = new IO.StreamReader (stream)
            rd.ReadToEnd()
  Assert.Equal(expected, written)

[<Theory;
  InlineData("srfi.pose");
  InlineData("hello.pose")>]
let ``Can parse`` (file:string) =
  let read =
            use stream = IO.File.OpenRead file
            use rd = new IO.BinaryReader (stream)
            readAll rd
  let written =
            use stream = new IO.MemoryStream()
            use w = new IO.StreamWriter (stream)
            for e in read do writeln w e
            w.Flush ()
            stream.Seek (0L, IO.SeekOrigin.Begin) |> ignore
            use rd = new IO.StreamReader (stream)
            rd.ReadToEnd()
  () // question is what to assert here
  //let expected = IO.File.ReadAllText file
  //Assert.Equal(expected, written)

