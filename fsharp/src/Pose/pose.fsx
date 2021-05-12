// Copyright 2021 Lassi Kortela
// Copyright 2021 Oskar Gewalli
// SPDX-License-Identifier: ISC
#load "Library.fs"
open System
open Pose
let testFile (filename: string)=
     use file = IO.File.OpenRead(filename)
     use rd = new IO.BinaryReader (file)
     try
         let read= readAll(rd)
         printfn "Read %s" filename
     with
     | e ->
         printfn "Failed to read %s" filename
         printfn "Got the error %A" e
testFile(IO.Path.Combine(__SOURCE_DIRECTORY__, "..","..","..", "examples","hello.pose"))
testFile(IO.Path.Combine(__SOURCE_DIRECTORY__, "..","..","..", "examples","srfi.pose"))
