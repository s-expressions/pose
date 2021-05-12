// Copyright 2021 Lassi Kortela
// Copyright 2021 Oskar Gewalli
// SPDX-License-Identifier: ISC
module Pose

open System

exception SyntaxError of string;

type Exp
  = EList   of Exp list
  | ESymbol of string
  | EString of string
  | EReal   of float //real
  | EInt    of int
  | EIntInf of int64 //IntInf.int;
module internal String=
  let implode (c:char list) = String (List.toArray c)

let stringContainsChar (s:string) (goalChar:char) = s.Contains goalChar

let charIsWhitespace char = Char.IsWhiteSpace char

let charIsAlphabetic char = Char.IsLetter char

let charIsNumeric char = Char.IsNumber char

let charIsTokenCommon char =
    ((charIsAlphabetic char) ||
     (charIsNumeric char) ||
     (stringContainsChar "_$!?<=>+-*" char));

let charIsTokenFirst char =
    ((charIsTokenCommon char) ||
     (stringContainsChar "/" char));

let charIsTokenNext char =
    ((charIsTokenFirst char) ||
     (stringContainsChar ".@~^%&" char));

let parseNumberOrSymbol string =
    ESymbol string;

module internal TextIO=
  open System.IO
  let input1 (br:BinaryReader) = if br.BaseStream.Position < br.BaseStream.Length then Some (br.ReadChar()) else None;
  let lookahead (br:BinaryReader) =
    if br.BaseStream.Position < br.BaseStream.Length
    then
      let originalPosition = br.BaseStream.Position in
      let read = br.ReadChar()
      br.BaseStream.Position <- originalPosition
      Some read
    else None
  let output (s:StreamWriter,s1:string) = s.Write s1
  let output1 (s:StreamWriter,s1:char)= s.Write s1
let rec skipRestOfLine stream =
    match TextIO.input1 stream with
      | None -> ()
      | Some '\n' -> ()
      | Some _ -> skipRestOfLine stream

let rec skipWhitespaceAndComments stream =
    match TextIO.lookahead stream with
      | None -> ()
      | Some ';' -> (skipRestOfLine stream;
                      skipWhitespaceAndComments stream)
      | Some char -> if charIsWhitespace char then
                         (TextIO.input1 stream;
                          skipWhitespaceAndComments stream)
                     else
                         ();

let readRestOfTokenAsString char stream =
    let rec loop chars = match TextIO.lookahead stream with
                           | None -> chars
                           | Some char -> if charIsTokenNext char then
                                              (TextIO.input1 stream;
                                               loop (char :: chars))
                                          else
                                              chars
    in String.implode (List.rev (loop []))

let readTokenAsString stream =
    match TextIO.input1 stream with
     | None -> raise (SyntaxError "End-of-file while expecting token")
     | Some char -> if charIsTokenFirst char then
                         raise (SyntaxError "Not a token first char")
                     else
                         readRestOfTokenAsString char stream;

let readIntegerRadix radix stream =
    ESymbol (readTokenAsString stream);

let readSharpsign stream =
    match TextIO.input1 stream with
      | None -> raise (SyntaxError "End-of-file while reading #")
      | Some 'b' -> readIntegerRadix 2 stream
      | Some 'o' -> readIntegerRadix 8 stream
      | Some 'x' -> readIntegerRadix 16 stream
      | Some char -> raise (SyntaxError "Unknown # character")

let readStringEscape endChar stream =
    match TextIO.input1 stream with
      | None -> raise (SyntaxError "Unterminated string escape")
      | Some 'n' -> '\n'
      | Some 't' -> '\t'
      | Some char -> if (char = '\\') || (char = endChar) then
                         char
                     else
                         raise (SyntaxError "Unknown string escape")

let readDelimitedString endChar stream =
    let rec loop chars =
            match TextIO.input1 stream with
              | None -> raise (SyntaxError "Unterminated string")
              | Some char -> if char = endChar then
                                 chars
                             else
                                 loop ((if char = '\\' then
                                            readStringEscape endChar stream
                                        else
                                            char)
                                       :: chars)
    in String.implode (List.rev (loop []))

let private read1 readList stream =
    (skipWhitespaceAndComments stream;
     match TextIO.lookahead stream with
       | None -> None
       | Some char ->
         Some (if charIsTokenFirst char then
                   parseNumberOrSymbol (readRestOfTokenAsString char stream)
               else
                   (TextIO.input1 stream;
                    match char with
                      | '"' -> EString (readDelimitedString char stream)
                      | '|' -> ESymbol (readDelimitedString char stream)
                      | '#' -> readSharpsign stream
                      | '(' -> readList stream
                      | ')' -> raise (SyntaxError "Stray closing parenthesis")
                      | _ -> raise (SyntaxError
                                   "Unknown character at top level"))))
let rec readList stream =
    let rec loop forms =
            (skipWhitespaceAndComments stream;
             match TextIO.lookahead stream with
               | Some ')' -> (TextIO.input1 stream; forms)
               | _ -> match read1 readList stream with
                       |  None -> raise (SyntaxError "Unterminated list")
                       |  Some form -> loop (form :: forms))
    in EList (List.rev (loop []))

let read s = read1 readList s

let readAll stream =
    let rec loop forms =
            match read stream with
             |  None -> List.rev forms
             |  Some form -> loop (form :: forms)
    in loop []

let rec write stream form =
    match form with
      | EList [] -> TextIO.output (stream, "()")
      | EList forms -> (let rec loop prefix =
                              function
                              | [] ->
                                TextIO.output1 (stream, ')')
                              | (form :: forms) ->
                                (TextIO.output1 (stream, prefix);
                                 write stream form;
                                 loop ' ' forms)
                        in loop '(' forms )
      | ESymbol s   -> TextIO.output (stream, s)
      | EString s   -> TextIO.output (stream, s)
      | EReal n     -> TextIO.output (stream, (string n))
      | EInt n      -> TextIO.output (stream, (string n))
      | EIntInf n   -> TextIO.output (stream, (string n))

let writeln stream form =
    (write stream form;
     TextIO.output1 (stream, '\n'))