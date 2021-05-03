exception SyntaxError of string;

datatype Exp
  = EList   of Exp list
  | ESymbol of string
  | EString of string
  | EReal   of real
  | EInt    of int
  | EIntInf of IntInf.int;

fun stringContainsChar s goalChar =
    let fun loop 0 = false
          | loop i = let val char = String.sub (s, i-1) in
                         if char = goalChar then
                             true
                         else
                             loop (i-1)
                     end
    in loop (String.size s) end;

fun charIsWhitespace char =
    let val cc = Char.ord char in
        (cc = 0x20) orelse (cc >= 0x09 andalso cc <= 0x0D)
    end;

fun charIsAlphabetic char =
    ((char >= #"A") andalso (char <= #"Z")) orelse
    ((char >= #"a") andalso (char <= #"z"));

fun charIsNumeric char =
    ((char >= #"0") andalso (char <= #"9"));

fun charIsTokenCommon char =
    ((charIsAlphabetic char) orelse
     (charIsNumeric char) orelse
     (stringContainsChar "_$!?<=>+-*" char));

fun charIsTokenFirst char =
    ((charIsTokenCommon char) orelse
     (stringContainsChar "/" char));

fun charIsTokenNext char =
    ((charIsTokenFirst char) orelse
     (stringContainsChar ".@~^%&" char));

fun parseNumberOrSymbol string =
    ESymbol string;

fun skipRestOfLine stream =
    case TextIO.input1 stream
     of NONE => ()
      | SOME #"\n" => ()
      | SOME _ => skipRestOfLine stream;

fun skipWhitespaceAndComments stream =
    case TextIO.lookahead stream
     of NONE => ()
      | SOME #";" => (skipRestOfLine stream;
                      skipWhitespaceAndComments stream)
      | SOME char => if charIsWhitespace char then
                         (TextIO.input1 stream;
                          skipWhitespaceAndComments stream)
                     else
                         ();

fun readRestOfTokenAsString char stream =
    let fun loop chars = case TextIO.lookahead stream
                          of NONE => chars
                           | SOME char => if charIsTokenNext char then
                                              (TextIO.input1 stream;
                                               loop (char :: chars))
                                          else
                                              chars
    in String.implode (List.rev (loop [])) end;

fun readTokenAsString stream =
    case TextIO.input1 stream
     of NONE => raise SyntaxError "End-of-file while expecting token"
      | SOME char => if charIsTokenFirst char then
                         raise SyntaxError "Not a token first char"
                     else
                         readRestOfTokenAsString char stream;

fun readIntegerRadix radix stream =
    ESymbol (readTokenAsString stream);

fun readSharpsign stream =
    case TextIO.input1 stream
     of NONE => raise SyntaxError "End-of-file while reading #"
      | SOME #"b" => readIntegerRadix 2 stream
      | SOME #"o" => readIntegerRadix 8 stream
      | SOME #"x" => readIntegerRadix 16 stream
      | SOME char => raise SyntaxError "Unknown # character";

fun readStringEscape endChar stream =
    case TextIO.input1 stream
     of NONE => raise SyntaxError "Unterminated string escape"
      | SOME #"n" => #"\n"
      | SOME #"t" => #"\t"
      | SOME char => if (char = #"\\") orelse (char = endChar) then
                         char
                     else
                         raise SyntaxError "Unknown string escape";

fun readDelimitedString endChar stream =
    let fun loop chars =
            case TextIO.input1 stream
             of NONE => raise SyntaxError "Unterminated string"
              | SOME char => if char = endChar then
                                 chars
                             else
                                 loop ((if char = #"\\" then
                                            readStringEscape endChar stream
                                        else
                                            char)
                                       :: chars)
    in String.implode (List.rev (loop [])) end;

fun readList stream =
    let fun loop forms =
            (skipWhitespaceAndComments stream;
             case TextIO.lookahead stream
              of SOME #")" => (TextIO.input1 stream; forms)
               | _ => case read stream
                       of NONE => raise SyntaxError "Unterminated list"
                       |  SOME form => loop (form :: forms))
    in EList (List.rev (loop [])) end
and read stream =
    (skipWhitespaceAndComments stream;
     case TextIO.lookahead stream
      of NONE => NONE
       | SOME char =>
         SOME (if charIsTokenFirst char then
                   parseNumberOrSymbol (readRestOfTokenAsString char stream)
               else
                   (TextIO.input1 stream;
                    case char
                     of #"\"" => EString (readDelimitedString char stream)
                      | #"|" => ESymbol (readDelimitedString char stream)
                      | #"#" => readSharpsign stream
                      | #"(" => readList stream
                      | #")" => raise SyntaxError "Stray closing parenthesis"
                      | _ => raise SyntaxError
                                   "Unknown character at top level")));

fun readAll stream =
    let fun loop forms =
            case read stream
             of NONE => List.rev forms
             |  SOME form => loop (form :: forms)
    in loop [] end;

fun write stream form =
    case form
     of EList [] => TextIO.output (stream, "()")
      | EList forms => (let fun loop prefix [] =
                                TextIO.output1 (stream, #")")
                              | loop prefix (form :: forms) =
                                (TextIO.output1 (stream, prefix);
                                 write stream form;
                                 loop #" " forms)
                        in loop #"(" forms end)
      | ESymbol s   => TextIO.output (stream, s)
      | EString s   => TextIO.output (stream, s)
      | EReal n     => TextIO.output (stream, (Real.toString n))
      | EInt n      => TextIO.output (stream, (Int.toString n))
      | EIntInf n   => TextIO.output (stream, (IntInf.toString n));

fun writeln stream form =
    (write stream form;
     TextIO.output1 (stream, #"\n"));

List.app (writeln TextIO.stdOut) (readAll TextIO.stdIn);
