# Copyright 2021 Lassi Kortela
# SPDX-License-Identifier: ISC

def whitespace_char?(c)
  (c == " ") or (c == "\t") or (c == "\n") or (c == "\r")
end

def token_first_char?(c)
  (("0" <= c and c <= "9") or
   ("A" <= c and c <= "Z") or
   ("a" <= c and c <= "z") or
   "_$!?<=>+-*/".include?(c))
end

def token_next_char?(c)
  token_first_char?(c) or ".@~^%&".include?(c)
end

class PoseSyntaxError < StandardError
end

def parse_number_or_symbol(s)
  return s.to_sym
end

class PoseReader
  def initialize(io)
    super()
    @io = io
  end

  def peek_char
    c = @io.getc
    @io.ungetc(c) unless c == nil
    c
  end

  def read_char
    @io.getc
  end

  def skip_rest_of_line
    loop do
      c = read_char
      if c == "\n" or c == nil
        break
      end
    end
  end

  def skip_whitespace_and_comments
    loop do
      c = peek_char
      if c == nil
        break
      elsif c == ";"
        skip_rest_of_line
      elsif whitespace_char?(c)
        read_char
      else
        break
      end
    end
  end

  def read_token_as_string
    c = read_char
    if not token_first_char?(c)
      raise PoseSyntaxError("Not a token first char")
    end
    s = c
    loop do
      c = peek_char
      if c == nil or not token_next_char?(c)
        break
      end
      s += read_char
    end
    s
  end

  def read_sharpsign
    c = read_char
    radix = { "b" => 2, "o" => 8, "x" => 16 }[c]
    if radix == nil
      raise PoseSyntaxError("Unknown #")
    end
    token = read_token_as_string
    value = parse_integer(token, radix)
    if value == nil
      raise PoseSyntaxError("Cannot parse integer from token")
    end
    value
  end

  def read_delimited_list(end_char)
    forms = []
    loop do
      skip_whitespace_and_comments
      if peek_char == end_char
        read_char
        break
      else
        begin
          forms.append(read)
        rescue EOFError
          raise PoseSyntaxError("Unterminated list")
        end
      end
    end
    forms
  end

  def read_delimited_string(end_char)
    s = ""
    loop do
      c = read_char
      if c == nil
        raise PoseSyntaxError("Unterminated string")
      elsif c == end_char
        break
      elsif c == "\\"
        c = read_char
        if c == nil
          raise PoseSyntaxError("Unterminated string escape")
        else
          raw = {
            '"' => '"',
            "|" => "|",
            "n" => "\n",
            "t" => "\t",
            "\\" => "\\",
          }[c]
          if raw == nil
            raise PoseSyntaxError("Unknown string escape")
          end
          s += raw
        end
      else
        s += c
      end
    end
    s
  end

  def read
    skip_whitespace_and_comments
    c = peek_char
    if c == nil
      raise EOFError
    elsif token_first_char?(c)
      parse_number_or_symbol(read_token_as_string)
    else
      c = read_char
      if c == "#"
        read_sharpsign
      elsif c == "|"
        read_delimited_string("|").to_sym
      elsif c == '"'
        read_delimited_string('"')
      elsif c == "("
        read_delimited_list(")")
      elsif c == ")"
        raise PoseSyntaxError("Stray closing parenthesis")
      else
        raise PoseSyntaxError("Unknown character at top level")
      end
    end
  end

  def read_all
    forms = []
    begin
      loop do
        forms.append(read)
      end
    rescue EOFError
      forms
    end
  end
end
