#! /usr/bin/env python3

# Copyright 2021 Lassi Kortela
# SPDX-License-Identifier: ISC

import string


def is_whitespace_char(ch):
    return (ch == " ") or (ch == "\t") or (ch == "\n") or (ch == "\r")


def is_token_first_char(ch):
    if ch in string.digits:
        return True
    if ch in string.ascii_letters:
        return True
    return ch in "_$!?<=>+-*/"


def is_token_next_char(ch):
    return is_token_first_char(ch) or (ch in ".@~^%&")


class Symbol:
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name  # TODO: vertical bars for complex symbols

    def __str__(self):
        return repr(self)


def parse_number_or_symbol(s):
    return Symbol(s)


class PoseSyntaxError(Exception):
    pass


class PoseReader:
    def __init__(self, stream):
        self.stream = stream
        self.lastchar = ""

    def peek_char(self):
        if self.lastchar == "":
            self.lastchar = self.stream.read(1)
        return self.lastchar

    def read_char(self):
        ch = self.peek_char()
        self.lastchar = ""
        return ch

    def skip_rest_of_line(self):
        while True:
            ch = self.read_char()
            if ch == "\n" or ch == "":
                break

    def skip_whitespace_and_comments(self):
        while True:
            ch = self.peek_char()
            if ch == "":
                break
            elif ch == ";":
                self.skip_rest_of_line()
            elif is_whitespace_char(ch):
                self.read_char()
            else:
                break

    def read_token_as_string(self):
        ch = self.read_char()
        if not is_token_first_char(ch):
            raise PoseSyntaxError("Not a token first char")
        s = ch
        while True:
            ch = self.peek_char()
            if ch == "" or not is_token_next_char(ch):
                break
            s += self.read_char()
        return s

    def read_sharpsign(self):
        ch = self.read_char()
        radix = {"b": 2, "o": 8, "x": 16}.get(ch)
        if radix is None:
            raise PoseSyntaxError("Unknown #")
        token = self.read_token_as_string()
        value = parse_integer(token, radix)
        if value is None:
            raise PoseSyntaxError("Cannot parse integer from token")
        return value

    def read_delimited_list(self, end_char):
        forms = []
        while True:
            self.skip_whitespace_and_comments()
            if self.peek_char() == end_char:
                self.read_char()
                break
            else:
                try:
                    forms.append(self.read())
                except EOFError:
                    raise PoseSyntaxError("Unterminated list")
        return forms

    def read_delimited_string(self, end_char):
        s = ""
        while True:
            ch = self.read_char()
            if ch == "":
                raise PoseSyntaxError("Unterminated string")
            elif ch == end_char:
                break
            elif ch == "\\":
                ch = self.read_char()
                if ch == "":
                    raise PoseSyntaxError("Unterminated string escape")
                else:
                    raw = {
                        '"': '"',
                        "|": "|",
                        "n": "\n",
                        "t": "\t",
                        "\\": "\\",
                    }.get(ch)
                    if raw is None:
                        raise PoseSyntaxError("Unknown string escape")
                    s += raw
            else:
                s += ch
        return s

    def read(self):
        self.skip_whitespace_and_comments()
        ch = self.peek_char()
        if ch == "":
            raise EOFError()
        elif is_token_first_char(ch):
            return parse_number_or_symbol(self.read_token_as_string())
        else:
            ch = self.read_char()
            if ch == "#":
                return self.read_sharpsign()
            elif ch == "|":
                return Symbol(self.read_delimited_string("|"))
            elif ch == '"':
                return self.read_delimited_string('"')
            elif ch == "(":
                return self.read_delimited_list(")")
            elif ch == ")":
                raise PoseSyntaxError("Stray closing parenthesis")
            else:
                raise PoseSyntaxError("Unknown character at top level")

    def read_all(self):
        forms = []
        try:
            while True:
                forms.append(self.read())
        except EOFError:
            return forms


if __name__ == "__main__":
    import sys

    for form in PoseReader(sys.stdin).read_all():
        print(repr(form))
