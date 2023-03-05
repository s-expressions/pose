#! /usr/bin/env python3

# Copyright 2021 Lassi Kortela
# SPDX-License-Identifier: ISC

import string


def is_whitespace_char(ch):
    return (ch == " ") or (ch == "\t") or (ch == "\n") or (ch == "\r")


def is_token_first_char(ch):
    return (
        (ch in string.digits) or (ch in string.ascii_letters) or (ch in "_$!?<=>+-*/")
    )


def is_token_next_char(ch):
    return is_token_first_char(ch) or (ch in ".@~^%&")


class Symbol:
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return repr(self.name)

    def __str__(self):
        return self.name

    def iswritable(self):
        return True


class PoseSyntaxError(Exception):
    pass
