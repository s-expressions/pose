#! /usr/bin/env python3

# Copyright 2022 Lassi Kortela
# SPDX-License-Identifier: ISC

import math


class PoseWriter:
    def __init__(self, stream):
        self.stream = stream

    def write(self, obj):
        if isinstance(obj, float):
            if not math.isfinite(obj):
                raise ValueError("POSE cannot represent {}".format(repr(obj)))
            self.stream.write(str(obj))
        elif isinstance(obj, int):
            self.stream.write(str(obj))
        elif isinstance(obj, str):
            self.stream.write('"')
            for ch in obj:
                if ch in '"\\':
                    self.stream.write("\\")
                self.stream.write(ch)
            self.stream.write('"')
        elif isinstance(obj, Symbol):
            if obj.iswritable():
                self.stream.write(str(obj))
            else:
                raise TypeError("POSE cannot represent {}".format(repr(obj)))
        elif isinstance(obj, list):
            self.stream.write("(")
            for element in obj:
                self.write(element)
            self.stream.write(")")
        else:
            raise TypeError("POSE cannot represent {}".format(repr(obj)))

    def write_all(self, objects):
        for obj in objects:
            self.write(obj)
