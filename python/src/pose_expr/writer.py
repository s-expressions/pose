#! /usr/bin/env python3

# Copyright 2022 Lassi Kortela
# SPDX-License-Identifier: ISC

import io
import math

from pose_expr import Symbol


class PoseWriter:
    def __init__(self, stream):
        self.stream = stream

    def write(self, obj):
        if isinstance(obj, float):
            if not math.isfinite(obj):
                raise ValueError(cannot(obj))
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
                raise TypeError(cannot(obj))
        elif isinstance(obj, list):
            self.stream.write("(")
            if len(obj):
                self.write(obj[0])
                for i in range(1, len(obj)):
                    self.stream.write(" ")
                    self.write(obj[i])
            self.stream.write(")")
        else:
            raise TypeError(cannot(obj))

    def write_all(self, objects):
        for obj in objects:
            self.write(obj)

    @classmethod
    def to_string(cls, obj):
        with io.StringIO() as stream:
            cls(stream).write(obj)
            return stream.getvalue()

    @staticmethod
    def cannot(obj):
        return "POSE cannot represent {}".format(repr(obj))
