#! /usr/bin/env python3

import io
import unittest

from pose_expr import PoseSyntaxError
from pose_expr.reader import PoseReader
from pose_expr.writer import PoseWriter


class TestPoseReader(unittest.TestCase):
    def test_smoke(self):
        self.assertEqual(PoseReader(io.StringIO("(1 2 3)")).read_all(), [[1, 2, 3]])
        with self.assertRaises(EOFError):
            PoseReader(io.StringIO("(")).read()
        with self.assertRaises(PoseSyntaxError):
            PoseReader(io.StringIO(")")).read()


if __name__ == "__main__":
    unittest.main()
