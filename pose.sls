;; Copyright 2021 Lassi Kortela
;; SPDX-License-Identifier: ISC

(library (pose)
  (export pose-read pose-read-all)
  (import (rnrs) (srfi private include))
  (include/resolve () "pose.scm"))
