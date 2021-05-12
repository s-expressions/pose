(import (rnrs) (pose))

(define (writeln x) (write x) (newline))

(for-each writeln (pose-read-all))
