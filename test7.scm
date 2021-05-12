(import (scheme base) (scheme file) (scheme write) (pose))

(define (writeln x) (write x) (newline))

(for-each writeln (pose-read-all))
