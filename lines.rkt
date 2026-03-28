
#lang racket

(require struct-plus-plus)

(struct++ path-node
  ([line string]
   [destination string]))

(define all-stations
  (delay
    (call-with-input-file "lines.txt" read-lines-file)))

(define (read-lines-file file)
  (define (remove-first-and-last s)
    (substring s 1 (- (string-length s) 1)))

  (for/hash ([line (sequence-tail (in-lines file) 1)])
    (let ([station-name (string-trim (first (regexp-match #px"^[^\\[]*" line)))]
          [line-names (map remove-first-and-last (regexp-match* #px"\\[[^\\]]*\\]" line))])
      (values station-name line-names))))

(displayln (force all-stations))
