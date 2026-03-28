
#lang racket

(require racket/struct)
(require json)
(require threading)

(define (write-path-node path-node port mode)
  (write-string (format "Take ~a Line to ~a" (path-node-line path-node) (path-node-node path-node)) port))

(struct path-node
  (line node)
  #:methods gen:custom-write
  [(define write-proc write-path-node)])

(define paths-json
  (delay
    (with-input-from-file "allpaths.json" read-json)))

(define (get-path src dest)
  (define (compile-path-node path-json)
    (path-node
     (hash-ref path-json 'line)
     (string->symbol (hash-ref path-json 'node))))

  (~>
   (force paths-json)
   (hash-ref src)
   (hash-ref dest)
   (map compile-path-node _)))

(define current-station '|Mornington Crescent|)

(define (raw-instruction path-node)
  (set! current-station (path-node-node path-node))
  (displayln path-node))

(define (goto station)
  (let ([path (get-path current-station station)])
    (for ([p path] [i (in-naturals)])
      (raw-instruction p)
      (when (< i (- (length path) 1))
        (raw-instruction p))))) ;; Force a no-op

(define-syntax-rule (code . args)
  (begin . args))

;; All of the basic instructions.
(define (add) (goto '|Upminster|))
(define (mul) (goto '|Chalfont & Latimer|))
(define (div) (goto '|Cannon Street|))
(define (mod) (goto '|Preston Road|))
(define (max) (goto '|Bounds Green|))
(define (bitnor) (goto '|Manor House|))
(define (bitand) (goto '|Holland Park|))
(define (bitshr) (goto '|Turnham Green|))
(define (bitshl) (goto '|Stepney Green|))
(define (square) (goto '|Russell Square|))
(define (bitnot) (goto '|Notting Hill Gate|))
(define (parseint) (goto '|Parsons Green|))
(define (seven) (goto '|Seven Sisters|))
(define (char<->codepoint) (goto '|Charing Cross|))
(define (stringcat) (goto '|Paddington|))
(define (leftsubstr) (goto '|Gunnersbury|))
(define (rightsubstr) (goto '|Mile End|))
(define (uppercase) (goto '|Upney|))
(define (lowercase) (goto '|Hounslow Central|))
(define (reversestr) (goto '|Turnpike Lane|))
(define (bank) (goto '|Bank|))
(define (hammersmith) (goto '|Hammersmith|))
(define (continuation) (goto '|Temple|))
(define (ifstmt) (goto '|Angel|))
(define (popstmt) (goto '|Marble Arch|))
(define (output-and-exit) (goto '|Mornington Crescent|))

;; Assumptions: At all times between instructions, the accumulator is
;; a string whose value we don't care about (this is true at the start
;; of the program). The values of Bank and Hammond can be clobbered at
;; any time and may be used (locally) for duping values.

(code
 (seven)
 (add)
 (seven)
 (add)
 (square)
 (square)
 (square)
 (square)
 (output-and-exit))
