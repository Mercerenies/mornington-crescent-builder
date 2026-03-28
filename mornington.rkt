
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
(define (op-add) (goto '|Upminster|))
(define (op-mul) (goto '|Chalfont & Latimer|))
(define (op-div) (goto '|Cannon Street|))
(define (op-mod) (goto '|Preston Road|))
(define (op-max) (goto '|Bounds Green|))
(define (op-bitnor) (goto '|Manor House|))
(define (op-bitand) (goto '|Holland Park|))
(define (op-bitshr) (goto '|Turnham Green|))
(define (op-bitshl) (goto '|Stepney Green|))
(define (op-square) (goto '|Russell Square|))
(define (op-bitnot) (goto '|Notting Hill Gate|))
(define (op-parseint) (goto '|Parsons Green|))
(define (op-seven) (goto '|Seven Sisters|))
(define (op-char<->codepoint) (goto '|Charing Cross|))
(define (op-stringcat) (goto '|Paddington|))
(define (op-leftsubstr) (goto '|Gunnersbury|))
(define (op-rightsubstr) (goto '|Mile End|))
(define (op-uppercase) (goto '|Upney|))
(define (op-lowercase) (goto '|Hounslow Central|))
(define (op-reversestr) (goto '|Turnpike Lane|))
(define (op-bank) (goto '|Bank|))
(define (op-hammersmith) (goto '|Hammersmith|))
(define (op-continuation) (goto '|Temple|))
(define (op-ifstmt) (goto '|Angel|))
(define (op-popstmt) (goto '|Marble Arch|))
(define (op-output-and-exit) (goto '|Mornington Crescent|))

;; Assumptions: At all times between instructions, the accumulator is
;; a string whose value we don't care about (this is true at the start
;; of the program). The values of Bank and Hammersmith can be
;; clobbered at any time and may be used (locally) for duping values.
;;
;; Unless otherwise stated, all "special" registers store a string
;; while at rest. Exceptions to this are: Bank/Hammersmith (whose
;; values are always considered volatile).
;;
;; The following registers are reserved to contain constants, for our
;; convenience:
;;
;; * Queen's Park - 1
;;
;; These constants are initialized by the prologue.

(define <one> '|Queen's Park|)

(define (output-and-exit register)
  (code (goto register)
        (op-output-and-exit)))

(define (store-seven dest-register)
  (code (goto dest-register)
        (op-seven)
        (goto dest-register)))

(define (swap a b)
  (code (goto a)
        (goto b)
        (goto a)))

(define (dest=src dest src)
  (code (goto src)
        (op-bank)
        (op-hammersmith)
        (goto dest)
        (op-hammersmith)
        (goto src)))

;; (dest is at station, src is at accumulator) when the math is done.
(define (binary-operation op dest src)
  (code (goto dest)
        (op)
        (goto src)
        (op)
        (goto dest)
        (op)
        (goto src)))

(define (dest+=src dest src)
  (binary-operation op-add dest src))

(define (dest*=src dest src)
  (binary-operation op-mul dest src))

(define (dest=src/dest dest src)
  (binary-operation op-div dest src))

(code
 (store-seven '|Bow Road|)
 (dest=src '|Willesden Junction| '|Bow Road|)
 (dest+=src '|Willesden Junction| '|Bow Road|)
; (dest=src/dest '|Willesden Junction| '|Bow Road|)
 (output-and-exit '|Willesden Junction|))
