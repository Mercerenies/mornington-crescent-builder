
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
;; The following registers are reserved to contain constants or other
;; reserved purposes, for our convenience:
;;
;; * North Harrow - -1
;;
;; * Oval - 0
;;
;; * Queen's Park - 1
;;
;; * Tufnell Park - 10
;;
;; * Rickmansworth - Stores a string at all times, can be used to
;; reset the accumulator to a safe string value.
;;
;; These constants are initialized by the prologue.

(define <negative-one> '|North Harrow|)
(define <zero> '|Oval|)
(define <one> '|Queen's Park|)
(define <ten> '|Tufnell Park|)
(define <string> '|Rickmansworth|)
(define <tmp> '|Tooting Bec|) ; Used for misc temporary purposes

;; Does not clobber register, but does clobber Bank/Hammersmith.
(define (prim-load-without-clobbering register)
  (code (goto register)
        (op-bank)
        (op-hammersmith)
        (goto register)
        (op-hammersmith)))

;; Sets accumulator to a string value. Clobbers Bank/Hammersmith.
(define (prim-reset-to-string)
  (prim-load-without-clobbering <string>))

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

(define (mov dest src)
  (code (goto src)
        (op-bank)
        (op-hammersmith)
        (goto dest)
        (op-hammersmith)
        (goto src)))

;; (dest is at station, src is at accumulator) when the math is performed.
(define (binary-operation op dest src)
  (code (goto dest)
        (op)
        (goto src)
        (op)
        (goto dest)
        (op)
        (goto src)))

(define (+= dest src)
  (binary-operation op-add dest src))

(define (*= dest src)
  (binary-operation op-mul dest src))

(define (/= dest src)
  (binary-operation op-div dest src))

(define (%= dest src)
  (binary-operation op-mod dest src))

(define (max= dest src)
  (binary-operation op-max dest src))

(define (prim-bitnot)
  ;; Visit twice so we negate the accumulator, leaving the value at
  ;; Notting Hill Gate unchanged.
  (code (op-bitnot)
        (op-bitnot)))

(define (prim-loop-start)
  (code (op-continuation)
        (prim-reset-to-string)))

(define (continue-if register)
  (code (prim-load-without-clobbering register)
        (op-ifstmt)))

(define-syntax-rule (do-while register body ...)
  (code (prim-loop-start)
        body ...
        (continue-if register)
        (op-popstmt)))

(define (decrement register)
  (+= register <negative-one>))

(define (increment register)
  (+= register <one>))

;; Loop for the register from 9 to 0 inclusive. Do not mutate the
;; register's value inside the loop body. (continue-if ...) inside the
;; body is permitted.
(define-syntax-rule (loop<9..0> register body ...)
  (code (mov register <ten>)
        (do-while register
         (decrement register)
         body ...)))

;; Initialize constants
(define (prologue)
  (code
   (store-seven <tmp>)
   ;; One
   (store-seven <one>)
   (/= <one> <tmp>)
   ;; Zero
   (store-seven <zero>)
   (%= <zero> <tmp>)
   ;; Negative one
   (prim-load-without-clobbering <zero>)
   (prim-bitnot)
   (goto <negative-one>)
   ;; Ten
   (store-seven <ten>)
   (increment <ten>)
   (increment <ten>)
   (increment <ten>)))

(code
 (prologue)
 (mov '|West Ham| <zero>)
 (loop<9..0> '|St. James's Park|
  (+= '|West Ham| '|St. James's Park|))
 (output-and-exit '|West Ham|))
