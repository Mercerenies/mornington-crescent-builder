
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
;; * Rickmansworth - Stores a string at all times, can be used to
;; reset the accumulator to a safe string value.
;;
;; These constants are initialized by the prologue.

(define <negative-one> '|North Harrow|)
(define <zero> '|Oval|)
(define <one> '|Queen's Park|)
(define <ten> '|Tufnell Park|)
(define <hundred> '|West Hampstead|)
(define <ten-thousand> '|Tower Hill|)
(define <million> '|Mill Hill East|)
(define <hundred-million> '|Hampstead|)
(define <ten-billion> '|Belsize Park|)
(define <trillion> '|Turnham Green|)
(define <hundred-trillion> '|Turnpike Lane|)
(define <ten-quadrillion> '|Queensbury|)
(define <quintillion> '|Queensway|)

(define <string> '|Rickmansworth|) ; Always a string
(define <tmp> '|Tooting Bec|) ; Used for misc temporary purposes

;; General purpose variables
(define <a> '|Arsenal|)
(define <b> '|Becontree|)
(define <c> '|Chesham|)
(define <d> '|Debden|)
(define <e> '|Edgware|)
(define <f> '|Fairlop|)
(define <g> '|Greenford|)
(define <h> '|Hainault|)
(define <i> '|Ickenham|)
(define <j> '|Kenton|)
(define <value> '|Victoria|)
(define <value^2> '|Vauxhall|)
(define <left-cmp> '|Leyton|)
(define <right-cmp> '|Roding Valley|)
(define <final-value> '|Finchley Central|)

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

;; (dest is at station, src is at accumulator) when the math is
;; performed. Do NOT call this with dest == src!
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

;; Clobbers tmp
(define (-= dest src)
  (mov <tmp> src)
  (*= <tmp> <negative-one>)
  (+= dest <tmp>))

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
        (op-ifstmt)
        (prim-reset-to-string)))

(define-syntax-rule (do-while register body ...)
  (code (prim-loop-start)
        body ...
        (continue-if register)
        (op-popstmt)
        (prim-reset-to-string)))

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

(define (small-numeral register value)
  (case value
    [(0) (mov register <zero>)]
    [(1) (mov register <one>)]
    [(2 3 4 5 6) (begin (store-seven register) (for ([i (in-range 0 (- 7 value))]) (decrement register)))]
    [(7) (store-seven register)]
    [(8 9) (begin (store-seven register) (for ([i (in-range 0 (- value 7))]) (increment register)))]
    [(10) (mov register <ten>)]
    [else (error "Invalid small numeral value: " value)]))

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
   (increment <ten>)
   ;; Hundred
   (mov <hundred> <ten>)
   (*= <hundred> <ten>)
   ;; Ten Thousand
   (mov <ten-thousand> <hundred>)
   (*= <ten-thousand> <hundred>)
   ;; Million
   (mov <million> <ten-thousand>)
   (*= <million> <hundred>)
   ;; Hundred Million
   (mov <hundred-million> <million>)
   (*= <hundred-million> <hundred>)
   ;; Ten Billion
   (mov <ten-billion> <hundred-million>)
   (*= <ten-billion> <hundred>)
   ;; Trillion
   (mov <trillion> <ten-billion>)
   (*= <trillion> <hundred>)
   ;; Hundred Trillion
   (mov <hundred-trillion> <trillion>)
   (*= <hundred-trillion> <hundred>)
   ;; Ten Quadrillion
   (mov <ten-quadrillion> <hundred-trillion>)
   (*= <ten-quadrillion> <hundred>)
   ;; Quintillion
   (mov <quintillion> <ten-quadrillion>)
   (*= <quintillion> <hundred>)))

(define (assemble-digital-value dest . registers)
  (mov dest (first registers))
  (for ([reg (rest registers)])
    (*= dest <ten>)
    (+= dest reg)))

(define (test-digit denom expected)
  (mov <left-cmp> <value^2>)
  (/= <left-cmp> denom)
  (%= <left-cmp> <ten>)
  (-= <left-cmp> expected)
  (continue-if <left-cmp>))

(code
 (prologue)
 (mov <value> <hundred-million>)
 (mov <tmp> <value>)
 (+= <value> <tmp>) ; value = 200,000,000
 (mov <a> <hundred-million>)
 (/= <a> <ten>) ; a = 10,000,000
 (-= <value> <a>)
 (-= <value> <a>)
 (-= <value> <a>)
 (-= <value> <a>)
 (-= <value> <a>)
 (-= <value> <a>) ; value = 140,000,000

 ;; Precompute small numerals
 (small-numeral <b> 2)
 (small-numeral <c> 3)
 (small-numeral <d> 4)
 (small-numeral <e> 5)
 (small-numeral <f> 6)
 (small-numeral <g> 7)
 (small-numeral <h> 8)
 (small-numeral <i> 9)

 ;; Build our upper bound of 138924439.
 (mov <value> <one>) ; 1
 (*= <value> <ten>)
 (+= <value> <c>) ; 3
 (*= <value> <ten>)
 (+= <value> <h>) ; 8
 (*= <value> <ten>)
 (+= <value> <i>) ; 9
 (*= <value> <ten>)
 (+= <value> <b>) ; 2
 (*= <value> <ten>)
 (+= <value> <d>) ; 4
 (*= <value> <ten>)
 (+= <value> <d>) ; 4
 (*= <value> <ten>)
 (+= <value> <c>) ; 3
 (*= <value> <ten>)
 (+= <value> <i>) ; 9

 (do-while <value>
   (decrement <value>)
   (decrement <value>) ; We start on an odd number and we know the answer is odd (see reasoning in underload-gen.rkt)
   (mov <value^2> <value>)
   (*= <value^2> <value>)
   (test-digit <one> <i>) ; 9
   (test-digit <hundred> <h>) ; 8
   (test-digit <ten-thousand> <g>) ; 7
   (test-digit <million> <f>) ; 6
   (test-digit <hundred-million> <e>) ; 5
   (test-digit <ten-billion> <d>) ; 4
   (test-digit <trillion> <c>) ; 3
   (test-digit <hundred-trillion> <b>) ; 2
   (*= <value> <ten>)
   (output-and-exit <value>)))
