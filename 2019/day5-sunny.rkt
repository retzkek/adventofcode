#lang racket

(require rackunit)

(provide read-program
         exe)


(define (val-at lst pos)
  (first (drop lst pos)))

(module+ test
  (check-eq? (val-at '(1 2 3) 0) 1)
  (check-eq? (val-at '(1 2 3) 1) 2)
  (check-eq? (val-at '(1 2 3) 2) 3))

(define (sub-at lst pos val)
  (append (take lst pos) (list val) (drop lst (+ pos 1))))

(module+ test
  (check-equal? (sub-at '(1 2 3) 0 9) '(9 2 3))
  (check-equal? (sub-at '(1 2 3) 1 9) '(1 9 3))
  (check-equal? (sub-at '(1 2 3) 2 9) '(1 2 9)))

(define (int->list n len)
  (map (λ (x) (remainder (quotient n (expt 10 x)) 10)) (range len)))

(module+ test
  (check-equal? (int->list 1234 4) '(4 3 2 1))
  (check-equal? (int->list 4 4) '(4 0 0 0)))

(define-syntax-rule (param mode arg prog)
  (if (eq? mode 0)
      (val-at prog arg)
      arg))

(define-syntax-rule (op2 op prog args step modes)
  (let* ([a (param (first modes) (second args) prog)]
         [b (param (second modes) (third args) prog)]
         [dest (fourth args)])
    (exe (sub-at prog dest (op a b)) (+ step 4))))

(define (op-input prog args step modes)
  (display "> ")
  (exe (sub-at prog (second args) (read)) (+ step 2)))

(define (op-output prog args step modes)
  (writeln (param (first modes) (second args) prog))
  (exe prog (+ step 2)))

(define (op-jump f prog args step modes)
  (exe prog (if (f (param (first modes) (second args) prog))
                (param (second modes) (third args) prog)
                (+ step 3))))

(define (op-cmp f prog args step modes)
  (let* ([a (param (first modes) (second args) prog)]
         [b (param (second modes) (third args) prog)]
         [val (if (f a b) 1 0)]
         [dest (fourth args)])
    (exe (sub-at prog dest val) (+ step 4))))

(define (exe prog step)
  (let* ([args (drop prog step)]
         [opmodes (first args)]
         [opcode (remainder opmodes 100)]
         [modecode (quotient opmodes 100)]
         [modes (int->list modecode 3)])
    (case opcode
      [(99) prog]
      [(1) (op2 + prog args step modes)]
      [(2) (op2 * prog args step modes)]
      [(3) (op-input prog args step modes)]
      [(4) (op-output prog args step modes)]
      [(5) (op-jump (λ (x) (not (eq? x 0))) prog args step modes)]
      [(6) (op-jump (λ (x) (eq? x 0)) prog args step modes)]
      [(7) (op-cmp < prog args step modes)]
      [(8) (op-cmp = prog args step modes)])))

(module+ test
  (check-equal? (exe '(1 4 5 4 11 88) 0) '(1 4 5 4 99 88))
  (check-equal? (exe '(2 4 5 4 3 33) 0) '(2 4 5 4 99 33))
  (parameterize ([current-input-port (open-input-string "23")])
    (check-equal? (exe '(3 3 99 0) 0) '(3 3 99 23)))
  (let ([out (open-output-string)])
    (parameterize ([current-output-port out])
      (check-equal? (exe '(4 3 99 23) 0) '(4 3 99 23))
      (check-equal? (get-output-string out) "23\n")))
  (check-equal? (exe '(1002 4 3 4 33) 0) '(1002 4 3 4 99)))

(define (read-program ip)
  (map string->number (string-split (read-line ip) ",")))

(module+ test
  (check-equal? (read-program (open-input-string "1,2,3")) '(1 2 3)))

(define (check-program prog newprog)
  (check-equal?
   (exe (read-program (open-input-string prog)) 0)
   (read-program (open-input-string newprog))))

(module+ test
  (check-program "1,0,0,0,99" "2,0,0,0,99")
  (check-program "2,3,0,3,99" "2,3,0,6,99")
  (check-program "2,4,4,5,99,0" "2,4,4,5,99,9801")
  (check-program "1,1,1,4,99,5,6,0,99" "30,1,1,4,2,5,6,0,99"))

(define (check-program-io program stdin stdout)
  (let ([out (open-output-string)])
    (parameterize ([current-input-port (open-input-string stdin)]
                   [current-output-port out])
      (exe (read-program (open-input-string program)) 0)
      (check-equal? (get-output-string out) stdout))))

(module+ test
  (check-program-io "3,9,8,9,10,9,4,9,99,-1,8" "8" "> 1\n")
  (check-program-io "3,9,8,9,10,9,4,9,99,-1,8" "2" "> 0\n")
  (check-program-io "3,9,7,9,10,9,4,9,99,-1,8" "2" "> 1\n")
  (check-program-io "3,9,7,9,10,9,4,9,99,-1,8" "10" "> 0\n")
  (check-program-io "3,3,1108,-1,8,3,4,3,99" "8" "> 1\n")
  (check-program-io "3,3,1108,-1,8,3,4,3,99" "2" "> 0\n")
  (check-program-io "3,3,1107,-1,8,3,4,3,99" "2" "> 1\n")
  (check-program-io "3,3,1107,-1,8,3,4,3,99" "10" "> 0\n")
  (check-program-io "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" "0" "> 0\n")
  (check-program-io "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" "22" "> 1\n")
  (check-program-io "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" "0" "> 0\n")
  (check-program-io "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" "22" "> 1\n")
  (check-program-io "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
                    "2" "> 999\n")
  (check-program-io "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
                    "8" "> 1000\n")
  (check-program-io "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
                    "9" "> 1001\n"))

(module+ main
  (display
   (exe
    (read-program
     (open-input-file
      (command-line
       #:program "intcode"
       #:args (filename)
       filename)))
    0)))
