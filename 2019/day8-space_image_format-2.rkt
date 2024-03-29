#lang racket

(require rackunit
         math/array)

(struct sif (width height layers data) #:transparent)

(define (read-sif sif-string width height)
  (let* ((dat (list->vector
             (map (λ (c) (- (char->integer c) (char->integer #\0)))
                  (string->list sif-string))))
         [layers (/ (vector-length dat) (* width height))]
         [a (build-array (vector layers height width)
                         (λ (lji) (match-let ([(vector l j i) lji])
                                    (vector-ref dat (+ (* l width height) (* j width) i)))))])
    (sif width height layers a)))

(define *test-sif* (sif 3 2 2 (array #[#[#[1 2 3]
                                         #[4 5 6]]
                                       #[#[7 8 9]
                                         #[0 1 2]]])))
(module+ test
  (check-equal? (read-sif "123456789012" 3 2) *test-sif*))

(define (count-in-layer sif l n)
  (array-count (λ (x) (= x n)) (array-slice-ref (sif-data sif) (list l (::) (::)))))

(define (count-per-layer sif n)
  (map (λ (l) (count-in-layer sif l n))
       (range (sif-layers sif))))

(module+ test
  (check-equal? (count-per-layer *test-sif* 0) '(0 1))
  (check-equal? (count-per-layer *test-sif* 1) '(1 1))
  (check-equal? (count-per-layer *test-sif* 4) '(1 0))
  (check-equal? (count-per-layer *test-sif* 7) '(0 1)))

(define (layer-with-min sif n)
  (argmin (λ (l) (count-in-layer sif l n))
       (range (sif-layers sif))))

(module+ test
  (check-equal? (layer-with-min *test-sif* 7) 0)
  (check-equal? (layer-with-min *test-sif* 3) 1)
  (check-equal? (layer-with-min *test-sif* 1) 0))

(define (decode-sif sif)
  (array-axis-fold (sif-data sif) 0 (λ (x c) (if (= c 2) x c))))

(module+ test
  (check-equal? (decode-sif (read-sif "0222112222120000" 2 2))
                (array #[#[0 1]
                         #[1 0]])))

(define (print-array arr)
  (for* ([a (in-array-axis arr 0)])
    (map (λ (x) (display (if (= x 0) " " "#"))) (array->list a))
    (newline)))

(module+ main
  (let* ([sif (command-line
              #:program "sif"
              #:args (filename width height)
              (read-sif
               (read-line
                (open-input-file
                 filename))
               (string->number width)
               (string->number height)))])
    (print-array (decode-sif sif))))
