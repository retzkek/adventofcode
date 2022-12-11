(in-package aoc)

(defstruct monkey
  id
  (items (make-array 100
                     :element-type 'integer
                     :adjustable t
                     :fill-pointer 0))
  operation test if-true if-false (count 0))

(defun parse (stream)
  (loop with monkey = nil
        with monkeys = (make-array 10
                                   :element-type 'monkey
                                   :fill-pointer 0
                                   :adjustable t)
        for line = (read-line stream nil)
        until (null line)
        do (cond
             ((scan "^Monkey \\d+:$" line)
              (when monkey (vector-push-extend monkey monkeys))
              (setq monkey (make-monkey
                            :id (parse-integer line :start 7 :end (1- (length line))))))
             ((scan "Starting items:" line)
              (do-matches (s e "\\d+" line)
                (vector-push-extend (parse-integer line :start s :end e)
                                    (monkey-items monkey))))
             ((scan "Operation:" line)
              )
             )
        finally (return monkeys)))

(with-input-from-string (in *example1*) (parse in))


(defparameter *example1* "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")
