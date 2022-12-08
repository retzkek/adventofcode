(in-package aoc)

(defparameter *example1*
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defstruct node name dir parent children size)

(defun root (node)
  (if (null (node-parent node))
      node
      (root (node-parent node))))

(defun total-size (node)
  (+ (node-size node)
     (reduce '+ (mapcar 'total-size (node-children node)))))

(defun total-sizes (node)
  (cons (total-size node)
        (apply 'nconc (mapcar 'total-sizes
                              (remove-if-not
                               (lambda (x) (node-dir x))
                               (node-children node))))))

(defun parse (stream)
  (loop with n = (make-node :name "/" :dir t :parent nil :children nil :size 0)
        for line = (read-line stream nil)
        until (null line)
        do
           (cond
             ((string= (subseq line 0 4) "$ cd")
              (let ((dir (subseq line 5)))
                (cond ((string= dir "/") (setq n (root n)))
                      ((string= dir "..") (setq n (node-parent n)))
                      (t (setq n (find-if (lambda (x) (string= (node-name x) dir))
                                          (node-children n)))))))
             ((string= line "$ ls") nil)
             ((string= (subseq line 0 3) "dir")
              (push (make-node :name (subseq line 4) :dir t :parent n :size 0)
                    (node-children n)))
             (t (let ((c (position #\  line)))
                  (push (make-node :name (subseq line (1+ c))
                                   :parent n
                                   :size (parse-integer (subseq line 0 c)))
                        (node-children n)))))
        finally (return (root n))))

(defun part1 (stream)
  (reduce '+ (remove-if (lambda (x) (> x 100000)) (total-sizes (parse stream)))))

(with-input-from-string (in *example1*) (part1 in))

(with-input-stream (in 2022 7) (part1 in))
