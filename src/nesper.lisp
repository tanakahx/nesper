(in-package :cl-user)

(defpackage :nesper
  (:use :cl)
  (:import-from :split-sequence
                :split-sequence)
  (:export :read-training-file
           :make-network
           :init-network
           :network-weight
           :network-bias
           :learn
           :answer
           :setf-network))

(in-package :nesper)

(defun make-matrix (row col &key (initial-element 0))
  (make-array (list row col) :initial-element initial-element))

(defun dim (matrix axis)
  (array-dimension matrix axis))

(defun matrix-transpose (matrix)
  (let ((new-matrix (make-array (reverse (array-dimensions matrix)))))
    (loop for row below (dim new-matrix 0)
       do (loop for col below (dim new-matrix 1)
             do (setf (aref new-matrix row col) (aref matrix col row))))
    new-matrix))

(defun matrix. (matrix1 matrix2)
  (when (/= (dim matrix1 1) (dim matrix2 0))
    (error "Dimension of array does not match."))
  (let* ((row (dim matrix1 0))
         (col (dim matrix2 1))
         (new-matrix (make-array (list row col))))
    (loop for i below row
       do (loop for j below col
             do (setf (aref new-matrix i j)
                      (loop for k below (dim matrix1 1)
                         sum (* (aref matrix1 i k) (aref matrix2 k j))))))
    new-matrix))

(defun matrix-f (fn &rest matrixes)
  (let ((row (dim (car matrixes) 0))
        (col (dim (car matrixes) 1)))
    (when (some #'(lambda (matrix)
                    (or (/= row (dim matrix 0))
                        (/= col (dim matrix 1))))
                matrixes)
      (error "Dimension of array dones not match."))
    (let ((new-matrix (make-array (list row col))))
      (loop for i below row
         do (loop for j below col
               do (setf (aref new-matrix i j)
                        (apply fn (mapcar #'(lambda (matrix) (aref matrix i j))
                                          matrixes)))))
      new-matrix)))

(defun matrix+ (&rest matrixes)
  (apply #'matrix-f #'+ matrixes))

(defun matrix- (&rest matrixes)
  (apply #'matrix-f #'- matrixes))

(defun matrix* (&rest matrixies)
  (apply #'matrix-f #'* matrixies))

(defun matrix1 (row col &optional (c 1))
  (make-matrix row col :initial-element c))

(defun matrix-c* (c matrix)
  (funcall #'matrix*
           (matrix1 (dim matrix 0) (dim matrix 1) c)
           matrix))

(defun sigmoid (x &optional (a 1))
  (/ 1 (+ 1 (exp (- (* a x))))))

(defun mean-squared-error (output expected)
  (/ (aref (matrix. (matrix-transpose (matrix- output expected))
                    (matrix- output expected))
           0 0)
     (dim expected 0)))

(defun make-network (nodes)
  (labels ((make (nodes &key (bias nil))
             (when (second nodes)
               (cons (make-matrix (second nodes)
                                  (if bias 1 (first nodes)))
                     (make (cdr nodes) :bias bias)))))
    (when (< (length nodes) 2)
      (error "Node size is not enough."))
    (list (init-network (make nodes))
          (init-network (make nodes :bias t)))))

(defun init-network (network)
  (loop for n in network
     do (loop for row below (dim n 0)
           do (loop for col below (dim n 1)
                 do (setf (aref n row col)
                          (- (random 2d0) 1d0))))
     finally (return network)))

(defun network-weight (network)
  (first network))

(defun network-bias (network)
  (second network))

(defun read-training-file (file &key (network nil network-supplied-p) input-count output-count)
  (destructuring-bind (input-count output-count)
      (if network-supplied-p
          (list (dim (first (network-bias network)) 0)
                (dim (car (last (network-bias network))) 0))
          (list input-count output-count))
    (flet ((set-training-data-level (data)
             (let ((x (make-matrix input-count 1))
                   (d (make-matrix output-count 1)))
               (loop for i below input-count
                  do (setf (aref x i 0) (nth i data)))
               (loop for i below output-count
                  do (setf (aref d i 0) (nth (+ i input-count) data)))
               (cons x d))))
      (with-open-file (in file)
        (loop for line = (read-line in nil :eof)
           until (eq line :eof)
           when (not (equal line ""))
           collect (set-training-data-level (mapcar #'read-from-string
                                                    (split-sequence:split-sequence #\Space line))))))))

(defun forward-prop (x ws bs &key (activation-f #'sigmoid))
  (loop repeat (length ws)
     for _ws = ws then (cdr _ws)
     for _bs = bs then (cdr _bs)
     for w = (car _ws)
     for b = (car _bs)
     for u = (matrix+ (matrix. w x) b) then (matrix+ (matrix. w z) b)
     for z = (matrix-f activation-f u)
     collect z into zs
     finally (return (cons x zs))))

(defun backward-prop (d zs ws)
  (let ((delta (matrix* (matrix* (car zs) (matrix- (matrix1 1 1) (car zs)))
                        (matrix- (car zs) d))))
    (loop repeat (1- (length ws))
       for _zs = (cdr zs) then (cdr _zs)
       for _ws = ws then (cdr _ws)
       for z  = (car _zs)
       for w  = (car _ws)
       for fp = (matrix* z (matrix- (matrix1 (dim z 0) (dim z 1)) z))
       for dn = delta then d
       for d  = (matrix* (matrix. (matrix-transpose w) dn) fp)
       collect d into ds
       finally (return (reverse (cons delta ds))))))

(defun update-weight (ws ds zs &key (epsilon 0.05))
  (mapcar #'(lambda (w d z)
              (matrix- w
                       (matrix-c* epsilon
                                  (matrix. d (matrix-transpose z)))))
          ws ds zs))

(defun calc-error (y d)
  (/ (aref (matrix. (matrix-transpose (matrix1 (dim y 0) (dim y 1)))
                    (matrix* (matrix- y d) (matrix- y d)))
           0 0)
     (dim y 0)))

(defun learn (network batch &key (learning-count 10000) (error-th 0.01) (verbose nil))
  (let ((ws (network-weight network))
        (bs (network-bias network)))
    (loop for learn below learning-count
       for max-error =
         (loop for x.d in batch
            for train from 0
            for x = (car x.d)
            for d = (cdr x.d)
            for zs = (forward-prop x ws bs)
            for y = (car (last zs))
            for ds = (backward-prop d (reverse zs) (reverse ws))
            for err = (calc-error y d)
            maximize err into max-error
            do (setf ws (update-weight ws ds zs)
                     bs (update-weight bs ds (make-list (length bs) :initial-element (matrix1 1 1))))
              (when verbose
                (format t "Learning: ~a Train: ~a Error: ~a~%" learn train err))
            finally (return max-error))
       do (when (<= max-error error-th)
            (return (values (list ws bs) t)))
         (when verbose
           (format t "Max error: ~a~%" max-error))
       finally (return (values (list ws bs) nil)))))

(defun answer (network input)
  (let ((x (make-matrix (length input) 1)))
    (loop for row below (dim x 0)
       do (setf (aref x row 0) (nth row input)))
    (let ((zs (forward-prop x (network-weight network) (network-bias network))))
      (aref (car (last zs))
            0 0))))

(defmacro setf-network (var nodes file &key input-count output-count learning-count)
  (let ((gnn (gensym))
        (gresult (gensym)))
    `(multiple-value-bind (,gnn ,gresult)
         (learn (make-network ,nodes)
                (read-training-file ,file
                                    :input-count ,input-count
                                    :output-count ,output-count)
                :learning-count ,learning-count)
       (when ,gresult
         (setf ,var ,gnn)))))
