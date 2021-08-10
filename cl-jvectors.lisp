;;;; cl-jvectors.lisp

(in-package #:cl-jvectors)

(setf cl-utter:*log-level* 'debug)

(defclass matrix ()
  ((rows
    :initarg :rows
    :accessor m/rows
    :initform (error "Must specify number of rows when defining a matrix"))
   (columns
    :initarg :columns
    :accessor m/columns
    :initform (error "Must specify number of columns when defining a matrix"))
   (data
    :initarg :data
    :accessor m/data)))

(cl-utter:deftest matrix-class-test
  (let ((matrix1 (make-instance 'matrix :rows 5 :columns 3)))
    (cl-utter:assertion eq (m/rows matrix1) 5)
    (cl-utter:assertion eq (m/columns matrix1) 3)))


(defmethod initialize-instance :after ((m matrix) &key)
  (with-slots (rows columns data) m
    (assert (> rows 0))
    (assert (> columns 0))
    (progn
      (setf data (make-array (list rows columns)
                             :initial-element 0.0))
      )))

(cl-utter:deftest init-matrix-test
  (let ((matrix1 (make-instance 'matrix :rows 4 :columns 2)))
    (cl-utter:assertion eq (aref (m/data matrix1) 0 0) 0.0)))


(defmethod mfill (val (m1 matrix))
  (setf (m/data m1) (make-array (list (m/rows m1) (m/columns m1))
                                :initial-element val)))


(defmethod m+ ((m1 matrix) (m2 matrix))
  (cond ((and (= (m/rows m1) (m/rows m2))
              (= (m/columns m1) (m/columns m2)))
         (let ((m3
                 (make-instance 'matrix :rows (m/rows m1) :columns (m/columns m1))))
           (mapc (lambda (i)
                   (mapc (lambda (j)
                           (setf (aref (m/data m3) i j) (+ (aref (m/data m1) i j) (aref (m/data m2) i j))))
                         (cl-utils:range 0 (- (m/columns m1) 1))))
                 (cl-utils:range 0 (- (m/rows m1) 1)))
           m3))
        (t
         (error "To add matrices, they must be the same dimension"))))

(defmethod m+ (scalar (m1 matrix))
  (let ((m3
          (make-instance 'matrix :rows (m/rows m1) :columns (m/columns m1))))
    (mapc (lambda (i)
            (mapc (lambda (j)
                    (setf (aref (m/data m3) i j) (+ scalar (aref (m/data m1) i j))))
                  (cl-utils:range 0 (- (m/columns m1) 1))))
          (cl-utils:range 0 (- (m/rows m1) 1)))
    m3))

(defmethod m+ ((m1 matrix) scalar)
  (m+ scalar m1))

(defmethod m+ (scalar1 scalar2)
  (+ scalar1 scalar2))

(defun m++ (&rest args)
  (reduce #'m+ args))

(cl-utter:deftest add-matrix-test
  (let ((matrix1 (make-instance 'matrix :rows 4 :columns 2))
        (matrix2 (make-instance 'matrix :rows 4 :columns 2)))
    (setf (aref (m/data matrix1) 0 0) 3)
    (setf (aref (m/data matrix2) 0 0) 5)
    (cl-utter:assertion eq (aref (m/data (m++ matrix1 matrix2 3 5)) 0 0) 16)))


(defmethod m* ((m1 matrix) (m2 matrix))
  (let ((m3
          (make-instance 'matrix :rows (m/columns m2) :columns (m/rows m1))))
    (mapcar (lambda (i k)
              (setf (aref (m/data m3) i k)
                    (reduce #'+
                            (mapcar (lambda (j l)
                                      (* (aref (m/data m1) i j) (aref (m/data m2) l k)))
                                    (cl-utils:range 0 (- (m/columns m1) 1))
                                    (cl-utils:range 0 (- (m/rows m2) 1))))))
            (cl-utils:range 0 (- (m/rows m1) 1))
            (cl-utils:range 0 (- (m/columns m2) 1)))
    m3))

(defmethod m* (scalar (m1 matrix))
  (let ((m3
          (make-instance 'matrix :rows (m/rows m1) :columns (m/columns m1))))
    (mapc (lambda (i)
            (mapc (lambda (j)
                    (setf (aref (m/data m3) i j) (* scalar (aref (m/data m1) i j))))
                  (cl-utils:range 0 (- (m/columns m1) 1))))
          (cl-utils:range 0 (- (m/rows m1) 1)))
    m3))

(defmethod m* ((m1 matrix) scalar)
  (m* scalar m1))

(defmethod m* (scalar1 scalar2)
  (* scalar1 scalar2))

(defmethod m** (&rest args)
  (reduce #'m* args))

(cl-utter:deftest multiply-matrix-test
  (let ((matrix1 (make-instance 'matrix :rows 4 :columns 2))
        (matrix2 (make-instance 'matrix :rows 2 :columns 4)))
    (setf (aref (m/data matrix1) 0 0) 3)
    (setf (aref (m/data matrix2) 0 0) 5)
    (setf (aref (m/data matrix1) 1 1) 3)
    (setf (aref (m/data matrix2) 1 1) 5)
    (cl-utter:assertion eq (aref (m/data (m** matrix1 matrix2 3)) 0 0) 45.0)))


(defun Imatrix (n)
  (let ((identity (make-instance 'matrix :rows n :columns n)))
    (mapc (lambda (i j)
            (setf (aref (m/data identity) i j) 1))
          (cl-utils:range 0 (- n 1))
          (cl-utils:range 0 (- n 1)))
    identity))

(cl-utter:deftest identity-test
  (let ((identitytest (Imatrix 3)))
    (cl-utter:assertion eq (m/rows identitytest) 3)
    (cl-utter:assertion eq (aref (m/data identitytest) 2 2) 1)))


(defmethod make-sub-matrix (x y (m1 matrix))
  (let ((submatrix (make-instance 'matrix :rows (- (m/rows m1) 1) :columns (- (m/columns m1) 1))))
    (mapc (lambda (i k)
            (mapc (lambda (j l)
                    (setf (aref (m/data submatrix) l k) (aref (m/data m1) j i)))
                  (remove-if (lambda (i1) (= i1 x)) (cl-utils:range 0 (- (m/columns m1) 1)))
                  (cl-utils:range 0 (- (m/columns submatrix) 1))))
          (remove-if (lambda (j1) (= j1 y)) (cl-utils:range 0 (- (m/rows m1) 1)))
          (cl-utils:range 0 (- (m/rows submatrix) 1)))
    submatrix))

(cl-utter:deftest submatrix-test
  (let ((matrix1 (make-instance 'matrix :rows 3 :columns 3)))
    (setf (aref (m/data matrix1) 1 1) 10)
    (cl-utter:assertion eq (aref (m/data (make-sub-matrix 0 0 matrix1)) 0 0) 10)))


(defmethod minor-matrix ((m1 matrix))
  (let ((m3 (make-instance 'matrix :rows (m/rows m1) :columns (m/columns m1))))
    (mapc (lambda (i)
            (mapc (lambda (j)
                    (setf (aref (m/data m3) i j)
                          (determinant (make-sub-matrix i j m1))))
                  (cl-utils:range 0 (- (m/columns m1) 1))))
          (cl-utils:range 0 (- (m/rows m1) 1)))
    m3))

(cl-utter:deftest minor-matrix-test
  (let ((matrix1 (make-instance 'matrix :rows 3 :columns 3)))
    (mfill 3 matrix1)
    (setf (aref (m/data matrix1) 1 1) 0)
    (setf (aref (m/data matrix1) 0 0) 10)
    (cl-utter:assertion eq (aref (m/data (minor-matrix matrix1)) 0 0) -9)))


(defmethod determinant ((m1 matrix))
  (cond ((= (m/rows m1) (m/columns m1))
         (case (m/rows m1)
           (1 (aref (m/data m1) 0 0))
           (2 (- (* (aref (m/data m1) 0 0) (aref (m/data m1) 1 1))
                 (* (aref (m/data m1) 1 0) (aref (m/data m1) 0 1))))
           (3 (reduce #'+ (list (* (aref (m/data m1) 0 0)
                                   (determinant (make-sub-matrix 0 0 m1)))
                                (- (* (aref (m/data m1) 1 0)
                                      (determinant (make-sub-matrix 1 0 m1))))
                                (* (aref (m/data m1) 2 0)
                                   (determinant (make-sub-matrix 2 0 m1))))))
           (otherwise
            (error "sorry, can't calc this"))))
        (t
         (error "Only square matrices have determinants"))))

(cl-utter:deftest determinant-test
  (let ((matrix1 (make-instance 'matrix :rows 3 :columns 3)))
    (mfill 1 matrix1)
    (setf (aref (m/data matrix1) 0 2) 0)
    (setf (aref (m/data matrix1) 2 0) 0)
    (cl-utter:assertion eq (determinant matrix1) -1)))



(defmethod transpose ((m1 matrix))
  (let ((m3 (make-instance 'matrix :rows (m/columns m1) :columns (m/rows m1))))
    (mapc (lambda (i)
            (mapc (lambda (j)
                    (setf (aref (m/data m3) j i) (aref (m/data m1) i j)))
                  (cl-utils:range 0 (- (m/columns m1) 1))))
          (cl-utils:range 0 (- (m/rows m1) 1)))
    m3))

(cl-utter:deftest transpose-test
  (let* ((matrix1 (make-instance 'matrix :rows 5 :columns 3)))
    (setf (aref (m/data matrix1) 0 2) 8)
    (cl-utter:assertion eq (m/rows (transpose matrix1)) 3)
    (cl-utter:assertion eq (aref (m/data (transpose matrix1)) 2 0) 8)))


(defmethod cofactor ((m1 matrix))
  (let ((m3 (minor-matrix m1)))
    (mapc (lambda (i)
            (mapc (lambda (j)
                    (if (evenp (+ i j))
                        (setf (aref (m/data m3) i j) (aref (m/data m3) i j))
                        (setf (aref (m/data m3) i j) (- (aref (m/data m3) i j)))))
                  (cl-utils:range 0 (- (m/columns m1) 1))))
          (cl-utils:range 0 (- (m/rows m1) 1)))
    m3))

(cl-utter:deftest cofactor-test
  (let ((matrix1 (make-instance 'matrix :rows 3 :columns 3)))
    (mfill 1 matrix1)
    (setf (aref (m/data matrix1) 0 2) 0)
    (setf (aref (m/data matrix1) 1 1) 10)
    (cl-utter:make-log 'debug "minor matrix is ~a" (m/data (cofactor matrix1)))
    (cl-utter:make-log 'debug "cofactor matrix is ~a" (m/data (minor-matrix matrix1)))
    (cl-utter:make-log 'debug "Adjugate matrix is ~a" (m/data (adjugate matrix1 )))
    (cl-utter:assertion eq (aref (m/data (cofactor matrix1)) 1 1) 1)))


(defmethod adjugate ((m1 matrix))
  (let ((m3 (cofactor m1)))
    (transpose m3)))

(defmethod invert-matrix ((m1 matrix))
  (if (eq (determinant m1) 0)
      (error "No inversion of this matrix exists, as the determinant is 0")
      (let ((m3 (adjugate m1))
            (det (determinant m1)))
        (mapc (lambda (i)
                (mapc (lambda (j)
                        (setf (aref (m/data m3) i j) (* (/ 1 det) (aref (m/data m3) i j))))
                      (cl-utils:range 0 (- (m/columns m3) 1))))
              (cl-utils:range 0 (- (m/rows m3) 1)))
        m3)))

(cl-utter:deftest inversion-test
  (let ((matrix1 (make-instance 'matrix :rows 3 :columns 3)))
    (mfill 10 matrix1)
    (setf (aref (m/data matrix1) 0 1) 5)
    (setf (aref (m/data matrix1) 1 1) 1)
    (setf (aref (m/data matrix1) 0 0) 3)
    (cl-utter:make-log 'debug "Data is ~a" (m/data matrix1))
    (cl-utter:make-log 'debug "Determinant is ~a" (determinant matrix1))
    (cl-utter:make-log 'debug "Minor matrix is ~a" (m/data (minor-matrix matrix1)))
    (cl-utter:make-log 'debug "Adjugate matrix is ~a" (m/data (adjugate matrix1)))
    (cl-utter:make-log 'debug "Inverse is ~a" (m/data (invert-matrix matrix1)))
    (cl-utter:make-log 'debug "Multiplying calculated inverse and original matrix... ~a" (m/data (m* matrix1 (invert-matrix matrix1))))
    (cl-utter:assertion eq (aref (m/data (m* matrix1 (invert-matrix matrix1))) 0 0)
                        (aref (m/data (Imatrix 3)) 0 0))
    (cl-utter:assertion eq (aref (m/data (m* matrix1 (invert-matrix matrix1))) 1 1)
                        (aref (m/data (Imatrix 3)) 1 1))
    (cl-utter:assertion eq (aref (m/data (m* matrix1 (invert-matrix matrix1))) 2 2)
                        (aref (m/data (Imatrix 3)) 2 2))))
