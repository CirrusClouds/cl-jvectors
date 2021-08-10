;;;; package.lisp

(defpackage #:cl-jvectors
  (:use #:cl)
  (:export :matrix
   :mfill
           :m+
   :m++
           :m*
   :m**
           :Imatrix
   :make-sub-matrix
           :minor-matrix
   :determinant
           :transpose
   :cofactor
           :adjugate
           :invert-matrix))
