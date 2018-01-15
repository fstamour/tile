;;;; utils.lisp

(in-package #:tile-utils)

(defun cat (&rest rest)
  "Coerce every argument as string and concatenate all. Returns a string."
  (format nil "~{~A~}" rest))

(defun make-adjustable-string (initial-string)
  (make-array (length initial-string)
              :fill-pointer (length initial-string)
              :adjustable t
              :initial-contents initial-string
              :element-type (array-element-type initial-string)))

(defun make-resizeable-array-of-string ()
  (make-array '(1)
              :fill-pointer 1
              :initial-contents (list (make-adjustable-string ""))
              :adjustable t))

(defun safe-subseq (string start &optional (end 0))
  "Like subseq, but clamps the start and end, also support negative number for ends."
  (let* ((length (length string))
         (start (min length (max 0 start)))
         (end (min (if (<= end 0)
                       (max 0 (+ length end))
                       (max length end))
                   length)))
    (subseq string start end)))

(defun vector-last (vector)
  (aref vector (1- (length vector))))

