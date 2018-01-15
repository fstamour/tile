;;;; buffer.lisp

(in-package #:tile)

;; Util
(defclass buffer ()
  ((row
    :accessor row
    :initarg :row
    :initform 0)
   (col
    :accessor col
    :initarg :col
    :initform 0)
   (width
    :accessor width
    :initarg :width
    :initform nil)
   (height
    :accessor height
    :initarg :height
    :initform nil)
   (content
    :accessor content
    :initarg :content
    :initform (make-resizeable-array-of-string))
   (handlers
    :accessor handlers
    :initform (make-hash-table))
   (parent
    :accessor parent
    :initarg parent
    :initform nil)
   (children
    :accessor children)
   (history
    :accessor history
    :initform (make-resizeable-array-of-string))))

;; TODO Could/Should call defgeneric explicitly

(defmethod handle-character ((buffer buffer) char-or-eof)
  "Look for an handler in the hashtable \"handlers\" and calls it returns true if it found one"
  (let ((handler (or (gethash char-or-eof (handlers buffer))
                     (gethash t (handlers buffer)))))
    (when handler
      (funcall handler buffer char-or-eof))))

(defun set-handler (buffer char handler)
  (setf (gethash char (handlers buffer)) handler))

(defmethod clear ((buffer buffer) char)
  (declare (ignore char))
  (setf (content buffer) (make-content)))

(defmethod push-char ((buffer buffer) (char character))
  (let ((string (vector-last (content buffer))))
    (setf (alexandria:last-elt (content buffer))
          (concatenate 'string string (princ-to-string char)))))

(defun get-last-content (buffer)
  (last-elt (content buffer)))

(defun push-content (buffer new-element)
  (vector-push-extend new-element (content buffer)))

(defun get-nth-history-entry (buffer n)
  (let ((history (history buffer)))
    (if (> 0 n) :todo
        )))

(defun push-history (buffer new-element)
  (vector-push-extend new-element (content buffer)))

