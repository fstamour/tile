;;;; tile.lisp

(in-package #:tile)


(defun prompt1 (stream)
  "Print a colored \"~>\"."
  (set-attributes stream (foreground-color 'green))
  (write-char #\~)
  (set-attributes stream (position 'reset-all-attributes *attributes*))
  (write-char #\>)
  (write-char #\Space))

;; TODO Prompt2 - output a bunch of spaces to match the previous line's indentation.

(defun print-escape (string)
  "Take a string and replace #\Esc characters by \"^[\"."
  (if (stringp string)
      (princ
       (cl-strings:replace-all string (string #\Esc) "^["))
      (princ string)))

(defun redraw (buffer)
  "Erase the screen and draw the buffer's content."
  (with-slots (row col content) buffer
    (erase-screen)
    (set-cursor-position row col)
    (loop :for chunk :across content
          :for i :from 0
          :do
             (unless (= i 0)
               (fresh-line))
             (if (listp chunk)
                 (case (first chunk)
                   (:result (print-escape (second chunk)))
                   (t (print-escape chunk)))
                 (progn
                   (prompt1 *standard-output*)
                   (print-escape chunk))))
    (force-output)))

(defun evaluate-last-entry (buffer)
  "Take the last entry in the buffer's content, evaluates it."
  (let* ((eof (gensym "eof"))
         (string (alexandria:last-elt (content buffer)))
         (form (when (and string (stringp string))
                 (read-from-string string nil eof))))
    (when (and form
               (not (eq eof form)))
      (eval form)) ))


(defun submit (buffer)
  "Evaluate last entry and add it to the content."
  (awhen (evaluate-last-entry buffer)
    (vector-push-extend (list :result it)
                        (content buffer)))
  (push-content buffer (make-adjustable-string ""))
  (vector-push-extend (make-adjustable-string "")
                      (content buffer))
  ;; Save history
  (push-history buffer (get-last-content buffer)))

(defmethod enter ((buffer buffer) char)
  "Handle returns."
  (declare (ignore char))
  (submit buffer))

(defmethod backspace ((buffer buffer) char)
  (declare (ignore char))
  (let ((string (vector-last (content buffer))))
    (setf (aref (content buffer) (1- (length (content buffer))))
          (safe-subseq string 0 -1))))

(defmethod setup-some-standard-handlers ((buffer buffer))
  (set-handler buffer #\Page 'clear)
  (set-handler buffer t 'push-char)
  (set-handler buffer #\Newline 'enter)
  (set-handler buffer #\Rubout 'backspace))

;; #\Esc[A => Scroll up
;; #\Esc[B => Scroll down
;; #\Esc[C => Scroll right
;; #\Esc[D => Scroll left

(defun repl (&optional (buffer (make-instance 'buffer)))
  (let ((eof (gensym "eof")))
    (set-handler buffer eof (lambda (buffer char)
                              (declare (ignore buffer char))
                              :quit))
    (setup-some-standard-handlers buffer)
    (redraw buffer)
    (loop
      :for char = (trivial-raw-io:read-char *query-io* nil eof)
      :do
         (case (handle-character buffer char)
           (:quit (return-from repl buffer)))

         (if (and
              (> 2 (length (content buffer)))
              (string= (esc "[A")
                       (last-elt (content buffer))))
             '(todo get last history  (vector-push-extend (content buffer))))
         (redraw buffer))))


