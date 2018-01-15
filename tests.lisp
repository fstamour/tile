
(defpackage #:tile-test
  (:use
   #:cl
   #:tile-utils
   #:tile
   #:prove))

(in-package #:tile-test)

(defun join (sep &rest strings)
  "Join a bunch of strings"
  ;; this is vulnerable to user input.
  (format nil (cat "~{~A~^" sep "~}") strings))

(defun rep (n &rest rest)
  "Concatenate \"rest\" n times."
  (format nil "~v@{~{~A~}~:*~}" n rest))

#+nil
(with-output-to-terminal
    (set-attributes
     *standard-output*
     (foreground-color 'black)
     (background-color 'white))
  (reset))


#+nil
(defun print-escape (string)
  (if (stringp string)
      (loop :for char :across string :do
        (princ (char-name char)))
      (princ string)))

;; (print-escape (cat "sdf" #\Esc 'qwer))

(plan 5)
(is (safe-subseq "" 0 -1) "")
(is (safe-subseq "" -1 -1) "")
(is (safe-subseq "" -1 10) "")
(is (safe-subseq "" 10 10) "")
(is (safe-subseq "asdf" 0 -1) "asd")
(finalize)


(defun run-repl (input)
  "Setup some streams to test the repl"
  (with-input-from-string (input-stream input)
    (let* ((buffer nil)
          (output
            (with-output-to-string (output-stream)
              (let ((*query-io* (make-two-way-stream input-stream output-stream))
                    (*standard-input* input-stream)
                    (*standard-output* output-stream))
                (setf buffer (repl))))))
      (declare (ignorable output))
      (values buffer #+nil output
              (with-output-to-string (*standard-output*)
                              (tile::redraw buffer))))))

(defun sanitize (repl-output)
  "Remove known escape sequences"
  (cl-ppcre:regex-replace-all
   `(:sequence #\esc
               (:alternation
                "[2J" "[0;0f"
                (:regex "\\[\\d{1,2}(;\\d{1,2})*m")))
   repl-output
   ""))

(defun repl-test (&key input expected-sanitized-output)
  (multiple-value-bind (buffer last-output)
      (run-repl input)
    (declare (ignorable buffer))
    (let ((sanitized (sanitize last-output)))
      (when expected-sanitized-output
        (is sanitized
            expected-sanitized-output
            :test #'equalp)))))

(defun esc (&rest rest)
  (format nil "~C~{~A~}" #\esc rest))

(repl-test :input ""
           :expected-sanitized-output "~> ")

(repl-test :input (string #\Newline)
           :expected-sanitized-output
           (join #\newline "~> " "~> NIL" "~> "))

(repl-test :input (cat "42" #\Newline)
           :expected-sanitized-output
           (join #\newline "~> 42" "~> 42" "~> "))

(repl-test :input (cat "42" #\Rubout)
           :expected-sanitized-output
           (join #\newline "~> 4"))

(run-repl (cat "hello" (rep 2 #\Esc "[D")))

#+nil
(string=
 (let ((buffer (make-instance 'tile::buffer)))
   (with-output-to-string (*standard-output*)
     (tile::setup-some-standard-handlers buffer)
     (loop :for char :across "some random string"
           :do (tile::handle-character buffer char))
     (loop :for i :below 6 :do
       (tile::handle-character buffer #\rubout))
     (tile::redraw buffer)))
 "[2J[0;0f[32m~[0m> some random ")
