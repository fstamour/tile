;;;; control-code.lisp

(in-package #:tile)

(defvar *attributes*
  '(reset-all-attributes
    bright
    dim
    underscore
    blink
    reverse
    hidden))

(defvar *colors*'
  (black
   red
   green
   yellow
   blue
   magenta
   cyan
   white))

(defun esc (suffix)
  (format t "~C~A" #\Esc suffix))

(defun esc* (suffix &rest attributes)
  (format t "~C[~{~D~^;~}~A" #\Esc attributes suffix))

(defun reset ()
  (esc "c"))

(defun enable-line-wrap () (esc "[7h"))
(defun disable-line-wrap () (esc "[7l"))

(defun set-cursor-position (row column)
  (esc* "f" row column))

(defun set-attributes (stream &rest attributes)
  (format stream "~C[~{~D~^;~}m" #\Esc attributes))

(defun foreground-color (color)
  (+ 30 (position color *colors*)))

(defun background-color (color)
  (+ 40 (position color *colors*)))

(defun erase-begining-of-line ()
  (esc* "K" 1))

(defun erase-up ()
  "Erase the screen from the current line to the top."
  (esc* "J" 1))

(defun erase-screen ()
  (esc* "J" 2))

