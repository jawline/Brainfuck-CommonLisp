(define-condition lex-error (error) ())

; Moves which shift the tape one index to the left or right
(defconstant tape-left (make-symbol "tape left"))
(defconstant tape-right (make-symbol "tape right"))

; Increment or decrement the cell currently pointed it
(defconstant inc-cell (make-symbol "increment cell"))
(defconstant dec-cell (make-symbol "decrement cell"))

; IO
(defconstant inp-char (make-symbol "read character"))
(defconstant outp-char (make-symbol "write character"))

; Control flow
(defconstant jump-forward (make-symbol "jump forward"))

(defun lex-to-token (word)
  (cond
    ((string= word "<") tape-left)
    ((string= word ">") tape-right)

    ((string= word "+") inc-cell)
    ((string= word "-") dec-cell)
 
    ((string= word ",") inp-char)
    ((string= word ".") outp-char)   

    ((string= word "[") jump-forward)

    (t (error (make-condition 'lex-error)))
  )
)

(defun validc (c)
  (and (not (eq c nil)) (not (eq c #\newline)))
)

(defun parse-stream (input)
  (let ((nextc (read-char input nil)))
    (cond
      ((eq nextc nil) nil)
      ((eq nextc #\[) (cons (list (lex-to-token nextc) (parse-stream input)) (parse-stream input)))
      ((eq nextc #\]) nil)
      ((eq nextc #\newline) (parse-stream input))
      (t (cons (list (lex-to-token nextc)) (parse-stream input)))
    )
  )
)

; ---- END OF PARSING ----

; ---- STATE ----
(defclass bf-state ()
  (
    (
      data
      :initarg :data
      :accessor data
    )
    (
      data-pointer
      :initarg :data-pointer
      :accessor name
    )
  )
)

(defun bf-mk-state ()
  (make-instance 'bf-state :data (make-array 30000) :data-pointer 0)
)
; ---- END OF STATE ----

; ---- CORE INTERP ----

(declaim (ftype (function) bf-run-program))

(defun bf-step-program (program ip state)
  (symbol-macrolet
    (
      (data (slot-value state 'data))
      (dp (slot-value state 'data-pointer))
      (data-under-dp (aref data dp))
      (current (first (aref program ip)))
      (subprogram (second (aref program ip)))
    )
    (cond
      ((>= ip (length program)) nil)
      ; IO
      ((eq current inp-char) (
        progn
        (setf data-under-dp (read-char *standard-input*))
        (+ ip 1)
      ))

      ((eq current outp-char) (
        progn
        (write (code-char data-under-dp))
        (+ ip 1)
      ))

      ; shift DP left and right
      ((eq current tape-left) (
        progn
        (setf dp (- dp 1))
        (+ ip 1)
      ))

      ((eq current tape-right) (
        progn
        (setf dp (+ dp 1))
        (+ ip 1)
      ))

      ; INC and DEC data
      ((eq current inc-cell) (
        progn
        (setf data-under-dp (+ data-under-dp 1))
        (+ ip 1)
      ))

      ((eq current dec-cell) (
        progn
        (setf data-under-dp (- data-under-dp 1))
        (+ ip 1)
      ))

      ; Looping
      ((eq current jump-forward) (
        progn
        (bf-run-program (make-array (length subprogram) :initial-contents subprogram) 0 state)
        (if (eq data-under-dp 0) (+ ip 1) ip)
      ))
    )
  )
)

(defun bf-run-program (program ip state)
  (let ((nip (bf-step-program program ip state)))
       (if nip (bf-run-program program nip state) nil))
)

; ---- END INTERP

(with-open-file (f (merge-pathnames "./hello_world.bf"))
  (let ((tokens (parse-stream f)))
    (bf-run-program (make-array (length tokens) :initial-contents tokens) 0 (bf-mk-state))
  )
)

(exit)
