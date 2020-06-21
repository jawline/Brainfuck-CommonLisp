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
(defconstant jump-back (make-symbol "jump back"))
(defconstant jump-forward (make-symbol "jump forward"))

(defun lex-to-token (word)
  (cond
    ((string= word "<") tape-left)
    ((string= word ">") tape-right)

    ((string= word "+") inc-cell)
    ((string= word "-") dec-cell)
 
    ((string= word ",") inp-char)
    ((string= word ".") outp-char)   

    ((string= word "]") jump-back)
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

(defclass interp-state ()
  (
    (
      program
      :initarg :program
      :accessor program
    )
    (
      data
      :initarg :data
      :accessor data
    )
    (
      instruction-pointer
      :initarg :instruction-pointer
      :accessor name
    )
    (
      data-pointer
      :initarg :data-pointer
      :accessor name
    )
  )
)

(defun bf-mk-state (program)
  (make-instance 'interp-state :program (make-array (length program) :initial-contents program) :data (make-array 30000) :instruction-pointer 0 :data-pointer 0)
)

(defun print-state (state)
  (progn
    (print (slot-value state 'program))
    (print (slot-value state 'instruction-pointer))
    (print (slot-value state 'data-pointer))
  )
)

(defun bf-step-inner (state)
  (symbol-macrolet
    (
      (program (slot-value state 'program))
      (data (slot-value state 'data))
      (ip (slot-value state 'instruction-pointer))
      (dp (slot-value state 'data-pointer))
      (data-under-dp (aref data dp))
      (current (first (aref program ip)))
    )
    (cond

      ; IO
      ((eq current inp-char) (
        progn
        (setf data-under-dp (read-char *standard-input*))
        (setf ip (+ ip 1))
      ))

      ((eq current outp-char) (
        progn
        (print (code-char data-under-dp))
        (setf ip (+ ip 1))
      ))

      ; shift DP left and right
      ((eq current tape-left) (
        progn
        (setf dp (- dp 1))
        (setf ip (+ ip 1))
      ))

      ((eq current tape-right) (
        progn
        (setf dp (+ dp 1))
        (setf ip (+ ip 1))
      ))

      ; INC and DEC data
      ((eq current inc-cell) (
        progn
        (setf data-under-dp (+ data-under-dp 1))
        (setf ip (+ ip 1))
      ))

      ((eq current dec-cell) (
        progn
        (setf data-under-dp (- data-under-dp 1))
        (setf ip (+ ip 1))
      ))

      ; Jump back and forward
      ((eq current jump-back) (
        progn
        (print "FK")
        (exit) 
      ))

      ((eq current jump-forward) (
        progn
        (print "UN")
        (exit)
      ))
    )
  )
)

(defun bf-step-program (state)
  (if 
      (<
        (slot-value state 'instruction-pointer)
        (length (slot-value state 'program))
      )
      (progn
       ;(print "CAN EXEC")
       (bf-step-inner state)
       ;(print-state state)
      )
      nil 
  )
)

(defun bf-run-program (state)
  (if
      (bf-step-program state)
      (bf-run-program state)
      nil 
  )
)

(with-open-file (f (merge-pathnames "./hello_world.bf"))
  (let ((tokens (parse-stream f)))
    (bf-run-program (bf-mk-state tokens))
  )
)

(exit)
