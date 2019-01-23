#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.8 2019-01-11 17:38:01-08 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; GIVEN

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))


(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

;; this is the error message if you dont have 2 arguments
;; *run-file* = sbi.scm 
;; filename = sbir file
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

;; reads the sbir file
;; beleve its called "readlist" because sbir file 
;; is in the format of a list
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))

;; this prints of the sbir file
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; modified from symbols.scm
;; defining hashtables, hashtable functions, and initialing specifc 
;; keys and values into function and variable hashtable.

;; label hashtable
(define *hash* (make-hash))
;; function hashtable
(define *functionHash* (make-hash))
;; variable hashtable
(define *variable-table* (make-hash))

;; from symbols.scm
;; hashtabel get/put functions to make it simpler
(define (function-get key)
        (hash-ref *functionHash* key '(no such key in
                                         *functionHash*)))
(define (function-put! key value)
        (hash-set! *functionHash* key value))

(define (variable-get key)
        (hash-ref *variable-table* key '(no such key in 
                                          variable-table)))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))

(define (label-get key)
        (hash-ref *hash* key '(no such key in 
                                          hash)))
(define (label-put! key value)
        (hash-set! *hash* key value))

;; putting keys and values into hashtable *functionHash*

(for-each
    (lambda (item)
             (function-put! (car item) (cadr item)))
    `(
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))  
        (log10_2 0.301029995663981195213738894724493026768189881)
        (log, (lambda(x)(log (if (equal? x 0) 0.0 x))))
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (sqrt, sqrt)
        (+ ,+)
        (- ,-) 
        (* ,*)
        (/       ,(lambda (x y)  (divide-sbir x y)))
        (%       ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y)))) 
        (abs ,abs) 
        (<       , <)
        (>       , >)
        (=       , (lambda (x y) (eqv? x y)))
        (<=      , (lambda (x y) (<= x y)))
        (>=      , (lambda (x y) (>= x y)))
        (<>      ,(lambda (x y) (not (equal? x y))))
        (^       ,(lambda (x y) (expt x y)))
        (floor, floor)
        (ceil, ceiling) 
        (round, round)
        (exp, exp)
        (sin, sin) 
        (cos, cos) 
        (tan, tan)
        (asin, asin)
        (acos, acos) 
        (atan,    (lambda(x)(atan (if (equal? x 0) 0.0 x))))
        (print,  (lambda poop (printer poop)))
        (let,     (lambda (x y) (letStatement x y)))
        (dim,  (lambda (x y) (dim-sbir x y)))
        (goto, (lambda (x) (label-get x)))
        (input   ,(lambda (x) (inputer x)))
     ))

;; putting in pi and e in variable hashtable to satisfy 25-pi-e-fns.sbir
(for-each
    (lambda (item)
             (variable-put! (car item) (cadr item)))
    `(
        (counter 0)
        (pi 3.141592653589793)
        (e 2.718281828459045)
     ))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;; this is from listhash.scm and will be modified for our use
;; Part a) of suggested outline "check each line for a label."

;; EXPLANATION
;; Because the sbir input is a list in a list, we have to use
;; 'caar' to the "first list", and then car that list, to get
;; the line number, since the line number will always be the 
;; car of its list. Next we check if within this list if there
;; is anything else besides the line number. If there are more
;; entries within that list, meaning the "cdar" is not null
;; we check the car (cadar) of the next element of the list to see 
;; if it is a symbol. If it is a symbol, it is a label, thus
;; we put it in our hash tabel. 
;; The (+ 1 1) is the else statements, since I guess you need
;; to have else statement for if statements. so it is filler?



(define (put-in-hash list)
    (when (not (null? list))
        (let ((first(caar list)))
            (when (number? first)
                (if (not(null? (cdar list)))
                    (if(symbol? (cadar list))
                        (hash-set! *hash* (cadar list)  list)
                        (+ 1 1)  
                    )
                (+ 1 1)
                )
            )
        )
        (put-in-hash (cdr list))
    )
)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; interpret-program function
;; outline part b

(define (interpret-program list)
    (cond
        ((null? list)
            list
        )
        ((null? (cdar list))
            (interpret-program (cdr list))
        )
        ((list? (cadar list))
            (cond 
                ((equal? 'print (caadar list))
                    (begin 
                        (evalexpr (cadar list))
                        ;; move to the next line
                        (interpret-program (cdr list))
                    )
                )
                ((equal? 'let (caadar list))
                    ;; cadar gets the statement which is in a list
                    (evalexpr (cadar list))
                    ;; move to the next line
                    (interpret-program (cdr list))
                )
                ((equal? 'dim (caadar list))
                    ; (display (cadar list))
                    ; (display (cadadr list))
                    (evalexpr (cadar list))
                    ;; move to the next line
                    (interpret-program (cdr list))
                )
                ((equal? 'goto (caadar list))
                    ; (display (cadar list))
                    (interpret-program (evalexpr (cadar list)))
                )
                ((equal? 'if (caadar list))
                    ; (newline)
                    ; (display (car(cdadar list)))
                    ; (newline)
                    ; (display (cadr(cdadar list)))
                    ; (newline)
                    ; (display (cdr list))
                    ; (newline)
                    (interpret-program(ifStatement (car(cdadar list)) 
                      (cadr(cdadar list))  list))

                )
                ((equal? 'input (caadar list))
                    (evalexpr (cadar list))
                    (interpret-program (cdr list))
                )
                (else 
                    (evalexpr (cadar list)) 
                    (inter (cdr list))
                )
            )
        )
            ((null? (cddar list))
                (interpret-program (cdr list))
            )
            ;; if there is (# label (statement))
            (else
                (cond
                    ((equal? 'goto (car (caddar list)))
                        (interpret-program (evalexpr (caddar list)))
                    )
                    ((equal? 'if (car(caddar list)))
                    (interpret-program(ifStatement (cadr(caddar list)) 
                          (caddr(caddar list)) list))
                    )
                    (else
                            (evalexpr (caddar list))
                            (interpret-program (cdr list))
                        )
                    )
                )  
        
    )
)

;; modifiying symbols.scm
;; These functions are called by (evalexp expr)
;; These are to check what kind of statement is passed in
;; to determine how to evaluate the expression
;; What is being passed in is a LIST because the format 
;; of SBIR files is (# LABEL (statement))

;; This checks if the car of the statement is in the function hashtable.
;; if so, return true, else return false.
(define (function? function)
    (if (list? function)
        (if (procedure? (hash-ref *functionHash* (car function))) 
            #t 
            #f
        )
        #f)
)


;; This checks the variable hashtable to see if the variable
;; is set to a number. If so, return true, else returns false.
(define (var? variable)
    (if (number? (variable-get variable )) 
        #t 
        #f
    )
)       




;; EVALEXP FUNCTIONS
;; INPUT Statement
(define (inputer . expr)
    (define (get-input expr)
        (when (not (null? (car expr)))
            (variable-put! (car expr) (void))
            (let ((object (read)))
                (cond 
                    ((eof-object? object)
                        (variable-put! 'counter -1)
                    )
                    ((number? object)
                        (variable-put! (car expr) object)
      (variable-put! 'counter (+ (variable-get 'counter) 1))
                    )
                    (else 
                        (begin (printf "invalid number: ~a~n" object))
                    )
                  )
            ) 
            (when (not (null? (cdr expr)))
            (get-input (cdr expr)))
        )
    )
    (get-input expr)
)


;; this funciton is in the function hashtable
;; input is a list, which we pass in 
;;when we do map(cdr) from evalexp "print".
;; Then for each item in the list, we display it 
;;and an extra white space afterwards
(define (printer input)
    ;; put it in (), (wf) because if you dont, it will be put in a list
    ;;https://docs.racket-lang.org/guide/lambda.html
    (for-each 
        (lambda (wf)
            (display wf)
            (display " ")
        ) 
        input

    )
    ;; can't put the new line inside lambda function 
    ;; cause when i did 02-exprs.sbir it would print the 
    ;; answer on a new line. but not exactly sure how it still
    ;; works when i do a newLine here
    (newline)
)

;; this function is called if we are printing elements of an array
;; the only problem with this is the index must 
;;be given in terms of a variable
;; because the function takes in 2 variables not (variable number)
(define (print-array variable1 variable2)
    (display  (vector-ref (variable-get variable1)  
      (- (variable-get variable2) 1)))
    (newline)
    ; (display (hash-ref *variable-table* variable1))
)

;; This checks the variable hashtable to see if the variable
;; is set to a number. If so, return true, else returns false.
; (define (var? variable)
;     (if (number? (variable-get variable )) 
;         #t 
;         #f
;     )
; )    

;; need to go over this one
;; only goes through inhere1
;; need to check that out with rest of the 
(define (letStatement variable value)

            (begin
                (variable-put! variable value)
                ; (display "in here1")
                ; (newline)
            )

)

;; this sets a specific index of an array to be the value
;; variable1 is the name of the array, variable2 
;;is the index of the array
;; and value is what to change the array to
(define (sbir-array variable1 variable2 value)
    ; (display variable1)
    ; (display variable2)
    ; (display value)
    ; (vector-ref (variable-get variable1)  (variable-get variable2))

    (vector-set! (variable-get variable1) 
    (- (variable-get variable2) 1) value)
)

;; x is the statement to be evaluated which is the loop control
;; y is the label, which you get the value, 
;;which is the top level of the list
;; z is the list (rest of the program after loop)
;; if x is true, get label y, else move on to rest of program
(define (ifStatement x y z)
  ; (display "in ifStatement")
  ;   (display x)
  ;       (display y)
  ;           (display z)
    (if (evalexpr x)
        (label-get y)
        (cdr z)
    )
)

;; not in use doesnt work
(define (superIf x y)

    (if ((function-get '=) (vector-ref (variable-get (car(cdadar x)))  
      (variable-get (cadr(cdadar x)))) (caddar x))
        (label-get (cadr x))
        (begin
        ; (display "hi")
        ; (newline) 
        ; (display (cdr y))
        (cdr y)
        )
    )
)

;; this function initalizes an array 
;; variable is the name of the array, value is the size of the array
;; i initalize each index to 0.0
(define (dim-sbir variable value)

    (if (number? value)
    ; (newline)
    ; (display "in funciton")
    ; (newline)
    ; (display variable)
    ; (newline)
    ; (display value)
    ; (newline)
    ;; puts the array in the variable hash table
        (variable-put! variable (make-vector value 0.0))
        (variable-put! variable (make-vector (variable-get value) 0.0))


    )
    ; (display (hash-ref *variable-table* variable))

    ; (display (make-vector 10))
    ; (display (make-vector value 1.0))
)


;; when '/' is called from sbir file
;; need to probably change later when divide by 0
;; stackoverflow source on how to do divison
(define (divide-sbir x y)
    (if (= y 0)
        (if (= x 0)
            "nan.0"
            (if (> x 0)
                "+inf.0"
                "-inf.0" 
            )
        )
        (exact->inexact (/ x y))
    )

)


;; MODIFIED FROM evalexp.scm
;; Determines what kind of statement is being passed in.
;; The statements are then evaluated passed on what its kind.
;; main changes are if the statement is a function? or a var?

(define (evalexpr expr)
    (cond 
        ((null? expr) 
            expr
        )
        ((number? expr) 
            expr
        )
        ((string? expr) 
            expr
        )
        ((var? expr) 
            (variable-get expr)
        )
        ((function? expr) 
            (cond
                ((equal? (car expr) 'let) 
                    (if(list? (cadr expr))
                        (begin
                            (sbir-array (cadadr expr) 
                            (car(cddadr expr)) (caddr expr))
                        )
                        (begin
                            (letStatement (cadr expr) (evalexpr 
                              (caddr expr)))
                        )
                    )
                )
                ((equal? (car expr) 'print)
                    (if(null? (cdr expr))
                        (newline)
                        (if(list? (cadr expr))
                            (begin
                                (print-array (cadadr expr) 
                                (car(cddadr expr)))
                            )
                            (begin
                                (apply(function-get (car expr)) 
                                (map evalexpr (cdr expr)))
                            )
                        )
                    )
                )
                ((equal? (car expr) 'dim)
                    (dim-sbir (cadadr expr) (caddr(cadr expr)))
                )
                ((equal? (car expr) 'input)
                    (apply inputer (cdr expr))
                )
                (else 
                    (apply(function-get (car expr)) 
                      (map evalexpr (cdr expr)))
                )   
            )
        )
        ((pair? expr) 
            (map evalexpr expr)
        )
        (else 
            expr
        )
    )
)         


;; TESTING HASH TABLES

;; FROM symbols.scm
;; this function is just being used to see if
;; hash tables are being inserted correctly
;; I don't call it, just for testing purpose
(define (show label item)
        (newline)
        (display label) (display ":") (newline)
        (display item) (newline))

;; FROM symbols.scm
;; this function is just being used to see if
;; hash tables are being inserted correctly
;; I don't call it, just for testing purpose

(define (what-kind value)
    (cond ((real? value) 'real)
          ((vector? value) 'vector)
          ((procedure? value) 'procedure)
          (else 'other)))
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`



;; FUNCTION MAIN
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              
              ;; given function which cats the SBIR file
              ; (write-program-by-line sbprogfile program)
              ;               (printf "~~~~~~~~ ~n")
               ;; this checks to see if the SBIR file has labels
               ;; if a line contains a label, 
               ;;it will put the label (key)
               ;; in the label hashtable (*hash), and the value will 
               ;; everything from the start of that line number
               (put-in-hash program)

               ;; interpret-program reads each line of the SBIR file
               ;; and calls evalexp depending on what the task.
               (interpret-program program))))


(main (vector->list (current-command-line-arguments))) 












