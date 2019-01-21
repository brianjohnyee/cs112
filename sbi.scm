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



;; brians yee
;; our stuff


;; define hashtabels

(define *hash* (make-hash))
(define *functionHash* (make-hash))
(define *variable-table* (make-hash))

;; hashtabel get/put functions to make it simpler
;; from symbols.scm
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

;; putting keys and values into hashtable *functionHash*
;; from symbols.scm
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
        (let,     (lambda (x y) (let-sbir x y)))
        (dim,  (lambda (x y) (dim-sbir x y)))

        ; (let,     (lambda (x y) (let-sbir x y)))
        ; (input,   (lambda (x) (let-sbir x (read)) (size (variable-get x))))
        ; (goto,    (lambda (x) (lable-get x)))
     ))





;; input is a list, which we pass in when we do map(cdr)
;; so i believe we do apply car(list) "print" and map on to
;; the cdr(list)
(define (printer input)
    ;; put it in () (wf) because if you dont, it will be put in a list
    ;;https://docs.racket-lang.org/guide/lambda.html
    (for-each 
        (lambda (wf)
            (display wf)
        ) 
        input

    )
    ;; can't put the new line inside lambda function 
    ;; cause when i did 02-exprs.sbir it would print the 
    ;; answer on a new line. but not exactly sure how it still
    ;; works when i do a newLine here
    (newline)
)

(define (let-sbir variable value)
    (if (var? value) 
        (let-sbir variable (variable-get value))
        (if (list? variable)
            (if (var? (cadr variable)) 
                (vector-set! (variable-get (car variable)) (-(variable-get (cadr variable))1) value)
                (vector-set! (variable-get (car variable)) (-(cadr variable)1) value)
            )
            (variable-put! variable value)
        )
    )
)

(define (dim-sbir variable value)
    (newline)
    (display "in funciton")
    (newline)
    (display variable)
    (newline)
    (display value)
    (newline)
    (display (make-vector 10))
    (display (make-vector value 1.0))
)


;; when '/' is called from sbir file
;; need to probably change later when divide by 0
;; stackoverflow source on how to do divison
;; https://stackoverflow.com/questions/1025390/how-do-i-get-mit-scheme-to-return-a-floating-point-number
(define (divide-sbir x y)
    (exact->inexact (/ x y))
)

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
                        (hash-set! *hash* (cadar list) (caar list))
                        (+ 1 1)  
                    )
                (+ 1 1)
                )
            )
        )
        (put-in-hash (cdr list))
    )
)


;; interpret-program function
;; outline part b

(define(interpret-program list)
    (when(not(null? list))
        (let ((first(caar list)))
            (if(null? (cdar list)) ;; the line only contains line number. believe this will be the first if statement and the rest of the ifs are nested in here
                (interpret-program (cdr list)) ;;https://stackoverflow.com/questions/30041672/if-else-clause-or-cond-in-racket
                (if(list? (cadar list)) ;; (# (statement)) this is a statement because it is a list in the list therefore statement
                    (cond 
                        ((equal? 'print (caadar list))
                            (begin ;; how to use 'begin' to have multiple statements https://stackoverflow.com/questions/11263359/is-there-a-possibility-of-multiple-statements-inside-a-conditional-statements-b
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
                            (display (cadar list))
                            ; (display (cadadr list))
                            (evalexpr (cadar list))
                            ;; move to the next line
                            (interpret-program (cdr list))
                        )
                    )
                    (+ 1 1)
                )
            )
        )
    )
)



;; need to create the function hash table, copy paste from symbol.scm 
;; and add in the other statements such as print, goto, etc.


;; modifiying symbols.scm
;; checks if it is in the function hash table
(define (function? function)
    (if (list? function)
        (if (procedure? (hash-ref *functionHash* (car function))) 
            #t 
            #f
        )
        #f)
)

(define (var? variable)
    (if (number? (variable-get variable )) #t #f)
)       





(define (evalexpr expr)
    (cond
        ((null? expr) 
            expr
        )
        ((number? expr) 
            expr
        )
        ((string? expr) 
            ;; the print statements 
            expr
        )
        ((var? expr) 
            ;; variables such as i j k etc
            (variable-get expr)
        )
        ;; checks if it is
        ((function? expr)

            (cond
                ((equal? (car expr) 'let)
                    ; (display (cadr expr))
                    (let-sbir (cadr expr) (evalexpr (caddr expr)))
                )
                ((equal? (car expr) 'print)
                    (display "print")
                    (apply(function-get (car expr)) (map evalexpr (cdr expr)))
                )
                ((equal? (car expr) 'dim)

                    (newline)
                    (display expr)
                    (newline)
                    ;; cadadr is the variable
                    (display (cadadr expr))
                    (newline)
                    ;; caddr(cadr) is the number
                    (display (caddr(cadr expr)))
                    (newline)
                    ;; caadr is asub
                    (display (caadr expr))
                    (newline)
                    ; (display (cdr(caadr expr)))
                    (display "ehr")
                                      (newline)
                    (dim-sbir (cadadr expr) (caddr(cadr expr)))
                )
                (else
                    (display "huh")
                    (apply(function-get (car expr)) (map evalexpr (cdr expr)))
                )
            )
        )
    )

)



















;; symbols.scm
;; this function is being used to see if my hash tables
;; are being insertted correctly
;; basically prints out the key and value of hash table
(define (show label item)
        (newline)
        (display label) (display ":") (newline)
        (display item) (newline))

;; symbols.scm
;; this function is being used to see if my hash tables
;; are being insertted correctly
;; basically prints out the key and value of hash table
(define (what-kind value)
    (cond ((real? value) 'real)
          ((vector? value) 'vector)
          ((procedure? value) 'procedure)
          (else 'other)))
;; this is the main of the program
(define (main arglist)
    ;; this part checks if there are 2 arguments in terminal
    ;; if not goes to the function 'usage-exit' to print
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
               
               ;;my tester used to test if i was getting the label
               ; (display (cdar program))

               ;;this puts the labels of the input into a hash table 
               (newline)
               (put-in-hash program)
               (newline)
               ;; GETTING ERROR RIGHT HERE
               ;; SO THE PROBLEM IS INTERPRET-PROGRAM
               (interpret-program program)
                ; (display  (hash-ref *hash* ))
                (newline)
                (printf "~~~~~~~~ ~n")
                ;; this is to test if the label hash is being inputted correctly
                ; (hash-for-each *hash*
                ;     (lambda (key value)
                ;     (printf "~s : ~s = ~s~n" key (what-kind value) value))
                ; )
              (write-program-by-line sbprogfile program))))

(when (terminal-port? *stdin*)
      (main (vector->list (current-command-line-arguments))))













