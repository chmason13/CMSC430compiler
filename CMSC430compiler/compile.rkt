#lang racket

(provide compile)

(require "top-level.rkt")
(require "desugar.rkt")
(require "cps.rkt")
(require "closure-convert.rkt")
(require "utils.rkt")

(define (compile e gc)
  ;Convert top-level expressions in scheme to readable input for desugaring pass
  (define scm-e (top-level e))
  
  ;(if (scheme-exp? scm-e)
  ;    (begin
  ;      (display "Valid Scheme\n")
  ;      (display (eval-scheme scm-e))
  ;      (display "\n"))
  ;    (display "Top-level Conversion Failed\n"))
  ;Desugar expressions to their simplest forms
  (define ir-e (simplify-ir (desugar scm-e)))
  
  ;(if (ir-exp? ir-e)
  ;    (begin
  ;      (display "Valid IR\n")
  ;      (display (eval-ir ir-e))
  ;      (display "\n"))
  ;    (display "Desugar Failed\n"))

  ;Removes set! by boxing all mutable local variables and removes shadowing by making
  ;all names unique to a single binding point
  (define alphatized (alphatize (assignment-convert ir-e)))
  
  ;(if (alphatized-exp? alphatized)
  ;    (begin 
  ;      (display "Valid Alphatization & Assignment Conversion\n")
  ;      (display (eval-ir alphatized))
  ;      (display "\n"))
  ;    (display "Assignment Conversion & Alphatization Failed\n"))

  ;Convert to Administrative Normal Forma
  (define anf (anf-convert alphatized))

  ;(if (anf-exp? anf)
  ;    (begin 
  ;      (display "Valid ANF Conversion\n")
  ;      (display (eval-ir anf))
  ;      (display "\n"))
  ;    (display "ANF Conversion Failed\n"))
  
  ;Convert intermediate representation into continuation passing style
  (define cps-e (cps-convert anf))
  
  ;(if (cps-exp? cps-e)
  ;    (begin 
  ;      (display "Valid CPS\n")
  ;      (display (eval-ir cps-e))
  ;      (display "\n"))
  ;    (display "CPS-Convert Failed\n"))
  ;Use bottum-up processing to closure convert all lambdas and build a list of procedures
  (define proc (closure-convert cps-e))

  ;(if (proc-exp? proc)
  ;    (begin
  ;      (display "Valid Procedure\n")
  ;      (display (eval-proc proc))
  ;      (display "\n"))
  ;    (display "Invalid Procedure\n"))  
  ;Walk procedures and emit llvm code as a string
  (define ll (proc->llvm proc gc))
  "")
  