;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; sasm/arch/x64.sls - X64 assembler
;;;  
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

#!r6rs
(library (sasm arch x64)
    (export sasm-assemble)
    (import (rnrs)
	    (sasm arch output)
	    (sasm arch conditions)
	    (sasm arch x64 framework)
	    (sasm arch x64 registers)
	    (sasm arch x64 mnemonics))

;;; Syntax
;; The syntax of assembly is similar with sassy.
;;
;; toplevel ::= section* | implicit-section*
;; section  ::= (section "name" code-vector*)
;; implicit-section ::= code-vector*
;; code-vector ::= label | operation
;; label    ::= (label "name")
;; operation ::= (mnemonic operand*)
;;
;; TODO macro
(define (sasm-assemble expr)
  (define output (make-sasm-output))
  (for-each (lambda (e) (sasm-assemble1 output #f e)) expr)
  output)

;; output = SASM output
;; section = #f or section, where #f is implicit section (.text)
;; expr = expression
(define (sasm-assemble1 output section expr)
  (define (ensure-section section)
    (or section
	(sasm-output-create-section! output ".text")))
  (define (immeidate/register e)
    (cond ((pair? e) 
	   (case (car e)
	     ((&) (apply & (cdr e)))
	     ((rel) (apply rel (cdr e)))
	     (else (assembler-error 'sasm-assemble e 
				    "unknown operand" (car e)))))
	  ((lookup-register e))
	  (else e)))
  (case (car expr)
    ((section)
     (when section 
       (assembler-error 'sasm-assemble expr "nested section is not allowed"))
     (unless (string? (cadr expr))
       (assembler-error 'sasm-assemble expr "section name must be string"))
     (let ((s (sasm-output-create-section! output (cadr expr))))
       (for-each (lambda (e) (sasm-assemble1 output s e)) (cddr expr))))
    ((label)
     (when section 
       (assembler-error 'sasm-assemble expr "nested label is not allowed"))
     (unless (symbol? (cadr expr))
       (assembler-error 'sasm-assemble expr "label must be symbol"))
     (let ((l (sasm-output-create-label! output (cadr expr))))
       (for-each (lambda (e) (sasm-assemble1 output l e)) (cddr expr))))
    ;; TODO more special case (e.g. macro)
    (else
     (cond ((lookup-mnemonic (car expr)) =>
	    (lambda (m)
	      (let-values (((code label?)
			    (apply m (map immeidate/register (cdr expr)))))
		(when label?
		  (sasm-output-add-referencing-label! output label? code))
		(sasm-section-push-code! (ensure-section section) code))))
	   (else
	    (assembler-error 'sasm-assemble1 expr 
			     "unknown mnemonic" (car expr)))))))

  )
