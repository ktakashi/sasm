;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; sasm/arch/x86/framework - Framework for x86 
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
(library (sasm arch x86 framework)
    (export define-mnemonic
	    define-register)
    (import (rnrs)
	    (sasm arch conditions))

  (define-syntax define-mnemonic
    (syntax-rules ()
      ((_ "parse" name
	  ((prefix op pf op2 extop ds modrm? immds ext (operands ...)) rest ...)
	  (defs ...))
       (define-mnemonic "parse" name (rest ...)
	 ;; (type applicable? encode)
	 (((lambda () 'ext)
	   (lambda (args) (mnemonic-applicable? '(operands ...) args))
	   (lambda (args)
	     (mnemonic-encode '(prefix 
				op 
				pf 
				op2 
				extop) 
			      'ds 
			      modrm? 
			      'immds
			      '(operands ...) args)))
	  defs ...)))
      ((_ "parse" name () ((type applicable? encoder) ...))
       ;; TODO how to check type?
       (define name
	 (let ((procs (list (cons applicable? encoder) ...)))
	   (lambda args
	     (let loop ((procs procs))
	       (cond ((null? procs) 
		      (mnemonic-error 'name 'name "invalid operands" args))
		     (((caar procs) args) ((cdar procs) args))
		     (else (loop (cdr procs)))))))))
      ((_ name opcodes ...)
       (define-mnemonic "parse" name (opcodes ...) ()))))

  (define-record-type register
    (fields name ;; for debug
	    kind index bits)
    (protocol
     (lambda (p)
       (lambda (name kind index bits ext8bit?)
	 ;; bits must be power of 2
	 (assert (zero? (bitwise-and bits (- bits 1))))
	 (p name kind (bitwise-ior index (if ext8bit? #x80 0)) bits)))))

  (define-syntax define-register
    (syntax-rules ()
      ((_ name kind index bit)
       (define-register name kind index bit #f))
      ((_ name kind index bit ext8bit?)
       (define name (make-register 'name 'kind index bit ext8bit?)))))
			  
   (define (mnemonic-applicable? operands args)
     (define (check-operand operand arg)
       (define (check-type operand arg)
	 (case (cadr operand)
	   ((b) ;; byte
	    (cond ((register? arg) (= (register-bits arg) 8))
		  ((integer? arg)  (or (<= 0 arg 255)))
		  (else #f)))
	   ((bs) ;; byte
	    (cond ((register? arg) (= (register-bits arg) 8))
		  ((integer? arg)  (or (<= -128 arg 127)))
		  (else #f)))
	   ((vqp)
	    (cond ((register? arg))
		  ;; depending on the other operand (register) ...
		  ((integer? arg)) ;; FIXME
		  (else #f)))
	   (else #f)))
       (define (check-address operand arg)
	 (case (car operand)
	   ((E) ;; general purpose register or address
	    ;; TODO memory address
	    (and (register? arg)
		 (eq? (register-kind arg) 'reg)))
	   ((G)
	    ;; TODO properly consider ModR/M
	    (and (register? arg)
		 (eq? (register-kind arg) 'reg)))
	   ;; special registers
	   ((AL) ;; al register...
	    (and (register? arg)
		 (eq? (register-kind arg) 'reg)
		 (= (register-bits arg) 8)
		 (= (register-index arg) 0)))
	   ((rAX) ;; AX, EAX and RAX
	    (and (register? arg)
		 (eq? (register-kind arg) 'reg)
		 (> (register-bits arg) 8) ;; more than 8bit
		 (= (register-index arg) 0)))
	   ((I) (integer? arg)) ;; immediate
	   (else #f)))
       (and (check-address operand arg)
	    (check-type operand arg)))
     (define (check-operands operands args)
       (let loop ((operands operands) (args args))
	 (or (null? operands)
	     (and (check-operand (car operands)
				 (car args))
		  (loop (cdr operands) (cdr args))))))
     (and (= (length operands) (length args))
	  (check-operands operands args))
     )
   
   ;; 
   (define (mnemonic-encode opcodes ds modrm? immds operands args)
     (define (compute-size info)
       ;; TODO immediate
       (let loop ((r 0) (info info))
	 (cond ((null? info) r)
	       ((car info) (loop (+ r 1) (cdr info)))
	       (else (loop r (cdr info))))))
     (let ((bv (make-bytevector (compute-size opcodes) 0)))
       (display opcodes) (newline)
       (let loop ((i 0) (opcodes opcodes))
	 (cond ((null? opcodes) bv)
	       ((car opcodes) 
		(bytevector-u8-set! bv i (car opcodes))
		(loop (+ i 1) (cdr opcodes)))
	       (else (loop i (cdr opcodes)))))))

  )

