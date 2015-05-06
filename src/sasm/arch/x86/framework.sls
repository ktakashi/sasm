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
	    define-register

	    ;; addressing
	    &
	    )
    (import (rnrs)
	    (rnrs mutable-pairs)
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
				op2)
			      'ds 
			      extop
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

  (define-record-type register+offset
    (fields offset)
    (parent register)
    (protocol
     (lambda (p)
       (lambda (reg offset)
	 (assert (integer? offset))
	 (let ((n (p (register-name reg) (register-kind reg)
		     (register-index reg) (register-bits reg) #f)))
	   (n offset))))))

  (define-record-type address
    (fields address))

  (define & 
    (case-lambda 
     ((reg) (if (register? reg) (& reg 0) (make-address reg)))
     ((reg offset) (make-register+offset reg offset))))

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
	   ((vqp vds) ;; FIXME
	    (cond ((register? arg))
		  ;; depending on the other operand (register) ...
		  ((integer? arg)) ;; FIXME
		  (else #f)))
	   (else #f)))
       (define (check-address operand arg)
	 (case (car operand)
	   ((E) ;; general purpose register or address
	    ;; TODO memory address
	    ;;  symbol: label
	    ;;  integer: direct address?
	    (or (and (register? arg) (eq? (register-kind arg) 'reg))
		(address? arg) ;; address
		(symbol? arg)))
	   ((G Z)
	    (and (and (register? arg) (not (register+offset? arg)))
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
       ;; (display operand) (display arg) (newline)
       (and (check-address operand arg)
	    (check-type operand arg)))
     (define (check-operands operands args)
       (let loop ((operands operands) (args args))
	 (or (null? operands)
	     (and (check-operand (car operands) (car args))
		  (loop (cdr operands) (cdr args))))))
     (and (= (length operands) (length args))
	  (check-operands operands args))
     )
   
   ;; Returns 2 values
   ;;  - bytevector: code
   ;;  - symbol: label (if there is otherwise #f)
   ;; opcodes contains u8 and #f including prefix to secondary opcode
   (define (mnemonic-encode opcodes ds extop modrm? immds operands args)
     (define (parse-operands operands args)
       (let loop ((operands operands) 
		  (args args) 
		  (rex '()))
	 (if (null? operands)
	     (values (reverse rex))
	     (let ((operand (car operands)) (arg (car args)))
	       ;; TODO this might not be enough
	       (cond ((register? arg)
		      (loop (cdr operands) (cdr args) (cons arg rex)))
		     (else (loop (cdr operands) (cdr args) rex)))))))
     (define (compose-rex rex)
       (if (null? rex)
	   #f
	   (bitwise-ior #x48 
			(if (exists (lambda (reg)
				      (> (register-index reg) 8)) rex)
			    #x4
			    #x0)
			#x0 ;; TODO SIB.index
			#x0 ;; TODO ModR/M.rm or SIB.base
			)))
     (define (imm->disp imm)
       (cond ((< imm #xFF)   #x40)
	     ((< imm #xFFFF) #x80)
	     (else #x00))) ;; mod.disp32?
     (define (compose-modrm operands args extop) 
       (define (find-modrm-arg operands args)
	 (let loop ((operands operands) (args args))
	   (case (caar operands)
	     ((C D E ES EST G H M N P Q R S T) (car args))
	     (else (loop (cdr operands) (cdr args))))))
       (let ((arg (find-modrm-arg operands args)))
	 (bitwise-ior (cond ((register+offset? arg)
			     (imm->disp (register+offset-offset arg)))
			    ((register? arg) #xc0)
			    ;; must be an integer, then
			    (else (imm->disp arg)))
		      (if extop (bitwise-arithmetic-shift-left extop 2) 0)
		      (if (register? arg)
			  (bitwise-and (register-index arg) #x03)
			  0
			  ))))
     (define (compose-sib operands args) 1)
     (define (displacement operands args size)
       (if size
	   '()
	   '()))
     (define (immediate r64? operands args)
       (define (->u8-list m count)
	 (let loop ((m m) (r '()) (i 0))
	   (if (= i count)
	       (reverse r)
	       (loop (bitwise-arithmetic-shift-right m 8)
		     (cons (bitwise-and m #xff) r)
		     (+ i 1)))))
       ;; I beleive there is only one immediate value in operands
       (let loop ((operands operands) (args args))
	 (if (null? operands)
	     '()
	     (case (caar operands)
	       ((I)
		(case (cadar operands)
		  ((b) (->u8-list (car args) 1))
		  ((w) (->u8-list (car args) 2))
		  ((l vds) (->u8-list (car args) 4))
		  ((q) (->u8-list (car args) 8))
		  ((vqp)
		   (if r64?
		       (->u8-list (car args) 8)
		       (->u8-list (car args) 4)))
		  (else (assert #f))))
	       (else (loop (cdr operands) (cdr args)))))))
     (define (find-label operands args) #f)
     (define (merge-reg-if-needed opcodes operands args)
       (define (last-pair p)
	 (let loop ((p p))
	   (cond ((null? p) '())
		 ((null? (cdr p)) p)
		 (else (loop (cdr p))))))
       (let loop ((operands operands) (args args))
	 (if (null? operands) 
	     opcodes
	     (case (caar operands)
	       ((Z) 
		(let ((p (last-pair opcodes)))
		  (set-car! p (bitwise-ior (car p) (register-index (car args))))
		  opcodes))
	       (else (loop (cdr operands) (cdr args)))))))
     ;; Intel 64 and IA32 Architectures Software Developer's Manual
     ;; Volume 2, Table 2-2.
     ;; NB, we don't support 16 bit addressing mode.
     (define (sib/disp? modrm)
       (if modrm
	   (let ((mod (bitwise-arithmetic-shift-right modrm 5))
		 (rm  (bitwise-and modrm #x03)))
	     (case mod
	       ((#x00) (case rm 
			 ((#x04) (values #t #f))
			 ((#x05) (values #f #b10)) ;; disp32
			 (else   (values #f #f))))
	       ((#x01) (case rm
			 ((#x04) (values #t #f))
			 (else   (values #f #b00))))
	       ((#x02) (case rm 
			 ((#x04) (values #t #f))
			 (else   (values #f #b10))))
	       (else (values #f #f))))
	   (values #f #f)))
     (define (find-disp-size modrm sib)
       (and sib
	    (case (bitwise-and sib #x03)
	      ((#x05)
	       (case (bitwise-arithmetic-shift-right modrm 5)
		 ((#x00) #b10)
		 ((#x01) #b00)
		 ((#x02) #b10)
		 (else #f)))
	      (else #f))))
	    
     (let ((rex (parse-operands operands args))
	   (opcode (merge-reg-if-needed (filter values opcodes) operands args))
	   (modrm (and modrm? (compose-modrm operands args extop))))
       (let*-values (((has-sib? disp-size) (sib/disp? modrm))
		     ((sib) (and has-sib?
				 (compose-sib operands args)))
		     ((rex.prefix) (compose-rex rex)))
	 (values (u8-list->bytevector 
		  `(,@(if rex.prefix (list rex.prefix) '())
		    ,@opcode
		    ,@(if modrm (list modrm) '())
		    ,@(if sib (list sib) '())
		    ,@(displacement operands args
				    (or disp-size (find-disp-size modrm sib)))
		    ,@(immediate rex.prefix operands args)))
		 (find-label operands args)))))

  )

