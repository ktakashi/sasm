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
;; references:
;;  - Intel® 64 and IA-32 Architectures Software Developer’s Manual
;;    Volume 2 (2A, 2B & 2C): Instruction Set Reference, A-Z
;;  - http://www.c-jump.com/CIS77/CPU/x86/lecture.html
(library (sasm arch x86 framework)
    (export define-x86-mnemonic
	    define-mnemonic ;; needed for x64 framework
	    define-register

	    register?
	    register-bits
	    register-name
	    register-index
	    register+displacement?
	    ;; addressing
	    &
	    )
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (sasm arch conditions))

  (define-syntax define-x86-mnemonic
    (syntax-rules ()
      ((_ name opcodes ...)
       (define-mnemonic name x86 opcodes ...))))
  ;; TODO
  (define (x86 opcodes operands args) '())

  (define-syntax define-mnemonic
    (syntax-rules ()
      ((_ "parse" name prefixer
	  ((prefix op pf op2 extop s d ds modrm? immds ext (operands ...))
	   rest ...)
	  (defs ...))
       (define-mnemonic "parse" name prefixer (rest ...)
	 ;; (type applicable? encode)
	 (((lambda () 'ext)
	   (lambda (args) (mnemonic-applicable? '(operands ...) args))
	   (lambda (args)
	     (mnemonic-encode (lambda (oc opr a) (prefixer 'name oc opr a))
			      '(prefix 
				op 
				pf 
				op2)
			      s d
			      'ds 
			      extop
			      modrm? 
			      'immds
			      '(operands ...) args)))
	  defs ...)))
      ((_ "parse" name prefixer () ((type applicable? encoder) ...))
       ;; TODO how to check type?
       (define name
	 (let ((procs (list (cons applicable? encoder) ...)))
	   (lambda args
	     (let loop ((procs procs))
	       (cond ((null? procs) 
		      (mnemonic-error 'name 'name "invalid operands" args))
		     (((caar procs) args) ((cdar procs) args))
		     (else (loop (cdr procs)))))))))
      ((_ name prefixer opcodes ...)
       (define-mnemonic "parse" name prefixer (opcodes ...) ()))))

  (define-record-type register
    (fields name ;; for debug
	    kind index bits)
    (protocol
     (lambda (p)
       (lambda (name kind index bits ext8bit?)
	 ;; bits must be power of 2
	 (assert (zero? (bitwise-and bits (- bits 1))))
	 (p name kind (bitwise-ior index (if ext8bit? #x80 0)) bits)))))

  (define-record-type register+displacement
    (fields displacement
	    offset ;; pair of offset register+scalar multiplier or #f
	    )
    (parent register)
    (protocol
     (lambda (p)
       (lambda (reg displacement offset)
	 (assert (integer? displacement))
	 (let ((n (p (register-name reg) (register-kind reg)
		     (register-index reg) (register-bits reg) #f)))
	   (n displacement offset))))))

  (define-record-type address
    (fields address))

  (define & 
    (case-lambda 
     ((reg) (if (register? reg) (& reg 0) (make-address reg)))
     ((reg disp) (& reg disp #f))
     ((reg disp offset) (& reg disp offset 0))
     ((reg disp offset multiplier) 
      (make-register+displacement reg disp 
				  (if offset (cons offset multiplier) #f)))))

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
	    (and (and (register? arg) (not (register+displacement? arg)))
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
   ;; TODO refactor it
   (define (mnemonic-encode prefixer
			    opcodes s d ds extop modrm? immds operands args)

     (define (parse-operands operands args)
       (let loop ((operands operands) 
		  (args args) 
		  (rex '()))
	 (if (null? operands)
	     (values (reverse rex))
	     (let ((operand (car operands)) (arg (car args)))
	       ;; TODO this might not be enough
	       (cond ((and (register? arg) (= (register-bits arg) 64))
		      (loop (cdr operands) (cdr args) (cons arg rex)))
		     (else (loop (cdr operands) (cdr args) rex)))))))

     (define (compose-rex rex modrm sib args)
       (if (null? rex)
	   #f
	   (let ((r/m (if (and d (= d 1)) (cadr args)  (car args))))
	     (bitwise-ior #x48 
			  (if (exists (lambda (reg)
					(> (register-index reg) 8)) rex)
			      #x4
			      #x0)
			  (if (and sib 
				   (register+displacement? r/m)
				   (register+displacement-offset r/m)
				   (>= (register-index
					(car (register+displacement-offset
					      r/m))) 8))
			      #x2
			      #x0)
			  (if (and (or sib modrm)
				   (register? r/m)
				   (>= (register-index r/m) 8))
			      #x1
			      #x0)))))

     (define (imm->disp imm)
       (cond ((zero? imm) #x00)
	     ((< imm #xFF)   #x40)
	     ((< imm #xFFFFFFFF) #x80)
	     (else #x00))) ;; mod.disp32?

     (define (compose-modrm operands args extop) 
       ;; TODO can we assume the args has at least 2 elements?
       ;; we also need to consider direction here. fxxk!!!
       (let ((r/m (if (and d (= d 1)) (cadr args) (car args)))
	     (reg (if (and d (= d 1)) (car args) (cadr args))))
	 (bitwise-ior (cond ((register+displacement? r/m)
			     (imm->disp 
			      (register+displacement-displacement r/m)))
			    ((register? r/m) #xc0)
			    ((integer? reg) (imm->disp reg))
			    ;; must be an integer, then
			    (else (imm->disp r/m)))
		      (cond (extop (bitwise-arithmetic-shift-left extop 2))
			    ((register? reg)
			     (bitwise-arithmetic-shift-left (register-index reg)
							    3))
			    (else 0))
		      (cond ((and (register+displacement? r/m)
				  (register+displacement-offset r/m))
			     ;; SIB
			     #x04)
			    ((register? r/m)
			     (bitwise-and (register-index r/m) #x07))
			    (else 0)))))

     (define (compose-sib operands args) 
       (let* ((r/m (if (and d (= d 1)) (cadr args) (car args)))
	      (offset (register+displacement-offset r/m))
	      (reg (car offset))
	      (multiplier (cdr offset)))
	 (bitwise-ior (case multiplier
			((1) #x00)
			((2) #x40)
			((4) #x80)
			((8) #xc0)
			(else (mnemonic-error 'SIB 'SIB
					      "invalid multiplier for SIB"
					      multiplier)))
		      (bitwise-arithmetic-shift-left
		       (bitwise-and (register-index reg) #x07) 3)
		      (bitwise-and (register-index r/m) #x07))))

     (define (->u8-list m count)
       (let loop ((m m) (r '()) (i 0))
	 (if (= i count)
	     (reverse r)
	     (loop (bitwise-arithmetic-shift-right m 8)
		   (cons (bitwise-and m #xff) r)
		   (+ i 1)))))

     (define (displacement operands args size)
       ;; For now, assume the second argument has displacement
       (if size
	   (let ((arg (cadr args)))
	     (if (register+displacement? arg)
		 (->u8-list (register+displacement-displacement arg) size)
		 ;; assume integer
		 (->u8-list arg size)))
	   '()))

     (define (immediate r64? operands args)
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
     ;; TODO should we also support 16bit addressing mode?
     (define (sib/disp? modrm)
       (if modrm
	   (let ((mod (bitwise-arithmetic-shift-right modrm 6))
		 (rm  (bitwise-and modrm #x07)))
	     (case mod
	       ((#x00) (case rm 
			 ((#x04) (values #t #f))
			 ((#x05) (values #f 4)) ;; disp32
			 (else   (values #f #f))))
	       ((#x01) (case rm
			 ((#x04) (values #t #f))
			 (else   (values #f 1))))
	       ((#x02) (case rm 
			 ((#x04) (values #t #f))
			 (else   (values #f 4))))
	       (else (values #f #f))))
	   (values #f #f)))

     (define (find-disp-size modrm sib)
       (and sib
	    (case (bitwise-and sib #x07)
	      ((#x05)
	       (case (bitwise-arithmetic-shift-right modrm 6)
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
		     ((rex.prefix) (compose-rex rex modrm sib args)))
	 (values (u8-list->bytevector 
		  `(,@(prefixer opcodes operands args)
		    ,@(if rex.prefix (list rex.prefix) '())
		    ,@opcode
		    ,@(if modrm (list modrm) '())
		    ,@(if sib (list sib) '())
		    ,@(displacement operands args
				    (or disp-size (find-disp-size modrm sib)))
		    ,@(immediate rex.prefix operands args)))
		 (find-label operands args)))))

  )

