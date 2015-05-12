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

	    make-relocation-address ;; for x64 framework
	    relocation-address?
	    ;; addressing
	    &
	    rel
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
	   '(operands ...)
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
      ((_ "parse" name prefixer () ((type operands encoder) ...))
       ;; TODO how to check type?
       (define name
	 (let ((procs (list (cons operands encoder) ...)))
	   (lambda args
	     (let ((applicables (find-applicable procs args)))
	       (if (null? applicables)
		   (mnemonic-error 'name 'name "invalid operands" args)
		   (let ((sorted (sort-applicable applicables)))
		     ((cdar sorted) args))))))))
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
  (define-record-type relocation-address
    (fields type ;; v/q. v: 32 bit, q: 64 bit (may not be needed)
	    label)
    (parent address)
    (protocol
     (lambda (p)
       (lambda (type label)
	 ;; set address 0 for my convenience.
	 ;; this value will be used to emit address displacement
	 ((p 0) type label)))))

  ;; this is for 32 bit
  (define (rel label) (make-relocation-address 'v label))

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

  ;; FIXME we are using too much definition from
  ;;   http://ref.x86asm.net/
  ;; gen-instr.scm should convert them to more understandable
  ;; values.
  (define (find-applicable procs args)
    (let loop ((procs procs) (r '()))
      (cond ((null? procs) r)
	    ((mnemonic-applicable? (caar procs) args) 
	     (loop (cdr procs) (cons (car procs) r)))
	    (else (loop (cdr procs) r)))))

  (define (sort-applicable procs) 
    ;; TODO support properly
    (define *immediate-size*
      '((b . 1) (bs . 1) (bss . 1) (w . 2) (d . 4) (ds . 4) (q . 8)
	(v . 4) (vs . 4) (vds . 4) (vqp . 8) 
	;; dummy for specific register one
	(#f . 100)))
    ;;(display procs) (newline)
    ;; by operand size
    (list-sort (lambda (a b)
		 (let ((ao (car a))
		       (bo (car b)))
		   (< (fold-left + 0 
				 (map (lambda (o)
					(cdr (assq (cadr o) *immediate-size*)))
				      ao))
		      (fold-left + 0
				 (map (lambda (o)
					(cdr (assq (cadr o) *immediate-size*)))
				      bo)))))
	       procs))

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
	  ((vds)
	   (cond ((register? arg) (<= (register-bits arg) 32))
		 ;; depending on the other operand (register) ...
		 ((integer? arg) (<= #x-80000000 arg #x7FFFFFFF))
		 ((symbol? arg))  ;; near jump/call
		 (else #f)))
	  ((vqp) ;; FIXME
	   (cond ((register? arg))
		 ;; depending on the other operand (register) ...
		 ((integer? arg)) ;; FIXME
		 ((address? arg)) ;; address?
		 (else #f)))
	  ;; ((v) (address? arg)) ;; TODO support this properly
	  ((q) 
	   ;; TODO address thing
	   (cond ((register? arg) (= (register-bits arg) 64))
		 ((integer? arg) (<= arg #xFFFFFFFFFFFFFFFF))
		 ((address? arg))
		 ((symbol? arg)) ;; far label
		 (else #f)))
	  ((#f) #t) ;; no type specified thus a specific register.
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
	  ;; label (no body can know relative offset from assembly!)
	  ((J) (symbol? arg))
	  (else #f)))
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
	      (cond ((and (register? arg) (= (register-bits arg) 64)
			  (memq (cadr operand) '(dqp vqp ptp)))
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
      (let ((r/m (if (and d (= d 1)) 
		     (and (pair? (cdr args)) (cadr args))
		     (car args)))
	    (reg (if (and d (= d 1)) 
		     (car args) 
		     (and (pair? (cdr args)) (cadr args)))))
	(bitwise-ior (cond ((register+displacement? r/m)
			    (imm->disp 
			     (register+displacement-displacement r/m)))
			   ((register? r/m) #xc0)
			   ((integer? reg) (imm->disp reg))
			   ((address? r/m) #x00) ;; addressing mode
			   ;; must be an integer, then
			   (else (imm->disp r/m)))
		     (cond (extop (bitwise-arithmetic-shift-left extop 3))
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
			   ((address? r/m) 
			    (if (relocation-address? r/m)
				#x05   ;; disp+rip (64 bit)
				#x04)) ;; displacement only
			   (else 0)))))

    (define (compose-sib operands args) 
      (let ((r/m (if (and d (= d 1)) (cadr args) (car args))))
	(if (register+displacement? r/m)
	    (let* ((offset (register+displacement-offset r/m))
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
			   (bitwise-and (register-index r/m) #x07)))
	    ;; address
	    ;; scale = 00
	    ;; index = 100 (invalid)
	    ;; base  = 101 (displacement only)
	    #b00100101
	    )))

    (define (->u8-list m count)
      (define (ensure-integer m)
	(if (address? m)
	    (address-address m)
	    m))
      (let loop ((m (ensure-integer m)) (r '()) (i 0))
	(if (= i count)
	    (reverse r)
	    (loop (bitwise-arithmetic-shift-right m 8)
		  (cons (bitwise-and m #xff) r)
		  (+ i 1)))))

    (define (displacement operands args size)
      ;; For now, assume the second argument has displacement
      (if size
	  (let ((arg (if (null? (cdr args)) (car args) (cadr args))))
	    (cond ((register+displacement? arg)
		   (->u8-list (register+displacement-displacement arg) size))
		  ((address? arg) (->u8-list (address-address arg) size))
		  ;; assume integer
		  (else (->u8-list arg size))))
	  '()))

    (define (immediate r64? operands args)
      ;; I beleive there is only one immediate value in operands
      (let loop ((operands operands) (args args))
	(if (null? operands)
	    '()
	    (case (caar operands)
	      ((I)
	       (case (cadar operands)
		 ((b bs) (->u8-list (car args) 1))
		 ((w) (->u8-list (car args) 2))
		 ((l vds) (->u8-list (car args) 4))
		 ((q) (->u8-list (car args) 8))
		 ((vqp)
		  (if r64?
		      (->u8-list (car args) 8)
		      (->u8-list (car args) 4)))
		 (else (assert #f))))
	      ((J)
	       ;; near jump/call will be fixup by bin format
	       (case (cadar operands)
		 ((vds) '(0 0 0 0))
		 ((q)   '(0 0 0 0 0 0 0 0))
		 (else (assert #f))))
	      (else (loop (cdr operands) (cdr args)))))))

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
		((#x00) 4)
		((#x01) 1)
		((#x02) 4)
		(else #f)))
	     (else #f))))

    (define (find-label operands args) 
      ;; for now very simple
      (let ((labels (filter (lambda (s) (or (symbol? s) 
					    (relocation-address? s)))
			    args)))
	;; I don't think there would multiple labels but for
	;; my convenience.
	(and (not (null? labels))
	     labels)))
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

