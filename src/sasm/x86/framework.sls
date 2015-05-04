;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; sasm/x86/framework - Framework for x86 
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
(library (sasm x86 framework)
    (export define-mnemonic)
    (import (rnrs))

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
		      (assertion-violation 'name "invalid args" args))
		     (((caar procs) args) ((cdar procs) args))
		     (else (loop (cdr procs)))))))))
      ((_ name opcodes ...)
       (define-mnemonic "parse" name (opcodes ...) ()))))

    (define (mnemonic-applicable? operands args)
      ;; TODO proper resolusion
      (= (length operands) (length args))
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

