;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; sasm/arch/x64/framework - Framework for x64
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

;; re-exporting from x86 version

#!r6rs
(library (sasm arch x64 framework)
    (export define-x64-mnemonic
	    define-mnemonic
	    define-register
	    register?
	    register-bits
	    register-name
	    register-index
	    register+displacement?
	    make-relocation-address ;; maybe shouldn't export this?
	    relocation-address?
	    &
	    rel)
    (import (rnrs) 
	    (except (sasm arch x86 framework) rel)
	    (sasm arch conditions))

  (define-syntax define-x64-mnemonic
    (syntax-rules ()
      ((_ name opcodes ...)
       (define-mnemonic name x64 opcodes ...))))

  ;; prefix thing
  ;; opcodes is a list of generated opcode
  ;; FIXME this is getting messier.
  (define (x64 mnemonic opcodes operands args)
    (if (eq? opcodes 'addressing) ;; kinda ugly
	#t
	;; as far as i know, referencing address requires #x67 prefix
	;; for now that's enough
	(cond ((memp register+displacement? args) =>
	       (lambda (regs) 
		 (let ((reg (car regs)))
		   (cond ((= (register-bits reg) 64) '())
			 ((= (register-bits reg) 32) '(#x67))
			 (else ;; 16 or 8
			  (mnemonic-error mnemonic mnemonic
				  "impossible combination of address sizes"
				  operands))))))
	      (else '()))))

  (define (rel label)
    (make-relocation-address 'q label))

)
