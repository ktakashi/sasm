;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; sasm/arch/x86/registers - x86 registers
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
(library (sasm arch x86 registers)
    (export EAX ECX EDX EBX ESP EBP ESI EDI
	    AX CX DX BX SP BP SI DI
	    AL CL DL BL AH CH DH BH
	    ;; TODO more (XMM and so)

	    lookup-register
	    )
    (import (rnrs) (sasm arch x86 framework))

  (define *register-table* (make-eq-hashtable))

  (define (lookup-register name) (hashtable-ref *register-table* name #f))

  (define-syntax define-x86-register
    (lambda (x)
      (define (->small name)
	(string->symbol 
	 (string-downcase (symbol->string (syntax->datum name)))))
      (syntax-case x ()
	((k name type nr bits)
	 (with-syntax ((sname (datum->syntax #'k (->small #'name))))
	   #'(begin
	       (define-register name type nr bits)
	       (define dummy
		 (begin 
		   (hashtable-set! *register-table* 'name name)
		   (hashtable-set! *register-table* 'sname name)))))))))

  (define-x86-register EAX reg 0 32)
  (define-x86-register ECX reg 1 32)
  (define-x86-register EDX reg 2 32)
  (define-x86-register EBX reg 3 32)
  (define-x86-register ESP reg 4 32)
  (define-x86-register EBP reg 5 32)
  (define-x86-register ESI reg 6 32)
  (define-x86-register EDI reg 7 32)

  (define-x86-register AX reg 0 16)
  (define-x86-register CX reg 1 16)
  (define-x86-register DX reg 2 16)
  (define-x86-register BX reg 3 16)
  (define-x86-register SP reg 4 16)
  (define-x86-register BP reg 5 16)
  (define-x86-register SI reg 6 16)
  (define-x86-register DI reg 7 16)

  (define-x86-register AL reg 0 8)
  (define-x86-register CL reg 1 8)
  (define-x86-register DL reg 2 8)
  (define-x86-register BL reg 3 8)
  (define-x86-register AH reg 4 8)
  (define-x86-register CH reg 5 8)
  (define-x86-register DH reg 6 8)
  (define-x86-register BH reg 7 8)


  )
