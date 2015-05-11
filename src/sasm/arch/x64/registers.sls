;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; sasm/arch/x64/registers - x64 registers
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
(library (sasm arch x64 registers)
    (export RAX RCX RDX RBX RSP RBP RSI RDI R8 R9 R10 R11 R12 R13 R14 R15

	    ;; from x86
	    EAX ECX EDX EBX ESP EBP ESI EDI
	    AX CX DX BX SP BP SI DI
	    AL CL DL BL AH CH DH BH
	    ;; TODO more


	    lookup-register
	    )
    (import (rnrs)
	    (sasm arch x64 framework)
	    (rename (sasm arch x86 registers)
		    (lookup-register x86:lookup-register)))

  (define *register-table* (make-eq-hashtable))

  (define (lookup-register name)
    (cond ((hashtable-ref *register-table* name #f))
	  (else (x86:lookup-register name))))

  (define-syntax define-x64-register
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
      
  (define-x64-register RAX reg 0 64)
  (define-x64-register RCX reg 1 64)
  (define-x64-register RDX reg 2 64)
  (define-x64-register RBX reg 3 64)
  (define-x64-register RSP reg 4 64)
  (define-x64-register RBP reg 5 64)
  (define-x64-register RSI reg 6 64)
  (define-x64-register RDI reg 7 64)
  (define-x64-register R8  reg 8 64)
  (define-x64-register R9  reg 9 64)
  (define-x64-register R10 reg 10 64)
  (define-x64-register R11 reg 11 64)
  (define-x64-register R12 reg 12 64)
  (define-x64-register R13 reg 13 64)
  (define-x64-register R14 reg 14 64)
  (define-x64-register R15 reg 15 64)

  )
