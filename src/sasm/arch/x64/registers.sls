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
	    )
    (import (sasm arch x64 framework)
	    (sasm arch x86 registers))

  (define-register RAX reg 0 64)
  (define-register RCX reg 1 64)
  (define-register RDX reg 2 64)
  (define-register RBX reg 3 64)
  (define-register RSP reg 4 64)
  (define-register RBP reg 5 64)
  (define-register RSI reg 6 64)
  (define-register RDI reg 7 64)
  (define-register R8  reg 8 64)
  (define-register R9  reg 9 64)
  (define-register R10 reg 10 64)
  (define-register R11 reg 11 64)
  (define-register R12 reg 12 64)
  (define-register R13 reg 13 64)
  (define-register R14 reg 14 64)
  (define-register R15 reg 15 64)

  )
