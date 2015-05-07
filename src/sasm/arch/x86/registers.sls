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

(library (sasm arch x86 registers)
    (export EAX ECX EDX EBX ESP EBP ESI EDI
	    AX CX DX BX SP BP SI DI
	    AL CL DL BL AH CH DH BH
	    ;; TODO more (XMM and so)
	    )
    (import (sasm arch x86 framework))

  (define-register EAX reg 0 32)
  (define-register ECX reg 1 32)
  (define-register EDX reg 2 32)
  (define-register EBX reg 3 32)
  (define-register ESP reg 4 32)
  (define-register EBP reg 5 32)
  (define-register ESI reg 6 32)
  (define-register EDI reg 7 32)

  (define-register AX reg 0 16)
  (define-register CX reg 1 16)
  (define-register DX reg 2 16)
  (define-register BX reg 3 16)
  (define-register SP reg 4 16)
  (define-register BP reg 5 16)
  (define-register SI reg 6 16)
  (define-register DI reg 7 16)

  (define-register AL reg 0 8)
  (define-register CL reg 1 8)
  (define-register DL reg 2 8)
  (define-register BL reg 3 8)
  (define-register AH reg 4 8)
  (define-register CH reg 5 8)
  (define-register DH reg 6 8)
  (define-register BH reg 7 8)


  )
