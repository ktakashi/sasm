;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; sasm/arch/output.sls - File format independent output
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

(library (sasm arch output)
    (export make-sasm-output sasm-output?
	    sasm-output-sections
	    sasm-output-symbols
	    sasm-output-labels

	    sasm-output-create-section!
	    ;; sasm-output-add-symbol!

	    sasm-section-name
	    sasm-section-code
	    sasm-section-push-code!
	    )
    (import (rnrs))

;; Contains assembled outputs
;; Binary format file object will be created from this.
(define-record-type sasm-output
  (fields (mutable sections) ;; list of sections, reverse order
	  (mutable symbols)  ;; list of symbols, unordered
	  (mutable labels)   ;; list of labels, unordered
	  )
  (protocol
   (lambda (p)
     (lambda ()
       (p '() '() '())))))

(define-record-type sasm-section
  (fields output	 ;; where this section belongs (for convenicne)
	  name		 ;; section name (e.g. .text)
	  (mutable code) ;; code (list of bytevector), reverse order
	  )
  (protocol
   (lambda (p)
     (lambda (output name)
       (p output name '())))))

;; create/retrieve section
;; we assume code vectors are ordered thus
;; (section ".text" v1 ...)
;; (section ".text" v2 ...)
;; would be (v1 ... v2 ...)
(define (sasm-output-create-section! output name)
  (let ((s* (sasm-output-sections output)))
    (cond ((memp (lambda (s) (string=? name (sasm-section-name s))) s*) => car)
	  (else (let ((s (make-sasm-section output name)))
		  (sasm-output-sections-set! output (cons s s*))
		  s)))))

(define (sasm-section-push-code! section code)
  (let ((c* (sasm-section-code section)))
    (sasm-section-code-set! section (cons code c*))))

)
