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
	    sasm-output-labels
	    sasm-output-referencing-labels

	    sasm-output-create-section!
	    sasm-output-add-referencing-label!

	    sasm-fragment?
	    sasm-fragment-name
	    sasm-fragment-output

	    sasm-section?
	    sasm-section-code
	    sasm-section-push-code!

	    sasm-output-create-label!
	    sasm-label?
	    sasm-label-name
	    sasm-label-section
	    )
    (import (rnrs))

;; Contains assembled outputs
;; Binary format file object will be created from this.
(define-record-type sasm-output
  (fields (mutable sections) ;; list of sections, reverse order
	  (mutable labels)   ;; list of defined labels, reverse order
	  ;; a list of referencing labels, unordered
	  ;; (labels . code)
	  (mutable referencing-labels)
	  )
  (protocol
   (lambda (p)
     (lambda ()
       (p '() '() '())))))

;; fragment can be section, label, etc.
(define-record-type sasm-fragment
  (fields output ;; where this section belongs (for convenicne)
	  name	 ;; the name of this fragment (e.g. .text). string
	  )
  (protocol
   (lambda (p)
     (lambda (output name)
       (p output name)))))

(define-record-type sasm-section
  (parent sasm-fragment)
  (fields (mutable code)) ;; code (list of bytevector), reverse order
  (protocol
   (lambda (p)
     (lambda (output name)
       ((p output name) '())))))

;; label
(define-record-type sasm-label
  (parent sasm-section) ;; for my convenience
  (protocol
   (lambda (n)
     (lambda (output name)
       ((n output name))))))

;; create/retrieve section
;; we assume code vectors are ordered thus
;; (section ".text" v1 ...)
;; (section ".text" v2 ...)
;; would be (v1 ... v2 ...)
(define (sasm-output-create-section! output name)
  (let ((s* (sasm-output-sections output)))
    (cond ((assp (lambda (n) (and (string? n) (string=? name n))) s*) => cdr)
	  (else 
	   (let ((s (make-sasm-section output name)))
	     (sasm-output-sections-set! output (cons (cons name s) s*))
	     s)))))

;; create label. the name must not be defined in labels
(define sasm-output-create-label!
  (let ((mark (list 'label)))
    (lambda (output name)
      (let ((l* (sasm-output-labels output)))
	(cond ((assoc name l*)
	       (error 'sasm-output-create-label! "duplicate label" name))
	      (else 
	       (let ((l (make-sasm-label output name))
		     (s* (sasm-output-sections output)))
		 (sasm-output-labels-set! output (cons (cons name l) l*))
		 (sasm-output-sections-set! output (cons (cons mark l) s*))
		 l)))))))

(define (sasm-output-add-referencing-label! output labels code)
  (let ((l* (sasm-output-referencing-labels output)))
    (sasm-output-referencing-labels-set! output (cons (cons labels code) l*))))

(define (sasm-section-push-code! section code)
  (let ((c* (sasm-section-code section)))
    (sasm-section-code-set! section (cons code c*))))

)
