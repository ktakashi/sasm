#!r6rs
(import (rnrs)
	(srfi :64)
	(sasm arch conditions)
	;; only for x64 mnemonic for now
	;; but we put prefix for future
	(prefix (sasm arch x64 mnemonics) x64:)
	(prefix (sasm arch x64 registers) x64:)
	(prefix (sasm arch x64 framework) x64:)
	)

(test-begin "Mnemonics")

;; invalid operands
(test-error "cpuid" mnemonic-error? (x64:CPUID 1 2 3))
(test-error "ADD" mnemonic-error? (x64:ADD x64:RAX "invalid"))

(define (find-accessor rtd slot)
  (let loop ((rtd rtd))
    (if rtd
	(let ((fields (record-type-field-names rtd)))
	  (let lp ((i 0))
	    (cond ((= i (vector-length fields)) (loop (record-type-parent rtd)))
		  ((eq? (vector-ref fields i) slot) 
		   (record-accessor rtd i))
		  (else (lp (+ i 1))))))
	;; slot name error
	(lambda (o) (error 'find-accessor "no such slot" slot)))))

(define-syntax test-values
  (syntax-rules (or ? ~ list)
    ((_ "tmp" name (e e* ...) (expected ...) (var ...) (var2 ... ) expr)
     (test-values "tmp" name (e* ...) (expected ... e) 
		  (var ... t) (var2 ... t2)
		  expr))
    ((_ "tmp" name () (expected ...) (var ...) (var2 ...) expr)
     (let ((var #f) ...)
       (test-assert 'expr
		    (let-values (((var2 ...) expr))
		      (set! var var2) ...
		      #t))
       (test-values "equal" name (expected ...) (var ...))))
    ;; compare
    ((_ "equal" name () ()) (values))
    ;; de-construct list
    ((_ "equal" name ((list e ...) e* ...) (v1 v* ...))
     (begin
       (test-values "list" name (e ...) v1)
       (test-values "equal" name (e* ...) (v* ...))))
    ;; record inspection
    ((_ "equal" name ((~ (slot e) ...) e* ...) (v1 v* ...))
     (begin
       (if (record? v1) ;; record must be opaque
	   (let ((rtd (record-rtd v1)))
	     (let ((acc (find-accessor rtd 'slot)))
	       (test-equal '(name (~ slot e)) e (acc v1)))
	     ...)
	   (test-assert '(name (~ (slot e) ...)) #f))
       (test-values "equal" name (e* ...) (v* ...))))
    ((_ "equal" name ((? pred) e* ...) (v1 v* ...))
     (begin
       (test-assert '(name (? pred)) (pred v1))
       (test-values "equal" name (e* ...) (v* ...))))
    ((_ "equal" name ((or e ...) e* ...) (v1 v* ...))
     (begin
       (test-assert '(name (or e ...)) (member v1 '(e ...)))
       (test-values "equal" name (e* ...) (v* ...))))
    ((_ "equal" name (e e* ...) (v1 v* ...))
     (begin
       (test-equal '(name e) e v1)
       (test-values "equal" name (e* ...) (v* ...))))
    ;; comparing list elements
    ((_ "list" name (e e* ...) v1)
     (begin 
       (test-values "equal" name (e) ((car v1)))
       (test-values "list"  name (e* ...) (cdr v1))))
    ((_ "list" name () v1) (values))
    ((_ (expected ...) expr)
     (test-values expr (expected ...) expr))
    ((_ name (expected ...) expr)
     (test-values "tmp" name (expected ...) () () () expr))))

(test-values (#vu8(#x0f #xa2) #f) (x64:CPUID))

(test-values (#vu8(#x48 #xb8 #x89 #x67 #x45 #x23 #x01 #x00 #x00 #x00) #f)
	     (x64:MOV x64:RAX #x123456789))
(test-values (#vu8(#x48 #xbb #x89 #x67 #x45 #x23 #x01 #x00 #x00 #x00) #f)
	     (x64:MOV x64:RBX #x123456789))

;; from http://www.c-jump.com/CIS77/CPU/x86/lecture.html
(test-values ((or #vu8(#x00 #xc1) #vu8(#x02 #xc8)) #f)
	     (x64:ADD x64:CL x64:AL))
(test-values ((or #vu8(#x01 #xc1) #vu8(#x03 #xc8)) #f) (x64:ADD x64:ECX x64:EAX))
;; this is kinda irregular 
;; (probably I don't understand what displacement exactly...)
;; NB: NASM emits the same instruction so should be fine...
(test-values (#vu8(#x81 #xc2 #x56 #x34 #x12 #x00) #f) 
	     (x64:ADD x64:EDX #x123456))
(test-values (#vu8(#x67 #x03 #x3B) #f) (x64:ADD x64:EDI (x64:& x64:EBX)))
(test-values (#vu8(#x67 #x03 #x46 #x08) #f) (x64:ADD x64:EAX (x64:& x64:ESI 8)))
(test-values (#vu8(#x67 #x03 #x9d #x78 #x56 #x34 #x12) #f)
	     (x64:ADD x64:EBX (x64:& x64:EBP #x12345678)))

(test-values (#vu8(#x03 #x14 #x25 #x10 #x00 #x00 #x00) #f)
	     (x64:ADD x64:EDX (x64:& #x10)))

(test-values (#vu8(#x67 #x03 #x0c #xbb) #f) 
	     (x64:ADD x64:ECX (x64:& x64:EBX 0 x64:EDI 4)))
;; this is an error
;; should we even exppose 16 bit register from x64?
(test-error "16bit address on 64bit" mnemonic-error?
	    (x64:ADD x64:AL (x64:& x64:BX)))

;; REX.X
(test-values (#vu8(#x4a #x03 #x2c #x90) #f)
	     (x64:ADD x64:RBP (x64:& x64:RAX 0 x64:R10 4)))

;; REX.R and REX.B
(test-values ((or #vu8(#x4d #x01 #xca) #vu8(#x4d #x03 #xd1)) #f) 
	     (x64:ADD x64:R10 x64:R9))

;; RAX ADD
(test-values (#vu8(#x48 #x83 #xc0 #x10) #f) (x64:ADD x64:RAX #x10))
;; no REX.W or #x67 addressing prefix 32bit register thing
(test-values (#vu8(#x83 #xc0 #x10) #f) (x64:ADD x64:EAX #x10))

;; CALL
(test-values (#vu8(#xff #xd0) #f) (x64:CALL x64:RAX))

(test-values (#vu8(#xff #x14 #x25 #x10 #x00 #x00 #x00) #f)
	     (x64:CALL (x64:& #x10)))

(test-values (#vu8(#xe8 #x00 #x00 #x00 #x00) '(bar))
	     (x64:CALL 'bar))

(define (relocation-addresses? l)
  (for-all x64:relocation-address? l))
(test-values (#vu8(#xff #x15 #x00 #x00 #x00 #x00) (? relocation-addresses?))
	     (x64:CALL (x64:rel 'bar)))
(test-values (#vu8(#xff #x15 #x00 #x00 #x00 #x00) 
	      (list (~ (label 'bar) (address 0))))
	     (x64:CALL (x64:rel 'bar)))

(test-end)
