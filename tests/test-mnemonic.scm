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

(define-syntax test-values
  (syntax-rules ()
    ((_ "tmp" name (e e* ...) (expected ...) (var ...) expr)
     (test-values "tmp" name (e* ...) (expected ... e) (var ... t) expr))
    ((_ "tmp" name () (expected ...) (var ...) expr)
     (let-values (((var ...) expr))
       (test-equal '(name expected) 'expected var)
       ...))
    ((_ (expected ...) expr)
     (test-values expr (expected ...) expr))
    ((_ name (expected ...) expr)
     (test-values "tmp" name (expected ...) () () expr))))

;;(test-equal 'expr (expected ...) (let-values ((results expr)) results))
(test-values (#vu8(#x0f #xa2) #f) (x64:CPUID))

(test-values (#vu8(#x48 #xb8 #x89 #x67 #x45 #x23 #x01 #x00 #x00 #x00) #f)
	     (x64:MOV x64:RAX #x123456789))
(test-values (#vu8(#x48 #xbb #x89 #x67 #x45 #x23 #x01 #x00 #x00 #x00) #f)
	     (x64:MOV x64:RBX #x123456789))

;; from http://www.c-jump.com/CIS77/CPU/x86/lecture.html
(test-values (#vu8(#x00 #xc1) #f) (x64:ADD x64:CL x64:AL))
(test-values (#vu8(#x01 #xc1) #f) (x64:ADD x64:ECX x64:EAX))
;; this is kinda irregular 
;; (probably I don't understand what displacement exactly...)
;; NB: NASM emits the same instruction so should be fine...
(test-values (#vu8(#x81 #xc2 #x56 #x34 #x12 #x00) #f) 
	     (x64:ADD x64:EDX #x123456))
;; Should we need #x67 prefix for them?
(test-values (#vu8(#x03 #x3B) #f) (x64:ADD x64:EDI (x64:& x64:EBX)))
(test-values (#vu8(#x03 #x46 #x08) #f) (x64:ADD x64:EAX (x64:& x64:ESI 8)))

(test-end)
