#!r6rs
(import (rnrs)
	(srfi :64)
	(sasm arch conditions)
	;; only for x64 mnemonic for now
	;; but we put prefix for future
	(prefix (sasm arch x64 mnemonics) x64:)
	(prefix (sasm arch x64 registers) x64:)
	)

(test-begin "Mnemonics")

;; invalid operands
(test-error "cpuid" mnemonic-error? (x64:CPUID 1 2 3))
(test-error "ADD" mnemonic-error? (x64:ADD x64:RAX "invalid"))

(define-syntax test-values
  (syntax-rules ()
    ((_ (expected ...) expr)
     (test-values 'expr (expected ...) expr))
    ((_ name (expected ...) expr)
     (test-equal 'expr (expected ...) (let-values ((results expr)) results)))))

(test-values '(#vu8(#x0f #xa2) #f) (x64:CPUID))

(test-values '(#vu8(#x48 #xb8 #x89 #x67 #x45 #x23 #x01 #x00 #x00 #x00) #f)
	     (x64:MOV x64:RAX #x123456789))
(test-values '(#vu8(#x48 #xbb #x89 #x67 #x45 #x23 #x01 #x00 #x00 #x00) #f)
	     (x64:MOV x64:RBX #x123456789))

(test-end)
