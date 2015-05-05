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

(test-end)