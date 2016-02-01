#!/bin/sh
#| -*- scheme -*-
exec sagittarius $0 "$@"
|#
;; parsing x86reference.xml to instruction definitions.
;; we dump most of the definition into s-expr so that
;; we can use it in Scheme world.

;; this is not R6RS portable script so if you need to
;; re-generate the definitions, then use Sagittarius.
(import (rnrs)
	(text sxml ssax)
	(text sxml sxpath)
	(text sxml tools)
	(getopt)
	(srfi :1)
	(srfi :26)
	(match)
	(pp))

;; the structure of element of given lists are the following:
;; element  ::= (prefix opcode (mnem-def ...))
;; prefix   ::= #f | #x0F
;; opcode   ::= integer
;; mnem-def ::= (name data-size ModR/M? immdata-size
;;               secondary-opcode extended-opcode 
;;               extension-name operands)
;; data-size ::= #f | 'b | 'w
;; ModR/M?   ::= #f | #t
;; immdata-size ::= #f | 'v | 'b | 'l | 'w
;; secondary-opcode ::= #f | integer
;; extended-opcode ::= #f | integer
;; extension-name ::= #f | symbol (e.g. sse1)
;; operands ::= (operand ...)
;; operand ::= (address type number)
(define *immediate-size*
  '((b . 1) (bs . 1) (bss . 1) (w . 2) (d . 4) (ds . 4) (q . 8)
    (v . 4) (vs . 4) (vds . 4) (vqp . 8)))
(define (emit-instr out arch one-byte two-bytes)
  (define (collect-names defs)
    (map (match-lambda
	  ((_ _ ((name rest ...) ...)) name)) defs))
  
  ;; we need to merge the same name mnemonics to one expression
  ;; so input is (prefix opcode ((name rest ...) ...)
  ;; the result is (name (prefix opcode rest ...) ...))
  (define (order-mnemonics mnemonics)
    (define store (make-eq-hashtable))
    (define (compare-by-size a b)
      (let* ((a-op (list-ref a 11))
	     (b-op (list-ref b 11))
	     (c (compare (length a-op) (length b-op))))
	(cond ((zero? c) 
	       (let ((ai (assq 'I a-op))
		     (bi (assq 'I b-op)))
		 (if (and ai bi)
		     (let ((at (assq (cadr ai) *immediate-size*))
			   (bt (assq (cadr bi) *immediate-size*)))
		       (< (cdr at) (cdr bt)))
		     ;; TODO
		     #t)))
	      ((positive? c) #f)
	      (else #t))))
    (define (sort-cons k v)
      (cons k (list-sort compare-by-size v)))
    (for-each 
     (match-lambda
      ((prefix op ((defs ...) ...)) 
       (for-each 
	(lambda (def)
	  (let ((name (car def))
		(rest (cdr def)))
	    (hashtable-update! 
	     store name
	     (lambda (val)
	       (cons (cons* prefix op rest) val)) '())))
	defs)))
     mnemonics)
    (hashtable-map cons store))
  (define (define-name)
    (string->symbol (format "define-~a-mnemonic" arch)))
  ;;(pp one-byte)
  ;;(pp two-bytes)
  (let ((name1 (apply lset-union eq? (collect-names one-byte)))
	(name2 (apply lset-union eq? (collect-names two-bytes))))
    (display ";; -*- mode: scheme; coding: utf-8; -*-" out)
    (newline out)
    (display ";; Generated by gen-instr.scm. !! DO NOT EDIT !!" out)
    (newline out)
    (display "#!r6rs" out)
    (newline out)
    (let ((names (lset-union eq? name1 name2)))
      (pp `(library (sasm arch ,(string->symbol arch) mnemonics)
	       (export ,@names
		       lookup-mnemonic)
	       (import (rnrs) (sasm arch ,(string->symbol arch) framework))
	     ,@(map (lambda (def) `(,(define-name) . ,def)) 
		    (order-mnemonics (append one-byte two-bytes)))
	     (define (lookup-mnemonic name) 
	       (hashtable-ref *mnemonic-table* name #f))
	     (define *mnemonic-table* 
	       (let ((ht (make-eq-hashtable)))
		 ,@(append-map 
		    (lambda (name)
		      (let ((sname (string->symbol 
				    (string-downcase (symbol->string name)))))
			`((hashtable-set! ht ',name ,name)
			  (hashtable-set! ht ',sname ,name))))
		    names)
		 ht))
	     )
	  out))))

;; from x86reference.dtd
;;  processor codes:
;;   00 - 8086
;;   01 - 80186
;;   02 - 80286
;;   03 - 80386
;;   04 - 80486
;;   05 - Pentium 1
;;   06 - Pentium with MMX
;;   07 - Pentium Pro
;;   08 - Pentium II
;;   09 - Pentium III
;;   10 - Pentium 4
;;   11 - Core 1
;;   12 - Core 2
;;   13 - Core i7
;;   99 - Itanium
(define (parse-definitions sxml oarch out)
  (define (check-proc-end path e) (null? ((sxpath path) e)))
  (define arch (arch->mode oarch))
  (define (has-modrm&immdata? syntx)
    (let ((src-a ((if-car-sxpath "/src/a/text()") syntx))
	  (dst-a ((if-car-sxpath "/dst/a/text()") syntx)))
      (define modrm-types 
	'("C" "D" "E" "ES" "EST" "G" "H" "M" "N" "P" "Q" "R" "S" "T"))
      (define imm-types 
	'("A"
	  ("I" . "B")
	  ("J" . "B")
	  "O"))
      (define (check s)
	(let ((as (if (pair? s) (car s) s))
	      (ds (if (pair? s) (cdr s) s)))
	  (or (equal? src-a as) (equal? dst-a ds))))
      (values (not (null? (filter-map check modrm-types)))
	      (not (null? (filter-map check imm-types))))))
  (define (immdata-size syntx)
    (let ((src-t ((if-car-sxpath "/src[a/text()='A' or a/text()='I' or a/text()='J' or a/text()='O']/t/text()") syntx))
	  (dst-t ((if-car-sxpath "/dst[a/text()='A' or a/text()='I' or a/text()='J' or a/text()='O']/t/text()") syntx))
	  ;; define it in toplevel...
	  (src-a ((if-car-sxpath "/src/a/text()") syntx))
	  (dst-a ((if-car-sxpath "/dst/a/text()") syntx)))

      ;;(assert (and src-t dst-t))
      (cond ((or (equal? "O" src-a) (equal? "O" dst-a))     'v)
	    ((or (equal? "b" src-t) (equal? "b" dst-t))     'b)
	    ((or (equal? "bs" src-t) (equal? "bs" dst-t))   'b)
	    ((or (equal? "bss" src-t) (equal? "bs" dst-t))  'b)
	    ((or (equal? "p" src-t) (equal? "p" dst-t))     'l)
	    ((or (equal? "v" src-t) (equal? "v" dst-t))     'v)
	    ((or (equal? "vds" src-t) (equal? "vds" dst-t)) 'v)
	    ((or (equal? "vqp" src-t) (equal? "vqp" dst-t)) 'v)
	    ((or (equal? "vs" src-t) (equal? "vs" dst-t))   'v)
	    ((or (equal? "w" src-t) (equal? "w" dst-t))     'w)
	    (else (assert #f)))))

  (define (parse-operand syntx)
    (define (parse-op op)
      (let ((displayed (sxml:attr op 'displayed)))
	;; if displayed is no, then we don't need it in syntax
	;; so fliter it out :)
	(and (not (and displayed (string=? displayed "no")))
	     (let ((t  ((if-car-sxpath "/t/text()") op))
		   (a  ((if-car-sxpath "/a/text()") op))
		   (nr (sxml:attr op 'nr))
		   (tp (sxml:attr op 'type))
		   (ad (sxml:attr op 'address)))
	       ;; attribute is stronger, I think.
	       ;; NB: at least if there's an attribute, then no content.
	       ;;(when (and (not a) t (not tp) (not ad)) (pp op))
	       ;;(when (and a (not t) (not tp) (not ad)) (pp op))
	       (list (cond (ad (string->number ad))
			   (a  (string->symbol a))
			   (else (string->symbol (car (sxml:content op)))))
		     (cond (tp (string->number tp))
			   (t  (string->symbol t))
			   ;; type might be #f
			   (else #f))
		     (and nr (string->number nr)))))))

    (let ((operands ((sxpath "/*[name()='src' or name()='dst']") syntx)))
      (filter-map parse-op operands)))

  (define (parse-entries prefix opcd entries)
    (define (filter-entries entries)
      ;; it's a bit tricky, if an entry has mode attr, then the
      ;; ones without mode is not taken. 
      ;; e.g.
      ;;  architecture is x64 and entries are like the following:
      ;;  ((entry ...) (entry (@ (mode "e"))))
      ;;  in this case only the latter entry is taken. however if
      ;;  it's *not* x64 then the first one must be taken.
      ;; even though DTD says 'e' should be behind of all other
      ;; mode but it seems not the case. so we need to check 
      ;; all entries first.
      (let loop ((entries entries) (match '()) (no-mode '()) (invalid? #f))
	(if (null? entries)
	    (if (null? match)
		(if invalid? '() (reverse! no-mode))
		(reverse! match))
	    (let* ((entry (car entries))
		   (mode (sxml:attr entry 'mode)))
	      (if mode
		  (if (memq (string->symbol mode) arch)
		      (let ((invd? (sxml:attr entry 'attr)))
			;; invalid instruction
			(loop (cdr entries)
			      (if (and invd? (string=? "invd" invd?))
				  match
				  (cons entry match))
			      no-mode
			      (or invalid?
				  (and invd? (string=? "invd" invd?)))))
		      (loop (cdr entries)
			    match
			    no-mode
			    invalid?))
		  (loop (cdr entries)
			match
			(cons entry no-mode)
			invalid?))))))

    (define (parse-entry entry)
      (define (number/false attr entry)
	(let ((a (sxml:attr entry attr)))
	  (and a (string->number a 16))))
      (define (group grp) (and grp (string->symbol (car (sxml:content grp)))))
      (and-let* (( (check-proc-end "/proc_end" entry) )
		 ( (not (equal? "undef" (sxml:attr entry 'attr))) )
		 ;; TODO how to handle multiple syntaxes?
		 (syntx  ((if-car-sxpath "/syntax") entry))
		 ;; no name, no definition
		 (name ((if-car-sxpath "/mnem/text()") syntx))
		 ;; we don't need category obsolate
		 ( (let ((grp1 ((if-car-sxpath "/grp1/text()") entry)))
		     (not (and grp1 (or (string=? "obsol" grp1)
					(string=? "prefix" grp1))))) ))
	(let ((data-size (case (number/false 'op_size entry)
			   ((0) 'b) ((1) 'w) (else #f)))
	      (ext ((if-car-sxpath "/instr_ext/text()") entry))
	      (sign (sxml:attr entry 'sign-ext))
	      (direction (sxml:attr entry 'direction))
	      (pref ((if-car-sxpath "/pref/text()") entry))
	      (extended-opcd ((if-car-sxpath "/opcd_ext/text()") entry))
	      (secondary-opcd ((if-car-sxpath "/sec_opcd/text()") entry)))
	  (let-values (((modrm? imm?) (has-modrm&immdata? syntx)))
	    (list (string->symbol name)
		  (and pref (string->number pref 16))
		  (and secondary-opcd (string->number secondary-opcd 16))
		  (and extended-opcd (string->number extended-opcd 16))
		  (and sign (string->number sign)) ; needed for ModR/M
		  (and direction (string->number direction)) ; ditto
		  data-size 
		  modrm?
		  ;; imm?
		  (and imm? (immdata-size syntx))
		  ;;sign 
		  ;;direction
		  (and ext (string->symbol ext))
		  (parse-operand syntx))))))
    (list prefix opcd (filter-map parse-entry (filter-entries entries))))
  
  (define (parse-pri-opcd pri-opcd prefix)
    (let ((opcd (string->number (sxml:attr pri-opcd 'value) 16)))
      ;; 0x0F in 8086 is POP but we don't want it so skip
      ;; if the instruction is only supported until certain
      ;; processor, then we don't want it either.
      (and (not (and (check-proc-end "/proc_end" pri-opcd)
		     (eqv? prefix opcd)))
	   (parse-entries prefix opcd ((sxpath "/entry") pri-opcd)))))

  (let ((one-byte ((if-car-sxpath "//x86reference/one-byte") sxml))
	(two-byte ((if-car-sxpath "//x86reference/two-byte") sxml)))
    (emit-instr out oarch
     ;; parse one-byte
     (filter-map (cut parse-pri-opcd <> #f) 
		 ((sxpath "/pri_opcd") one-byte))
     ;; parse two-byte
     (filter-map (cut parse-pri-opcd <> #x0F) 
		 ((sxpath "/pri_opcd") two-byte)))))

(define (arch->mode arch)
  (case (string->symbol arch)
    ((x64) '(r p e))
    ;; 32bit
    ((x86) '(r p))
    (else (assertion-violation 'arch "x64 or x86 is required" arch))))

(define (safe-parse in)
  (with-exception-handler
   (lambda (e) (if (warning? e) #t (raise e)))
   (lambda () (ssax:xml->sxml in '()))))

(define (main args)
  (with-args (cdr args)
      ((arch  (#\a "arch") #t "x64")
       (file  (#\i "input") #t "x86reference.xml")
       (ofile (#\o "output") #t #f))
    (let ((sxml (call-with-input-file file safe-parse)))
      (call-with-port (if ofile 
			  (open-file-output-port ofile 
						 (file-options no-fail)
						 (buffer-mode block)
						 (native-transcoder))
			  (current-output-port))
	(cut parse-definitions sxml arch <>)))))
