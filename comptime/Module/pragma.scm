;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/pragma.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun  7 08:44:07 1996                          */
;*    Last change :  Sun Nov 18 10:48:35 2012 (serrano)                */
;*    Copyright   :  1996-2012 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The pragma clause compilation                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_pragma
   (import module_module
	   module_eval
	   tools_error
	   tools_shape
	   type_type
	   engine_param
	   ast_var
	   ast_env
	   ast_remove
	   ast_node
	   type_env
	   effect_feffect
	   (find-location tools_location))
   (export (make-pragma-compiler)
	   (pragma-finalizer)))

;*---------------------------------------------------------------------*/
;*    make-pragma-compiler ...                                         */
;*---------------------------------------------------------------------*/
(define (make-pragma-compiler)
   (instantiate::ccomp
      (id 'pragma)
      (producer (pragma-producer *module*))
      (consumer (lambda (m c) ((pragma-producer m) c)))
      (finalizer pragma-finalizer)))

;*---------------------------------------------------------------------*/
;*    pragma-producer ...                                              */
;*---------------------------------------------------------------------*/
(define (pragma-producer module)
   (lambda (clause)
      (match-case clause
	 ((?- . ?protos)
	  (for-each (lambda (proto) (pragma-parser proto module clause))
		    protos)
	  '())
	 (else
	  (user-error "pragma" "Illegal clause" clause '())))))
   
;*---------------------------------------------------------------------*/
;*    pragma-parser ...                                                */
;*---------------------------------------------------------------------*/
(define (pragma-parser proto module clause)
   (match-case proto
      (((and ?id (? symbol?)) . ?prop)
       (set! *pragma-list* (cons (list id module prop clause) *pragma-list*)))
      (else
       (user-error "pragma" "Illegal clause" clause '()))))

;*---------------------------------------------------------------------*/
;*    *pragma-list* ...                                                */
;*---------------------------------------------------------------------*/
(define *pragma-list* '())

;*---------------------------------------------------------------------*/
;*    pragma-finalizer ...                                             */
;*---------------------------------------------------------------------*/
(define (pragma-finalizer)
   (for-each (lambda (pragma)
		(match-case pragma
		   ((?id ?module ?prop* ?clause)
		    (let ((global (let ((global (find-global/module id module)))
				     (if (global? global)
					 global
					 (find-global/module id 'foreign)))))
		       (if (not (global? global))
			   (warning `(@ ,id ,module)
				    "Can't find global variable for pragma -- "
				    pragma)
			   (set-pragma-properties! global prop* clause))))
		   (else
		    (internal-error "pragma-finalizer"
				    "Illegal \"pragma\" finalizer form"
				    pragma))))
	     *pragma-list*)
   (set! *pragma-list* '())
   'void)

;*---------------------------------------------------------------------*/
;*    set-pragma-properties! ...                                       */
;*---------------------------------------------------------------------*/
(define (set-pragma-properties! global prop* clause)
   (for-each (lambda (prop) (set-pragma-property! global prop clause))
	     prop*))
 
;*---------------------------------------------------------------------*/
;*    set-pragma-property! ...                                         */
;*---------------------------------------------------------------------*/
(define (set-pragma-property! global prop clause)
   (define (sfun-error p::bstring g::global)
      (if (not *all-export-mutable?*)
	  (user-error (string-append "pragma(" p ")")
	     "property is not concerning a function"
	     (shape g)
	     '())))
   (match-case prop
      ((? symbol?)
       (case prop
	  ;; the side-effect-free pragma
	  ((side-effect-free)
	   (let ((val (global-value global)))
	      (if (not (fun? val))
		  (sfun-error "side-effect-free" global)
		  (fun-side-effect-set! val #f))))
	  ;; the no-cfa-top pragma
	  ((no-cfa-top)
	   (let ((val (global-value global)))
	      (if (not (fun? val))
		  (sfun-error "no-cfa-top" global)
		  (fun-top?-set! val #f))))
	  ;; don't trace the argument of the function during
	  ;; the initflow analysis
	  ((no-init-flow)
	   (let ((val (global-value global)))
	      (if (not (fun? val))
		  (sfun-error "no-init-flow" global)
		  (sfun-property-set! val (cons prop (sfun-property val))))))
	  ;; this function is a generated allocator. That property is
	  ;; used when emitting symbol tables for profiling
	  ((allocator)
	   (let ((val (global-value global)))
	      (if (not (sfun? val))
		  (sfun-error "allocator" global)
		  (sfun-property-set! val (cons prop (sfun-property val))))))
	  ((no-trace)
	   (let ((val (global-value global)))
	      (if (not (sfun? val))
		  (sfun-error "no-trace" global)
		  (sfun-property-set! val (cons prop (sfun-property val))))))
	  ((nesting)
	   (let ((val (global-value global)))
	      (if (cfun? val)
		  (global-pragma-set! global
		     (cons 'nesting (global-pragma global))))))
	  ((args-safe)
	   (let ((val (global-value global)))
	      (if (cfun? val)
		  (global-pragma-set! global
		     (cons 'args-safe
			(global-pragma global))))))
	  ((fail-safe)
	   (let ((val (global-value global)))
	      (if (or (sfun? val) (cfun? val))
		  (global-pragma-set! global
		     (cons 'fail-safe
			(global-pragma global))))))
	  (else
	   (user-error "Parse error" "Illegal \"pragma\" form" clause '()))))
      (((and (? symbol?) ?key) . ?val)
       (case key
	  ;; the predicate-of pragma
	  ((predicate-of)
	   (if (not (and (pair? val) (symbol? (car val))))
	       (user-error "Parse error" "Illegal \"predicate-of\" pragma" prop)
	       (let ((type  (use-type! (car val) (find-location prop)))
		     (value (global-value global)))
		  (if (not (fun? value))
		      (sfun-error "predicate-of" global)
		      (begin
			 (fun-predicate-of-set! value type)
			 ;; predicate cannot be remove until
			 ;; coercers introduction pass
			 (remove-var-from! 'coerce global)
			 ;; furthermore a predicate is _always_
			 ;; free of side effects
			 (fun-side-effect-set! value #f))))))
	  ((effect)
	   (let ((value (global-value global)))
	      (if (not (fun? value))
		  (sfun-error "effect" global)
		  (fun-effect-set! value (parse-effect prop)))))
	  (else
	   (user-error "Parse error" "Illegal \"pragma\" form" prop '()))))
      (else
       (user-error "Parse error"
	  "Illegal \"pragma\" form"
	  (if (pair? prop) prop clause)
	  '()))))
	 
