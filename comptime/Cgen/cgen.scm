;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cgen/cgen.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  2 13:17:04 1996                          */
;*    Last change :  Mon Dec 10 00:21:44 2012 (serrano)                */
;*    Copyright   :  1996-2012 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The C production code.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cgen_cgen
   (include "Tools/trace.sch"
	    "Tools/location.sch")
   (import  tools_error
	    tools_shape
	    engine_param
	    type_type
	    type_tools
	    type_cache
	    type_typeof
	    object_class
	    object_slots
	    ast_var
	    ast_node
	    ast_local
	    ast_sexp
	    ast_app
	    ast_dump
	    sync_node
	    effect_effect
	    cgen_cop
	    cgen_emit-cop
	    cgen_capp
	    backend_c_emit
	    backend_c_prototype
	    backend_backend
	    backend_cplib)
   (export  (cgen-function ::global)
	    (node-setq::setq variable::variable value::node)
	    (generic node->cop::cop ::node ::procedure ::bool)
	    (make-local-svar/name::local ::symbol ::type)
	    (bdb-let-var::cop ::cop loc)
	    *the-global*
	    *return-kont*
	    *id-kont*
	    (block-kont cop loc)
	    *stop-kont*))

;*---------------------------------------------------------------------*/
;*    cgen-function ...                                                */
;*---------------------------------------------------------------------*/
(define (cgen-function global::global)
   (when (require-prototype? global)
      (enter-function (global-id global))
      (trace cgen "cgen global: " (shape global) #\Newline)
      (trace (cgen 2) "*void-kont*: " *void-kont* #\Newline)
      (trace (cgen 2) "*return-kont*: " *return-kont* #\Newline)
      (set! *the-global* global)
      (let ((sh (shape global)))
	 (assert (sh) (string? (global-name global))))
      (let* ((sfun (widen!::sfun/C (global-value global)
		      (label (instantiate::clabel
				(loc (sfun-loc (global-value global)))
				(name (global-name global))))
		      (integrated #t)))
	     (loc  (sfun-loc sfun))
	     (cop  (node->cop (sfun-body sfun)
			      (if (eq? (global-type global) *void*)
				  *void-kont*
				  *return-kont*)
			      #f)))
	 (reset-bdb-loc!)
	 (newline *c-port*)
	 (newline *c-port*)
	 (display "/* " *c-port*)
	 (display (shape global) *c-port*)
	 (display " */" *c-port*)
	 ;; we have to emit a dummy location otherwise gdb get confused
	 ;; with the function arguments! Thus all functions looks like
	 ;; starting at the module definition site instead of there correct
	 ;; definition site
	 (emit-bdb-loc #f)
	 ;; we have to reset the loc because we must not emit location
	 ;; before the first c expression otherwise gdb get confused
	 (reset-bdb-loc!)
	 (clabel-body-set! (sfun/C-label sfun) cop)
	 (global->c global)
	 (let ((cop (block-kont (sfun/C-label sfun) loc)))
	    ;; we define a local variable that acts as a temporary variable
	    (display "{ AN_OBJECT;" *c-port*)
	    ;; when compiling for debugging, we have to insert a dummy
	    ;; statement otherwise gdb get confused
	    (if (and (> *bdb-debug* 0) (location? loc))
		(begin
		   (emit-bdb-loc loc)
		   (display "{ obj_t ___ = BUNSPEC; } /* bdb dummy init stmt */"
			    *c-port*)))
	    ;; we now may emit the body
	    (emit-cop cop)
	    ;; emit the current location before the closing bracket
	    (emit-bdb-loc (get-current-bdb-loc))
	    ;; and then clause the function body
	    (fprint *c-port* "}"))
	 (no-bdb-newline)
	 (leave-function))))
  
;*---------------------------------------------------------------------*/
;*    *the-global* ...                                                 */
;*    -------------------------------------------------------------    */
;*    This variable is use to implement global tail calls.             */
;*---------------------------------------------------------------------*/
(define *the-global* #unspecified)

;*---------------------------------------------------------------------*/
;*    global->c ...                                                    */
;*---------------------------------------------------------------------*/
(define (global->c g::global)
   (with-access::global g (type id name import value)
      (when (eq? import 'export) (display "BGL_EXPORTED_DEF " *c-port*))
      (display (make-typed-declaration
		type
		(string-append
		 name
		 (if (null? (sfun-args value))
		     "()"
		     (string-append
		      "("
		      (let loop ((args (sfun-args value)))
			 (if (null? (cdr args))
			     (with-access::local (car args) (name id type)
				(string-append
				 (make-typed-declaration type name) ")"))
			     (with-access::local (car args) (name id type)
				(string-append
				 (make-typed-declaration type name)
				 ", "
				 (loop (cdr args))))))))))
	       *c-port*))
   (no-bdb-newline))

;*---------------------------------------------------------------------*/
;*    *return-kont* ...                                                */
;*---------------------------------------------------------------------*/
(define *return-kont*
   (lambda (cop)
      (instantiate::creturn
	 (loc   (cop-loc cop))
	 (value (cond
		   ((csetq? cop)
		    (instantiate::csequence
		       (loc    (cop-loc cop))
		       (c-exp? #t)
		       (cops (list cop (instantiate::catom
					  (value #unspecified))))))
		   (else
		    cop))))))

;*---------------------------------------------------------------------*/
;*    *id-kont* ...                                                    */
;*---------------------------------------------------------------------*/
(define *id-kont* (lambda (cop) cop))
      
;*---------------------------------------------------------------------*/
;*    *void-kont* ...                                                  */
;*---------------------------------------------------------------------*/
(define *void-kont*
   (lambda (cop)
      (instantiate::cvoid (value cop))))

;*---------------------------------------------------------------------*/
;*    *stop-kont* ...                                                  */
;*---------------------------------------------------------------------*/
(define *stop-kont*
   (lambda (cop)
      (instantiate::stop (value cop))))

;*---------------------------------------------------------------------*/
;*    block-kont ...                                                   */
;*---------------------------------------------------------------------*/
(define (block-kont cop loc)
   (cond
      ((isa? cop cblock)
       cop)
      (else
       (instantiate::cblock
	  (body cop)
	  (loc  loc)))))

;*---------------------------------------------------------------------*/
;*    *fail-kont* ...                                                  */
;*---------------------------------------------------------------------*/
(define *fail-kont* (lambda (cop) cop))

;*---------------------------------------------------------------------*/
;*    *exit-kont* ...                                                  */
;*---------------------------------------------------------------------*/
(define *exit-kont* (lambda (cop) cop))

;*---------------------------------------------------------------------*/
;*    make-setq-kont ...                                               */
;*---------------------------------------------------------------------*/
(define (make-setq-kont var loc kont)
   (lambda (cop)
      (if (cfail? cop)
	  cop
	  (kont (instantiate::csetq
		   (var (instantiate::varc (variable var)))
		   (value (cond
			     ((csetq? cop)
			      (instantiate::csequence
				 (loc (cop-loc cop))
				 (c-exp? #t)
				 (cops (list cop
					       (instantiate::catom
						  (loc (cop-loc cop))
						  (value #unspecified))))))
			     (else
			      cop)))
		   (loc loc))))))
			  
;*---------------------------------------------------------------------*/
;*    node->cop ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (node->cop::cop node::node kont::procedure inpushexit::bool))

;*---------------------------------------------------------------------*/
;*    node->cop ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::atom kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::atom kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::atom node (value loc)
      (kont (instantiate::catom
	       (value value)
	       (loc   loc)))))

;*---------------------------------------------------------------------*/
;*    node->cop ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::kwote kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::kwote kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (internal-error "node->cop" "Unexpected `kwote' node" (shape node)))

;*---------------------------------------------------------------------*/
;*    node->cop ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::var kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::var kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (trace (cgen 4) "(node->cop node::var kont): " (shape node) #\Newline
	  "  var-variable-name: " (variable-name (var-variable node))
	  #\Newline)
   (with-access::var node (variable loc)
      (kont (instantiate::varc
	       (variable variable)
	       (loc      loc)))))

;*---------------------------------------------------------------------*/
;*    node->cop ::closure ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::closure kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::closure kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (internal-error "node->cop" "Unexpected `closure' node" (shape node)))

;*---------------------------------------------------------------------*/
;*    node->cop ::sequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::sequence kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::sequence kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::sequence node (nodes loc)
      (let ((exp nodes))
	 (cond
	    ((null? exp)
	     (kont (instantiate::nop
		      (loc loc))))
	    ((null? (cdr exp))
	     (let ((cop (node->cop (car exp) kont inpushexit)))
		(instantiate::stop (value cop))))
	    (else
	     (let ((inpushexit (or inpushexit (is-push-exit? (car exp)))))
		(let loop ((exp exp)
			   (new '()))
		   (if (null? (cdr exp))
		       (let ((cop (node->cop (car exp) kont inpushexit)))
			  (instantiate::csequence
			     (loc  (cop-loc cop))
			     (cops (reverse! (cons cop new)))))
		       (begin
			  (if (not (side-effect? (car exp)))
			      (loop (cdr exp) new)
			      (loop (cdr exp)
				 (cons (node->cop (car exp) *stop-kont* inpushexit)
				    new))))))))))))

;*---------------------------------------------------------------------*/
;*    is-push-exit? ...                                                */
;*---------------------------------------------------------------------*/
(define (is-push-exit? node::node)
   (when (isa? node app)
      (with-access::app node (fun)
	 (let ((var (var-variable fun)))
	    (when (global? var)
	       (eq? (variable-id var) 'push-exit!))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::sync ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::sync kont inpushexit)
   (node->cop (sync->sequence node) kont inpushexit))

;*---------------------------------------------------------------------*/
;*    extern->cop ...                                                  */
;*---------------------------------------------------------------------*/
(define (extern->cop format::bstring args-safe node::extern kont inpushexit)
   (trace (cgen 3)
	  "(extern->cop node::extern kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::extern node (expr* loc)
      (node-args->cop expr*
	 args-safe
	 loc
	 (lambda (new-args)
	    (kont (instantiate::cpragma
		     (loc loc)
		     (format format)
		     (args new-args))))
	 inpushexit)))
   
;*---------------------------------------------------------------------*/
;*    node->cop ::pragma ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::pragma kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::pragma kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::pragma node (format)
      (extern->cop format #f node kont inpushexit)))

;*---------------------------------------------------------------------*/
;*    node->cop ::private ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::private kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::getfield kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::private node (c-format)
      (extern->cop c-format #t node kont inpushexit)))
   
;*---------------------------------------------------------------------*/
;*    node->cop ::cast ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::cast kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::cast kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::cast node (arg type loc)
      (node-args->cop (list arg)
	 #t
	 loc
	 (lambda (new-args)
	    (kont (instantiate::ccast
		     (type type)
		     (loc loc)
		     (arg (car new-args)))))
	 inpushexit)))

;*---------------------------------------------------------------------*/
;*    node->cop ::setq ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::setq kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::setq kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::setq node (value loc)
      (let ((var (var-variable (setq-var node))))
	 (if (and (var? value) (eq? var (var-variable value)))
	     (kont (*void-kont* (instantiate::catom
				   (loc loc)
				   (value #unspecified))))
	     (node->cop value (make-setq-kont var loc kont) inpushexit)))))

;*---------------------------------------------------------------------*/
;*    node->cop ::conditional ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::conditional kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::conditional kont): " (shape node) #\Newline
	  "  loc: " (node-loc node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::conditional node (test true false loc)
      (let* ((aux   (make-local-svar/name 'test *bool*))
	     (ctest (node->cop (node-setq aux test) *id-kont* inpushexit)))
	 (if (and (csetq? ctest) (eq? (varc-variable (csetq-var ctest)) aux))
	     (instantiate::cif
		(test (csetq-value ctest))
		(true (block-kont (node->cop true kont inpushexit) loc))
		(false (block-kont (node->cop false kont inpushexit) loc))
		(loc   loc))
	     (instantiate::cblock
		(loc loc)
		(body
		 (instantiate::csequence
		    (loc loc)
		    (cops
		     (list
		      (instantiate::local-var
			 (vars (list aux))
			 (loc  loc))
		      ctest
		      (instantiate::cif
			 (test (instantiate::varc
				  (variable aux)
				  (loc loc)))
			 (false (block-kont (node->cop false kont inpushexit)
				   loc))
			 (true (block-kont (node->cop true kont inpushexit)
				  loc))
			 (loc  loc)))))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::fail ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::fail kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::fail kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::fail node (proc msg obj loc)
      (node-args->cop (list proc msg obj)
	 #f
	 loc
	 (lambda (new-args)
	    (*fail-kont*
	       (instantiate::cfail
		  (loc loc)
		  (proc (car new-args))
		  (msg (cadr new-args))
		  (obj (caddr new-args)))))
	 inpushexit)))

;*---------------------------------------------------------------------*/
;*    node->cop ::select ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::select kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::select kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::select node (clauses test item-type loc)
      (for-each (lambda (clause)
		   (set-cdr! clause (node->cop (cdr clause) kont inpushexit)))
		clauses)
      (let ((aux  (make-local-svar/name 'aux item-type)))
	 (let ((cop (node->cop (node-setq aux test) *id-kont* inpushexit)))
	    (if (and (csetq? cop)
		     (eq? (varc-variable (csetq-var cop)) aux))
		(instantiate::cswitch
		   (loc  loc)
		   (test (csetq-value cop))
		   (clauses clauses))
		(instantiate::cblock
		   (loc loc)
		   (body (instantiate::csequence
			    (loc loc)
			    (cops (list (instantiate::local-var
					   (loc  loc)
					   (vars (list aux)))
					cop
					(instantiate::cswitch
					   (loc  loc)
					   (test (instantiate::varc
						    (loc loc)
						    (variable aux)))
					   (clauses clauses))))))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::let-fun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::let-fun kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::let-fun kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::let-fun node (body locals loc)
      ;; local function are open-coded of their first call site.
      ;; So, the compilation of `let-fun' construction is just
      ;; the declaration of all local functions' formals (and
      ;; a initialization mark in local function to express the
      ;; need of integration of the first call site).
      (let loop ((locals      locals)
		 (all-formals '()))
	 (if (null? locals)
	     (block-kont
	      (bdb-let-var
	       (instantiate::csequence
		  (loc loc)
		  (cops (list (instantiate::local-var
				 (loc  loc)
				 (vars all-formals))
			      (node->cop body kont inpushexit))))
	       loc)
	      #f)
	     (let ((local (car locals)))
		(set-variable-name! local)
		(let* ((fun (widen!::sfun/C (local-value local)
			       (label (instantiate::clabel
					 (loc  (sfun-loc (local-value local)))
					 (name (local-name local))))
			       (integrated #f)))
		       (formals (sfun-args fun)))
		   (for-each set-variable-name! formals)
		   (loop (cdr locals) (append formals all-formals))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::let-var kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::let-var kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::let-var node (body bindings loc)
      (let ((decls (instantiate::local-var
		      (loc loc)
		      (vars (map car bindings))))
	    (sets  (map (lambda (x)
			   (set-variable-name! (car x))
			   (node->cop (node-setq (car x) (cdr x))
			      *stop-kont* inpushexit))
			bindings))
	    (body  (let ((cop (node->cop body kont inpushexit)))
		      (instantiate::stop
			 (value cop)))))
	 (block-kont
	  (bdb-let-var
	   (instantiate::csequence
	      (loc loc)
	      (cops (cons decls (append sets (list body)))))
	   loc)
	  loc))))

;*---------------------------------------------------------------------*/
;*    bdb-let-var ...                                                  */
;*---------------------------------------------------------------------*/
(define (bdb-let-var cop loc)
   (if (and (>fx *bdb-debug* 0) (location? loc))
       (instantiate::bdb-block
	  (body cop)
	  (loc  loc))
       cop))
   
;*---------------------------------------------------------------------*/
;*    node->cop ::set-ex-it ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::set-ex-it kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::set-ex-it kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::set-ex-it node (var body loc)
      (let ((exit (var-variable var)))
	 (set-variable-name! exit)
	 (instantiate::csequence
	    (loc loc)
	    (cops (list
		   (instantiate::cpragma
		      (loc    loc)
		      (format "jmp_buf_t jmpbuf")
		      (args '()))
		   (instantiate::local-var
		      (loc loc)
		      (vars (list (var-variable var))))
		   (instantiate::cset-ex-it
		      (loc loc)
		      (exit (instantiate::varc
			       (loc loc)
			       (variable exit)))
		      (jump-value (node->cop (instantiate::pragma
						(loc loc)
						(type *_*)
						(format "BGL_EXIT_VALUE()")
						(expr* '()))
					     kont inpushexit))
		      (body (instantiate::csequence
			       (loc loc)
			       (cops
				(list
				 (node->cop
				  (node-setq
				   exit
				   (instantiate::pragma
				      (loc loc)
				      (type *_*)
				      (format
				       (string-append
					"("
					(string-sans-$
					 (type-name (local-type exit)))
					")jmpbuf"))
				      (expr* '())))
				  *id-kont* inpushexit)
				 (node->cop body kont inpushexit))))))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::jump-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::jump-ex-it kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::jump-ex-it kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::jump-ex-it node (exit value loc)
      (let* ((vaux  (make-local-svar/name 'aux *obj*))
	     (vcop  (node->cop (node-setq vaux value) *id-kont* inpushexit))
	     (exit  exit)
	     (eaux  (make-local-svar/name 'exit *procedure*))
	     (ecop  (node->cop (node-setq eaux exit) *id-kont* inpushexit)))
	 (cond
	    ((and (csetq? vcop) (eq? (varc-variable (csetq-var vcop)) vaux)
		  (csetq? ecop) (eq? (varc-variable (csetq-var ecop)) eaux))
	     (*exit-kont*
	      (instantiate::cjump-ex-it
		 (loc loc)
		 (exit (csetq-value ecop))
		 (value (csetq-value vcop)))))
	    ((and (csetq? vcop) (eq? (varc-variable (csetq-var vcop)) vaux))
	     (instantiate::cblock
		(loc loc)
		(body (instantiate::csequence
			 (loc loc)
			 (cops (list (instantiate::local-var
					(loc loc)
					(vars (list eaux)))
				       (instantiate::csequence
					  (loc loc)
					  (cops (list ecop)))
				       (*exit-kont*
					(instantiate::cjump-ex-it
					   (loc loc)
					   (exit (instantiate::varc
						    (loc loc)
						    (variable eaux)))
					   (value (csetq-value vcop))))))))))
	    ((and (csetq? ecop) (eq? (varc-variable (csetq-var ecop)) eaux))
	     (instantiate::cblock
		(loc loc)
		(body (instantiate::csequence
			 (loc loc)
			 (cops (list (instantiate::local-var
					(loc loc)
					(vars (list vaux)))
				     (instantiate::csequence
					(loc loc)
					(cops (list vcop)))
				     (*exit-kont*
				      (instantiate::cjump-ex-it
					 (loc loc)
					 (exit (csetq-value ecop))
					 (value (instantiate::varc
						   (loc loc)
						   (variable vaux)))))))))))
	    (else
	     (instantiate::cblock
		(loc loc)
		(body (instantiate::csequence
			 (loc loc)
			 (cops
			  (list
			   (instantiate::local-var
			      (loc loc)
			      (vars (list eaux vaux)))
			   (instantiate::csequence
			      (loc loc)
			      (cops (list ecop vcop)))
			   (*exit-kont*
			    (instantiate::cjump-ex-it
			       (loc loc)
			       (exit (instantiate::varc
					(variable eaux)))
			       (value (instantiate::varc
					 (variable vaux)))))))))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::make-box ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::make-box kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::make-box kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::make-box node (value loc)
      (if (or (var? value) (atom? value) (kwote? value))
	  (node->cop value
		     (lambda (v) (kont (instantiate::cmake-box
					  (loc loc)
					  (value v))))
		      inpushexit)
	  (let* ((aux  (make-local-svar/name 'cellval *obj*))
		 (cval (node->cop (node-setq aux value) *id-kont* inpushexit)))
	     (instantiate::cblock
		(loc loc)
		(body (instantiate::csequence
			 (loc loc)
			 (cops (list
				(instantiate::local-var
				   (loc loc)
				   (vars (list aux)))
				cval
				(kont
				 (instantiate::cmake-box
				    (loc loc)
				    (value (instantiate::varc
					      (loc loc)
					      (variable aux))))))))))))))

;*---------------------------------------------------------------------*/
;*    node->cop ::box-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::box-ref kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::box-ref kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::box-ref node (var loc)
      (kont (node->cop var
	       (lambda (v) (instantiate::cbox-ref
			      (loc loc)
			      (var v)))
	        inpushexit))))

;*---------------------------------------------------------------------*/
;*    node->cop ::box-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->cop node::box-set! kont inpushexit)
   (trace (cgen 3)
	  "(node->cop node::box-set! kont): " (shape node) #\Newline
	  "  kont: " kont #\Newline)
   (with-access::box-set! node (var value loc)
      (let ((v (var-variable var)))
	 (node->cop value
	    (lambda (vl) (kont (instantiate::cbox-set!
				  (loc loc)
				  (var (instantiate::varc
					  (loc loc)
					  (variable v)))
				  (value vl))))
	     inpushexit))))

;*---------------------------------------------------------------------*/
;*    node-setq ...                                                    */
;*---------------------------------------------------------------------*/
(define (node-setq::setq variable::variable value::node)
   (instantiate::setq
      (loc (node-loc value))
      (type *unspec*)
      (var (instantiate::var
	      (loc #f)
	      (type (variable-type variable))
	      (variable variable)))
      (value value)))

;*---------------------------------------------------------------------*/
;*    make-local-svar/name ...                                         */
;*---------------------------------------------------------------------*/
(define (make-local-svar/name::local id::symbol type::type)
   (let ((local (make-local-svar id type)))
      (if (not (string? (local-name local)))
	  (error "make-local-svar/name" "Illegal local name" local))
      local))

;*---------------------------------------------------------------------*/
;*    no-bdb-newline ...                                               */
;*    -------------------------------------------------------------    */
;*    Emit a newline only if not compiling for bdb.                    */
;*---------------------------------------------------------------------*/
(define (no-bdb-newline)
   (if (eq? *bdb-debug* 0)
       (newline *c-port*)))

;*---------------------------------------------------------------------*/
;*    node-args->cop ...                                               */
;*---------------------------------------------------------------------*/
(define (node-args->cop args args-safe loc kont inpushexit)
   (let loop ((old-actuals  args)
	      (new-actuals  '())
	      (aux          (make-local-svar/name 'aux *obj*))
	      (auxs         '())
	      (exps         '()))
      (if (null? old-actuals)
	  (if (null? auxs)
	      (kont (reverse! new-actuals))
	      (instantiate::cblock
		 (body (instantiate::csequence
			  (loc loc)
			  (cops (list
				 (instantiate::local-var
				    (vars auxs)
				    (loc  loc))
				 (instantiate::csequence (cops exps))
				 (kont (reverse! new-actuals))))))))
	  (let ((cop (node->cop (node-setq aux (car old-actuals))
			*id-kont* inpushexit)))
	     (if (and (csetq? cop)
		      (eq? (varc-variable (csetq-var cop)) aux)
		      (or args-safe
			  (catom? (csetq-value cop))
			  (varc? (csetq-value cop))
			  (cpragma? (csetq-value cop))))
		 (loop (cdr old-actuals)
		       (cons (csetq-value cop) new-actuals)
		       aux
		       auxs
		       exps)
		 (begin
		    (local-type-set! aux (get-type (car old-actuals)))
		    (loop (cdr old-actuals)
			  (cons (instantiate::varc
				   (variable aux)
				   (loc      loc))
				new-actuals)
			  (make-local-svar/name 'aux *obj*)
			  (cons aux auxs)
			  (cons cop exps))))))))
   
   
