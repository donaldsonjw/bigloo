;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/let.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jan  1 11:37:29 1995                          */
;*    Last change :  Thu Jul  3 09:07:16 2014 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `let->ast' translator                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_let
   (include "Ast/node.sch"
	    "Tools/trace.sch"
	    "Tools/location.sch")
   (import  type_cache
	    tools_progn
	    tools_shape
	    tools_location
	    tools_misc
	    engine_param
	    ast_ident
	    ast_sexp
	    ast_local
	    ast_substitute
	    ast_occur
	    ast_remove
	    backend_backend)
   (export  (let-sym? ::obj)
	    (let-sym::symbol)
	    (let->node::node <sexp> <stack> ::obj ::symbol)
	    (letrec*->node::node <sexp> <stack> ::obj ::symbol)))

;*---------------------------------------------------------------------*/
;*    *let* ...                                                        */
;*---------------------------------------------------------------------*/
(define *let* (gensym 'let))

;*---------------------------------------------------------------------*/
;*    let-sym ...                                                      */
;*---------------------------------------------------------------------*/
(define (let-sym)
   *let*)

;*---------------------------------------------------------------------*/
;*    let-sym? ...                                                     */
;*---------------------------------------------------------------------*/
(define (let-sym? sym)
   (eq? sym *let*))

;*---------------------------------------------------------------------*/
;*    let->node ...                                                    */
;*---------------------------------------------------------------------*/
(define (let->node exp stack oloc site)
   (trace (ast 3)
      "*** LET *******: " (shape exp) #\Newline
      "            loc: " (find-location/loc exp #f) #\Newline
      "        old-loc: " oloc #\Newline
      "           body: " (match-case exp
			     ((?- ?- . ?body)
			      (find-location/loc body #f))
			     (else
			      '???))
      #\Newline)
   (match-case exp
      ((?- () . ?body)
       ;; we don't remove explicit user let.
       (let* ((nloc (find-location/loc exp oloc))
	      (bloc (if (pair? body)
			(find-location/loc (car body) nloc)
			nloc))
	      (body (sexp->node (normalize-progn body) stack bloc site)))
	  (trace (ast 3)
	     "make-empty-let: " (shape exp) #\Newline
	     "bloc: " bloc #\Newline
	     "nloc: " nloc #\Newline)
	  (instantiate::let-var
	     (loc nloc)
	     (type (strict-node-type *_* (node-type body)))
	     (bindings '())
	     (body body)
	     (removable? (backend-remove-empty-let (the-backend))))))
      ((?- ?bindings . ?-)
       (if (or (not (or (pair? bindings) (null? bindings)))
	       (let loop ((bindings bindings))
		  (if (null? bindings)
		      #f
		      (let ((binding (car bindings)))
			 (match-case binding
			    ((?- ?-)
			     (loop (cdr bindings)))
			    (else
			     #t))))))
	   (error-sexp->node
	      (string-append "Illegal " (symbol->string (car exp)) "' form")
	      exp
	      (find-location/loc exp oloc))
	   (make-smart-generic-let
	      (car exp)
	      (make-generic-let exp stack oloc site)
	      site)))
      (else
       (error-sexp->node
	  (string-append "Illegal " (symbol->string (car exp)) "' form")
	  exp
	  (find-location/loc exp oloc)))))

;*---------------------------------------------------------------------*/
;*    make-generic-let ...                                             */
;*---------------------------------------------------------------------*/
(define (make-generic-let exp stack oloc site)
   (let* ((bindings   (cadr exp))
	  (loc        (find-location/loc exp oloc))
	  (bloc       (if (pair? (cddr exp))
			  (find-location/loc (caddr exp) #f)
			  #f))
	  (bloc-exp   (if (pair? (cddr exp))
			  (caddr exp)
			  #f))
	  (body       (normalize-progn (cddr exp)))
	  (loc-bis    (find-location/loc body loc))
	  (nloc       (if (location? bloc)
			  bloc
			  loc))
	  (frame      (map (lambda (binding)
			      (let* ((var-id (parse-id (car binding) nloc))
				     (id (car var-id))
				     (type (cdr var-id)))
				 (if (user-symbol? id)
				     (make-user-local-svar id type)
				     (make-local-svar id type))))
			   bindings))
	  (new-stack  (append frame stack)))
      (trace (ast 3)
	     "make-generic-let: " (shape exp) #\Newline
	     "loc: " loc #\Newline
	     "bloc: " bloc " [exp: " (shape bloc-exp) "]" #\Newline
	     "loc-bis: " loc-bis #\Newline
	     "nloc: " nloc #\Newline)
      (let* ((body     (sexp->node body new-stack nloc 'value))
	     (bstack   (if (or (eq? (car exp) 'let) (let-sym? (car exp)))
			   stack
			   new-stack))
	     (bindings (map (lambda (binding var)
			       (cons var
				     (sexp->node
				      (normalize-progn (cdr binding))
				      bstack
				      (find-location/loc binding nloc)
				      'value)))
			    bindings
			    frame))
	     (loc      (let ((loc (find-location/loc
				   exp 
				   (if (pair? bindings)
				       (node-loc (cdr (car bindings)))
				       (node-loc body)))))
			  (if (location? loc)
			      loc
			      oloc)))
	     (node (instantiate::let-var
		      (loc loc)
		      (type *_*)
		      (bindings bindings)
		      (body body))))
	 (occur-node! node)
	 (node-remove! node)
	 node)))

;*---------------------------------------------------------------------*/
;*    make-smart-generic-let ...                                       */
;*    -------------------------------------------------------------    */
;*    We patch bindings which concerns a function and where the        */
;*    variable is never mutated. These bindings are put all together   */
;*    in a labels form.                                                */
;*    -------------------------------------------------------------    */
;*    We try to apply the following transformation:                    */
;*    (let (... (f (labels ((aux args body)) aux)) ...) ...)           */
;*       -->                                                           */
;*    (labels ((f args body)) (let (...) ...))                         */
;*---------------------------------------------------------------------*/
(define (make-smart-generic-let let/letrec node-let site)
   (let loop ((bindings (let-var-bindings node-let))
	      (fun      '())
	      (value    '()))
      (if (null? bindings)
	  (begin
	     (trace (ast 3)
		    "make-smart-generic-let: " (shape node-let) #\Newline
		    "    fun: " (length fun) #\Newline
		    "    values: " (length value) #\Newline)
	     (cond
		((null? fun)
		 (let ((vars (map car (let-var-bindings node-let))))
		    (let-or-letrec let/letrec node-let vars)))
		((null? value)
		 (let->labels fun (let-var-body node-let) site))
		(else
		 (let ((vars (map car (let-var-bindings node-let))))
		    ;; first we ajust let-var bindings
		    (let-var-bindings-set! node-let (reverse! value))
		    ;; then, we send the let form to the `let-or-letrec'
		    ;; function
		    (let* ((nlet (let-or-letrec let/letrec node-let vars))
			   (nbody (let->labels fun (let-var-body nlet) site)))
		       (with-access::let-var nlet (body type)
			  (set! body nbody)
			  (set! type (strict-node-type *_* type)))
		       nlet)))))
	  (let* ((binding (car bindings))
		 (var     (car binding))
		 (sexp    (cdr binding)))
	     (if (let-fun? sexp)
		 (let* ((locals (let-fun-locals sexp))
			(body   (let-fun-body   sexp)))
		    (if (or (null? locals) (not (null? (cdr locals))))
			;; several functions are introduced by the let-fun
			;; construction or, the body of the construction
			;; include several forms. We skip ...
			(loop (cdr bindings)
			      fun
			      (cons (car bindings) value))
			(if (var? body)
			    (let ((res (var-variable body))
				  (aux (car locals)))
			       (if (or (not (eq? res aux))
				       ;; the result of the labels
				       ;; construction is not the
				       ;; introduced variable.
				       (eq? (local-access var) 'write)
				       (not (or
					     (eq? (local-type var) *procedure*)
					     (eq? (local-type var) *_*)
					     (eq? (local-type var) *obj*))))
				   ;; the variable is mutated, skip it
				   (loop (cdr bindings)
					 fun
					 (cons (car bindings) value))
				   ;; yes, we have found one
				   (loop (cdr bindings)
					 (cons (car bindings) fun)
					 value)))
			    (loop (cdr bindings)
				  fun
				  (cons (car bindings) value)))))
		 (loop (cdr bindings)
		       fun
		       (cons (car bindings) value)))))))
	      
;*---------------------------------------------------------------------*/
;*    let-or-letrec ...                                                */
;*    -------------------------------------------------------------    */
;*    Let differs from letrec in the sense that in a letrec form all   */
;*    bindings must be introduced by the unspecified value and         */
;*    it must exists an initialization stage which initializes all     */
;*    introduced local variables. This means that in a letrec form     */
;*    all variables have to be bound to unspecified then, they have    */
;*    to be mutated to their correct values.                           */
;*---------------------------------------------------------------------*/
(define (let-or-letrec let/letrec node-let vars)
   
   (define (safe-rec-val? val)
      (or (atom? val) (closure? val) (kwote? val)
	  (and (sequence? val) (every safe-rec-val? (sequence-nodes val)))))

   (define (safe-rec-val-optim? val vars::pair-nil)
      (or (safe-rec-val? val)
	  (cond
	     ((null? val)
	      #t)
	     ((atom? val)
	      #t)
	     ((var? val)
	      (not (memq (var-variable val) vars)))
	     ((sequence? val)
	      (safe-rec-val-optim? (sequence-nodes val) vars))
	     ((app? val)
	      (with-access::app val (fun args)
		 (and (safe-rec-val-optim? fun vars)
		      (safe-rec-val-optim? args vars))))
	     ((pair? val)
	      (every (lambda (v) (safe-rec-val-optim? v vars)) val))
	     ((app-ly? val)
	      (with-access::app-ly val (fun arg)
		 (and (safe-rec-val-optim? fun vars)
		      (safe-rec-val-optim? arg vars))))
	     ((funcall? val)
	      (with-access::funcall val (fun args)
		 (and (safe-rec-val-optim? fun vars)
		      (safe-rec-val-optim? args vars))))
	     ((extern? val)
	      (with-access::extern val (expr*)
		 (every (lambda (e)
			   (safe-rec-val-optim? e vars))
		    expr*)))
	     ((conditional? val)
	      (with-access::conditional val (test true false)
		 (and (safe-rec-val-optim? test vars)
		      (safe-rec-val-optim? true vars)
		      (safe-rec-val-optim? false vars))))
	     ((setq? val)
	      (with-access::setq val (var value)
		 (and (safe-rec-val-optim? var vars)
		      (safe-rec-val-optim? value vars))))
	     ((fail? val)
	      (with-access::fail val (proc msg obj)
		 (and (safe-rec-val-optim? proc vars)
		      (safe-rec-val-optim? msg vars)
		      (safe-rec-val-optim? obj vars))))
	     ((select? val)
	      (with-access::select val (test clauses)
		 (and (safe-rec-val-optim? test vars)
		      (every (lambda (clause)
				(safe-rec-val-optim? (cdr clause) vars))
			 clauses))))
	     ((let-fun? val)
	      (with-access::let-fun val (body locals)
		 (and (safe-rec-val-optim? body vars)
		      (every (lambda (f)
				(safe-rec-val-optim?
				   (sfun-body (local-value f)) vars))
			 locals))))
	     ((let-var? val)
	      (with-access::let-var val (body bindings)
		 (and (safe-rec-val-optim? body vars)
		      (every (lambda (binding)
				(safe-rec-val-optim? (cdr binding) vars))
			 bindings))))
	     ((set-ex-it? val)
	      (with-access::set-ex-it val (var body)
		 (and (safe-rec-val-optim? var vars)
		      (safe-rec-val-optim? body vars))))
	     ((jump-ex-it? val)
	      (with-access::jump-ex-it val (exit value)
		 (and (safe-rec-val-optim? exit vars)
		      (safe-rec-val-optim? value vars))))
	     ((make-box? val)
	      (with-access::make-box val (value)
		 (safe-rec-val-optim? value vars)))
	     ((box-ref? val)
	      (with-access::box-ref val (var)
		 (safe-rec-val-optim? var vars)))
	     ((box-set!? val)
	      (with-access::box-set! val (var value)
		 (and (safe-rec-val-optim? var vars)
		      (safe-rec-val-optim? value vars))))
	     (else
	      #f))))

   (define (safe-let? node)
      (with-access::let-var node (bindings)
	 (every (lambda (b) (safe-rec-val? (cdr b))) bindings)))
   
   (define (safe-let-optim? node)
      (with-access::let-var node (bindings)
	 (every (lambda (b)
		   (when (eq? (variable-access (car b)) 'read)
		      (safe-rec-val-optim? (cdr b) vars)))
	    bindings)))

   (cond
      ((or (eq? let/letrec 'let) (let-sym? let/letrec))
       node-let)
      ((safe-let? node-let)
       node-let)
      ((and (>=fx *optim* 1) (not *call/cc?*) (safe-let-optim? node-let))
       node-let)
      ((eq? let/letrec 'letrec*)
       (let* ((bindings (let-var-bindings node-let))
	      (body     (let-var-body node-let))
	      (seq      (if (sequence? body)
			    body
			    (instantiate::sequence
			       (loc (node-loc body))
			       (type *_*)
			       (nodes (list body))))))
	  (let-var-body-set! node-let seq)
	  (let loop ((bindings  bindings)
		     (nsequence '()))
	     (if (null? bindings)
		 (begin
		    (let-var-body-set!
		     node-let
		     (instantiate::sequence
			(loc (node-loc seq))
			(type *_*)
			(nodes (append (reverse! nsequence)
				       (sequence-nodes seq)))))
		    node-let)
		 (let* ((binding (car bindings))
			(var (car binding))
			(val (cdr binding))
			(loc (node-loc val)))
		    (let ((init (instantiate::setq
				   (loc loc)
				   (type *unspec*)
				   (var (instantiate::var
					   (type *_*)
					   (loc loc)
					   (variable var)))
				   (value val))))
		       (use-variable! var loc 'set!)
		       (set-cdr! binding
				 (sexp->node #unspecified '() loc 'value))
		       (loop (cdr bindings)
			     (cons init nsequence))))))))
      (else
       (let* ((bindings (let-var-bindings node-let))
	      (body     (let-var-body node-let))
	      (seq      (if (sequence? body)
			    body
			    (instantiate::sequence
			       (loc (node-loc body))
			       (type *_*)
			       (nodes (list body))))))
	  (let-var-body-set! node-let seq)
	  (let loop ((bindings  bindings)
		     (nbindings '())
		     (nsequence (sequence-nodes seq)))
	     (if (null? bindings)
		 (let* ((typ (if (pair? nsequence)
				 (node-type (car (last-pair nsequence)))
				 *unspec*))
			(seq (instantiate::sequence
				(loc (node-loc seq))
				(type *_*)
				(nodes nsequence)))
			(letb (instantiate::let-var
				 (loc (node-loc node-let))
				 (type (strict-node-type *_* (node-type node-let)))
				 (bindings nbindings)
				 (body seq)
				 (removable? #t))))
		    (let-var-body-set! node-let letb)
		    node-let)
		 (let* ((binding (car bindings))
			(var     (car binding))
			(val     (cdr binding))
			(loc     (node-loc val))
			(nvar    (make-local-svar (gensym) *_*)))
		    (let ((init (instantiate::setq
				   (loc loc)
				   (type *unspec*)
				   (var (instantiate::var
					   (type *_*)
					   (loc loc)
					   (variable var)))
				   (value (instantiate::var
					     (type *_*)
					     (loc loc)
					     (variable nvar))))))
		       (use-variable! var loc 'set!)
		       (use-variable! nvar loc 'set!)
		       (set-cdr! binding
				 (sexp->node #unspecified '() loc 'value))
		       (loop (cdr bindings)
			     (cons (cons nvar val) nbindings)
			     (cons init nsequence))))))))))
 
;*---------------------------------------------------------------------*/
;*    let->labels ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function creates a `labels' construction for variables      */
;*    introduced in a `let' form which are never mutated and bound     */
;*    to functions.                                                    */
;*---------------------------------------------------------------------*/
(define (let->labels value-bindings node site)
   ;; we compute (allocate) the list of new functions
   (let ((old-funs (map car value-bindings))
	 (new-funs (map (lambda (binding)
			   (let* ((ovar (car binding))
				  (val  (cdr binding))
				  (aux  (car (let-fun-locals val)))
				  (id   (if (local-user? ovar)
					    (local-id ovar)
					    (local-id aux)))
				  (new  (make-local-svar id (local-type aux))))
			      (local-value-set! new (local-value aux))
			      (local-user?-set! new (or (local-user? aux)
							(local-user? ovar)))
			      (local-name-set! new (local-name aux))
			      new))
			value-bindings)))
      (let loop ((vbindings value-bindings)
		 (nvars     new-funs))
	 (if (null? vbindings)
	     ;; the call to substitute! has only the goal to introduce
	     ;; the `closure' constructions
	     (let* ((body (substitute! old-funs new-funs node site))
		    (funs (reverse! new-funs))
		    (loc  (if (and (pair? funs)
				   (sfun? (local-value (car funs))))
			      (node-loc (sfun-body (local-value (car funs))))
			      (node-loc node))))
		(instantiate::let-fun
		   (loc loc)
		   (type (node-type node))
		   (locals funs)
		   (body body)))
	     ;; we style have to alpha-convert the body of `var'
	     (let* ((binding (car vbindings))
		    (nvar (car nvars))
		    (sfun (local-value nvar))
		    (body (sfun-body sfun))
		    (val (cdr binding))
		    (aux (car (let-fun-locals val))))
		(sfun-body-set! sfun
				(substitute! (cons aux old-funs)
					     (cons nvar new-funs)
					     body
					     'value))
		;; ok, it is finished, we loop now.
		(loop (cdr vbindings) (cdr nvars)))))))

;*---------------------------------------------------------------------*/
;*    letrec*->node ...                                                */
;*    -------------------------------------------------------------    */
;*    Decompose a letrec* in a let* and a labels, i.e.,                */
;*      (LETREC* ((f1 (lambda (x) ...))                                */
;*    	    (f2 (lambda (y) ...))                                      */
;*    	    (v1 i1)                                                    */
;*    	    (v2 i2)                                                    */
;*    	    (f3 (lambda (y) ...))                                      */
;*    	    (v3 i3)                                                    */
;*    	    ...)                                                       */
;*         body)                                                       */
;*                                                                     */
;*      ==>                                                            */
;*                                                                     */
;*      (let* ((v1 i1)                                                 */
;*    	 (v2 i2)                                                       */
;*    	 (v3 i1))                                                      */
;*         (letrec ((f1 (lambda (x) ...))                              */
;*    	      (f2 (lambda (x) ...))                                    */
;*    	      (f3 (lambda (x) ...)))                                   */
;*    	body))                                                         */
;*---------------------------------------------------------------------*/
(define (letrec*->node sexp stack loc site)

   (define (free-vars sexp v vars)
      ;; compute an over approximation of all the
      ;; free vars appearing in sexp
      (let loop ((sexp sexp)
		 (res '()))
	 (cond
	    ((symbol? sexp)
	     (cond
		((or (eq? sexp v) (not (memq sexp vars))) res)
		((memq sexp res) res)
		(else (cons sexp res))))
	    ((not (pair? sexp))
	     res)
	    ((eq? (car sexp) 'quote)
	     res)
	    (else
	     (loop (car sexp) (loop (cdr sexp) res))))))

   (define (split-function-bindings ebindings)
      ;; extract the head bindings that do not bind functions
      (let loop ((l ebindings)
		 (nonfunctions '()))
	 (cond
	    ((null? l)
	     (values (reverse nonfunctions) '()))
	    ((function? (cadr (car (car l))))
	     (values (reverse nonfunctions) l))
	    (else
	     (loop (cdr l) (cons (car l) nonfunctions))))))

   (define (used? var ebindings)
      (any (lambda (eb) (memq var (caddr eb))) ebindings))

   (define (op? val)
      (memq val '(+ - *
		  +fx -fx *fx
		  +elong -elong *elong
		  +llong -llong *llong
		  +s8 -s8 *s8
		  +u8 -u8 *u8
		  +s16 -s16 *s16
		  +u16 -u16 *u16
		  +s32 -s32 *s32
		  +u32 -u32 *u32
		  +s64 -s64 *s64
		  +u64 -u64 *u64
		  +f32 -f32 *f32
		  +f64 -f64 *f64

		  > >= < <= =
		  >fx >=fx <fx <=fx =fx
		  >elong >=elong <elong <=elong =elong
		  >llong >=llong <llong <=llong =llong
		  >s8 >=s8 <s8 <=s8 =s8
		  >u8 >=u8 <u8 <=u8 =u8
		  >s16 >=s16 <s16 <=s16 =s16
		  >u16 >=u16 <u16 <=u16 =u16
		  >s32 >=s32 <s32 <=s32 =s32
		  >u32 >=u32 <u32 <=u32 =u32
		  >s64 >=s64 <s64 <=s64 =s64
		  >u64 >=u64 <u64 <=u64 =u64
		  >f32 >=f32 <f32 <=f32 =f32
		  >f64 >=f64 <f64 <=f64 =f64

		  eq? equal?
		  
		  bit-lsh bit-rsh bit-ursh bit-not bit-xor
		  bit-lshelong bit-rshelong bit-urshelong bit-notelong bit-xorelong
		  bit-lshs8 bit-rshs8 bit-urshs8 bit-nots8 bit-xors8
		  bit-lshu8 bit-rshu8 bit-urshu8 bit-notu8 bit-xoru8
		  bit-lshs16 bit-rshs16 bit-urshs16 bit-nots16 bit-xors16
		  bit-lshu16 bit-rshu16 bit-urshu16 bit-notu16 bit-xoru16
		  bit-lshs32 bit-rshs32 bit-urshs32 bit-nots32 bit-xors32
		  bit-lshu32 bit-rshu32 bit-urshu32 bit-notu32 bit-xoru32
		  bit-lshs64 bit-rshs64 bit-urshs64 bit-nots64 bit-xors64
		  bit-lshu64 bit-rshu64 bit-urshu64 bit-notu64 bit-xoru64)))
   
   (define (side-effect-ebinding? e)
      (let loop ((val (cadr (car e))))
	 (match-case val
	    ((? function?) #f)
	    ((or (? number?) (? string?)) #f)
	    ((? symbol?) #f)
	    (((kwote quote) . ?-) #f)
	    (((? op?) ?e1 ?e2) (or (loop e1) (loop e2)))
	    (((kwote not) ?e) (loop e))
	    ((if ?test ?then ?else) (or (loop test) (loop then) (loop else)))
	    (else #t))))
   
   (define (split-post-bindings ebindings)
      (with-trace 2 "split-post-bindings"
	 (let ((letrec*-bindings '())
	       (post-bindings '()))
	    (let loop ((es ebindings)
		       (rec*-bindings '())
		       (post-bindings '()))
	       (if (null? es)
		   (values (reverse! rec*-bindings) (reverse! post-bindings))
		   (let* ((e (car es))
			  (var (cadr e))
			  (val (cadr (car e)))
			  (rest (cdr es)))
		      (if (or (function? val)
			      (used? var ebindings)
			      (any side-effect-ebinding? rest))
			  (loop rest (cons e rec*-bindings) post-bindings)
			  (loop rest rec*-bindings (cons e post-bindings)))))))))

   (define (split-pre-bindings ebindings)
      (with-trace 2 "split-pre-bindings"
	 (let loop ((ebindings ebindings)
		    (pre-bindings '()))
	    (cond
	       ((null? ebindings)
		(values (reverse! pre-bindings) '()))
	       ((and (function? (cadr (caar ebindings)))
		     (any (lambda (var)
			     (any (lambda (eb)
				     (eq? (cadr eb) var))
				(cdr ebindings)))
			(caddr (car ebindings))))
		(trace-item "optim fun: " (shape (caar ebindings)))
		(values (reverse! pre-bindings) ebindings))
	       ((any (lambda (eb)
			(memq (cadr eb) (caddr (car ebindings))))
		   (cdr ebindings))
		;; one of the variables introduced in the next bindings
		;; appears free in the value of the current binding
		(loop (cdr ebindings) (cons (car ebindings) pre-bindings)))
	       (else
		(trace-item "optim free: " (shape (caar ebindings)))
		(values (reverse! (cons (car ebindings) pre-bindings))
		   (cdr ebindings)))))))

   (define (stage2 ebindings body stack loc site)
      ;; stage 2, try to put as many variables as possible on head positions
      (with-trace 2 "letrec*, stage2"
	 (trace-item "ebindings="
	    (map (lambda (b) (shape (caar b))) ebindings))
	 (cond
	    ((null? ebindings)
	     (sexp->node body stack loc site))
	    ((null? (cdr ebindings))
	     (let->node
		(epairify-propagate-loc
		   `(letrec* ,(map car ebindings) ,body)
		   loc)
		stack loc site))
	    (else
	     (multiple-value-bind (pre-bindings letrec*-bindings)
		(split-pre-bindings ebindings)
		(trace-item "pre-bindings="
		   (map (lambda (x) (shape (caar x))) pre-bindings))
		(trace-item "rec*="
		   (map (lambda (x) (shape (caar x))) letrec*-bindings))
		(cond
		   ((null? pre-bindings)
		    (let->node
		       (epairify-propagate-loc
			  `(letrec* ,(map car letrec*-bindings) ,body)
			  loc)
		       stack loc site))
		   ((null? letrec*-bindings)
		    (let->node
		       (epairify-propagate-loc
			  `(letrec* ,(map car pre-bindings) ,body)
			  loc)
		       stack loc site))
		   (else
		    (letrec*->node
		       (epairify-propagate-loc
			  `(letrec* ,(map car pre-bindings)
			      (letrec* ,(map car letrec*-bindings)
				 ,body))
			  loc)
		       stack loc site))))))))

   (define (stage1 ebindings body)
      ;; stage 1, try to put as many variables as possible on tail positions
      (with-trace 2 "letrec*, stage1"
	 (trace-item "ebindings=" (map (lambda (b) (shape (caar b))) ebindings))
	 (cond
	    ((null? ebindings)
	     (sexp->node body stack loc site))
	    (else
	     (multiple-value-bind (letrec*-bindings post-bindings)
		(split-post-bindings ebindings)
		(trace-item "rec*="
		   (map (lambda (x) (shape (caar x))) letrec*-bindings))
		(trace-item "post="
		   (map (lambda (x) (shape (caar x))) post-bindings))
		(if (null? post-bindings)
		    ;; stage 2, try to put on head positions,
		    ;; as many variables as possible
		    (stage2 letrec*-bindings body stack loc site)
		    (stage1 letrec*-bindings
		       (epairify-propagate-loc
			  (letrecstar (map car post-bindings) body)
			  loc))))))))
      
   (define (decompose-letrec* bindings body)
      ;; for each bindings, extra the variable name and the set
      ;; of scope free variables used in the expression
      (let* ((vars (map (lambda (b) (fast-id-of-id (car b) loc))
		      bindings))
	     (ebindings (map (lambda (b v)
				(list b v (free-vars (cadr b) v vars)))
			   bindings vars)))
	 (stage1 ebindings (epairify-propagate-loc `(begin ,@body) loc))))

   (match-case sexp
      ((letrec* () . ?body)
       ;; not a real letrec*
       (sexp->node (epairify-propagate-loc `(begin ,@body) loc) stack loc site))
      ((letrec* (and (? list?) ?bindings) . ?body)
       (decompose-letrec* bindings body))
      (else
       (error-sexp->node (string-append "Illegal 'letrec*' form")
	  exp (find-location/loc exp loc)))))

;*---------------------------------------------------------------------*/
;*    letstar ...                                                      */
;*---------------------------------------------------------------------*/
(define (letstar bindings body)
   (if (null? bindings)
       body
       `(let (,(car bindings))
	   ,(letstar (cdr bindings) body))))

;*---------------------------------------------------------------------*/
;*    letrecstar ...                                                   */
;*---------------------------------------------------------------------*/
(define (letrecstar bindings body)
   (cond
      ((null? bindings)
       body)
      ((function? (cadr (car bindings)))
       `(letrec (,(car bindings))
	   ,(letrecstar (cdr bindings) body)))
      (else
       `(let (,(car bindings))
	   ,(letrecstar (cdr bindings) body)))))

;*---------------------------------------------------------------------*/
;*    function? ...                                                    */
;*    -------------------------------------------------------------    */
;*    Does exp contain a lambda definition (possibly nested)           */
;*---------------------------------------------------------------------*/
(define (function? exp)

   (define (any* pred lst)
      (when (pair? lst)
	 (or (pred (car lst)) (any* pred (cdr lst)))))

   (match-case exp
      ((quote . ?-)
       #f)
      ((labels ((?id . ?-)) ?id)
       #t)
      ((?var . (and ?args ??-))
       (or (eq? var 'lambda)
	   (and (symbol? var) (eq? (fast-id-of-id var #f) 'lambda))
	   (function? var)
	   (any* function? args)))
      (else
       #f)))
