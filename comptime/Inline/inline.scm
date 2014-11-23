;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Inline/inline.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 10 09:04:27 1995                          */
;*    Last change :  Mon Nov 11 10:00:30 2013 (serrano)                */
;*    Copyright   :  1995-2013 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The ast inlining.                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module inline_inline
   (include "Ast/node.sch"
	    "Tools/trace.sch"
	    "Inline/inline.sch")
   (import  inline_walk
	    inline_app
	    ast_alphatize
	    engine_param
	    tools_shape
	    tools_error)
   (export  (inline-sfun! ::variable ::long ::obj)
	    (wide-class isfun::sfun
	       (original-body::node read-only)
	       (recursive-calls (default #unspecified)))
	    (generic inline-node::node ::node ::long ::obj)
	    (inline-node*! node* kfactor stack)))

;*---------------------------------------------------------------------*/
;*    inline-sfun! ...                                                 */
;*---------------------------------------------------------------------*/
(define (inline-sfun! variable kfactor stack)
   (trace inline
	  "--- SCANNING: " (shape variable) " ---- kactor: " kfactor #\Newline)
   (let* ((sfun     (variable-value variable))
	  (isfun    (if (isfun? sfun?)
			sfun
			(widen!::isfun sfun (original-body (sfun-body sfun)))))
	  (o-body   (isfun-original-body isfun))
	  (inl-body (if (inline-app? variable *kfactor*
			   (+fx 1 (length (sfun-args sfun))) '())
			;; if at least one call to `variable' can be
			;; inlined, we duplicate its body.
			(begin
			   (trace inline "DUPLICATING " (shape variable)
				  "'s body" #\Newline)
			   (alphatize '() '() #f o-body))
			o-body)))
      (sfun-body-set! sfun
	 (inline-node inl-body kfactor (cons variable stack)))
      (trace inline
	     "--- END SCANNING: " (shape variable) " ----" #\Newline)))

;*---------------------------------------------------------------------*/
;*    inline-node ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (inline-node::node node::node kfactor::long stack))

;*---------------------------------------------------------------------*/
;*    inline-node ::atom ...                                           */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::atom kfactor stack)
   node)

;*---------------------------------------------------------------------*/
;*    inline-node ::var ...                                            */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::var kfactor stack)
   (with-access::var node (variable loc)
      (if (and (>fx *optim* 0)
	       (global? variable)
	       (eq? (global-import variable) 'static)
	       (not (global-eval? variable))
	       (or (eq? (global-access variable) 'read)
		   (and (eq? (global-init variable) #t)
			(= (global-occurrencew variable) 1)))
	       (or (atom? (global-src variable))
		   (kwote? (global-src variable))))
	  (begin
	     (trace inline "*** inlining global variable: " (shape variable)
		    #\Newline)
	     (alphatize '() '() loc (global-src variable)))
	  node)))

;*---------------------------------------------------------------------*/
;*    inline-node ::kwote ...                                          */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::kwote kfactor stack)
   node)
       
;*---------------------------------------------------------------------*/
;*    inline-node ::sequence ...                                       */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::sequence kfactor stack)
   (inline-node*! (sequence-nodes node) kfactor stack)
   node)

;*---------------------------------------------------------------------*/
;*    inline-node ::sync ...                                           */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::sync kfactor stack)
   (sync-mutex-set! node (inline-node (sync-mutex node) kfactor stack))
   (sync-prelock-set! node (inline-node (sync-prelock node) kfactor stack))
   (sync-body-set! node (inline-node (sync-body node) kfactor stack))
   node)

;*---------------------------------------------------------------------*/
;*    inline-node ::app ...                                            */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::app kfactor stack)
   (inline-node*! (app-args node) kfactor stack)
   (inline-app node kfactor stack))
 
;*---------------------------------------------------------------------*/
;*    inline-node ::app-ly ...                                         */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::app-ly kfactor stack)
   (with-access::app-ly node (fun arg)
      (set! fun (inline-node fun kfactor stack))
      (set! arg (inline-node arg kfactor stack))
      node))

;*---------------------------------------------------------------------*/
;*    inline-node ::funcall ...                                        */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::funcall kfactor stack)
   ;; for statistics we mark that this call is not inlined
   (set! *non-inlined-calls* (+fx *non-inlined-calls* 1))
   (funcall-fun-set! node (inline-node (funcall-fun node) kfactor stack))
   (inline-node*! (funcall-args node) kfactor stack)
   node)

;*---------------------------------------------------------------------*/
;*    inline-node ::extern ...                                         */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::extern kfactor stack)
   (inline-node*! (extern-expr* node) kfactor stack)
   node)

;*---------------------------------------------------------------------*/
;*    inline-node ::cast ...                                           */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::cast kfactor stack)
   (inline-node (cast-arg node) kfactor stack)
   node)

;*---------------------------------------------------------------------*/
;*    inline-node ::setq ...                                           */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::setq kfactor stack)
   (setq-value-set! node (inline-node (setq-value node) kfactor stack))
   node)

;*---------------------------------------------------------------------*/
;*    inline-node ::conditional ...                                    */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::conditional kfactor stack)
   (with-access::conditional node (test true false)
       (set! test (inline-node test kfactor stack))
       (set! true (inline-node true kfactor stack))
       (set! false (inline-node false kfactor stack))
       node))

;*---------------------------------------------------------------------*/
;*    inline-node ::fail ...                                           */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::fail kfactor stack)
   (with-access::fail node (proc msg obj)
      (set! proc (inline-node proc kfactor stack))
      (set! msg (inline-node msg kfactor stack))
      (set! obj (inline-node obj kfactor stack))
      node))

;*---------------------------------------------------------------------*/
;*    inline-node ::select ...                                         */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::select kfactor stack)
   (select-test-set! node (inline-node (select-test node) kfactor stack))
   (for-each (lambda (clause)
		(set-cdr! clause (inline-node (cdr clause) kfactor stack)))
	     (select-clauses node))
   node)

;*---------------------------------------------------------------------*/
;*    inline-node ::let-fun ...                                        */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::let-fun kfactor stack)
   ;; MS mar 2011, former versions were inlining the body of the let-fun
   ;; before the defined functions. I think it should give better
   ;; results the other way around.
   (for-each (lambda (local)
		(inline-sfun! local kfactor stack))
	     (let-fun-locals node))
   (let-fun-body-set! node (inline-node (let-fun-body node) kfactor stack))
   node)

;*---------------------------------------------------------------------*/
;*    inline-node ::let-var ...                                        */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::let-var kfactor stack)
   (for-each (lambda (binding)
		(set-cdr! binding (inline-node (cdr binding) kfactor stack)))
	     (let-var-bindings node))
   (let-var-body-set! node (inline-node (let-var-body node) kfactor stack))
   node)

;*---------------------------------------------------------------------*/
;*    inline-node ::set-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::set-ex-it kfactor stack)
   (set-ex-it-body-set! node (inline-node (set-ex-it-body node) kfactor stack))
   node)

;*---------------------------------------------------------------------*/
;*    inline-node ::jump-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::jump-ex-it kfactor stack)
   (with-access::jump-ex-it node (exit value)
      (set! exit (inline-node exit kfactor stack)) 
      (set! value (inline-node value kfactor stack))
      node))

;*---------------------------------------------------------------------*/
;*    inline-node ::make-box ...                                       */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::make-box kfactor stack)
   (make-box-value-set! node (inline-node (make-box-value node) kfactor stack))
   node)

;*---------------------------------------------------------------------*/
;*    inline-node ::box-ref ...                                        */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::box-ref kfactor stack)
   (box-ref-var-set! node (inline-node (box-ref-var node) kfactor stack))
   node)

;*---------------------------------------------------------------------*/
;*    inline-node ::box-set! ...                                       */
;*---------------------------------------------------------------------*/
(define-method (inline-node node::box-set! kfactor stack)
   (with-access::box-set! node (var value)
      (set! var (inline-node var kfactor stack))
      (set! value (inline-node value kfactor stack))
      node))

;*---------------------------------------------------------------------*/
;*    inline-node*! ...                                                */
;*---------------------------------------------------------------------*/
(define (inline-node*! node* kfactor stack)
   (if (null? node*)
       'done
       (begin
	  (set-car! node* (inline-node (car node*) kfactor stack))
	  (inline-node*! (cdr node*) kfactor stack))))
   
   

