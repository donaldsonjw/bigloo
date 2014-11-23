;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/cinfo2.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb 25 13:50:29 1999                          */
;*    Last change :  Mon Nov 14 17:46:32 2011 (serrano)                */
;*    Copyright   :  1999-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The format cfa_info used is to big. Its compilation was          */
;*    requiring to many memory. I have simply slit that module.        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_info2

   (import type_type
	   type_cache
	   ast_var
	   ast_node
	   cfa_info)

   (include "Cfa/cinfo2.sch")
   
   (export ;; arithmetic
           (wide-class pre-arithmetic-app::app
	      (spec-types::pair read-only))
	   (wide-class arithmetic-app::app
	      (approx::approx read-only)
	      (spec-types::pair read-only))
	   
           ;; procedure
	   (wide-class pre-make-procedure-app::app
	      ;; the allocation owner
	      (owner::variable read-only))
	   (wide-class pre-procedure-ref-app::app)
	   (wide-class pre-procedure-set!-app::app)
	   
	   (wide-class make-procedure-app::app
	      ;; the approx of the make-procedure (i.e. *procedure*)
	      approx::approx
	      ;; the approximations of the values holded by the procedure
	      values-approx::vector
	      ;; a stamp to avoid infinit loops when loosing a procedure.
	      ;; This slot also reveals if the procedure has been lost.
	      ;; If the procedure has, lost-stamp > 0.
	      (lost-stamp::long (default -1))
	      ;; the X and T closure property (only used by cfa_closure)
	      (X-T?::bool (default #t))
	      (X::bool (default #f))
	      (T::bool (default #f))
	      ;; an allocation owner
	      (owner::variable read-only)
	      ;; a stamp use for the stack loosing propagation
	      (stack-stamp (default '())))
	   (wide-class procedure-ref-app::app
	      (approx::approx read-only))
	   (wide-class procedure-set!-app::app
	      (approx::approx read-only)
	      vapprox)

	   ;; vector
	   (wide-class pre-make-vector-app::app
	      ;; the allocation owner
	      (owner::variable read-only))
	   
	   (wide-class make-vector-app::app
	      ;; the approx of the make-vector (i.e. *vector*)
	      approx::approx
	      ;; the approximation of the values holded by the vector
	      (value-approx::approx read-only)
	      ;; a stamp to avoid infinit loop when loosing a vector
	      (lost-stamp::long (default -1))
	      ;; an allocation owner
	      (owner::variable read-only)
	      ;; a stamp use for the stack loosing propagation
	      (stack-stamp (default '()))
	      ;; Is the vector subject to a vector-ref or a vector-set?
	      ;; If not, this vector cannot be optimized. This is mandatory
	      ;; otherwise this analysis fails for code like:
	      ;; (let ((v #unspecified))
	      ;;     (set! v (make-vector 10 0.0))
	      ;;     (set! v #f)
	      ;;     ...)
	      ;; A type error is detected because v is given
	      ;; an erroneous type.
	      (seen?::bool (default #f)))
	       
	   ;; pair
	   (wide-class pre-cons-app::app
	      ;; the allocation owner
	      (owner::variable read-only))
	   (wide-class pre-cons-ref-app::app
	      (get::procedure read-only))
	   (wide-class pre-cons-set!-app::app
	      (get::procedure read-only))
	   
	   (wide-class cons-app::app
	      ;; the approx of the cons (i.e. *pair*)
	      approx::approx
	      ;; the approximation of the values holded by the pair
	      (approxes::pair read-only)
	      ;; a stamp to avoid infinit loop when loosing a pair
	      (lost-stamp::long (default -1))
	      ;; an allocation owner
	      (owner::variable read-only)
	      ;; a stamp use for the stack loosing propagation
	      (stack-stamp (default '()))
	      ;; Is the cons subject to a cons-ref or a cons-set!
	      ;; If not, this cons cannot be optimized. This is mandatory
	      ;; otherwise this analysis fails for code like:
	      ;; (let ((v #unspecified))
	      ;;     (set! v (cons 1 2))
	      ;;     (set! v #f)
	      ;;     ...)
	      ;; A type error is detected because v is given
	      ;; an erroneous type.
	      (seen?::bool (default #f)))
	   (wide-class cons-ref-app::app
	      (approx::approx read-only)
	      (get::procedure read-only))
	   (wide-class cons-set!-app::app
	      (approx::approx read-only)
	      (get::procedure read-only))
	       
	   ;; struct
	   (wide-class pre-make-struct-app::app
	      ;; the allocation owner
	      (owner::variable read-only))
	   (wide-class pre-struct-ref-app::app)
	   (wide-class pre-struct-set!-app::app)
	   
	   (wide-class make-struct-app::app
	      ;; the approx of the make-struct (i.e. *struct*)
	      approx::approx
	      ;; the approximation of the values holded by the struct
	      (value-approx::approx read-only)
	      ;; a stamp to avoid infinit loop when loosing a struct
	      (lost-stamp::long (default -1))
	      ;; an allocation owner
	      (owner::variable read-only)
	      ;; a stamp use for the stack loosing propagation
	      (stack-stamp (default '())))
      
	   (wide-class struct-ref-app::app (approx::approx read-only))
	   (wide-class struct-set!-app::app (approx::approx read-only))))
   
