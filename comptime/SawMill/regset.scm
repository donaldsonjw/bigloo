;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/SawMill/regset.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 16 15:37:03 2005                          */
;*    Last change :  Mon Nov 14 19:13:17 2011 (serrano)                */
;*    Copyright   :  2005-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Register sets for regiter allocation                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_regset
   
   (import  engine_param
	    type_type
	    ast_var
	    ast_node
	    backend_backend
	    saw_lib
	    saw_node2rtl
	    saw_defs
	    saw_register-allocation)

   (include "SawMill/regset.sch")
   
   (export  (class regset
	       (length::int (default 0))
	       (msize::int read-only)
	       (regv::vector read-only)
	       (regl::pair-nil read-only)
	       (string::bstring (default '#())))
	    (make-empty-regset::regset ::pair-nil)
	    (list->regset::regset ::pair-nil ::pair-nil)
	    (regset->list::pair-nil ::regset)
	    (duplicate-regset::regset ::regset)
	    (regset-member?::bool ::rtl_reg/ra ::regset)
	    (regset-empty? ::regset)
	    (regset-add!::bool ::regset ::rtl_reg/ra)
	    (regset-add*!::bool ::regset ::pair-nil)
	    (regset-remove!::bool ::regset ::rtl_reg/ra)
	    (regset-union!::bool ::regset ::regset)
	    (regset-union*!::bool ::regset ::pair-nil)
	    (regset-for-each ::procedure ::regset)
	    (regset-filter::pair-nil ::procedure ::regset)
	    (regset-dump s::regset . p)))

;*---------------------------------------------------------------------*/
;*    *register-cache* ...                                             */
;*---------------------------------------------------------------------*/
(define *register-cache* #f)
(define *register-cache-cached* #f)

;*---------------------------------------------------------------------*/
;*    regset-ref ...                                                   */
;*---------------------------------------------------------------------*/
(define (regset-ref s i)
   (char->integer (string-ref (regset-string s) i)))

;*---------------------------------------------------------------------*/
;*    regset-set! ...                                                  */
;*---------------------------------------------------------------------*/
(define (regset-set! s i v)
   (string-set! (regset-string s) i (integer->char v)))

;*---------------------------------------------------------------------*/
;*    make-empty-regset ...                                            */
;*---------------------------------------------------------------------*/
(define (make-empty-regset::regset registers::pair-nil)
   (let* ((l (length registers))
	  (vec (if (eq? *register-cache* registers)
		   *register-cache-cached*
		   (let ((v (make-vector l)))
		      (for-each (lambda (r)
				   (vector-set! v (rtl_reg/ra-num r) r))
				registers)
		      v))))
      ;; update the cache
      (set! *register-cache* registers)
      (set! *register-cache-cached* vec)
      ;; build the regset
      (instantiate::regset
	 (length 0)
	 (msize l)
	 (string (make-string (+fx 1 (/fx l 8)) #a000))
	 (regv vec)
	 (regl registers))))

;*---------------------------------------------------------------------*/
;*    list->regset ...                                                 */
;*---------------------------------------------------------------------*/
(define (list->regset::regset lst::pair-nil registers::pair-nil)
   (let ((s (make-empty-regset registers)))
      (for-each (lambda (e) (regset-add! s e)) lst)
      s))

;*---------------------------------------------------------------------*/
;*    regset->list ...                                                 */
;*---------------------------------------------------------------------*/
(define (regset->list::pair-nil s::regset)
   (let ((r '()))
      (regset-for-each (lambda (e) (set! r (cons e r))) s)
      (reverse! r) ))

;*---------------------------------------------------------------------*/
;*    duplicate-regset ...                                             */
;*---------------------------------------------------------------------*/
(define (duplicate-regset::regset s0::regset)
   (let ((s (make-empty-regset (regset-regl s0))))
      (regset-union! s s0)
      s))

;*---------------------------------------------------------------------*/
;*    regset-member? ...                                               */
;*---------------------------------------------------------------------*/
(define (regset-member?::bool reg::rtl_reg/ra s::regset)
   (with-access::rtl_reg/ra reg (num)
      (let ((base (/fx num 8))
	    (bit (remainderfx num 8)))
	 (when (<fx base (string-length (regset-string s)))
	    (>fx (bit-and (regset-ref s base) (bit-lsh 1 bit)) 0)))))

;*---------------------------------------------------------------------*/
;*    regset-empty? ...                                                */
;*---------------------------------------------------------------------*/
(define (regset-empty? s::regset)
   (=fx (regset-length s) 0))

;*---------------------------------------------------------------------*/
;*    regset-add! ...                                                  */
;*---------------------------------------------------------------------*/
(define (regset-add!::bool s::regset reg::rtl_reg/ra)
   (and (not (regset-member? reg s))
	(with-access::rtl_reg/ra reg (num)
	   (let ((base (/fx num 8))
		 (bit (remainderfx num 8)))
	      (with-access::regset s (length)
		 (set! length (+fx length 1))
		 (let ((nval (bit-or (regset-ref s base) (bit-lsh 1 bit))))
		    (regset-set! s base nval)
		    #t))))))
   
;*---------------------------------------------------------------------*/
;*    regset-add*! ...                                                 */
;*---------------------------------------------------------------------*/
(define (regset-add*!::bool s::regset regs::pair-nil)
   (for-each (lambda (r) (regset-add! s r)) regs))
   
;*---------------------------------------------------------------------*/
;*    regset-remove! ...                                               */
;*---------------------------------------------------------------------*/
(define (regset-remove!::bool s::regset reg::rtl_reg/ra)
   (and (regset-member? reg s)
	(with-access::rtl_reg/ra reg (num)
	   (let ((base (/fx num 8))
		 (bit (remainderfx num 8)))
	      (with-access::regset s (length)
		 (set! length (-fx length 1)))
	      (let ((nval (bit-and (regset-ref s base)
				   (bit-not (bit-lsh 1 bit)))))
		 (regset-set! s base nval)
		 #t)))))
   
;*---------------------------------------------------------------------*/
;*    regset-union! ...                                                */
;*---------------------------------------------------------------------*/
(define (regset-union!::bool s1::regset s2::regset)
   (let ((st1 (regset-string s1))
	 (st2 (regset-string s2)))
      (let loop ((i (-fx (string-length st1) 1))
		 (res #f))
	 (cond
	    ((=fx i -1)
	     res)
	    ((char=? (string-ref st1 i) (string-ref st2 i))
	     (loop (-fx i 1) res))
	    (else
	     (let ((n2 (regset-ref s2 i)))
		(let liip ((j 1)
			   (res res)
			   (n1 (regset-ref s1 i)))
		   (cond
		      ((=fx j 256)
		       (loop (-fx i 1) res))
		      ((=fx (bit-and n1 j) (bit-and n2 j))
		       (liip (bit-lsh j 1) res n1))
		      ((=fx (bit-and n1 j) 0)
		       (with-access::regset s1 (length)
			  (set! length (+fx length 1))
			  (let ((n1 (bit-or n1 j)))
			     (regset-set! s1 i n1)
			     (liip (bit-lsh j 1) #t n1))))
		      (else
		       (liip (bit-lsh j 1) res n1))))))))))

;*---------------------------------------------------------------------*/
;*    regset-union*! ...                                               */
;*---------------------------------------------------------------------*/
(define (regset-union*!::bool s::regset ss::pair-nil)
   (cond
      ((null? ss)
       #f)
      ((null? (cdr ss))
       (regset-union! s (car ss)))
      (else
       (let loop ((ss ss)
		  (res #f))
	  (if (null? ss)
	      res
	      (loop (cdr ss) (or (regset-union! s (car ss)) res)))))))

;*---------------------------------------------------------------------*/
;*    regset-for-each ...                                              */
;*---------------------------------------------------------------------*/
(define (regset-for-each proc::procedure s::regset)
   (let loop ((i 0))
      (when (<fx i (regset-msize s))
	 (let ((e (vector-ref (regset-regv s) i)))
	    (when (regset-member? e s) (proc e))
	    (loop (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    regset-filter ...                                                */
;*---------------------------------------------------------------------*/
(define (regset-filter proc::procedure s::regset)
   (let loop ((i 0))
      (if (<fx i (regset-msize s))
	  (let ((e (vector-ref (regset-regv s) i)))
	     (if (and (regset-member? e s) (proc e))
		 (cons e (loop (+fx i 1)))
		 (loop (+fx i 1))))
	  '())))

;*---------------------------------------------------------------------*/
;*    regset-first ...                                                 */
;*---------------------------------------------------------------------*/
(define (regset-first proc::procedure s::regset)
   (let loop ((i 0))
      (if (<fx i (regset-msize s))
	  (let ((e (vector-ref (regset-regv s) i)))
	     (if (and (regset-member? e s) (proc e))
		 e
		 (loop (+fx i 1))))
	  #f)))

;*---------------------------------------------------------------------*/
;*    regset-dump ...                                                  */
;*---------------------------------------------------------------------*/
(define (regset-dump s::regset . p)
   (let ((p (if (pair? p) (car p) (current-output-port))))
      (display "{" p)
      (regset-for-each (lambda (e)
			  (display " " p)
			  (dump e p 0))
		       s)
      (display "}" p)))
		       

   
