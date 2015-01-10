;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/unicode.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar 20 19:17:18 1995                          */
;*    Last change :  Tue Dec 23 06:33:09 2014 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Unicode (UCS-2) strings handling.                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __unicode
   
   (import  __error
	    __param
	    __bexit
	    __object
	    __thread)
   
   (use     __type
	    __bigloo
	    __tvector
	    __ucs2
	    __bignum
	    __bit
	    
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    __r4_strings_6_7
	    
	    __evenv)
   
   (extern (macro c-ucs2-string?::bool (::obj)
		  "UCS2_STRINGP")
	   (c-make-ucs2-string::ucs2string (::int ::ucs2)
					   "make_ucs2_string")
	   (macro c-ucs2-string-length::int (::ucs2string)
		  "UCS2_STRING_LENGTH")
	   (macro c-ucs2-string-ref::ucs2 (::ucs2string ::int)
		  "UCS2_STRING_REF")
	   (macro c-ucs2-string-set!::obj (::ucs2string ::int ::ucs2)
		  "UCS2_STRING_SET")
	   
	   (c-ucs2-string=?::bool (::ucs2string ::ucs2string)
				  "ucs2_strcmp")
	   (ucs2-strcicmp::bool (::ucs2string ::ucs2string)
				"ucs2_strcicmp")
	   (ucs2-string_lt::bool (::ucs2string ::ucs2string)
				 "ucs2_string_lt")
	   (ucs2-string_le::bool (::ucs2string ::ucs2string)
				 "ucs2_string_le")
	   (ucs2-string_gt::bool (::ucs2string ::ucs2string)
				 "ucs2_string_gt")
	   (ucs2-string_ge::bool (::ucs2string ::ucs2string)
				 "ucs2_string_ge")
	   (ucs2-string_cilt::bool (::ucs2string ::ucs2string)
				   "ucs2_string_cilt")
	   (ucs2-string_cile::bool (::ucs2string ::ucs2string)
				   "ucs2_string_cile")
	   (ucs2-string_cigt::bool (::ucs2string ::ucs2string)
				   "ucs2_string_cigt")
	   (ucs2-string_cige::bool (::ucs2string ::ucs2string)
				   "ucs2_string_cige")
	   
	   (c-ucs2-string-copy::ucs2string (::ucs2string)
					   "c_ucs2_string_copy")
	   (c-subucs2-string::ucs2string (::ucs2string ::int ::int)
					 "c_subucs2_string")
	   (c-ucs2-string-append::ucs2string (::ucs2string ::ucs2string)
					     "ucs2_string_append")
	   
	   (c-ucs2-string->utf8-string::bstring (::ucs2string)
						"ucs2_string_to_utf8_string")
	   (c-utf8-string->ucs2-string::ucs2string (::bstring)
						   "utf8_string_to_ucs2_string"))
   
   (java   (class foreign
	      (method static c-ucs2-string?::bool (::obj)
		      "UCS2_STRINGP")
	      (method static c-make-ucs2-string::ucs2string (::int ::ucs2)
		      "make_ucs2_string")
	      (method static c-ucs2-string-length::int (::ucs2string)
		      "UCS2_STRING_LENGTH")
	      (method static c-ucs2-string-ref::ucs2 (::ucs2string ::int)
		      "UCS2_STRING_REF")
	      (method static c-ucs2-string-set!::obj (::ucs2string ::int ::ucs2)
		      "UCS2_STRING_SET")
	      
	      (method static c-ucs2-string=?::bool (::ucs2string ::ucs2string)
		      "ucs2_strcmp")
	      (method static ucs2-strcicmp::bool (::ucs2string ::ucs2string)
		      "ucs2_strcicmp")
	      (method static ucs2-string_lt::bool (::ucs2string ::ucs2string)
		      "ucs2_string_lt")
	      (method static ucs2-string_le::bool (::ucs2string ::ucs2string)
		      "ucs2_string_le")
	      (method static ucs2-string_gt::bool (::ucs2string ::ucs2string)
		      "ucs2_string_gt")
	      (method static ucs2-string_ge::bool (::ucs2string ::ucs2string)
		      "ucs2_string_ge")
	      (method static ucs2-string_cilt::bool (::ucs2string ::ucs2string)
		      "ucs2_string_cilt")
	      (method static ucs2-string_cile::bool (::ucs2string ::ucs2string)
		      "ucs2_string_cile")
	      (method static ucs2-string_cigt::bool (::ucs2string ::ucs2string)
		      "ucs2_string_cigt")
	      (method static ucs2-string_cige::bool (::ucs2string ::ucs2string)
		      "ucs2_string_cige")
	      
	      (method static c-ucs2-string-copy::ucs2string (::ucs2string)
		      "c_ucs2_string_copy")
	      (method static c-subucs2-string::ucs2string (::ucs2string ::int ::int)
		      "c_subucs2_string")
	      (method static c-ucs2-string-append::ucs2string (::ucs2string ::ucs2string)
		      "c_ucs2_string_append")
	      
	      (method static c-ucs2-string->utf8-string::bstring (::ucs2string)
		      "ucs2_string_to_utf8_string")
	      (method static c-utf8-string->ucs2-string::ucs2string (::bstring)
		      "utf8_string_to_ucs2_string")))
   
   (export  (inline ucs2-string?::bool ::obj)
	    (inline make-ucs2-string::ucs2string ::int
		    #!optional (filler::ucs2 (char->ucs2 #\space)))
	    (inline ucs2-string::ucs2string . ucs2s)
	    (inline ucs2-string-length::int ::ucs2string)
	    (inline ucs2-string-ref::ucs2 ::ucs2string ::int)
	    (inline ucs2-string-set!::obj ::ucs2string ::int ::ucs2)
	    (inline ucs2-string-ref-ur::ucs2 ::ucs2string ::int)
	    (inline ucs2-string-set-ur!::obj ::ucs2string ::int ::ucs2)
	    (inline ucs2-string=?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string-ci=?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string<?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string>?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string<=?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string>=?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string-ci<?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string-ci>?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string-ci<=?::bool ::ucs2string ::ucs2string)
	    (inline ucs2-string-ci>=?::bool ::ucs2string ::ucs2string)
	    (inline subucs2-string::ucs2string ::ucs2string ::int ::int)
	    (inline subucs2-string-ur::ucs2string ::ucs2string ::int ::int)
	    (ucs2-string-append::ucs2string . ucs2-strings)
	    (ucs2-string->list ::ucs2string)
	    (list->ucs2-string::ucs2string ::obj)
	    (inline ucs2-string-copy::ucs2string ::ucs2string)
	    (ucs2-string-fill!::ucs2string ::ucs2string ::ucs2)
	    (ucs2-string-upcase::ucs2string ::ucs2string)
	    (ucs2-string-downcase::ucs2string ::ucs2string)
	    (ucs2-string-upcase!::ucs2string ::ucs2string)
	    (ucs2-string-downcase!::ucs2string ::ucs2string)
	    (inline ucs2-string->utf8-string::bstring ::ucs2string)
	    (inline utf8-string->ucs2-string::ucs2string ::bstring)
	    (inverse-utf8-table ::vector)
	    (utf8-char-size::long c::char)
	    (ascii-string?::bool ::bstring)
	    (utf8-string?::bool ::bstring #!optional strict::bool)
	    (inline utf8-string-left-replacement? ::bstring ::long)
	    (inline utf8-string-right-replacement? ::bstring ::long)
	    (utf8-string-encode::bstring str::bstring #!optional strict::bool (start::long 0) (end::long (string-length str)))
	    (utf8-string-length::long ::bstring)
	    (utf8-string-ref::bstring ::bstring ::long)
	    (utf8-string-index->string-index::long ::bstring ::long)
	    (utf8-string-append::bstring ::bstring ::bstring)
	    (utf8-string-append*::bstring . strings)
	    (utf8-substring::bstring str::bstring ::long #!optional (end::long (utf8-string-length str)))
	    (utf8->8bits::bstring ::bstring ::obj)
	    (utf8->8bits!::bstring ::bstring ::obj)
	    (utf8->iso-latin::bstring ::bstring)
	    (utf8->iso-latin!::bstring ::bstring)
	    (utf8->iso-latin-15::bstring ::bstring)
	    (utf8->iso-latin-15!::bstring ::bstring)
	    (utf8->cp1252::bstring ::bstring)
	    (utf8->cp1252!::bstring ::bstring)
	    (8bits->utf8::bstring ::bstring ::obj)
	    (8bits->utf8!::bstring ::bstring ::obj)
	    (iso-latin->utf8::bstring ::bstring)
	    (iso-latin->utf8!::bstring ::bstring)
	    (cp1252->utf8::bstring ::bstring)
	    (cp1252->utf8!::bstring ::bstring))
   
   (pragma  (c-ucs2-string? (predicate-of ucs2string) no-cfa-top nesting)
	    (ucs2-string? side-effect-free no-cfa-top nesting)
	    (c-ucs2-string-ref side-effect-free no-cfa-top nesting)
	    (ucs2-string-ref-ur side-effect-free no-cfa-top nesting)
	    (ucs2-string-ref side-effect-free no-cfa-top nesting)
	    (c-ucs2-string-length side-effect-free no-cfa-top nesting)
	    (ucs2-string-length side-effect-free no-cfa-top nesting)
	    (ucs2-string=? side-effect-free nesting)
	    (ucs2-string-ci=? side-effect-free nesting)
	    (ucs2-string<? side-effect-free nesting)
	    (ucs2-string>? side-effect-free nesting)
	    (ucs2-string<=? side-effect-free nesting)
	    (ucs2-string>=? side-effect-free nesting)
	    (ucs2-string-ci<? side-effect-free nesting)
	    (ucs2-string-ci>? side-effect-free nesting)
	    (ucs2-string-ci<=? side-effect-free nesting)
	    (ucs2-string-ci>=? side-effect-free nesting)))
  
;*---------------------------------------------------------------------*/
;*    ucs2-string? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string? obj)
   (c-ucs2-string? obj))

;*---------------------------------------------------------------------*/
;*    make-ucs2-string ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (make-ucs2-string k #!optional
				 (filler::ucs2 (char->ucs2 #\space)))
   (c-make-ucs2-string k filler))
 
;*---------------------------------------------------------------------*/
;*    ucs2-string ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string . ucs2s)
   (list->ucs2-string ucs2s))

;*---------------------------------------------------------------------*/
;*    ucs2-string-length ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-length ucs2-string)
   (c-ucs2-string-length ucs2-string))

;*---------------------------------------------------------------------*/
;*    ucs2-string-ref ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-ref ucs2-string k)
   (if ($string-bound-check? k (ucs2-string-length ucs2-string))
       (c-ucs2-string-ref ucs2-string k)
       (error 'ucs2-string-ref
	      (string-append "index out of range [0.."
			     (integer->string
			      (-fx (ucs2-string-length ucs2-string) 1))
			     "]")
	      k)))
 
;*---------------------------------------------------------------------*/
;*    ucs2-string-set! ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-set! ucs2-string k ucs2)
   (if ($string-bound-check? k (ucs2-string-length ucs2-string))
       (c-ucs2-string-set! ucs2-string k ucs2)
       (error 'ucs2-string-set!
	      (string-append "index out of range [0.."
			     (integer->string
			      (-fx (ucs2-string-length ucs2-string) 1))
			     "]")
	      k)))

;*---------------------------------------------------------------------*/
;*    ucs2-string-ref-ur ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-ref-ur ucs2-string k)
   (c-ucs2-string-ref ucs2-string k))
 
;*---------------------------------------------------------------------*/
;*    ucs2-string-set-ur! ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-set-ur! ucs2-string k ucs2)
   (c-ucs2-string-set! ucs2-string k ucs2))

;*---------------------------------------------------------------------*/
;*    ucs2-string=? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string=? ucs2-string1 ucs2-string2)
   (c-ucs2-string=? ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string-ci=? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-ci=? ucs2-string1 ucs2-string2)
   (ucs2-strcicmp ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string<? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string<? ucs2-string1 ucs2-string2)
   (ucs2-string_lt ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string>? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string>? ucs2-string1 ucs2-string2)
   (ucs2-string_gt ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string<=? ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string<=? ucs2-string1 ucs2-string2)
   (ucs2-string_le ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string>=? ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string>=? ucs2-string1 ucs2-string2)
   (ucs2-string_ge ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string-ci<? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-ci<? ucs2-string1 ucs2-string2)
   (ucs2-string_cilt ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string-ci>? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-ci>? ucs2-string1 ucs2-string2)
   (ucs2-string_cigt ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string-ci<=? ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-ci<=? ucs2-string1 ucs2-string2)
   (ucs2-string_cile ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    ucs2-string-ci>=? ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-ci>=? ucs2-string1 ucs2-string2)
   (ucs2-string_cige ucs2-string1 ucs2-string2))

;*---------------------------------------------------------------------*/
;*    suucs2-string ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (subucs2-string ucs2-string start end)
   ;; no macro on inline so we don't use `and'
   (if (if (>=fx end start)
	   (if ($string-bound-check? start
		  (+fx (ucs2-string-length ucs2-string) 1))
	       ($string-bound-check? end
		  (+fx (ucs2-string-length ucs2-string) 1))
	       #f)
	   #f)
       (c-subucs2-string ucs2-string start end)
       (error "subucs2-string" "Illegal index" (cons start end))))

;*---------------------------------------------------------------------*/
;*    suucs2-string-ur ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (subucs2-string-ur ucs2-string start end)
   (c-subucs2-string ucs2-string start end))

;*---------------------------------------------------------------------*/
;*    ucs2-string-append ...                                           */
;*---------------------------------------------------------------------*/
(define (ucs2-string-append . list)
   (if (null? list)
       (make-ucs2-string 0)
       (let loop ((list list))
	  (if (null? (cdr list))
	      (car list)
	      (c-ucs2-string-append (car list) (loop (cdr list)))))))

;*---------------------------------------------------------------------*/
;*    list->ucs2-string ...                                            */
;*---------------------------------------------------------------------*/
(define (list->ucs2-string list)
   (let* ((len    (length list))
	  (ucs2-string (make-ucs2-string len)))
      (let loop ((i 0)
		 (l list))
	 (if (=fx i len)
	     ucs2-string
	     (begin
		(ucs2-string-set! ucs2-string i (car l))
		(loop (+fx i 1) (cdr l)))))))

;*---------------------------------------------------------------------*/
;*    ucs2-string->list ...                                            */
;*---------------------------------------------------------------------*/
(define (ucs2-string->list ucs2-string)
   (let ((len (ucs2-string-length ucs2-string)))
      (let loop ((i   0)
		 (acc '()))
	 (if (=fx i len)
	     (reverse! acc)
	     (loop (+fx i 1)
		   (cons (ucs2-string-ref ucs2-string i)
			 acc))))))

;*---------------------------------------------------------------------*/
;*    ucs2-string-copy ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string-copy ucs2-string)
   (c-ucs2-string-copy ucs2-string))

;*---------------------------------------------------------------------*/
;*    ucs2-string-fill! ...                                            */
;*---------------------------------------------------------------------*/
(define (ucs2-string-fill! ucs2-string ucs2)
   (let ((len (ucs2-string-length ucs2-string)))
      (let loop ((i 0))
	 (if (=fx i len)
	     ucs2-string
	     (begin
		(ucs2-string-set! ucs2-string i ucs2)
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    ucs2-string-upcase ...                                           */
;*---------------------------------------------------------------------*/
(define (ucs2-string-upcase ucs2-string)
   (let* ((len (ucs2-string-length ucs2-string))
	  (res (make-ucs2-string len)))
      (let loop ((i 0))
	 (if (=fx i len)
	     res
	     (begin
		(ucs2-string-set! res
		   i (ucs2-upcase (ucs2-string-ref ucs2-string i)))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    ucs2-string-downcase ...                                         */
;*---------------------------------------------------------------------*/
(define (ucs2-string-downcase ucs2-string)
   (let* ((len (ucs2-string-length ucs2-string))
	  (res (make-ucs2-string len)))
      (let loop ((i 0))
	 (if (=fx i len)
	     res
	     (begin
		(ucs2-string-set! res
		   i (ucs2-downcase (ucs2-string-ref ucs2-string i)))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    ucs2-string-upcase! ...                                          */
;*---------------------------------------------------------------------*/
(define (ucs2-string-upcase! ucs2-string)
   (let* ((len (ucs2-string-length ucs2-string))
	  (res ucs2-string))
      (let loop ((i 0))
	 (if (=fx i len)
	     res
	     (begin
		(ucs2-string-set! res
		   i (ucs2-upcase (ucs2-string-ref ucs2-string i)))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    ucs2-string-downcase! ...                                        */
;*---------------------------------------------------------------------*/
(define (ucs2-string-downcase! ucs2-string)
   (let* ((len (ucs2-string-length ucs2-string))
	  (res ucs2-string))
      (let loop ((i 0))
	 (if (=fx i len)
	     res
	     (begin
		(ucs2-string-set! res
		   i (ucs2-downcase (ucs2-string-ref ucs2-string i)))
		(loop (+fx i 1)))))))
		 		 
;*---------------------------------------------------------------------*/
;*    ucs2-string->utf8-string ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-string->utf8-string ucs2)
   (string-ascii-sentinel-mark! (c-ucs2-string->utf8-string ucs2)))

;*---------------------------------------------------------------------*/
;*    8bits ...                                                        */
;*---------------------------------------------------------------------*/
(define 8bits-inv
   '((#xe2
      (#x80 (#x90 . #\-)
	    (#x91 . #\-)
	    (#x92 . #\-)
	    (#x93 . #\-)
	    (#x94 . #\-)
	    (#x95 . #\-)
	    (#x98 . #\`)
	    (#x99 . #\')
	    (#x9a . #\,)
	    (#x9b . #\`)
	    (#xa4 . #\.)
	    (#xa7 . #\.)
	    (#xb2 . #\')
	    (#xb3 . #\")
	    (#xb5 . #\`)
	    (#xbb . #\")
	    (#xb8 . #\^)
	    (#xb9 . #\<)
	    (#xba . #\>))
      (#x81 (#x83 . #\*))
      (#x82 (#x8d . #\()
	    (#x8e . #\))))))

(define 8bits-inv-latin-15
   '((#xe2
	(#x80 (#x90 . #\-)
	   (#x91 . #\-)
	   (#x92 . #\-)
	   (#x93 . #\-)
	   (#x94 . #\-)
	   (#x95 . #\-)
	   (#x98 . #\`)
	   (#x99 . #\')
	   (#x9a . #\,)
	   (#x9b . #\`)
	   (#xa4 . #\.)
	   (#xa7 . #\.)
	   (#xb2 . #\')
	   (#xb3 . #\")
	   (#xb5 . #\`)
	   (#xbb . #\")
	   (#xb8 . #\^)
	   (#xb9 . #\<)
	   (#xba . #\>))
	(#x81 (#x83 . #\*))
	(#x82 (#x8d . #\()
	   (#x8e . #\))
	   (#xac . #a164) ; euro currency sign
	   ))
     (#xc5
	(#x93 . #a189) ;xbd oe ligature
	(#x92 . #a188) ;xbc OE ligature
	(#xb8 . #a190) ;xbe Y diaeresis
	(#xa1 . #a168) ;xa8 s with caron
	(#xa0 . #a166) ;xa6 S with caron
	(#xbe . #a184) ;xb8 z with caron
	(#xbd . #a180) ;xb4 Z with caron
	)))
   
;*---------------------------------------------------------------------*/
;*    cp1252 ...                                                       */
;*---------------------------------------------------------------------*/
(define cp1252
   '#("\xe2\x82\xac" ;; 0x80
      ""             ;; 0x81
      "\xe2\x80\x9a" ;; 0x82
      "\xc6\x92"     ;; 0x83
      "\xe2\x80\x9e" ;; 0x84
      "\xe2\x80\xa6" ;; 0x85
      "\xe2\x80\xa0" ;; 0x86
      "\xe2\x80\xa1" ;; 0x87
      "\xcb\x86"     ;; 0x88
      "\xe2\x80\xb0" ;; 0x89
      "\xc5\xa0"     ;; 0x8a
      "\xe2\x80\xb9" ;; 0x8b
      "\xc5\x92"     ;; 0x8c
      ""             ;; 0x8d
      "\xc5\xbd"     ;; 0x8e
      ""             ;; 0x8f
      ""             ;; 0x90
      "\xe2\x80\x98" ;; 0x91
      "\xe2\x80\x99" ;; 0x92
      "\xe2\x80\x9c" ;; 0x93
      "\xe2\x80\x9d" ;; 0x94
      "\xe2\x80\xa2" ;; 0x95
      "\xe2\x80\x93" ;; 0x96
      "\xe2\x80\x94" ;; 0x97
      "\xcb\x9c"     ;; 0x98
      "\xe2\x84\xa2" ;; 0x99
      "\xc5\xa1"     ;; 0x9a
      "\xe2\x80\xba" ;; 0x9b
      "\xc5\x93"     ;; 0x9c
      ""             ;; 0x9d
      "\xc5\xbe"     ;; 0x9e
      "\xc5\xb8"))   ;; 0x9f

(define cp1252-inv #f)

;*---------------------------------------------------------------------*/
;*    inverse-utf8-table ...                                           */
;*---------------------------------------------------------------------*/
(define (inverse-utf8-table table)
   
   (define (make-table-entry s char)
      (let ((len (string-length s)))
	 (let loop ((i 0))
	    (if (=fx len i)
		(integer->char char)
		(list (cons (char->integer (string-ref s i)) (loop (+fx i 1))))))))
   
   (define (add-table-entry table s char)
      (let loop ((e (car (make-table-entry s char)))
		 (table table))
	 (if (null? e)
	     table
	     (let* ((n (car e))
		    (o (assq n table)))
		(if o
		    (let ((st (cdr o)))
		       (set-cdr! o (loop (cadr e) st))
		       table)
		    (cons e table))))))
   
   (let ((len (vector-length table)))
      (let loop ((i 0)
		 (res '()))
	 (if (=fx i len)
	     res
	     (let ((s (vector-ref table i)))
		(if (>fx (string-length s) 0)
		    (loop (+fx i 1) (add-table-entry res s (+fx #x80 i)))
		    (loop (+fx i 1) res)))))))

;*---------------------------------------------------------------------*/
;*    utf8-string->ucs2-string ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (utf8-string->ucs2-string utf8)
   (c-utf8-string->ucs2-string utf8))

;*---------------------------------------------------------------------*/
;*    utf8->8bits-length ...                                           */
;*---------------------------------------------------------------------*/
(define (utf8->8bits-length str len)
   (let loop ((size 0)
	      (i 0))
      (if (>=fx i len)
	  size
	  (let ((c (char->integer (string-ref str i))))
	     (cond
		((<fx c #xc2)
		 (loop (+fx size 1) (+fx i 1)))
		((<=fx c #xdf)
		 (loop (+fx size 1) (+fx i 2)))
		((<=fx c #xef)
		 (loop (+fx size 1) (+fx i 3)))
		((<=fx c #xf7)
		 (loop (+fx size 1) (+fx i 4)))
		((<=fx c #xfb)
		 (loop (+fx size 1) (+fx i 5)))
		(else
		 (loop (+fx size 1) (+fx i 6))))))))

;*---------------------------------------------------------------------*/
;*    ascii-string? ...                                                */
;*---------------------------------------------------------------------*/
(define (ascii-string? str)
   (>=fx (string-ascii-sentinel str) (string-length str)))

;*---------------------------------------------------------------------*/
;*    string-ascii-sentinel-mark! ...                                  */
;*---------------------------------------------------------------------*/
(define (string-ascii-sentinel-mark! string)
   (let ((len (string-length string)))
      (let loop ((i 0))
	 (when (<fx i len)
	    (if (>fx (char->integer (string-ref string i)) 127)
		(string-ascii-sentinel-set! string i)
		(loop (+fx i 1)))))
      string))

;*---------------------------------------------------------------------*/
;*    utf8-string? ...                                                 */
;*---------------------------------------------------------------------*/
(define (utf8-string? str #!optional strict::bool)
   
   (define (in-range? c minc maxc)
      (let ((n (char->integer c)))
	 (and (>=fx n minc) (<=fx n maxc))))
   
   (let ((len (string-length str)))
      (let loop ((r 0))
	 (if (=fx r len)
	     #t
	     (let* ((c (string-ref str r))
		    (n (char->integer c)))
		(cond
		   ((<=fx n #x7f)
		    ;; 1 byte
		    (loop (+fx r 1)))
		   ((<fx n #xc2)
		    ;; error, reserved
		    #f)
		   ((<fx n #xdf)
		    ;; two chars encoding
		    (when (and (<fx (+fx 1 r) len)
			       (in-range? (string-ref str (+fx r 1)) #x80 #xbf))
		       (loop (+fx r 2))))
		   ((and (>=fx n #xd8) (<=fx n #xdb))
		    ;; utf16 escape
		    (when (and (<fx r (-fx len 3))
			       (in-range? (string-ref str (+fx r 1)) #xdc #xdf)
			       (in-range? (string-ref str (+fx r 2)) #xdc #xdf)
			       (in-range? (string-ref str (+fx r 3)) #xdc #xdf))
		       (loop (+fx r 4))))
		   ((<=fx n #xdf)
		    ;; utf16 error
		    (when (and (<fx (+fx 1 r) len)
			       (in-range? (string-ref str (+fx r 1)) #x80 #xbf))
		       (loop (+fx r 2))))
		   ((<=fx n #xef)
		    ;; 3 bytes sequence
		    (when (and (<fx r (-fx len 2))
			       (in-range? (string-ref str (+fx r 1)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 2)) #x80 #xbf))
		       (loop (+fx r 3))))
		   ((=fx n #xf0)
		    ;; 4 bytes sequence special1
		    (when (and (<fx r (-fx len 3))
			       (in-range? (string-ref str (+fx r 1)) #x90 #xbf)
			       (in-range? (string-ref str (+fx r 2)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 3)) #x80 #xbf))
		       (loop (+fx r 4))))
		   ((or (=fx n #xf4)
			(and (or (=fx n #xf8) (=fx n #xfc)) (not strict)))
		    ;; 4 bytes sequence special2
		    (when (and (<fx r (-fx len 3))
			       (in-range? (string-ref str (+fx r 1)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 2)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 3)) #x80 #xbf))
		       (loop (+fx r 4))))
		   ((<=fx n #xf7)
		    ;; 4 bytes sequence
		    (when (and (<fx r (-fx len 3))
			       (in-range? (string-ref str (+fx r 1)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 2)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 3)) #x80 #xbf))
		       (loop (+fx r 4))))
		   ((<=fx n #xfb)
		    (when (and (<fx r (-fx len 4))
			       (in-range? (string-ref str (+fx r 1)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 2)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 3)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 4)) #x80 #xbf))
		       (loop (+fx r 5))))
		   ((<=fx n #xfd)
		    (when (and (<fx r (-fx len 5))
			       (in-range? (string-ref str (+fx r 1)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 2)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 3)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 4)) #x80 #xbf)
			       (in-range? (string-ref str (+fx r 5)) #x80 #xbf))
		       (loop (+fx r 6))))
		   (else
		    #f)))))))

;*---------------------------------------------------------------------*/
;*    utf8-string-encode ...                                           */
;*    -------------------------------------------------------------    */
;*    Replace all the occurrences of illegal UTF8 characters with      */
;*    the UNICODE replacement character EF BF BD.                      */
;*---------------------------------------------------------------------*/
(define (utf8-string-encode str::bstring #!optional strict::bool (start::long 0) (end::long (string-length str)))
   
   (define (in-range? c minc maxc)
      (let ((n (char->integer c)))
	 (and (>=fx n minc) (<=fx n maxc))))
   
   (define (string-unicode-fix! s j)
      ;; Unicode Replacement Character EF BF BD */
      (string-set! s j #a239)
      (string-set! s (+fx j 1) #a191)
      (string-set! s (+fx j 2) #a189))

   (cond
      ((or (>fx end (string-length str))
	   (<fx start 0)
	   (>fx start end))
       (error "utf8-string-encode" "Illegal indexes" (cons start end)))
      ((ascii-string? str)
       ;; no need to replace anything, its an ascii string
       (string-copy str))
      (else
       (let* ((len (-fx end start))
	      (res (make-string (*fx 3 len))))
	  ;; mask the UTF8 sentinel
	  (string-ascii-sentinel-set! res (string-ascii-sentinel str))
	  (let loop ((r start)
		     (w 0))
	     (if (=fx r end)
		 (string-shrink! res w)
		 (let* ((c (string-ref str r))
			(n (char->integer c)))
		    (cond
		       ((<=fx n #x7f)
			;; 1 byte
			(string-set! res w c)
			(loop (+fx r 1) (+fx w 1)))
		       ((<fx n #xc2)
			;; error, reserved
			(string-unicode-fix! res w)
			(loop (+fx r 1) (+fx w 3)))
		       ((<fx n #xd8)
			;; two chars encoding
			(if (and (<fx (+fx 1 r) len)
				 (in-range? (string-ref str (+fx r 1)) #x80 #xbf))
			    (begin
			       (string-set! res w c)
			       (string-set! res (+fx w 1) (string-ref str (+fx r 1)))
			       (loop (+fx r 2) (+fx w 2)))
			    (begin
			       (string-unicode-fix! res w)
			       (loop (+fx r 1) (+fx w 3)))))
		       ((and (>=fx n #xd8) (<=fx n #xdb))
			;; utf16 escape
			(if (and (<fx r (-fx len 3))
				   (in-range? (string-ref str (+fx r 1)) #xdc #xdf)
				   (in-range? (string-ref str (+fx r 2)) #xdc #xdf)
				   (in-range? (string-ref str (+fx r 3)) #xdc #xdf))
			   (begin
			      (string-set! res w c)
			      (string-set! res (+fx w 1) (string-ref str (+fx r 1)))
			      (string-set! res (+fx w 2) (string-ref str (+fx r 2)))
			      (string-set! res (+fx w 3) (string-ref str (+fx r 3)))
			      (loop (+fx r 4) (+fx w 4)))
			   (begin
			      (string-unicode-fix! res w)
			      (loop (+fx r 1) (+fx w 3)))))
		       ((<=fx n #xdf)
			;; utf16 error
			(if (and (<fx (+fx 1 r) len)
				 (in-range? (string-ref str (+fx r 1)) #x80 #xbf))
			    (begin
			       (string-set! res w c)
			       (string-set! res (+fx w 1) (string-ref str (+fx r 1)))
			       (loop (+fx r 2) (+fx w 2)))
			    (begin
			       (string-unicode-fix! res w)
			       (loop (+fx r 1) (+fx w 3)))))
		       ((<=fx n #xef)
			;; 3 bytes sequence
			(if (and (<fx r (-fx len 2))
				 (in-range? (string-ref str (+fx r 1)) #x80 #xbf)
				 (in-range? (string-ref str (+fx r 2)) #x80 #xbf))
			    (begin
			       (string-set! res w c)
			       (string-set! res (+fx w 1) (string-ref str (+fx r 1)))
			       (string-set! res (+fx w 2) (string-ref str (+fx r 2)))
			       (loop (+fx r 3) (+fx w 3)))
			    (begin
			       (string-unicode-fix! res w)
			       (loop (+fx r 1) (+fx w 3)))))
		       ((=fx n #xf0)
			;; 4 bytes sequence special1
			(if (and (<fx r (-fx len 3))
				 (in-range? (string-ref str (+fx r 1)) #x90 #xbf)
				 (in-range? (string-ref str (+fx r 2)) #x80 #xbf)
				 (in-range? (string-ref str (+fx r 3)) #x80 #xbf))
			    (begin
			       (string-set! res w c)
			       (string-set! res (+fx w 1) (string-ref str (+fx r 1)))
			       (string-set! res (+fx w 2) (string-ref str (+fx r 2)))
			       (string-set! res (+fx w 3) (string-ref str (+fx r 3)))
			       (loop (+fx r 4) (+fx w 4)))
			    (begin
			       (string-unicode-fix! res w)
			       (loop (+fx r 1) (+fx w 3)))))
		       ((or (=fx n #xf4)
			    (and (or (=fx n #xf8) (=fx n #xfc)) (not strict)))
			;; 4 bytes sequence special2
			(if (and (<fx r (-fx len 3))
				 (in-range? (string-ref str (+fx r 1)) #x80 #xbf)
				 (in-range? (string-ref str (+fx r 2)) #x80 #xbf)
				 (in-range? (string-ref str (+fx r 3)) #x80 #xbf))
			    (begin
			       (string-set! res w c)
			       (string-set! res (+fx w 1) (string-ref str (+fx r 1)))
			       (string-set! res (+fx w 2) (string-ref str (+fx r 2)))
			       (string-set! res (+fx w 3) (string-ref str (+fx r 3)))
			       (loop (+fx r 4) (+fx w 4)))
			    (begin
			       (string-unicode-fix! res w)
			       (loop (+fx r 1) (+fx w 3)))))
		       ((<=fx n #xf7)
			;; 4 bytes sequence
			(if (and (<fx r (-fx len 3))
				 (in-range? (string-ref str (+fx r 1)) #x80 #xbf)
				 (in-range? (string-ref str (+fx r 2)) #x80 #xbf)
				 (in-range? (string-ref str (+fx r 3)) #x80 #xbf))
			    (begin
			       (string-set! res w c)
			       (string-set! res (+fx w 1) (string-ref str (+fx r 1)))
			       (string-set! res (+fx w 2) (string-ref str (+fx r 2)))
			       (string-set! res (+fx w 3) (string-ref str (+fx r 3)))
			       (loop (+fx r 4) (+fx w 4)))
			    (begin
			       (string-unicode-fix! res w)
			       (loop (+fx r 1) (+fx w 3)))))
		       ((<=fx n #xfb)
			(if (and (<fx r (-fx len 4))
				 (in-range? (string-ref str (+fx r 1)) #x80 #xbf)
				 (in-range? (string-ref str (+fx r 2)) #x80 #xbf)
				 (in-range? (string-ref str (+fx r 3)) #x80 #xbf)
				 (in-range? (string-ref str (+fx r 4)) #x80 #xbf))
			    (begin
			       (string-set! res w c)
			       (string-set! res (+fx w 1) (string-ref str (+fx r 1)))
			       (string-set! res (+fx w 2) (string-ref str (+fx r 2)))
			       (string-set! res (+fx w 3) (string-ref str (+fx r 3)))
			       (string-set! res (+fx w 4) (string-ref str (+fx r 4)))
			       (loop (+fx r 5) (+fx w 5)))
			    (begin
			       (string-unicode-fix! res w)
			       (loop (+fx r 1) (+fx w 3)))))
		       ((<=fx n #xfd)
			(if (and (<fx r (-fx len 5))
				 (in-range? (string-ref str (+fx r 1)) #x80 #xbf)
				 (in-range? (string-ref str (+fx r 2)) #x80 #xbf)
				 (in-range? (string-ref str (+fx r 3)) #x80 #xbf)
				 (in-range? (string-ref str (+fx r 4)) #x80 #xbf)
				 (in-range? (string-ref str (+fx r 5)) #x80 #xbf))
			    (begin
			       (string-set! res w c)
			       (string-set! res (+fx w 1) (string-ref str (+fx r 1)))
			       (string-set! res (+fx w 2) (string-ref str (+fx r 2)))
			       (string-set! res (+fx w 3) (string-ref str (+fx r 3)))
			       (string-set! res (+fx w 4) (string-ref str (+fx r 4)))
			       (string-set! res (+fx w 5) (string-ref str (+fx r 5)))
			       (loop (+fx r 6) (+fx w 6)))
			    (begin
			       (string-unicode-fix! res w)
			       (loop (+fx r 1) (+fx w 3)))))
		       (else
			#f)))))))))

;*---------------------------------------------------------------------*/
;*    utf8-char-size ...                                               */
;*---------------------------------------------------------------------*/
(define (utf8-char-size c)
   (let ((n (char->integer c)))
      (cond
	 ((<=fx n #x7f) 1)
	 ((<=fx n #xc0) 2)
	 ((<fx n #xc2) (error "utf8-string" "Badly formed UTF8 string" c))
	 ((<=fx n #xdf) 2)
	 ((<=fx n #xef) 3)
	 ((or (=fx n #xf0) (=fx n #xf4) (<=fx n #xf7)) 4)
	 ((=fx n #xf8) 4) ;; see utf8-string-append
	 ((<=fx n #xfb) 5)
	 ((=fx n #xfc) 4) ;; see utf8-string-append
	 ((<=fx n #xfd) 6)
	 (else (error "utf8-string" "Badly formed UTF8 string" c)))))

;*---------------------------------------------------------------------*/
;*    utf8-string-length ...                                           */
;*    -------------------------------------------------------------    */
;*    Return the number of characters of an UTF8 string.               */
;*---------------------------------------------------------------------*/
(define (utf8-string-length str)
   (let ((len (string-length str))
	 (sen (string-ascii-sentinel str)))
      (if (>=fx sen len)
	  len
	  (let loop ((r sen)
		     (l sen))
	     (if (=fx r len)
		 l
		 (loop (+fx r (utf8-char-size (string-ref str r)))
		    (+fx l 1)))))))

;*---------------------------------------------------------------------*/
;*    utf8-string-ref ...                                              */
;*---------------------------------------------------------------------*/
(define (utf8-string-ref str i)
   (let ((sentinel (string-ascii-sentinel str)))
      (if (<fx i sentinel)
	  (string-ascii-sentinel-mark! (string (string-ref str i)))
	  (let ((len (string-length str)))
	     (let loop ((r sentinel) (i (-fx i sentinel)))
		(let* ((c (string-ref str r))
		       (s (utf8-char-size c)))
		   (if (=fx i 0)
		       (string-ascii-sentinel-mark! (substring str r (+fx r s)))
		       (loop (+fx r s) (-fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    utf8-string-index->string-index ...                              */
;*---------------------------------------------------------------------*/
(define (utf8-string-index->string-index str i)
   (cond
      ((<fx i 0)
       -1)
      ((<fx i (string-ascii-sentinel str))
       i)
      (else
       (let ((len (string-length str)))
	  (let loop ((r 0) (i i))
	     (cond
		((=fx i 0)
		 r)
		((<fx r len)
		 (let ((c (string-ref str r)))
		    (loop (+fx r (utf8-char-size c)) (-fx i 1))))
		(else
		 -1)))))))

;*---------------------------------------------------------------------*/
;*    utf8-string-left-replacement? ...                                */
;*    -------------------------------------------------------------    */
;*    This predicate returns #t iff the last UTF8 char is a            */
;*    replacement char.                                                */
;*---------------------------------------------------------------------*/
(define-inline (utf8-string-left-replacement? str len)
   (and (>=fx len 4) (=fx (char->integer (string-ref-ur str (-fx len 4))) #xf8)))

;*---------------------------------------------------------------------*/
;*    utf8-string-right-replacement? ...                               */
;*    -------------------------------------------------------------    */
;*    Does the STR starts with the right-end-side of an UTF8           */
;*    replacement char?                                                */
;*---------------------------------------------------------------------*/
(define-inline (utf8-string-right-replacement? str len)
   (and (>=fx len 4) (=fx (char->integer (string-ref-ur str 0)) #xfc)))
   
;*---------------------------------------------------------------------*/
;*    utf8-string-append ...                                           */
;*    -------------------------------------------------------------    */
;*    This function handles cases where the last char of the           */
;*    concatanated char is a UNICODE remplacement char.                */
;*---------------------------------------------------------------------*/
(define (utf8-string-append-old left right)

   (define (string-append-utf8-left left right)
      ;; append two strings, the left one being UTF8
      (let ((s (string-append left right)))
	 (string-ascii-sentinel-set! s (string-ascii-sentinel left))
	 s))

   (let ((lenf (string-length left)))
      (cond
	 ((and (>=fx lenf 4)
	       (=fx (char->integer (string-ref-ur left (-fx lenf 4))) #xf8))
	  ;; the left string is an UTF8 string with potentially a
	  ;; replace char at its last index
	  (let ((lenr (string-length right)))
	     (if (>=fx lenr 4)
		 (let ((cr1 (char->integer (string-ref-ur right 0))))
		    (if (=fx cr1 #xfc)
			(let* ((tmp ($make-string/wo-fill (+fx (-fx lenf 4) lenr)))
			       (cl1 (char->integer (string-ref-ur left (-fx lenf 4))))
			       (cl2 (char->integer (string-ref-ur left (-fx lenf 3))))
			       (cl3 (char->integer (string-ref-ur left (-fx lenf 2))))
			       (cl4 (char->integer (string-ref-ur left (-fx lenf 1))))
			       (cr2 (char->integer (string-ref-ur right 1)))
			       (cr3 (char->integer (string-ref-ur right 2)))
			       (cr4 (char->integer (string-ref-ur right 3)))
			       (zzzzzz (bit-and #b111111 cr4))
			       (yyyy (bit-and #b1111 cr3))
			       (xx (bit-and (bit-rsh cl3 4) #b11))
			       (wwww (bit-and cl3 #b1111))
			       (uuuuu (bit-or
					 (bit-lsh (bit-and cl4 #b111) 2)
					 (bit-and (bit-rsh cl2 4) #b11))))
			   (blit-string! left 0 tmp 0 (-fx lenf 4))
			   (blit-string! right 2 tmp (-fx lenf 2) (-fx lenr 2))
			   ;; byte 1
			   (string-set! tmp (-fx lenf 4)
			      (integer->char
				 (bit-or
				    (bit-and cl1 #b11110000)
				    (bit-rsh uuuuu 2))))
			   ;; byte 2
			   (string-set! tmp (-fx lenf 3)
			      (integer->char cl2))
			   ;; byte 3
			   (string-set! tmp (-fx lenf 2)
			      (integer->char
				 (bit-or #x80
				    (bit-or (bit-lsh xx 4) yyyy))))
			   ;; byte 4
			   (string-set! tmp (-fx lenf 1)
			      (integer->char cr4))
			   tmp)
			(string-append-utf8-left left right)))
		 (string-append-utf8-left left right))))
	 ((not (ascii-string? left))
	  (string-append-utf8-left left right))
	 ((not (ascii-string? right))
	  (let ((s (string-append left right)))
	     (string-ascii-sentinel-set! s
		(+fx (string-length left) (string-ascii-sentinel right)))
	     s))
	 (else
	  (string-append left right)))))

;*---------------------------------------------------------------------*/
;*    utf8-string-append-fill! ...                                     */
;*    -------------------------------------------------------------    */
;*    This function must used only in left to right string append.     */
;*    -------------------------------------------------------------    */
;*    Append the LEFT and RIGHT string into the BUFFER at index        */
;*    This function handles cases where the last char of the           */
;*    concatanated char is a UNICODE remplacement char.                */
;*    -------------------------------------------------------------    */
;*    This function sets the BUFFER ascii sentinel.                    */
;*---------------------------------------------------------------------*/
(define (utf8-string-append-fill! buffer index str)
   (let ((len (string-length str)))
      (cond
	 ((ascii-string? str)
	  ;; left string is ascii
	  (blit-string! str 0 buffer index len)
	  ;; update the sentinel
	  (let ((nindex (+fx index len))
		(sentinel (string-ascii-sentinel buffer)))
	     (when (>=fx sentinel index)
		(string-ascii-sentinel-set! buffer nindex))
	     nindex))
	 ((and (>=fx index 4)
	       (utf8-string-right-replacement? str len)
	       (utf8-string-left-replacement? buffer index))
	  (blit-string! str 4 buffer index (-fx len 4))
	  ;; collapsed character
	  (let* ((cl1 (char->integer (string-ref-ur buffer (-fx index 4))))
		 (cl2 (char->integer (string-ref-ur buffer (-fx index 3))))
		 (cl3 (char->integer (string-ref-ur buffer (-fx index 2))))
		 (cl4 (char->integer (string-ref-ur buffer (-fx index 1))))
		 (cr2 (char->integer (string-ref-ur str 1)))
		 (cr3 (char->integer (string-ref-ur str 2)))
		 (cr4 (char->integer (string-ref-ur str 3)))
		 (zzzzzz (bit-and #b111111 cr4))
		 (yyyy (bit-and #b1111 cr3))
		 (xx (bit-and (bit-rsh cl3 4) #b11))
		 (wwww (bit-and cl3 #b1111))
		 (uuuuu (bit-or
			   (bit-lsh (bit-and cl4 #b111) 2)
			   (bit-and (bit-rsh cl2 4) #b11))))
	     ;; byte 1
	     (string-set! buffer (-fx index 4)
		(integer->char
		   (bit-or
		      (bit-and cl1 #b11110000)
		      (bit-rsh uuuuu 2))))
	     ;; byte 2
	     (string-set! buffer (-fx index 3)
		(integer->char cl2))
	     ;; byte 3
	     (string-set! buffer (-fx index 2)
		(integer->char
		   (bit-or #x80
		      (bit-or (bit-lsh xx 4) yyyy))))
	     ;; byte 4
	     (string-set! buffer (-fx index 1)
		(integer->char cr4)))
	  ;; shrink the buffer
	  (+fx index (-fx len 4)))
	 (else
	  ;; an utf8 string
	  (blit-string! str 0 buffer index len)
	  (let ((sentinel (string-ascii-sentinel buffer)))
	     (when (>=fx sentinel index)
		(string-ascii-sentinel-set! buffer (+fx index (string-ascii-sentinel str))))
	     (+fx index len))))))

;*---------------------------------------------------------------------*/
;*    utf8-string-append ...                                           */
;*    -------------------------------------------------------------    */
;*    Append two UTF8 strings and update the ascii-sentinel.           */
;*    -------------------------------------------------------------    */
;*    This function handles cases where the last char of the           */
;*    concatanated char is a UNICODE remplacement char.                */
;*---------------------------------------------------------------------*/
(define (utf8-string-append left right)
   (let* ((llen (string-length left))
	  (rlen (string-length right))
	  (buffer ($make-string/wo-fill (+fx llen rlen))))
      (let ((nindex (utf8-string-append-fill! buffer 0 left)))
	 (let ((nindex (utf8-string-append-fill! buffer nindex right)))
	    (let ((s (string-shrink! buffer nindex)))
	       (if (string=? s (utf8-string-append-old left right))
		   s
		   (error "utf8-string-append" "bad append"
		      (cons left right))))))))

;*---------------------------------------------------------------------*/
;*    utf8-string-append* ...                                          */
;*    -------------------------------------------------------------    */
;*    Append N UTF8 strings and update the ascii-sentinel.             */
;*---------------------------------------------------------------------*/
(define (utf8-string-append* . strings)
   (let ((len 0))
      (for-each (lambda (str)
		   (set! len (+fx len (string-length str))))
	 strings)
      (let ((buffer ($make-string/wo-fill len))
	    (index 0))
	 (for-each (lambda (str)
		      (set! index (utf8-string-append-fill! buffer index str)))
	    strings)
	 (string-shrink! buffer index))))

;*---------------------------------------------------------------------*/
;*    utf8-substring ...                                               */
;*---------------------------------------------------------------------*/
(define (utf8-substring str::bstring start::long
	   #!optional (end::long (utf8-string-length str)))
   (let ((len (string-length str)))
      (cond
	 ((or (<fx start 0) (>fx start len))
	  (error "utf8-substring"
	     (string-append "Illegal start index \"" str "\"")
	     start))
	 ((<fx end 0)
	  (error "utf8-substring"
	     (string-append "Illegal end index \"" str "\"")
	     end))
	 ((or (<fx end start) (>fx end len))
	  (error "utf8-substring"
	     (string-append "Illegal end index \"" str "\"")
	     end))
	 ((=fx start end)
	  "")
	 ((ascii-string? str)
	  (substring str start end))
	 (else
	  (let loop ((r 0)
		     (n 0)
		     (i -1))
	     (if (=fx r len)
		 (string-ascii-sentinel-mark! (substring str i r))
		 (let* ((c (string-ref str r))
			(s (utf8-char-size c)))
		    (cond
		       ((=fx n start)
			(loop (+fx r s) (+fx n 1) r))
		       ((=fx n end)
			(substring str i r))
		       (else
			(loop (+fx r s) (+fx n 1) i))))))))))

;*---------------------------------------------------------------------*/
;*    utf8->8bits-fill! ...                                            */
;*---------------------------------------------------------------------*/
(define (utf8->8bits-fill! nstr::bstring str::bstring len::int table)
   
   (define (error-too-short r)
      (error "utf8->8bits"
	 "String too short"
	 (string-for-read (substring str (maxfx 0 (-fx r 10)) len))))
   
   (define (error-ill r)
      (error "utf8->8bits"
	 (format "Illegal character \"~x\" at index ~a"
	    (char->integer (string-ref str r))
	    r)
	 (string-for-read (substring str r (minfx len (+fx r 10))))))
   
   (define (error-subtable r)
      (error "utf8->8bits"
	 (string-append "Cannot encode at index " (integer->string r))
	 (string-for-read (substring str r (minfx len (+fx r 10))))))

   (define (table->8bits subtable r w n)
      (let liip ((subtable (assq n table))
		 (nr (+fx r 1)))
	 (cond
	    ((not subtable)
	     (error-subtable r))
	    ((char? (cdr subtable))
	     (string-set! nstr w (cdr subtable))
	     nr)
	    ((=fx nr len)
	     (error-too-short r))
	    (else
	     (let ((nc (char->integer (string-ref str nr))))
		(liip (assq nc (cdr subtable)) (+fx nr 1)))))))

   (let loop ((r 0)
	      (w 0))
      (if (=fx r len)
	  (string-ascii-sentinel-mark! nstr)
	  (let* ((c (string-ref str r))
		 (n (char->integer c)))
	     (cond
		((<=fx n #x7f)
		 (string-set! nstr w c)
		 (loop (+fx r 1) (+fx w 1)))
		((<fx n #xc2)
		 (error-ill r))
		((and table (assq n table))
		 =>
		 (lambda (subtable)
		    (loop (table->8bits subtable r w n) (+fx w 1))))
		((<=fx n #xdf)
		 (if (=fx r (-fx len 1))
		     (error-too-short r)
		     (let* ((nc (string-ref str (+fx r 1)))
			    (nn (char->integer nc)))
			(let ((m (bit-or (bit-lsh (bit-and n #x1f) 6)
				    (bit-and #x3f nn))))
			   (if (>fx m #xff)
			       (begin
				  ;; before bigloo3.9b, an error was raised
				  ;; here. it has been found judicious to
				  ;; replace the ill char with .
				  (string-set! nstr w #\.)
				  (loop (+fx r 2) (+fx w 1)))
			       (begin
				  (string-set! nstr w (integer->char m))
				  (loop (+fx r 2) (+fx w 1))))))))
		(else
		 (error-ill r)))))))

;*---------------------------------------------------------------------*/
;*    utf8->8bits ...                                                  */
;*---------------------------------------------------------------------*/
(define (utf8->8bits str table)
   (let* ((len (string-length str))
	  (nlen (utf8->8bits-length str len)))
      (if (=fx len nlen)
	  (string-copy str)
	  (utf8->8bits-fill! (make-string nlen) str len table))))

;*---------------------------------------------------------------------*/
;*    utf8->8bits! ...                                                 */
;*---------------------------------------------------------------------*/
(define (utf8->8bits! str table)
   (let* ((len (string-length str))
	  (nlen (utf8->8bits-length str len)))
      (if (=fx len nlen)
	  str
	  (utf8->8bits-fill! (make-string nlen) str len table))))

;*---------------------------------------------------------------------*/
;*    utf8->iso-latin ...                                              */
;*---------------------------------------------------------------------*/
(define (utf8->iso-latin str)
   (utf8->8bits str 8bits-inv))

;*---------------------------------------------------------------------*/
;*    utf8->iso-latin! ...                                             */
;*---------------------------------------------------------------------*/
(define (utf8->iso-latin! str)
   (utf8->8bits! str 8bits-inv))

;*---------------------------------------------------------------------*/
;*    utf8->iso-latin-15 ...                                           */
;*---------------------------------------------------------------------*/
(define (utf8->iso-latin-15 str)
   (utf8->8bits str 8bits-inv-latin-15))

;*---------------------------------------------------------------------*/
;*    utf8->iso-latin-15! ...                                          */
;*---------------------------------------------------------------------*/
(define (utf8->iso-latin-15! str)
   (utf8->8bits! str 8bits-inv-latin-15))

;*---------------------------------------------------------------------*/
;*    utf8->cp1252 ...                                                 */
;*---------------------------------------------------------------------*/
(define (utf8->cp1252 str)
   (unless cp1252-inv (set! cp1252-inv (inverse-utf8-table cp1252)))
   (utf8->8bits str cp1252-inv))

;*---------------------------------------------------------------------*/
;*    utf8->cp1252! ...                                                */
;*---------------------------------------------------------------------*/
(define (utf8->cp1252! str)
   (unless cp1252-inv (set! cp1252-inv (inverse-utf8-table cp1252)))
   (utf8->8bits! str cp1252-inv))

;*---------------------------------------------------------------------*/
;*    8bits->utf8-length ...                                           */
;*---------------------------------------------------------------------*/
(define (8bits->utf8-length str len table)
   
   (define (table-ref-len c)
      (let ((v (-fx c #x80)))
	 (if (<fx v (vector-length table))
	     (string-length (vector-ref table v))
	     2)))
   
   (let loop ((size 0)
	      (i 0))
      (if (=fx i len)
	  size
	  (let ((c (char->integer (string-ref-ur str i))))
	     (if (>=fx c #x80)
		 (if table
		     (loop (+fx size (table-ref-len c)) (+fx i 1))
		     (loop (+fx size 2) (+fx i 1)))
		 (loop (+fx size 1) (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    8bits->utf8-fill! ...                                            */
;*---------------------------------------------------------------------*/
(define (8bits->utf8-fill! nstr str len table)
   (let loop ((r 0)
	      (w 0))
      (if (=fx r len)
	  (string-ascii-sentinel-mark! nstr)
	  (let ((c (char->integer (string-ref-ur str r))))
	     (cond
		((>=fx c #xc0)
		 (string-set-ur! nstr w (integer->char #xc3))
		 (string-set-ur! nstr (+fx w 1) (integer->char (-fx c #x40)))
		 (loop (+fx r 1) (+fx w 2)))
		((>=fx c #x80)
		 (if table
		     (let ((v (-fx c #x80)))
			(if (<fx v (vector-length table))
			    (let* ((enc (vector-ref table v))
				   (len (string-length enc)))
			       (blit-string! enc 0 nstr w len)
			       (loop (+fx r 1) (+fx w len)))
			    (begin
			       (string-set-ur! nstr w (integer->char #xc2))
			       (string-set-ur! nstr (+fx w 1) (integer->char c))
			       (loop (+fx r 1) (+fx w 2)))))
		     (begin
			(string-set-ur! nstr w (integer->char #xc2))
			(string-set-ur! nstr (+fx w 1) (integer->char c))
			(loop (+fx r 1) (+fx w 2)))))
		(else
		 (string-set-ur! nstr w (integer->char c))
		 (loop (+fx r 1) (+fx w 1))))))))

;*---------------------------------------------------------------------*/
;*    8bits->utf8 ...                                                  */
;*---------------------------------------------------------------------*/
(define (8bits->utf8 str table)
   (let* ((len (string-length str))
	  (nlen (8bits->utf8-length str len table)))
      (if (=fx len nlen)
	  (string-copy str)
	  (8bits->utf8-fill! ($make-string/wo-fill nlen) str len table))))

;*---------------------------------------------------------------------*/
;*    8bits->utf8! ...                                                 */
;*---------------------------------------------------------------------*/
(define (8bits->utf8! str table)
   (let* ((len (string-length str))
	  (nlen (8bits->utf8-length str len table)))
      (if (=fx len nlen)
	  str
	  (8bits->utf8-fill! ($make-string/wo-fill nlen) str len table))))

;*---------------------------------------------------------------------*/
;*    iso-latin->utf8 ...                                              */
;*---------------------------------------------------------------------*/
(define (iso-latin->utf8 str)
   (8bits->utf8 str #f))
   
;*---------------------------------------------------------------------*/
;*    iso-latin->utf8! ...                                             */
;*---------------------------------------------------------------------*/
(define (iso-latin->utf8! str)
   (8bits->utf8! str #f))

;*---------------------------------------------------------------------*/
;*    cp1252->utf8 ...                                                 */
;*---------------------------------------------------------------------*/
(define (cp1252->utf8 str)
   (8bits->utf8 str cp1252))
   
;*---------------------------------------------------------------------*/
;*    cp1252->utf8! ...                                                */
;*---------------------------------------------------------------------*/
(define (cp1252->utf8! str)
   (8bits->utf8! str cp1252))
