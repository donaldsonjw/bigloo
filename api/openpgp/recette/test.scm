(module test
	(main main))


(define-macro (debug . str)
   (when (>fx (bigloo-compiler-debug) 0)
      `(tprint ,@str)))

(define-macro (+fx+ x . L)
   (cond
      ((null? L) x)
      (else `(+fx ,x (+fx+ ,@L)))))

(define-macro (-fx- x . L)
   `(-fx ,x (+fx+ ,@L)))

(define (bignum-bit-length::long b::bignum)
   (let loop ((b b)
	      (res 0))
      (let ((divided (/bx b #z256)))
	 (cond
	    ((zerobx? b) res)
	    ((zerobx? divided) ;; this is the last octet
	     (let ((x (bignum->fixnum b)))
		(cond
		   ((<fx x #x02) (+fx res 1))
		   ((<fx x #x04) (+fx res 2))
		   ((<fx x #x08) (+fx res 3))
		   ((<fx x #x10) (+fx res 4))
		   ((<fx x #x20) (+fx res 5))
		   ((<fx x #x40) (+fx res 6))
		   ((<fx x #x80) (+fx res 7))
		   (else (+fx res 8)))))
	    (else
	     (loop divided (+fx res 8)))))))

(define-inline (/ceilingfx x y)
   (let ((q (quotientfx x y))
	 (r (remainderfx x y)))
      (cond
	 ((zerofx? r) q)
	 ((>fx r 0)   (+fx q 1))
	 (else        (-fx q 1)))))

(define-inline (bignum->char::char b::bignum)
   (when (not (<bx b #z256))
      (error "bignum->char" "bignum must be < 256" b))
   (integer->char-ur (bignum->fixnum b)))
(define-inline (char->bignum::bignum c::char)
   (fixnum->bignum (char->integer c)))
(define (bin-str->bignum::bignum str::bstring)
   (let loop ((i 0)
	      (res #z0))
      (if (=fx i (string-length str))
	  res
	  (loop (+fx i 1)
		(+bx (*bx res #z256)
		     (fixnum->bignum (char->integer (string-ref str i))))))))
(define (bignum->bin-str::bstring b::bignum #!optional (len::long -1))
   (let* ((len (if (=fx len -1)
		   (/ceilingfx (bignum-bit-length b) 8)
		   len))
	  (str (make-string len)))
      (bignum->bin-str! str 0 b len)
      str))
(define (bignum->bin-str! buffer::bstring at::long x::bignum #!optional
			  (len::long -1))
   (define (last-char-digit::char x::bignum)
      (integer->char-ur (bignum->fixnum (remainderbx x #z256))))

   (let ((len (if (=fx len -1)
		  (/ceilingfx (bignum-bit-length x) 8)
		  len)))
      (let loop ((x x)
		 (i (-fx len 1)))
	 (cond
	    ((and (<fx i 0)
		  (zerobx? x))
	     buffer)
	    ((<fx i 0)
	     (error "bignum->bin-str!" "integer too large" x))
	    (else
	     (string-set! buffer (+fx at i) (last-char-digit x))
	     (loop (/bx x #z256) (-fx i 1)))))))

(define (make-random-string len::long #!key (show-trace #f))
   ;; TODO: currently make-random-string falls back to bigloo-random (with a
   ;; warning, but still). try to get more random-bytes from /dev/random, and
   ;; make fallback optional.
   ;; Furthermore: /dev/urandom is faster, but probably still safe.
   (define (make-bglrandom-string len)
      (let ((res (make-string len)))
	 (let loop ((i 0))
	    (cond
	       ((>=fx i len)
		res)
	       (else
		(string-set! res i (integer->char-ur (random 256)))
		(loop (+fx i 1)))))))

   (if (file-exists? "/dev/urandom")
       (let ((p (open-input-file "/dev/urandom")))
	  (if (input-port? p)
	      (unwind-protect
		 (let ((str (read-chars len p)))
		    (if (and (string? str)
			     (=fx len (string-length str)))
			str
			(begin
			   (warning "/dev/random did not work")
			   (make-bglrandom-string len))))
		 (close-input-port p))
	      (make-bglrandom-string len)))
       (make-bglrandom-string len)))

;; length is in bit.
(define (make-random-bignum len::long #!key (show-trace #f))
   (if (=fx len 0)
       #z0
       (let* ((str-len (/fx (+fx len 7) 8))
	      (str (make-random-string str-len))
	      (last-bits (remainder len 8))
	      (mask (case last-bits
		       ((0) #xFF) ((7) #x7F) ((6) #x3F)
		       ((5) #x1F) ((4) #x0F) ((3) #x07)
		       ((2) #x03) ((1) #x01)))
	      (first-char (string-ref str 0))
	      (first-char-n (char->integer first-char))
	      (masked (integer->char-ur (bit-and mask first-char-n))))
	  (string-set! str 0 masked)
	  (let loop ((i 0)
		     (res #z0))
	     (if (>=fx i str-len)
		 res
		 (loop (+fx i 1)
		       (+bx (*bx #z256 res)
			    (char->bignum (string-ref str i)))))))))

(define-inline (char-xor::char c1::char c2::char)
   (integer->char-ur (bit-xor (char->integer c1) (char->integer c2))))

(define (string-xor::bstring str1::bstring str2::bstring)
   (let ((len (string-length str1)))
      (when (not (=fx len (string-length str2)))
	 (error "string-xor" "strings don't have same length" str2))
      (let ((res (make-string len)))
	 (let loop ((i 0))
	    (cond
	       ((>=fx i len)
		res)
	       (else
		(string-set! res i (char-xor (string-ref str1 i)
					     (string-ref str2 i)))
		(loop (+fx i 1))))))))

;; str1 = str1^str2
(define-inline (string-xor! str1 str2 len)
   (string-xor-buffer! str1 0 str1 0 str2 0 len))
(define-inline (string-xor-buffer! target target-pos str1 str1-pos str2
				   str2-pos len)
   (let loop ((i 0))
      (if (=fx i len)
	  target
	  (begin
	     (string-set! target (+fx target-pos i)
			  (char-xor (string-ref str1 (+fx str1-pos i))
				    (string-ref str2 (+fx str2-pos i))))
	     (loop (+fx i 1))))))

(define (read-armored-base64-data p)
   (let ((data (let loop ((str ""))
		  (let ((l (read-line p)))
		     (cond
			((eof-object? l) str)
			((string-prefix? "--" l) str)
			(else (loop (string-append str l))))))))
      (base64-decode data)))

(define-inline (str->hex-string::bstring str::bstring)
   (string-hex-extern str))

(define-inline (hex-str->string::bstring hex-str::bstring)
   (string-hex-intern hex-str))
(define-inline (hex-str->string!::bstring hex-str::bstring)
   (string-hex-intern! hex-str))

(define (gcd-ext x y)
   (let loop ((x x)
	      (y y)
	      (u1 #z1)
	      (u2 #z0)
	      (v1 #z0)
	      (v2 #z1))
      (if (zerobx? y)
	  (list x u1 v1)
	  (let ((q (quotientbx x y))
		(r (remainderbx x y)))
	     (loop y r u2 (-bx u1 (*bx q u2)) v2 (-bx v1 (*bx q v2)))))))

(define (mod-inverse x b)
   (let* ((x1 (modulobx x b))
	  (g (gcd-ext x1 b)))
      (if (not (=bx (car g) #z1))
	  (error 'mod-inverse
		 "internal error, numbers are not relatively prime"
		 (cons x b))
	  (modulobx (cadr g) b))))

(define (expt-modbx x y m)
   
   (define (expt-mod n e m)
      (cond
	 ((zerobx? e)
	  #z1)
	 ((evenbx? e)
	  (expt-mod (modulobx (*bx n n) m) (quotientbx e #z2) m))
	 (else
	  (modulobx (*bx n (expt-mod n (-bx e #z1) m)) m))))
   
   (expt-mod x y m))

(define (sha1sum-bin::bstring in)
   (hex-str->string! (sha1sum in)))

(define (sha256sum-bin::bstring in)
   (hex-str->string! (sha256sum in)))

(define (md5sum-bin::bstring in)
   (hex-str->string! (md5sum in)))

;*---------------------------------------------------------------------*/
;*    random-prime ...                                                 */
;*---------------------------------------------------------------------*/
(define (make-random-prime start::bignum end::bignum #!key (show-trace #f))
   ;; TODO: make-random-prime does not use real random-port.			   
   (define (product-of-primes n)
      (let loop ((n (-fx n 1)) (p #z2) (i 3))
	 (cond
	    ((=fx n 0)
	     p)
	    ((=bx #z1 (gcdbx (fixnum->bignum i) p))
	     (loop (-fx n 1) (*bx p (fixnum->bignum i)) (+fx i 2)))
	    (else
	     (loop n p (+fx i 2))))))
   
   (when show-trace
      (display ".")
      (flush-output-port (current-output-port)))
   
   (let ((prod-small-primes (product-of-primes 300)))
      
      (define (likely-prime? n)
	 (and (=bx #z1 (gcdbx n prod-small-primes))
	      (=bx #z1 (expt-modbx #z2 (-bx n #z1) n))))
      
      (let loop ((i 1))
	 (when show-trace
	    (display "+")
	    (flush-output-port (current-output-port)))
	 (let* ((x (+bx start (randombx (-bx end start))))
		(n (if (oddbx? x) x (+bx x #z1))))
	    (if (or (>=bx n end) (not (likely-prime? n)))
		(loop (+ i 1))
		n)))))



#;(do ((i #z0 (+bx i #z1)))
    ((=bx i #z256))
    (let* ((bc (bignum->char i))
	   (cb (char->bignum bc)))
       (printf "bignum: ~a char: ~a eq: ~a~%"
	  i bc (=bx i cb))))

#;(do ((i #z0 (+bx i #z1))
     (val #z1 (*bx val #z2)))
    ((=bx i #z2048))
  (let* ((bs (bignum->bin-str val))
	 (sb (bin-str->bignum bs))
	 (bs1 (bignum->bin-str (+bx val #z1)))
	 (sb1 (bin-str->bignum bs1)))
    (when (not (=bx sb val))
	  (printf "error in converting val: ~a~%" val))
    (when (not (=bx sb1 (+bx val #z1)))
	  (printf "error in converting val: ~a~%" val))))



#;(do ((i #z0 (+bx i #z1))
     (val #z1 (*bx val #z2)))
    ((=bx i #z2048))
  (let* ((bs (bignum->bin-str val))
	 (sh (str->hex-string bs))
	 (bs1 (bignum->bin-str (+bx val #z1)))
	 (sh1 (str->hex-string bs1)))
    (when (not (string=? (hex-str->string sh) bs))
	  (printf "error in converting val: ~a~%" val))
    (when (not (string=? (hex-str->string sh1) bs1))
	  (printf "error in converting val: ~a~%" val))))


(let ((test-str (make-string 256)))
  
  (do ((i 0 (+ i 1)))
      ((= i 256))
    (string-set! test-str i (integer->char i)))
  
  (do (( i 0 (+ i 1)))
      ((= i 256))
    (printf "ch ~a to uint32 ~a~%" (string-ref test-str i) (fixnum->uint32 (char->integer (string-ref test-str i))))))


(define t (make-date :sec 54 :min 57 :hour 1 :day 12 :month 11 :year 2009 :timezone (* -7 3600) ))

(define (main args)
  (print t)
  (print (date->seconds t))
  (print (seconds->date (date->seconds t))))
 
#;(for-each (lambda (x)
	    (let* ((hs (str->hex-string (bignum->bin-str x)))
		   (hs-len (string-length hs)))
	      (print "last-8: " (if (> hs-len 8)
				    (substring hs (-fx hs-len 8) hs-len)
				    hs-len))	   
	    (print (string=? (hex-str->string hs)
			     (bignum->bin-str x)))
	    (newline)))
	'(#z126771391819999701026445440529156671024011294783185608595196344040409804172686106579939252201334924984097125624785782136153240556393504204888566258646597041550350387146108116912342329506630382481012149248992912191357766154045438221855544428606768656935115351771054110483593475909449758093040683000428302510907
	  #z1410979250075607633295418248112605277159948731297
	  #z31455833241606798896226841675509266289520344481158184958444539841801112132381533588069622269766346467461200777567201925541846666206839784635415195169884207912865398433398871787784686013849043101188846689828923440439071342727443562418854072879261797869206644039758323132924590353157433186829454212594168795306
	  #z44847538663660346120260255291409664831319526150163162805482495202739436239505932970113792220590040011276507025113442994299795801651540804239451931808118432458592221740144903242565894622225672379217733462137122460770397355789922813384960960005902300644059262452547402330508379141728723973638573838256981570477
	  #z28327403645311261513929228671048544604589259994746432600642753470322763033641497777833534888783202366232793026675009838824391402951664303762858639959780756413434518394635542539867057168316990728623778021329954199001664705068148068284405517457102420854841845627005177670669521306554107943626341288046766014935512419320599243552972082381328661184593979311342930745683163334942861612235567334696880741468646798429873601121898810185826320814155375347906660415434170114940870657492451142442415407600586900192228125000136100412597121162925349118885571239388214861306173088636329462275949419637470390081415641001122529796363
	  #z5
	  #z18751297129046268197124124829694760708179757390903740010556634828620171234854777631804021489752123303694159008288670087175290909531791336662597715357839703253176623293184859163135868092053667639739795567636113018737705697022993158703623199890189274027803669466692993368274076809607323857309530769381804978233134198456254794620441796348382488080499981569626239910855798803177160905735706351972928230670605903011513150090800275281070154329466852972711418255009577694437297000232608741456747426892895454908906069966025910056751816674186169826711724735993557492844293316722633207104903231875548262320975748962601478509664
	  ))
