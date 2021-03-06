;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/number.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov 28 10:52:56 1992                          */
;*    Last change :  Sun Jun 15 09:19:39 2014 (serrano)                */
;*                                                                     */
;*    On test les operateurs generiques sur les nombres                */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module number
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-number)))

;*---------------------------------------------------------------------*/
;*    llong constant ...                                               */
;*---------------------------------------------------------------------*/
(define (foo-ok::llong x::llong)
  (let ((bar #l268435456))
    (quotientllong x bar)))

(define (foo-ko::llong x::llong)
  (quotientllong x #l268435456))

;*---------------------------------------------------------------------*/
;*    test-number ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-number)
   (test-module "number" "number.scm")
   (test "=.1" (= 1 1) #t)
   (test "=.2" (= 1 1 1) #t)
   (test "=.3" (= 1 2 1) #f)
   (test "=.4" (= 1.0 1.0 1.0) #t)
   (test "=.5" (= 1.0 2.0 1.0) #f)
   (test "=.6" (= 1.0 1) #t)
   (test "=.7" (= 1 1.0) #t)
   (test "=.8" (= 1.1 1) #f)
   (test "=.9" (= 1 1.1) #f)
   (test "=.10" (= 1 #e1) #t)
   (test "=.11" (= 1 #e2) #f)
   (test "=.12" (= #e1 1) #t)
   (test "=.13" (= #e2 3) #f)
   (test "=.14" (= #e2 #e2 #e2) #t)
   (test "=.15" (= #e2 #e2 #e3) #f)
   (test "=.16" (= 1 #l1) #t)
   (test "=.17" (= 1 #l2) #f)
   (test "=.18" (= #l1 1) #t)
   (test "=.19" (= #l2 3) #f)
   (test "=.20" (= #l2 #l2 #l2) #t)
   (test "=.21" (= #l2 #l2 #l3) #f)
   (test "=.22" (= 1. #e1) #t) 
   (test "=.23" (= 1. #e2) #f)
   (test "=.24" (= #e1 1.) #t)
   (test "=.25" (= #e2 3.) #f)
   (test "=.26" (= 1. #l1) #t)
   (test "=.27" (= 1. #l2) #f)
   (test "=.28" (= #l1 1.) #t)
   (test "=.29" (= #l2 3.) #f)
   (test "=.30" (= #l2 #s64:2) #t)
   (test "=.31" (= #l2 #u64:2) #t)
   (test "=.32" (= #l2 #s32:2) #t)
   (test "=.33" (= #l2 #u32:2) #t)
   (test "=.34" (= #l2 #s16:2) #t)
   (test "=.35" (= #l2 #u16:2) #t)
   (test "=.36" (= #l2 #s8:2) #t)
   (test "=.37" (= #l2 #u8:2) #t)
   (test "=.38" (= #s64:2 #l2) #t)
   (test "=.39" (= #u64:2 #l2) #t)
   (test "=.40" (= #s32:2 #l2) #t)
   (test "=.41" (= #u32:2 #l2) #t)
   (test "=.42" (= #s16:2 #l2) #t)
   (test "=.43" (= #u16:2 #l2) #t)
   (test "=.44" (= #s8:2 #l2) #t)
   (test "=.45" (= #u8:2 #l2) #t)
   (test "<.1" (< 1 2) #t)
   (test "<.2" (< 1 1) #f) 
   (test "<.3" (< 2 1) #f)
   (test "<.4" (< 1 2 3) #t)
   (test "<.5" (< 1 2 1 3) #f)
   (test "<.6" (<elong #e1 #e2) #t)
   (test "<.7" (<elong #e2 #e1) #f) 
   (test "<.8" (< 1 #e2) #t) 
   (test "<.9" (< 10 #e2) #f) 
   (test "<.10" (<llong #l1 #l2) #t)
   (test "<.11" (<llong #l2 #l1) #f) 
   (test "<.12" (< 1 #l2) #t) 
   (test "<.13" (< 10 #l2) #f) 
   (test ">.1" (> 1 2) #f)
   (test ">.2" (> 1 1) #f)
   (test ">.3" (> 2 1) #t)
   (test ">.4" (> 3 2 1) #t)
   (test ">.5" (> 3 1 2 1) #f)
   (test ">.6" (> #e3 #e1) #t)
   (test ">.7" (> #e3 #e11) #f)
   (test ">.8" (> #l3 #e1) #t)
   (test ">.9" (> #l3 #e11) #f)
   (test ">=.1" (>= 1 2) #f)
   (test ">=.2" (>= 1 1) #t)
   (test ">=.3" (>= 2 1) #t)
   (test ">=.4" (>= 3 2 1) #t)
   (test ">=.5" (>= 3 1 2 1) #f)
   (test ">=.6" (>= #e1 #l2) #f)
   (test ">=.7" (>= #l1 #l1) #t)
   (test ">=.8" (>= #l2 #e1) #t)
   (test "<=.1" (<= 1 2) #t)
   (test "<=.2" (<= 1 1) #t)
   (test "<=.3" (<= 2 1) #f)
   (test "<=.4" (<= 1 2 3) #t)
   (test "<=.5" (<= 1 2 1 3) #f)
   (test "<=.6" (<= #l1 #e2) #t)
   (test "<=.7" (<= #e1 #e1) #t)
   (test "<=.8" (<= #l2 #l1) #f)
   (test "+.1" (+ 1 2 3 4) 10)
   (test "+.2" (+ #e1 #e2) #e3)
   (test "+.3" (+ #l1 #e2) #l3)
   (test "+.4" (+ 1 2 3) 6)
   (test "+.5" (let ((x 1)) (+ x 2 3 4)) 10)
   (test "-.1" (- 1 2 3 4) -8)
   (test "-.2" (let ((x 4.0)) (- x)) -4.0)
   (test "-.3" (- #l2 #e1) #l1)
   (test "-.4" (- 1 2 3 4) -8)
   (test "-.5" (let ((x 1)) (- x 2 3 4)) -8)
   (test "*.1" (* 1 2 3 4) 24)
   (test "*.2" (* #l2 #e2) #l4)
   (test "*.3" (* (car (list 0)) 0) 0)
   (test "/.1" (/ 12 3 2 1) 2)
   (test "/.2" (/ (/ 4)) 4.0)
   (test "/.3" (/ 4. #l2) 2.0)
   (test "/.4" (/ 4. #e2) 2.0)
   (test "/.5" (/ 1 #e2) 0.5)
   (test "/.6" (/ 1 #l2) 0.5)
   (test "/.7" (/ #e1 #e2) 0.5)
   (test "/.8" (/ #e1 #l2) 0.5)
   (test "/.9" (/ #l1 #e2) 0.5)
   (test "/.10" (/ #l1 #l2) 0.5)
   (test "/.11" (/ 1. #l2) 0.5)
   (test "/.12" (/ #l1 2.) 0.5)
   (test "/.13" (/ 1. #e2) 0.5)
   (test "/.14" (/ #e1 2.) 0.5)
   (test "/fx.1" (/fx 5 4) 1)
   (test "/fx.2" (integer? (/fx 5 4)) #t)
   (test "zero?.1" (zero? 3) #f)
   (test "zero?.2" (zero? 0) #t)
   (test "zero?.3" (zero? 3.0) #f)
   (test "zero?.4" (zero? 0.0) #t)
   (test "zero?.5" (zero? #e0) #t)
   (test "zero?.6" (zero? #l0) #t)
   (test "positive?.1" (positive? 5.0) #t)
   (test "positive?.2" (positive? -5.0) #f)
   (test "positive?.3" (positive? 5) #t)
   (test "positive?.4" (positive? -5) #f)
   (test "positive?.5" (positive? #e-5) #f)
   (test "positive?.6" (positive? #l-5) #f)
   (test "negative?.1" (negative? 5.0) #f)
   (test "negative?.2" (negative? -5.0) #t)
   (test "negative?.3" (negative? 5) #f)
   (test "negative?.4" (negative? -5) #t)
   (test "negative?.5" (negative? #e-5) #t)
   (test "negative?.6" (negative? #l-5) #t)
   (test "odd?.1" (odd? 4) #f)
   (test "odd?.2" (odd? 3) #t)
   (test "odd?.3" (odd? #e4) #f)
   (test "odd?.4" (odd? #e3) #t)
   (test "odd?.5" (odd? #l4) #f)
   (test "odd?.6" (odd? #l3) #t)
   (test "even?.1" (even? 4) #t)
   (test "even?.2" (even? 3) #f)
   (test "even?.3" (even? #e4) #t)
   (test "even?.4" (even? #e3) #f)
   (test "even?.5" (even? #l4) #t)
   (test "even?.6" (even? #l3) #f)
   (test "quotient.1" (quotient 13 4) 3)
   (test "quotient.1" (quotient #e13 #e4) #e3)
   (test "quotient.1" (quotient #l13 #l4) #l3)
   (test "modulo.1" (modulo 13 4) 1)
   (test "modulo.1" (modulo #e13 #e4) #e1)
   (test "modulo.1" (modulo #l13 #l4) #l1)
   (test "remainder.1" (remainder 13 4) 1)
   (test "remainder.1" (remainder #e13 #e4) #e1)
   (test "remainder.1" (remainder #l13 #l4) #l1)
   (test "quotient.2" (quotient -13 4) -3)
   (test "quotient.2" (quotient #e-13 #e4) #e-3)
   (test "quotient.2" (quotient #l-13 #l4) #l-3)
   (test "modulo.2" (modulo -13 4) 3)
   (test "modulo.2" (modulo #e-13 #e4) #e3)
   (test "modulo.2" (modulo #l-13 #l4) #l3)
   (test "remainder.2" (remainder -13 4) -1)
   (test "remainder.2" (remainder #e-13 #e4) #e-1)
   (test "remainder.2" (remainder #l-13 #l4) #l-1)
   (test "quotient.3" (quotient 13 -4) -3)
   (test "quotient.3" (quotient #e13 #e-4) #e-3)
   (test "quotient.3" (quotient #l13 #l-4) #l-3)
   (test "modulo.3" (modulo 13 -4) -3)
   (test "modulo.3" (modulo #e13 #e-4) #e-3)
   (test "modulo.3" (modulo #l13 #l-4) #l-3)
   (test "remainder.3" (remainder 13 -4) 1)
   (test "remainder.3" (remainder #e13 #e-4) #e1)
   (test "remainder.3" (remainder #l13 #l-4) #l1)
   (test "quotient.4" (quotient -13 -4) 3)
   (test "quotient.4" (quotient #e-13 #e-4) #e3)
   (test "quotient.4" (quotient #l-13 #l-4) #l3)
   (test "modulo.4" (modulo -13 -4) -1)
   (test "modulo.4" (modulo #e-13 #e-4) #e-1)
   (test "modulo.4" (modulo #l-13 #l-4) #l-1)
   (test "remainder.4" (remainder -13 -4) -1)
   (test "remainder.4" (remainder #e-13 #e-4) #e-1)
   (test "remainder.4" (remainder #l-13 #l-4) #l-1)
   (test "gcd" (gcd 32 -36) 4)
   (test "gcd" (gcd) 0)
   (test "gcd" (gcdfx 1071 294 4410 1029 273 -42147 57330) 21)
   (test "gcd" (gcdelong #e1071 #e294 #e4410 #e1029 #e273 #e-42147 #e57330) #e21)
   (test "gcd" (gcdllong #l1071 #l294 #l4410 #l1029 #l273 #l-42147 #l57330) #l21)
   (test "gcd" (gcd 1071 #l294 4410 #e1029 273 #l-42147 #e57330) #l21)
   (test "lcm" (lcm 32 -36) 288)
   (test "lcm" (lcm) 1)
   (test "lcm" (lcmfx 840 168 420 -588 924 8400 2520) 1940400)
   (test "lcm" (lcmelong #e840 #e168 #e420 #e-588 #e924 #e8400 #e2520) #e1940400)
   (test "lcm" (lcmllong #l840 #l168 #l420 #l-588 #l924 #l8400 #l2520) #l1940400)
   (test "lcm" (lcm 840 #l168 420 #e-588 924 #l8400 #e2520) #l1940400)
   (test "flonum->fixnum" (flonum->fixnum (cosfl 0.0)) 1)
   (test "atan" (tan (atan 1)) (fixnum->flonum 1))
   (test "flofix.<" (< 3.0 3) #f)
   (test "flofix.<" (< 3.1 3) #f)
   (test "flofix.<" (< 3.1 4) #t)
   (test "flofix.>" (> 3 3.0) #f)
   (test "flofix.>" (> 3 3.1) #f)
   (test "flofix.>" (> 4 3.1) #t)
   (test "flofix.<=" (<= 3 3.0) #t)
   (test "flofix.<=" (<= 3 3.1) #t)
   (test "flofix.<=" (<= 3 2.0) #f)
   (test "flofix.>=" (>= 3.0 3) #t)
   (test "flofix.>=" (>= 3.1 3) #t)
   (test "flofix.>=" (>= 3.1 4) #f)
   (let ((x 3.0) (y 3))
      (test "flofix.<" (< x y) #f))
   (let ((x 3.1) (y 3))
      (test "flofix.<" (< x y) #f))
   (let ((x 3.1) (y 4))
      (test "flofix.<" (< x y) #t))
   (let ((x 3) (y 3.0))
      (test "flofix.>" (> x y) #f))
   (let ((x 3) (y 3.1))
      (test "flofix.>" (> x y) #f))
   (let ((x 4) (y 3.1))
      (test "flofix.>" (> x y) #t))
   (let ((x 3) (y 3.0))
      (test "flofix.<=" (<= x y) #t))
   (let ((x 3) (y 3.1))
      (test "flofix.<=" (<= x y) #t))
   (let ((x 3) (y 2.0))
      (test "flofix.<=" (<= x y) #f))
   (let ((x 3.0) (y 3))
      (test "flofix.>=" (>= x y) #t))
   (let ((x 3.1) (y 3))
      (test "flofix.>=" (>= 3.1 3) #t))
   (let ((x 3.1) (y 4))
      (test "flofix.>=" (>= 3.1 4) #f))
   (let ((x -0.2) (y 0))
      (test "flofix.<" (< x y) #t))
   (test "string->number.1" (string->number "0") 0)
   (test "string->number.2" (string->number "e") #f)
   (test "string->number.3" (string->number "0" 2) 0)
   (test "string->number.4" (string->number "0" 8) 0)
   (test "string->number.5" (string->number "0" 16) 0)
   (test "string->number.6" (string->number "") #f)
   (test "string->number.7" (string->number "7F" 16) 127)
   (test "string->number.8" (string->number "314t") #f)
   (test "string->number.9" (string->number #"5") 5)
   (test "string->number.10" (string->number #"-5") -5)
   (test "string->number.11" (string->number #"+5") 5)
   (test "string->number.12" (string->number #"5\n") #f)
   (test "string->number.13" (string->number #"-5\n") #f)
   (test "string->number.14" (string->number #"+5\n") #f)
   (test "string->number.15" (string->number "-") #f)
   (test "string->number.16" (string->number "-1") -1)
   (test "string->number.17" (string->number "+") #f)
   (test "string->number.18" (string->number "+1") 1)
   (test "string->number.10" (string->number "ABCDEF" 16) #xABCDEF)
   (test "string->number.11" (string->number "E3") #f)
   (test "number->string" (number->string 0 2) "0")
   (test "number->string" (number->string 0 8) "0")
   (test "number->string" (number->string 0 10) "0")
   (test "number->string" (number->string 0 16) "0")
   (test "number->string" (number->string 7 2) "111")
   (test "number->string" (number->string -7 2) "-111")
   (test "number->string" (number->string 8 2) "1000")
   (test "number->string" (number->string -8 2) "-1000")
   (test "number->string" (number->string 9 2) "1001")
   (test "number->string" (number->string -9 2) "-1001")
   (test "number->string" (number->string 127 16) "7f")
   (test "number->string" (number->string -127 16) "-7f")
   (test "number->string" (number->string 127 8) "177")
   (test "number->string" (number->string -127 8) "-177")
   (test "elong.1" (find-runtime-type #e1) "elong")
   (test "elong.2" #e1 #e1)
   (test "elong.3" (elong? #exfeedabee) #t)
   (test "elong?.1" (elong? #e1) #t) 
   (test "elong?.2" (elong? #l1) #f)
   (test "elong->string" (elong->string #e1) "1")
   (test "string->elong" (string->elong "1") #e1)
   (test "elong->fixnum" (elong->fixnum #e123456) 123456)
   (test "fixnum->elong" (fixnum->elong 123456) #e123456)
   (test "elong->flonum" (elong->flonum #e123456) 123456.)
   (test "flonum->elong" (flonum->elong 123456.) #e123456)
   (test "llong.1" (find-runtime-type #l1) "llong")
   (test "llong.2" #l1 #l1)
   (test "llong.3"  (> #lxfeedabee #lxfeedabe0) #t)
   (test "llong?.1" (llong? #l1) #t)
   (test "llong?.2" (llong? #e1) #f)
   (test "llong->string" (llong->string #l1) "1")
   (test "string->llong" (string->llong "1") #l1)
   (test "llong->fixnum" (llong->fixnum #l123456) 123456)
   (test "fixnum->llong" (fixnum->llong 123456) #l123456)
   (test "llong->flonum" (llong->flonum #l123456) 123456.)
   (test "flonum->llong" (flonum->llong 123456.) #l123456)
   (test "integer?.1" (integer? 1) #t)
   (test "integer?.2" (integer? #e1) #t)
   (test "integer?.3" (integer? #l1) #t)
   (test "integer?.4" (integer? 1.) #t)
   (test "integer?.5" (integer? 1.1) #f)
   (test "fixnum?.1" (fixnum? 1) #t)
   (test "fixnum?.2" (fixnum? #e1) #f)
   (test "fixnum?.3" (fixnum? #l1) #f)
   (test "fixnum?.4" (fixnum? 1.) #f)
   (test "fixnum?.5" (fixnum? 1.1) #f)
   (test "fixnum?.6" (fixnum? (- (car (list 1)) 1)) #t)
   (test "fixnum?.7" (fixnum? (+ (car (list -1)) 1)) #t)
   (test "Infinity" (flonum? (/fl 1.0 0.0)) #t)
   (test "bit-or" (bit-or 1 2) 3)
   (test "bit-orelong" (bit-orelong #e1 #e2) #e3)
   (test "bit-orllong" (bit-orllong #l1 #l2) #l3)
   (test "bit-and" (bit-and 1 3) 1)
   (test "bit-andelong" (bit-andelong #e1 #e3) #e1)
   (test "bit-andllong" (bit-andllong #l1 #l3) #l1)
   (test "bit-lsh" (bit-lsh 1 3) 8)
   (test "bit-lshelong" (bit-lshelong #e1 3) #e8)
   (test "bit-lshllong" (bit-lshllong #l1 3) #l8)
   (test "bit-rsh" (bit-rsh 8 3) 1)
   (test "bit-ursh" (bit-ursh 8 3) 1)
   (test "bit-rshelong" (bit-rshelong #e8 3) #e1)
   (test "bit-rshllong" (bit-rshllong #l8 3) #l1)
   (test "bit-urshelong" (bit-urshelong #e2 1) #e1)
   (test "bit-urshllong" (bit-urshllong #l2 1) #l1)
   (test "abs.1" (= (abs 0) 0) #t)
   (test "abs.2" (= (abs 1) 1) #t)
   (test "abs.3" (= (abs -1) 1) #t)
   (test "abs.4" (= (abs -2147483) 2147483) #t)
;*    (test "abs.5" (= (abs -2147483648) 2147483648) #t)               */
   (test "abs.6" (= (abs 0.0) 0) #t)
   (test "abs.7" (= (abs 1.1) 1.1) #t)
   (test "abs.8" (= (abs -1.1) 1.1) #t)
   (test "eabs.1" (= (abs #e0) #e0) #t)
   (test "eabs.2" (= (abs #e1) #e1) #t)
   (test "eabs.3" (= (abs #e-1) #e1) #t)
   (test "eabs.4" (= (abs (- #e2147483648)) #e2147483648) #t)
   (test "eabs.5" (= (abs (- #e9223372036854775808)) #e9223372036854775808) #t)
   (test "labs.1" (= (abs #l0) #l0) #t)
   (test "labs.2" (= (abs #l1) #l1) #t)
   (test "labs.3" (= (abs #l-1) #l1) #t)
   (test "labs.4" (= (abs #l-2147483648) #l2147483648) #t)
   (test "negllong.1" (negllong #l-2147483648) #l2147483648)
   (test "negllong.2" (negllong #l-214748364) #l214748364)
   (test "negllong.3" (negllong #l123456) #l-123456)
   (test "negllong.4" (negllong #l214748364) #l-214748364)
   (test "negllong.5" (negllong #l2147483648) #l-2147483648)
   (test "expt.1" (expt 2 -1) 0.5)
   (test "expt.2" (expt 2 -1.0) 0.5)
   (test "expt.3" (expt -1 256) 1)
   (test "expt.4" (expt -1 255) -1)
   (test "expt.5" (eval '(expt -1 256)) 1)
   (test "expt.6" (eval '(expt -1 255)) -1)
   (test "max.1" (max 1) 1)
   (test "max.2" (max 1 2) 2)
   (test "max.3" (max 2 1) 2)
   (test "max.4" (max 2 1 0) 2)
   (test "max.5" (max 2 3 0) 3)
   (test "max.6" (max 2 3 4) 4)
   (test "max.7" (max 3 4) 4)
   (test "max.8" (max 3.9 4) 4.)
   (test "max.9" (max 3.9 4.) 4.)
   (test "min.1" (min 1) 1)
   (test "min.2" (min 1 2) 1)
   (test "min.3" (min 2 1) 1)
   (test "min.4" (min 2 1 0) 0)
   (test "min.5" (min 2 3 0) 0)
   (test "min.6" (min 2 3 4) 2)
   (test "min.7" (min 3 4) 3) 
   (test "min.8" (min 3.9 4) 3.9)
   (let ((un 1) (deux 2.0) (trois 3) (quatre 4.0))
      (test "maxb.1" (max un) 1)
      (test "maxb.2" (max un deux) deux)
      (test "maxb.3" (max deux un) deux)
      (test "maxb.4" (max deux un 0) deux)
      (test "maxb.4" (max deux trois 0) 3.)
      (test "maxb.4" (max deux trois quatre) quatre)
      (test "minb.1" (min un) 1)
      (test "minb.2" (min un deux) 1.)
      (test "minb.3" (min deux un) 1.)
      (test "minb.4" (min deux un 0) 0.)
      (test "minb.4" (min deux trois 0) 0.)
      (test "minb.4" (min deux trois quatre) deux))
   (test "maxfx.1" (maxfx 1) 1)
   (test "maxfx.2" (maxfx 1 2) 2)
   (test "maxfx.3" (maxfx 2 1) 2)
   (test "maxfx.4" (maxfx 2 1 0) 2)
   (test "maxfx.4" (maxfx 2 3 0) 3)
   (test "maxfx.4" (maxfx 2 3 4) 4)
   (test "minfx.1" (minfx 1) 1)
   (test "minfx.2" (minfx 1 2) 1)
   (test "minfx.3" (minfx 2 1) 1)
   (test "minfx.4" (minfx 2 1 0) 0)
   (test "minfx.4" (minfx 2 3 0) 0)
   (test "minfx.4" (minfx 2 3 4) 2)
   (test "maxelong.1" (maxelong #e1) #e1)
   (test "maxelong.2" (maxelong #e1 #e2) #e2)
   (test "maxelong.3" (maxelong #e2 #e1) #e2)
   (test "maxelong.4" (maxelong #e2 #e1 #e0) #e2)
   (test "maxelong.4" (maxelong #e2 #e3 #e0) #e3)
   (test "maxelong.4" (maxelong #e2 #e3 #e4) #e4)
   (test "minelong.1" (minelong #e1) #e1)
   (test "minelong.2" (minelong #e1 #e2) #e1)
   (test "minelong.3" (minelong #e2 #e1) #e1)
   (test "minelong.4" (minelong #e2 #e1 #e0) #e0)
   (test "minelong.4" (minelong #e2 #e3 #e0) #e0)
   (test "minelong.4" (minelong #e2 #e3 #e4) #e2)
   (test "maxllong.1" (maxllong #l1) #l1)
   (test "maxllong.2" (maxllong #l1 #l2) #l2)
   (test "maxllong.3" (maxllong #l2 #l1) #l2)
   (test "maxllong.4" (maxllong #l2 #l1 #l0) #l2)
   (test "maxllong.4" (maxllong #l2 #l3 #l0) #l3)
   (test "maxllong.4" (maxllong #l2 #l3 #l4) #l4)
   (test "minllong.1" (minllong #l1) #l1)
   (test "minllong.2" (minllong #l1 #l2) #l1)
   (test "minllong.3" (minllong #l2 #l1) #l1)
   (test "minllong.4" (minllong #l2 #l1 #l0) #l0)
   (test "minllong.4" (minllong #l2 #l3 #l0) #l0)
   (test "minllong.4" (minllong #l2 #l3 #l4) #l2)
   (let ((un 1) (deux 2) (trois 3) (quatre 4))
      (test "maxfxb.1" (maxfx un) 1)
      (test "maxfxb.2" (maxfx un deux) deux)
      (test "maxfxb.3" (maxfx deux un) deux)
      (test "maxfxb.4" (maxfx deux un 0) deux)
      (test "maxfxb.4" (maxfx deux trois 0) 3)
      (test "maxfxb.4" (maxfx deux trois quatre) quatre)
      (test "minbfx.1" (minfx un) 1)
      (test "minbfx.2" (minfx un deux) 1)
      (test "minbfx.3" (minfx deux un) 1)
      (test "minbfx.4" (minfx deux un 0) 0)
      (test "minbfx.4" (minfx deux trois 0) 0)
      (test "minbfx.4" (minfx deux trois quatre) deux))
   (let ((s (real->string 0.0)))
      (with-input-from-string s
	 (lambda ()
	    (let ((v (read)))
	       (test "read.1" (real? v) #t)
	       (test "read.2" (and (>fl v -0.001) (<fl v 0.001)) #t)))))
   (test "floor" (floor -4.3) -5.0)
   (test "floor.2" (floor 3.5) 3.0)
   (test "ceiling" (ceiling -4.3) -4.0)
   (test "ceiling.2" (ceiling 3.5) 4.0)
   (test "truncate" (truncate -4.3) -4.0)
   (test "truncate.2" (truncate 3.5) 3.0)
   (test "round" (round -4.3) -4.0)
   (test "round.2" (round 3.5) 4.0)
   (test "round.3" (round 7) 7)
   (test "modulo.1" (modulo 13 4) 1)
   (test "remainder.1" (remainder 13 4) 1)
   (test "modulo.2" (modulo -13 4) 3)
   (test "remainder.2" (remainder -13 4) -1)
   (test "modulo.3" (modulo 13 -4) -3)
   (test "remainder.3" (remainder 13 -4) 1)
   (test "modulo.4" (modulo -13 -4) -1)
   (test "remainder.4" (remainder -13 -4) -1)
   (test "llong constant" (foo-ko #l1) (foo-ok #l1))
   (test "min/maxvalfx" (<fx (minvalfx) (maxvalfx)) #t)
   (test "min/maxvalelong" (<elong (minvalelong) (maxvalelong)) #t)
   (test "min/maxvalllong" (<llong (minvalllong) (maxvalllong)) #t)
   (test "minval.1" (<= (minvalllong) (minvalelong)) #t)
   (test "minval.2" (<= (minvalllong) (minvalfx)) #t)
   (test "maxval.1" (>= (maxvalllong) (maxvalelong)) #t)
   (test "maxval.2" (>= (maxvalllong) (maxvalfx)) #t)
   (test "equal.1" (equal? 1 1) #t)
   (test "equal.2" (equal? 1 2) #f)
   (test "equal.3" (equal? 2 1) #f)
   (test "equal.4" (equal? 1 #e1) #t)
   (test "equal.5" (equal? #e1 #e1) #t)
   (test "equal.6" (equal? #e1 1) #t)
   (test "equal.7" (equal? 1 #l1) #t)
   (test "equal.8" (equal? #l1 #l1) #t)
   (test "equal.9" (equal? #l1 1) #t)
   (test "equal.10" (equal? (car (list 1)) (car (list 1))) #t)
   (test "equal.11" (equal? (car (list 1)) 2) #f)
   (test "equal.12" (equal? 2 (car (list 1))) #f)
   (test "equal.13" (equal? (car (list 1)) (car (list #e1))) #t)
   (test "equal.14" (equal? (car (list #e1)) (car (list #e1))) #t)
   (test "equal.15" (equal? (car (list #e1)) (car (list 1))) #t)
   (test "equal.16" (equal? (car (list 1)) (car (list #l1))) #t)
   (test "equal.17" (equal? (car (list #l1)) (car (list #l1))) #t)
   (test "equal.18" (equal? (car (list #l1)) (car (list 1))) #t)
   (test "equal.19" (equal? (car (list #l1)) (car (list 2))) #f)
   (test "eqv.1" (eqv? 1 1) #t)
   (test "eqv.2" (eqv? 1 2) #f)
   (test "eqv.3" (eqv? 2 1) #f)
   (test "eqv.4" (eqv? 1 #e1) #t)
   (test "eqv.5" (eqv? #e1 #e1) #t)
   (test "eqv.6" (eqv? #e1 1) #t)
   (test "eqv.7" (eqv? 1 #l1) #t)
   (test "eqv.8" (eqv? #l1 #l1) #t)
   (test "eqv.9" (eqv? #l1 1) #t)
   (test "eqv.10" (eqv? (car (list 1)) (car (list 1))) #t)
   (test "eqv.11" (eqv? (car (list 1)) 2) #f)
   (test "eqv.12" (eqv? 2 (car (list 1))) #f)
   (test "eqv.13" (eqv? (car (list 1)) (car (list #e1))) #t)
   (test "eqv.14" (eqv? (car (list #e1)) (car (list #e1))) #t)
   (test "eqv.15" (eqv? (car (list #e1)) (car (list 1))) #t)
   (test "eqv.16" (eqv? (car (list 1)) (car (list #l1))) #t)
   (test "eqv.17" (eqv? (car (list #l1)) (car (list #l1))) #t)
   (test "eqv.18" (eqv? (car (list #l1)) (car (list 1))) #t)
   (test "eqv.19" (eqv? (car (list #l1)) (car (list 2))) #f)
   (test "equal-eval.1" (eval '(equal? 1 1)) #t)
   (test "equal-eval.2" (eval '(equal? 1 2)) #f)
   (test "equal-eval.3" (eval '(equal? 2 1)) #f)
   (test "equal-eval.4" (eval '(equal? 1 #e1)) #t)
   (test "equal-eval.5" (eval '(equal? #e1 #e1)) #t)
   (test "equal-eval.6" (eval '(equal? #e1 1)) #t)
   (test "equal-eval.7" (eval '(equal? 1 #l1)) #t)
   (test "equal-eval.8" (eval '(equal? #l1 #l1)) #t)
   (test "equal-eval.9" (eval '(equal? #l1 1)) #t)
   (test "equal-eval.10" (eval '(equal? (car (list 1)) (car (list 1)))) #t)
   (test "equal-eval.11" (eval '(equal? (car (list 1)) 2)) #f)
   (test "equal-eval.12" (eval '(equal? 2 (car (list 1)))) #f)
   (test "equal-eval.13" (eval '(equal? (car (list 1)) (car (list #e1)))) #t)
   (test "equal-eval.14" (eval '(equal? (car (list #e1)) (car (list #e1)))) #t)
   (test "equal-eval.15" (eval '(equal? (car (list #e1)) (car (list 1)))) #t)
   (test "equal-eval.16" (eval '(equal? (car (list 1)) (car (list #l1)))) #t)
   (test "equal-eval.17" (eval '(equal? (car (list #l1)) (car (list #l1)))) #t)
   (test "equal-eval.18" (eval '(equal? (car (list #l1)) (car (list 1)))) #t)
   (test "equal-eval.19" (eval '(equal? (car (list #l1)) (car (list 2)))) #f)
   (test "eqv-eval.1" (eval '(eqv? 1 1)) #t)
   (test "eqv-eval.2" (eval '(eqv? 1 2)) #f)
   (test "eqv-eval.3" (eval '(eqv? 2 1)) #f)
   (test "eqv-eval.4" (eval '(eqv? 1 #e1)) #t)
   (test "eqv-eval.5" (eval '(eqv? #e1 #e1)) #t)
   (test "eqv-eval.6" (eval '(eqv? #e1 1)) #t)
   (test "eqv-eval.7" (eval '(eqv? 1 #l1)) #t)
   (test "eqv-eval.8" (eval '(eqv? #l1 #l1)) #t)
   (test "eqv-eval.9" (eval '(eqv? #l1 1)) #t)
   (test "eqv-eval.10" (eval '(eqv? (car (list 1)) (car (list 1)))) #t)
   (test "eqv-eval.11" (eval '(eqv? (car (list 1)) 2)) #f)
   (test "eqv-eval.12" (eval '(eqv? 2 (car (list 1)))) #f)
   (test "eqv-eval.13" (eval '(eqv? (car (list 1)) (car (list #e1)))) #t)
   (test "eqv-eval.14" (eval '(eqv? (car (list #e1)) (car (list #e1)))) #t)
   (test "eqv-eval.15" (eval '(eqv? (car (list #e1)) (car (list 1)))) #t)
   (test "eqv-eval.16" (eval '(eqv? (car (list 1)) (car (list #l1)))) #t)
   (test "eqv-eval.17" (eval '(eqv? (car (list #l1)) (car (list #l1)))) #t)
   (test "eqv-eval.18" (eval '(eqv? (car (list #l1)) (car (list 1)))) #t)
   (test "eqv-eval.19" (eval '(eqv? (car (list #l1)) (car (list 2)))) #f)
   (seed-random! 10)
   (let* ((v1 (random 10))
	  (v2 (random 100)))
      (seed-random! 10)
      (let* ((v3 (random 10))
	     (v4 (random 100)))
	 (test "random.1" (and (>=fx v3 0) (<fx v3 10)) #t)
	 (test "random.2" (and (>=fx v4 0) (<fx v4 100)) #t)
	 (test "random.3" (cons v1 v2) (cons v3 v4)))))
	 
   

