;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/examples/bgst-repl.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 23 08:56:34 2008                          */
;*    Last change :  Wed Jul 23 09:27:24 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    A repl with gstreamer facilities.                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bgst_repl
   (main main)
   (library multimedia pthread gstreamer))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (print "Welcome to the Bigloo+Gstreamer repl...")
   (eval '(library-load 'gstreamer))
   (repl))
