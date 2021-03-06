;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/bmem/bmem/param.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 25 09:24:44 2003                          */
;*    Last change :  Wed Oct 24 10:16:49 2012 (serrano)                */
;*    Copyright   :  2003-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Bmem parameters                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bmem_param
   (export *bmem*
	   *bmem-version*

	   *fout*
	   *file*
	   *function*
	   *type*

	   *hostname*
	   *port*
	   *uri*

	   *css-path*
	   *sizeof-word*))

;*---------------------------------------------------------------------*/
;*    Global parameters                                                */
;*---------------------------------------------------------------------*/
(define *bmem* "bglmem")
(define *bmem-version* "0.4")

;*---------------------------------------------------------------------*/
;*    IOs                                                              */
;*---------------------------------------------------------------------*/
(define *fout* #f)
(define *file* #f)
(define *function* #f)
(define *type* #f)

;*---------------------------------------------------------------------*/
;*    HTTP connection                                                  */
;*---------------------------------------------------------------------*/
(define *hostname* #f)
(define *port* #f)
(define *uri* #f)

;*---------------------------------------------------------------------*/
;*    CSS path                                                         */
;*---------------------------------------------------------------------*/
(define *css-path-variables* `("HTTP_HOP_CONFIG" "HTTP_HOP_ETC"))
(define *css-path* (filter string? (map getenv *css-path-variables*)))

;*---------------------------------------------------------------------*/
;*    Word size                                                        */
;*    -------------------------------------------------------------    */
;*    This value is overriden when the .bmem file is read.             */
;*---------------------------------------------------------------------*/
(define *sizeof-word* #l4)

