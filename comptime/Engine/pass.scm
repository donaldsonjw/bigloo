;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Engine/pass.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 25 10:49:57 1994                          */
;*    Last change :  Fri Nov  5 14:58:38 2004 (serrano)                */
;*    Copyright   :  1994-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The pass tools                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_pass
   (import engine_param
	   init_main)
   (export *current-pass*
	   (stop-on-pass ::symbol ::procedure)))

;*---------------------------------------------------------------------*/
;*    *current-pass* ...                                               */
;*---------------------------------------------------------------------*/
(define *current-pass* '())

;*---------------------------------------------------------------------*/
;*    stop-on-pass ...                                                 */
;*---------------------------------------------------------------------*/
(define (stop-on-pass pass thunk)
   (if (eq? *pass* pass)
       (begin
	  (thunk)
	  (compiler-exit 0))))

