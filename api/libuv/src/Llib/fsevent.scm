;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/fsevent.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:27:21 2014                          */
;*    Last change :  Sat Oct 11 16:37:48 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV fsevent                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_fs-event

   (include "uv.sch")

   (import __libuv_types)
   
   (export (uv-fs-event-start ::UvFsEvent ::procedure ::bstring)
	   (uv-fs-event-stop ::UvFsEvent)
	   (inline uv-fs-event-rename::int)
	   (inline uv-fs-event-change::int)))

;*---------------------------------------------------------------------*/
;*    %uv-init ::UvFsEvent ...                                         */
;*---------------------------------------------------------------------*/
(define-method (%uv-init o::UvFsEvent)
   (with-access::UvFsEvent o ($builtin loop)
      (set! $builtin ($uv-handle-t ($bgl_uv_fs_event_new o loop)))
      o))

;*---------------------------------------------------------------------*/
;*    uv-fs-event-start ...                                            */
;*---------------------------------------------------------------------*/
(define (uv-fs-event-start o::UvFsEvent proc path::bstring)
   (with-access::UvFsEvent o ($builtin loop cb)
      (with-access::UvLoop loop (%mutex %gcmarks)
	 (synchronize %mutex
	    ;; store in the loop for the GC
	    (set! %gcmarks (cons o %gcmarks))
	    ;; force Bigloo to add the extern clause for bgl_uv_fs_event_cb
	    (when (null? %gcmarks)
	       ($bgl_uv_fs_event_cb $uv_fs_event_nil $string-nil 0 0))))
      (if (correct-arity? proc 4)
	  (begin
	     (set! cb proc)
	     ($uv_fs_event_start ($uv-fs-event-t $builtin) $BGL_UV_FS_EVENT_CB
		path 0))
	  (error "uv-fs-event-start" "wrong procedure arity" proc))))

;*---------------------------------------------------------------------*/
;*    uv-fs-event-stop ...                                             */
;*---------------------------------------------------------------------*/
(define (uv-fs-event-stop o::UvFsEvent)
   (with-access::UvFsEvent o ($builtin)
      ($uv_fs_event_stop ($uv-fs-event-t $builtin))))
      
;*---------------------------------------------------------------------*/
;*    uv-fs-event-rename ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-event-rename)
   $UV_RENAME)

;*---------------------------------------------------------------------*/
;*    uv-fs-event-change ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (uv-fs-event-change)
   $UV_CHANGE)
