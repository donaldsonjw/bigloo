/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bde/bmem/lib/list.c                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Apr 13 06:52:54 2003                          */
/*    Last change :  Fri Dec 14 10:50:57 2012 (serrano)                */
/*    Copyright   :  2003-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Simple and naive list library                                    */
/*=====================================================================*/
#include <stdlib.h>
#include <bigloo.h>
#include <bmem.h>

/*---------------------------------------------------------------------*/
/*    pa_pair_t *                                                      */
/*    pa_cons ...                                                      */
/*---------------------------------------------------------------------*/
pa_pair_t *
pa_cons( void *car, pa_pair_t *cdr ) {
   pa_pair_t *new = (pa_pair_t *)malloc( sizeof( pa_pair_t ) );

   new->car = car;
   new->cdr = cdr;

   return new;
}

/*---------------------------------------------------------------------*/
/*    pa_pair_t *                                                      */
/*    pa_reverse ...                                                   */
/*---------------------------------------------------------------------*/
pa_pair_t *
pa_reverse( pa_pair_t *lst ) {
   pa_pair_t *new = 0;

   while( PA_PAIRP( lst ) ) {
      new = pa_cons( PA_CAR( lst ), new );
      lst = PA_CDR( lst );
   }

   return new;
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    for_each ...                                                     */
/*---------------------------------------------------------------------*/
void
for_each( void (*fun)(void *, void *), pa_pair_t *lst, void *arg ) {
   while( PA_PAIRP( lst ) ) {
      fun( PA_CAR( lst ), arg );
      lst = PA_CDR( lst );
   }
}

/*---------------------------------------------------------------------*/
/*    pa_pair_t *                                                      */
/*    pa_assq ...                                                      */
/*---------------------------------------------------------------------*/
pa_pair_t *
pa_assq( void *val, pa_pair_t *lst ) {
   while( PA_PAIRP( lst ) ) {
      pa_pair_t *hd = (pa_pair_t *)PA_CAR( lst );
      if( PA_CAR( hd ) == val )
	 return (pa_pair_t *)PA_CAR( lst );

      lst = PA_CDR( lst );
   }

   return 0;
}
   
