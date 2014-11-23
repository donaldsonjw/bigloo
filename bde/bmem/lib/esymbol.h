/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bde/bmem/lib/esymbol.h               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Apr 14 15:37:24 2003                          */
/*    Last change :  Wed Nov  5 19:35:07 2014 (serrano)                */
/*    Copyright   :  2003-14 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Extended symbols                                                 */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    esymbol_t                                                        */
/*---------------------------------------------------------------------*/
typedef struct esymbol {
   struct  symbol symbol;
   void   *alloc_info;
   int     class_alloc; 
   int     class_offset; 
   int     stamp;
} esymbol_t;

#define ESYMBOL_SIZE (sizeof( struct esymbol ) )

#define ESYMBOL_TO_STRING( sym ) \
   BSTRING_TO_STRING( SYMBOL_TO_STRING( CSYMBOL( sym ) ) )
