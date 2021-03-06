/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/csymbol.c               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Feb 12 14:51:41 1992                          */
/*    Last change :  Sun Sep 13 17:36:41 2015 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Symbol handling (creation and hash tabling).                     */
/*=====================================================================*/
#include <string.h>
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
extern obj_t make_vector( BGL_LONG_T, obj_t );
extern BGL_LONG_T get_hash_power_number( char *, BGL_ULONG_T );
extern bool_t bigloo_strcmp( obj_t, obj_t );

/*---------------------------------------------------------------------*/
/*    Global C variables                                               */
/*---------------------------------------------------------------------*/
static obj_t c_symtab = BUNSPEC;

/*---------------------------------------------------------------------*/
/*    Symbol mutex                                                     */
/*---------------------------------------------------------------------*/
static obj_t symbol_mutex = BUNSPEC;
DEFINE_STRING( symbol_mutex_name, _1, "symbol-mutex", 12 );

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_symbol_table ...                                        */
/*---------------------------------------------------------------------*/
void
bgl_init_symbol_table() {
   if( !VECTORP( c_symtab ) ) {
      c_symtab = make_vector( SYMBOL_HASH_TABLE_SIZE, BNIL );
      symbol_mutex = bgl_make_spinlock( symbol_mutex_name );
   }
}
          
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_get_symtab ...                                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_get_symtab() {
   if( !VECTORP( c_symtab ) ) {
      bgl_init_symbol_table();
   }

   return c_symtab;
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    make_symbol ...                                                  */
/*---------------------------------------------------------------------*/
static obj_t
make_symbol( obj_t name ) {
   obj_t symbol;

   symbol = GC_MALLOC( SYMBOL_SIZE );

#if( !defined( TAG_SYMBOL ) )   
   symbol->symbol_t.header = MAKE_HEADER( SYMBOL_TYPE, SYMBOL_SIZE );
#endif   
   symbol->symbol_t.string = name;
   symbol->symbol_t.cval = BNIL;

   return BSYMBOL( symbol );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bstring_to_symbol ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
bgl_bstring_to_symbol( obj_t name ) {
   BGL_LONG_T hash_number;
   obj_t bucket;
   char *cname = BSTRING_TO_STRING( name );

   hash_number = get_hash_power_number( cname, SYMBOL_HASH_TABLE_SIZE_SHIFT );
   
   BGL_MUTEX_LOCK( symbol_mutex );
   bucket = VECTOR_REF( c_symtab, hash_number );
   
   if( NULLP( bucket ) ) {
      obj_t symbol = make_symbol( name );
      obj_t pair = MAKE_PAIR( symbol, BNIL );

      VECTOR_SET( c_symtab, hash_number, pair );
      
      BGL_MUTEX_UNLOCK( symbol_mutex );
      return symbol;
   } else {
      obj_t run = bucket, back = bucket;
      
      while( !NULLP( run ) &&
	     SYMBOL( CAR( run ) ).string &&
	     !bigloo_strcmp( SYMBOL( CAR( run ) ).string, name ) )
         back = run, run = CDR( run );
      
      if( !NULLP( run ) ) {
	 BGL_MUTEX_UNLOCK( symbol_mutex );
         return CAR( run );
      }
      else {
         obj_t symbol = make_symbol( name );
	 obj_t pair = MAKE_PAIR( symbol, BNIL );
	 
         SET_CDR( back, pair );

	 BGL_MUTEX_UNLOCK( symbol_mutex );
         return symbol;
      }
   }
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bstring_to_symbol ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bstring_to_symbol( obj_t name ) {
   return bgl_bstring_to_symbol(
      string_to_bstring_len(
	 BSTRING_TO_STRING( name ), STRING_LENGTH( name ) ) );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_string_to_symbol_len ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_string_to_symbol_len( char *cname, BGL_LONG_T len ) {
   return bgl_bstring_to_symbol( string_to_bstring_len( cname, len ) );
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    string_to_symbol ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
string_to_symbol( char *cname ) {
   return bgl_bstring_to_symbol( string_to_bstring( cname ) );
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    symbol_exists_sans_lock_p ...                                    */
/*---------------------------------------------------------------------*/
static int
symbol_exists_sans_lock_p( char *name, BGL_LONG_T hash_number ) {
   obj_t bucket;

   bucket = VECTOR_REF( c_symtab, hash_number );
   
   if( NULLP( bucket ) ) {
      return 0;
   } else {
      while( SYMBOL( CAR( bucket ) ).string &&
	     strcmp( (char *)BSTRING_TO_STRING(SYMBOL( CAR( bucket ) ).string),
		     name ) ) {
	 bucket = CDR( bucket );
	 
	 if( NULLP( bucket) ) {
	    return 0;
	 }
      }
      
      return 1;
   }
}
   
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    symbol_exists_p ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
symbol_exists_p( char *name ) {
   int r;
   BGL_LONG_T hn = get_hash_power_number( name, SYMBOL_HASH_TABLE_SIZE_SHIFT );

   BGL_MUTEX_LOCK( symbol_mutex );
   r = symbol_exists_sans_lock_p( name, hn );
   BGL_MUTEX_UNLOCK( symbol_mutex );

   return r;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_symbol_genname ...                                           */
/*    -------------------------------------------------------------    */
/*    Gensym names are generated lazily when the function              */
/*    SYMBOL->STRING is invoked. This function is in charge of         */
/*    this generation.                                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_symbol_genname( obj_t o, char *name ) {
   BGL_LONG_T hn;
   char gn[ 40 ];
   static BGL_LONG_T gensym_counter = 999;
   obj_t pair;
   size_t n = strlen( name );

   if( n > 20 ) n = 20;

   strncpy( gn, name, 20 );
   BGL_MUTEX_LOCK( symbol_mutex );
   
   while( 1 ) {
      sprintf( &gn[ n ], "%ld", ++gensym_counter );

      hn = get_hash_power_number( gn, SYMBOL_HASH_TABLE_SIZE_SHIFT );


      if( !symbol_exists_sans_lock_p( gn, hn ) ) break;
   }

   /* the name is generated, store it in the symbol itself */
   SYMBOL( o ).string = string_to_bstring( gn );

   /* and store the object in the hash table */
   pair = MAKE_PAIR( o, VECTOR_REF( c_symtab, hn ) );
   VECTOR_SET( c_symtab, hn, pair );
   
   BGL_MUTEX_UNLOCK( symbol_mutex );
   
   return SYMBOL( o ).string;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gensym ...                                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_gensym( obj_t name ) {
   obj_t o = make_symbol( 0L );

   if( name == BFALSE ) {
      return o;
   } else {
      bgl_symbol_genname( o, BSTRING_TO_STRING( name ) );
      return o;
   }
}
