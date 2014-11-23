/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/cforeign.c              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct 19 08:45:22 1993                          */
/*    Last change :  Mon Jan 14 19:58:21 2008 (serrano)                */
/*    -------------------------------------------------------------    */
/*    La gestion de l'interface etrangere                              */
/*=====================================================================*/
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    Importations                                                     */
/*---------------------------------------------------------------------*/
extern obj_t string_to_symbol( char * );

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    cobj_to_foreign ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
cobj_to_foreign( obj_t id, void *cobj ) {
   obj_t handle;

   handle = GC_MALLOC( FOREIGN_SIZE );

   handle->foreign_t.header = MAKE_HEADER( FOREIGN_TYPE, FOREIGN_SIZE );
   handle->foreign_t.cobj   = (void *)cobj;
   handle->foreign_t.id     = id;

   return BREF( handle );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    void_star_to_obj ...                                             */
/*---------------------------------------------------------------------*/
BGL_EXPORTED_DEF
obj_t
void_star_to_obj( void *cobj ) {
   static obj_t id = BUNSPEC;

   if( !SYMBOLP( id ) )
      id = string_to_symbol( "VOID*" );

   return cobj_to_foreign( id, cobj );
}

/*---------------------------------------------------------------------*/
/*    BGL_LONG_T                                                             */
/*    obj_to_cobj ...                                                  */
/*---------------------------------------------------------------------*/
BGL_EXPORTED_DEF
BGL_LONG_T
obj_to_cobj( obj_t obj ) {
   if( INTEGERP( obj ) )
      return (BGL_LONG_T)CINT( obj );
   if( BOOLEANP( obj ) )
      return (BGL_LONG_T)((BGL_LONG_T)CBOOL( obj ));
   if( STRINGP( obj ) )
      return (BGL_LONG_T)BSTRING_TO_STRING( obj );
   if( CHARP( obj ) )
      return (BGL_LONG_T)((BGL_LONG_T)CCHAR( obj ));
   if( FOREIGNP( obj ) )
      return (BGL_LONG_T)FOREIGN_TO_COBJ( obj );
   if( REALP( obj ) )
      return (BGL_LONG_T)the_failure( string_to_bstring( "obj->cobj" ),
				string_to_bstring( "Can't cast a real to foreign" ),
				obj);
   else
      return (BGL_LONG_T)the_failure( string_to_bstring( "obj->cobj" ),
				string_to_bstring( "Illegal object type" ),
				obj);
}
 
