/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/ci18n.c                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Dec 19 08:16:32 2013                          */
/*    Last change :  Tue Feb 18 19:31:10 2014 (serrano)                */
/*    Copyright   :  2013-14 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    C i18n implementation                                            */
/*=====================================================================*/
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <bigloo.h>

#if( BGL_HAVE_UNISTRING )
#  include <unistr.h>
#  include <uninorm.h>
#  include <unicase.h>
#  include <locale.h>
#endif

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_strcoll ...                                                  */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_UNISTRING )
BGL_RUNTIME_DEF
int
bgl_strcoll( obj_t left, obj_t right ) {
   return u8_strcoll( BSTRING_TO_STRING( left ), BSTRING_TO_STRING( right ) );
}
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_utf8_string_locale_upcase ...                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_utf8_string_locale_upcase( obj_t str ) {
   size_t len = STRING_LENGTH( str );
   
#if( BGL_HAVE_UNISTRING )
   uint8_t *src = BSTRING_TO_STRING( str );
   size_t buflen;
   uint8_t *buf;
   obj_t res;

   buf = u8_toupper( src, len, NULL, NULL, NULL, &buflen );
   res = string_to_bstring_len( (char *)buf, buflen );
   free( buf );
   
   return bgl_string_shrink( res, buflen );
#else
   char *src = BSTRING_TO_STRING( str );
   obj_t res = make_string_sans_fill( len );
   BGL_LONG_T i;

   for( i = 0; i < len; i++ ) {
      STRING_SET( res, i, toupper( src[ i ] ) );
   }

   return res;
#endif   
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_utf8_string_locale_downcase ...                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_utf8_string_locale_downcase( obj_t str ) {
   size_t len = STRING_LENGTH( str );

#if( BGL_HAVE_UNISTRING )
   uint8_t *src = BSTRING_TO_STRING( str );
   size_t buflen;
   uint8_t *buf;
   obj_t res;

   buf = u8_tolower( src, len, NULL, NULL, NULL, &buflen );
   res = string_to_bstring_len( (char *)buf, buflen );
   free( buf );
   
   return bgl_string_shrink( res, buflen );
#else
   char *src = BSTRING_TO_STRING( str );
   obj_t res = make_string_sans_fill( len );
   BGL_LONG_T i;

   for( i = 0; i < len; i++ ) {
      STRING_SET( res, i, tolower( src[ i ] ) );
   }

   return res;
#endif   
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_utf8_string_locale_capitalize ...                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_utf8_string_locale_capitalize( obj_t str ) {
   size_t len = STRING_LENGTH( str );
   
#if( BGL_HAVE_UNISTRING )
   uint8_t *src = BSTRING_TO_STRING( str );
   size_t buflen;
   uint8_t *buf;
   obj_t res;

   buf = u8_totitle( src, len, NULL, NULL, NULL, &buflen );
   res = string_to_bstring_len( (char *)buf, buflen );
   free( buf );
   
   return bgl_string_shrink( res, buflen );
#else
   return string_to_bstring_len( BSTRING_TO_STRING( str ), len );
#endif   
}
