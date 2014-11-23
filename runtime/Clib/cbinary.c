/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/cbinary.c               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun  7 09:02:35 1994                          */
/*    Last change :  Sat Apr 19 12:19:37 2014 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Binary input and output ports.                                   */
/*=====================================================================*/
#include <stdio.h>
#include <string.h>
#include <errno.h>


#if defined( _MSC_VER) || defined( _MINGW_VER ) 
#  define _BGL_WIN32_VER
#endif

#if( !(defined( NeXT ) && (defined( mc68000 ) || defined( i386 ))) )
#   if HAVE_TERMIO
#      include <termio.h>
#   endif
#endif
#if !defined( sony_news ) && \
    !(defined( NeXT ) && (defined( mc68000 ) || defined( i386 ))) && \
    !defined( _BGL_WIN32_VER )
#   include <unistd.h>
#endif
#ifndef _BGL_WIN32_VER
#  include <sys/file.h>
#endif
#include <bigloo.h>
#if( !defined( __alpha ) && !defined( sony_news ) && \
	  !(defined( NeXT ) && defined( mc68000 )) )
#   include <ctype.h>
#endif

/*---------------------------------------------------------------------*/
/*    MAGIC_WORD ...                                                   */
/*---------------------------------------------------------------------*/
#if defined( MAGIC_WORD )
#   undef MAGIC_WORD
#endif

#define MAGIC_WORD "1966"

/*---------------------------------------------------------------------*/
/*    Les recuperations externes                                       */
/*---------------------------------------------------------------------*/
extern obj_t obj_to_string( obj_t );
extern obj_t string_to_obj( obj_t, obj_t );
extern obj_t c_constant_string_to_string( char * );
extern obj_t make_string_sans_fill( int );
extern obj_t bgl_string_shrink();

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_binary_port ...                                         */
/*    -------------------------------------------------------------    */
/*    Cette procedure alloue tous les ports binaires. Qu'ils soient    */
/*    en sortie ou en entree.                                          */
/*---------------------------------------------------------------------*/
static obj_t
bgl_make_binary_port( char *name, FILE *file, bool_t io ) {
   obj_t binary_port;

   binary_port = GC_MALLOC( BINARY_PORT_SIZE );

   binary_port->binary_port_t.header = MAKE_HEADER( BINARY_PORT_TYPE, 0 );
   binary_port->binary_port_t.file = file;
   binary_port->binary_port_t.name = string_to_bstring( name );
   binary_port->binary_port_t.io = (int)io;

   return BREF( binary_port );
}

/*---------------------------------------------------------------------*/
/*    open_output_binary_file ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
open_output_binary_file( obj_t name ) {
   FILE *file;
   
   if( !(file = fopen( BSTRING_TO_STRING( name ), "wb" )) )
      return BFALSE;

   return bgl_make_binary_port( BSTRING_TO_STRING( name ),
				file,
				BINARY_PORT_OUT );
}

/*---------------------------------------------------------------------*/
/*    append_output_binary_file ...                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
append_output_binary_file( obj_t name ) {
   FILE *file;
   
   if( !(file = fopen( BSTRING_TO_STRING( name ), "a+b" )) )
      return BFALSE;

   return bgl_make_binary_port( BSTRING_TO_STRING( name ),
				file,
				BINARY_PORT_OUT );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    close_binary_port ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
close_binary_port( obj_t port ) {
   if( BINARY_PORT( port ).io < 2 ) {
      BINARY_PORT( port ).io = 2;
      fclose( BINARY_PORT( port ).file );
   }

   return port;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_flush_binary_port ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_flush_binary_port( obj_t port ) {
   fflush( BINARY_PORT( port ).file );

   return port;
}


/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    open_input_binary_file ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
open_input_binary_file( obj_t name ) {
   FILE *file;
   obj_t binary_port;

   if( !(file = fopen( BSTRING_TO_STRING( name ), "rb" )) )
      return BFALSE;
   else {
      binary_port = bgl_make_binary_port( BSTRING_TO_STRING( name ),
					  file,
					  BINARY_PORT_IN );
      return BREF( binary_port );
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_output_string ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_output_string( obj_t port, obj_t s ) {
   return (int)fwrite( BSTRING_TO_STRING( s ),
		       STRING_LENGTH( s ),
		       1,
		       BINARY_PORT( port ).file );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_input_string ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_input_string( obj_t port, int len ) {
   obj_t s = make_string_sans_fill( len );
   char *cs = BSTRING_TO_STRING( s );
   size_t sz = fread( cs, 1, (size_t)len, BINARY_PORT( port ).file );

   if( (BGL_LONG_T)sz < (len / 2) )
      return bgl_string_shrink( s, (BGL_LONG_T)sz );
   else
      return string_to_bstring_len( cs, (BGL_LONG_T)sz );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_input_fill_string ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_input_fill_string( obj_t port, obj_t s ) {
   int len = STRING_LENGTH( s );
   char *cs = BSTRING_TO_STRING( s );

   return (int)fread( cs, 1, len, BINARY_PORT( port ).file );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    output_obj ...                                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
output_obj( obj_t port, obj_t obj ) {
   FILE *file = BINARY_PORT( port ).file;
   obj_t string;
   unsigned char slen[ 4 ];
   BGL_LONG_T clen;

   /* Le calcul de la chaine a dumper */
   string = obj_to_string( obj );

   /* Le mot magique */
   fwrite( MAGIC_WORD, 4, 1, file );

   /* La longueur de la chaine */
   clen = STRING_LENGTH( string );
	
   slen[ 0 ] = (unsigned char)clen;
   slen[ 1 ] = (unsigned char)(clen>>8);
   slen[ 2 ] = (unsigned char)(clen>>16);
   slen[ 3 ] = (unsigned char)(clen>>24);
	
   fwrite( slen, 4, 1, file );
	
   /* La chaine elle meme */
   fwrite( BSTRING_TO_STRING( string ), clen, 1, file );
   
   return obj;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    input_obj ...                                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
input_obj( obj_t port ) {
   FILE *file = BINARY_PORT( port ).file;
   unsigned char slen[ 4 ];
   BGL_LONG_T clen;
   char magic[ 4 ];
   size_t size;

#if( defined( BIGLOO_TRACE ) )
   PUSH_TRACE( string_to_symbol( "input_obj" ) );
#endif

   /* le test de fin de fichier avant un essai de lecture */
   if( feof( file ) )
      return BEOF;
      
   /* la cle magique */
   size = fread( magic, 4, 1, file );

   if( feof( file ) || !size )
      return BEOF;
   
   if( (size != 1) || (memcmp( magic, MAGIC_WORD, 4 )) )
      C_SYSTEM_FAILURE( BGL_IO_READ_ERROR,
			"input_obj",
			"corrupted file",
			port );

   /* la longueur */
   size = fread( slen, 4, 1, file );
   
   if( (size != 1) )
      C_SYSTEM_FAILURE( BGL_IO_READ_ERROR,
			"input_obj",
			"corrupted file",
			port );

   clen = ((BGL_LONG_T)slen[ 0 ]) + (((BGL_LONG_T)slen[ 1 ]) << 8) +
          (((BGL_LONG_T)slen[ 2 ]) << 16) + (((BGL_LONG_T)slen[ 3 ]) << 24);

   /* On fait deux cas en fonction de la taille de l'objet a lire */
   if( clen < 1024 ) {
      char  string[ 1024 + STRING_SIZE ];
      obj_t res, strobj = (obj_t)string;

#if( !defined( TAG_STRING ) || defined( BUMPY_GC ) )
      strobj->string_t.header = MAKE_HEADER( STRING_TYPE, 0 );
#endif		
      strobj->string_t.length = clen;
      
      fread( BSTRING_TO_STRING( BSTRING( string ) ), clen, 1, file );

      res = string_to_obj( BSTRING( string ), BFALSE );

#if( defined( BIGLOO_TRACE ) )
      POP_TRACE();
#endif
      return res;
   } else {
      obj_t  res, string;

      string = (obj_t)malloc( STRING_SIZE + clen );

      if( !string )
         C_SYSTEM_FAILURE( BGL_IO_ERROR,
			   "input_obj",
			   "can't allocate string", port );

#if( !defined( TAG_STRING ) || defined( BUMPY_GC ) )
      string->string_t.header = MAKE_HEADER( STRING_TYPE, 0 );
#endif		
      string->string_t.length = clen;
		
      fread( BSTRING_TO_STRING( BSTRING( string ) ), clen, 1, file );
      
      res = string_to_obj( BSTRING( string ), BFALSE );

      free( string );
		
#if( defined( BIGLOO_TRACE ) )
      POP_TRACE(); 
#endif
		
      return res;
   }
}
