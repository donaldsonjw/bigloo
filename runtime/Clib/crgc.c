/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/crgc.c                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Sep 13 11:58:32 1998                          */
/*    Last change :  Thu Nov 19 18:29:29 2015 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Rgc runtime (mostly port handling).                              */
/*=====================================================================*/
#include <stdio.h>
#include <errno.h> 
#include <string.h>
#if defined( _MSC_VER) || defined( _MINGW_VER )
#   define _BGL_WIN32_VER
#endif
#ifndef _BGL_WIN32_VER
#   include <dirent.h>
#   include <sys/file.h>
#   include <sys/time.h>
#else
#   include <io.h>
#   include <windows.h>
#endif
#if( !(defined( NeXT ) && (defined( mc68000 ) || defined( i386 ))) )
#   if HAVE_TERMIO
#      include <termio.h>
#   endif
#endif
#if !defined( sony_news ) && \
    !(defined( NeXT ) && (defined( mc68000 ) || defined( i386 ))) && \
    !defined( _MSC_VER )
#   include <unistd.h>
#endif
#include <bigloo.h>
#if( defined( sony_news ) || (defined( NeXT ) && defined( mc68000 )) )
#   include <ctype.h>
#endif
#if POSIX_FILE_OPS
#   include <unistd.h>
#endif

/*---------------------------------------------------------------------*/
/*    isascii                                                          */
/*---------------------------------------------------------------------*/
#if( !defined( isascii ) )
#   define isascii( c ) (!((c) & ~0177))
#endif

/*---------------------------------------------------------------------*/
/*    C imports                                                        */
/*---------------------------------------------------------------------*/
extern obj_t bigloo_case_sensitive;
extern obj_t bgl_string_to_keyword_len( char *, BGL_LONG_T );
extern obj_t bgl_string_to_symbol_len( char *, BGL_LONG_T );
extern obj_t make_string_sans_fill( BGL_LONG_T );
extern obj_t string_to_bstring_len( char *, BGL_LONG_T );
extern int bgl_debug();
extern obj_t bgl_escape_C_string( unsigned char *, BGL_LONG_T, BGL_LONG_T );
extern obj_t bgl_escape_scheme_string( unsigned char *, BGL_LONG_T, BGL_LONG_T );
extern BGL_LONG_T default_io_bufsiz;

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    rgc_double_buffer ...                                            */
/*    -------------------------------------------------------------    */
/*    Double the size of the internal RGC buffer.                      */
/*---------------------------------------------------------------------*/
static void
rgc_double_buffer( obj_t port ) {
   BGL_LONG_T bufsiz = BGL_INPUT_PORT_BUFSIZ( port );
   obj_t oldbuf = BGL_INPUT_PORT_BUFFER( port );
   obj_t newbuf;
   
   if( !STRINGP( oldbuf ) )
      C_SYSTEM_FAILURE( BGL_IO_READ_ERROR, "read", "Can't enlarge buffer", port );

   newbuf = make_string_sans_fill( bufsiz * 2 );
   memmove( &STRING_REF( newbuf, 0 ), &STRING_REF( oldbuf, 0 ), bufsiz );
   INPUT_PORT( port ).buf = newbuf;
}
  
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    rgc_shift_buffer ...                                             */
/*    -------------------------------------------------------------    */
/*    Shift the buffer left.                                           */
/*---------------------------------------------------------------------*/
static void
rgc_shift_buffer( obj_t port ) {
   obj_t buf = BGL_INPUT_PORT_BUFFER( port );
   BGL_LONG_T bufpos = INPUT_PORT( port ).bufpos;
   BGL_LONG_T matchstart = INPUT_PORT( port ).matchstart;

   /* shift the buffer left */
   INPUT_PORT( port ).lastchar = STRING_REF( buf, matchstart - 1 );
   memmove( &STRING_REF( buf, 0 ), &STRING_REF( buf, matchstart ), bufpos - matchstart + 1 );

   /* adjust the various cursors */
   INPUT_PORT( port ).bufpos -= matchstart;
   INPUT_PORT( port ).matchstop -= matchstart;
   INPUT_PORT( port ).forward -= matchstart;
   INPUT_PORT( port ).matchstart = 0;
}

/*---------------------------------------------------------------------*/
/*    static BGL_LONG_T                                                      */
/*    sysread ...                                                      */
/*---------------------------------------------------------------------*/
static BGL_LONG_T
sysread( obj_t port, char *buf, BGL_LONG_T o, size_t size ) {
   BGL_LONG_T r = INPUT_PORT( port ).sysread( port, &buf[ o ], size );

   if( r < 0 ) {
      int e = (errno == BGL_ECONNRESET ?
	       BGL_IO_CONNECTION_ERROR : BGL_IO_READ_ERROR);

      C_SYSTEM_FAILURE( e, "read", strerror( errno ), port );
   } else {
      return r;
   }
}

/*---------------------------------------------------------------------*/
/*    static bool_t                                                    */
/*    rgc_fillsize_buffer ...                                          */
/*---------------------------------------------------------------------*/
static BGL_LONG_T
rgc_fillsize_buffer( obj_t port, char *buf, BGL_LONG_T bufpos, BGL_LONG_T size ) {
   BGL_LONG_T r;
   BGL_LONG_T fb = INPUT_PORT( port ).fillbarrier;
   
   if( fb == 0 ) {
      INPUT_PORT( port ).bufpos = bufpos;
      return 0;
   }
   
   if( (fb > 0) && (size > fb) ) size = fb;

   r = sysread( port, buf, bufpos, size );
   
   if( fb > 0 ) {
      INPUT_PORT( port ).fillbarrier = (fb - r);
   }
   
   bufpos += r;

   INPUT_PORT( port ).bufpos = bufpos;

   return r > 0;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    rgc_fill_buffer ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
rgc_fill_buffer( obj_t port ) {
   if( INPUT_PORT_CLOSEP( port ) ) {
      C_SYSTEM_FAILURE( BGL_IO_READ_ERROR, "read", "input-port closed", port );
   } else {
      BGL_LONG_T bufpos = INPUT_PORT( port ).bufpos;

      /* the read reached end-of-buffer, update the forward ptr  */
      INPUT_PORT( port ).forward = bufpos;

      /* the input port that has seen its eof      */
      /* cannot be filled anymore                  */
      if( INPUT_PORT( port ).eof ) {
	 return (bool_t)0;
      } else {
	 unsigned char *buf = &RGC_BUFFER_REF( port, 0 );
	 BGL_LONG_T bufsize = BGL_INPUT_PORT_BUFSIZ( port );

	 if( bufpos < bufsize ) {
	 fill:
	    /* the buffer is not full, we fill it */
#if( defined( RGC_0 ) )	 
	    if( rgc_fillsize_buffer( port, buf, bufpos, bufsize - bufpos ) ) {
	       /* add the new sentinel */
	       /* RGC 0 */
	       buf[ INPUT_PORT( port ).bufpos ] = 0;
	       return (bool_t)1;
	    } else {
	       return (bool_t)0;
	    }
#else
	    return rgc_fillsize_buffer( port, buf, bufpos, bufsize - bufpos );
#endif	 
	 } else {
	    if( INPUT_PORT( port ).matchstart > 0 ) {
	       /* we are in the middle of a match, shift the buffer first */
	       rgc_shift_buffer( port );
	    
	       bufpos = INPUT_PORT( port ).bufpos;

	       goto fill;
	    } else {
	       /* the current token is too large for the buffer */
	       /* we have to enlarge it.                        */
	       /* Note: see rgc_size_fil_buffer for other       */
	       /* enlarge_buffer                                */
	       rgc_double_buffer( port );

	       bufsize = BGL_INPUT_PORT_BUFSIZ( port );
	       buf = &RGC_BUFFER_REF( port, 0 );
	    
	       goto fill;
	    }
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_LONG_T                                                             */
/*    bgl_rgc_blit_string ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF BGL_LONG_T
bgl_rgc_blit_string( obj_t p, char *s, BGL_LONG_T o, BGL_LONG_T l ) {
   BGL_LONG_T avail = RGC_BUFFER_AVAILABLE( p );
   
   if( INPUT_PORT_CLOSEP( p ) ) {
      C_SYSTEM_FAILURE( BGL_IO_CLOSED_ERROR, "rgc-blit-string", "input-port closed", p );
   } 

   RGC_START_MATCH( p );

   if( INPUT_PORT( p ).eof ) {
      /* no need try to read anything new, use what we have */
/*       fprintf( stderr, "rgc_blit_string.2 EOF l=%d avail=%d matchstart=%d matchstop=%d bufpos=%d\n", l, */
/* 	       INPUT_PORT( p ).matchstart, INPUT_PORT( p ).matchstop, INPUT_PORT( p ).bufpos ); */
      if( l > avail ) l = avail;
   }

   if( avail >= l ) {
      memmove( &s[ o ], &RGC_BUFFER_REF( p, INPUT_PORT( p ).matchstart ), l );

      INPUT_PORT( p ).matchstart += l;
      INPUT_PORT( p ).forward = INPUT_PORT( p ).matchstart;
      RGC_STOP_MATCH( p, INPUT_PORT( p ).matchstart );
      
      INPUT_PORT( p ).filepos += l;

      return l;
   } else {
      BGL_LONG_T o0 = o;

      /* collect what we have */
      if( avail > 0 ) {
	 memmove( &s[ o ], &RGC_BUFFER_REF( p, INPUT_PORT( p ).matchstart ), avail );
	 o += avail;
	 l -= avail;
      }

      /* read what we need */
      if( l > 0 ) {
_loop:
	 if( !(INPUT_PORT( p ).eof) ) {
	    BGL_LONG_T size = l < default_io_bufsiz ? l : default_io_bufsiz;
	    BGL_LONG_T r = sysread( p, s, o, size );

	    o += r;
	    l -= r;

	    if( l > 0 ) {
	       if( (BGL_LONG_T)PORT( p ).kindof != (BGL_LONG_T)KINDOF_DATAGRAM ) {
		  goto _loop;
	       }
	    }
	 }
      }

      INPUT_PORT( p ).forward = 0;
      INPUT_PORT( p ).bufpos = 0;
      INPUT_PORT( p ).matchstart = 0;
      INPUT_PORT( p ).matchstop = 0;
      INPUT_PORT( p ).lastchar = '\n';
#if( defined( RGC_0 ) )
      RGC_BUFFER_SET( p, 0, '\0' );
#endif

      INPUT_PORT( p ).filepos += (o - o0);

      return (o - o0);
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    rgc_buffer_unget_char ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
rgc_buffer_unget_char( obj_t ip, int c ) {
   INPUT_PORT( ip ).filepos--;
   
   if( INPUT_PORT( ip ).matchstop > 0 ) {
      INPUT_PORT( ip ).matchstop--;
   } else {
      RGC_BUFFER_SET( ip , 0, c );
#if( defined( RGC_0 ) )
      RGC_BUFFER_SET( ip , 1, '\0' );
#endif
   }

   return c;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    rgc_reserve_space ...                                            */
/*---------------------------------------------------------------------*/
static void
rgc_reserve_space( obj_t port, BGL_LONG_T amount ) {
   BGL_LONG_T bufsize = BGL_INPUT_PORT_BUFSIZ( port );
   BGL_LONG_T bufpos = INPUT_PORT( port ).bufpos;
   BGL_LONG_T matchstop = INPUT_PORT( port ).matchstop;
   unsigned char *buffer = &RGC_BUFFER_REF( port, 0 );

   if( matchstop >= amount ) return;

   if( (matchstop + (bufsize - bufpos)) >= amount ) {
      BGL_LONG_T diff = amount - matchstop;

      /* we shift the buffer to the right */
      memmove( (char *)&RGC_BUFFER_REF( port, amount ),
	       (char *)&RGC_BUFFER_REF( port, matchstop ),
	       bufpos - matchstop );

#if( defined( RGC_0 ) )
      RGC_BUFFER_SET( port, bufpos + diff, 0 );
#endif

      INPUT_PORT( port ).bufpos += diff;
      INPUT_PORT( port ).matchstop += diff;
   } else {
      rgc_double_buffer( port );
      rgc_reserve_space( port, amount );
   }
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    rgc_buffer_insert_substring ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
rgc_buffer_insert_substring( obj_t ip, obj_t str, BGL_LONG_T from, BGL_LONG_T to ) {
   BGL_LONG_T len = to - from;
   BGL_LONG_T matchstop;

   if( INPUT_PORT_CLOSEP( ip ) ) return 0;

   if( from >= to ) return 1;

   rgc_reserve_space( ip, len );

   matchstop = INPUT_PORT( ip ).matchstop;

   /* we insert the given buffer */
   memmove( (char *)&RGC_BUFFER_REF( ip, matchstop - len ),
	    &STRING_REF( str, from ),
	    len );
   
   if( INPUT_PORT( ip ).filepos >= len ) {
      INPUT_PORT( ip ).filepos -= len;
   } else {
      INPUT_PORT( ip ).filepos = 0;
   }

   matchstop -= len;
   INPUT_PORT( ip ).matchstop  = matchstop;
   INPUT_PORT( ip ).forward = matchstop;
   INPUT_PORT( ip ).matchstart = matchstop;

   return 1;
}   

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    rgc_buffer_insert_char ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
rgc_buffer_insert_char( obj_t ip, int c ) {
   BGL_LONG_T matchstop;

   if( INPUT_PORT_CLOSEP( ip ) ) return 0;

   rgc_reserve_space( ip, 1 );

   matchstop = INPUT_PORT( ip ).matchstop;

   /* we insert the given buffer */
   RGC_BUFFER_SET( ip, matchstop - 1, c );

   if( INPUT_PORT( ip ).filepos >= 1 )
      INPUT_PORT( ip ).filepos--;
   else
      INPUT_PORT( ip ).filepos = 0;

   matchstop--;
   INPUT_PORT( ip ).matchstop  = matchstop;
   INPUT_PORT( ip ).forward = matchstop;
   INPUT_PORT( ip ).matchstart = matchstop;

   return 1;
}   

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    rgc_buffer_bol_p ...                                             */
/*    -------------------------------------------------------------    */
/*    Is the matchstart position located at the beginning of a line?   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
rgc_buffer_bol_p( obj_t ip ) {
   if( INPUT_PORT( ip ).matchstart > 0 ) {
      return RGC_BUFFER_REF( ip, INPUT_PORT( ip ).matchstart - 1 ) == '\n';
   } else {
      return INPUT_PORT( ip ).lastchar == '\n';
   }
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    rgc_buffer_eol_p ...                                             */
/*    -------------------------------------------------------------    */
/*    Does the buffer contain, at its first non match position, a `\n' */
/*    character?                                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
rgc_buffer_eol_p( obj_t ip, BGL_LONG_T forward, BGL_LONG_T bufpos ) {
   if( forward == bufpos ) {
      if( rgc_fill_buffer( ip ) )
	 return rgc_buffer_eol_p( ip, INPUT_PORT( ip ).forward, INPUT_PORT( ip ).bufpos );
      else {
	 return 0;
      }
   } else {
      INPUT_PORT( ip ).forward = forward;
      INPUT_PORT( ip ).bufpos = bufpos;
      return RGC_BUFFER_GET_CHAR( ip, forward ) == '\n';
   }
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    rgc_buffer_bof_p ...                                             */
/*    -------------------------------------------------------------    */
/*    Is the match position at the beginning of the file?              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
rgc_buffer_bof_p( obj_t ip ) {
   return INPUT_PORT( ip ).filepos == 0;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    rgc_buffer_eof_p ...                                             */
/*    -------------------------------------------------------------    */
/*    Is the input port at its end-of-file position?                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
rgc_buffer_eof_p( obj_t ip ) {
   BGL_LONG_T m = INPUT_PORT( ip ).matchstop;
   BGL_LONG_T p = INPUT_PORT( ip ).bufpos;
   BGL_LONG_T e = INPUT_PORT( ip ).eof;
   BGL_LONG_T s = BGL_INPUT_PORT_BUFSIZ( ip );

/*    fprintf( stderr, "rgc_buffer_eof_p f=%d s=%d p=%d\n", f, s, p ); */
/*    return (f == s) && (f == p);                                     */
   return e && (m == p);
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    rgc_buffer_eof_p ...                                             */
/*    -------------------------------------------------------------    */
/*    Is the input port at its end-of-file position?                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
rgc_buffer_eof2_p( obj_t ip, BGL_LONG_T forward, BGL_LONG_T bufpos ) {
   if( forward < bufpos ) {
      INPUT_PORT( ip ).forward = forward;
      INPUT_PORT( ip ).bufpos = bufpos;
      
      return 0;
   } else {
      if( INPUT_PORT( ip ).eof ) {
	 INPUT_PORT( ip ).forward = forward;
	 INPUT_PORT( ip ).bufpos = bufpos;
	 
	 return 1;
      } else {
	 return !rgc_fill_buffer( ip );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    file_charready ...                                               */
/*---------------------------------------------------------------------*/
static int
file_charready( FILE *f ) {
#ifndef _BGL_WIN32_VER
#   if( BGL_HAVE_SELECT )
       fd_set readfds;
       struct timeval timeout;
       int fno = fileno( f ) + 1;

       FD_ZERO( &readfds );
       FD_SET( fileno( f ), &readfds );
       timeout.tv_sec = 0; timeout.tv_usec = 0;

       return (select( fno, &readfds, NULL, NULL, &timeout ) > 0);
#   else
       return 0;
#   endif
#else
    HANDLE hFile = (HANDLE)_get_osfhandle( _fileno( f ) );

    return ((WaitForSingleObject( hFile, 0) == WAIT_OBJECT_0) ? 1 : 0);
#endif
}

/*---------------------------------------------------------------------*/
/*    boot_t                                                           */
/*    bgl_rgc_charready ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_rgc_charready( obj_t port ) {
   switch( (BGL_LONG_T)PORT( port ).kindof ) {
      case (BGL_LONG_T)KINDOF_CLOSED:
	 return 0;
	 
      case (BGL_LONG_T)KINDOF_STRING:
	 return ((INPUT_PORT( port ).matchstop) < INPUT_PORT( port ).bufpos);
	 
      case (BGL_LONG_T)KINDOF_FILE:
	 return ((INPUT_PORT( port ).matchstop) < INPUT_PORT( port ).bufpos)
	    || (!feof( PORT_FILE( port ) )
		&& !INPUT_PORT( port ).eof);
	 
      case (BGL_LONG_T)KINDOF_PROCPIPE:
      case (BGL_LONG_T)KINDOF_PIPE:
      case (BGL_LONG_T)KINDOF_CONSOLE:
      case (BGL_LONG_T)KINDOF_SOCKET:
      case (BGL_LONG_T)KINDOF_DATAGRAM:
	 return ((INPUT_PORT( port ).matchstop) < INPUT_PORT( port ).bufpos)
	    || file_charready( PORT_FILE( port ) );
	 
      case (BGL_LONG_T)KINDOF_PROCEDURE:
      case (BGL_LONG_T)KINDOF_GZIP:
	 /* to know if a char is available we only could call the procedure */
	 /* this could block, so we just return true                        */
	 return 1;

      default:
	 return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_substring ...                                         */
/*    -------------------------------------------------------------    */
/*    This function makes no bound checks because these tests have     */
/*    already been performed in the grammar.                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
rgc_buffer_substring( obj_t ip, BGL_LONG_T offset, BGL_LONG_T end ) {
   BGL_LONG_T start = INPUT_PORT( ip ).matchstart;
   BGL_LONG_T len = end - offset;
   char *s = (char *)&RGC_BUFFER_REF( ip, start + offset );

   return string_to_bstring_len( s, len );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_escape_substring ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
rgc_buffer_escape_substring( obj_t ip, BGL_LONG_T offset, BGL_LONG_T end, bool_t strict ) {
   BGL_LONG_T start = INPUT_PORT( ip ).matchstart;
   char *s = (char *)&RGC_BUFFER_REF( ip, start );
   
   if( strict )
      return bgl_escape_scheme_string( s, offset, end );
   else 
      return bgl_escape_C_string( s, offset, end );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_symbol ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
rgc_buffer_symbol( obj_t ip ) {
   BGL_LONG_T start = INPUT_PORT( ip ).matchstart;
   char *s = &RGC_BUFFER_REF( ip, start );
   
   return bgl_string_to_symbol_len( s, RGC_BUFFER_MATCH_LENGTH( ip ) );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_subsymbol ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
rgc_buffer_subsymbol( obj_t ip, BGL_LONG_T offset, BGL_LONG_T end ) {
   BGL_LONG_T start = INPUT_PORT( ip ).matchstart;
   BGL_LONG_T len = end - offset;
   char *s = &RGC_BUFFER_REF( ip, start + offset );

   return bgl_string_to_symbol_len( s, len );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_upcase_subsymbol ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
rgc_buffer_upcase_subsymbol( obj_t ip, BGL_LONG_T offset, BGL_LONG_T end ) {
   BGL_LONG_T start = INPUT_PORT( ip ).matchstart;
   BGL_LONG_T len = end - offset;
   unsigned char *s = &RGC_BUFFER_REF( ip, start + offset );
   BGL_LONG_T i = RGC_BUFFER_MATCH_LENGTH( ip );

   for( i = 0; i < len; i++ ) {
      if( isascii( s[ i ] ) ) s[ i ] = toupper( s[ i ] );
   }

   return bgl_string_to_symbol_len( s, len );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_downcase_subsymbol ...                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
rgc_buffer_downcase_subsymbol( obj_t ip, BGL_LONG_T offset, BGL_LONG_T end ) {
   BGL_LONG_T start = INPUT_PORT( ip ).matchstart;
   BGL_LONG_T len = end - offset;
   unsigned char *s = &RGC_BUFFER_REF( ip, start + offset );
   BGL_LONG_T i = RGC_BUFFER_MATCH_LENGTH( ip );

   for( i = 0; i < len; i++ ) {
      if( isascii( s[ i ] ) ) s[ i ] = tolower( s[ i ] );
   }

   return bgl_string_to_symbol_len( s, len );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_keyword ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
rgc_buffer_keyword( obj_t ip ) {
   BGL_LONG_T start = INPUT_PORT( ip ).matchstart;
   unsigned char *s = &RGC_BUFFER_REF( ip, start );
   
   if( *s == ':' ) s++;

   return bgl_string_to_keyword_len( s, RGC_BUFFER_MATCH_LENGTH( ip ) - 1 );
}
 
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_downcase_keyword ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
rgc_buffer_downcase_keyword( obj_t ip ) {
   BGL_LONG_T start = INPUT_PORT( ip ).matchstart;
   unsigned char *s = &RGC_BUFFER_REF( ip, start );
   BGL_LONG_T i, len = RGC_BUFFER_MATCH_LENGTH( ip ) - 1;

   if( *s == ':' ) s++;
   
   for( i = 0; i < len; i++ ) {
      if( isascii( s[ i ] ) ) s[ i ] = tolower( s[ i ] );
   }

   return bgl_string_to_keyword_len( s, len );
}
 
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_upcase_keyword ...                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
rgc_buffer_upcase_keyword( obj_t ip ) {
   BGL_LONG_T start = INPUT_PORT( ip ).matchstart;
   unsigned char *s = &RGC_BUFFER_REF( ip, start );
   unsigned char *walk;
   BGL_LONG_T i, len = RGC_BUFFER_MATCH_LENGTH( ip ) - 1;
   
   if( *s == ':' ) s++;
   
   for( i = 0; i < len; i++ ) {
      if( isascii( s[ i ] ) ) s[ i ] = toupper( s[ i ] );
   }

   return bgl_string_to_keyword_len( s, len );
}

/* {*---------------------------------------------------------------------*} */
/* {*    CHEAT_BUFFER_AT                                                  *} */
/* {*---------------------------------------------------------------------*} */
/* #define CHEAT_BUFFER_AT( s ) \                                      */
/*    BGL_LONG_T stop  = s; \                                                */
/*    char bck; \                                                      */
/*    bck = RGC_BUFFER_REF( ip, stop ); \                              */
/*    RGC_BUFFER_SET( ip, stop, '\0' );                                */
/*                                                                     */
/* {*---------------------------------------------------------------------*} */
/* {*    CHEAT_BUFFER                                                     *} */
/* {*---------------------------------------------------------------------*} */
/* #define CHEAT_BUFFER() \                                            */
/*    CHEAT_BUFFER_AT( INPUT_PORT( ip ).matchstop )                    */
/*                                                                     */
/* {*---------------------------------------------------------------------*} */
/* {*    RESTORE_BUFFER                                                   *} */
/* {*---------------------------------------------------------------------*} */
/* #define RESTORE_BUFFER() \                                          */
/*    RGC_BUFFER_SET( ip, stop, bck );                                 */
/*                                                                     */
/*---------------------------------------------------------------------*/
/*    BGL_LONG_T                                                             */
/*    rgc_buffer_fixnum ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF BGL_LONG_T
rgc_buffer_fixnum( obj_t ip ) {
   BGL_LONG_T res = 0;
   BGL_LONG_T stop = INPUT_PORT( ip ).matchstop;
   BGL_LONG_T start = INPUT_PORT( ip ).matchstart;
   char *buf = &RGC_BUFFER_REF( ip, 0 );
   BGL_LONG_T i;
   
   if( buf[ start ] == '-' || buf[ start ] == '+' ) {
      i = start + 1;
   } else {
      i = start;
   }

   for( ; i < stop; i++ ) {
      res = (res * 10) + (buf[ i ] - '0');
   }
   
/*    CHEAT_BUFFER();                                                  */
/*    res = atol( (const char *)&RGC_BUFFER_REF( ip, INPUT_PORT(ip).matchstart ) ); */
/*    RESTORE_BUFFER();                                                */

   return ( buf[ start ] == '-' ) ? -res : res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_bignum ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
rgc_buffer_bignum( obj_t ip ) {
   BGL_LONG_T start = INPUT_PORT( ip ).matchstart;
   BGL_LONG_T stop = INPUT_PORT( ip ).matchstop;
   char *buf = &RGC_BUFFER_REF( ip, 0 );
   obj_t res;
   
   if( (stop < INPUT_PORT( ip ).bufpos) && isspace( buf[ stop ] ) ) {
      return bgl_string_to_bignum( &RGC_BUFFER_REF( ip, start ), 10 );
   } else {
      BGL_LONG_T sz = stop - start;
      char *tmp = alloca( sz +1 );
      memcpy( tmp, &buf[ start ], sz );
      tmp[ sz ] = 0;
      
      return bgl_string_to_bignum( tmp, 10 );
   }
/*                                                                     */
/*                                                                     */
/*    CHEAT_BUFFER();                                                  */
/*                                                                     */
/*    res = bgl_string_to_bignum( &RGC_BUFFER_REF( ip, start ), 10 );  */
/*                                                                     */
/*    RESTORE_BUFFER();                                                */
/*                                                                     */
/*    return res;                                                      */
}
   
/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    rgc_buffer_flonum ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF double
rgc_buffer_flonum( obj_t ip ) {
   double res;
   BGL_LONG_T stop = INPUT_PORT( ip ).matchstop;
   BGL_LONG_T start = INPUT_PORT( ip ).matchstart;
   char *buf = &RGC_BUFFER_REF( ip, 0 );

   if( (stop < INPUT_PORT( ip ).bufpos) && isspace( buf[ stop ] ) ) {
      return strtod( &buf[ INPUT_PORT( ip ).matchstart ], 0 );
   } else {
      BGL_LONG_T sz = stop - start;
      char *tmp = alloca( sz +1 );
      memcpy( tmp, &buf[ start ], sz );
      tmp[ sz ] = 0;

      return strtod( tmp, 0 );
   }
/*    CHEAT_BUFFER();                                                  */
/*                                                                     */
/*    res = strtod( (const char *)&RGC_BUFFER_REF(ip, INPUT_PORT(ip).matchstart), 0 ); */
/*                                                                     */
/*    RESTORE_BUFFER();                                                */
/*                                                                     */
/*    return res;                                                      */
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    rgc_buffer_integer ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
rgc_buffer_integer( obj_t ip ) {
   /* code relies on the fact that maxvals are <= than -minvals        */
   /* generally we have maxval = -minval - 1 (for fx, elong and llong).*/
   BGL_LONG_T stop = INPUT_PORT( ip ).matchstop;
   BGL_LONG_T start = INPUT_PORT( ip ).matchstart;
   BGL_LONG_T res = 0;
   int sign = +1;
   BGL_LONG_T maxvalfx = BGL_LONG_MAX >> TAG_SHIFT;
   BGL_LONG_T maxvalelong = BGL_LONG_MAX;
   BGL_LONGLONG_T maxvalllong = BGL_LONGLONG_MAX;

   /* the sign */
   if( RGC_BUFFER_REF( ip, start ) == '+' ) {
      start++;
   } else {
      if( RGC_BUFFER_REF( ip, start ) == '-' ) {
	 start++;
	 sign = -1;
      }
   }

   /* skip the 0 padding */
   while( (start < stop) && RGC_BUFFER_REF( ip, start ) == '0' )
      start++;

   /* the real number */
   while( start < stop ) {
      char current = RGC_BUFFER_REF( ip, start ) - '0';

      /* a more accurate, but slower test would be:
       * if (res > (maxvalfx / 10 - current)) goto llong; */
      if ( res > (maxvalelong / 10 - 9) ) goto llong;

      res = (res * 10) + current;
      start++;
   }

   if( res <= maxvalfx )
      return BINT( res * sign );
   else
      return make_belong( res * sign );
      
llong:
   {
      BGL_LONGLONG_T lres = (BGL_LONGLONG_T)res;

      while( start < stop ) {
	 char current = RGC_BUFFER_REF( ip, start ) - '0';

	 /* a more accurate, but slower test would be:
	  * if (res > (maxvalllong / 10 - current)) return ...; */
	 if ( lres > (maxvalllong / 10 - 9) )
	    return rgc_buffer_bignum( ip );
	 
	 lres = lres * 10 + current;
	 start++;
      }
      return make_bllong( lres * sign );
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    rgc_debug_port ...                                               */
/*---------------------------------------------------------------------*/
int rgc_debug_port( obj_t port, char *msg ) {
   BGL_LONG_T matchstart = INPUT_PORT( port ).matchstart;
   BGL_LONG_T matchstop = INPUT_PORT( port ).matchstop;
   BGL_LONG_T forward = INPUT_PORT( port ).forward;
   BGL_LONG_T bufsiz = BGL_INPUT_PORT_BUFSIZ( port );
   BGL_LONG_T bufpos = INPUT_PORT( port ).bufpos;
   int eof = INPUT_PORT( port ).eof;
   
   fprintf( stderr, "RGC_DEBUG(%s) port=%p:%s mstart=%d mstop=%d forward=%d bpos=%d bsiz=%d %s\n", msg, 
	    port, BSTRING_TO_STRING( INPUT_PORT_NAME( port ) ),
	    matchstart, matchstop, forward, bufpos, bufsiz,
	    eof ? "eof" : "" );
}
      
	    
