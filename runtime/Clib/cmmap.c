/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/cmmap.c                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Jul 10 10:46:32 2005                          */
/*    Last change :  Mon Jun 28 11:22:31 2010 (serrano)                */
/*    Copyright   :  2005-10 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Bigloo C mmap implementation                                 */
/*=====================================================================*/
#include <bigloo.h>
#if !defined( _MSC_VER) && !defined( _MINGW_VER )
#  include <sys/mman.h>
#endif
#include <fcntl.h>
#include <string.h>

#ifndef O_BINARY
#  define O_BINARY 0
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    mmap_fail ...                                                    */
/*---------------------------------------------------------------------*/
obj_t
mmap_fail( char *op, obj_t obj ) {
   C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR, op, strerror( errno ), obj );
   
   return BFALSE;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_open_mmap ...                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_open_mmap( obj_t name, bool_t r, bool_t w ) {
#if HAVE_MMAP   
   int fd = open( BSTRING_TO_STRING( name ),
		  ((r && w) ? O_RDWR : (r ?  O_RDONLY : O_WRONLY )) | O_BINARY );
   
   if( -1 == fd ) {
      return mmap_fail( "open-mmap", name );
   } else {
      struct stat s;
      void *map = 0;
      
      if( -1 == fstat( fd, &s ) ) {
	 close( fd );
	 mmap_fail( "open-mmap", name );
      }

      if( s.st_size > 0 ) 
	 map = mmap( 0, s.st_size,
		     (r ? PROT_READ : 0) | (w ? PROT_WRITE : 0),
		     MAP_SHARED,
		     fd,
		     0 );

      if( (void *)-1 == map  ) {
	 close( fd );
	 return mmap_fail( "open-mmap", name );
      } else {
	 obj_t mm;

	 mm = GC_MALLOC( BGL_MMAP_SIZE );

	 mm->mmap_t.header = MAKE_HEADER( MMAP_TYPE, 0 );
	 mm->mmap_t.name = name;
	 mm->mmap_t.length = s.st_size;
	 mm->mmap_t.fd = fd;
	 mm->mmap_t.map = (unsigned char *)map;
	 mm->mmap_t.rp = 0;
	 mm->mmap_t.wp = 0;

	 /* The glibc looks like erroneous. Contrarily to the Linux    */
	 /* man pages, MMAP does not change st_ctime and st_mtime      */
	 /* for mmapped filed with PROT_WRITE and MAP_SHARED.          */
	 return BREF( mm );
      }
   }
#else
   int fd = r ? open( BSTRING_TO_STRING( name ), O_RDONLY | O_BINARY ) : 0;
   int afd = w ? open( BSTRING_TO_STRING( name ), O_WRONLY | O_BINARY ) : 0;
   
   if( -1 == fd ) {
      return mmap_fail( "open-mmap", name );
   } else {
      if( -1 == afd ) {
	 close( fd );
	 return mmap_fail( "open-mmap", name );
      } else {
	 struct stat s;
      
	 if( -1 == fstat( fd, &s ) ) {
	    mmap_fail( "open-mmap", name );
	 } else {
	    obj_t mm;

	    mm = GC_MALLOC( BGL_MMAP_SIZE );

	    mm->mmap_t.header = MAKE_HEADER( MMAP_TYPE, 0 );
	    mm->mmap_t.name = name;
	    mm->mmap_t.length = s.st_size;
	    mm->mmap_t.fd = fd;
	    mm->mmap_t.afd = afd;
	    mm->mmap_t.rp = 0;
	    mm->mmap_t.wp = 0;
	    mm->mmap_t.ar = 0;
	    mm->mmap_t.aw = 0;

	    return BREF( mm );
	 }
      }
   }
#endif
}


/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_string_to_mmap ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_string_to_mmap( obj_t s, bool_t r, bool_t w ) {
   obj_t mm;

   mm = GC_MALLOC( BGL_MMAP_SIZE );

   mm->mmap_t.header = MAKE_HEADER( MMAP_TYPE, 0 );
   mm->mmap_t.name = s;
   mm->mmap_t.length = STRING_LENGTH( s );
   mm->mmap_t.fd = 0;
   mm->mmap_t.map = BSTRING_TO_STRING( s );
   mm->mmap_t.rp = 0;
   mm->mmap_t.wp = 0;

   return BREF( mm );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_sync_mmap ...                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_sync_mmap( obj_t mm ) {
#if HAVE_MMAP
  if( -1 == msync(  BGL_MMAP( mm ).map, BGL_MMAP( mm ).length, MS_SYNC ) )
      return mmap_fail( "sync-mmap", mm );
   else
      return mm;
#endif
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_close_mmap ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_close_mmap( obj_t mm ) {
   BGL_LONG_T r1 = 0, r2 = 0;

   if( BGL_MMAP( mm ).fd )
      r1 = close( BGL_MMAP( mm ).fd );
   
#if HAVE_MMAP
   if( BGL_MMAP( mm ).map
       && BGL_MMAP( mm ).map != (unsigned char *)BSTRING_TO_STRING( BGL_MMAP( mm ).name ) )
      r2 = munmap( BGL_MMAP( mm ).map, BGL_MMAP( mm ).length );
   else
      r2 = 0;
#else
   if( BGL_MMAP( mm ).afd )
      r2 = close( BGL_MMAP( mm ).afd ) ;
#endif      

   if( r1 == -1 || r2 == -1 )
      return mmap_fail( "close-mmap", mm );
   else   
      return BTRUE;
}

#if !HAVE_MMAP   
/*---------------------------------------------------------------------*/
/*    unsigned char                                                    */
/*    bgl_mmap_nommap_ref ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
unsigned char
bgl_mmap_nommap_ref( obj_t mm, BGL_LONG_T i ) {
   if( BGL_MMAP( mm ).map ) {
      /* a C string used as an mmap */
      return BGL_MMAP( mm ).map[ i ];
   }
   if( !BGL_MMAP( mm ).fd ) {
      C_SYSTEM_FAILURE( BGL_IO_READ_ERROR, "mmap-ref", "write only mmap", mm );
   } else {
      unsigned char buf[ 1 ];

      if( i != BGL_MMAP( mm ).ar ) {
	 if( -1 == lseek( BGL_MMAP( mm ).fd, i, SEEK_SET ) ) {
	    mmap_fail( "mmap-ref", mm );
	 }
      }
      
      if( -1 == read( BGL_MMAP( mm ).fd, buf, 1 ) ) {
	 mmap_fail( "mmap-ref", mm );
      } else {
	 BGL_MMAP( mm ).ar = i + 1;
	 return buf[ 0 ];
      }
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_mmap_nommap_set ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF
obj_t
bgl_mmap_nommap_set( obj_t mm, BGL_LONG_T i, unsigned char c ) {
   if( BGL_MMAP( mm ).map ) {
      /* a C string used as an mmap */
      return BGL_MMAP( mm ).map[ i ];
   }
   if( !BGL_MMAP( mm ).fd ) {
      C_SYSTEM_FAILURE( BGL_IO_WRITE_ERROR,
			"mmap-write!",
			"read only mmap",
			mm );
   } else {
      if( i != BGL_MMAP( mm ).aw ) {
	 if( -1 == lseek( BGL_MMAP( mm ).afd, i, SEEK_SET ) ) {
	    mmap_fail( "mmap-ref", mm );
	 }
      }
      
      if( -1 == write( BGL_MMAP( mm ).afd, &c, 1 ) ) {
	 mmap_fail( "mmap-ref", mm );
      } else {
	 BGL_MMAP( mm ).aw = i + 1;
	 return mm;
      }
   }
}
#endif
