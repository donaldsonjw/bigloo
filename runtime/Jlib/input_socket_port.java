/*=====================================================================*/
/*    .../prgm/project/bigloo/runtime/Jlib/input_socket_port.java      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Dec  5 11:53:13 2000                          */
/*    Last change :  Fri Oct  3 18:12:52 2014 (serrano)                */
/*    Copyright   :  2000-14 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    JVM Socket input ports implementation.                           */
/*=====================================================================*/
package bigloo;
import bigloo.foreign;
import java.io.*;
import java.net.*;

/*---------------------------------------------------------------------*/
/*    INPUT_SOCKET_PORT                                                */
/*---------------------------------------------------------------------*/
public class input_socket_port extends input_port {
   public InputStream in;
   private Socket socket;

   public input_socket_port( final Socket s, final byte[] buf ) {
      super( "[socket]", buf );
      socket = s;
      InputStream is;
      
      try { 
	 is = socket.getInputStream();
      } catch( IOException _ ) {
	 is = null;
      }

      in = is;
      if( in == null )
	 foreign.fail( "socket", "Cannot find socket input stream", this );
   }

   public void close() {
      eof = true;
      other_eof = true;
      buffer = null;
      try {
	 in.close();
      } catch( Throwable _ ) {
	 ;
      }
      super.close();
   }

   public boolean rgc_charready() {
      try {
	 return ( (forward+1) < bufpos) || (0 < in.available());
      } catch (final Exception _) {
	 return false;
      }
   }

   public boolean rgc_fill_buffer() throws IOException {
      final int bufsize = this.buffer.length;
      int bufpose = this.bufpos;
      final int matchstart = this.matchstart;

      // if the buffer is not full, we fill it */
      if (bufpose < bufsize) {
	 return rgc_size_fill_con_buffer( bufpose, bufsize-bufpose );
      }

      if (0 < matchstart) {
	 // we shift the buffer left and we fill the buffer */
	 final byte[] buffer = this.buffer;
	 final int movesize = bufpose-matchstart;

	 for ( int i = 0 ; i < movesize ; ++i )
	    buffer[i] = buffer[matchstart+i];

	 bufpose -= matchstart;
	 this.matchstart = 0;
	 this.matchstop -= matchstart;
	 this.forward -= matchstart;
	 this.lastchar = buffer[matchstart-1];

	 return rgc_size_fill_con_buffer( bufpose, bufsize-bufpose );
      }

      // we current token is too large for the buffer */
      // we have to enlarge it.                       */
      rgc_double_buffer();

      return rgc_fill_buffer();
   }

   final boolean rgc_size_fill_con_buffer( int bufpose, final int size )
      throws IOException {
      // we start reading at BUFPOSE - 1 because we have */
      // to remove the '\0' sentinel that ends the buffer */
      final byte[] buffer = this.buffer;

      // FIX Dustin DeWeese" <dustin.deweese gmail.com> Feb 2006.
      // final int nbread = in.read( buffer, bufpose-1, (a < size ? a : size) );
      System.out.println( "bufpose=" + bufpose + " size=" + size + " len="
			+ buffer.length );
      final int nbread = in.read( buffer, bufpose, size );
      System.out.println( "nread=" + nbread );

      if (nbread == -1)
	 eof = true;
      else
	 bufpose += nbread;

      this.bufpos = bufpose;
      return (0 < bufpos);
   }

   public Object bgl_input_port_clone( input_port src )
      {
	 super.bgl_input_port_clone( src );
	 in = ((input_socket_port)src).in;
	 socket = ((input_socket_port)src).socket;

	 return this;
      }
   
   public boolean timeout_set( int to ) {
      try {
	 socket.setSoTimeout( to );
	 return true;
      } catch( Exception _ ) {
	 return false;
      }
   }
}
