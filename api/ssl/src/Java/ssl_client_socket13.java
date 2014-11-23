/*=====================================================================*/
/*    .../project/bigloo/api/ssl/src/Java/ssl_client_socket13.java     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Jul  8 11:11:03 2005                          */
/*    Last change :  Sun Sep  7 16:21:37 2008 (serrano)                */
/*    Copyright   :  2005-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    JDK>=1.3 SSL Java socket binding                                 */
/*=====================================================================*/

package bigloo.ssl;
import bigloo.*;
import java.io.*;
import java.net.*;
import javax.net.ssl.*;

public class ssl_client_socket extends client_socket {
   public ssl_client_socket(final byte[] hostname,
			    final int port,
			    final int ms,
			    final int proto,
			    final Object cert,
			    final Object pkey,
			    Object caList,
			    Object acceptedCerts,
			    byte[] inbuf,
			    byte[] outbuf ) {
      super();
      
      try {
         SSLSocketFactory sslsocketfactory = (SSLSocketFactory) SSLSocketFactory.getDefault();
         SSLSocket sslsocket = (SSLSocket)sslsocketfactory.createSocket( new String( hostname ), port );
      
         socket = sslsocket;

         set_socket_io_ports( socket, inbuf, outbuf );
      }
      catch( final IOException _ ) {
         socket_error( "make-ssl-client-socket",
                       "cannot create socket",
                       unspecified.unspecified );
      }
   }
}
