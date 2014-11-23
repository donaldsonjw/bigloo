/*=====================================================================*/
/*    .../bigloo/api/gstreamer/src/Plugin/bglgst_portsink.c            */
/*    -------------------------------------------------------------    */
/*    Author      :  Cyprien Nicolas                                   */
/*    Creation    :  Tue Jul 29 10:08:12 2008                          */
/*    Last change :  Wed Feb 13 16:48:18 2013 (serrano)                */
/*    Copyright   :  2008-13 Cyprien Nicolas & Manuel Serrano          */
/*    -------------------------------------------------------------    */
/*    Bigloo OUTPUT-PORT plugin.                                       */
/*    -------------------------------------------------------------    */
/*    GStreamer                                                        */
/*    Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>      */
/*                       2000 Wim Taymans <wim@fluendo.com>            */
/*                                                                     */
/*    This library is free software; you can redistribute it and/or    */
/*    modify it under the terms of the GNU Library General Public      */
/*    License as published by the Free Software Foundation; either     */
/*    version 2 of the License, or (at your option) any later version. */
/*---------------------------------------------------------------------*/
#include "bglgst_port.h"

#ifndef G_PARAM_STATIC_STRINGS
#  define G_PARAM_STATIC_STRINGS 0
#endif
  
/*---------------------------------------------------------------------*/
/*     Bigloo imports                                                  */
/*---------------------------------------------------------------------*/
extern obj_t bglgst_register_port( obj_t );
extern obj_t bglgst_unregister_port( obj_t );

static GstStaticPadTemplate sinktemplate =
  GST_STATIC_PAD_TEMPLATE( "sink",
                           GST_PAD_SINK,
                           GST_PAD_ALWAYS,
                           GST_STATIC_CAPS_ANY );

GST_DEBUG_CATEGORY_STATIC( bgl_gst_port_sink_debug );

#define GST_CAT_DEFAULT bgl_gst_port_sink_debug

#define DEFAULT_PORT BFALSE
#define DEFAULT_AUTOCLOSE TRUE

/*---------------------------------------------------------------------*/
/*    PortSink signals and args                                        */
/*---------------------------------------------------------------------*/
enum {
   PROP_0,
   PROP_PORT,
   PROP_BUFFER,
   PROP_URI,
   PROP_AUTOCLOSE,
};

/*---------------------------------------------------------------------*/
/*    Boilerplate                                                      */
/*---------------------------------------------------------------------*/
#define _do_init( _ ) \
  GST_DEBUG_CATEGORY_INIT( bgl_gst_port_sink_debug, \
			   "bglportsink", \
			   0, \
			   "bglportsink element" );

GST_BOILERPLATE_FULL( BglPortSink,
		      bgl_gst_port_sink,
		      GstBaseSink,
		      GST_TYPE_BASE_SINK,
		      _do_init );

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_gst_port_sink_set_property ...                               */
/*---------------------------------------------------------------------*/
static void
bgl_gst_port_sink_set_property( GObject *object,
				guint prop_id,
				const GValue *value,
				GParamSpec *pspec) {
   BglPortSink *sink = BGL_GST_PORT_SINK( object );
   obj_t obj;
  
   switch( prop_id ) {
      case PROP_PORT:
	 obj = (obj_t)g_value_get_pointer( value );
	 if( !OUTPUT_PORTP( obj ) ) {
	    C_SYSTEM_FAILURE( BGL_TYPE_ERROR,
			      "bglport",
			      "Illegal output-port",
			      obj );
	 }
	 if( sink->port != BFALSE ) {
	    bglgst_unregister_port( sink->port );
	 }

	 bglgst_register_port( obj );
	 sink->port = obj;
	 break;

      case PROP_BUFFER:
	 sink->buffer = (obj_t)g_value_get_pointer( value );
	 break;

      case PROP_URI:
	 sink->uri = (char *)g_value_get_string( value );
	 break;

      case PROP_AUTOCLOSE:
	 sink->autoclose = g_value_get_boolean( value );
	 break;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_gst_port_sink_get_property ...                               */
/*---------------------------------------------------------------------*/
static void
bgl_gst_port_sink_get_property( GObject *object,
				guint prop_id,
				GValue *value,
				GParamSpec *pspec ) {
   BglPortSink *sink = BGL_GST_PORT_SINK( object );

   switch( prop_id ) {
      case PROP_PORT:
	 g_value_set_pointer( value, sink->port );
	 break;
	 
      case PROP_BUFFER:
	 g_value_set_pointer( value, sink->buffer );
	 break;
	 
      case PROP_URI:
	 if( sink->port == BFALSE )
	    g_value_set_string( value, NULL );
	 else
	    g_value_set_string( value,
				BSTRING_TO_STRING( INPUT_PORT_NAME( sink->port ) ) );
	 break;

      case PROP_AUTOCLOSE:
	 g_value_set_boolean( value, sink->autoclose );
	 break;
   }
}


static gboolean bgl_gst_port_sink_query (GstPad * pad, GstQuery * query);

static GstFlowReturn bgl_gst_port_sink_render (GstBaseSink * sink,
    GstBuffer * buffer);

static gboolean bgl_gst_port_sink_start (GstBaseSink * basesink);
static gboolean bgl_gst_port_sink_stop (GstBaseSink * basesink);

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_gst_port_sink_base_init ...                                  */
/*---------------------------------------------------------------------*/
static void
bgl_gst_port_sink_base_init( gpointer g_class ) {
   static GstElementDetails element_details = {
      "Bigloo output-port sink",
      "Sink",
      "Write stream to a bigloo port",
      "Cyprien Nicolas <Cyprien.Nicolas@sophia.inria.fr>"
   };
   GstElementClass *element_class = GST_ELEMENT_CLASS( g_class );

   gst_element_class_add_pad_template(
      element_class, gst_static_pad_template_get( &sinktemplate ) );
   
   gst_element_class_set_details(
      element_class, &element_details );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_gst_port_sink_class_init ...                                 */
/*---------------------------------------------------------------------*/
static void
bgl_gst_port_sink_class_init( BglPortSinkClass *klass ) {
   GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
   GstBaseSinkClass *gstbasesink_class = GST_BASE_SINK_CLASS (klass);

   gobject_class->set_property =
      GST_DEBUG_FUNCPTR( bgl_gst_port_sink_set_property );
   gobject_class->get_property =
      GST_DEBUG_FUNCPTR( bgl_gst_port_sink_get_property );
  
   g_object_class_install_property( gobject_class, PROP_PORT,
       g_param_spec_pointer( "port", "output-port",
			     "output-port to write",
			     G_PARAM_READWRITE ) );
   g_object_class_install_property( gobject_class, PROP_BUFFER,
       g_param_spec_pointer( "buffer", "string",
			     "a output-port buffer",
			     G_PARAM_READWRITE ) );
   g_object_class_install_property( gobject_class, PROP_URI,
       g_param_spec_string( "uri", "uri",
			    "open an output-port to uri to write data", NULL,
			    G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS ) );
   g_object_class_install_property( gobject_class, PROP_AUTOCLOSE,
       g_param_spec_boolean( "autoclose", "boolean",
			     "Auto-close the output port when stopped",
			     DEFAULT_AUTOCLOSE,
			     G_PARAM_READWRITE ) );
  
   gstbasesink_class->get_times = NULL;
   gstbasesink_class->start = GST_DEBUG_FUNCPTR( bgl_gst_port_sink_start );
   gstbasesink_class->stop = GST_DEBUG_FUNCPTR( bgl_gst_port_sink_stop );
   gstbasesink_class->render = GST_DEBUG_FUNCPTR( bgl_gst_port_sink_render );
   gstbasesink_class->event = NULL;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_gst_port_sink_init ...                                       */
/*---------------------------------------------------------------------*/
static void
bgl_gst_port_sink_init( BglPortSink *portsink, BglPortSinkClass *g_class ) {
   GstPad *pad;

   pad = GST_BASE_SINK_PAD( portsink );

   gst_pad_set_query_function( pad,
			       GST_DEBUG_FUNCPTR( bgl_gst_port_sink_query ) );

   portsink->port = BFALSE;
   portsink->buffer = BFALSE;
   portsink->uri = NULL;
   portsink->autoclose = DEFAULT_AUTOCLOSE;
   portsink->bytes_written = 0;

   gst_base_sink_set_sync( GST_BASE_SINK (portsink), FALSE );
}

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    bgl_gst_port_sink_query ...                                      */
/*---------------------------------------------------------------------*/
static gboolean
bgl_gst_port_sink_query( GstPad * pad, GstQuery * query ) {
   BglPortSink *self;
   GstFormat format;

   self = BGL_GST_PORT_SINK( GST_PAD_PARENT( pad ) );

   switch( GST_QUERY_TYPE( query ) ) {
      case GST_QUERY_POSITION:
	 gst_query_parse_position( query, &format, NULL );
	 
	 switch( format ) {
	    case GST_FORMAT_DEFAULT:
	    case GST_FORMAT_BYTES:
	       gst_query_set_position( query, GST_FORMAT_BYTES, self->bytes_written );
	       return TRUE;
	    default:
	       return FALSE;
	 }

      case GST_QUERY_FORMATS:
	 gst_query_set_formats( query, 2, GST_FORMAT_DEFAULT, GST_FORMAT_BYTES );
	 return TRUE;

      default:
	 return gst_pad_query_default( pad, query );
   }
}

/*---------------------------------------------------------------------*/
/*    static GstFlowReturn                                             */
/*    bgl_gst_port_sink_render ...                                     */
/*---------------------------------------------------------------------*/
static GstFlowReturn
bgl_gst_port_sink_render( GstBaseSink *sink, GstBuffer *buffer ) {
   BglPortSink *portsink;
   guint size;
   guint8 *data;
   gint written;
  
   portsink = BGL_GST_PORT_SINK( sink );

   size = GST_BUFFER_SIZE( buffer );
   data = GST_BUFFER_DATA( buffer );

   GST_DEBUG_OBJECT( portsink, "writing %u bytes", size );

redo:
   if (size > 0 && data != NULL) {
      written = OUTPUT_PORT( portsink->port ).syswrite( portsink->port, data, size > 32768 ? 32768 : size );

      if( written < 0 ) {
	 GST_ELEMENT_ERROR( portsink, RESOURCE, WRITE,
			    ("Error while writing to port <%p>.", portsink->port),
			    ("%s", g_strerror( errno )) );
    
	 return GST_FLOW_ERROR;
      }
    
      size -= written;
      data += written;
      portsink->bytes_written += written;

      GST_DEBUG_OBJECT( portsink, "wrote %d bytes, %d left", written, size );

      if (size > 0)
	 goto redo;

      return GST_FLOW_OK;
   }
  
   return GST_FLOW_UNEXPECTED;
}

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    bgl_gst_port_sink_start ...                                      */
/*---------------------------------------------------------------------*/
static gboolean
bgl_gst_port_sink_start( GstBaseSink *basesink ) {
   BglPortSink *sink = BGL_GST_PORT_SINK( basesink );

   if( !OUTPUT_PORTP( sink->port ) ) {
      if( sink->uri == NULL ) {
	 GST_ELEMENT_ERROR( sink, RESOURCE, WRITE,
			    ("Error while writing to file"),
			    ("No file or port element set") );
	 return FALSE;
      } else {
	 obj_t b = sink->buffer != BFALSE ?
	    sink->buffer : make_string_sans_fill( 1024 );
	 sink->port = bgl_open_output_file( string_to_bstring( sink->uri), b );
	 bglgst_register_port( sink->port );
      }
   }
   return TRUE;
}

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    bgl_gst_port_sink_stop ...                                       */
/*---------------------------------------------------------------------*/
static gboolean
bgl_gst_port_sink_stop( GstBaseSink * basesink ) {
   BglPortSink *sink = BGL_GST_PORT_SINK( basesink );
  
   bglgst_unregister_port( sink->port );
  
   if( sink->autoclose == TRUE || sink->uri ) {
      bgl_close_output_port( sink->port );
      sink->uri = 0L;
   }
  
   return TRUE;
}


      
    
  
