#*=====================================================================*/
#*    serrano/prgm/project/bigloo/bde/Makefile                         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Mon Jun  8 08:36:43 1998                          */
#*    Last change :  Wed Nov  4 11:58:58 2015 (serrano)                */
#*    Copyright   :  1998-2015 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The Makefile for the Bigloo development environment.             */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    The default configuration                                        */
#*---------------------------------------------------------------------*/
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# etags for info
ETAGS		= etags
# etags flags for info scanning
ETFLAGS		= --language=none \
                  --r '/^ - \(essential \|library \|bigloo \)?\(procedure\|syntax\): [^ ]+/' \
                  --r '/^ - bigloo module clause: [^ ]+/' \
                  --r '/^ - bigloo rgc procedure: [^ ]+/' \
                  --r '/^ - bigloo generic: [^ ]+/'
# the shell to be used
SHELL           = /bin/sh

#*---------------------------------------------------------------------*/
#*    Compilation flags                                                */
#*---------------------------------------------------------------------*/
#BFLAGS		= -jvm -jvm-jar -O2 -fsharing -unsafe
BFLAGS		= -O2 -fsharing -unsafe $(SHRD_BDE_OPT)
EFLAGS		=

#*---------------------------------------------------------------------*/
#*    Sources and Objects                                              */
#*---------------------------------------------------------------------*/
#*--- afile -----------------------------------------------------------*/
AFILE_FILE	= afile
AFILE_OBJ	= $(AFILE_FILE:%=afile/%.o)

#*--- jfile -----------------------------------------------------------*/
JFILE_FILE	= jfile
JFILE_OBJ	= $(JFILE_FILE:%=jfile/%.o)

#*--- bpp -------------------------------------------------------------*/
BPP_FILE	= bpp reader
BPP_OBJ		= $(BPP_FILE:%=bpp/%.o)

#*--- btags -----------------------------------------------------------*/
BTAGS_FILE	= btags
BTAGS_OBJ	= $(BTAGS_FILE:%=btags/%.o)

#*--- bdepend ---------------------------------------------------------*/
BDEPEND_FILE	= bdepend
BDEPEND_OBJ	= $(BDEPEND_FILE:%=bdepend/%.o)

#*--- bmake -----------------------------------------------------------*/
BMAKE_FILE	= bmake template
BMAKE_OBJ	= $(BMAKE_FILE:%=bmake/%.o)

#*--- bprof -----------------------------------------------------------*/
BPROF_FILE	= bprof
BPROF_OBJ	= $(BPROF_FILE:%=bprof/%.o)

#*--- mco -------------------------------------------------------------*/
MCO_FILE	= mco
MCO_OBJ		= $(MCO_FILE:%=mco/%.o)

#*--- jas -------------------------------------------------------------*/
JAS_LINKFILE	= lib classfile peep opcode wide labels stack produce as \
                  profile
JAS_FILE	= jas 
JAS_AFILE	= jas/.afile
JAS_LINKSRC	= $(JAS_LINKFILE:%=jas/%.scm)
JAS_LINKOBJ	= $(JAS_LINKFILE:%=jas/%.o)
JAS_OBJ		= $(JAS_FILE:%=jas/%.o)

#*---------------------------------------------------------------------*/
#*    Population                                                       */
#*---------------------------------------------------------------------*/
OBJECTS		= $(AFILE_OBJ) $(BPP_OBJ) $(BTAGS_OBJ) $(BDEPEND_OBJ) \
                  $(BMAKE_OBJ) $(BPROF_OBJ) $(MCO_OBJ) $(JFILE_OBJ) \
                  $(JAS_OBJ)
SOURCES_FILES	= $(OBJECTS:%.o=%.scm)

POPULATION	= $(SOURCES_FILES) \
                  bpp/README \
	          bmake/makefile.appli \
	          bmake/makefile.library \
                  jas/jas.tex \
                  Makefile

#*---------------------------------------------------------------------*/
#*     Suffixes ...                                                    */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .sch .o .el .elc

#*---------------------------------------------------------------------*/
#*    all                                                              */
#*---------------------------------------------------------------------*/
all: $(BOOTBINDIR)/bglafile \
     $(BOOTBINDIR)/bgljfile \
     $(BOOTBINDIR)/bgldepend \
     $(BOOTBINDIR)/bglmco \
     $(BOOTBINDIR)/bglmake \
     $(BOOTBINDIR)/bglpp \
     $(BOOTBINDIR)/bgltags \
     $(BOOTBINDIR)/bglprof \
     $(BOOTBINDIR)/bgljas \
     dobmem

boot:
	$(MAKE) all BFLAGS="$(BFLAGS) -ld-relative"

dobmem: $(BOOTBINDIR)/bglafile
	@ if [ "$(LDPRELOADSUPPORT)" = "yes" \
                -a "$(PTHREADLIBS)" != "no" ]; then \
	    (cd bmem && $(MAKE)) || exit 1; \
	  fi

$(BOOTBINDIR)/bglafile: $(AFILE_OBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^

afile.class: afile/afile.scm
	(cd afile; \
         $(BIGLOO) -c -jvm $(BFLAGS) $(EFLAGS) afile.scm -o afile.class; \
         mv afile.class ..)

$(BOOTBINDIR)/bgljfile: $(JFILE_OBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^

jfile.class: jfile/jfile.scm
	(cd jfile; $(BIGLOO) -c -jvm $(BFLAGS) $(EFLAGS) jfile.scm -o jfile.class; \
         mv jfile.class ..)

$(BOOTBINDIR)/bgldepend: $(BDEPEND_OBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^

$(BOOTBINDIR)/bglmake: $(BMAKE_OBJ)
	$(BIGLOO) -no-hello $(BFLAGS) $(EFLAGS) -o $@ $^

$(BOOTBINDIR)/bglprof: $(BPROF_OBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^

$(BOOTBINDIR)/bglmco: $(MCO_OBJ) ../comptime/Module/checksum.o
	$(BIGLOO) -no-hello $(BFLAGS) $(EFLAGS) -o $@ $^

$(BOOTBINDIR)/bglpp: $(BPP_OBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^

$(BOOTBINDIR)/bgltags: $(BTAGS_OBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $^

jas: $(BOOTBINDIR)/bgljas
$(BOOTBINDIR)/bgljas: $(JAS_LINKSRC) $(JAS_AFILE) $(JAS_OBJ) $(JAS_LINKOBJ)
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -o $@ $(JAS_OBJ) $(JAS_LINKOBJ)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
install: all
	if [ $(BINDIR) != $(BOOTBINDIR) ]; then \
	  cp $(BOOTBINDIR)/bglafile$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/bglafile$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bglafile$(EXE_SUFFIX); \
	  cp $(BOOTBINDIR)/bglafile.sh $(DESTDIR)$(BINDIR)/bglafile.sh; \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bglafile.sh; \
	  cp $(BOOTBINDIR)/bgljfile$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/bgljfile$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bgljfile$(EXE_SUFFIX); \
	  cp $(BOOTBINDIR)/bgldepend$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/bgldepend$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bgldepend$(EXE_SUFFIX); \
	  cp $(BOOTBINDIR)/bglmake$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/bglmake$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bglmake$(EXE_SUFFIX); \
	  cp $(BOOTBINDIR)/bglprof$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/bglprof$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bglprof$(EXE_SUFFIX); \
	  cp $(BOOTBINDIR)/bglmco$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/bglmco$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bglmco$(EXE_SUFFIX); \
	  cp $(BOOTBINDIR)/bglpp$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/bglpp$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bglpp$(EXE_SUFFIX); \
	  cp $(BOOTBINDIR)/bgltags$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/bgltags$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bgltags$(EXE_SUFFIX); \
	  cp $(BOOTBINDIR)/bgljas$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/bgljas$(EXE_SUFFIX); \
          chmod $(MODEXE) $(DESTDIR)$(BINDIR)/bgljas$(EXE_SUFFIX); \
	  if [ "$(LDPRELOADSUPPORT)" = "yes" \
               -a "$(PTHREADLIBS)" != "no" ]; then \
	    (cd bmem; $(MAKE) install) || exit 1; \
          fi; \
        fi

#*---------------------------------------------------------------------*/
#*    uninstall                                                        */
#*---------------------------------------------------------------------*/
uninstall:
	- $(RM) -f $(DESTDIR)$(BINDIR)/bglafile$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/bgljfile$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/bgldepend$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/bglmake$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/bglprof$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/bglmco$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/bglpp$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/bgltags$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/bgljas$(EXE_SUFFIX)
	- (cd bmem; $(MAKE) uninstall)

#*---------------------------------------------------------------------*/
#*    touchall                                                         */
#*---------------------------------------------------------------------*/
touchall:
	touch $(SOURCE_FILES)

#*---------------------------------------------------------------------*/
#*     clean ...                                                       */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	@- $(RM) -f $(OBJECTS)
	@- $(RM) -f $(JAS_LINKOBJ)
	@- $(RM) -f $(JAS_LINKFILE:%=jas/%.c)
	@- $(RM) -f $(OBJECTS:%.o=%.c)
	@- $(RM) -f .afile
	@- $(RM) -f jas/.afile
	@- $(RM) -f *.class
	@ find . \( -name '*[~%]' \
                       -o -name '.??*[~%]' \
                       -o -name '#*#' \
                       -o -name '?*#' \
                       -o -name \*core \) \
                     -type f -exec rm {} \;   
	@ (cd bmem; $(MAKE) clean)
	@ $(BEEP)
	@ echo "cleanup done..."
	@ echo "-------------------------------"

cleanall: clean
	@- $(RM) -f $(BOOTBINDIR)/bglafile 
	@- $(RM) -f $(BOOTBINDIR)/bgljfile 
	@- $(RM) -f $(BOOTBINDIR)/bgldepend 
	@- $(RM) -f $(BOOTBINDIR)/bglmake 
	@- $(RM) -f $(BOOTBINDIR)/bglprof
	@- $(RM) -f $(BOOTBINDIR)/bglmco
	@- $(RM) -f $(BOOTBINDIR)/bglpp 
	@- $(RM) -f $(BOOTBINDIR)/bgltags
	@- $(RM) -f $(BOOTBINDIR)/bgljas
	@- $(RM) -f $(JAS_LINKSRC)

distclean: cleanall

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    Prepare Bde for a distribution                                   */
#*---------------------------------------------------------------------*/
distrib:
	@ if [ `pwd` = $(BOOTDIR)/bde ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi

#*---------------------------------------------------------------------*/
#*    jvm                                                              */
#*---------------------------------------------------------------------*/
jvm: afile.class jfile.class

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*    -------------------------------------------------------------    */
#*    This entry is used by the bigloo/Makefile (the main Bigloo       */
#*    makefile) to get the list of file that populate a revision.      */
#*---------------------------------------------------------------------*/
.PHONY: pop
pop:
	@ echo $(POPULATION:%=bde/%)
	@ (cd bmem; $(MAKE) pop)

#*---------------------------------------------------------------------*/
#*     .scm --> .o                                                     */
#*---------------------------------------------------------------------*/
jas/%.o: jas/%.scm $(JAS_AFILE)
	$(BIGLOO) $(BFLAGS) -g $(EFLAGS) -c jas/$*.scm -o jas/$*.o -afile jas/.afile

%.o: %.scm
	$(BIGLOO) $(BFLAGS) -g $(EFLAGS) -c $*.scm -o $*.o

#*---------------------------------------------------------------------*/
#*     .scm --> .class                                                 */
#*---------------------------------------------------------------------*/
jas/%.class: jas/%.scm $(JAS_AFILE)
	$(BIGLOO) -jvm $(BFLAGS) $(EFLAGS) -c jas/$*.scm -o jas/$*.class -afile jas/.afile

%.class: %.scm
	$(BIGLOO) -jvm $(BFLAGS) $(EFLAGS) -c $*.scm -o $*.class

#*---------------------------------------------------------------------*/
#*    checksum.o                                                       */
#*---------------------------------------------------------------------*/
../comptime/Module/checksum.o: ../comptime/Module/checksum.scm
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -c $*.scm -o $*.o \
          -access engine_param mco/mco.scm

#*---------------------------------------------------------------------*/
#*    jas link files                                                   */
#*---------------------------------------------------------------------*/
jas/%.scm: ../comptime/Jas/%.scm
	cp $< $@

jas/classfile.scm: ../comptime/Jas/classfile.scm
	cat $< | sed 's|Jas/classfile.sch|jas/classfile.sch|' > $@ || exit $@

jas/classfile.scm: jas/classfile.sch

jas/classfile.sch: ../comptime/Jas/classfile.sch
	cp $< $@

#*---------------------------------------------------------------------*/
#*    Jas afile                                                        */
#*---------------------------------------------------------------------*/
$(JAS_AFILE): $(BGLBUILDBINDIR)/bglafile $(JAS_LINKSRC)
	(cd jas; $(BGLAFILE) *.scm -o ../$(JAS_AFILE))

#*---------------------------------------------------------------------*/
#*    bmake dependencies                                               */
#*---------------------------------------------------------------------*/
bmake/template.o: bmake/makefile.library bmake/makefile.appli
