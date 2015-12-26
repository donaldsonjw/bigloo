
#*=====================================================================*/
#*    serrano/prgm/project/bigloo/arch/debian/makedeb.sh.in            */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed May 23 05:45:55 2012                          */
#*    Last change :  Tue Nov  4 08:16:10 2014 (serrano)                */
#*    Copyright   :  2012-14 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    Script to build the debian Bigloo packages                       */
#*=====================================================================*/

# configuration and variables
version=4.2c
minor=

bglprefix=/opt/bigloo

repodir=/users/serrano/prgm/distrib
basedir=`dirname $0`
bglconfigureopt=

#* depend=libpcre3                                                    */
#* builddepend=libpcre3-dev                                           */
builddepend=
depend=

fakeroot=fakeroot

libs="sqlite ssl alsa flac wav mpg123 gstreamer avahi"

while : ; do
  case $1 in
    "")
      break;;
    -h|--help)
      echo "usage makedeb.sh [-l LIB] [--depend DEP] [--builddepend DEP] opt1 opt2 ...";
      exit 1;;
    -l|--lib)
      shift;
      libs="$1 $libs";;
    --depend)
      shift;
      depend="$1 $depend";;
    --builddepend)
      shift;
      builddepend="$1 $builddepend";;
    --prefix)
      shift;
      bglprefix=$1;;
    --fakeroot)
      shift;
      fakeroot=$1;;
    --version)
      shift;
      version=$1;;
    *)
      bglconfigureopt="$1 $bglconfigureopt";;

  esac
  shift
done

if [ "$REPODIR " != " " ]; then
  repodir=$REPODIR;
fi

pkg=bigloo

maemo=`pkg-config maemo-version --modversion 2> /dev/null`
if [ $? = 0 ]; then
  debian=maemo`echo $maemo | sed -e "s/[.].*$//"`
  libs="sqlite ssl alsa gstreamer flac wav avahi mpg123"
else
  case `cat /etc/issue | awk '{ print $1 }'` in
    Debian)
      debian=debian;;
    Ubuntu)
      debian=ubuntu;;
    *)
      debian=debian;;
  esac
fi

if [ -f /usr/include/phidget21.h -o -f /usr/local/include/phidget21.h ]; then
  libs="phidget $libs";
fi

curdir=`pwd`

cd $curdir

/bin/rm -rf build.$pkg
mkdir build.$pkg
cd build.$pkg

tar xfz $repodir/bigloo$version$minor.tar.gz
mv bigloo$version$minor bigloo-$version

cp $repodir/bigloo$version$minor.tar.gz bigloo-$version.tar.gz
cd bigloo-$version

dh_make -C gpl -s -e Manuel.Serrano@inria.fr -f ../bigloo-$version.tar.gz <<EOF

EOF

if [ ! -f $basedir/makedeb.sh ]; then
  echo "ERROR: Cannot find \"$basedir/makedeb.sh\"" >&2 
  echo "current directory is: $PWD" >&2
  echo "exiting..." >&2;
fi

# check which ssl is available
apt-cache search 'libssl' | grep 0\.9\.8
if [ $? = 0 ]; then
  libssldepend=libssl0.9.8
else
  apt-cache search 'libssl' | grep 1\.0\.0
  if [ $? = 0 ]; then
     libssldepend=libssl1.0.0
  else
     libssldepend=libssl
  fi
fi

# which avahi core is available
avahicore=`apt-cache search 'libavahi-core' | grep -v dev | awk '{print $1}'`
avahidepend="$avahicore, libavahi-common3, libavahi-client3"

# configure debian files
configure() {
  src=$1
  dest=$2

  cat $src \
    | sed -e "s/@BIGLOOVERSION@/$version$minor/g" \
    | sed -e "s|@BGLPREFIX@|$bglprefix|g" \
    | sed -e "s|@BGLCONFIGUREOPT@|$bglconfigureopt|g" \
    | sed -e "s|@EXTRADEPEND@|$depend|g" \
    | sed -e "s|@LIBSSLDEPEND@|$libssldepend|g" \
    | sed -e "s|@AVAHIDEPEND@|$avahidepend|g" \
    | sed -e "s|@EXTRABUILDDEPEND@|$builddepend|g" \
    > $dest.tmp
  for l in $libs; do
    cond="@IF`echo $l | tr [:lower:] [:upper:]`@"

    cat $dest.tmp \
      | sed -e "s|$cond ||" \
      > $dest.tmp2 \
      && mv $dest.tmp2 $dest.tmp
  done

  cat $dest.tmp \
      | sed -e "s|@[A-Z0-9]*@.*$||" \
      > $dest && /bin/rm $dest.tmp
}  

# debian specific configuration
for p in control rules postinst changelog; do
  if [ -f $basedir/$p.in ]; then
    configure $basedir/$p.in debian/$p
  elif [ -f $basedir/$p.$pkg ]; then
    configure $basedir/$p.$pkg debian/$p
  elif [ -f $basedir/$p ]; then
    configure $basedir/$p debian/$p
  fi
done

dpkg-buildpackage -r$fakeroot && 

if [ -d $repodir/$debian ]; then
  cp ../*_"$version""$minor"_*.deb $repodir/$debian
fi

cd $curdir

