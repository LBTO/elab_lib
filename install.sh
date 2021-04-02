#!/bin/sh
#
# Elab_lib installation script.
#
# Usage:
#
# ./install.sh: install in default IDL library directory
#               (you will probably need root privileges)
#
# ./install.sh <path>: install in the specified path
#
# Written by: A: Puglisi, Jan 2020

case $# in
   0) 
      echo
      echo "Running IDL to detect the library dir..."
      echo
      IDLDIR=`/usr/local/bin/idl -e "print, pref_get('IDL_DIR')" | tail -1  2>/dev/null`
      INSTALLDIR=${IDLDIR}/lib/elab_lib
      ;;
   1) INSTALLDIR=$1 ;;
   *) echo "Too many arguments specified"
      exit 1
      ;;
esac

echo
echo "Elab_lib will be installed in this directory:"
echo
echo "--> ${INSTALLDIR} <--"
echo

read -p "Continue? (y/n) " yesno
first=`echo $yesno | cut -c 1`

if [ $first = "y" ]; then

    # A little ed magic...
    VERSIONFILE=/tmp/svnversion.txt
    echo -n "return,'" > $VERSIONFILE
    echo -n `svnversion` >> $VERSIONFILE
    echo "'" >> $VERSIONFILE

    ed -s elab_version.pro <<EOF
/;;; BEGIN GENERATED/+,/;;; END GENERATED/-d
/;;; BEGIN GENERATED/ r $VERSIONFILE
w
q
EOF

    echo
    echo -n "Installing in $INSTALLDIR ... "
    install -d $INSTALLDIR
    cp -a *.pro $INSTALLDIR

    echo "done!"
    echo

fi
