#!/bin/bash
#
# Shell procedure to make the AO Software exporting kit

echo
echo "!!!WARNING to build the export kit, a full clean must be performed."
echo
echo -n "Continue? [yes/no] "
read answer && if [ "$answer" = "yes" ] ; then \
   make clean ; \
   make clean-contrib ; \
   DATE=`date +%Y-%m-%d` ; \
   SOURCETAR=AoSup_$DATE.tgz ; \
   CONFTAR=AoConf_$DATE.tgz ; \
   tar cvfz /tmp/$SOURCETAR --exclude=.svn --exclude CVS --exclude=calib --exclude=conf .  ; \
   mv /tmp/$SOURCETAR .  ; \
   tar cvfz /tmp/$CONFTAR --exclude=.svn --exclude CVS ./conf ./calib ; \
   mv /tmp/$CONFTAR .  ; \
   echo; echo "Now you may want to move the kit files: $SOURCETAR, $CONFTAR"; \
   echo "somewhere else: otherwise they will be deleted by next \"make clean\""; echo ;\
else echo "Nothing done"; fi
