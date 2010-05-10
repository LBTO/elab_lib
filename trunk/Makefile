# Makefile for all the Adopt software

SUBSYSTEMS_TO_BUILD = lib MsgD-RTDB StagesCtrl WFSCtrl TTCtrl BCUCtrl idl \
                      SimpleMotorCtrl GUI GUIs ChUI Tester scripts MirrorDiagnostic \
                      PowerCtrl idl_control PyModules MasterDiagnostic MirrorCtrl Housekeeper AdamHousekeeper \
                      CameraLensCtrl Arbitrator HexapodCtrl DiagnApp SysCheck idl_wrapper \
                      OptLoopDiagnostic
					  #InterferometerCtrl 

SUBSYSTEMS_TO_INSTALL = MsgD-RTDB StagesCtrl WFSCtrl TTCtrl BCUCtrl idl\
                      SimpleMotorCtrl GUI GUIs ChUI Tester scripts MirrorDiagnostic \
                      PowerCtrl idl_control PyModules MasterDiagnostic MirrorCtrl Housekeeper AdamHousekeeper \
                      CameraLensCtrl StartUp Arbitrator HexapodCtrl DiagnApp SysCheck idl_wrapper \
                      OptLoopDiagnostic
					  #InterferometerCtrl

SUBSYSTEMS_TO_CLEAN =      lib MsgD-RTDB StagesCtrl WFSCtrl TTCtrl BCUCtrl idl \
                      SimpleMotorCtrl GUI GUIs ChUI Tester scripts MirrorDiagnostic \
                      PowerCtrl idl_control PyModules MasterDiagnostic MirrorCtrl Housekeeper AdamHousekeeper \
                      CameraLensCtrl Arbitrator HexapodCtrl DiagnApp idl_wrapper SysCheck StartUp \
                      OptLoopDiagnostic
					  #InterferometerCtrl 

all: 
	for dir in ${SUBSYSTEMS_TO_BUILD}; do \
	 (cd $$dir; ${MAKE} all) || break; \
	done

contrib:
	(cd contrib; ${MAKE} all)

install: all install-paths
	for dir in ${SUBSYSTEMS_TO_INSTALL}; do \
	 (cd $$dir; ${MAKE} install) || break; \
	done


install-paths:
	install -d $(ADOPT_ROOT)
	install -d $(ADOPT_ROOT)/bin
	install -d $(ADOPT_ROOT)/idl
	install -d $(ADOPT_ROOT)/idl/dlm
	install -d $(ADOPT_ROOT)/conf
	install -d $(ADOPT_ROOT)/calib
	install -d $(ADOPT_ROOT)/py

install-conf:
	@echo "!!!WARNING!!! Installing the conf directory will overwrite your current configuration. Continue? [yes/no]"
	@read answer && if [ "$$answer" = "yes" ] ; then install -d $(ADOPT_ROOT) && \
             rsync -avC --delete conf $(ADOPT_ROOT) && echo "configuration installed"; else echo "Nothing done"; fi

install-calib:
	@echo "!!!WARNING!!! Installing the calib directory will overwrite your current calibration. Continue? [yes/no]"
	@read answer && if [ "$$answer" = "yes" ] ; then install -d $(ADOPT_ROOT)/calib/$(ADOPT_SIDE) && \
             install -d $(ADOPT_ROOT)/calib && \
             rsync -avC --delete calib $(ADOPT_ROOT) && echo "calibration installed"; else echo "Nothing done"; fi

install-contrib:
	(cd contrib; $(MAKE) install)

sensor:
	(cd SensorCheck; $(MAKE))

clean:
	rm -f *.pyc
	rm -f adopt_environment.sh
	rm -f install_environment.sh
	rm -f *.tgz
	for dir in ${SUBSYSTEMS_TO_CLEAN}; do \
	 (cd $$dir; ${MAKE} clean) || break; \
	done

clean-contrib:
	(cd contrib; ${MAKE} clean)

kit:
	./makekit.sh
