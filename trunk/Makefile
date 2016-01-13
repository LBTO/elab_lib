# Makefile for all the Adopt software

include makefile.gen

SUBSYSTEMS =          MsgD-RTDB StagesCtrl WFSCtrl TTCtrl BCUCtrl idl DiagnApp \
                      SimpleMotorCtrl GUI GUIs  ChUI Tester scripts MirrorDiagnostic \
                      PowerCtrl idl_control PyModules MasterDiagnostic MirrorCtrl Housekeeper AdamHousekeeper \
                      CameraLensCtrl Arbitrator HexapodCtrl SysCheck idl_wrapper \
                      OptLoopDiagnostic StartUp AnemometerMon lnIF TrueSense # DBFiller
					  #InterferometerCtrl 



all: $(SUBSYSTEMS)

#Dependencies between subdirs
$(SUBSYSTEMS): lib
MirrorDiagnostic: DiagnApp

lib $(SUBSYSTEMS):
	$(Q)$(MAKE) $(MAKEOPTS) -C $@

.PHONY:  install lib $(SUBSYSTEMS) contrib

contrib:
	$(Q)$(MAKE) $(MAKEOPTS) -C contrib all

install: install-paths
	for dir in ${SUBSYSTEMS}; do \
	 (cd $$dir; ${MAKE} install) || break; \
	done


install-paths:
	install -d $(ADOPT_ROOT)
	install -d $(ADOPT_ROOT)/bin
	install -d $(ADOPT_ROOT)/idl
	install -d $(ADOPT_ROOT)/idl/dlm
	install -d $(ADOPT_ROOT)/py

install-contrib:
	(cd contrib; $(MAKE) install)

sensor:
	(cd SensorCheck; $(MAKE))

clean: 
	rm -f *.pyc
	rm -f adopt_environment.sh
	rm -f install_environment.sh
	rm -f *.tgz
	for dir in lib ${SUBSYSTEMS}; do \
	 (cd $$dir; ${MAKE} clean) || break; \
	done

clean-contrib:
	$(Q)$(MAKE) $(MAKEOPTS) -C contrib clean

kit:
	./makekit.sh
