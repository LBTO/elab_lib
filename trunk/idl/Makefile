#clean:
#	rm -rf  $(ADOPT_SOURCE)/idl/errordb.pro
#	rm -rf  $(ADOPT_SOURCE)/idl/msgcodes.pro
#	rm -rf  $(ADOPT_SOURCE)/idl/vartypes.pro
#
all: errordb.pro msgcodes.pro vartypes.pro

errordb.pro: ../lib/base/errordb.txt
	$(Q)$(MAKE) $(MAKEOPTS) -C $(ADOPT_SOURCE)/lib/base errordb.pro
	cp $(ADOPT_SOURCE)/lib/base/errordb.pro .

msgcodes.pro: ../lib/base/msgcodes.txt
	$(Q)$(MAKE) $(MAKEOPTS) -C $(ADOPT_SOURCE)/lib/base msgcodes.pro
	cp $(ADOPT_SOURCE)/lib/base/msgcodes.pro .

vartypes.pro: ../lib/base/vartypes.txt
	$(Q)$(MAKE) $(MAKEOPTS) -C $(ADOPT_SOURCE)/lib/base vartypes.pro
	cp $(ADOPT_SOURCE)/lib/base/vartypes.pro .

install: all
	rm -rf  $(ADOPT_ROOT)/idl/adsec_lib
	rm -rf  $(ADOPT_ROOT)/idl/elab_lib
	rm -rf  $(ADOPT_ROOT)/idl/wfs_lib
	install -d $(ADOPT_ROOT)/idl
	install init_idl_ao.pro $(ADOPT_ROOT)/idl
	install errordb.pro $(ADOPT_ROOT)/idl
	install msgcodes.pro $(ADOPT_ROOT)/idl
	install vartypes.pro $(ADOPT_ROOT)/idl
	rsync -aC elab_lib $(ADOPT_ROOT)/idl
	rsync -aC wfs_lib $(ADOPT_ROOT)/idl
	rsync -aC adsec_lib $(ADOPT_ROOT)/idl

clean:
