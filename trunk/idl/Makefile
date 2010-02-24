

all: errordb.pro msgcodes.pro vartypes.pro

errordb.pro: ../lib/base/errordb.txt
	(cd $(ADOPT_SOURCE)/lib/base; $(MAKE) errordb.pro)
	cp $(ADOPT_SOURCE)/lib/base/errordb.pro .

msgcodes.pro: ../lib/base/msgcodes.txt
	(cd $(ADOPT_SOURCE)/lib/base; $(MAKE) msgcodes.pro)
	cp $(ADOPT_SOURCE)/lib/base/msgcodes.pro .

vartypes.pro: ../lib/base/vartypes.txt
	(cd $(ADOPT_SOURCE)/lib/base; $(MAKE) vartypes.pro)
	cp $(ADOPT_SOURCE)/lib/base/vartypes.pro .

install: all
	install -d $(ADOPT_ROOT)/idl
	install init_idl_ao.pro $(ADOPT_ROOT)/idl
	install errordb.pro $(ADOPT_ROOT)/idl
	install msgcodes.pro $(ADOPT_ROOT)/idl
	install vartypes.pro $(ADOPT_ROOT)/idl
	rsync -aC oaa_lib $(ADOPT_ROOT)/idl
	rsync -aC wfs_lib $(ADOPT_ROOT)/idl
	rsync -aC adsec_lib $(ADOPT_ROOT)/idl

clean:
	rm -f errordb.pro msgcodes.pro vartypes.pro
