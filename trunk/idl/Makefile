

all: errordb.pro msgcodes.pro vartypes.pro

errordb.pro: ../lib/base/errordb.txt
	(cd $(ADOPT_SOURCE)/lib/base; ../makerrordb.py errordb.txt)

msgcodes.pro: ../lib/base/msgcodes.txt
	(cd $(ADOPT_SOURCE)/lib/base; ../makerrordb.py msgcodes.txt)

vartypes.pro: ../lib/base/vartypes.txt
	(cd $(ADOPT_SOURCE)/lib/base; ../makerrordb.py vartypes.txt)

install:
	install -d $(ADOPT_ROOT)/idl
	install init_idl_ao.pro $(ADOPT_ROOT)/idl
	install errordb.pro $(ADOPT_ROOT)/idl
	install msgcodes.pro $(ADOPT_ROOT)/idl
	install vartypes.pro $(ADOPT_ROOT)/idl
	cp -a oaa_lib $(ADOPT_ROOT)/idl
	cp -a wfs_lib $(ADOPT_ROOT)/idl
	cp -a adsec_lib $(ADOPT_ROOT)/idl

clean:
	rm -f errordb.pro msgcodes.pro vartypes.pro
