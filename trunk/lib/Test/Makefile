include  $(ADOPT_SOURCE)/makefile.gen

TARGETS = Test_AOApp
all: $(TARGETS)
      
# Dependencies      
#include $(ADOPT_SOURCE)/lib/base/makefile.base
#include $(ADOPT_SOURCE)/lib/makefile.logger
#include $(ADOPT_SOURCE)/lib/makefile.configlib
include $(ADOPT_SOURCE)/lib/makefile.aoapp


Test_AOApp: aoapp_compile Test_AOApp.o 
	$(LINKPP) -o Test_AOApp Test_AOApp.o $(AOAPP_LNK)

clean:
	rm -f *.o *.d $(TARGETS)

