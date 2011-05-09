# General Supervisor makefile
include $(ADOPT_SOURCE)/makefile.gen


.PHONY : baselib otherlibs 

# LIB OBJECTS
# These archives shouldn't be linked directly: link ARCHIVES instead
OBJECTS = Logger.o RTDBvar.o stdconfig.o AOApp.o  \
          iolib.o commlib.o \
          UdpConnection.o TcpConnection.o \
          NaNutils.o Utils.o Reorder.o Paths.o \
          AdamLib.o AdamTestLib.o bcu_diag.o aoapplib.o \
          mastdiagnlib.o diagBuffer.o AdsecDiagnLib.o intmatlib.o telemetry.o 

# LIB ARCHIVES
# Only these archives should be linked by AOApp's
ARCHIVES = aoapp.a configlib.a netlib.a adamlib.a adamtestlib.a logger.a iolib.a mastdiagnlib.a adsecdiagnlib.a


################## TARGETS ###################

all: baselib $(OBJECTS) otherlibs AOStates.h AOStates.c $(ARCHIVES)

include $(ADOPT_LIB)/base/makefile.base

install:

# We can get away with BASE_COMPILE because it will not call this Makefile again
baselib: $(BASE_COMPILE)

# We cannot use AOSLIB_COMPILE etc. because they will call this Makefile again through their dependencies, causing an infinite loop
otherlibs:
	$(Q)$(MAKE) $(MAKEOPTS) -C aoslib
	$(Q)$(MAKE) $(MAKEOPTS) -C BcuLib
	$(Q)$(MAKE) $(MAKEOPTS) -C hwlib
	$(Q)$(MAKE) $(MAKEOPTS) -C idllib
	$(Q)$(MAKE) $(MAKEOPTS) -C IntfLib
	$(Q)$(MAKE) $(MAKEOPTS) -C arblib
	$(Q)$(MAKE) $(MAKEOPTS) -C drawlib
	$(Q)$(MAKE) $(MAKEOPTS) -C env




clean:
	$(Q)$(MAKE) $(MAKEOPTS) -C base clean
	$(Q)$(MAKE) $(MAKEOPTS) -C aoslib clean
	$(Q)$(MAKE) $(MAKEOPTS) -C BcuLib clean
	$(Q)$(MAKE) $(MAKEOPTS) -C hwlib clean
	$(Q)$(MAKE) $(MAKEOPTS) -C idllib clean
	$(Q)$(MAKE) $(MAKEOPTS) -C IntfLib clean
	$(Q)$(MAKE) $(MAKEOPTS) -C arblib clean
	$(Q)$(MAKE) $(MAKEOPTS) -C drawlib clean
	$(Q)rm -f *.aux *.dvi *.log *.toc *.tmp
	$(Q)rm -f *.o *.d *.a
	$(Q)rm -f $(ARCHIVES)

tags:
	ctags -t *.[ch]

docs: AOApp.pdf 

AOApp.pdf:  AOApp.cpp AOApp.h AOApp.dpu
	dpu -pdf AOApp


################### OBJECTS ##################

# The presence of "baselib" dependency force the rebuild of the target any time.
# Moreover, doesn't make any sense to compile a single object directly!
#AOApp.o: baselib AOApp.cpp AOStates.h AOStates.c AOApp.h AOExcept.h 
AOApp.o: AOApp.cpp AOStates.h AOStates.c AOApp.h AOExcept.h 
aoapplib.o: aoapplib.cpp aoapplib.h 
AdsecDiagnLib.o: AdsecDiagnLib.cpp AdsecDiagnLib.h AdsecDiagnLibStructs.h AdSecConstants.h 
RTDBvar.o: RTDBvar.cpp RTDBvar.h aotypes.h
Logger.o: Logger.cpp Logger.h AOExcept.h
stdconfig.o: stdconfig.cpp stdconfig.h
Utils.o: Utils.cpp Utils.h aotypes.h
NaNutils.o: NaNutils.cpp NaNutils.h
AdamLib.o: AdamLib.cpp AdamLib.h 
AdamTestLib.o: AdamTestLib.cpp AdamTestLib.h 
UdpConnection.o: UdpConnection.cpp UdpConnection.h
TcpConnection.o: TcpConnection.cpp TcpConnection.h
commlib.o: commlib.c commlib.h 
iolib.o: iolib.c iolib.h 
eev39.o: eev39.c eev39.h 
bcucommand.o: bcucommand.cpp bcucommand.h
Hexapod.o: Hexapod.cpp Hexapod.h TcpConnection.o Logger.o stdconfig.o
MastDiagnLib.o: MastDiagnLib.cpp MastDiagnLib.h
bcu_diag.o: bcu_diag.cpp bcu_diag.h diagBuffer.h iolib.o
diagBuffer.o: diagBuffer.cpp diagBuffer.h
Reorder.o: Reorder.cpp Reorder.h
intmatlib.o: intmatlib.cpp intmatlib.h
telemetry.o: telemetry.c telemetry.h

AOStates.c AOStates.h: AOStates.txt
	./makerrordb.py AOStates.txt -py AOStates
	mv -f AOStates.py ../PyModules/AdOpt


################## LIBRARY ARCHIVES ###################
# Creates some library archives that can used (compiled and 
# linked) by client applications  using the corresponding
# "makefile.xyz"
# 
# NOTE 1: others library archives are available in lib's subfolders.
# NOTE 2: all library archives depends on 'baselib' for some dinamically
#		  created header files (i.e. errordb.h, msgcodes.h, ...)

# Configlib
configlib.a: stdconfig.o Paths.o
	$(AR) configlib.a stdconfig.o Paths.o
	$(RANLIB) configlib.a

# AOAPP 
aoapp.a: AOApp.o RTDBvar.o aoapplib.o AOStates.c AOStates.h
	$(AR) aoapp.a AOApp.o RTDBvar.o aoapplib.o
	$(RANLIB) aoapp.a

logger.a: Logger.o Utils.o NaNutils.o
	$(AR) logger.a Logger.o Utils.o NaNutils.o
	$(RANLIB) logger.a

# NETLIB
netlib.a: TcpConnection.o UdpConnection.o
	$(AR) netlib.a TcpConnection.o UdpConnection.o
	$(RANLIB) netlib.a

# ADAMLIB
adamlib.a: AdamLib.o
	$(AR) adamlib.a AdamLib.o
	$(RANLIB) adamlib.a

# ADAMTESTLIB
adamtestlib.a: AdamTestLib.o
	$(AR) adamtestlib.a AdamTestLib.o
	$(RANLIB) adamtestlib.a

# IOLIB
iolib.a: iolib.o
	$(AR) iolib.a iolib.o
	$(RANLIB) iolib.a

# MASTDIAGNLIB
mastdiagnlib.a: mastdiagnlib.o
	$(AR) mastdiagnlib.a mastdiagnlib.o
	$(RANLIB) mastdiagnlib.a

# ADSECDIAGNLIB
adsecdiagnlib.a: AdsecDiagnLib.o
	$(AR) adsecdiagnlib.a AdsecDiagnLib.o
	$(RANLIB) adsecdiagnlib.a

