GR			:	Astrophys.o ExtMath.o getparms.o GRMain.o IDLInterface.o Messages.o RFactorKappa.o RFactorN.o
				g++ -shared -o MWGRTransfer.so Astrophys.o ExtMath.o getparms.o GRMain.o IDLInterface.o Messages.o RFactorKappa.o RFactorN.o
Astrophys.o		:	Astrophys.cpp Astrophys.h ExtMath.h GRMain.h IDLInterface.h Messages.h RFactorKappa.h RFactorN.h
				g++ -c -O3 -fPIC Astrophys.cpp
ExtMath.o		:	ExtMath.cpp Astrophys.h ExtMath.h GRMain.h IDLInterface.h Messages.h RFactorKappa.h RFactorN.h
				g++ -c -O3 -fPIC ExtMath.cpp
getparms.o		:	getparms.cpp Astrophys.h ExtMath.h GRMain.h IDLInterface.h Messages.h RFactorKappa.h RFactorN.h
				g++ -c -O3 -fPIC getparms.cpp
GRMain.o		:	GRMain.cpp Astrophys.h ExtMath.h GRMain.h IDLInterface.h Messages.h RFactorKappa.h RFactorN.h
				g++ -c -O3 -fPIC GRMain.cpp
IDLInterface.o	:	IDLInterface.cpp Astrophys.h ExtMath.h GRMain.h IDLInterface.h Messages.h RFactorKappa.h RFactorN.h
				g++ -c -O3 -fPIC IDLInterface.cpp
Messages.o		:	Messages.cpp Astrophys.h ExtMath.h GRMain.h IDLInterface.h Messages.h RFactorKappa.h RFactorN.h
				g++ -c -O3 -fPIC Messages.cpp
RFactorKappa.o	:	RFactorKappa.cpp Astrophys.h ExtMath.h GRMain.h IDLInterface.h Messages.h RFactorKappa.h RFactorN.h
				g++ -c -O3 -fPIC RFactorKappa.cpp
RFactorN.o		:	RFactorN.cpp Astrophys.h ExtMath.h GRMain.h IDLInterface.h Messages.h RFactorKappa.h RFactorN.h
				g++ -c -O3 -fPIC RFactorN.cpp