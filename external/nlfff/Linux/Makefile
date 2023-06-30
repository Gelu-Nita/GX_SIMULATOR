CC = g++
CXXFLAGS += -std=c++11 -fPIC -O2 -fpermissive -I../common/BaseMath -I../common/BasePhysics -I../common/General -I../common/IDL -I../common/Multithread -I../common/Utils -I../sources
# CXXFLAGS += -DDEBUG_CPP11THREAD

LDFLAGS = -shared -lm -pthread

TARGET=../binaries/WWNLFFFReconstruction.so
SOURCES=$(shell echo ../common/BaseMath/*.cpp ../common/BasePhysics/*.cpp ../common/Multithread/*.cpp ../common/Utils/*.cpp ../sources/*.cpp)
OBJECTS=$(SOURCES:.cpp=.o)

all: $(TARGET)

clean:
	rm -f $(OBJECTS) $(TARGET)

$(TARGET) : $(OBJECTS)
	$(CC) $(CXXFLAGS) $(OBJECTS) -o $@ $(LDFLAGS)
