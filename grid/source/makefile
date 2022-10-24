RenderIrregular	:	Render.o
			g++ $^ -shared -o  RenderIrregular.so
Render.o	:	Render.cpp
			g++ -c -std=c++11 -O3 -fPIC -D LINUX Render.cpp
