RenderIrregular	:	Render.o 
			g++ -shared -Wl,-soname,RenderIrregular.so -o RenderIrregular.so Render.o
Render.o	:	Render.cpp
			g++ -c -O3 -fPIC -D LINUX Render.cpp