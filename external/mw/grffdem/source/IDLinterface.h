#pragma once

#define D3(s1, s2, i1, i2, i3) ((i1)+((i2)+(i3)*(s2))*(s1))

#ifndef LINUX
extern "C" __declspec(dllexport) int GET_MW(int argc, void **argv);
#else
extern "C" int GET_MW(int argc, void **argv);
#endif
