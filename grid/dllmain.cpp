#include <Windows.h>
#include "WinMulti.h"

BOOL APIENTRY DllMain(HMODULE hModule, DWORD  ul_reason_for_call, LPVOID lpReserved)
{
 SYSTEM_INFO si;
 GetSystemInfo(&si);
 ProcessorNumber=si.dwNumberOfProcessors;
 if (ProcessorNumber<1) ProcessorNumber=1;

 return TRUE;
}