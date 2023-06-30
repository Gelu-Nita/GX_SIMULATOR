#include <mutex>

#ifdef _WINDOWS
#include "Windows.h"
#endif

extern std::mutex global_mutex_print;

#define _CONSOLE_DEBUG_NO_MUTEX
#include "console_debug.h"
#undef _CONSOLE_DEBUG_NO_MUTEX

void console_start()
{
#ifdef DEBUG_CPP11THREAD
#ifdef _WINDOWS
        AllocConsole();
        FILE* fDummy;
        freopen_s(&fDummy, "CONOUT$", "w", stdout);
#endif
#ifdef __GNUC__
        FILE* f = popen("uname -a", "w");
#endif
#endif
}
