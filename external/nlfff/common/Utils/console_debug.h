#pragma once
#include <iostream>
#include <mutex>

#ifndef _CONSOLE_DEBUG_NO_MUTEX
extern 
#endif
std::mutex global_mutex_print;

void console_start();

#ifdef DEBUG_CPP11THREAD
#define console_debug(s) \
{ \
std::unique_lock<std::mutex> locker_p(global_mutex_print); \
std::cout << s << std::endl; \
}
#else
#define console_debug(s)
#endif
