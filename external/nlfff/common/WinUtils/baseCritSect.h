#pragma once

class CbaseCritSect
{
protected:
    CRITICAL_SECTION cs;

public:
    CbaseCritSect()
    {
        ::InitializeCriticalSection(&cs);
    }

    ~CbaseCritSect()
    {
        ::DeleteCriticalSection(&cs);
    }

    void EnterCriticalSection()
    {
        ::EnterCriticalSection(&cs);
    }

    void LeaveCriticalSection()
    {
        ::LeaveCriticalSection(&cs);
    }
};