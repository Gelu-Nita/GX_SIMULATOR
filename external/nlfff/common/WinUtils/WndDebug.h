#ifndef _AGS_WNDDEBUG_H
#define _AGS_WNDDEBUG_H

//#include "precompile.h"
#include "baseCritSect.h"
#include <stdio.h>

class BWndDebug
{
protected:
    FILE *m_file;
    char m_sFileProp[8];
    BOOL m_bFlush;
    HWND m_hWndListBox;
    BOOL m_bLastString;
    BOOL m_bTimeStamp;

    char s[1024];

    CbaseCritSect cs;

    LARGE_INTEGER frequency_time, timeBegin;

public:
    BWndDebug(LPCSTR _lpsFileName = "", LPCSTR _sFileProp = "wb", BOOL _bFlush = TRUE, 
                         HWND _hWnd = NULL, BOOL _bLastString = TRUE, BOOL _bTimeStamp = FALSE);
    virtual ~BWndDebug();
    void Init(LPCSTR _lpsFileName = "", LPCSTR _sFileProp = "wb", BOOL _bFlush = TRUE, 
                         HWND _hWnd = NULL, BOOL _bLastString = TRUE, BOOL _bTimeStamp = FALSE);
    void Close();
    void Say(char *format, ...);
};

#endif

