#include "stdafx.h"

//#include <afxext.h>         // MFC extensions
//#include <time.h>

#include "WndDebug.h"

#ifdef _MSVC_BUILD
#pragma warning (disable : 4996)
#endif

//------------------------------------------------------------------ 24-Apr-2001
BWndDebug::BWndDebug(LPCSTR _lpsFileName, LPCSTR _sFileProp, BOOL _bFlush, 
                     HWND _hWnd, BOOL _bLastString, BOOL _bTimeStamp)
    : m_file(NULL),
      m_hWndListBox(NULL)
{
    Init(_lpsFileName, _sFileProp, _bFlush, _hWnd, _bLastString, _bTimeStamp);
}

//------------------------------------------------------------------ 24-Apr-2001
BWndDebug::~BWndDebug()
{
    Close();
}

//------------------------------------------------------------------ 24-Apr-2001
void BWndDebug::Init(LPCSTR _lpsFileName, LPCSTR _sFileProp, BOOL _bFlush, 
                     HWND _hWnd, BOOL _bLastString, BOOL _bTimeStamp)
{
    Close();

    m_file = NULL;
    m_sFileProp[0] = 0;
    if (_sFileProp && _sFileProp[0] != 0)
        strcpy(m_sFileProp, _sFileProp);
    m_bFlush = _bFlush;

    m_hWndListBox = NULL;
    m_bLastString = _bLastString;
    if (_lpsFileName[0] != 0)
        m_file = fopen(_lpsFileName, m_sFileProp);
    if (_hWnd)
        m_hWndListBox = _hWnd;

#ifdef _MSVC_BUILD
    m_bTimeStamp = _bTimeStamp;
    if (m_bTimeStamp)
    {
        ::QueryPerformanceFrequency(&frequency_time);
        ::QueryPerformanceCounter(&timeBegin);
    }   
#else
    m_bTimeStamp = FALSE;
#endif
}

//------------------------------------------------------------------ 24-Apr-2001
void BWndDebug::Close()
{
    if (m_file)
    {
        ::fflush(m_file);
        ::fclose(m_file);
        m_file = NULL;
    }

    m_hWndListBox = NULL;
}
  
//------------------------------------------------------------------ 24-Apr-2001
void BWndDebug::Say(char *format, ...)
{
    if (!m_hWndListBox && !m_file)
        return;

    cs.EnterCriticalSection();
    int pos = 0;

#ifdef _MSVC_BUILD
    if (m_bTimeStamp)
    {
        LARGE_INTEGER timeEnd;
        ::QueryPerformanceCounter(&timeEnd);
	    __int64 work_time = timeEnd.QuadPart - timeBegin.QuadPart;
	    work_time = (work_time * 1000000) / frequency_time.QuadPart;

        sprintf(s, "%11.2f: ", double(work_time) / 1000.);
        pos = (int)strlen(s);
    }   
#endif

    va_list ap;
    va_start(ap, format);
    vsprintf(s+pos, format, ap);
    va_end(ap);

#ifdef _MSVC_BUILD
    if (m_hWndListBox)
    {
        ::SendMessage(m_hWndListBox, LB_ADDSTRING, 0, (LPARAM)s);
        if (m_bLastString)
        {
            int iC = (int)::SendMessage(m_hWndListBox, LB_GETCOUNT, 0,0);
            if (iC != LB_ERR && iC > 0)
                ::SendMessage(m_hWndListBox, LB_SETCURSEL, iC-1, 0);
        }
    }
#endif

    if (m_file)
    {
        ::fprintf(m_file, s);
        ::fprintf(m_file, "\r\n");
        if (m_bFlush)
            ::fflush(m_file);
    }
    cs.LeaveCriticalSection();
}


