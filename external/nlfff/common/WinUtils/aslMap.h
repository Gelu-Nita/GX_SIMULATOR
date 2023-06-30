#if !defined(AGS_20141108_013000_ASLMAP_H)
#define AGS_20141108_013000_ASLMAP_H

#ifdef _MSVC_BUILD
#pragma warning(disable:4996)
#endif

#include <stdlib.h>

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifdef _MSVC_BUILD
#pragma warning(disable:4996)
#endif

#define _MAX_BUFFER 64

#ifdef _MSVC_BUILD
//EXTERN_C IMAGE_DOS_HEADER __ImageBase;
#define HINST_THISCOMPONENT ((HINSTANCE)&__ImageBase)
#endif

template <class mapType, int maplength> class aslMap
{
// Attributes
protected:
    char *keys[maplength];
    mapType values[maplength];
    int lng;
    char IniFileName[_MAX_PATH];

// Construction
public:
	aslMap()
        : lng(0)
    {
#ifdef _MSVC_BUILD
        char modfile[_MAX_PATH];
        ::GetModuleFileName(HINST_THISCOMPONENT, modfile, sizeof(modfile));

        char drive[_MAX_DRIVE];
        char directr[_MAX_DIR];
        char fname[_MAX_FNAME];
        char ext[_MAX_EXT];
        _splitpath( modfile, drive, directr, fname, ext );
        _makepath( IniFileName, drive, directr, fname, ".ini" );
#endif
    }
	virtual ~aslMap()
    {} // NB! 2do!

// Operations
public:
    bool add(const char *key, mapType value)
    {
        bool isNew = true;
        int pos = -1;
        for (int i = 0; i < lng; i++)
            if (!stricmp(key, keys[i]))
            {
                isNew = false;
                pos = i;
                break;
            }

        if (pos < 0)
        {
            pos = lng;
            size_t keylen = strlen(key)+1;
            keys[pos] = new char[keylen];
            strcpy_s(keys[pos], keylen, key);
            lng++;
        }
        values[pos] = value;

        return isNew;
    }

    bool get(const char *key, mapType& value)
    {
        for (int i = 0; i < lng; i++)
            if (!stricmp(key, keys[i]))
            {
                value = values[i];
                return true;
            }

        return false;
    }

    int length()
    {
        return lng;
    }

    int state(char *buffer, int /* buflen */)
    {
        char s[_MAX_BUFFER];
        buffer[0] = 0;
        for (int i = 0; i < lng; i++)
        {
            strcat(buffer, keys[i]);
            strcat(buffer, " = ");
            sprintf(s, "%le", (double)(values[i]));
            strcat(buffer, s);
            strcat(buffer, "\r\n");
        }

        return lng;
    }

    bool proceed(bool bGet, const char *key, mapType& value, mapType defaultvalue)
    {
        if (bGet)
            return get(key, value);
        else
        {
#ifdef _MSVC_BUILD
            char buffer[_MAX_BUFFER];
            char pkey[_MAX_BUFFER];
            strcpy(pkey, key);
            char psect[_MAX_BUFFER], defl[_MAX_BUFFER];
            char *ps, *pe;
            char *p = strchr(pkey, '.');
            if (p)
            {
                *p = 0;
                ps = pkey;
                pe = p+1;
            }
            else
            {
                strcpy(psect, "General");
                ps = psect;
                pe = pkey;
            }
            sprintf(defl, "%le", (double)defaultvalue);
            ::GetPrivateProfileString(ps, pe, defl, buffer, _MAX_BUFFER-1, IniFileName);
       
            double v =  atof(buffer);

            return add(key, (mapType)v);
#else
            return add(key, defaultvalue);
#endif
        }
    }
};

#endif // AGS_20141108_013000_ASLMAP_H
