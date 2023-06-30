#pragma once

#include "stdDefinitions.h"

enum {MFO_TYPE_NONE = 0, MFO_TYPE_INT32 = 3, MFO_TYPE_INT64 = 4, MFO_TYPE_DOUBLE = 5};

#define IDLU_BUFSIZE 2048

//------------------------------------------------------------------
class CbinData
{
public:
    uint8_t *data;
    int nDims;
    int N[8];
    uint64_t N64[8];
    int type;
    char name[256];

	CbinData()
        : data(nullptr),
          nDims(0),
          type(MFO_TYPE_NONE)
    {
        for (int k = 0; k < 8; k++)
        {
            N[k] = 1;
            N64[k] = 1;
        }
    }
	virtual ~CbinData()
    {}
};

#define MFO_BIN_MAX_DATA 64

//------------------------------------------------------------------
class CbinDataStruct
{
public:
    CbinData data[MFO_BIN_MAX_DATA];
    int nRead;

    CbinDataStruct();
    virtual ~CbinDataStruct();

public:
    uint32_t Create(int *N, REALTYPE_A *f, REALTYPE_A *fy = nullptr, REALTYPE_A *fz = nullptr);
    uint32_t Copy(REALTYPE_A *f, REALTYPE_A *fy = nullptr, REALTYPE_A *fz = nullptr, int idx = 0);
    uint32_t Delete();
    int Write(FILE *fid);

    static int WriteHeader(FILE *fid);
    static size_t Write(FILE *fid, CbinData *);
    static size_t Write(FILE *fid, REALTYPE_A *, size_t, char *);
    static size_t Write(FILE *fid, int *, size_t, char *);
    static size_t Write64(FILE *fid, int64_t *, size_t, char *);
    static int WriteFooter(FILE *fid);

    int *GetDimensions(int idx = 0);
    uint64_t *GetDimensions64(int idx = 0);
    int GetNDims(int idx = 0);
    int findEntry(char *);
};
