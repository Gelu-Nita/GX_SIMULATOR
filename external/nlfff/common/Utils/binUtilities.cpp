#include "stdio.h"
#include "string_ex.h"

#include "binUtilities.h"

#ifdef _MSVC_BUILD
#pragma warning(disable:4996)
#endif

//------------------------------------------------------------------
CbinDataStruct::CbinDataStruct()
    : nRead(0)
{
}

//------------------------------------------------------------------
CbinDataStruct::~CbinDataStruct()
{
    Delete();
}

//------------------------------------------------------------------
uint32_t CbinDataStruct::Copy(REALTYPE_A *f, REALTYPE_A *fy, REALTYPE_A *fz, int idx)
{
    size_t sz = sizeof(REALTYPE_A)*data[idx].N[0]*data[idx].N[1]*data[idx].N[2];

    memcpy(f, data[idx].data, sz);
    if (fy && fz)
    {
        memcpy(fy, data[idx+1].data, sz);
        memcpy(fz, data[idx+2].data, sz);
    }

    return 0;
}

//------------------------------------------------------------------
uint32_t CbinDataStruct::Create(int *N, REALTYPE_A *f, REALTYPE_A *fy, REALTYPE_A *fz)
{
    Delete();

    uint64_t sz = sizeof(REALTYPE_A)*N[0]*N[1]*N[2];

    for (int k = 0; k < 3; k++)
        if (k == 0 || k > 0 && fy && fz)
        {
            data[k].nDims = 3;
            data[k].N[0] = N[0];
            data[k].N64[0] = N[0];
            data[k].N[1] = N[1];
            data[k].N64[1] = N[1];
            data[k].N[2] = N[2];
            data[k].N64[2] = N[2];
            data[k].data = new uint8_t[sz];
            data[k].type = MFO_TYPE_DOUBLE;
        }

    if (fy && fz)
    {
        strcpy(data[0].name, "BX");
        nRead = 3;
    }
    else
    {
        strcpy(data[0].name, "S");
        nRead = 1;
    }
    memcpy(data[0].data, f, sz);
    if (fy && fz)
    {
        strcpy(data[1].name, "BY");
        memcpy(data[1].data, fy, sz);
        strcpy(data[2].name, "BZ");
        memcpy(data[2].data, fz, sz);
    }

    return 0;
}

//------------------------------------------------------------------
uint32_t CbinDataStruct::Delete()
{
    for (int i = 0; i < nRead; i++)
    {
        delete [] data[i].data;
        data[i].data = NULL;
    }

    nRead = 0;
    return 0;
}

//------------------------------------------------------------------
int CbinDataStruct::WriteHeader(FILE * fid)
{
    unsigned char buffer[IDLU_BUFSIZE];
    int val;
    memset(buffer, 0, IDLU_BUFSIZE);
    fwrite("AGSB64v0100000  ", sizeof(uint8_t), 16, fid);
    fwrite(buffer, sizeof(uint8_t), 1060, fid);
    val = 255;
    fwrite(&val, sizeof(uint8_t), 4, fid);

    return 0;
}

//------------------------------------------------------------------
size_t CbinDataStruct::Write(FILE * fid, REALTYPE_A *d, size_t lng, char * name)
{
    CbinData data;
    strcpy(data.name, name);
    data.nDims = 1;
    data.N[0] = (int)lng;
    data.N64[0] = lng;
    data.type = MFO_TYPE_DOUBLE;
    data.data = (uint8_t *)d;

    return Write(fid, &data);
}

//------------------------------------------------------------------
size_t CbinDataStruct::Write(FILE * fid, int *v, size_t lng, char * name)
{
    CbinData data;
    strcpy(data.name, name);
    data.nDims = 1;
    data.N[0] = (int)lng;
    data.N64[0] = lng;
    data.type = MFO_TYPE_INT32;
    data.data = (uint8_t *)v;

    return Write(fid, &data);
}

//------------------------------------------------------------------
size_t CbinDataStruct::Write64(FILE * fid, int64_t *v, size_t lng, char * name)
{
    CbinData data;
    strcpy(data.name, name);
    data.nDims = 1;
    data.N[0] = (int)lng;
    data.N64[0] = lng;
    data.type = MFO_TYPE_INT64;
    data.data = (uint8_t *)v;

    return Write(fid, &data);
}

//------------------------------------------------------------------
int CbinDataStruct::Write(FILE * fid)
{
    for (int i = 0; i < nRead; i++)
        Write(fid, data+i);

    return nRead;
}

//------------------------------------------------------------------
size_t CbinDataStruct::Write(FILE * fid, CbinData *data)
{
    int val = 0;
    char buffer[256];
    fwrite(&val, sizeof(uint8_t), 4, fid); // not terminator

    int namelng = (int)strlen(data->name);
    int nl = namelng>>2;
    int storenamelng = (nl+1)<<2;
    memset(buffer, ' ', storenamelng);
    memcpy(buffer, data->name, namelng);
    fwrite(&namelng, sizeof(uint8_t), 4, fid);
    fwrite(&storenamelng, sizeof(uint8_t), 4, fid);
    fwrite(buffer, sizeof(uint8_t), storenamelng, fid);

    fwrite(&(data->type), sizeof(uint8_t), 4, fid);

    int bytesperelem = 0;
    if (data->type == MFO_TYPE_INT32)
        bytesperelem = 4;
    else if (data->type == MFO_TYPE_INT64)
        bytesperelem = 8;
    else if (data->type == MFO_TYPE_DOUBLE)
        bytesperelem = 8;
    fwrite(&bytesperelem, sizeof(uint8_t), 4, fid);

    int nDims = data->nDims;
    size_t numel = 1;
    for (int d = 0; d < nDims; d++)
        numel *= data->N64[d];
    size_t nbytes = numel*bytesperelem;

    fwrite(&nbytes, sizeof(uint8_t), 8, fid);
    fwrite(&numel, sizeof(uint8_t), 8, fid);
    fwrite(&nDims, sizeof(uint8_t), 4, fid);
    val = 8;
    fwrite(&val, sizeof(uint8_t), 4, fid); // maxdims
    for (int d = 0; d < val; d++)
        fwrite(&(data->N64[d]), sizeof(uint8_t), 8, fid);

    size_t chunk = 0x20000000;
    size_t rest = nbytes;
    uint8_t *p = data->data;
    while (rest > 0)
    {
        size_t currw = chunk;
        if (rest < chunk)
            currw = rest;
        size_t w = fwrite(p, sizeof(uint8_t), currw, fid);
        //int pValue;
        //errno_t err = _get_errno(&pValue);
        rest -= w;
        p += w;
        if (w != currw)
            break;
    }

    return nbytes - rest;
}

//------------------------------------------------------------------
int CbinDataStruct::WriteFooter(FILE * fid)
{
    int val = 255;
    fwrite(&val, sizeof(uint8_t), 4, fid);
    fwrite(&val, sizeof(uint8_t), 4, fid);
    fwrite(&val, sizeof(uint8_t), 4, fid);
    fwrite(&val, sizeof(uint8_t), 4, fid);

    return 0;
}

//------------------------------------------------------------------
int CbinDataStruct::GetNDims(int idx)
{
    return data[idx].nDims;
}

//------------------------------------------------------------------
int *CbinDataStruct::GetDimensions(int idx)
{
    return data[idx].N;
}

//------------------------------------------------------------------
uint64_t *CbinDataStruct::GetDimensions64(int idx)
{
    return data[idx].N64;
}

//------------------------------------------------------------------
int CbinDataStruct::findEntry(char *entry)
{
    int idx = -1;
    for (int k = 0; k < nRead; k++)
        if (!str_ex_compare(data[k].name, entry, false))
        {
            idx = k;
            break;
        }

    return idx;
}
