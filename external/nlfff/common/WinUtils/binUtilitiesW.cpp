#include "binUtilitiesW.h"

//------------------------------------------------------------------
int CbinDataStructW::Read(FILE * fid, aslMap<int, 64> *map)
{
    Delete();

    unsigned char buffer[IDLU_BUFSIZE];
    memset(buffer, 'q', IDLU_BUFSIZE);
    char name[256];
    size_t res;
    int hdrsize = 1080;
    res = fread(buffer, sizeof(uint8_t), hdrsize, fid);
    if (res < hdrsize)
        return -1;
    if (buffer[0] != 'A' || buffer[1] != 'G' || buffer[2] != 'S' || buffer[3] != 'B')
        return -2;

    bool b32 = true;
    int NBytesSize = 4;
    if (buffer[4] == '6' && buffer[5] == '4')
    {
        b32 = false;
        NBytesSize = 8;
    }

    while (true)
    {
        res = fread(buffer, sizeof(uint8_t), 4, fid);    if (res < 4) return -1;
        int terminator = *((int *)buffer);
        if (terminator != 0)
            break;

        res = fread(buffer, sizeof(uint8_t), 4, fid);    if (res < 4) return -1;
        int namelng = *((int *)buffer);
        res = fread(buffer, sizeof(uint8_t), 4, fid);    if (res < 4) return -1;
        int storenamelng = *((int *)buffer);
        res = fread(buffer, sizeof(char), storenamelng, fid);    if (res < storenamelng) return -1;
        strncpy(name, (char *)buffer, namelng);
        name[namelng] = 0;

        map->add(name, nRead);
        strcpy(data[nRead].name, name);

        res = fread(buffer, sizeof(uint8_t), 4, fid);    if (res < 4) return -1;
        int type = *((int *)buffer);
        data[nRead].type = type;
        res = fread(buffer, sizeof(uint8_t), 4, fid);    if (res < 4) return -1;
//        int bytesperelem = *((int *)buffer);

        uint64_t nbytes, nelems;
        res = fread(buffer, sizeof(uint8_t), NBytesSize, fid);    if (res < NBytesSize) return -1;
        if (NBytesSize == 4)
            nbytes = *((int *)buffer);
        else
            nbytes = *((uint64_t *)buffer);
        res = fread(buffer, sizeof(uint8_t), NBytesSize, fid);    if (res < NBytesSize) return -1;
        if (NBytesSize == 4)
            nelems = *((int *)buffer);
        else
            nelems = *((uint64_t *)buffer);

        res = fread(buffer, sizeof(uint8_t), 4, fid);    if (res < 4) return -1;
        data[nRead].nDims= *((int *)buffer);
        res = fread(buffer, sizeof(uint8_t), 4, fid);    if (res < 4) return -1;
        int maxDims = *((int *)buffer); // 8
        for (int k = 0; k < maxDims; k++)
        {
            res = fread(buffer, sizeof(uint8_t), NBytesSize, fid);    if (res < NBytesSize) return -1;
            uint64_t dimsize = *((uint64_t *)buffer); // 8
            if (k < data[nRead].nDims)
            {
                data[nRead].N[k] = (int)dimsize;
                data[nRead].N64[k] = dimsize;
            }
        }
        data[nRead].data = (void *)(new uint8_t[nbytes]);
        res = fread(data[nRead].data, sizeof(uint8_t), nbytes, fid);    if (res < nbytes) return -1;
        nRead++;
    }

    return nRead;
}

