#pragma once

#include "binUtilities.h"
#include "aslMap.h"

class CbinDataStructW : public CbinDataStruct
{
public:
    CbinDataStructW() {}
    virtual ~CbinDataStructW() {}

    int Read(FILE *fid, aslMap<int, 64> *map);
};
