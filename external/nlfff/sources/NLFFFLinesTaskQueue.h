#pragma once

#include "LinesTaskQueue.h"

class CNLFFFLinesTaskQueue : public CLinesTaskQueue
{
public:
    enum Status { None = 0, Processed = 1, Lined = 2, Closed = 4, BaseVoxel = 8 };

protected:
    REALTYPE_A *seeds;

    REALTYPE_A *params;

private:
    void parseGlobalIdx(int idx, int *kx, int *ky, int *kz)
    {
        *kx = idx % NF[0];
        int kyz = (idx-*kx) / NF[0];
        *ky = kyz % NF[1];
        *kz = (kyz-*ky) / NF[1];
    }

    void InitOutput(int id)
    {
        if (physLength)
            physLength[id] = 0;
        if (avField)
            avField[id] = 0;
        if (startIdx)
            startIdx[id] = 0;
        if (endIdx)
            endIdx[id] = 0;
        if (voxelStatus)
            voxelStatus[id] = Status::None;
        if (passed)
            passed[id] = false;
    }

public:
    CNLFFFLinesTaskQueue(int _nTasks, CagmVectorFieldOps *_field, REALTYPE_A *_seeds, int _Nseeds,
        uint32_t _cond = 0x3, REALTYPE_A _chromoLevel = 0,
        REALTYPE_A *_physLength = nullptr, REALTYPE_A *_avField = nullptr,
        int *_voxelStatus = nullptr, int *_codes = nullptr,
        int *_startIdx = nullptr, int *_endIdx = nullptr, int *_apexIdx = nullptr,
        int maxResult = 50000,
        uint64_t _maxCoordLength = 0, int *_linesLength = nullptr, REALTYPE_A *_coords = nullptr, uint64_t *_linesStart = nullptr, int *_linesIndex = nullptr, int *seedIdx = nullptr,
        bool _bFixAffinity = false)
        : CLinesTaskQueue(_nTasks, _field, 
            _cond, _chromoLevel,
            _physLength, _avField,
            _voxelStatus, _codes, 
            _startIdx, _endIdx, _apexIdx, maxResult,
            _maxCoordLength, _linesLength, _coords, _linesStart, _linesIndex, seedIdx, _bFixAffinity)
      , seeds(_seeds)
    {
        Nseeds = _Nseeds;
        globalIdx = nullptr;

        autoParams = (Nseeds <= 0 || !seeds);
        memcpy(NF, field->GetDimensions(), 3*sizeof(int));
        int nVox = (autoParams ? NF[0]*NF[1]*NF[2] : Nseeds);

        InitQueue(nVox);

        if (cond != Conditions::NoCond)
            passed = new int[nVox];

        if (!autoParams)
        {
            globalIdx = new int[nVox];
            params = new REALTYPE_A[3 * sizeof(REALTYPE_A) * nVox];
            for (int id = 0; id < Nseeds; id++)
            {
                InitOutput(id);
                params[3*id + 0] = seeds[3*id + 0];
                params[3*id + 1] = seeds[3*id + 1];
                params[3*id + 2] = seeds[3*id + 2];
                globalIdx[id] = getGlobalID(seeds + 3*id);
            }
        }
        else
        {
            params = new REALTYPE_A[3 * sizeof(REALTYPE_A)];

            for (int kz = 0; kz < NF[2]; kz++)
                for (int ky = 0; ky < NF[1]; ky++)
                    for (int kx = 0; kx < NF[0]; kx++)
                        InitOutput(getGlobalID(kx, ky, kz));
        }
    }

    virtual ~CNLFFFLinesTaskQueue()
    {
        delete[] params;
        delete[] globalIdx;
        delete[] passed;
    }

    virtual void * GetParams(uint32_t _queueID)
    {
        if (!autoParams)
            return params + _queueID*3;
        else
        {
            int kx, ky, kz;
            parseGlobalIdx(_queueID, &kx, &ky, &kz);
            params[0] = kx;
            params[1] = ky;
            params[2] = kz;
            return params;
        }
    }
};
