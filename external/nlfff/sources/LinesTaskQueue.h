#pragma once

#include "math.h"
#include "agmVectorField.h"

class CLinesTaskQueue
{
public:
    enum Status { None = 0, Processed = 1, Lined = 2, Closed = 4, BaseVoxel = 8 };
    enum Conditions { NoCond = 0, PassClosed = 1, PassOpen = 2 };

protected:
    int nQueue, n;

    CagmVectorFieldOps *field;
    int NF[3];

    REALTYPE_A chromoLevel;
    uint32_t cond;

    uint64_t maxCoordLength;

    REALTYPE_A coordTol;
    REALTYPE_A closedTol;

    // Caller allocated!
    // length = Nseeds, 1-D
    REALTYPE_A *physLength, *avField;
    int *startIdx, *endIdx, *apexIdx;
    int *codes;
    int *voxelStatus;

    // length = maxCoordLength x 3, 2-D
    REALTYPE_A *coords;
    // length = Nseeds, 1-D
    int *linesLength;
    uint64_t *linesStart;
    int *linesIndex;
    int *seedIdx;

    REALTYPE_A *distance;

    int *passed;

    int Nseeds;
    bool autoParams;
    int *globalIdx;

public:
    uint64_t cumLength;
    int nLines, nPassed;
    int nNonStored;

public:
    CLinesTaskQueue(int _nTasks, CagmVectorFieldOps *_field,
        uint32_t _cond = 0x3, REALTYPE_A _chromoLevel = 0,
        REALTYPE_A *_physLength = nullptr, REALTYPE_A *_avField = nullptr,
        int *_voxelStatus = nullptr, int *_codes = nullptr,
        int *_startIdx = nullptr, int *_endIdx = nullptr, int *_apexIdx = nullptr,
        int maxResult = 50000,
        uint64_t _maxCoordLength = 0, int *_linesLength = nullptr, REALTYPE_A *_coords = nullptr, uint64_t *_linesStart = nullptr, int *_linesIndex = nullptr, int *_seedIdx = nullptr,
        bool _bFixAffinity = false)
        : 
        n(0)
        , field(_field)
        , cond(_cond)
        , chromoLevel(_chromoLevel)
        , maxCoordLength(_maxCoordLength)
        , linesLength(_linesLength)
        , codes(_codes)
        , voxelStatus(_voxelStatus)
        , physLength(_physLength)
        , avField(_avField)
        , startIdx(_startIdx)
        , endIdx(_endIdx)
        , apexIdx(_apexIdx)
        , coords(_coords)
        , linesStart(_linesStart)
        , linesIndex(_linesIndex)
        , seedIdx(_seedIdx)
        , cumLength(0)
        , coordTol(1e-3)
        , closedTol(3e-3)
        , nPassed(0)
        , passed(nullptr)
        , nLines(0)
        , nNonStored(0)
    {
        distance = new REALTYPE_A[maxResult];
    }

    virtual ~CLinesTaskQueue()
    {
        delete[] distance;
    }

    virtual uint32_t InitQueue(int _nQueue)
    {
        return nQueue = _nQueue;
    }

    virtual long NextQueueItem(int taskN = -1)
    {
        if (n >= nQueue)
            return -1; //  ABaseTaskQueue::NoItems;

        long nret = n;
        n++;
        return nret;
    }

    bool needProcessing(uint32_t queueID)
    {
        return (passed ? !passed[queueID] : true);
    }

    void setVoxelStatus(int idx, int status, bool set = false)
    {
        if (set)
            voxelStatus[idx] = status;
        else
            voxelStatus[idx] |= status;
    }

    void getDivPoint(REALTYPE_A *p1, REALTYPE_A *p2)
    {
        REALTYPE_A t = (chromoLevel - p1[2])/(p2[2] - p1[2]);
        p1[0] = p1[0] + t*(p2[0] - p1[0]);
        p1[1] = p1[1] + t*(p2[1] - p1[1]);
        p1[2] = chromoLevel;
    }

#define d_sdist(c1, c2) sqrt((0[c2]-0[c1])*(0[c2]-0[c1]) + (1[c2]-1[c1])*(1[c2]-1[c1]) + (2[c2]-2[c1])*(2[c2]-2[c1]))
#define d_snorm(c1) sqrt(0[c1]*0[c1] + 1[c1]*1[c1] + 2[c1]*2[c1])

#define FS_VOID 0
#define FS_IN   1
#define FS_OUT  2
#define FS_END  3

    virtual uint32_t processLine(uint32_t queueID, REALTYPE_A *point, REALTYPE_A *result, int resLength, int _code, int _code4over)
    {
        if (voxelStatus)
            setVoxelStatus(queueID, Status::Processed, true);
        if (apexIdx)
            apexIdx[queueID] = 0;
        if (seedIdx)
            seedIdx[queueID] = 0;
        if (startIdx)
            startIdx[queueID] = 0;
        if (endIdx)
            endIdx[queueID] = 0;
        if (physLength)
            physLength[queueID] = 0;
        if (avField)
            avField[queueID] = 0;
        if (codes)
            codes[queueID] = 0;

        REALTYPE_A thisPhysLength = 0;
        REALTYPE_A thisAvField = 0;
        if (resLength > 1) // && point[2] >= floor(chromoLevel))
        {
            int state = FS_VOID;
            // find fragment
            int start, end;
            bool found = false;
            int kPoint = -1;
            for (int k = 0; k < resLength; k++)
            {
                if (state == FS_END)
                    break;

                if (fabs(result[3*k] - point[0]) <= coordTol && fabs(result[3*k+1] - point[1]) <= coordTol && fabs(result[3*k+2] - point[2]) <= coordTol)
                {
                    found = true;
                    kPoint = k;
                }

                bool above = (result[3*k+2] >= chromoLevel);
                switch (state)
                {
                case FS_VOID:
                    if (above)
                    {
                        start = k;
                        state = FS_IN;
                    }
                    else 
                        state = FS_OUT;
                    break;
                case FS_IN:
                    if (!above)
                    {
                        end = k;
                        state = (found ? FS_END : FS_OUT);
                    }
                    break;
                case FS_OUT:
                    if (above)
                    {
                        start = k-1;
                        state = FS_IN;
                    }
                    break;
                }
            }
            if (state == FS_IN)
                end = resLength-1;
            else if (state == FS_OUT)
                start = end = 0;

            if (found && end-start+1 >= 2)
            {
                resLength = end-start+1;
                bool bStartClosed = false, bEndClosed = false;
                Status sClosed;
                if (result[3*start+2] <= chromoLevel && result[3*(start+1)+2] >= chromoLevel)
                    getDivPoint(result + 3*start, result + 3*(start+1));

                if (result[3*start+2] <= chromoLevel + closedTol)
                    bStartClosed = true;

                if (result[3*(end-1)+2] >= chromoLevel && result[3*end+2] <= chromoLevel && result[3*end+2] != result[3*(end-1)+2])
                    getDivPoint(result + 3*end, result + 3*(end-1));

                if (resLength > 2 || result[3 * end + 2] != result[3 * (end - 1) + 2])
                {
                    if (result[3 * end + 2] <= chromoLevel + closedTol)
                        bEndClosed = true;

                    sClosed = (bStartClosed && bEndClosed ? Status::Closed : Status::None);

                    REALTYPE_A B0[3], B1[3], B, Bprev;

                    field->getPoint(result, B0);
                    distance[start] = 0;

                    int posmin = start;
                    REALTYPE_A Blng = 0, Bmin = d_snorm(B0);
                    Bprev = d_snorm(B0);
                    for (int k = start + 1; k <= end; k++)
                    {
                        field->getPoint(result + 3 * k, B1);
                        B = d_snorm(B1);
                        if (B < Bmin)
                        {
                            Bmin = B;
                            posmin = k;
                        }
                        REALTYPE_A step = d_sdist(result + 3 * k, result + 3 * (k - 1));
                        distance[k] = distance[k - 1] + step;
                        Blng += (Bprev + B)*0.5*step;

                        Bprev = B;
                    }
                    thisPhysLength = distance[end];
                    thisAvField = Blng / thisPhysLength;

                    int apexid = 0, seedid = 0, startid = 0, endid = 0;
                    if (apexIdx)
                        apexid = apexIdx[queueID] = getGlobalID(result + 3 * posmin);
                    if (seedIdx)
                        seedid = seedIdx[queueID] = getGlobalID(point);
                    for (int k = start; k <= end; k++)
                    {
                        if (voxelStatus && k == kPoint)
                            setVoxelStatus(queueID, Status::Processed | Status::Lined | sClosed | Status::BaseVoxel);
                        if (k == start && startIdx)
                            startid = startIdx[queueID] = getGlobalID(result + 3 * k);
                        if (k == end && endIdx)
                            endid = endIdx[queueID] = getGlobalID(result + 3 * k);
                        if (physLength && k == kPoint)
                            physLength[queueID] = thisPhysLength;
                        if (avField && k == kPoint)
                            avField[queueID] = thisAvField;
                    }

                    if ((sClosed && (cond & Conditions::PassClosed)) || (!sClosed && (cond & Conditions::PassOpen)))
                    {
                        for (int k = start; k <= end; k++)
                            proceedVox(result + 3 * k, queueID, sClosed, thisPhysLength, thisAvField, apexid, seedid, startid, endid);
                    }

                    nPassed++;
                    if (coords)
                    {
                        if (4 * (cumLength + resLength) > maxCoordLength)
                        {
                            _code |= _code4over;
                            nNonStored++;
                        }
                        else
                        {
                            for (int k = start; k <= end; k++)
                            {
                                uint64_t gpos = 4 * (cumLength + k - start);
                                memcpy(coords + gpos, result + 3 * k, 3 * sizeof(REALTYPE_A));
                                coords[gpos + 3] = distance[k] - distance[posmin];
                            }

                            if (linesStart)
                                linesStart[nLines] = cumLength;
                            if (linesLength)
                                linesLength[nLines] = resLength;
                            if (linesIndex)
                                linesIndex[nLines] = queueID;
                            nLines++;
                            cumLength += resLength;
                        }
                    }

                    if (codes)
                        codes[queueID] = _code;
                }
            }
        }

        return 0;
    }

    int getGlobalID(int kx, int ky, int kz)
    {
        return (kz*NF[1] + ky)*NF[0] + kx;
    }

    int getGlobalID(REALTYPE_A x, REALTYPE_A y, REALTYPE_A z)
    {
        int kx = (int)floor(x);
        int ky = (int)floor(y);
        int kz = (int)floor(z);

        return getGlobalID(kx, ky, kz);
    }

    int getGlobalID(REALTYPE_A *coord)
    {
        return getGlobalID(coord[0], coord[1], coord[2]);
    }

    uint32_t proceedThisVox(int queueID, Status sClosed, REALTYPE_A thisPhysLength, REALTYPE_A thisAvField, int apexid, int seedid, int startid, int endid)
    {
        if (voxelStatus)
            setVoxelStatus(queueID, Status::Processed | Status::Lined | sClosed);

        if (physLength)
            physLength[queueID] = thisPhysLength;
        if (avField)
            avField[queueID] = thisAvField;

        if (apexIdx)
            apexIdx[queueID] = apexid;
        if (seedIdx)
            seedIdx[queueID] = seedid;
        if (startIdx)
            startIdx[queueID] = startid;
        if (endIdx)
            endIdx[queueID] = endid;

        passed[queueID] = true;

        return 0;
    }

    uint32_t proceedVox(REALTYPE_A *coord, int /* queueID */, Status sClosed, REALTYPE_A thisPhysLength, REALTYPE_A thisAvField, int apexid, int seedid, int startid, int endid)
    {
        int gidx = getGlobalID(coord);

        if (autoParams)
            return proceedThisVox(gidx, sClosed, thisPhysLength, thisAvField, apexid, seedid, startid, endid);

        for (int k = 0; k < Nseeds; k++)
        {
            if (globalIdx[k] == gidx)
                proceedThisVox(k, sClosed, thisPhysLength, thisAvField, apexid, seedid, startid, endid);
        }

        return 0;
    }

    uint32_t SetResult(uint32_t queueID, REALTYPE_A *point, REALTYPE_A *result, int resLength, int _code, int _code4over)
    {
        return processLine(queueID, point, result, resLength, _code, _code4over);
    }
};
