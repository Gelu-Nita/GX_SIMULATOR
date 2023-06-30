#pragma once

#include <thread>
#include <mutex>

#include "TaskQueueProcessor.h"
#include "NLFFFLinesTaskQueue.h"

//---------------------------------------------------------------------------------------
class LQPTask : public ATQPTask
{
protected:
    double data[3];
    unsigned long queueID;

public:
    void setData(double *_data) { memcpy(data, _data, 3 * sizeof(double)); }
    double *getData() { return data; }
    void setID(unsigned long id) { queueID = id; }
    unsigned long getID() { return queueID; }
};

//---------------------------------------------------------------------------------------
class LQPTaskFactory : public ATQPTaskFactory
{
public:
    LQPTaskFactory() : ATQPTaskFactory() {}
    virtual ~LQPTaskFactory() {}

    virtual ATQPTask *create()
    {
        LQPTask *task = new LQPTask;
        initialize(task);

        return task;
    }
};

//---------------------------------------------------------------------------------------
class LQPSupervisor : public ATQPSupervisor
{
public:
    CNLFFFLinesTaskQueue *queue;

protected:
    std::mutex mutex_store;
    
public:
    LQPSupervisor(int *N,
        CagmVectorField *v,
        uint32_t _cond, REALTYPE_A chromoLevel,
        REALTYPE_A *_seeds, int _Nseeds,
        int nProc,
        REALTYPE_A step, REALTYPE_A tolerance, REALTYPE_A boundAchieve,
        int *_nLines, int *_nPassed,
        int *_voxelStatus, REALTYPE_A *_physLength, REALTYPE_A *_avField,
        int *_linesLength, int *_codes,
        int *_startIdx, int *_endIdx, int *_apexIdx,
        uint64_t _maxCoordLength, uint64_t *_totalLength, REALTYPE_A *_coords, uint64_t *_linesStart, int *_linesIndex, int *seedIdx,
        LQPTaskFactory *factory)
        : ATQPSupervisor(0, factory)
    {
        int maxResult = 50000;

        queue = new CNLFFFLinesTaskQueue(nProc,
            v, _seeds, _Nseeds, _cond, chromoLevel,
            _physLength, _avField,
            _voxelStatus, _codes,
            _startIdx, _endIdx, _apexIdx,
            maxResult,
            4 * _maxCoordLength, _linesLength, _coords, _linesStart, _linesIndex, seedIdx);

        Reset();
    }

    void Reset()
    {
    }

    virtual ~LQPSupervisor()
    {
        delete queue;
    }

    virtual bool getTask(ATQPTask*& task)
    {
        task = nullptr;
        long n = queue->NextQueueItem();
        if (n < 0)
            return false;
        task = factory->create();
        ((LQPTask *)task)->setData((double *)queue->GetParams(n));
        ((LQPTask *)task)->setID(n);
        return true;
    }

    bool needProcessing(uint32_t queueID)
    {
        return queue->needProcessing(queueID);
    }

    uint32_t SetResult(uint32_t queueID, REALTYPE_A *point, REALTYPE_A *result, int resLength, int _code, int _code4over)
    {
        std::unique_lock<std::mutex> locker(mutex_store);
        uint32_t res = queue->SetResult(queueID, point, result, resLength, _code, _code4over);

        return res;
    }
};
