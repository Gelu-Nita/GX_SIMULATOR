#pragma once

#include <thread>
#include "TaskQueueProcessor.h"

//---------------------------------------------------------------------------------------
class WQPTaskD : public ATQPTask
{
protected:
    int layer;
    size_t queueID;

public:
    void setData(int _layer, size_t _queueID)
    {
        layer = _layer;
        queueID = _queueID;
    }

    int *getData() { return &layer; }
};

//---------------------------------------------------------------------------------------
class WQPTaskFactoryD : public ATQPTaskFactory
{
public:
    WQPTaskFactoryD() : ATQPTaskFactory() {}
    virtual ~WQPTaskFactoryD() {}

    virtual ATQPTask *create()
    {
        WQPTaskD *task = new WQPTaskD;
        initialize(task);

        return task;
    }
};

//---------------------------------------------------------------------------------------
class WQPSupervisorD : public ATQPSupervisor
{
protected:
    int n_proc;

public:
    WQPSupervisorD(int *N, int _nProc, int _ext, WQPTaskFactoryD *factory)
        : ATQPSupervisor(N[2]-1, factory)
        , n_proc(_nProc)
    {
        for (int i = 0; i < n_task; i++)
            ((WQPTaskD *)tasks[i])->setData(i+1, i);
    }

    virtual ~WQPSupervisorD()
    {
    }
};
