#pragma once

#include <thread>
#include "TaskQueueProcessor.h"
#include <cmath>

//---------------------------------------------------------------------------------------
class WQPTask : public ATQPTask
{
protected:
    int data[9];
    size_t queueID;

public:
    void setData(int *_data, size_t _queueID) 
    { 
        memcpy(data, _data, 9*sizeof(int));
        queueID = _queueID;
    }

    int *getData() { return data; }
    size_t getQueueID() { return queueID; }
};

//---------------------------------------------------------------------------------------
class WQPTaskFactory : public ATQPTaskFactory
{
public:
    WQPTaskFactory() : ATQPTaskFactory() {}
    virtual ~WQPTaskFactory() {}

    virtual ATQPTask *create()
    {
        WQPTask *task = new WQPTask;
        initialize(task);

        return task;
    }
};

//---------------------------------------------------------------------------------------
class WQPSupervisor : public ATQPSupervisor
{
protected:
    int n_proc;

    int ext;
    int M[3];
    int chunkSize;

public:
    WQPSupervisor(int *N, int _nChunks, int _nProc, int _ext, WQPTaskFactory *_factory)
        : ATQPSupervisor(_nChunks, _factory)
        , n_proc(_nProc)
        , ext(_ext)
    {
        int *Mminz = new int[n_task];
        int *dphLz = new int[n_task];
        int *dphHz = new int[n_task];
        M[0] = N[0];
        M[1] = N[1];
        divider(n_task, N[2], M + 2, Mminz, dphLz, dphHz);
        chunkSize = M[2];

        int params[9];
        memset(params, 0, 9*sizeof(int));
        for (int i = 0; i < n_task; i++)
        {
            params[2] = Mminz[i];
            params[5] = dphLz[i];
            params[8] = dphHz[i];
            ((WQPTask *)tasks[i])->setData(params, i);
        }

        delete[] Mminz;
        delete[] dphLz;
        delete[] dphHz;
    }

    int *ChunkSize()
    {
        return M;
    }

    int NProc()
    {
        return n_proc;
    }

    void divider(int d, int N, int *M, int *Mmin, int *dphL, int *dphH)
    {
        int Nd = (int)std::ceil((double)(N - 2 * ext) / d) + 2 * ext;
        *M = Nd;
        for (int kd = 0; kd < d - 1; kd++)
        {
            Mmin[kd] = kd*(Nd - 2 * ext);
            if (kd == 0)
                dphL[kd] = 0;
            else
                dphL[kd] = ext;
            dphH[kd] = ext;
        }
        Mmin[d - 1] = N - Nd;
        if (d == 1)
            dphL[0] = 0;
        else
            dphL[d - 1] = d*Nd - N - (d - 2) * 2 * ext - ext;
        dphH[d - 1] = 0;
    }

    virtual ~WQPSupervisor()
    {
    }
};
