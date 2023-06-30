#pragma once

#include "WiegelmannTaskQueue11.h"
#include "agpWiegelmann.h"

class WQPProcessor : public ATQPProcessor
{
protected:
    WQPTask *this_task;
    CagpWiegelmann *w;

public:
    WQPProcessor(WQPSupervisor *supervisor, int *_N, int _id, double *steps) : ATQPProcessor(_id)
    {
        w = new CagpWiegelmann(_N, _id);
        w->Allocate(supervisor->ChunkSize(), steps);
    }
    virtual ~WQPProcessor() 
    {
        delete w;
    }

    unsigned long Bind(CagmVectorField *_sourceB, CagmScalarField *_sourceW, CagmVectorField *_sourceGradW,
        CagmVectorField *_baseField, CagmVectorField *_baseWeight,
        CagmScalarField *_absField, CagmScalarField *_absWeight,
        CagmScalarField *_losField, CagmScalarField *_losWeight,
        double *_vcos,
        CagmVectorFieldOps *_outF, double *Lt)
    {
        w->Bind(_sourceB, _sourceW, _sourceGradW, _baseField, _baseWeight, _absField, _absWeight, _losField, _losWeight, _vcos, _outF, Lt);
        return 0;
    }

    virtual bool setTask(ATQPTask * _task)
    {
        bool finish = ATQPProcessor::setTask(_task);
        if (!finish)
        {   // cast *_task to the real object:
            this_task = (WQPTask *)_task;
        }
        return finish;
    }

    virtual bool proceed()
    {
        w->setTaskParams(this_task->getData(), this_task->getQueueID());
        w->ActionCore();

        return true;
    }
};

