#pragma once

#include "LinesTaskQueue11.h"
#include "LinesProcessor.h"

class LQPProcessor : public ATQPProcessor
{
protected:
    LQPTask *this_task;
    CLinesProcessor *w;

public:
    LQPProcessor(LQPSupervisor *supervisor, int _id, CagmVectorField *_v, int _dir, REALTYPE_A _step, REALTYPE_A _relErr, REALTYPE_A _absErr
        , REALTYPE_A _boundAchieve, REALTYPE_A _boundAchieveBottom, int _maxLength, int *_passed) : ATQPProcessor(_id)
    {
        w = new CLinesProcessor(supervisor, _id, _v, _dir, _step, _relErr, _absErr
            , _boundAchieve, _boundAchieveBottom, _maxLength, _passed);
    }
    virtual ~LQPProcessor()
    {
        delete w;
    }

    virtual bool setTask(ATQPTask * _task)
    {
        bool finish = ATQPProcessor::setTask(_task);
        if (!finish)
        {   // cast *_task to the real object:
            this_task = (LQPTask *)_task;
        }
        return finish;
    }

    virtual bool proceed()
    {
        double *params = this_task->getData();
        w->setTaskParams(params);
        w->queueID = this_task->getID();
        w->ActionCore();

        delete this_task;

        return true;
    }
};
