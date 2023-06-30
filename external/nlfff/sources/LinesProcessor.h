#pragma once

#include "LinesTaskQueue11.h"

class CLinesTaskQueue;
class CagmVectorField;
class CagmRKF45;

class CLinesProcessor
{
public:
    uint32_t queueID;

protected:
    CagmRKF45 *rkf45;
    CagmVectorField *v;
    int dir;
    REALTYPE_A step;
    REALTYPE_A boundAchieve, boundAchieveBottom;
    int maxLength;
    int *passed;
    REALTYPE_A *coord;
    REALTYPE_A *linesteps;

    REALTYPE_A point[3];

    LQPSupervisor *supervisor;

public:
    CLinesProcessor(LQPSupervisor *, int, CagmVectorField *, int, REALTYPE_A, REALTYPE_A, REALTYPE_A, REALTYPE_A, REALTYPE_A, int, int *);
    virtual ~CLinesProcessor();

    virtual uint32_t setTaskParams(void * params);
    virtual uint32_t ActionCore();
};
