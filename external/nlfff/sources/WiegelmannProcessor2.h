#pragma once

#include "WiegelmannTaskQueue11.h"
#include "agpWiegelmann.h"
#include "agmScalarField.h"
#include "agmVectorField.h"

class WQPSupervisor;

class CWiegelmannProcessor
{
public:
    // output
    CagmVectorFieldOps *outF;

protected:
    // input
    CagmVectorField *sourceB;
    CagmScalarField *sourceW;
    CagmVectorField *baseField;
    CagmVectorField *baseWeight;
    CagmScalarField *absField;
    CagmScalarField *absWeight;
    CagmScalarField *losField;
    CagmScalarField *losWeight;

    CagmVectorField *sourceGradW;

    uint32_t ms;
    int nPriority;
    int nChunks;

    WQPTaskFactory *factory;
    WQPSupervisor *supervisor;
    TaskQueueProcessor *proc;
    std::vector<ATQPProcessor *> processors;

public:
    CWiegelmannProcessor(int *N, int chSize, int nThreads, int nExtension, REALTYPE_A *steps = nullptr); //, int nPriority = THREAD_PRIORITY_LOWEST, uint32_t _ms = INFINITE, REALTYPE_A *steps = nullptr, bool bFixAffinity = false);
    virtual ~CWiegelmannProcessor();

    uint32_t Bind(CagmVectorField *_sourceB, CagmScalarField *_sourceW,
        CagmVectorField *_baseField, CagmVectorField *_baseWeight, CagmVectorField *_baseField2, CagmVectorField *_baseWeight2,
        CagmScalarField *_absField, CagmScalarField *_absWeight, CagmScalarField *_absField2, CagmScalarField *_absWeight2,
        CagmScalarField *_losField, CagmScalarField *_losWeight, CagmScalarField *_losField2, CagmScalarField *_losWeight2,
        REALTYPE_A *_vcos,
        CagmVectorFieldOps *_outF, REALTYPE_A *Lt);

    uint32_t Step();

    int GetChuckSize();

    static int parallelDefine(int *N, int chSizeMax, int chSizeOpt, int nTasks0, int nStencils, int *pnChunks, int *pnTasks, int *pnExtension);
};
