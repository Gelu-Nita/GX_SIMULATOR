#include "stdDefinitions.h"
#include <thread>
#include "WiegelmannProcessor2.h"
#include "WiegelmannTaskQueue11.h"
#include "WiegelmannTaskProcessor11.h"
#include "mfoGlobals.h"
#include <cmath>

CWiegelmannProcessor::CWiegelmannProcessor(int *N, int _nChunks, int nThreads, int nExtension, REALTYPE_A *steps) //, int _nPriority, uint32_t _ms, REALTYPE_A *steps, bool bFixAffinity)
    : nChunks(_nChunks),
    sourceGradW(nullptr)
{
    factory = new WQPTaskFactory();
    supervisor = new WQPSupervisor(N, _nChunks, nThreads, nExtension, factory);
    for (int i = 0; i < nThreads; i++)
        processors.push_back(new WQPProcessor(supervisor, N, i, steps));
    proc = new TaskQueueProcessor;

    //::SetThreadPriority(::GetCurrentThread(), THREAD_PRIORITY_LOWEST);
}

CWiegelmannProcessor::~CWiegelmannProcessor()
{
    size_t num_proc = processors.size();
    for (size_t i = 0; i < num_proc; i++)
        delete processors[i];

    delete supervisor;
    delete proc;
    delete factory;

    delete sourceGradW;
}

uint32_t CWiegelmannProcessor::Bind(CagmVectorField *_sourceB, CagmScalarField *_sourceW, 
                                 CagmVectorField *_baseField, CagmVectorField *_baseWeight, CagmVectorField *_baseField2, CagmVectorField *_baseWeight2,
                                 CagmScalarField *_absField, CagmScalarField *_absWeight, CagmScalarField *_absField2, CagmScalarField *_absWeight2,
                                 CagmScalarField *_losField, CagmScalarField *_losWeight, CagmScalarField *_losField2, CagmScalarField *_losWeight2,
                                 REALTYPE_A *_vcos,
                                 CagmVectorFieldOps *_outF, REALTYPE_A *Lt)
{
    sourceB = _sourceB;
    sourceW = _sourceW;
    if (sourceGradW)
        delete sourceGradW;
    sourceGradW = new CagmVectorField(sourceB->N);
    sourceGradW->gradScheme(sourceW, WiegelmannDerivStencil);
    baseField = _baseField;
    baseWeight = _baseWeight;
    absField = _absField;
    absWeight = _absWeight;
    losField = _losField;
    losWeight = _losWeight;
    outF = _outF;

    for (int n = 0; n < processors.size(); n++)
        ((WQPProcessor *)(processors[n]))->Bind(sourceB, sourceW, sourceGradW, baseField, baseWeight, absField, absWeight, losField, losWeight, _vcos, outF, Lt);

    return 0;
}

uint32_t CWiegelmannProcessor::Step()
{
    outF->zero();
    supervisor->reset();

    proc->proceed(processors, supervisor);

    return 0;
}

int CWiegelmannProcessor::GetChuckSize()
{
    int *M = supervisor->ChunkSize();
    return M[2];
}

int CWiegelmannProcessor::parallelDefine(int *N, int chSizeMax, int chSizeOpt, int nTasks0, int nStencils, int *pnChunks, int *pnTasks, int *pnExtension)
{
    *pnTasks = (int)std::thread::hardware_concurrency();

    if (nStencils == 3)
        *pnExtension = 2;
    else
        *pnExtension = 4;
    int minChunks = (int)std::ceil((double)(N[2] - *pnExtension) / (double)(chSizeMax - *pnExtension));
    int optChunks = (int)std::ceil((double)(N[2] - *pnExtension) / (double)(chSizeOpt - *pnExtension));
    if (optChunks < 1)
        optChunks = 1;

    if (*pnTasks > optChunks)
        *pnTasks = optChunks;

    *pnChunks = *pnTasks;
    if (*pnChunks < minChunks)
        *pnChunks = minChunks;

    return *pnTasks;
}


