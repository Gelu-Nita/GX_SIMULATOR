#include "stdDefinitions.h"
#include "MagFieldOps.h"
#include "mfoGlobals.h"
#include "WiegelmannProcessor2.h"
#include "agmRotate3D.h"

#include "console_debug.h"

static bool getMetrics(CagmVectorField *init, CagmVectorField *prev, CagmVectorField *B, CagmMetrics *m)
{
    if (WiegelmannGetMetricsTheta)
        B->getThetaMetrics(WiegelmannWeightBound, WiegelmannDerivStencil, m->FFtheta);
    if (WiegelmannGetMetricsDiffInit)
        B->getDifference(init, WiegelmannWeightBound, WiegelmannDerivStencil, m->dabs, m->dcos, m->cm);
    if (WiegelmannGetMetricsDiffPrev)
        B->getDifference(prev, WiegelmannWeightBound, WiegelmannDerivStencil, m->pdabs, m->pdcos, m->pcm);

    return 0;
}

 static bool proceedDL(double dL, double L, int stepN, double *mL, double *mAv)
 {
     //WiegelmannProcdLStdVal = WiegelmannProcdLStdValMain;
     //WiegelmannProcdLStdWin = WiegelmannProcdLStdWinMain;
     if (stepN == 0)
         mAv[stepN] = 0.0;
     else if (stepN < WiegelmannProcdLStdWin)
         mAv[stepN] = L;
     else
     {
         for (int k = 1; k < WiegelmannProcdLStdWin; k++)
             mAv[k-1] = mAv[k];
         mAv[WiegelmannProcdLStdWin-1] = L;

         double s = 0, s2 = 0;
         for (int k = 0; k < WiegelmannProcdLStdWin; k++)
         {
             s += mAv[k];
             s2 += mAv[k]*mAv[k];
         }

         REALTYPE_A w = WiegelmannProcdLStdWin;
         double var = sqrt(w/(w-1)*(w*s2/(s*s)-1));
         if (var < WiegelmannProcdLStdVal)
             return true;
     }

     if (stepN == 0)
        mL[stepN] = 0.0;
    else if (stepN <= WiegelmannProcdLIter)
        mL[stepN] = dL;
    else
    {
        double maxdl = 0.0;
        for (int k = 1; k < WiegelmannProcdLIter; k++)
        {
            mL[k-1] = mL[k];
            if (mL[k-1] > maxdl)
                maxdl = mL[k-1];
        }
        mL[WiegelmannProcdLIter-1] = dL;
        if (mL[WiegelmannProcdLIter-1] > maxdl)
            maxdl = mL[WiegelmannProcdLIter-1];

        if (maxdl < WiegelmannProcdLStop)
            return true;
    }

    return false;
}

uint32_t mfoWiegelmannProcedureCore(CagmVectorField *field, CagmScalarField *weight, 
    CagmVectorField *baseField, CagmVectorField *baseWeight, CagmVectorField *baseField2, CagmVectorField *baseWeight2,
    CagmScalarField *absField, CagmScalarField *absWeight, CagmScalarField *absField2, CagmScalarField *absWeight2,
    CagmScalarField *losField, CagmScalarField *losWeight, CagmScalarField *losField2, CagmScalarField *losWeight2,
    REALTYPE_A *vcos, int depth, PROTO_mfoWiegelmannCallback callback, REALTYPE_A *maxStep)
{
    CagmMetrics *metrics = new CagmMetrics(1);

    int k;
    REALTYPE_A *memoryL = new REALTYPE_A[WiegelmannProcdLIter+1];
    REALTYPE_A *memoryAv = new REALTYPE_A[WiegelmannProcdLStdWin];

    int *N = field->GetDimensions();
    CagmVectorField *vF = new CagmVectorField(N);
    vF->SetSteps(field->GetSteps());

    int NB[3];
    memcpy(NB, N, 3*sizeof(int));
    NB[0] = 2;
    CagmVectorField boundsx(NB);
    memcpy(NB, N, 3*sizeof(int));
    NB[1] = 2;
    CagmVectorField boundsy(NB);
    memcpy(NB, N, 3*sizeof(int));
    NB[2] = 2;
    CagmVectorField boundsz(NB);
    field->getBounds(&boundsx, &boundsy, &boundsz);
    NB[2] = 1;

    CagmRotate3D rotator(vcos);

    if (WiegelmannProcCondType == 2)
        field->condWeight(WiegelmannProcCondBase, baseField, baseWeight, WiegelmannProcCondBase2, baseField2, baseWeight2,
            WiegelmannProcCondAbs, absField, absWeight, WiegelmannProcCondAbs2, absField2, absWeight2,
            WiegelmannProcCondLOS, losField, losWeight, WiegelmannProcCondLOS2, losField2, losWeight2, &rotator);

    int nChunks, nTasks, nExtension;
    CWiegelmannProcessor::parallelDefine(N, WiegelmannChunkSizeMax, WiegelmannChunkSizeOpt, WiegelmannChunkTasks, WiegelmannDerivStencil, &nChunks, &nTasks, &nExtension);
    CWiegelmannProcessor *proc = new CWiegelmannProcessor(N, nChunks, nTasks, nExtension, field->GetSteps());
    //int nq = proc->GetQueueN();
    int nq = nChunks;
    REALTYPE_A *Lt = new REALTYPE_A[nq];

    proc->Bind(field, weight, baseField, baseWeight, baseField2, baseWeight2, 
               absField, absWeight, absField2, absWeight2, 
               losField, losWeight, losField2, losWeight2, vcos, vF, Lt);

    int d = proc->GetChuckSize();

    int stepN = 0;
    int stop = 0;

    proc->Step();
    int iterN = 1;

    REALTYPE_A L0 = 0;
    for (k = 0; k < nq; k++)
        L0 += Lt[k];

    REALTYPE_A L, Lprev = L0;
    proceedDL(0.0, 0.0, 0, memoryL, memoryAv);

    CagmVectorField *initField = nullptr;
    if (WiegelmannGetMetricsDiffInit)
        initField = new CagmVectorField(*field);

    CagmVectorField *prevField = new CagmVectorField(*field);
    prevField->SetSteps(field->GetSteps());
    CagmVectorField *prevVF = new CagmVectorField(*vF);
    prevVF->SetSteps(field->GetSteps());

    memcpy(NB, N, 3*sizeof(int));
    NB[2] = 1;
    CagmVectorField vt(NB);
    field->getPlane(&vt, PLANE_Z, 0, 0);

    CagmScalarField t(NB);
    t.abs(&vt);
    REALTYPE_A Bav = t.avPhys();

    vF->getPlane(&vt, PLANE_Z, 0, 0);
    t.abs(&vt);
    REALTYPE_A Fmax = t.maxval();
    REALTYPE_A step0;
    if (WiegelmannProcUsePrev > 0)
        step0 = WiegelmannProcUsePrev;
    else
        step0 = Bav/Fmax*WiegelmannProcStep0;
    REALTYPE_A step = step0, dL;
    *maxStep = step0;

    int reason = 0;

    while (true)
    {
        vF->mult(step);
        field->add(vF);
        field->setBounds(&boundsx, &boundsy, &boundsz);
        if (WiegelmannProcCondType == 2)
            field->condWeight(WiegelmannProcCondBase, baseField, baseWeight, WiegelmannProcCondBase2, baseField2, baseWeight2,
                WiegelmannProcCondAbs, absField, absWeight, WiegelmannProcCondAbs2, absField2, absWeight2,
                WiegelmannProcCondLOS, losField, losWeight, WiegelmannProcCondLOS2, losField2, losWeight2, &rotator);

        uint32_t rc = proc->Step();
        iterN++;

        //if (rc == WAIT_TIMEOUT)
        //    reason = 5;
        //else if (rc == WAIT_FAILED)
        //    reason = 6;
        //else
        //{
            L = 0;
            for (int k = 0; k < nq; k++)
                L += Lt[k];

            dL = Lprev - L;
            if (dL/Lprev <= -WiegelmannProcFunctionalLimit)
            {
                field->Copy(*prevField);
                vF->Copy(*prevVF);
                if (step < step0*WiegelmannProcStepLim)
                    reason = 1;
                else
                    step *= WiegelmannProcStepDecr;
            }
            else
            {
                stepN++;

                getMetrics(initField, prevField, field, metrics);
                metrics->setBase(L/L0, step, stepN, depth, iterN);

                Lprev = L;
                prevField->Copy(*field);
                prevVF->Copy(*vF);

                step *= WiegelmannProcStepIncr;
                if (step > step0*WiegelmannProcStepMax)
                    step = step0*WiegelmannProcStepMax;
                if (step > *maxStep)
                    *maxStep = step;

                if (stepN > WiegelmannProcMaxSteps)
                    reason = 3;
                else if (proceedDL(dL/L0, L/L0, stepN, memoryL, memoryAv))
                    reason = 7;

                if (callback && stepN%WiegelmannProtocolStep == 0)
                    callback(step/step0, d, nChunks, nTasks, depth, dL/L0/step, metrics, field, &stop);

                // theta etc. reasons from 8
            }
//        }

        if (reason != 0)
        {
            field->Copy(*prevField);
            vF->Copy(*prevVF);
            break;
        }

        if (stop)
        {
            reason = 4;
            break;
        }
    }

    field->setBounds(&boundsx, &boundsy, &boundsz);
    if (WiegelmannProcCondType == 2)
        field->condWeight(WiegelmannProcCondBase, baseField, baseWeight, WiegelmannProcCondBase2, baseField2, baseWeight2,
            WiegelmannProcCondAbs, absField, absWeight, WiegelmannProcCondAbs2, absField2, absWeight2,
            WiegelmannProcCondLOS, losField, losWeight, WiegelmannProcCondLOS2, losField2, losWeight2, &rotator);

    stop = reason;
    getMetrics(initField, prevField, field, metrics);
    metrics->setBase(L / L0, step, stepN, depth, iterN);
    if (callback)
        callback(step/step0, d, nChunks, nTasks, depth, dL/L0, metrics, field, &stop);

    delete initField;
    delete prevField;
    delete prevVF;
    delete vF;
    delete proc;
    delete [] Lt;
    delete [] memoryL;

    return reason;
}

static int xfloor(REALTYPE_A v)
{
    int c = (int)ceil(v);
    if (c - v < 0.5)
        return c;
    else
        return c-1;
}

__declspec( dllexport ) uint32_t mfoWiegelmannProcedure(CagmVectorField *field, CagmScalarField *weight, 
    CagmVectorField *baseField, CagmVectorField *baseWeight, CagmVectorField *baseField2, CagmVectorField *baseWeight2,
    CagmScalarField *absField, CagmScalarField *absWeight, CagmScalarField *absField2, CagmScalarField *absWeight2, 
    CagmScalarField *losField, CagmScalarField *losWeight, CagmScalarField *losField2, CagmScalarField *losWeight2, 
    REALTYPE_A *vcos, PROTO_mfoWiegelmannCallback callback)
{
    console_start();

    if (!baseField || !baseWeight)
        WiegelmannProcCondBase = 0;
    if (!baseField2 || !baseWeight2)
        WiegelmannProcCondBase2 = 0;
    if (!absField || !absWeight)
        WiegelmannProcCondAbs = 0;
    if (!absField2 || !absWeight2)
        WiegelmannProcCondAbs2 = 0;
    if (!losField || !losWeight)
        WiegelmannProcCondLOS = 0;
    if (!losField2 || !losWeight2)
        WiegelmannProcCondLOS2 = 0;
    if (WiegelmannProcCondBase == 0 && WiegelmannProcCondBase2 == 0 &&
        WiegelmannProcCondAbs == 0 && WiegelmannProcCondAbs2 == 0 &&
        WiegelmannProcCondLOS == 0 && WiegelmannProcCondLOS2 == 0)
        WiegelmannProcCondType = 0;

    REALTYPE_A maxStep;
    bool bMatr = (bool)WiegelmannMatryoshkaUse;
    int depth;
    if (bMatr)
    {
        int minN = field->N[0];
        if (minN > field->N[1])
            minN = field->N[1];

        REALTYPE_A d = (REALTYPE_A)minN/WiegelmannMatryoshkaDeepMinN;
        depth = (int)ceil(log(d)/log(WiegelmannMatryoshkaFactor));
        if (depth < 2)
            bMatr = false;
    }
    uint32_t dwRC = 0;
    if (!bMatr)
    {
        WiegelmannProcStepIncr = WiegelmannProcStepIncrMain;
        WiegelmannProcStepDecr = WiegelmannProcStepDecrMain;
        WiegelmannProcStepLim = WiegelmannProcStepLimMain;
        WiegelmannProcdLStop = WiegelmannProcdLStopMain;
        WiegelmannProcdLIter = WiegelmannProcdLIterMain;
        WiegelmannProcFunctionalLimit = WiegelmannProcFunctionalLimitMain;
        WiegelmannProcdLStdVal = WiegelmannProcdLStdValMain;
        WiegelmannProcdLStdWin = WiegelmannProcdLStdWinMain;
        dwRC = mfoWiegelmannProcedureCore(field, weight, baseField, baseWeight, baseField2, baseWeight2,
            absField, absWeight, absField2, absWeight2,
            losField, losWeight, losField2, losWeight2, vcos, 1, callback, &maxStep);
    }
    else
    {
        REALTYPE_A factor = WiegelmannMatryoshkaFactor; //pow(d, 1.0/(depth-1));
        int *Ns = new int[3*depth];
        REALTYPE_A *steps = new REALTYPE_A[3*depth];
        Ns[0] = field->N[0];
        Ns[1] = field->N[1];
        Ns[2] = field->N[2];
        steps[0] = 1.0;
        steps[1] = 1.0;
        steps[2] = 1.0;
        for (int i = 1; i < depth; i++)
        {
            Ns[3*i  ] = xfloor( (Ns[3*(i-1)  ]-1.0)/factor + 1.0 ); 
            Ns[3*i+1] = xfloor( (Ns[3*(i-1)+1]-1.0)/factor + 1.0 ); 
            Ns[3*i+2] = xfloor( (Ns[3*(i-1)+2]-1.0)/factor + 1.0 ); 
        }
        REALTYPE_A cf = factor;
        for (int i = 1; i < depth; i++)
        {
            //steps[3*i  ] = (Ns[3*i  ]-1.0)/(Ns[0]-1.0) * cf;
            //steps[3*i+1] = (Ns[3*i+1]-1.0)/(Ns[1]-1.0) * cf;
            //steps[3*i+2] = (Ns[3*i+2]-1.0)/(Ns[2]-1.0) * cf;
            steps[3*i  ] = (Ns[3*i  ]-1.0)/(Ns[0]-1.0);
            steps[3*i+1] = (Ns[3*i+1]-1.0)/(Ns[1]-1.0);
            steps[3*i+2] = (Ns[3*i+2]-1.0)/(Ns[2]-1.0);
            cf *= factor;
        }

        WiegelmannProcStepIncr = WiegelmannProcStepIncrInit;
        WiegelmannProcStepDecr = WiegelmannProcStepDecrInit;
        WiegelmannProcStepLim = WiegelmannProcStepLimInit;
        WiegelmannProcdLStop = WiegelmannProcdLStopInit;
        WiegelmannProcdLIter = WiegelmannProcdLIterInit;
        WiegelmannProcFunctionalLimit = WiegelmannProcFunctionalLimitInit;
        WiegelmannProcdLStdVal = WiegelmannProcdLStdValInit;
        WiegelmannProcdLStdWin = WiegelmannProcdLStdWinInit;

        int xBounds[2], yBounds[2], zBounds[2];
        int *pN0 = Ns+3*(depth-1);
        CagmVectorField *v0 = new CagmVectorField(pN0);
        v0->stretch(field);
        v0->SetSteps(steps+3*(depth-1));
        CagmScalarField *sW0 = new CagmScalarField(pN0);
        sW0->Weight(SWF_COS, WiegelmannWeightBound, xBounds, yBounds, zBounds);
        sW0->SetSteps(steps+3*(depth-1));
        mfoWiegelmannProcedureCore(v0, sW0, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, vcos, depth, callback, &maxStep);
        WiegelmannProcUsePrev = maxStep*WiegelmannProcPrevStepFactor;
        delete sW0;
        CagmVectorField *v1;
        for (int i = depth-2; i > 0; i--)
        {
            WiegelmannProcStepIncr = WiegelmannProcStepIncrMatr;
            WiegelmannProcStepDecr = WiegelmannProcStepDecrMatr;
            WiegelmannProcStepLim = WiegelmannProcStepLimMatr;
            WiegelmannProcdLStop = WiegelmannProcdLStopMatr;
            WiegelmannProcdLIter = WiegelmannProcdLIterMatr;
            WiegelmannProcFunctionalLimit = WiegelmannProcFunctionalLimitMatr;
            WiegelmannProcdLStdVal = WiegelmannProcdLStdValMatr;
            WiegelmannProcdLStdWin = WiegelmannProcdLStdWinMatr;
            pN0 = Ns+3*i;
            v1 = new CagmVectorField(pN0);
            v1->stretch(v0);
            v1->SetSteps(steps+3*i);

            //int nconv[] = { 3,3,3 };
            //CagmScalarField conv(nconv);
            //conv.CreateConvWindow();
            //CagmVectorField smoothed(*v1);
            //smoothed.conv(v1, &conv);
            //v1->Copy(smoothed);

            delete v0;
            CagmScalarField *sW0 = new CagmScalarField(pN0);
            sW0->Weight(SWF_COS, WiegelmannWeightBound, xBounds, yBounds, zBounds);
            sW0->SetSteps(steps+3*i);
            mfoWiegelmannProcedureCore(v1, sW0, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr, vcos, i+1, callback, &maxStep);
            WiegelmannProcUsePrev = maxStep*WiegelmannProcPrevStepFactor;
            delete sW0;
            v0 = v1;
        }

        int NB[3];
        memcpy(NB, field->N, 3*sizeof(int));
        NB[0] = 2;
        CagmVectorField boundsx(NB);
        memcpy(NB, field->N, 3*sizeof(int));
        NB[1] = 2;
        CagmVectorField boundsy(NB);
        memcpy(NB, field->N, 3*sizeof(int));
        NB[2] = 2;
        CagmVectorField boundsz(NB);
        field->getBounds(&boundsx, &boundsy, &boundsz);

        field->stretch(v0);

        //int nconv[] = {3,3,3};
        //CagmScalarField conv(nconv);
        //conv.CreateConvWindow();
        //CagmVectorField smoothed(*field);
        //smoothed.conv(field, &conv);
        //field->Copy(smoothed);
        //smoothed.conv(field, &conv);
        //field->Copy(smoothed);

        field->setBounds(&boundsx, &boundsy, &boundsz);

        delete v0;

        delete [] Ns;
        delete [] steps;

        WiegelmannProcStepIncr = WiegelmannProcStepIncrMain;
        WiegelmannProcStepDecr = WiegelmannProcStepDecrMain;
        WiegelmannProcStepLim = WiegelmannProcStepLimMain;
        WiegelmannProcdLStop = WiegelmannProcdLStopMain;
        WiegelmannProcdLIter = WiegelmannProcdLIterMain;
        WiegelmannProcFunctionalLimit = WiegelmannProcFunctionalLimitMain;
        WiegelmannProcdLStdVal = WiegelmannProcdLStdValMain;
        WiegelmannProcdLStdWin = WiegelmannProcdLStdWinMain;

        dwRC = mfoWiegelmannProcedureCore(field, weight, baseField, baseWeight, baseField2, baseWeight2, 
                                          absField, absWeight, absField2, absWeight2, 
                                          losField, losWeight, losField2, losWeight2, vcos, 1, callback, &maxStep);
    }

    return dwRC;
}
