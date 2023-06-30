#include <stdio.h>
#include "mfoGlobals.h"
#include "MagFieldOps.h"
#include "console_debug.h"

#include "idl_export_ext.h"

int mfoNLFFFVersion(int argc, void* argv[])
{
    IDL_STRING *idlstr = (IDL_STRING *)argv[0];
    char *buffer = new char[512];
    int L = utilGetVersion(buffer, 512);
    strcpy(idlstr->s, buffer);
    idlstr->slen = (IDL_STRING_SLEN_T)strlen(buffer);

    delete [] buffer;

    return L;
}

int mfoNLFFF(int argc, void* argv[])
{
    utilInitialize();

    CidlPassParameterMap *m = (CidlPassParameterMap *)argv[0];
    int nErrors = m->parse(&mapInt, &mapuint64_t, &mapDouble);
    _proceedGlobals();

    int *N = (int *)argv[1];

    CagmVectorField *v = new CagmVectorField(N, (REALTYPE_A *)argv[2], (REALTYPE_A *)argv[3], (REALTYPE_A *)argv[4]);
    // make potential!?

    // weight
    CagmScalarField *sW = new CagmScalarField(N);
    // WiegelmannWeightType == SWF_COS!
    int xBounds[2], yBounds[2], zBounds[2];
    sW->Weight(1, WiegelmannWeightBound, xBounds, yBounds, zBounds);

    // abs cond, weight (5,6)
    // los cond, weight (7,8, 9)
    // base cond, weight (10, 11), (12,13), (14,15)
    // abs2 cond, weight2 (16,17)
    // los2 cond, weight2 (18,19)
    // base2 cond, weight2 (20,21), (22,23), (24,25)
    REALTYPE_A *fx = nullptr, *wx = nullptr, *fy = nullptr, *wy = nullptr, *fz = nullptr, *wz = nullptr, *fl = nullptr, *wl = nullptr, *fa = nullptr, *wa = nullptr;
    REALTYPE_A *fx2 = nullptr, *wx2 = nullptr, *fy2 = nullptr, *wy2 = nullptr, *fz2 = nullptr, *wz2 = nullptr, *fl2 = nullptr, *wl2 = nullptr, *fa2 = nullptr, *wa2 = nullptr;
    REALTYPE_A vc[] = {0, 0, 1};

    if (argc > 6 && argv[5] && argv[6])
    {
        fa = (REALTYPE_A *)(argv[5]);
        wa = (REALTYPE_A *)(argv[6]);
    }

    if (argc > 8 && argv[7] && argv[8])
    {
        fl = (REALTYPE_A *)(argv[7]);
        wl = (REALTYPE_A *)(argv[8]);
        if (argc > 9 && argv[9])
        {
            vc[0] = ((REALTYPE_A *)argv[9])[0];
            vc[1] = ((REALTYPE_A *)argv[9])[1];
            vc[2] = ((REALTYPE_A *)argv[9])[2];
        }
    }

    if (argc > 11 && argv[10] && argv[11])
    {
        fx = (REALTYPE_A *)(argv[10]);
        wx = (REALTYPE_A *)(argv[11]);
    }

    if (argc > 13 && argv[12] && argv[13])
    {
        fy = (REALTYPE_A *)(argv[12]);
        wy = (REALTYPE_A *)(argv[13]);
    }

    if (argc > 15 && argv[14] && argv[15])
    {
        fz = (REALTYPE_A *)(argv[14]);
        wz = (REALTYPE_A *)(argv[15]);
    }

    if (argc > 17 && argv[16] && argv[17])
    {
        fa2 = (REALTYPE_A *)(argv[16]);
        wa2 = (REALTYPE_A *)(argv[17]);
    }

    if (argc > 19 && argv[18] && argv[19])
    {
        fl2 = (REALTYPE_A *)(argv[18]);
        wl2 = (REALTYPE_A *)(argv[19]);
    }

    if (argc > 21 && argv[20] && argv[21])
    {
        fx2 = (REALTYPE_A *)(argv[20]);
        wx2 = (REALTYPE_A *)(argv[21]);
    }

    if (argc > 23 && argv[22] && argv[23])
    {
        fy2 = (REALTYPE_A *)(argv[22]);
        wy2 = (REALTYPE_A *)(argv[23]);
    }

    if (argc > 25 && argv[24] && argv[25])
    {
        fz2 = (REALTYPE_A *)(argv[24]);
        wz2 = (REALTYPE_A *)(argv[25]);
    }

    // argv[26] reserved for user-defined weight

    CagmScalarField *absConditions = nullptr, *absWeight = nullptr;
    CagmScalarField *absConditions2 = nullptr, *absWeight2 = nullptr;
    if (fa)
    {
        absConditions = new CagmScalarField(N);
        absConditions->setField(fa);
        absWeight = new CagmScalarField(N);
        absWeight->setField(wa);
    }
    if (fa2)
    {
        absConditions2 = new CagmScalarField(N);
        absConditions2->setField(fa2);
        absWeight2 = new CagmScalarField(N);
        absWeight2->setField(wa2);
    }

    CagmScalarField *losConditions = nullptr, *losWeight = nullptr;
    CagmScalarField *losConditions2 = nullptr, *losWeight2 = nullptr;
    if (fl)
    {
        losConditions = new CagmScalarField(N);
        losConditions->setField(fl);
        losWeight = new CagmScalarField(N);
        losWeight->setField(wl);
    }
    if (fl2)
    {
        losConditions2 = new CagmScalarField(N);
        losConditions2->setField(fl2);
        losWeight2 = new CagmScalarField(N);
        losWeight2->setField(wl2);
    }

    CagmVectorField *baseConditions = nullptr, *baseWeight = nullptr;
    CagmVectorField *baseConditions2 = nullptr, *baseWeight2 = nullptr;
    if (fx || fy || fz)
    {
        baseConditions = new CagmVectorField(N);
        baseConditions->zero();
        baseWeight = new CagmVectorField(N);
        baseWeight->zero();
        if (fx)
        {
            baseConditions->CopyComp(fx, 0);
            baseWeight->CopyComp(wx, 0);
        }
        if (fy)
        {
            baseConditions->CopyComp(fy, 1);
            baseWeight->CopyComp(wy, 1);
        }
        if (fz)
        {
            baseConditions->CopyComp(fz, 2);
            baseWeight->CopyComp(wz, 2);
        }
    }
    if (fx2 || fy2 || fz2)
    {
        baseConditions2 = new CagmVectorField(N);
        baseConditions2->zero();
        baseWeight2 = new CagmVectorField(N);
        baseWeight2->zero();
        if (fx2)
        {
            baseConditions2->CopyComp(fx2, 0);
            baseWeight2->CopyComp(wx2, 0);
        }
        if (fy2)
        {
            baseConditions2->CopyComp(fy2, 1);
            baseWeight2->CopyComp(wy2, 1);
        }
        if (fz2)
        {
            baseConditions2->CopyComp(fz2, 2);
            baseWeight2->CopyComp(wz2, 2);
        }
    }

    // Callback, 27
    PROTO_mfoWiegelmannCallback callback = nullptr;
    if (argc > 27 && argv[27])
        callback = (PROTO_mfoWiegelmannCallback)(argv[27]);

    if (WiegelmannSynchronous)
    {
        mfoWiegelmannProcedure(v, sW, baseConditions, baseWeight, baseConditions2, baseWeight2, 
            absConditions, absWeight, absConditions2, absWeight2, 
            losConditions, losWeight, losConditions2, losWeight2, vc, callback);
        v->GetComp((REALTYPE_A *)argv[2], 0);
        v->GetComp((REALTYPE_A *)argv[3], 1);
        v->GetComp((REALTYPE_A *)argv[4], 2);
    }

    return 0;
}

int mfoLines(int argc, void* argv[])
{
    std::map<std::string, int> mapInt;
    std::map<std::string, uint64_t> mapuint64_t;
    std::map<std::string, double> mapDouble;

    uint32_t conditions = 0x3;
    int n_processes = 0;
    REALTYPE_A chromo_level = 1;
    REALTYPE_A step = 1;
    REALTYPE_A tolerance = 1e-3;
    REALTYPE_A toleranceBound = 1e-3;
    REALTYPE_A toleranceCoord = 1e-3;      // 2do!
    REALTYPE_A toleranceClosed = 1e-2;     // 2do!

    mapInt.insert({"reduce_passed", conditions});
    mapInt.insert({"n_processes", n_processes});

    mapDouble.insert({ "chromo_level", chromo_level });
    mapDouble.insert({ "step", step });
    mapDouble.insert({ "tolerance", tolerance });
    mapDouble.insert({ "toleranceBound", toleranceBound });
    mapDouble.insert({ "toleranceCoord", toleranceCoord });
    mapDouble.insert({ "toleranceClosed", toleranceClosed });

    int c = 0;
    //#pragma pack(push, 1)
    CidlPassParameterMap *m = (CidlPassParameterMap *)argv[c++];
    //#pragma pack(pop, 1)
    int nErrors = m->parse(&mapInt, &mapuint64_t, &mapDouble);
    if (nErrors != 0)
        return LIB_STATE_NO_PARAMETER;

    int cond = mapInt["reduce_passed"];
    conditions = cond;
    n_processes = mapInt["n_processes"];

    chromo_level = mapDouble["chromo_level"];
    step = mapDouble["step"];
    tolerance = mapDouble["tolerance"];
    toleranceBound = mapDouble["toleranceBound"];
    toleranceCoord = mapDouble["toleranceCoord"];
    toleranceClosed = mapDouble["toleranceClosed"];

    uint64_t *largv = (uint64_t *)argv;

    int *N = (int *)argv[c++];
    REALTYPE_A *Bx = (REALTYPE_A *)argv[c++];
    REALTYPE_A *By = (REALTYPE_A *)argv[c++];
    REALTYPE_A *Bz = (REALTYPE_A *)argv[c++];

    int *status = (int *)argv[c++];
    REALTYPE_A *physLength = (REALTYPE_A *)argv[c++];
    REALTYPE_A *avField = (REALTYPE_A *)argv[c++];
    int *startIdx = (int *)argv[c++];
    int *endIdx = (int *)argv[c++];
    int *apexIdx = (int *)argv[c++];
    int *seedIdx = (int *)argv[c++];

    REALTYPE_A *seeds = (REALTYPE_A *)argv[c++];
    int *Nseeds_a = (int *)argv[c++];
    int Nseeds = 0;
    if (Nseeds_a)
        Nseeds = *Nseeds_a;
    uint64_t *maxLength_a = (uint64_t *)argv[c++];
    uint64_t maxLength = 0;
    if (maxLength_a)
        maxLength = *maxLength_a;

    uint64_t *totalLength = (uint64_t *)argv[c++];
    int *nLines = (int *)argv[c++];
    int *nPassed = (int *)argv[c++];
    REALTYPE_A *coord = (REALTYPE_A *)argv[c++];
    uint64_t *linesStart = (uint64_t *)argv[c++];
    int *linesLength = (int *)argv[c++];
    int *linesIndex = (int *)argv[c++];

    int *codes = (int *)argv[c++];

    return mfoGetLines(N, Bx, By, Bz,
        conditions, chromo_level,
        seeds, Nseeds,
        n_processes,
        step, tolerance, toleranceBound,
        nLines, nPassed,
        status, physLength, avField,
        linesLength, codes,
        startIdx, endIdx, apexIdx,
        maxLength, totalLength, coord, linesStart, linesIndex, seedIdx);
}