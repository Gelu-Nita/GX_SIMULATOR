#pragma once

class CagmMetricsJB
{
public:
    REALTYPE_A *m;
    REALTYPE_A *mj;
    REALTYPE_A *mB;

    CagmMetricsJB(int length)
    {
        m = new REALTYPE_A[length];
        mj = new REALTYPE_A[length];
        mB = new REALTYPE_A[length];
        for (int k = 0; k < length; k++)
        {
            m[k] = 0;
            mj[k] = 0;
            mB[k] = 0;
        }
    }
    ~CagmMetricsJB()
    {
        delete [] m;
        delete [] mj;
        delete [] mB;
    }

    void set(CagmMetricsJB *_m, int pos)
    {
        m[pos] = _m->m[0];
        mj[pos] = _m->mj[0];
        mB[pos] = _m->mB[0];
    }
};

class CagmMetricsLim
{
public:
    CagmMetricsJB *mW;
    CagmMetricsJB *mL;

    CagmMetricsLim(int length)
    {
        mW = new CagmMetricsJB(length);
        mL = new CagmMetricsJB(length);
    }
    ~CagmMetricsLim()
    {
        delete mW;
        delete mL;
    }

    void set(CagmMetricsLim *_m, int pos)
    {
        mW->set(_m->mW, pos);
        mL->set(_m->mL, pos);
    }
};

class CagmMetricsCos
{
public:
    REALTYPE_A *c;
    REALTYPE_A *B4c;

    CagmMetricsCos(int length)
    {
        c = new REALTYPE_A[length];
        B4c = new REALTYPE_A[length];
        for (int k = 0; k < length; k++)
        {
            c[k] = 0;
            B4c[k] = 0;
        }
    }
    ~CagmMetricsCos()
    {
        delete [] c;
        delete [] B4c;
    }

    void set(CagmMetricsCos *_m, int pos)
    {
        c[pos] = _m->c[0];
        B4c[pos] = _m->B4c[0];
    }
};

class CagmMetricsCosLim
{
public:
    CagmMetricsCos *mW;
    CagmMetricsCos *mL;

    CagmMetricsCosLim(int length)
    {
        mW = new CagmMetricsCos(length);
        mL = new CagmMetricsCos(length);
    }
    ~CagmMetricsCosLim()
    {
        delete mW;
        delete mL;
    }

    void set(CagmMetricsCosLim *_m, int pos)
    {
        mW->set(_m->mW, pos);
        mL->set(_m->mL, pos);
    }
};

class CagmMetrics
{
public:
    int *depth;
    int *iterN;
    REALTYPE_A *steps;
    int *stepNs;
    REALTYPE_A *dLs;
    REALTYPE_A *dtheta;
    REALTYPE_A *residual;
    CagmMetricsLim *FFtheta;
    CagmMetricsLim *dabs;
    CagmMetricsLim *dcos;
    CagmMetricsLim *pdabs;
    CagmMetricsLim *pdcos;
    CagmMetricsCosLim *cm;
    CagmMetricsCosLim *pcm;

    CagmMetrics(int length)
    {
        depth = new int[length];
        iterN = new int[length];
        steps = new REALTYPE_A[length];
        stepNs = new int[length];
        dLs = new REALTYPE_A[length];
        dtheta = new REALTYPE_A[length];
        residual = new REALTYPE_A[length];
        for (int k = 0; k < length; k++)
        {
            depth[k] = 0;
            iterN[k] = 0;
            steps[k] = 0;
            stepNs[k] = 0;
            dLs[k] = 0;
            dtheta[k] = 0;
            residual[k] = 0;
        }
        FFtheta = new CagmMetricsLim(length);
        dabs = new CagmMetricsLim(length);
        dcos = new CagmMetricsLim(length);
        pdabs = new CagmMetricsLim(length);
        pdcos = new CagmMetricsLim(length);
        cm = new CagmMetricsCosLim(length);
        pcm = new CagmMetricsCosLim(length);
    }
    ~CagmMetrics()
    {
        delete [] depth;
        delete [] iterN;
        delete[] steps;
        delete[] stepNs;
        delete [] dLs;
        delete [] dtheta;
        delete [] residual;
        delete FFtheta;
        delete dabs;
        delete dcos;
        delete pdabs;
        delete pdcos;
        delete cm;
        delete pcm;
    }

    void setBase(REALTYPE_A _dLs, REALTYPE_A _step, int _stepN, int _depth, int _iterN)
    {
        dLs[0] = _dLs;
        steps[0] = _step;
        stepNs[0] = _stepN;
        depth[0] = _depth;
        iterN[0] = _iterN;
    }

    void set(CagmMetrics *_m, int pos)
    {
        steps[pos] = _m->steps[0];
        stepNs[pos] = _m->stepNs[0];
        dLs[pos] = _m->dLs[0];
        dtheta[pos] = _m->dtheta[0];
        residual[pos] = _m->residual[0];
        FFtheta->set(_m->FFtheta, pos);
        dabs->set(_m->dabs, pos);
        dcos->set(_m->dcos, pos);
        pdabs->set(_m->pdabs, pos);
        pdcos->set(_m->pdcos, pos);
        cm->set(_m->cm, pos);
        pcm->set(_m->pcm, pos);
    }
};
