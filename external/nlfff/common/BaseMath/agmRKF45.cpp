// rkf45.cpp : Defines the entry point for the console application.
//

#include "stdDefinitions.h"

#include <stdio.h>

#define _USE_MATH_DEFINES
#include <math.h>

#include "agmRKF45.h"

#pragma warning (disable : 4127)
#pragma warning (disable : 4514)

#define MAX_KOP 10000
#define MAX_NFE 30000
#define EPS_U26 26.0
#define REMIN   1e-12

static REALTYPE_A coefs[] = {0.90, 0.81, 0.45, 0.34, 0.22};

static bool b = false;

//-----------------------------------------------------------------------------
static double mepsilon()
{
    double eps = 1.0, eps1 = 1.5;
    while (eps1 > 1)
    {
        eps /= 2.0;
        eps1 = eps + 1.0;
    }

    return eps;
}

//-----------------------------------------------------------------------------
CagmRKF45::CagmRKF45(double absErr, double relErr, RKF45_FUNCTION_VECTOR func, int n, void *par, 
    RKF45_FUNCTION_VECTOR_COND fcond, double absBoundAchieve)
    : m_funcv(func),
      m_n(n),
      m_vY(n),
      m_vYP(n),
      m_f1(n),
      m_f2(n),
      m_f3(n),
      m_f4(n),
      m_f5(n),
      m_bVect(true),
      m_fcondv(fcond),
      m_absBoundAchieve(absBoundAchieve)
{
    m_eps = mepsilon();
    m_u26 = EPS_U26*m_eps;
    reinit(absErr, relErr, par);
}

//-----------------------------------------------------------------------------
CagmRKF45::CagmRKF45(double absErr, double relErr, RKF45_FUNCTION_SCALAR func, void *par, 
    RKF45_FUNCTION_SCALAR_COND fcond, double absBoundAchieve)
    : m_funcs(func),
      m_n(1),
      m_vY(1),
      m_vYP(1),
      m_f1(1),
      m_f2(1),
      m_f3(1),
      m_f4(1),
      m_f5(1),
      m_bVect(false),
      m_fconds(fcond),
      m_absBoundAchieve(absBoundAchieve)
{
    m_eps = mepsilon();
    m_u26 = EPS_U26*m_eps;
    reinit(absErr, relErr, par);
}

//-----------------------------------------------------------------------------
void CagmRKF45::reinit(void *par)
{
    m_status = CagmRKF45::Status::None;
    m_bInit  = false;
    m_kop = 0;
    m_nfe = 0;
    m_bByStep = false;
    m_par = par;
}

//-----------------------------------------------------------------------------
void CagmRKF45::reinit(double absErr, double relErr, void *par)
{
    reinit(par);
    reset(absErr, relErr);
}

//-----------------------------------------------------------------------------
void CagmRKF45::reset(double absErr, double relErr)
{
    setAbsErr(absErr);
    setRelErr(relErr);
}

//-----------------------------------------------------------------------------
void CagmRKF45::setAbsErr(double absErr)
{
    m_absErr = absErr;
}

//-----------------------------------------------------------------------------
void CagmRKF45::setRelErr(double relErr)
{
    m_relErr = relErr;
}

//-----------------------------------------------------------------------------
CagmRKF45::Status CagmRKF45::calculate(double& t, CagmRKF45Vect& vY, double tOut, bool bByStep)
{
    if (m_absErr < 0 || m_relErr < 0)
        return m_status = CagmRKF45::Status::WrongCall;

    // can continue?
    if ((t == tOut) && !(m_bInit && bByStep))
        return m_status = CagmRKF45::Status::WrongCall;

    if (m_status == CagmRKF45::Status::TooLittleStep && m_absErr <= m_saveAE && m_relErr <= m_saveRE)
        return m_status = CagmRKF45::Status::TooLittleStep;

    if (m_status == CagmRKF45::Status::WrongErrBound && m_absErr == 0)
        return m_status = CagmRKF45::Status::WrongErrBound;

    //if (m_status == CagmRKF45::Status::TooManyCalcs)
    m_nfe = 0;

    m_saveRE = m_relErr;
    m_saveAE = m_absErr;

    // relative error cannot be too low
    double remin = REMIN;
    double rer = 2*m_eps + remin;
    if (m_relErr < rer)
    {
        m_relErr = rer;
        // NB! I suppose it's not error! 
        // 2do: how to inform user?
    }

    m_t = t;
    m_tOut = tOut;
    if (m_bVect)
        m_vY = vY.e;
    else
    {
        m_dY = vY[0];
        m_vY[0] = m_dY;
    }

    m_bByStep = bByStep;
    m_dt = m_tOut - m_t;

    if (!m_bInit)
    {
        if (m_bVect)
            m_funcv(m_par, m_t, m_vY, m_vYP);
        else
        {
            m_funcs(m_par, m_t, m_dY, m_dYP);
            m_vYP[0] = m_dYP;
        }
        m_nfe++;
        m_bInit = true;

        if (m_t == m_tOut)
            return m_status = CagmRKF45::Status::EndByStep;

        double h = fabs(m_dt);

        bool bH = false;
        for (int k = 0; k < m_n; k++)
        {
            double tol = m_relErr*fabs(m_vY.e[k]) + m_absErr;
            if (tol > 0.0)
            {
                bH = true;
                double ypk = fabs(m_vYP.e[k]);
                if (ypk*pow(m_h, 5) > tol)
                    m_h = pow(tol/ypk, 0.2);
            }
        }
        if (!bH)
            h = 0.0;

        m_h = fabs(m_t);
        if (fabs(m_dt) > m_h)
            m_h = fabs(m_dt);
        m_h *= m_u26;
        if (m_h < h)
            m_h = h;
    }

    m_status = integrator();
    t = m_t;
    vY = m_vY.e;

    return m_status;
}

//-----------------------------------------------------------------------------
double CagmRKF45::estCond(uint32_t dwRes)
{
    if (dwRes == 0)
        return 1;
    else if (dwRes < 2)
        return coefs[0];
    else if (dwRes < 4)
        return coefs[1];
    else if (dwRes < 8)
        return coefs[2];
    else if (dwRes < 16)
        return coefs[3];
    else
        return coefs[4];
}

//-----------------------------------------------------------------------------
CagmRKF45::Status CagmRKF45::integrator()
// must be defined: m_h, m_t, m_tOut, m_dt, m_kop, m_u26, m_n, m_vY, m_vYP, m_nfe, m_relErr, m_absErr, m_bByStep
{
    if (m_h * m_dt < 0)     // fortran 80
        m_h = -m_h;

    if (fabs(m_h) >= 2.0*fabs(m_dt))
        m_kop++;

    if (m_kop >= MAX_KOP)
    {
        m_kop = 0;
        return CagmRKF45::Status::TooManyExits;
    }

    if (fabs(m_dt) <= m_u26)    // fortran 85
    {   // too close to the end; extrapolate to the end
        for (int i = 0; i < m_n; i++)   // fortran 90
            m_vY.e[i] += m_dt*m_vYP.e[i];

        if (m_bVect)
            m_funcv(m_par, m_tOut, m_vY, m_vYP);
        else
        {
            m_dY = m_vY.e[0];
            m_funcs(m_par, m_tOut, m_dY, m_dYP);
            m_dYP = m_vYP.e[0];
        }
        m_nfe++;
        m_t = m_tOut;
        return CagmRKF45::Status::End;
    }

    bool bOutput = false;   // fortran 95
    double scale = 2.0/m_relErr;
    m_ae = scale*m_absErr;

    bool bNearBound = false;
    double nearBoundH = 0;
    double esttol = 0.0;
    bool isnext = false;
    while (true) // step by step...
    { 
        bool bHFailed = false; // fortran 100 + conditions
        double hmin = m_u26*fabs(m_t);  // min. possible step

        m_dt = m_tOut - m_t;
        if (fabs(m_dt) < 2.0*fabs(m_h))
        {
            if (fabs(m_dt) > fabs(m_h))
                m_h = 0.5*m_dt;
            else
            {   // next successful step will terminate integration up to tOut
                bOutput = true;
                m_h = m_dt;         // fortran 150
            }
        }

        uint32_t dwRes = 0;
        while (true)
        {
            if (m_nfe > MAX_NFE)       // fortran 200
                return CagmRKF45::Status::TooManyCalcs;

            dwRes = 0;
            // here: m_t, m_h, m_n, m_vY, m_vYP defined already
            if (m_bVect)
            {
                dwRes = fehl(m_f1);     // fortran 220;                     
                if (m_fcondv && dwRes == 0)
                    dwRes = m_fcondv(m_par, m_f1);
            }
            else
            {
                dwRes = fehl(m_df1);
                if (m_fconds && dwRes == 0)
                    dwRes = m_fconds(m_par, m_f1[0]);
            }
            m_nfe += 5;

            // check step success
            if (dwRes == 0)
            {
                CagmRKF45::Status status;
                double eeoet = getEeEt(status);
                if (status != CagmRKF45::Status::None)
                    return status;
                esttol = fabs(m_h)*eeoet*scale / 752400.0;
                if (esttol <= 1.0) // success
                    break;

                // unsuccessful step   
                bHFailed = true;
                bOutput = false;
                // next step, decrease up to 0.1
                double s = 0.1;
                if (esttol < 59049.0)
                    s = 0.9 / pow(esttol, 0.2);

                m_h *= s;
                if (fabs(m_h) <= hmin)
                    return CagmRKF45::Status::TooLittleStep;
            }
            else // dwRes != 0
            {
                bNearBound = true;
                nearBoundH = m_h;
                double s = estCond(dwRes);
                m_h *= s;
                if (fabs(m_h) <= m_absBoundAchieve)
                    return isnext ? CagmRKF45::Status::EndByCond : CagmRKF45::Status::EndNoMove;
            }
        }

        // success
        isnext = true;
        m_t += m_h;     // fortran 260

        if (m_bVect)
        {
            m_vY = m_f1.e;
            m_funcv(m_par, m_t, m_vY, m_vYP);
        }
        else
        {
            m_dY = m_df1;
            m_funcs(m_par, m_t, m_dY, m_dYP);
            m_vY[0] = m_dY;
            m_vYP[0] = m_dYP;
        }

        m_nfe++;

        // next step, increase up to 5
        double s = 5.0;
        if (esttol > 1.889568e-4)
            s = 0.9/pow(esttol, 0.2);
        if (bHFailed && s > 1.0)
            s = 1.0;
        if (bNearBound && s > 1.3 && fabs((nearBoundH - m_h) / nearBoundH) < 0.2)
            s = 1.3;
        double h = s*fabs(m_h);
        if (h < hmin)
            h = hmin;
        m_h = (m_h > 0 ? h : -h);

        // do we need next step?
        if (bOutput)
        {
            m_t = m_tOut;
            return CagmRKF45::Status::End;
        }
        else
        {
            if (m_bByStep)
            {   // one step OK
                return CagmRKF45::Status::EndByStep;
            }
        }
    }
}

//-----------------------------------------------------------------------------
double CagmRKF45::getEeEt(CagmRKF45::Status& status)
{
    status = CagmRKF45::Status::None;
    double eeoet = 0;
    if (m_bVect)
    {
        for (int k = 0; k < m_n; k++)
        {
            double et = fabs(m_vY.e[k]) + fabs(m_f1.e[k]) + m_ae;
            if (et <= 0.0)
            {
                status = CagmRKF45::Status::WrongErrBound;
                return 0;
            }

            double ee = fabs((-2090.0*m_vYP.e[k] + (21970.0*m_f3.e[k] - 15048.0*m_f4.e[k])) +
                (22528.0*m_f2.e[k] - 27360.0*m_f5.e[k]));
            ee /= et;
            if (eeoet < ee)
                eeoet = ee;
        }       // fortran 250 
    }
    else
    {
        double et = fabs(m_dY) + fabs(m_df1) + m_ae;
        if (et <= 0.0)
        {
            status = CagmRKF45::Status::WrongErrBound;
            return 0;
        }

        double ee = fabs((-2090.0*m_dYP + (21970.0*m_df3 - 15048.0*m_df4)) +
            (22528.0*m_df2 - 27360.0*m_df5));
        eeoet = ee / et;
    }

    return eeoet;
}

//-----------------------------------------------------------------------------
CagmRKF45::~CagmRKF45()
{
    //m_vY.clear();
    //m_vYP.clear();
    //m_f1.clear();
    //m_f2.clear();
    //m_f3.clear();
    //m_f4.clear();
    //m_f5.clear();
}

//-----------------------------------------------------------------------------
uint32_t CagmRKF45::fehl(CagmRKF45Vect& s)
{
    double ch;
    int k;
    uint32_t rcx[5];

    ch = m_h/4.0;
    for (k = 0; k < m_n; k++)
        m_f5.e[k] = m_vY.e[k] + ch*m_vYP.e[k];
    rcx[4] = m_funcv(m_par, m_t + ch, m_f5, m_f1)<<4;

    ch = 3.0*m_h/32.0;
    for (k = 0; k < m_n; k++)
        m_f5.e[k] = m_vY.e[k] + ch*(m_vYP.e[k] + 3.0*m_f1.e[k]);
    rcx[3] = m_funcv(m_par, m_t + 3.0*m_h/8.0, m_f5, m_f2)<<3;

    ch = m_h/2197.0;
    for (k = 0; k < m_n; k++)
        m_f5.e[k] = m_vY.e[k] + ch*(1932.0*m_vYP.e[k] + (7296.0*m_f2.e[k] - 7200.0*m_f1.e[k]));
    rcx[1] = m_funcv(m_par, m_t + 12.0*m_h/13.0, m_f5, m_f3)<<1;

    ch = m_h/4104.0;
    for (k = 0; k < m_n; k++)
        m_f5.e[k] = m_vY.e[k] + ch*((8341.0*m_vYP.e[k] - 845.0*m_f3.e[k]) +
                                (29440.0*m_f2.e[k] - 32832.0*m_f1.e[k]));
    rcx[0] = m_funcv(m_par, m_t + m_h, m_f5, m_f4);

    ch = m_h/20520.0;
    for (k = 0; k < m_n; k++)
        m_f1.e[k] = m_vY.e[k] + ch*((-6080.0*m_vYP.e[k] + (9295.0*m_f3.e[k] - 5643.0*m_f4.e[k])) +
                                (41040.0*m_f1.e[k] - 28352.0*m_f2.e[k]));
    rcx[2] = m_funcv(m_par, m_t + m_h/2.0, m_f1, m_f5)<<2;

    ch = m_h/7618050.0;
    for (k = 0; k < m_n; k++)
        s.e[k] = m_vY.e[k] + ch*((902880.0*m_vYP.e[k] + (3855735.0*m_f3.e[k] - 1371249.0*m_f4.e[k])) +
                                (3953664.0*m_f2.e[k] + 277020.0*m_f5.e[k]));

    return rcx[0] + rcx[1] + rcx[2] + rcx[3] + rcx[4];
}

//-----------------------------------------------------------------------------
uint32_t CagmRKF45::fehl(double& s)
{
    double ch;
    uint32_t rc;

    ch = m_h/4.0;
    m_df5 = m_dY + ch*m_dYP;
    rc = rc | m_funcs(m_par, m_t + ch, m_df5, m_df1)<<4;

    ch = 3.0*m_h/32.0;
    m_df5 = m_dY + ch*(m_dYP + 3.0*m_df1);
    rc = rc | m_funcs(m_par, m_t + 3.0*m_h/8.0, m_df5, m_df2)<<3;

    ch = m_h/2197.0;
    m_df5 = m_dY + ch*(1932.0*m_dYP + (7296.0*m_df2 - 7200.0*m_df1));
    rc = rc | m_funcs(m_par, m_t + 12.0*m_h/13.0, m_df5, m_df3)<<1;

    ch = m_h/4104.0;
    m_df5 = m_dY + ch*((8341.0*m_dYP - 845.0*m_df3) + 
                       (29440.0*m_df2 - 32832.0*m_df1));
    rc = rc | m_funcs(m_par, m_t + m_h, m_df5, m_df4);

    ch = m_h/20520.0;
    m_df1 = m_dY + ch*((-6080.0*m_dYP + (9295.0*m_df3 - 5643.0*m_df4)) + 
                       (41040.0*m_df1 - 28352.0*m_df2));
    rc = rc | m_funcs(m_par, m_t + m_h/2.0, m_df1, m_df5)<<2;

    ch = m_h/7618050.0;
    s = m_dY + ch*((902880.0*m_dYP + (3855735.0*m_df3 - 1371249.0*m_df4)) + 
                   (3953664.0*m_df2 + 277020.0*m_df5));

    return rc;
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
class T_als
{
public:
    double als;
};

class T_gyro
{
public:
    double em;
    double ab;
};

uint32_t f1(void *p, const double /*t*/, const CagmRKF45Vect& v, CagmRKF45Vect& vp)
{
    double r = v[0]*v[0]+v[1]*v[1];
    r *= sqrt(r)/(((T_als *)p)->als);
    vp[0] = v[2];
    vp[1] = v[3];
    vp[2] = -v[0]/r;
    vp[3] = -v[1]/r;

    return 0;
}

uint32_t f2(void * /*p*/, const double t, const double /*d*/, double& dp)
{
    dp = cos(t);
    return 0;
}

uint32_t f3(void *p, const double /*t*/, const double d, double& dp)
{
    dp = (((T_gyro *)p)->em) - (((T_gyro *)p)->ab)*d;
    return 0;
}

int maintest(int /*argc*/, char* /*argv[]*/)
{
/*
    double t;
    int i;
    double s;
    Vect v(4);
    double ecc = 0.25;
    double a = 3.141592653589/4.0;
    T_als *pals = new T_als();
    pals->als = a*a;
    v[0] = 1.0 - ecc;
    v[1] = 0;
    v[2] = 0;
    v[3] = a*sqrt((1.0+ecc)/(1.0-ecc));
    t = 0;
    s = 0.5;

    RKF45 *rkf45 = new RKF45(0, 1e-9, f1, 4, pals);

    for (i = 0; i < 40; i++)
    {
        RKF45::Status status = rkf45->calculate(t, v, t+s, false);
        if (status != RKF45::Status::End)
            break;

        printf("%le %le\n", v[0], v[1]);
    }
*/
/*
    Vect v1(1);
    v1[0] = 0;
    s = 0.1;
    t = 0;

    RKF45 *rkf45s = new RKF45(0, 1e-9, f2, nullptr);

    for (i = 0; i < 50; i++)
    {
        RKF45::Status status = rkf45s->calculate(t, v1, t+s, false);
        if (status != RKF45::Status::End)
            break;

        printf("%le %le\n", v1[0], sin(t));
    }
*/
/*
    CagmRKF45Vect v2(1);
    v2[0] = 0;
    T_gyro *pgyro = new T_gyro();
    pgyro->em = 1;
    pgyro->ab = -3;
    s = 0.025;
    t = 0;

    CagmRKF45 *rkf45s3 = new CagmRKF45(0, 1e-9, f3, pgyro);

    for (i = 0; i < 60; i++)
    {
        CagmRKF45::Status status = rkf45s3->calculate(t, v2, t+s, false);
        if (status != CagmRKF45::Status::End)
            break;

        printf("%le %le\n", v2[0], pgyro->em/pgyro->ab*(1-exp(-t*pgyro->ab)));
    }
*/

    return 0;
}
