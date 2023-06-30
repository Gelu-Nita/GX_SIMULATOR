#pragma once

#include <math.h>
#include <limits.h>
#include <float.h>

#ifdef AS_REAL_AS_FLOAT
#define REALTYPE_MAX FLT_MAX 
#define v_pi_c 3.141592653589793238462643f
#define v_ln10 2.302585092994046f

static REALTYPE_A factorials[] = {1.0f, 1.0f, 2.0f, 6.0f, 24.0f, 120.0f, 720.0f, 5040.0f, 40320.0f, 362880.0f, 3628800.0f, 39916800.0f, 479001600.0f, 6227020800.0f, 87178291200.0f,
        1307674368000.0f, 20922789888000.0f, 355687428096000.0f, 6402373705728000.0f, 1.21645100408832e+17f, 2.43290200817664e+18f,
        5.109094217170944e+19f, 1.124000727777608e+21f, 2.585201673888498e+22f, 6.204484017332394e+23f, 1.551121004333099e+25f,
        4.032914611266057e+26f, 1.088886945041835e+28f, 3.048883446117138e+29f, 8.841761993739701e+30f, 2.65252859812191e+32f, 
        8.222838654177922e+33f,  2.631308369336935e+35f};


static REALTYPE_A CnkS_123[] = {0.0f, 1.3333333e-01f, 7.6190476e-02f, 5.0793651e-02f, 3.6940837e-02f, 2.8416028e-02f, 2.2732823e-02f, 1.8721148e-02f,
        1.5765177e-02f, 1.3513009e-02f, 1.1750443e-02f, 1.0340390e-02f, 9.1914575e-03f, 8.2406170e-03f, 7.4431380e-03f, 6.7664891e-03f,
        6.1865043e-03f, 5.6848958e-03f, 5.2475961e-03f, 4.8636257e-03f, 4.5243030e-03f, 4.2226828e-03f, 3.9531498e-03f, 3.7111203e-03f,
        3.4928191e-03f, 3.2951123e-03f, 3.1153789e-03f, 2.9514115e-03f, 2.8013398e-03f, 2.6635693e-03f, 2.5367324e-03f, 2.4196535e-03f,
        2.3113116e-03f};

static REALTYPE_A CnkS_121[] = {0.0, 6.6666667e-01f, 5.3333333e-01f, 4.5714286e-01f, 4.0634921e-01f, 3.6940837e-01f, 3.4099234e-01f, 3.1825952e-01f,
        2.9953837e-01f, 2.8377319e-01f, 2.7026018e-01f, 2.5850974e-01f, 2.4816935e-01f, 2.3897789e-01f, 2.3073728e-01f, 2.2329414e-01f,
        2.1652765e-01f, 2.1034115e-01f, 2.0465625e-01f, 1.9940865e-01f, 1.9454503e-01f, 1.9002072e-01f, 1.8579804e-01f, 1.8184489e-01f,
        1.7813377e-01f, 1.7464095e-01f, 1.7134584e-01f, 1.6823046e-01f, 1.6527905e-01f, 1.6247771e-01f, 1.5981414e-01f, 1.5727741e-01f,
        1.5485776e-01f};
#define v_180 180.0f
#define v_1 1.0f
#define v_2 2.0f
#define v_3 3.0f
#define v_4 4.0f
#define v_onehalf 1.5f
#define v_025 0.25f
#define v_05 0.5f
#define v_15to8 (15.0f/8.0f)
#define v_safe 1.00001f

#define v_220 220.0f
#define v_10000 10000.0f
#else
#define REALTYPE_MAX DBL_MAX
#define v_pi_c 3.141592653589793238462643
#define v_ln10 2.302585092994046

static REALTYPE_A factorials[] = {1.0, 1.0, 2.0, 6.0, 24.0, 120.0, 720.0, 5040.0, 40320.0, 362880.0, 3628800.0, 39916800.0, 479001600.0, 6227020800.0, 87178291200.0,
        1307674368000.0, 20922789888000.0, 355687428096000.0, 6402373705728000.0, 1.21645100408832e+17, 2.43290200817664e+18,
        5.109094217170944e+19, 1.124000727777608e+21, 2.585201673888498e+22, 6.204484017332394e+23, 1.551121004333099e+25,
        4.032914611266057e+26, 1.088886945041835e+28, 3.048883446117138e+29, 8.841761993739701e+30, 2.65252859812191e+32, 
        8.222838654177922e+33, 2.631308369336935e+35};


static REALTYPE_A CnkS_123[] = {0.0, 1.3333333e-01, 7.6190476e-02, 5.0793651e-02, 3.6940837e-02, 2.8416028e-02, 2.2732823e-02, 1.8721148e-02,
        1.5765177e-02, 1.3513009e-02, 1.1750443e-02, 1.0340390e-02, 9.1914575e-03, 8.2406170e-03, 7.4431380e-03, 6.7664891e-03,
        6.1865043e-03, 5.6848958e-03, 5.2475961e-03, 4.8636257e-03, 4.5243030e-03, 4.2226828e-03, 3.9531498e-03, 3.7111203e-03,
        3.4928191e-03, 3.2951123e-03, 3.1153789e-03, 2.9514115e-03, 2.8013398e-03, 2.6635693e-03, 2.5367324e-03, 2.4196535e-03,
        2.3113116e-03};

static REALTYPE_A CnkS_121[] = {0.0, 6.6666667e-01, 5.3333333e-01, 4.5714286e-01, 4.0634921e-01, 3.6940837e-01, 3.4099234e-01, 3.1825952e-01,
        2.9953837e-01, 2.8377319e-01, 2.7026018e-01, 2.5850974e-01, 2.4816935e-01, 2.3897789e-01, 2.3073728e-01, 2.2329414e-01,
        2.1652765e-01, 2.1034115e-01, 2.0465625e-01, 1.9940865e-01, 1.9454503e-01, 1.9002072e-01, 1.8579804e-01, 1.8184489e-01,
        1.7813377e-01, 1.7464095e-01, 1.7134584e-01, 1.6823046e-01, 1.6527905e-01, 1.6247771e-01, 1.5981414e-01, 1.5727741e-01,
        1.5485776e-01};
#define v_180 180.0
#define v_1 1.0
#define v_2 2.0
#define v_3 3.0
#define v_4 4.0
#define v_onehalf 1.5
#define v_025 0.25
#define v_05 0.5
#define v_15to8 (15.0/8.0)
#define v_safe 1.00001
#define v_gammaonehalf 1.772453850905516027298167
#define v_sqrtpi v_gammaonehalf

#define v_220 220.0
#define v_10000 10000.0

#endif

#define MMATH_DIM_X 0
#define MMATH_DIM_Y 1
#define MMATH_DIM_Z 2

inline REALTYPE_A Rad2Deg(REALTYPE_A x)
{
    return x*v_180/v_pi_c;
}

inline REALTYPE_A Deg2Rad(REALTYPE_A x)
{
    return x*v_pi_c/v_180;
}

// v_erfc = 1-v_erf
//   v_erf(x)  = 2/sqrt(pi) * Integral(exp(-t^2) dt) [0...x]
//   v_erfc(x) = 2/sqrt(pi) * Integral(exp(-t^2) dt) [x..+inf]
//   NB: v_erf(0)  = 0 v_erf(+inf)  = 1
//   NB: v_erfc(0) = 1 v_erfc(+inf) = 0
inline REALTYPE_A v_erfc(REALTYPE_A _x)
{
// Abramowitz, Stegun, pp.120-122 (russ. transl.)
// abs.error <= 3e-7

    REALTYPE_A x = fabs(_x);
#ifdef AS_REAL_AS_FLOAT
	REALTYPE_A v = (((((0.0000430638f*x + 0.0002765672f)*x + 0.0001520143f)*x + 0.0092705272f)*x
				+ 0.0422820123f)*x + 0.0705230784f)*x + 1.0f;
#else
	REALTYPE_A v = (((((0.0000430638*x + 0.0002765672)*x + 0.0001520143)*x + 0.0092705272)*x
				+ 0.0422820123)*x + 0.0705230784)*x + 1.0;
#endif

	v *= v;
	v *= v;
	v *= v;
	v *= v;

	REALTYPE_A res = v_1/v;

    return (_x < 0 ? 2 - res : res);
}

inline REALTYPE_A v_gamma(REALTYPE_A _x) // 1 <= x <= 33
{
    _x -= 1;
    int entier = (int)floor(_x);
    return factorials[entier] * (((((-0.1010678*_x+0.4245549)*_x-0.6998588)*_x+0.9512363)*_x-0.5748646)*_x+1);
}

inline double v_getmax(REALTYPE_A x1, REALTYPE_A x2)
{
    return x1 > x2 ? x1 : x2;
}

inline double v_getmin(REALTYPE_A x1, REALTYPE_A x2)
{
    return x1 < x2 ? x1 : x2;
}

inline double v_dmax(int n, REALTYPE_A *x, int *_im = nullptr)
{
    double m = -DBL_MAX;
    int im = -1;
    for (int i = 0; i < n; i++)
    {
        if (m < x[i])
        {
            im = i;
            m = x[i];
        }
    }

    if (_im)
        *_im = im;

    return m;
}

inline double v_dmin(int n, REALTYPE_A *x, int *_im = nullptr)
{
    double m = -DBL_MAX;
    int im = -1;
    for (int i = 0; i < n; i++)
    {
        if (m > x[i])
        {
            im = i;
            m = x[i];
        }
    }

    if (_im)
        *_im = im;

    return m;
}

inline bool v_interpInArray(int n, REALTYPE_A *x, REALTYPE_A *Y, REALTYPE_A x0, REALTYPE_A *Y0)
{   // suppose x is sorted ascending
    if (x0 < x[0])
        return false;
    if (x0 > x[n-1])
        return false;

    int left = 0, right = n-1, m;
    while (right - left > 1)
    {
        m = (right+left)/2;
        if (x0 == x[m])
        {
            *Y0 = Y[m];
            break;
        }
        if (x0 < x[m])
            right = m;
        else
            left = m;
    }

    *Y0 = (Y[right]-Y[left])/(x[right]-x[left]) * (x0-x[left]) + Y[left];

    return true;
}

inline bool v_interpInArrayEqdist(int n, REALTYPE_A *x, REALTYPE_A x0, REALTYPE_A *Y0)
{   // suppose x is sorted ascending
    if (x0 < x[0])
        return false;
    if (x0 > x[n - 1])
        return false;

    int left = 0, right = n - 1, m;
    while (right - left > 1)
    {
        m = (right+left) / 2;
        if (x0 == x[m])
        {
            *Y0 = m;
            return true;
        }
        if (x0 < x[m])
            right = m;
        else
            left = m;
    }

    *Y0 = (right - left) / (x[right] - x[left]) * (x0 - x[left]) + left;
    return true;
}
