#pragma once

inline double sqr(double x)
{
 return x*x;
}

inline double min(double a, double b)
{
 return (a<b) ? a : b;
}

inline double max(double a, double b)
{
 return (a>b) ? a : b;
}

inline int min(int a, int b)
{
 return (a<b) ? a : b;
}

inline int max(int a, int b)
{
 return (a>b) ? a : b;
}

inline double sign(double x)
{
 return (x<0.0) ? -1.0 : 1.0;
}

#define dNaN (double(HUGE_VAL))

#ifndef LINUX
#define finite _finite
#else
#define finite isfinite
#endif

double IntTabulated(double *x, double *y, int N);
double InterpolateBilinear(double *arr, double i1, double i2, int N1, int N2, double missing);
double InterpolBilinear(double *arr, double *x1arr, double *x2arr, double x1, double x2, int N1, int N2);

double LogFactorial(int n);
