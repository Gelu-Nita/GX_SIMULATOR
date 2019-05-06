#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

inline double sqr(double x)
{
 return x*x;
}

inline double max(double a, double b)
{
 return (a>b) ? a : b;
}

inline double min(double a, double b)
{
 return (a<b) ? a : b;
}

inline double sign(double x)
{
 return (x<0.0) ? -1.0 : 1.0;
}

#define finite isfinite
#define dNaN (double(HUGE_VAL))

double Gamma(double z);
double LnGamma(double z);
double InvFactorial(int n);

void polint(const double *xa, const double *ya, int n, double x, double *y, double *dy);