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

double Erf(double);
void FindBesselJ(double, int, double*, double*);
void FindBesselJ_WH(double, double, double*, double*);
double ExpBesselK(int, double);
double Gamma(double z);

class IntegrableFunction
{
 public:
 virtual double F(double x)=0;
};

#define JMAX 20
double qromb(IntegrableFunction *F, double a, double b, double EPS, int *err);
double trapzd(IntegrableFunction *F, double a, double b, int N);
double qrombLog(IntegrableFunction *F, double a, double b, double EPS, int *err);
double trapzdLog(IntegrableFunction *F, double a, double b, int N);

#define MAXIT 20
#define BrentMAXIT 100
double SecantRoot(IntegrableFunction *F, double x1, double x2, double EPS);
double BrentRoot(IntegrableFunction *F, double x1, double x2, double EPS);

void spline_init(double *x, double *y, int n, double yp1, double ypn, double *y2);
void spline_interp(double *xa, double *ya, double *y2a, int n, double x, double *y, double *y1);