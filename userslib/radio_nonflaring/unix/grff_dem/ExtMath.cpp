#include <math.h>
#include "Messages.h"

double IntTabulated(double *x, double *y, int N)
{
 double s=0;
 for (int i=1; i<N; i++) s+=0.5*(y[i-1]+y[i])*(x[i]-x[i-1]);
 return s;
}

double InterpolateBilinear(double *arr, double i1, double i2, int N1, int N2, double missing)
/* Interpolation on an equidistant grid (like the IDL interpolate function),
   i1 and i2 - fractional indices of the required point. */
{
 if (i1<0 || i1>(N1-1) || i2<0 || i2>(N2-1)) return missing;

 int j=int(i1);
 int k=int(i2);
 double t=i1-j;
 double u=i2-k;

 double y1=arr[N2*j+k];
 double y2=arr[N2*(j+1)+k];
 double y3=arr[N2*(j+1)+k+1];
 double y4=arr[N2*j+k+1];

 return (1.0-t)*(1.0-u)*y1+t*(1.0-u)*y2+t*u*y3+(1.0-t)*u*y4;
}

double InterpolBilinear(double *arr, double *x1arr, double *x2arr, double x1, double x2, int N1, int N2)
/* Interpolation on an arbitrary grid (like the IDL interpol function). 
   Performs extrapolation, if the point is outside the range. */   
{
 int j, j1, k, k1, l;

 if (x1<x1arr[0])
 {
  j=0;
  j1=1;
 }
 else if (x1>x1arr[N1-1])
 {
  j=N1-2;
  j1=N1-1;
 }
 else
 {
  j=0;
  j1=N1-1;
  while ((j1-j)>1)
  {
   l=(j1+j) >> 1;
   if (x1arr[l]>x1) j1=l;
   else j=l;
  }
 }
 double dx1=x1arr[j1]-x1arr[j];
 double t=(x1-x1arr[j])/dx1;

 if (x2<x2arr[0])
 {
  k=0;
  k1=1;
 }
 else if (x2>x2arr[N2-1])
 {
  k=N2-2;
  k1=N2-1;
 }
 else
 {
  k=0;
  k1=N2-1;
  while ((k1-k)>1)
  {
   l=(k1+k) >> 1;
   if (x2arr[l]>x2) k1=l;
   else k=l;
  }
 }
 double dx2=x2arr[k1]-x2arr[k];
 double u=(x2-x2arr[k])/dx2;
                                                           
 double y1=arr[N2*j+k];
 double y2=arr[N2*j1+k];
 double y3=arr[N2*j1+k1];
 double y4=arr[N2*j+k1];

 return (1.0-t)*(1.0-u)*y1+t*(1.0-u)*y2+t*u*y3+(1.0-t)*u*y4;
}

double gammln(double xx) 
{ 
 int j;
 double x, tmp, y, ser;
 static const double cof[14]={57.1562356658629235, -59.5979603554754912, 14.1360979747417471, -0.491913816097620199, 
                              0.339946499848118887e-4, 0.465236289270485756e-4, -0.983744753048795646e-4, 0.158088703224912494e-3, 
                              -0.210264441724104883e-3, 0.217439618115212643e-3, -0.164318106536763890e-3, 0.844182239838527433e-4, 
                              -0.261908384015814087e-4, 0.368991826595316234e-5};
 y=x=xx;
 tmp=x+5.24218750000000000;
 tmp=(x+0.5)*log(tmp)-tmp;
 ser=0.999999999999997092;
 for (j=0; j<14; j++) ser+=cof[j]/++y;
 return tmp+log(2.5066282746310005*ser/x);
}

double LogFactorial(int n)
{
 #define Nmax 21

 static double a[Nmax];
 static int init=1;

 if (init)
 {
  double f=1.0;

  for (int i=0; i<Nmax; i++)
  {
   f*=((i>0) ? i : 1); 
   a[i]=log(f);
  }

  init=0;
 }

 return (n<Nmax) ? a[n] : gammln(n+1.0);
}