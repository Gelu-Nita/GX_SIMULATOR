#include <math.h>
#include <float.h>
#include <stdlib.h>
#include <malloc.h>
#include "ExtMath.h"

double LnGamma(double xx)
{
 double x, y, tmp, ser;
 static double cof[6]={ 76.18009172947146,
	                   -86.50532032941677,
                        24.01409824083091,
					   -1.231739572450155,
                        0.1208650973866179e-2,
					-0.5395239384953e-5};
 int j;
 y=x=xx;
 tmp=x+5.5;
 tmp-=(x+0.5)*log(tmp);
 ser=1.000000000190015;
 for (j=0; j<=5; j++) ser+=cof[j]/++y;
 return -tmp+log(2.5066282746310005*ser/x);
}

double Gamma(double z)
{
 return exp(LnGamma(z));
}

const double FactorialArray[10]={1.0, 
	                             1.0/2,
								 1.0/2/3,
								 1.0/2/3/4,
								 1.0/2/3/4/5,
								 1.0/2/3/4/5/6,
								 1.0/2/3/4/5/6/7,
								 1.0/2/3/4/5/6/7/8,
								 1.0/2/3/4/5/6/7/8/9,
								 1.0/2/3/4/5/6/7/8/9/10};

double InvFactorial(int n) //1/n!
{
 if (n<=10) return FactorialArray[n-1];
 else
 {
  double res=FactorialArray[9];
  for (int i=11; i<=n; i++) res/=i;
  return res;
 }
}

void polint(const double *xa, const double *ya, int n, double x, double *y, double *dy)
{
 int i, m, ns=1;
 double den, dif, dift, ho, hp, w;
 double c[10], d[10]; //fixed-size arrays; n must be <=10
 dif=fabs(x-xa[1]);
 for (i=1; i<=n; i++)
 {
  if ((dift=fabs(x-xa[i]))<dif)
  {
   ns=i;
   dif=dift;
  }
  c[i]=ya[i];
  d[i]=ya[i];
 }
 *y=ya[ns--];
 for (m=1; m<n; m++)
 {
  for (i=1; i<=n-m; i++)
  {
   ho=xa[i]-x;
   hp=xa[i+m]-x;
   w=c[i+1]-d[i];
   den=ho-hp; 
   den=w/den;
   d[i]=hp*den;
   c[i]=ho*den;
  }
  *y+=(*dy=(2*ns<(n-m) ? c[ns+1] : d[ns--]));
 }
}