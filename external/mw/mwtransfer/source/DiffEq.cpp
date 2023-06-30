#include <math.h>
#include <float.h>
#include "ExtMath.h"
#include "DiffEq.h"

#define MAXSIZE 4

#define a2 0.2
#define a3 0.3
#define a4 0.6
#define a5 1.0
#define a6 0.875
#define b21 0.2
#define b31 (3.0/40.0)
#define b32 (9.0/40.0)
#define b41 0.3
#define b42 (-0.9)
#define b43 1.2
#define b51 (-11.0/54.0) 
#define b52 2.5
#define b53 (-70.0/27.0)
#define b54 (35.0/27.0)
#define b61 (1631.0/55296.0)
#define b62 (175.0/512.0)
#define b63 (575.0/13824.0)
#define b64 (44275.0/110592.0)
#define b65 (253.0/4096.0)
#define c1 (37.0/378.0)
#define c3 (250.0/621.0)
#define c4 (125.0/594.0)
#define c6 (512.0/1771.0)
#define dc5 (-277.00/14336.0)
#define dc1 (c1-2825.0/27648.0)
#define dc3 (c3-18575.0/48384.0)
#define dc4 (c4-13525.0/55296.0)
#define dc6 (c6-0.25)

void rkck(double *y, double *dydx, int n, double x, double h, double *yout, double *yerr, 
	      DEIntegrand *F, int *IntError)
{
 int i;
 double ak2[MAXSIZE], ak3[MAXSIZE], ak4[MAXSIZE], ak5[MAXSIZE], ak6[MAXSIZE], ytemp[MAXSIZE];

 for (i=0; i<n; i++) ytemp[i]=y[i]+b21*h*dydx[i];
 F->F(x+a2*h, ytemp, ak2); 
 for (i=0; i<n; i++) ytemp[i]=y[i]+h*(b31*dydx[i]+b32*ak2[i]);
 F->F(x+a3*h, ytemp, ak3); 
 for (i=0; i<n; i++) ytemp[i]=y[i]+h*(b41*dydx[i]+b42*ak2[i]+b43*ak3[i]);
 F->F(x+a4*h, ytemp, ak4); 
 for (i=0; i<n; i++) ytemp[i]=y[i]+h*(b51*dydx[i]+b52*ak2[i]+b53*ak3[i]+b54*ak4[i]);
 F->F(x+a5*h, ytemp, ak5); 
 for (i=0; i<n; i++) ytemp[i]=y[i]+h*(b61*dydx[i]+b62*ak2[i]+b63*ak3[i]+b64*ak4[i]+b65*ak5[i]);
 F->F(x+a6*h, ytemp, ak6); 
 for (i=0; i<n; i++) 
 {
  yout[i]=y[i]+h*(c1*dydx[i]+c3*ak3[i]+c4*ak4[i]+c6*ak6[i]);
  if (!finite(yout[i])) *IntError=1;
 }
 for (i=0; i<n; i++) 
 {
  yerr[i]=h*(dc1*dydx[i]+dc3*ak3[i]+dc4*ak4[i]+dc5*ak5[i]+dc6*ak6[i]);
  if (!finite(yerr[i])) *IntError=1;
 }
}

#define SAFETY 0.9
#define PGROW -0.2
#define PSHRNK -0.25
#define ERRCON 1.89e-4

void rkqs(double *y, double *dydx, int n, double *x, double htry, double *hdid, double *hnext, 
	      DEIntegrand *F, int *IntError)
{
 int i;
 double errmax, h, htemp, xnew;
 double yerr[MAXSIZE], ytemp[MAXSIZE];

 h=htry; 
 *IntError=0;
 int OK=0;

 do
 {
  rkck(y, dydx, n, *x, h, ytemp, yerr, F, IntError); 
  if (!(*IntError))
  {
   errmax=F->MaxRatio(yerr, ytemp); 
   errmax/=F->EPS(ytemp);
   if (errmax<=1.0) OK=1;
   else
   {
    htemp=SAFETY*h*pow(errmax, PSHRNK);
    h=(h>=0.0) ? max(htemp, 0.1*h) : min(htemp, 0.1*h);
    xnew=(*x)+h;
    if (xnew==(*x)) *IntError=1;
   }
  }
 } while (!OK && !(*IntError));

 if (errmax>ERRCON) *hnext=SAFETY*h*pow(errmax, PGROW);
 else *hnext=5.0*h;
 *x+=(*hdid=h);
 for (i=0; i<n; i++) y[i]=ytemp[i];
}