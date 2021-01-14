#include <math.h>
#include <float.h>
#include "ExtMath.h"
#include "Plasma.h"

void FindNeutralsEffect(double f, double theta, int sigma, double f_p, double f_B, double T0, double n_e, double n_H, double n_He,
                        double *j, double *k)
{
 double N, FZh;

 FindPlasmaDispersion(f, f_p, f_B, theta, sigma, &N, &FZh, 0, 0, 0, 0); 
 if (finite(N))
 {
  double jH, kH;
  jH=kH=0;

  if (n_e>0 && n_H>0 && T0>2500 && T0<50000)
  {
   double kT=sqrt(kB*T0/ieH);
   double xi=4.862*kT*(1.0-0.2096*kT+0.0170*kT*kT-0.00968*kT*kT*kT);
   kH=1.2737207e-11*n_e*n_H*sqrt(T0)/(sqr(f)*N)*exp(-xi);

   jH=kH*sqr(N*f/c)*kB*T0;
  }

  double jHe, kHe;
  jHe=kHe=0;

  if (n_e>0 && n_He>0 && T0>2500 && T0<25000)
  {
   double kT=sqrt(kB*T0/ieH);
   kHe=5.9375453e-13*n_e*n_He*sqrt(T0)/(sqr(f)*N)*(1.868+7.415*kT-22.56*kT*kT+15.59*kT*kT*kT);

   jHe=kHe*sqr(N*f/c)*kB*T0;
  }

  *j=(jH+jHe)*FZh;
  *k=(kH+kHe)*FZh;
 }
 else
 {
  *j=0.0;
  *k=1e100;
 }
}