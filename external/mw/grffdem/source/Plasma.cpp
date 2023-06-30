#include <math.h>
#include "ExtMath.h"
#include "Plasma.h"
#include "Messages.h"

#define cst_min 1e-5

void FindPlasmaDispersion(double f, double f_p, double f_B, double theta, int sigma,
	                      double *N, double *FZh, double *L, double *T, double *st_out, double *ct_out)
{
 double f_c=(sigma==-1) ? f_B/2+sqrt(sqr(f_p)+sqr(f_B)/4) : f_p; //cutoff frequency

 if (f<=f_c) *N=dNaN;
 else
 {
  double ct=cos(theta);
  double st=sin(theta);
  if (fabs(ct)<cst_min)
  {
   ct=cst_min*sign(ct);
   st=sqrt(1.0-sqr(ct))*sign(st);
  }
  if (fabs(st)<cst_min)
  {
   st=cst_min*sign(st);
   ct=sqrt(1.0-sqr(st))*sign(ct);
  }

  double u=sqr(f_B/f);
  double v=sqr(f_p/f);

  double Delta=sqrt(sqr(u*sqr(st))+4.0*u*sqr((1.0-v)*ct));
  *N=sqrt(1.0-2.0*v*(1.0-v)/(2.0*(1.0-v)-u*sqr(st)+double(sigma)*Delta)); //refraction index

  if (FZh) 
   *FZh=u ? 2.0*(u*sqr(st)+2.0*sqr(1.0-v)-double(sigma)*sqr(u*sqr(st))/Delta)/sqr(2.0*(1.0-v)-u*sqr(st)+double(sigma)*Delta) : 1.0;

  if (L!=0 || T!=0)
  {
   double Tloc=2.0*sqrt(u)*(1.0-v)*ct/(u*sqr(st)-double(sigma)*Delta); //axial polarization coefficient;

   if (T) *T=Tloc;
   if (L) *L=(v*sqrt(u)*st+Tloc*u*v*st*ct)/(1.0-u-v+u*v*sqr(ct)); //longitudinal polarization coefficient
  }

  if (st_out) *st_out=st;
  if (ct_out) *ct_out=ct;
 }
}

double SahaH(double n0, double T0)
{
 double x=0.0;

 if (T0>0.0 && n0>0.0)
 {
  double xi=pow(2.0*M_PI*me*kB*T0/sqr(hPl), 1.5)/n0*exp(-ieH/kB/T0); 
  x=xi ? 2.0/(sqrt(1.0+4.0/xi)+1.0) : 0.0;
 } 

 return x;
}

void SahaHe(double n_p, double T0, double *a12, double *a2)
{
 *a12=0;
 *a2=0;

 if (T0>0.0 && n_p>0.0)
 {
  double A=4.0*pow(2.0*M_PI*me*kB*T0/sqr(hPl), 1.5)/n_p;

  double xi12=A*exp(-ieHe12/kB/T0);
  *a12=xi12/(1.0+xi12); //helium I+II ionization fraction

  double xi2=A*exp(-ieHe2/kB/T0);
  *a2=xi2/(1.0+xi2); //helium II ionization fraction
 }
}

void FindIonizationsSolar(double n0, double T0, double *n_e, double *n_H, double *n_He)
{
 double n_Htotal=n0*0.922;
 double n_Hetotal=n0*0.078;

 double a=SahaH(n_Htotal, T0);
 double n_p=n_Htotal*a;
 *n_H=n_Htotal*(1.0-a);

 double a12, a2;
 SahaHe(n_p, T0, &a12, &a2);
 *n_He=n_Hetotal*(1.0-a12);
 *n_e=n_p+n_Hetotal*(a12+a2)+n_Htotal*1e-3;
}
