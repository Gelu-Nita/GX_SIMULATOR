#include <math.h>
#include <float.h>
#include <malloc.h>
#include "ExtMath.h"
#include "Plasma.h"
#include "Coulomb.h"
#include "Zeta.h"
#include "Messages.h"

void FindFF_single(double f, double theta, int sigma, double f_p, double f_B, double T0, double n_e, int ABcode,
                   double *j, double *k)
{
 double N, FZh;

 FindPlasmaDispersion(f, f_p, f_B, theta, sigma, &N, &FZh, 0, 0, 0, 0); 
 if (finite(N))
 {
  if (n_e>0)
  {
   double lnC=lnC1(T0, f);
   double zeta=Zeta_Solar(T0, f, ABcode);

   double jff=8*e*e*e*e*e*e*N/(3.0*sqrt(2.0*M_PI)*sqrt(me*me*me)*c*c*c)*
              sqr(n_e)*lnC/sqrt(kB*T0)*(1.0+zeta);
   double kff=8*e*e*e*e*e*e/(3.0*sqrt(2.0*M_PI)*N*c*sqr(f)*sqrt(me*me*me))*
              sqr(n_e)*lnC/(sqrt(kB*T0)*kB*T0)*(1.0+zeta);

   *j=jff*FZh;
   *k=kff*FZh;
  }
  else
  {
   *j=0.0;
   *k=0.0;
  }
 }
 else
 {
  *j=0.0;
  *k=1e100;
 }
}

void FindFF_DEM_XO(double f, double theta, double f_p, double f_B, double *T_arr, double *lnT_arr, double *DEM_arr, int NT, int ABcode,
                   double *jX, double *kX, double *jO, double *kO)
{
 double NX, FZhX, NO, FZhO;
 double aj, ak;

 FindPlasmaDispersion(f, f_p, f_B, theta, -1, &NX, &FZhX, 0, 0, 0, 0); 
 FindPlasmaDispersion(f, f_p, f_B, theta,  1, &NO, &FZhO, 0, 0, 0, 0); 

 if (finite(NX) || finite(NO))
 {
  double *I_j=(double*)malloc(sizeof(double)*NT);
  double *I_k=(double*)malloc(sizeof(double)*NT);

  for (int i=0; i<NT; i++)
  {
   I_j[i]=(DEM_arr[i]>0) ? 
          DEM_arr[i]*lnC1(T_arr[i], f)/sqrt(kB*T_arr[i])*(1.0+Zeta_Solar(T_arr[i], f, ABcode))*T_arr[i] : 0.0; //j, a=1/2
   I_k[i]=I_j[i]/(kB*T_arr[i]);                                                                                //k, a=3/2
  }

  aj=IntTabulated(lnT_arr, I_j, NT);
  ak=IntTabulated(lnT_arr, I_k, NT);

  free(I_j);
  free(I_k);
 }

 if (finite(NX))
 {
  *jX=8*e*e*e*e*e*e*NX/(3.0*sqrt(2.0*M_PI)*sqrt(me*me*me)*c*c*c)*aj*FZhX;
  *kX=8*e*e*e*e*e*e/(3.0*sqrt(2.0*M_PI)*NX*c*sqr(f)*sqrt(me*me*me))*ak*FZhX;
 }
 else
 {
  *jX=0.0;
  *kX=1e100;
 }

 if (finite(NO))
 {
  *jO=8*e*e*e*e*e*e*NO/(3.0*sqrt(2.0*M_PI)*sqrt(me*me*me)*c*c*c)*aj*FZhO;
  *kO=8*e*e*e*e*e*e/(3.0*sqrt(2.0*M_PI)*NO*c*sqr(f)*sqrt(me*me*me))*ak*FZhO;
 }
 else
 {
  *jO=0.0;
  *kO=1e100;
 }
}