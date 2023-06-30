#include <math.h>
#include <float.h>
#include <malloc.h>
#include "ExtMath.h"
#include "Plasma.h"

void FindGR_single(double f, double theta, int sigma, int s, double f_p, double f_B, double n_e, double T0, double LB, 
	               double *tau, double *I0)
{
 double N, L, T, st, ct;

 FindPlasmaDispersion(f, f_p, f_B, theta, sigma, &N, 0, &L, &T, &st, &ct); 
 if (finite(N))
 {
  if (f_p<=0 || T0<=0)
  {
   *tau=0.0;
   *I0=0.0;
  }
  else
  {
   double lnQ=log(kB*T0/me/c/c*sqr(s*N*st)/2)*(s-1);
   *tau=exp(lnQ-LogFactorial(s))*M_PI*e*e*n_e/(f*me*c)*s*s/N*LB*sqr(T*ct+L*st+1.0)/(1.0+sqr(T));
   *I0=kB*T0*sqr(f*N/c);
  }
 }
 else
 {
  *tau=1e100;
  *I0=0.0;
 }
}

void FindGR_DDM_XO(double f, double theta, int s, double f_p, double f_B, double *T_arr, double *lnT_arr, double *DDM_arr, int NT, double LB, 
	               double *tauX, double *I0X, double *tauO, double *I0O)
{
 double NX, LX, TX, stX, ctX, NO, LO, TO, stO, ctO;
 double I_tau, I_J;

 FindPlasmaDispersion(f, f_p, f_B, theta, -1, &NX, 0, &LX, &TX, &stX, &ctX); 
 FindPlasmaDispersion(f, f_p, f_B, theta,  1, &NO, 0, &LO, &TO, &stO, &ctO); 

 if (finite(NX) || finite(NO))
 {
  double *Int_tau=(double*)malloc(sizeof(double)*NT);
  double *Int_J=(double*)malloc(sizeof(double)*NT);

  for (int i=0; i<NT; i++)
  {
   Int_tau[i]=(DDM_arr[i]>0) ? 
	          DDM_arr[i]*exp(log(kB*T_arr[i]/me/c/c)*(s-1))*T_arr[i] : 0.0; //integrand for tau, degree of s-1
   Int_J[i]=Int_tau[i]*(kB*T_arr[i]/me/c/c);                                //integrand for j, degree of s
  }

  I_tau=IntTabulated(lnT_arr, Int_tau, NT);
  I_J=IntTabulated(lnT_arr, Int_J, NT);

  free(Int_tau);
  free(Int_J);
 
  if (finite(NX))
  {
   if (f_p<=0)
   {
    *tauX=0.0;
    *I0X=0.0;
   }
   else
   {
    double lnQ=log(sqr(s*NX*stX)/2)*(s-1);
    *tauX=exp(lnQ-LogFactorial(s))*I_tau*M_PI*e*e/(f*me*c)*s*s/NX*LB*sqr(TX*ctX+LX*stX+1.0)/(1.0+sqr(TX));
    *I0X=me*I_J/I_tau*sqr(f*NX);
   }
  }
  else
  {
   *tauX=1e100;
   *I0X=0.0;
  }

  if (finite(NO))
  {
   if (f_p<=0)
   {
    *tauO=0.0;
    *I0O=0.0;
   }
   else
   {
    double lnQ=log(sqr(s*NO*stO)/2)*(s-1);
    *tauO=exp(lnQ-LogFactorial(s))*I_tau*M_PI*e*e/(f*me*c)*s*s/NO*LB*sqr(TO*ctO+LO*stO+1.0)/(1.0+sqr(TO));
    *I0O=me*I_J/I_tau*sqr(f*NO);
   }
  }
  else
  {
   *tauO=1e100;
   *I0O=0.0;
  }
 }
}
