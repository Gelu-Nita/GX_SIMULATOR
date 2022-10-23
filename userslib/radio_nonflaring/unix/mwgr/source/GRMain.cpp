#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <stdlib.h>
#include "Messages.h"
#include "Astrophys.h"
#include "ExtMath.h"
#include "RFactorKappa.h"
#include "RFactorN.h"

#define FFF 1 //free-free only
#define THM 2 //relativistic thermal distribution
#define KAP 6 //kappa-distribution
#define NDS 50 //n-distribution

class EmWave
{
 public:
 int Valid, sigma;
 double nu, nu_p, nu_B, theta;
 double ct, st;
 double y, N, N_z, T, L, Delta, Zfactor;
 EmWave(double nu, double theta, int sigma, double nu_p, double nu_B, double cst_min);
};

EmWave :: EmWave(double _nu, double _theta, int _sigma, double _nu_p, double _nu_B, double cst_min)
{
 nu=_nu;
 theta=_theta;
 nu_p=_nu_p;
 nu_B=_nu_B;
 sigma=_sigma;

 double nu_c=(sigma==-1) ? nu_B/2+sqrt(sqr(nu_p)+sqr(nu_B)/4) : nu_p; //cutoff frequency
 
 if (nu<=nu_c) Valid=0;
 else
 {
  ct=cos(theta);
  st=sin(theta);
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

  y=nu/nu_B;

  double u=sqr(nu_B/nu);
  double v=sqr(nu_p/nu);
  Delta=sqrt(sqr(u*sqr(st))+4.0*u*sqr((1.0-v)*ct));
  N=sqrt(1.0-2.0*v*(1.0-v)/(2.0*(1.0-v)-u*sqr(st)+double(sigma)*Delta)); //refraction index
  N_z=N*ct; //longitudinal component of the refraction index
  T=2.0*sqrt(u)*(1.0-v)*ct/(u*sqr(st)-double(sigma)*Delta); //axial polarization coefficient;
  L=(v*sqrt(u)*st+T*u*v*st*ct)/(1.0-u-v+u*v*sqr(ct)); //longitudinal polarization coefficient
  Zfactor=u ? (2.0*sqr(1.0-v)+u*sqr(st)-double(sigma)*sqr(u*sqr(st))/Delta)/
	          sqr(2.0*(1.0-v)-u*sqr(st)+double(sigma)*Delta) : 0.5; //Zheleznyakov's correction to free-free

  Valid=finite(N);
 } 
}

void FindGRLevelMaxwell(double nu, double nu_B, double nu_p, double theta, 
	                    int s, double LB, double beta, double T0,
						double *tauX, double *tauO, double *I0X, double *I0O)
{
 EmWave wX=EmWave(nu, theta, -1, nu_p, nu_B, 1e-5);
 if (wX.Valid)
 {
  if ((nu_p==0.0) || (T0==0.0))
  {
   *tauX=0.0;
   *I0X=0.0;
  }
  else
  {
   double g=wX.T*wX.ct+wX.L*wX.st;
   double slna=log(sqr(wX.N*wX.st*beta*s)/2)*s;
   *tauX=M_PI*2.0*M_PI*nu/c*sqr(nu_p/nu)*fabs(LB)*sqr(1.0+g)/(1.0+sqr(wX.T))/(sqr(wX.N*wX.st*beta)*wX.N)*
	     ((slna<700) ? exp(slna)*InvFactorial(s) : exp(slna-LnGamma(1.0+s)));
   *I0X=sqr(wX.N*nu/c)*kB*fabs(T0);
  }
 }
 else
 {
  *tauX=1e100;
  *I0X=0.0;
 } 

 EmWave wO=EmWave(nu, theta, 1, nu_p, nu_B, 1e-5);
 if (wO.Valid)
 {
  if ((nu_p==0.0) || (T0==0.0))
  {
   *tauO=0.0;
   *I0O=0.0;
  }
  else
  {
   double g=wO.T*wO.ct+wO.L*wO.st;
   double slna=log(sqr(wO.N*wO.st*beta*s)/2)*s;
   *tauO=M_PI*2.0*M_PI*nu/c*sqr(nu_p/nu)*fabs(LB)*sqr(1.0+g)/(1.0+sqr(wO.T))/(sqr(wO.N*wO.st*beta)*wO.N)*
	     ((slna<700) ? exp(slna)*InvFactorial(s) : exp(slna-LnGamma(1.0+s)));
   *I0O=sqr(wO.N*nu/c)*kB*fabs(T0);
  }
 }
 else
 {
  *tauO=1e100;
  *I0O=0.0;
 } 
}

void FindGRLevelKappa(double nu, double nu_B, double nu_p, double theta, 
	                  int s, double LB, double beta, double T0, double kappa,
				      double *tauX, double *tauO, double *I0X, double *I0O)
{
 EmWave wX=EmWave(nu, theta, -1, nu_p, nu_B, 1e-5);
 if (wX.Valid)
 {
  if ((nu_p==0.0) || (T0==0.0))
  {
   *tauX=0.0;
   *I0X=0.0;
  }
  else
  {
   double slna=log((kappa-1.5)*sqr(beta*wX.N*wX.st*s)/2)*(s-1);
   *tauX=sqr(M_PI)/c*sqr(nu_p/nu*s)*nu*fabs(LB)/wX.N/(1.0+sqr(wX.T))*sqr(wX.T*wX.ct+wX.L*wX.st+1.0)*
	     ((slna<700) ? exp(slna)*InvFactorial(s) : exp(slna-LnGamma(1.0+s)))*
	  	 Gamma(kappa-s+0.5)/Gamma(kappa-0.5);
   *I0X=sqr(nu*wX.N/c)*kB*fabs(T0)*(kappa-1.5)/(kappa-s-0.5);
   *I0X*=RFactorKappaApprox(*tauX, kappa-s);
  }
 }
 else
 {
  *tauX=1e100;
  *I0X=0.0; 
 } 

 EmWave wO=EmWave(nu, theta, 1, nu_p, nu_B, 1e-5);
 if (wO.Valid)
 {
  if ((nu_p==0.0) || (T0==0.0))
  {
   *tauO=0.0;
   *I0O=0.0;
  }
  else
  {
   double slna=log((kappa-1.5)*sqr(beta*wO.N*wO.st*s)/2)*(s-1);
   *tauO=sqr(M_PI)/c*sqr(nu_p/nu*s)*nu*fabs(LB)/wO.N/(1.0+sqr(wO.T))*sqr(wO.T*wO.ct+wO.L*wO.st+1.0)*
	     ((slna<700) ? exp(slna)*InvFactorial(s) : exp(slna-LnGamma(1.0+s)))*
		 Gamma(kappa-s+0.5)/Gamma(kappa-0.5);
   *I0O=sqr(nu*wO.N/c)*kB*fabs(T0)*(kappa-1.5)/(kappa-s-0.5);
   *I0O*=RFactorKappaApprox(*tauO, kappa-s);
  }
 }
 else
 {
  *tauO=1e100;
  *I0O=0.0; 
 }
}

int FindGRLevelN(double nu, double nu_B, double nu_p, double theta, 
	             int s, double LB, double beta, double T0, int k,
			  double *tauX, double *tauO, double *I0X, double *I0O)
{
 int res=0;

 EmWave wX=EmWave(nu, theta, -1, nu_p, nu_B, 1e-5);
 if (wX.Valid)
 {
  if ((nu_p==0.0) || (T0==0.0))
  {
   *tauX=0.0;
   *I0X=0.0;
  }
  else
  {
   double slna=log(sqr(beta*wX.N*wX.st*s)/2)*s;
   *tauX=sqr(M_PI)*sqrt(M_PI)/c*sqr(nu_p/nu)/Gamma(1.5+k)*Gamma(0.5+s+k)/Gamma(0.5+s)*
	     nu*fabs(LB)/wX.N/(1.0+sqr(wX.T))/sqr(beta*wX.N*wX.st)*sqr(wX.T*wX.ct+wX.L*wX.st+1.0)*
		 ((slna<700) ? exp(slna)*InvFactorial(s) : exp(slna-LnGamma(1.0+s)));
   *I0X=sqr(nu*wX.N/c)*kB*fabs(T0)*(0.5+s+k)/(0.5+s);
   double R=RFactorNApprox(*tauX, s, k);
   if (!finite(R))
   {
    R=1.0;
    if (*tauX>1e-5) res=1;
   }
   *I0X*=R;
  }
 }
 else
 {
  *tauX=1e100;
  *I0X=0.0;
 } 

 EmWave wO=EmWave(nu, theta, 1, nu_p, nu_B, 1e-5);
 if (wO.Valid)
 {
  if ((nu_p==0.0) || (T0==0.0))
  {
   *tauO=0.0;
   *I0O=0.0;
  }
  else
  {
   double slna=log(sqr(beta*wO.N*wO.st*s)/2)*s;
   *tauO=sqr(M_PI)*sqrt(M_PI)/c*sqr(nu_p/nu)/Gamma(1.5+k)*Gamma(0.5+s+k)/Gamma(0.5+s)*
	     nu*fabs(LB)/wO.N/(1.0+sqr(wO.T))/sqr(beta*wO.N*wO.st)*sqr(wO.T*wO.ct+wO.L*wO.st+1.0)*
		 ((slna<700) ? exp(slna)*InvFactorial(s) : exp(slna-LnGamma(1.0+s)));
   *I0O=sqr(nu*wO.N/c)*kB*fabs(T0)*(0.5+s+k)/(0.5+s);
   double R=RFactorNApprox(*tauO, s, k);
   if (!finite(R))
   {
    R=1.0;
    if (*tauO>1e-5) res=1;
   }
   *I0O*=R;
  }
 }
 else
 {
  *tauO=1e100;
  *I0O=0.0;
 } 

 return res;
}

inline double Zmean(double T0)
{
 return (T0>3.5e4) ? 1.146 : 1.0;
}

double Coulomb(double T0, double nu)
{
 return (T0<2e5) ? 18.2+1.5*log(T0)-log(nu) : 24.573+log(T0/nu);
}

double Hminus(EmWave *w, double ne, double nH, double T0)
{
 double kT=sqrt(kB*T0/ieH);
 double xi=4.862*kT*(1.0-0.2096*kT+0.0170*kT*kT-0.00968*kT*kT*kT);
 return nH ? 2.15e-29*ne*nH*kB*T0*sqr(ieH/hPl/w->nu)*exp(-xi)/kT/w->N : 0;
}

void FF_Maxwell(EmWave *w, double ne, double nH, double T0, double *j, double *k)
{
 if (!w->Valid)
 {
  *j=0.0;
  *k=1e100;
 }
 else if (ne==0.0)
 {
  *k=0.0;
  *j=0.0;
 }
 else
 {
  double lnL=Coulomb(T0, w->nu);
  double kFF=9.786e-3*sqr(ne)*lnL/(w->N*sqr(w->nu)*T0*sqrt(T0));
  double kH=Hminus(w, ne, nH, T0);

  *k=(kFF+kH)*2.0*w->Zfactor*Zmean(T0); 
  *j=(*k)*sqr(w->N*w->nu/c)*kB*T0; 
 }
}

void FF_kappa(EmWave *w, double ne, double nH, double T0, double kappa, double *j, double *k)
{
 if (!w->Valid)
 {
  *j=0.0;
  *k=1e100;
 }
 else if (ne==0.0)
 {
  *j=0.0;
  *k=0.0;
 }
 else
 {
  double lnL=Coulomb(T0, w->nu);

  double Ak=Gamma(kappa+1.0)/Gamma(kappa-0.5)*pow(kappa-1.5, -1.5);
  double kFF=Ak*8.0*sqr(e*e*e)*sqr(ne)*lnL/
	         (3.0*sqrt(2.0*M_PI)*w->N*c*sqr(w->nu)*m*kB*T0*sqrt(m*kB*T0))*
			 (1.0-0.575*pow(6.0/kappa, 1.1)/lnL);
  double jFF=Ak*(kappa-1.5)/kappa*8.0*sqr(e*e*e)*w->N*sqr(ne)*lnL/
	         (3.0*sqrt(2.0*M_PI)*mc2*sqrt(mc2)*sqrt(kB*T0))*
			 (1.0-0.525*pow(4.0/kappa, 1.25)/lnL);
  kFF*=(2.0*w->Zfactor*Zmean(T0));
  jFF*=(2.0*w->Zfactor*Zmean(T0));

  double kH=Hminus(w, ne, nH, T0);
  kH*=(2.0*w->Zfactor*Zmean(T0));
  double jH=kH*sqr(w->N*w->nu/c)*kB*T0;

  *k=kFF+kH; 
  *j=jFF+jH;
 }
}

void FF_n(EmWave *w, double ne, double nH, double T0, double n, double *j, double *k)
{
 if (!w->Valid)
 {
  *j=0.0;
  *k=1e100;
 }
 else if (ne==0.0)
 {
  *j=0.0;
  *k=0.0;
 }
 else
 {
  double lnL=Coulomb(T0, w->nu);

  double jFF=4.0/3/sqrt(2.0)*sqr(e*e*e)/mc2/sqrt(mc2)*w->N*sqr(ne)*lnL/sqrt(kB*T0)*Gamma(n/2+0.5)/Gamma(n/2+1.0);
  jFF*=(2.0*w->Zfactor*Zmean(T0));
  // kFF=0

  double kH=Hminus(w, ne, nH, T0);
  kH*=(2.0*w->Zfactor*Zmean(T0));
  double jH=kH*sqr(w->N*w->nu/c)*kB*T0;

  *k=kH; 
  *j=jFF+jH;
 }
}

typedef struct
{
 double jX, jO, kX, kO;
} EmLocal;

void FindFFem(double nu, double theta, 
			  double nu_p, double nu_B, double ne, double nH, double T, int ED, double kn,
			  EmLocal *FF)
{
 FF->jX=FF->jO=FF->kX=FF->kO=0.0;
 if (T>0.0)
 {
  EmWave wX=EmWave(nu, theta, -1, nu_p, nu_B, 1e-5);
  EmWave wO=EmWave(nu, theta,  1, nu_p, nu_B, 1e-5);

  switch (ED)
  {
   case FFF:
   case THM: FF_Maxwell(&wX, ne, nH, T, &(FF->jX), &(FF->kX));
	         FF_Maxwell(&wO, ne, nH, T, &(FF->jO), &(FF->kO));
			 break;
   case KAP: FF_kappa(&wX, ne, nH, T, kn, &(FF->jX), &(FF->kX));
	         FF_kappa(&wO, ne, nH, T, kn, &(FF->jO), &(FF->kO));
			 break;
   case NDS: FF_n(&wX, ne, nH, T, kn, &(FF->jX), &(FF->kX));
	         FF_n(&wO, ne, nH, T, kn, &(FF->jO), &(FF->kO));
			 break;
   default: FF_Maxwell(&wX, ne, nH, T, &(FF->jX), &(FF->kX));
	        FF_Maxwell(&wO, ne, nH, T, &(FF->jO), &(FF->kO));
  }
 }
}

typedef struct
{
 double B, Bx, By, Bz;
 double n0, T0;
 double ne, nH, nu_p, nu_B, beta;
 double theta;
 double zc, z1, z2;
 double kn;
 int ED, Smax;
} GRNode;

typedef struct
{
 int Type; //0 - gyrolayer, 1 - transverse layer
 double z;
 int s;
 double B, Bx, By, Bz, LB, dt_dz;
} GRLevel;

int GRTransfer(int NSteps, double **Parms, double *nu, 
	           double *Rw, double *Lw, double *Rs, double *Ls, double *Re, double *Le)
{
 int res=0;
 int NErr=0;

 int Nnu=(int)Parms[0][18];
 double dnu=pow(10.0, Parms[0][16]);
 nu[0]=Parms[0][15]; 
 for (int i=1; i<Nnu; i++) nu[i]=nu[i-1]*dnu;

 double sang=Parms[0][0]/(sqr(AU)*sfu);
 int Interpol=0; //(int)Parms[0][5];

 GRNode *Nodes=(GRNode*)malloc(sizeof(GRNode)*NSteps);

 for (int i=0; i<NSteps; i++)
 {
  Nodes[i].B=Parms[i][13];
  Nodes[i].Bx=Parms[i][13]*sin(Parms[i][14]*M_PI/180)*cos(Parms[i][29]*M_PI/180);
  Nodes[i].By=Parms[i][13]*sin(Parms[i][14]*M_PI/180)*sin(Parms[i][29]*M_PI/180);
  Nodes[i].Bz=Parms[i][13]*cos(Parms[i][14]*M_PI/180);
  Nodes[i].n0=Parms[i][11];
  Nodes[i].T0=Parms[i][2];
  Nodes[i].theta=Parms[i][14]*M_PI/180;
  Nodes[i].zc=(i>0) ? Nodes[i-1].zc+(Parms[i-1][1]+Parms[i][1])/2 : 0;
  Nodes[i].z1=Nodes[i].zc-Parms[i][1]/2;
  Nodes[i].z2=Nodes[i].zc+Parms[i][1]/2;
  Nodes[i].kn=Parms[i][4];
  Nodes[i].ED=(int)Parms[i][17];
  Nodes[i].Smax=(int)Parms[i][28];

  if (Parms[i][26]+Parms[i][27]>0)
  {
   Nodes[i].ne=Parms[i][27]; //electron concentration (assumed to equal the proton concentration)
   Nodes[i].nH=Parms[i][26]; //neutral hydrogen concentration
  }
  else
  {
   double alpha=Saha(Nodes[i].n0, fabs(Nodes[i].T0)); //ionization coefficient
   Nodes[i].ne=Nodes[i].n0*alpha; //electron concentration
   Nodes[i].nH=Nodes[i].n0*(1.0-alpha); //neutral hydrogen concentration
  }
  Nodes[i].nu_p=e*sqrt(Nodes[i].ne/m/M_PI); 
  Nodes[i].nu_B=e*Nodes[i].B/m/c/(2.0*M_PI); 
  Nodes[i].beta=sqrt(kB*fabs(Nodes[i].T0)/m)/c;
 }

 int NMaxLevels=10;
 GRLevel *Levels=(GRLevel*)malloc(sizeof(GRLevel)*NMaxLevels);
 int *lsrt=(int*)malloc(sizeof(int)*NMaxLevels);

 for (int i=0; i<Nnu; i++)
 {
  double B_nu=nu[i]/e*m*c*2*M_PI;

  for (int j=0; j<NSteps; j++)
  {
   EmLocal FF;
   FindFFem(nu[i], Nodes[j].theta, 
	        Nodes[j].nu_p, Nodes[j].nu_B, Nodes[j].ne, Nodes[j].nH, Nodes[j].T0, Nodes[j].ED, Nodes[j].kn,
		    &FF);

   int Smax;
   switch (Nodes[j].ED)
   {
	case FFF: Smax=0;
		      break;
	case THM: 
	case NDS: Smax=Nodes[j].Smax;
		      break;
	case KAP: Smax=min(Nodes[j].Smax, int(Nodes[j].kn-0.6));
		      break;
	default: Smax=0;
   }

   int NLevels=0;

   if (j<(NSteps-1))
   {
	if (Nodes[j].Bz*Nodes[j+1].Bz<0.0)
	{
	 double z=Nodes[j].zc-Nodes[j].Bz*(Nodes[j+1].zc-Nodes[j].zc)/(Nodes[j+1].Bz-Nodes[j].Bz);
	 if (z<=Nodes[j].z2)
	 {
      if (NLevels>=NMaxLevels)
	  {
	   NMaxLevels=NLevels+1;
	   Levels=(GRLevel*)realloc(Levels, sizeof(GRLevel)*NMaxLevels);
	   lsrt=(int*)realloc(lsrt, sizeof(int)*NMaxLevels);
	  }
	  Levels[NLevels].Type=1;
	  Levels[NLevels].z=z;
	  Levels[NLevels].B=Nodes[j].B+(Nodes[j+1].B-Nodes[j].B)*(z-Nodes[j].zc)/(Nodes[j+1].zc-Nodes[j].zc);
	  Levels[NLevels].Bx=Levels[NLevels].By=1;
	  Levels[NLevels].Bz=0;
	  Levels[NLevels].dt_dz=(Nodes[j+1].theta-Nodes[j].theta)/(Nodes[j+1].zc-Nodes[j].zc);
	  NLevels++;
	 }
	}

	if (Nodes[j].B>0.0 && Nodes[j+1].B>0.0 && Nodes[j].B!=Nodes[j+1].B)
	{
	 int smin=(int)ceil(B_nu/max(Nodes[j].B, Nodes[j+1].B));
	 int smax=(int)floor(B_nu/min(Nodes[j].B, Nodes[j+1].B));
	 smin=max(smin, 2);
	 smax=min(smax, Smax);
	 double dB_dz=(Nodes[j+1].B-Nodes[j].B)/(Nodes[j+1].zc-Nodes[j].zc);

	 for (int s=smin; s<=smax; s++)
	 {
	  double z=Nodes[j].zc+(B_nu/s-Nodes[j].B)*(Nodes[j+1].zc-Nodes[j].zc)/(Nodes[j+1].B-Nodes[j].B);
	  if (z<=Nodes[j].z2)
	  {
	   if (NLevels>=NMaxLevels)
	   {
	    NMaxLevels=NLevels+1;
	    Levels=(GRLevel*)realloc(Levels, sizeof(GRLevel)*NMaxLevels);
	    lsrt=(int*)realloc(lsrt, sizeof(int)*NMaxLevels);
	   }
	   Levels[NLevels].Type=0;
	   Levels[NLevels].z=z;
	   Levels[NLevels].s=s;
	   Levels[NLevels].B=B_nu/s;
	   Levels[NLevels].Bx=Nodes[j].Bx+(Nodes[j+1].Bx-Nodes[j].Bx)*(z-Nodes[j].zc)/(Nodes[j+1].zc-Nodes[j].zc);
	   Levels[NLevels].By=Nodes[j].By+(Nodes[j+1].By-Nodes[j].By)*(z-Nodes[j].zc)/(Nodes[j+1].zc-Nodes[j].zc);
	   Levels[NLevels].Bz=Nodes[j].Bz+(Nodes[j+1].Bz-Nodes[j].Bz)*(z-Nodes[j].zc)/(Nodes[j+1].zc-Nodes[j].zc);
	   Levels[NLevels].LB=Levels[NLevels].B/dB_dz;
	   NLevels++;
	  }
	 }
	}
   }

   if (j>0)
   {
	if (Nodes[j-1].Bz*Nodes[j].Bz<0)
	{
	 double z=Nodes[j-1].zc-Nodes[j-1].Bz*(Nodes[j].zc-Nodes[j-1].zc)/(Nodes[j].Bz-Nodes[j-1].Bz);
	 if (z>Nodes[j].z1)
	 {
      if (NLevels>=NMaxLevels)
	  {
	   NMaxLevels=NLevels+1;
	   Levels=(GRLevel*)realloc(Levels, sizeof(GRLevel)*NMaxLevels);
	   lsrt=(int*)realloc(lsrt, sizeof(int)*NMaxLevels);
	  }
	  Levels[NLevels].Type=1;
	  Levels[NLevels].z=z;
	  Levels[NLevels].B=Nodes[j-1].B+(Nodes[j].B-Nodes[j-1].B)*(z-Nodes[j-1].zc)/(Nodes[j].zc-Nodes[j-1].zc);
	  Levels[NLevels].Bx=Levels[NLevels].By=1;
	  Levels[NLevels].Bz=0;
	  Levels[NLevels].dt_dz=(Nodes[j].theta-Nodes[j-1].theta)/(Nodes[j].zc-Nodes[j-1].zc);
	  NLevels++;
	 }
	}

	if (Nodes[j-1].B>0.0 && Nodes[j].B>0.0 && Nodes[j-1].B!=Nodes[j].B)
	{
	 int smin=(int)ceil(B_nu/max(Nodes[j-1].B, Nodes[j].B));
	 int smax=(int)floor(B_nu/min(Nodes[j-1].B, Nodes[j].B));
	 smin=max(smin, 2);
	 smax=min(smax, Smax);
	 double dB_dz=(Nodes[j].B-Nodes[j-1].B)/(Nodes[j].zc-Nodes[j-1].zc);

	 for (int s=smin; s<=smax; s++)
	 {
	  double z=Nodes[j-1].zc+(B_nu/s-Nodes[j-1].B)*(Nodes[j].zc-Nodes[j-1].zc)/(Nodes[j].B-Nodes[j-1].B);
	  if (z>Nodes[j].z1)
	  {
	   if (NLevels>=NMaxLevels)
	   {
	    NMaxLevels=NLevels+1;
	    Levels=(GRLevel*)realloc(Levels, sizeof(GRLevel)*NMaxLevels);
	    lsrt=(int*)realloc(lsrt, sizeof(int)*NMaxLevels);
	   }
	   Levels[NLevels].Type=0;
	   Levels[NLevels].z=z;
	   Levels[NLevels].s=s;
	   Levels[NLevels].B=B_nu/s;
	   Levels[NLevels].Bx=Nodes[j-1].Bx+(Nodes[j].Bx-Nodes[j-1].Bx)*(z-Nodes[j-1].zc)/(Nodes[j].zc-Nodes[j-1].zc);
       Levels[NLevels].By=Nodes[j-1].By+(Nodes[j].By-Nodes[j-1].By)*(z-Nodes[j-1].zc)/(Nodes[j].zc-Nodes[j-1].zc);
	   Levels[NLevels].Bz=Nodes[j-1].Bz+(Nodes[j].Bz-Nodes[j-1].Bz)*(z-Nodes[j-1].zc)/(Nodes[j].zc-Nodes[j-1].zc);
	   Levels[NLevels].LB=Levels[NLevels].B/dB_dz;
	   NLevels++;
	  }
	 }
	}
   }

   for (int l=0; l<NLevels; l++) lsrt[l]=l;
   for (int p=0; p<(NLevels-1); p++) for (int q=p+1; q<NLevels; q++) if (Levels[lsrt[p]].z>Levels[lsrt[q]].z)
   {
	int u=lsrt[p];
	lsrt[p]=lsrt[q];
	lsrt[q]=u;
   }

   for (int l=0; l<=NLevels; l++)
   {
	double tauX, tauO, dIX, dIO, eX, eO, I0X, I0O;

	double dz=((l==NLevels) ? Nodes[j].z2 : Levels[lsrt[l]].z)-((l==0) ? Nodes[j].z1 : Levels[lsrt[l-1]].z);

	if (dz>0.0)
	{
	 tauX=FF.kX*dz;
	 eX=exp(-tauX);
	 dIX=(FF.kX==0.0) ? FF.jX*dz : FF.jX/FF.kX*((1.0-eX) ? 1.0-eX : tauX);

	 tauO=FF.kO*dz;
	 eO=exp(-tauO);
	 dIO=(FF.kO==0.0) ? FF.jO*dz : FF.jO/FF.kO*((1.0-eO) ? 1.0-eO : tauO);

     if (Nodes[j].Bz<0)
     {
      Lw[i]=dIX+Lw[i]*eX;
      Ls[i]=dIX+Ls[i]*eX;
      Le[i]=dIX+Le[i]*eX;
      Rw[i]=dIO+Rw[i]*eO;
      Rs[i]=dIO+Rs[i]*eO;
      Re[i]=dIO+Re[i]*eO;
     }
     else
     {
      Lw[i]=dIO+Lw[i]*eO;
      Ls[i]=dIO+Ls[i]*eO;
      Le[i]=dIO+Le[i]*eO; 
      Rw[i]=dIX+Rw[i]*eX;
      Rs[i]=dIX+Rs[i]*eX;
      Re[i]=dIX+Re[i]*eX;
     }
	}

	if (l<NLevels)
	{
	 double B, Bx, By, Bz, ne, nu_B, nu_p, theta, T0, beta, kn;

	 B=Levels[lsrt[l]].B;   //B  is always interpolated
	 Bx=Levels[lsrt[l]].Bx; //Bx is always interpolated
	 By=Levels[lsrt[l]].By; //By is always interpolated
	 Bz=Levels[lsrt[l]].Bz; //Bz is always interpolated
	 nu_B=e*B/m/c/(2.0*M_PI); //nu_B is always interpolated
	 theta=acos(Bz/sqrt(sqr(Bx)+sqr(By)+sqr(Bz))); //theta is always interpolated

	 if (!Interpol)
	 {
	  ne=Nodes[j].ne;
	  nu_p=Nodes[j].nu_p;
	  T0=Nodes[j].T0;
	  beta=Nodes[j].beta;
	  kn=Nodes[j].kn;
	 }
	 else
	 {
	  double z=Levels[lsrt[l]].z;
	  double D, n0;
	  if (z<Nodes[j].zc)
	  {
	   D=(z-Nodes[j-1].zc)/(Nodes[j].zc-Nodes[j-1].zc);
	   n0=Nodes[j-1].n0+D*(Nodes[j].n0-Nodes[j-1].n0);
	   T0=Nodes[j-1].T0+D*(Nodes[j].T0-Nodes[j-1].T0);
	   kn=Nodes[j-1].kn+D*(Nodes[j].kn-Nodes[j-1].kn);
	  }
	  else
	  {
	   D=(z-Nodes[j].zc)/(Nodes[j+1].zc-Nodes[j].zc);
	   n0=Nodes[j].n0+D*(Nodes[j+1].n0-Nodes[j].n0);
	   T0=Nodes[j].T0+D*(Nodes[j+1].T0-Nodes[j].T0);
	   kn=Nodes[j].kn+D*(Nodes[j+1].kn-Nodes[j].kn);
	  }
	  ne=n0*Saha(n0, fabs(T0));
	  nu_p=e*sqrt(ne/m/M_PI); 
	  beta=sqrt(kB*fabs(T0)/m)/c;
	 }

	 if (Levels[lsrt[l]].Type) //Transverse layer
	 {
	  double logQT=1.4540156e17*ne*sqr(B)*B/sqr(sqr(nu[i]))/fabs(Levels[lsrt[l]].dt_dz);
	  double QT=exp(-logQT);

      double a=Lw[i];
	  Lw[i]=Rw[i];
	  Rw[i]=a;

	  a=Le[i]*QT+Re[i]*(1.0-QT);
      Re[i]=Re[i]*QT+Le[i]*(1.0-QT);
      Le[i]=a;
	 }
	 else //Gyrolayer
	 {
	  switch (Nodes[j].ED)
	  {
	   case THM: FindGRLevelMaxwell(nu[i], nu_B, nu_p, theta, 
	                                Levels[lsrt[l]].s, Levels[lsrt[l]].LB, beta, T0,
						            &tauX, &tauO, &I0X, &I0O);
		         break;
	   case KAP: FindGRLevelKappa(nu[i], nu_B, nu_p, theta, 
	                              Levels[lsrt[l]].s, Levels[lsrt[l]].LB, beta, T0, kn,
				                  &tauX, &tauO, &I0X, &I0O);
		         break;
	   case NDS: NErr|=FindGRLevelN(nu[i], nu_B, nu_p, theta, 
	                                Levels[lsrt[l]].s, Levels[lsrt[l]].LB, beta, T0, int(kn/2),
				                    &tauX, &tauO, &I0X, &I0O);
	  }

	  eX=exp(-tauX);
	  dIX=I0X*((1.0-eX) ? 1.0-eX : tauX);
      eO=exp(-tauO);
	  dIO=I0O*((1.0-eO) ? 1.0-eO : tauO);
                  
	  if (Bz<0)
      {
       Lw[i]=dIX+Lw[i]*eX;
       Ls[i]=dIX+Ls[i]*eX;
       Le[i]=dIX+Le[i]*eX; 
       Rw[i]=dIO+Rw[i]*eO;
       Rs[i]=dIO+Rs[i]*eO;
       Re[i]=dIO+Re[i]*eO;
      }
      else
      {
       Lw[i]=dIO+Lw[i]*eO;
       Ls[i]=dIO+Ls[i]*eO;
       Le[i]=dIO+Le[i]*eO; 
       Rw[i]=dIX+Rw[i]*eX;
       Rs[i]=dIX+Rs[i]*eX;
       Re[i]=dIX+Re[i]*eX;
	  }
	 }
	}
   }
  }

  Le[i]*=sang;  
  Re[i]*=sang;
  Lw[i]*=sang;
  Rw[i]*=sang;
  Ls[i]*=sang;
  Rs[i]*=sang;
 }

 free(Levels);
 free(lsrt);
 free(Nodes);

 if (NErr)
 {
  IDLmsg("GRTransfer warning: R-factor for n-distribution was not calculated (set to 1).");
  res=1;
 }

 return res;
}