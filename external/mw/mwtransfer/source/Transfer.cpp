#include <stdlib.h>
#include <math.h>
#include "Transfer.h"
#include "Messages.h"
#include "ExtMath.h"
#include "DiffEq.h"
#include "Astrophys.h"

 Ray :: Ray(int N, double *z_in, double *n0, double *B, double *theta, double *psi, 
	        double *jX, double *jO, double *kX, double *kO, int clos, int spline_on)
 {
  int i0=0;
  int Nval=N;

  if (!clos)
  {
   int i1, i2;
   for (i1=0; i1<N; i1++) if (z_in[i1]>0) break;
   for (i2=N-1; i2>=0; i2--) if (z_in[i2]>0) break;
   i0=i1;
   Nval=i2-i1+1;
  }

  RayM(Nval, z_in+i0, n0+i0, B+i0, theta+i0, psi+i0, 
	   jX+i0, jO+i0, kX+i0, kO+i0, clos, spline_on);
 }

 void Ray :: RayM(int N, double *z_in, double *n0, double *B, double *theta, double *psi, 
	              double *jX, double *jO, double *kX, double *kO, int clos, int spline_on)
 {
  z_arr=dz_arr=n0_arr=Bx_arr=By_arr=Bz_arr=jX_arr=jO_arr=kX_arr=kO_arr=0;
  n0_2arr=Bx_2arr=By_2arr=Bz_2arr=jX_2arr=jO_2arr=kX_2arr=kO_2arr=0;
 
  Npoints=N;
  Closed=clos;
  spl_on=spline_on;
  OK=1;

  int Nmin=spl_on ? 3 : (Closed ? 2 : 1);
  if (Npoints<Nmin)
  {
   IDLmsg("MWTransfer error: %d or more nodes are required.", Nmin);
   OK=0;
  }

  if (OK)
  {
   int badz=0;
   if (Closed)
   {
	for (int i=1; i<Npoints; i++) if (z_in[i]<=z_in[i-1]) badz=1;
   }
   else	
   {
	for (int i=0; i<Npoints; i++) if (z_in[i]<=0.0) badz=1;
   }
   if (badz)
   {
	IDLmsg("MWTransfer error: %s.", Closed ? "coordinates along the line-of-sight must be increasing" :
		                                     "all intervals must be positive");
    OK=0;
   }
  }

  if (OK)
  {
   z_arr=(double*)malloc(sizeof(double)*Npoints);
   dz_arr=(double*)malloc(sizeof(double)*Npoints);
   n0_arr=(double*)malloc(sizeof(double)*Npoints);
   Bx_arr=(double*)malloc(sizeof(double)*Npoints);
   By_arr=(double*)malloc(sizeof(double)*Npoints);
   Bz_arr=(double*)malloc(sizeof(double)*Npoints);
   jX_arr=(double*)malloc(sizeof(double)*Npoints);
   jO_arr=(double*)malloc(sizeof(double)*Npoints);
   kX_arr=(double*)malloc(sizeof(double)*Npoints);
   kO_arr=(double*)malloc(sizeof(double)*Npoints);

   if (Closed)
   {
	for (int i=0; i<Npoints; i++) z_arr[i]=z_in[i];
	dz_arr[0]=(z_arr[1]-z_arr[0])/2;
	for (int i=1; i<(Npoints-1); i++) dz_arr[i]=(z_arr[i+1]-z_arr[i-1])/2;
	dz_arr[Npoints-1]=(z_arr[Npoints-1]-z_arr[Npoints-2])/2;
	zmin=z_arr[0];
	zmax=z_arr[Npoints-1];
   }
   else
   {
	for (int i=0; i<Npoints; i++) dz_arr[i]=z_in[i];
	z_arr[0]=0.0;
	for (int i=1; i<Npoints; i++) z_arr[i]=z_arr[i-1]+(dz_arr[i-1]+dz_arr[i])/2;
	zmin=z_arr[0]-dz_arr[0]/2;
	zmax=z_arr[Npoints-1]+dz_arr[Npoints-1]/2;
   }

   for (int i=0; i<Npoints; i++)
   {
	n0_arr[i]=n0[i];
	Bx_arr[i]=B[i]*sin(theta[i])*cos(psi[i]);
	By_arr[i]=B[i]*sin(theta[i])*sin(psi[i]);
	Bz_arr[i]=B[i]*cos(theta[i]);
	jX_arr[i]=jX[i];
	jO_arr[i]=jO[i];
	kX_arr[i]=kX[i];
	kO_arr[i]=kO[i];
   }

   if (spl_on)
   {
	n0_2arr=(double*)malloc(sizeof(double)*Npoints);
	Bx_2arr=(double*)malloc(sizeof(double)*Npoints);
	By_2arr=(double*)malloc(sizeof(double)*Npoints);
	Bz_2arr=(double*)malloc(sizeof(double)*Npoints);
	jX_2arr=(double*)malloc(sizeof(double)*Npoints);
	jO_2arr=(double*)malloc(sizeof(double)*Npoints);
	kX_2arr=(double*)malloc(sizeof(double)*Npoints);
	kO_2arr=(double*)malloc(sizeof(double)*Npoints);

	spline_init(z_arr, n0_arr, Npoints, dNaN, dNaN, n0_2arr);
	spline_init(z_arr, Bx_arr, Npoints, dNaN, dNaN, Bx_2arr);
	spline_init(z_arr, By_arr, Npoints, dNaN, dNaN, By_2arr);
	spline_init(z_arr, Bz_arr, Npoints, dNaN, dNaN, Bz_2arr);
	spline_init(z_arr, jX_arr, Npoints, dNaN, dNaN, jX_2arr);
	spline_init(z_arr, jO_arr, Npoints, dNaN, dNaN, jO_2arr);
	spline_init(z_arr, kX_arr, Npoints, dNaN, dNaN, kX_2arr);
	spline_init(z_arr, kO_arr, Npoints, dNaN, dNaN, kO_2arr);
   }
  }
 }

 Ray :: ~Ray()
 {
  if (z_arr) free(z_arr);
  if (dz_arr) free(dz_arr);
  if (n0_arr) free(n0_arr);
  if (Bx_arr) free(Bx_arr);
  if (By_arr) free(By_arr);
  if (Bz_arr) free(Bz_arr);
  if (jX_arr) free(jX_arr);
  if (jO_arr) free(jO_arr);
  if (kX_arr) free(kX_arr);
  if (kO_arr) free(kO_arr);
  if (n0_2arr) free(n0_2arr);
  if (Bx_2arr) free(Bx_2arr);
  if (By_2arr) free(By_2arr);
  if (Bz_2arr) free(Bz_2arr);
  if (jX_2arr) free(jX_2arr);
  if (jO_2arr) free(jO_2arr);
  if (kX_2arr) free(kX_2arr);
  if (kO_2arr) free(kO_2arr);
 }

 void Ray :: GetLocalParms(double z, double *n0, double *Bx, double *By, double *Bz, 
	                                 double *jX, double *jO, double *kX, double *kO)
 {
  if (Npoints==1)
  {
   *n0=n0_arr[0];
   *Bx=Bx_arr[0];
   *By=By_arr[0];
   *Bz=Bz_arr[0];
   *jX=jX_arr[0];
   *jO=jO_arr[0];
   *kX=kX_arr[0];
   *kO=kO_arr[0];
  }
  else
  {
   int i1, i2;
   if (z<=z_arr[0])
   {
    i1=0;
    i2=1;
   }
   else if (z>=z_arr[Npoints-1])
   {
    i1=Npoints-2;
    i2=Npoints-1;
   }
   else
   {
    i1=0; 
    i2=Npoints-1;
    while (i2-i1>1) 
    {
     int k=(i1+i2)>>1;
     if (z_arr[k]>z) i2=k;
     else i1=k;
    } 
   }
   double h=z_arr[i2]-z_arr[i1];
   double a=(z_arr[i2]-z)/h;
   double b=(z-z_arr[i1])/h;

   if (spl_on)
   {
	double A=(a*a*a-a)*(h*h)/6.0;
	double B=(b*b*b-b)*(h*h)/6.0;

	*n0=a*n0_arr[i1]+b*n0_arr[i2]+A*n0_2arr[i1]+B*n0_2arr[i2];
	*Bx=a*Bx_arr[i1]+b*Bx_arr[i2]+A*Bx_2arr[i1]+B*Bx_2arr[i2];
	*By=a*By_arr[i1]+b*By_arr[i2]+A*By_2arr[i1]+B*By_2arr[i2];
	*Bz=a*Bz_arr[i1]+b*Bz_arr[i2]+A*Bz_2arr[i1]+B*Bz_2arr[i2];
	*jX=a*jX_arr[i1]+b*jX_arr[i2]+A*jX_2arr[i1]+B*jX_2arr[i2];
	*jO=a*jO_arr[i1]+b*jO_arr[i2]+A*jO_2arr[i1]+B*jO_2arr[i2];
	*kX=a*kX_arr[i1]+b*kX_arr[i2]+A*kX_2arr[i1]+B*kX_2arr[i2];
	*kO=a*kO_arr[i1]+b*kO_arr[i2]+A*kO_2arr[i1]+B*kO_2arr[i2];
   }
   else
   {
	*n0=a*n0_arr[i1]+b*n0_arr[i2];
	*Bx=a*Bx_arr[i1]+b*Bx_arr[i2];
	*By=a*By_arr[i1]+b*By_arr[i2];
	*Bz=a*Bz_arr[i1]+b*Bz_arr[i2];
	*jX=a*jX_arr[i1]+b*jX_arr[i2];
	*jO=a*jO_arr[i1]+b*jO_arr[i2];
	*kX=a*kX_arr[i1]+b*kX_arr[i2];
	*kO=a*kO_arr[i1]+b*kO_arr[i2];
   }
  }
 }

 class StokesIntegrand : public DEIntegrand
 {
  public:
  double omega, Ithres, EPSh, EPSl;
  Ray *r;
  void F(double z, double *S, double *dS_dz);
  double MaxRatio(double *S_err, double *S);
  double EPS(double *S);
 };

 void StokesIntegrand :: F(double z, double *S, double *dS_dz)
 {
  double n0, Bx, By, Bz, jX, jO, kX, kO;
  r->GetLocalParms(z, &n0, &Bx, &By, &Bz, &jX, &jO, &kX, &kO);

  double Bn2=sqr(Bx)+sqr(By);
  double Bn=sqrt(Bn2);
  double B=sqrt(Bn2+sqr(Bz));

  double ct=B ? Bz/B : 1.0;
  double st=B ? Bn/B : 0.0;
  double c2psi=Bn2 ? (sqr(Bx)-sqr(By))/Bn2 : 1.0;
  double s2psi=Bn2 ? 2.0*Bx*By/Bn2 : 0.0;

  double omega_B=2.0*M_PI*2.80e6*B;
  double omega_p=2.0*M_PI*8.98e3*sqrt(n0);
  double u=sqr(omega_B/omega);
  double v=sqr(omega_p/omega);
  double sD=sqrt(sqr(u*sqr(st))+4.0*u*sqr((1.0-v)*ct));
  double NX=sqrt(1.0-2.0*v*(1.0-v)/(2.0*(1.0-v)-u*sqr(st)-sD));
  double NO=sqrt(1.0-2.0*v*(1.0-v)/(2.0*(1.0-v)-u*sqr(st)+sD));
  double TX=2.0*sqrt(u)*(1.0-v)*ct/(u*sqr(st)+sD);
  double dN=-v*sD/(NX+NO)/(1.0-u-v+u*v*sqr(ct));
  double TX2m1=-2.0*u*sqr(st)/(u*sqr(st)+sD);
  double TX2p1=sqr(TX)+1.0;

  double muQ=TX2m1/TX2p1*c2psi;
  double muU=TX2m1/TX2p1*s2psi;
  double muV=-2.0*TX/TX2p1;
  double dk=omega/c*dN;
  double j=jX+jO;
  double dj=jX-jO;
  double kappa=(kX+kO)/2; 
  double dkappa=(kX-kO)/2; 

  double Sj[4]; //spontaneous emission
  Sj[0]=j;
  Sj[1]=muQ*dj;
  Sj[2]=muU*dj;
  Sj[3]=muV*dj;

  double R[4][4]; //R-K
  R[1][2]=-muV*dk;
  R[2][1]=-R[1][2];
  R[1][3]=muU*dk;
  R[3][1]=-R[1][3];
  R[2][3]=-muQ*dk;
  R[3][2]=-R[2][3];

  R[0][0]=R[1][1]=R[2][2]=R[3][3]=-kappa;
  R[0][1]=R[1][0]=-muQ*dkappa;
  R[0][2]=R[2][0]=-muU*dkappa;
  R[0][3]=R[3][0]=-muV*dkappa;

  for (int A=0; A<4; A++)
  {
   dS_dz[A]=Sj[A];
   for (int B=0; B<4; B++) dS_dz[A]+=R[A][B]*S[B];
  } 
 }

 double StokesIntegrand :: MaxRatio(double *S_err, double *S)
 {
  double r=0.0;
  if (S[0]) for (int i=0; i<4; i++) r=max(r, fabs(S_err[i]/S[0])); 
  return r;
 }

 double StokesIntegrand :: EPS(double *S)
 {
  return (S[0]>Ithres) ? EPSh : EPSl;
 }

 int Ray :: RadiationTransferExact(double nu, double *S_in, double Imax, double MinStep, 
	                               double *S_out)
 {
  int res=0;

  StokesIntegrand si;
  si.r=this;
  si.omega=nu*2.0*M_PI;
  si.EPSh=1e-10;
  si.EPSl=1e-3;
  si.Ithres=Imax*1e-6;

  double MaxStep=1e100;
  for (int i=0; i<Npoints; i++) MaxStep=min(MaxStep, dz_arr[i]);
  MaxStep*=0.1; 

  double S[4];
  for (int A=0; A<4; A++) S[A]=S_in[A];
  double dS_dz[4];

  double z=zmin;
  double h=MaxStep;
  double hdid, hnext;

  int Steps=0;
  int IntError=0;

  while (z<zmax && !IntError)
  {
   si.F(z, S, dS_dz);
   rkqs(S, dS_dz, 4, &z, h, &hdid, &hnext, &si, &IntError);
   h=min(hnext, MaxStep);
   if (h<MinStep) IntError=1;
   if ((z+h)>zmax) h=zmax-z;
  }

  if (!IntError) for (int A=0; A<4; A++) S_out[A]=S[A];
  else
  {
   IDLmsg("MWTransfer error: integration error in the exact polarization transfer equation.");
   res=1;
  }

  return res;
 }

 class ZSolveFunction : public IntegrableFunction
 {
  public:
  int N;
  double *z_arr, *Bz_arr, *Bz_2arr;
  double F(double z);
 };

 double ZSolveFunction :: F(double z)
 {
  double res;
  spline_interp(z_arr, Bz_arr, Bz_2arr, N, z, &res, 0);
  return res;
 }

 void Ray :: RadiationTransferRL(double nu, double R_in, double L_in, double *RL_out)
 {
  double Lw, Rw, Ls, Rs, Le, Re;
  Rw=Rs=Re=R_in;
  Lw=Ls=Le=L_in;
  tauXtotal=tauOtotal=0.0;

  for (int i=0; i<Npoints; i++)
  {
   double tau=-kO_arr[i]*dz_arr[i]; //optical depth = k*L, O-mode
   tauOtotal+=tau;
   double eO=(tau<700) ? exp(tau) : 0.0; 
   double dIO=(kO_arr[i]==0.0 || tau>700) ? 
	          0.0 : jO_arr[i]/kO_arr[i]*((1.0-eO) ? 1.0-eO : -tau);
   tau=-kX_arr[i]*dz_arr[i]; //optical depth = k*L, X-mode
   tauXtotal+=tau;
   double eX=(tau<700) ? exp(tau) : 0.0;
   double dIX=(kX_arr[i]==0.0 || tau>700) ? 
	          0.0 : jX_arr[i]/kX_arr[i]*((1.0-eX) ? 1.0-eX : -tau);

   if (i>0) if (Bz_arr[i]*Bz_arr[i-1]<0.0)
   {
    double a=Lw;
    Lw=Rw;
    Rw=a;

	double B_avg, n0_avg, d_a_d_z;

	if (spl_on)
	{
	 ZSolveFunction zsf;
	 zsf.N=Npoints;
	 zsf.z_arr=z_arr;
	 zsf.Bz_arr=Bz_arr;
	 zsf.Bz_2arr=Bz_2arr;

	 double z0=BrentRoot(&zsf, z_arr[i-1], z_arr[i], 1.0);

	 spline_interp(z_arr, n0_arr, n0_2arr, Npoints, z0, &n0_avg, 0);

	 double Bx0, By0, Bz0, Bx01, By01, Bz01, Bn0;
	 spline_interp(z_arr, Bx_arr, Bx_2arr, Npoints, z0, &Bx0, &Bx01);
	 spline_interp(z_arr, By_arr, By_2arr, Npoints, z0, &By0, &By01);
	 spline_interp(z_arr, Bz_arr, Bz_2arr, Npoints, z0, &Bz0, &Bz01);
	 B_avg=sqrt(sqr(Bx0)+sqr(By0)+sqr(Bz0));
	 Bn0=sqrt(sqr(Bx0)+sqr(By0));
	 d_a_d_z=(Bz0*(Bx0*Bx01+By0*By01)-sqr(Bn0)*Bz01)/(sqr(B_avg)*Bn0);
	 d_a_d_z=fabs(d_a_d_z); 
	}
	else
	{
	 double B1=sqrt(sqr(Bx_arr[i-1])+sqr(By_arr[i-1])+sqr(Bz_arr[i-1]));
	 double B2=sqrt(sqr(Bx_arr[i])+sqr(By_arr[i])+sqr(Bz_arr[i]));
	 double theta1=B1 ? acos(Bz_arr[i-1]/B1) : 0.0;
	 double theta2=B2 ? acos(Bz_arr[i]/B2) : 0.0;

	 double dz_avg=(dz_arr[i]+dz_arr[i-1])/2;
     d_a_d_z=fabs((theta2-theta1)/dz_avg);
     B_avg=(B1+B2)/2;
     n0_avg=(n0_arr[i]+n0_arr[i-1])/2;
	}

    double QT=1.4540156e17*n0_avg*sqr(B_avg)*B_avg/sqr(sqr(nu))/d_a_d_z;
    QT=exp(-QT);
    a=Le*QT+Re*(1.0-QT);
    Re=Re*QT+Le*(1.0-QT);
    Le=a;
   }

   if (Bz_arr[i]<0)
   {
    Lw=dIX+Lw*eX;
    Ls=dIX+Ls*eX;
    Le=dIX+Le*eX;
    Rw=dIO+Rw*eO;
    Rs=dIO+Rs*eO;
    Re=dIO+Re*eO;
   }
   else
   {
    Lw=dIO+Lw*eO;
    Ls=dIO+Ls*eO;
    Le=dIO+Le*eO;
    Rw=dIX+Rw*eX;
    Rs=dIX+Rs*eX;
    Re=dIX+Re*eX;
   }
  }

 RL_out[0]=Lw;
 RL_out[1]=Rw;
 RL_out[2]=Ls;
 RL_out[3]=Rs;
 RL_out[4]=Le;
 RL_out[5]=Re;
}