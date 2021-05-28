#include <stdlib.h>
#include <math.h>
#include <float.h>
#include "ExtMath.h"
#include "Transfer.h"
#include "Messages.h"
#include "Astrophys.h"
#include "Std_DF.h"
#include "GS.h"

void Find_jk_GS(DF **mdf, int N_nu, double *nu, int mode,
	            double theta, double nu_p, double nu_B, double cst_min,
				double nu_cr, double nu_cr_WH, int Npoints, int Q_on, int m_on,
				double *j, double *k)
{
 for (int i=0; i<N_nu; i++)
 {
  EmWave w=EmWave(nu[i], theta, mode, nu_p, nu_B, cst_min);
  if (nu[i]>nu_cr) GS_jk_approx_MultiDF(&w, mdf, Npoints, Q_on, j+i, k+i);
  else GS_jk_MultiDF(&w, mdf, nu[i]<nu_cr_WH, j+i, k+i);
 }

 if (m_on)
 {
  if (nu_cr!=nu_cr_WH && nu[0]<nu_cr && nu[N_nu-1]>nu_cr)
  {
   int i0;
   for (i0=1; i0<N_nu; i0++) if (nu[i0]>nu_cr) break;
 
   if (j[i0]!=0.0 && k[i0]!=0.0)
   {
    EmWave w=EmWave(nu[i0], theta, mode, nu_p, nu_B, cst_min);
    double j0, k0;
	GS_jk_MultiDF(&w, mdf, nu[i0]<nu_cr_WH, &j0, &k0);
	double mj=j0/j[i0];
	double mk=k0/k[i0];
	for (int i=i0; i<N_nu; i++)
	{
	 j[i]*=mj;
	 k[i]*=mk; 
	}
   }
  }

  if (nu_cr_WH<=nu_cr && nu[0]<nu_cr_WH && nu[N_nu-1]>nu_cr_WH)
  {
   int i0;
   for (i0=1; i0<N_nu; i0++) if (nu[i0]>nu_cr_WH) break;
 
   if (j[i0]!=0.0 && k[i0]!=0.0)
   {
    EmWave w=EmWave(nu[i0], theta, mode, nu_p, nu_B, cst_min);
    double j0, k0;
	GS_jk_MultiDF(&w, mdf, 1, &j0, &k0);
	double mj=j0/j[i0];
	double mk=k0/k[i0];
	for (int i=i0; i<N_nu; i++)
	{
	 j[i]*=mj;
	 k[i]*=mk; 
	}
   }
  }
 }

 if (nu_cr<0.0 && nu_cr_WH>0.0)
 {
  EmWave w=EmWave(nu_cr_WH, theta, mode, nu_p, nu_B, cst_min);
  if (w.Valid)
  {
   double ja, ka;
   GS_jk_approx_MultiDF(&w, mdf, Npoints, Q_on, &ja, &ka);
   if (ja!=0.0 && ka!=0.0)
   {
    double je, ke;
    GS_jk_MultiDF(&w, mdf, 1, &je, &ke);
	double mj=je/ja;
	double mk=ke/ka;
	for (int i=0; i<N_nu; i++)
	{
	 j[i]*=mj;
	 k[i]*=mk;
	}
   }
  }
 }
}

void Add_jk_FF(double n0, double T0,
	           int N_nu, double *nu, int mode,
	           double theta, double nu_p, double nu_B, double cst_min,
			   double *j, double *k)
{
 if (T0>0.0) for (int i=0; i<N_nu; i++)
 {
  EmWave w=EmWave(nu[i], theta, mode, nu_p, nu_B, cst_min);
  double jFF, kFF;
  FF_jk(&w, n0, T0, &jFF, &kFF);
  j[i]+=jFF;
  k[i]+=kFF;
 }
}

void FillCutoff(int Nnu, double *nu, double nu_B, double nu_p, int *cutoff)
{
 double nu_X=nu_B/2+sqrt(sqr(nu_p)+sqr(nu_B)/4);
 
 for (int i=0; i<Nnu; i++) cutoff[i]=(nu[i]<=nu_X);
}

int FindLocalGS(double *ParmIn, int Nnu, double *nu, 
	            double *jX, double *jO, double *kX, double *kO, int *cutoff, double *n0)
{
 int res=0;

 double T0=ParmIn[2];
 int Npoints=(int)ParmIn[5];
 *n0=ParmIn[11];
 double B=ParmIn[13];
 double theta=ParmIn[14];
 double y_cr=ParmIn[25];
 double y_cr_WH=ParmIn[26];
 int m_on=(int)ParmIn[27];
 int Q_on=(int)ParmIn[28];

 double nu_B=2.80e6*B;
 double nu_cr=nu_B*y_cr;
 double nu_cr_WH=nu_B*y_cr_WH;

 *n0*=Saha(*n0, fabs(T0)); 

 Multi_Std_DF *MDF=new Multi_Std_DF(*n0, ParmIn);

 if (!MDF->OK) res=1;
 else
 {
  double nu_p=8.98e3*sqrt(*n0+MDF->n_b);

  FillCutoff(Nnu, nu, nu_B, nu_p, cutoff);
  
  Find_jk_GS(MDF->mdf, Nnu, nu, -1, theta, nu_p, nu_B, CST_MIN_DEFAULT, 
             nu_cr, nu_cr_WH, Npoints, Q_on, m_on, jX, kX);
  Find_jk_GS(MDF->mdf, Nnu, nu,  1, theta, nu_p, nu_B, CST_MIN_DEFAULT, 
	         nu_cr, nu_cr_WH, Npoints, Q_on, m_on, jO, kO);

  Add_jk_FF(*n0, T0, Nnu, nu, -1, theta, nu_p, nu_B, CST_MIN_DEFAULT, jX, kX);
  Add_jk_FF(*n0, T0, Nnu, nu,  1, theta, nu_p, nu_B, CST_MIN_DEFAULT, jO, kO);
 }

 delete MDF;

 return res;
}

int MWTransfer(int NSteps, double **Parms, double *nu, double **RL, double **S)
{
 int res=0;

 double A=Parms[0][0];
 nu[0]=Parms[0][15];
 double dnu=Parms[0][16];
 int Nnu=(int)Parms[0][18];
 int Closed=(int)Parms[0][30];
 int spl_on=(int)Parms[0][31];
 double MinStep=Parms[0][32];
 
 dnu=pow(10.0, dnu);
 for (int i=1; i<Nnu; i++) nu[i]=nu[i-1]*dnu;

 double sang=A/(sqr(AU)*sfu);

 double **jX, **jO, **kX, **kO;
 int **cutoff;
 jX=(double**)malloc(sizeof(double*)*NSteps);
 jO=(double**)malloc(sizeof(double*)*NSteps);
 kX=(double**)malloc(sizeof(double*)*NSteps);
 kO=(double**)malloc(sizeof(double*)*NSteps);
 cutoff=(int**)malloc(sizeof(int*)*NSteps);
 for (int i=0; i<NSteps; i++)
 {
  jX[i]=(double*)malloc(sizeof(double)*Nnu);
  jO[i]=(double*)malloc(sizeof(double)*Nnu);
  kX[i]=(double*)malloc(sizeof(double)*Nnu);
  kO[i]=(double*)malloc(sizeof(double)*Nnu);
  cutoff[i]=(int*)malloc(sizeof(int)*Nnu);
 }

 double *z=(double*)malloc(sizeof(double)*NSteps); 
 double *n0=(double*)malloc(sizeof(double)*NSteps); 
 double *B=(double*)malloc(sizeof(double)*NSteps); 
 double *theta=(double*)malloc(sizeof(double)*NSteps);
 double *psi=(double*)malloc(sizeof(double)*NSteps);

 //calculating the emissivities and absorption coefficients
 int err=0;
 for (int i=0; i<NSteps && !err; i++) 
 {
  double *ParmIn=Parms[i];
  z[i]=ParmIn[1];
  B[i]=ParmIn[13];
  theta[i]=ParmIn[14];
  psi[i]=ParmIn[29];
  err=FindLocalGS(ParmIn, Nnu, nu, jX[i], jO[i], kX[i], kO[i], cutoff[i], n0+i);
 }

 if (err)
 {
  IDLmsg("MWTransfer error: gyrosynchrotron calculation error.");
  res=1;
 }
 else 
 {
  double *jXf=(double*)malloc(sizeof(double)*NSteps); 
  double *jOf=(double*)malloc(sizeof(double)*NSteps); 
  double *kXf=(double*)malloc(sizeof(double)*NSteps); 
  double *kOf=(double*)malloc(sizeof(double)*NSteps);

  double S_in[4];
  for (int k=0; k<4; k++) S_in[k]=0.0;

  for (int i=0; i<Nnu; i++)
  {
   for (int j=0; j<NSteps; j++)
   {
	jXf[j]=jX[j][i];
	jOf[j]=jO[j][i];
	kXf[j]=kX[j][i];
	kOf[j]=kO[j][i];
   }

   Ray *r=new Ray(NSteps, z, n0, B, theta, psi, jXf, jOf, kXf, kOf, Closed, spl_on);
   if (r->OK)
   {
	r->RadiationTransferRL(nu[i], 0.0, 0.0, RL[i]);
	
	if (S)
	{
	 for (int k=0; k<4; k++) S[i][k]=0.0;

	 int lc=0;
	 for (int j=0; j<NSteps; j++) lc=lc || cutoff[j][i];

	 double Imax=RL[i][4]+RL[i][5];

	 if (!lc && Imax>0) r->RadiationTransferExact(nu[i], S_in, Imax, MinStep, S[i]);	 
	}

	for (int k=0; k<6; k++) RL[i][k]*=sang;
	if (S) for (int k=0; k<4; k++) S[i][k]*=sang;
   }
   else res=2;

   delete r;
  }

  free(jXf);
  free(jOf);
  free(kXf);
  free(kOf);
 }

 for (int i=0; i<NSteps; i++)
 {
  free(jX[i]);
  free(jO[i]);
  free(kX[i]);
  free(kO[i]);
  free(cutoff[i]);
 }
 free(jX);
 free(jO);
 free(kX);
 free(kO);
 free(cutoff);
 free(z);
 free(n0);
 free(B);
 free(theta);
 free(psi);

 return res;
}

int FaradayRotation(double *ParmsG, double **ParmsLA, double *S_out)
{
 int Npoints=(int)ParmsG[0];
 int Closed=(int)ParmsG[1];
 int spl_on=(int)ParmsG[2];
 double MinStep=ParmsG[3];

 double *zero=(double*)malloc(sizeof(double)*Npoints);
 for (int i=0; i<Npoints; i++) zero[i]=0.0;

 int res=0;

 Ray *r=new Ray(Npoints, 
	            ParmsLA[0], //z_in
	            ParmsLA[1], //n0,
				ParmsLA[2], //B
				ParmsLA[3], //theta
				ParmsLA[4], //psi
	            zero, zero, zero, zero, //jX, jO, kX, kO
				Closed, spl_on);

 if (r->OK)
 {
  double nu=ParmsG[4];
  if (nu<=0.0)
  {
   IDLmsg("FaradayRotation error: emission frequency must be positive.");
   res=-4;
  }
  else
  {
   double S_in[4];
   S_in[0]=ParmsG[5]; //I
   S_in[1]=ParmsG[6]; //Q
   S_in[2]=ParmsG[7]; //U
   S_in[3]=ParmsG[8]; //V

   if (S_in[0]==0.0) for (int A=0; A<4; A++) S_out[A]=0.0;
   else if (S_in[0]<0.0)
   {
    IDLmsg("FaradayRotation error: emission intensity cannot be negative.");
    res=-5;
   }
   else
   {
	double Ipol=sqrt(sqr(S_in[1])+sqr(S_in[2])+sqr(S_in[3]));
	if (S_in[0]<Ipol) 
	 IDLmsg("FaradayRotation warning: polarized intensity exceeds total intensity.");

	res=r->RadiationTransferExact(nu, S_in, S_in[0], MinStep, S_out);
   }
  }
 }
 else res=-3;

 delete r;
 free(zero);

 return res;
}