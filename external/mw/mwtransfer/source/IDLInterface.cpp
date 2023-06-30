#include <stdlib.h>
#include "Messages.h"
#include "ExtMath.h"
#include "MWMain.h"
#include "Astrophys.h"

extern "C" int GetMWTransfer(int argc, void **argv)
{
 #define InSizeF 33
 #define RLSize 7
 #define SSize 5

 if (argc<3)
 {
  IDLmsg("GetMWTransfer error: not enough parameters.");
  return -1;
 }

 int NSteps=*((int*)argv[0]);
 if (NSteps==(-1)) NSteps=((int*)argv[0])[1];
 
 if (NSteps<1)
 {
  IDLmsg("GetMWTransfer error: number of nodes must be positive.");
  return -2;
 }

 float *Parms=(float*)argv[1];
 float *RL=(float*)argv[2];
 float *S=(argc>3) ? (float*)argv[3] : 0;

 int Nnu=(int)Parms[18];
 if (Nnu<1)
 {
  IDLmsg("GetMWTransfer error: number of emission frequencies must be positive.");
  return -3;
 }

 double **ParmsD=(double**)malloc(sizeof(double*)*NSteps);

 for (int i=0; i<NSteps; i++)
 {
  ParmsD[i]=(double*)malloc(sizeof(double)*InSizeF);
  float *ParmIn=Parms+i*InSizeF;
  for (int j=0; j<InSizeF; j++)
  {
   ParmsD[i][j]=(double)ParmIn[j];
   if (j==14 || j==20 || j==21 || j==29) ParmsD[i][j]*=(M_PI/180);
   if (j==6 || j==7 || j==8) ParmsD[i][j]*=(eV*1e6);
  }
 }

 double *nu=(double*)malloc(sizeof(double)*Nnu);
 double **RLD=(double**)malloc(sizeof(double*)*Nnu);
 for (int i=0; i<Nnu; i++) RLD[i]=(double*)malloc(sizeof(double)*6);
 double **SD=0;
 if (S)
 {
  SD=(double**)malloc(sizeof(double*)*Nnu);
  for (int i=0; i<Nnu; i++) SD[i]=(double*)malloc(sizeof(double)*4);
 }

 int res=MWTransfer(NSteps, ParmsD, nu, RLD, SD);

 if (!res) for (int i=0; i<Nnu; i++)
 {
  float *r=RL+i*RLSize;
  r[0]=float(nu[i]/1e9);
  for (int j=0; j<6; j++) r[j+1]=float(RLD[i][j]);
  if (S)
  {
   r=S+i*SSize;
   r[0]=float(nu[i]/1e9);
   for (int j=0; j<4; j++) r[j+1]=float(SD[i][j]);
  }
 }

 for (int i=0; i<NSteps; i++) free(ParmsD[i]);
 free(ParmsD);
 free(nu);
 for (int i=0; i<Nnu; i++) free(RLD[i]);
 free(RLD);
 if (SD)
 {
  for (int i=0; i<Nnu; i++) free(SD[i]);
  free(SD);
 }

 return res;
}

extern "C" float Get_MW_Single(int argc, void **argv)
{
 #define InSizeF 33
 #define InSizeS 29
 #define RLSize 7
 #define SSize 5

 if (argc<2)
 {
  IDLmsg("Get_MW_Single error: not enough parameters.");
  return -1;
 }

 float *ParmIn=(float*)argv[0];
 float *s=(float*)argv[1];

 int Nnu=(int)ParmIn[18];
 if (Nnu<1)
 {
  IDLmsg("Get_MW_Single error: number of emission frequencies must be positive.");
  return -3;
 }

 float theta=ParmIn[14];

 float ParmF[InSizeF];
 for (int i=0; i<InSizeS; i++) ParmF[i]=ParmIn[i];
 for (int i=InSizeS; i<InSizeF; i++) ParmF[i]=0.0;

 float *RL=(float*)malloc(sizeof(float)*Nnu*RLSize);

 short NSteps=1;

 void *ARGV[3];
 ARGV[0]=(void*)&NSteps;
 ARGV[1]=(void*)ParmF;
 ARGV[2]=(void*)RL;

 float res=(float)GetMWTransfer(3, ARGV);

 if (!res) for (int i=0; i<Nnu; i++)
 {
  float *rr=RL+i*RLSize;
  float *ss=s+i*SSize;
  ss[0]=rr[0];
  ss[1]=(theta>90.0) ? rr[4] : rr[5];
  ss[3]=(theta>90.0) ? rr[5] : rr[4];
 }

 free(RL);

 return res;
}

extern "C" float GET_MW(int argc, void **argv)
{
 #define InSizeF 33
 #define InSizeS 29
 #define RLSize 7

 if (argc<3)
 {
  IDLmsg("Get_MW error: not enough parameters.");
  return -1;
 }

 int NSteps=*((int*)argv[0]);
 float *ParmS=(float*)argv[1];
 float *RL=(float*)argv[2];

 float *ParmF=(float*)malloc(sizeof(float)*InSizeF*NSteps);
 for (int i=0; i<NSteps; i++)
 {
  float *ss=ParmS+i*InSizeS;
  float *ff=ParmF+i*InSizeF;
  for (int j=0; j<InSizeS; j++) ff[j]=ss[j];
  for (int j=InSizeS; j<InSizeF; j++) ff[j]=0.0;
 }

 void *ARGV[3];
 ARGV[0]=(void*)&NSteps;
 ARGV[1]=(void*)ParmF;
 ARGV[2]=(void*)RL;

 float res=(float)GetMWTransfer(3, ARGV);

 free(ParmF);

 return res;
}

extern "C" int GetFaradayRotation(int argc, void **argv)
{
 if (argc<3)
 {
  IDLmsg("GetFaradayRotation error: not enough parameters.");
  return -1;
 }

 double *ParmsG=(double*)argv[0];
 double *ParmsL=(double*)argv[1];
 double *S_out=(double*)argv[2];

 int Npoints=(int)ParmsG[0];
 
 if (Npoints<1)
 {
  IDLmsg("GetFaradayRotation error: number of nodes must be positive.");
  return -2;
 }

 #define InSizeParmsL 5
 double *ParmsLA[InSizeParmsL];
 for (int i=0; i<InSizeParmsL; i++) ParmsLA[i]=(double*)malloc(sizeof(double)*Npoints);
 
 for (int i=0; i<Npoints; i++)
 {
  double *P=ParmsL+i*InSizeParmsL;
  ParmsLA[0][i]=P[0]; //z
  ParmsLA[1][i]=P[1]; //n0
  ParmsLA[2][i]=P[2]; //B
  ParmsLA[3][i]=P[3]*M_PI/180; //theta
  ParmsLA[4][i]=P[4]*M_PI/180; //psi
 }

 int res=FaradayRotation(ParmsG, ParmsLA, S_out);

 for (int i=0; i<InSizeParmsL; i++) free(ParmsLA[i]);

 return res;
}