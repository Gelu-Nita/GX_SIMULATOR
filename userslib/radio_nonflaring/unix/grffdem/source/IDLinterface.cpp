#include <stdlib.h>
#include "MWtransfer.h"
#include "Coulomb.h"
#include "Zeta.h"
#include "Plasma.h"
#include "Messages.h"
#ifndef LINUX
#include <ppl.h>
#else
#include <omp.h>
#endif

#ifndef LINUX
extern "C" __declspec(dllexport) int GET_MW_main(int argc, void **argv)
#else
extern "C" int GET_MW_main(int argc, void **argv)
#endif
{
 if (argc<7)
 {
  IDLmsg("GET_MW_main error: not enough parameters in the function call.");
  return -1;
 }

 int *Lparms=(int*)argv[0];
 double *Rparms=(double*)argv[1];
 double *Parms=(double*)argv[2];
 double *T_arr=(double*)argv[3];
 double *DEM_arr=(double*)argv[4];
 double *DDM_arr=(double*)argv[5];
 double *RL=(double*)argv[6];
           
 int res=MW_Transfer(Lparms, Rparms, Parms, T_arr, DEM_arr, DDM_arr, RL);

 return res;
}

#ifndef LINUX
extern "C" __declspec(dllexport) int GET_MW(int argc, void **argv)
#else
extern "C" int GET_MW(int argc, void **argv)
#endif
{
 if (argc<7)
 {
  IDLmsg("GET_MW error: not enough parameters in the function call.");
  return -1;
 }

 int *Lparms=(int*)argv[0];
 double *Rparms=(double*)argv[1];
 double *Parms1=(double*)argv[2];
 double *T_arr=(double*)argv[3];
 double *DEM_arr=(double*)argv[4];
 double *DDM_arr=(double*)argv[5];
 double *RL=(double*)argv[6];

 #define InSize1 15

 int Nz=Lparms[0];
 double *Parms=(double*)malloc(sizeof(double)*InSize*Nz);

 int NT=Lparms[2];

 int DEM_on_global=(Lparms[3]==0);
 int DDM_on_global=(Lparms[4]==0);

 for (int j=0; j<Nz; j++)
 {
  double *p=Parms+j*InSize;
  double *p1=Parms1+j*InSize1;

  for (int i=0; i<=7; i++) p[i]=p1[i]; //parameters 0-7 are the same in both functions
  p[8]=p1[9]; //n_H
  p[9]=p1[10]; //n_He
  p[10]=DEM_on_global ? p1[11] : 1; //DEM key
  p[11]=DDM_on_global ? p1[12] : 1; //DDM key
  p[12]=p1[13]; //abundance key
  p[13]=p[14]=0; //Maxwellian distribution only

  int DEM_on=(p[10]==0 && NT>1);
  int DDM_on=(p[11]==0 && NT>1);

  if (!DEM_on && !DDM_on)
  {
   double T0=p1[1];

   if (T0<1e5)
   {
    double n_p=p1[8];
    double n_H=p1[9];

    if (n_p==0 && n_H==0)
    {
     double n0=p1[2];

     double n_e, n_He;

     FindIonizationsSolar(n0, T0, &n_e, &n_H, &n_He);

     p[2]=n_e;
     p[8]=n_H;
     p[9]=n_He; 
    }
   } //otherwise, if T0=>1e5, n_e (p[2]) has already been assigned equal to n_0(p1[2])
  }
 }
           
 int res=MW_Transfer(Lparms, Rparms, Parms, T_arr, DEM_arr, DDM_arr, RL);

 free(Parms);

 return res;
}

#ifndef LINUX
extern "C" __declspec(dllexport) int GET_MW_SLICE(int argc, void** argv)
#else
extern "C" int GET_MW_SLICE(int argc, void** argv)
#endif
{
 if (argc<7)
 {
  IDLmsg("GET_MW_SLICE error: not enough parameters in the function call.");
  return -1;
 }

 int *Lparms_M=(int*)argv[0];
 double *Rparms_M=(double*)argv[1];
 double *Parms_M=(double*)argv[2];
 double *T_arr=(double*)argv[3];
 double *DEM_arr_M=(double*)argv[4];
 double *DDM_arr_M=(double*)argv[5];
 double *RL_M=(double*)argv[6];

 int Npix=Lparms_M[0];
 int Nz=Lparms_M[1];
 int Nf=Lparms_M[2];
 int NT=Lparms_M[3];

 #ifndef LINUX

 concurrency::parallel_for(0, Npix, [&](int pix)
 {
  void *ARGV[7];
  ARGV[0]=(void*)(Lparms_M+1);
  ARGV[1]=(void*)(Rparms_M+pix*RpSize);
  ARGV[2]=(void*)(Parms_M+pix*Nz*InSize);
  ARGV[3]=(void*)T_arr;
  ARGV[4]=(void*)(DEM_arr_M+pix*Nz*NT);
  ARGV[5]=(void*)(DDM_arr_M+pix*Nz*NT);
  ARGV[6]=(void*)(RL_M+pix*Nf*OutSize);

  GET_MW(7, ARGV);
 });

 #else

 #pragma omp parallel for
 for(int pix=0; pix<Npix; pix++)
 {
  void *ARGV[7];
  ARGV[0]=(void*)(Lparms_M+1);
  ARGV[1]=(void*)(Rparms_M+pix*RpSize);
  ARGV[2]=(void*)(Parms_M+pix*Nz*InSize);
  ARGV[3]=(void*)T_arr;
  ARGV[4]=(void*)(DEM_arr_M+pix*Nz*NT);
  ARGV[5]=(void*)(DDM_arr_M+pix*Nz*NT);
  ARGV[6]=(void*)(RL_M+pix*Nf*OutSize);

  GET_MW(7, ARGV);
 }

 #endif

 return 0;
}

#ifndef LINUX
extern "C" __declspec(dllexport) double CoulombLogarithm(int argc, void **argv)
#else
extern "C" double CoulombLogarithm(int argc, void **argv)
#endif
{
 double T=*((double*)argv[0]);
 double f=*((double*)argv[1]);

 return lnC1(T, f);
}

#ifndef LINUX
extern "C" __declspec(dllexport) double ZetaSolar(int argc, void **argv)
#else
extern "C" double ZetaSolar(int argc, void **argv)
#endif
{
 double T=*((double*)argv[0]);
 double f=*((double*)argv[1]);
 int ABcode=*((int*)argv[2]);

 return Zeta_Solar(T, f, ABcode);
}

#ifndef LINUX
extern "C" __declspec(dllexport) double SahaIonizationH(int argc, void **argv)
#else
extern "C" double SahaIonizationH(int argc, void **argv)
#endif
{
 double n0=*((double*)argv[0]);
 double T0=*((double*)argv[1]);
 return SahaH(n0, T0);
}

#ifndef LINUX
extern "C" __declspec(dllexport) double FindIonizations(int argc, void **argv)
#else
extern "C" double FindIonizations(int argc, void **argv)
#endif
{
 double n0=*((double*)argv[0]);
 double T0=*((double*)argv[1]);
 double *n_e=(double*)argv[2];
 double *n_H=(double*)argv[3];
 double *n_He=(double*)argv[4];

 FindIonizationsSolar(n0, T0, n_e, n_H, n_He);

 return *n_e/n0;
}