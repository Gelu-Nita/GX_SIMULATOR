#include <malloc.h>
#include <math.h>
#include <float.h>
#include <memory.h>
#include "MWtransfer.h"
#include "Plasma.h"
#include "ExtMath.h"
#include "DEM.h"
#include "FF.h"
#include "Neutrals.h"
#include "GR.h"
#include "Messages.h"

typedef struct
{
 double B, theta, psi, Bx, By, Bz;
 double B1, B2, Bz1, Bz2;
 double B_a[2], B_b[2], Bx_a[2], Bx_b[2], By_a[2], By_b[2], Bz_a[2], Bz_b[2];
 double dB_dz[2], dtheta_dz[2];
 double n_e, T0;
 double n_H, n_He;
 double dz;
 double f_p;
 int DEM_on, DDM_on, FF_on, GR_on, HHe_on, s_max, j_ofs, ABcode, dfcode;
 double kn;
} Voxel;

void ProcessVoxels(int Nz0, double *Parms, int NT, double *T_arr, double *lnT_arr, double *DEM_arr, double *DDM_arr, Voxel *V)
{
 for (int j=0; j<Nz0; j++)
 {
  double *p=Parms+j*InSize;

  V[j].dz=max(p[0], 0.0);
  V[j].T0=max(p[1], 0.0);
  V[j].n_e=max(p[2], 0.0);
  V[j].B=max(p[3], 0.0);
  V[j].theta=p[4]*M_PI/180;
  V[j].psi=p[5]*M_PI/180;

  int em_flag=(int)p[6];
  V[j].GR_on=((em_flag & 1)==0);
  V[j].FF_on=((em_flag & 2)==0);
  V[j].HHe_on=((em_flag & 4)==0);

  V[j].s_max=(int)p[7];
  V[j].n_H=max(p[8], 0.0); 
  V[j].n_He=max(p[9], 0.0);

  V[j].DEM_on=(p[10]==0 && NT>1);
  V[j].DDM_on=(p[11]==0 && NT>1);

  V[j].ABcode=(int)p[12];
  if (V[j].ABcode<0 || V[j].ABcode>2) V[j].ABcode=0;

  V[j].dfcode=(int)p[13];
  if (V[j].dfcode<0 || V[j].dfcode>2) V[j].dfcode=0;

  V[j].kn=p[14]; //probably, has to be checked for correct values

  V[j].j_ofs=j;

  V[j].Bx=V[j].B*sin(V[j].theta)*cos(V[j].psi);
  V[j].By=V[j].B*sin(V[j].theta)*sin(V[j].psi);
  V[j].Bz=V[j].B*cos(V[j].theta);

  if (V[j].DDM_on) DDM_moments(T_arr, lnT_arr, DDM_arr+NT*j, NT, &(V[j].n_e), &(V[j].T0));
  else if (V[j].DEM_on) DEM_moments(T_arr, lnT_arr, DEM_arr+NT*j, NT, &(V[j].n_e), &(V[j].T0));
                 
  V[j].f_p=e*sqrt(V[j].n_e/me/M_PI); 
 }
}

void CompressVoxels(Voxel *V, int Nz0, int *Nz)
{
 int jmin;
 for (jmin=0; jmin<Nz0; jmin++) if (V[jmin].n_e>0) break;

 int jmax;
 for (jmax=Nz0-1; jmax>=0; jmax--) if (V[jmax].n_e>0) break;

 *Nz=0;

 for (int j=jmin; j<=jmax; j++) if (V[j].dz>0)
 {
  if (*Nz!=j) memcpy(V+(*Nz), V+j, sizeof(Voxel));
  (*Nz)++;
 }
}

void ProcessVoxelGradients(Voxel *V, int Nz)
{
 for (int j=0; j<Nz; j++)
 {
  if (j==0 && j==(Nz-1))
  {
   V[j].B1=V[j].B2=V[j].B;
   V[j].Bz1=V[j].Bz2=V[j].Bz;
   V[j].B_a[0]=V[j].B_a[1]=V[j].Bx_a[0]=V[j].Bx_a[1]=V[j].By_a[0]=V[j].By_a[1]=V[j].Bz_a[0]=V[j].Bz_a[1]=0;
   V[j].B_b[0]=V[j].B_b[1]=V[j].B;
   V[j].Bx_b[0]=V[j].Bx_b[1]=V[j].Bx;
   V[j].By_b[0]=V[j].By_b[1]=V[j].By;
   V[j].Bz_b[0]=V[j].Bz_b[1]=V[j].Bz;
   V[j].dB_dz[0]=V[j].dB_dz[1]=V[j].dtheta_dz[0]=V[j].dtheta_dz[1]=0;
  }
  else 
  {
   for (int k=0; k<2; k++)
   {
    int j1, j2;
    double z1, z2;

    if (j==0)
    {
     j1=j;
     j2=j+1;
     z1=V[j1].dz/2;
     z2=V[j1].dz+V[j2].dz/2;
    }
    else if (j==(Nz-1))
    {
     j1=j-1;
     j2=j;
     z1=-V[j1].dz/2;
     z2=V[j2].dz/2;
    }
    else
    {
     if (k==0)
     {
      j1=j-1;
      j2=j;
      z1=-V[j1].dz/2;
      z2=V[j2].dz/2;
     }
     else
     {
      j1=j;
      j2=j+1;
      z1=V[j1].dz/2;
      z2=V[j1].dz+V[j2].dz/2;
     }
    }
                
    V[j].B_a[k]=V[j].dB_dz[k]=(V[j1].B-V[j2].B)/(z1-z2);
    V[j].B_b[k]=(V[j2].B*z1-V[j1].B*z2)/(z1-z2);
    V[j].Bx_a[k]=(V[j1].Bx-V[j2].Bx)/(z1-z2);
    V[j].Bx_b[k]=(V[j2].Bx*z1-V[j1].Bx*z2)/(z1-z2);
    V[j].By_a[k]=(V[j1].By-V[j2].By)/(z1-z2);
    V[j].By_b[k]=(V[j2].By*z1-V[j1].By*z2)/(z1-z2);
    V[j].Bz_a[k]=(V[j1].Bz-V[j2].Bz)/(z1-z2);
    V[j].Bz_b[k]=(V[j2].Bz*z1-V[j1].Bz*z2)/(z1-z2);
    V[j].dtheta_dz[k]=(V[j1].theta-V[j2].theta)/(z1-z2);
   }

   V[j].B1=V[j].B_b[0];
   V[j].B2=V[j].B_a[1]*V[j].dz+V[j].B_b[1];
   V[j].Bz1=V[j].Bz_b[0];
   V[j].Bz2=V[j].Bz_a[1]*V[j].dz+V[j].Bz_b[1];
  }
 }
}

typedef struct
{
 int s; //harmonic number; if <2, then QT layer assumed
 double z0; //location, relative to the voxel start
} Level;

void AddLevel(Level **l, int s, double z0, int *Nlev, int *NlevMax)
{
 int old=0;

 for (int i=0; i<*Nlev; i++) if ((*l)[i].s==s && (*l)[i].z0==z0)
 {
  old=1;
  break;
 }

 if (!old)
 {
  (*l)[*Nlev].s=s;
  (*l)[*Nlev].z0=z0;
  (*Nlev)++;

  if (*Nlev>=*NlevMax)
  {
   (*NlevMax)+=10;
   *l=(Level*)realloc(*l, sizeof(Level)*(*NlevMax));
  }
 }
}

void SortLevels(Level *l, int Nlev)
{
 if (Nlev>1)
 {
  Level a;

  for (int i=0; i<(Nlev-1); i++) for (int j=i+1; j<Nlev; j++) if (l[i].z0>l[j].z0)
  {
   memcpy(&a, l+i, sizeof(Level));
   memcpy(l+i, l+j, sizeof(Level));
   memcpy(l+j, &a, sizeof(Level));
  }
 }
}

int MW_Transfer(int *Lparms, double *Rparms, double *Parms, double *T_arr, double *DEM_arr, double *DDM_arr, double *RL)
{
 int res=0;

 int Nz0=Lparms[0];
 int Nf=Lparms[1];
 int NT=Lparms[2];

 double Sang=Rparms[0]/(sqr(AU)*sfu);
 
 double *f=(double*)malloc(sizeof(double)*Nf);
 if (Rparms[1]>0)
 {
  f[0]=Rparms[1];
  double dnu=pow(10.0, Rparms[2]);
  for (int i=1; i<Nf; i++) f[i]=f[i-1]*dnu;
 }
 else for (int i=0; i<Nf; i++) f[i]=RL[i*OutSize]*1e9;

 double *lnT_arr=0;
 if (NT>1)
 {
  lnT_arr=(double*)malloc(sizeof(double)*NT);
  for (int i=0; i<NT; i++) lnT_arr[i]=log(T_arr[i]);
 } 

 Voxel *V=(Voxel*)malloc(sizeof(Voxel)*Nz0);

 ProcessVoxels(Nz0, Parms, NT, T_arr, lnT_arr, DEM_arr, DDM_arr, V);

 int Nz;
 CompressVoxels(V, Nz0, &Nz);

 ProcessVoxelGradients(V, Nz);

 int NlevMax=10;
 Level *l=(Level*)malloc(sizeof(Level)*NlevMax);

 for (int i=0; i<Nf; i++)
 {
  double Lw=RL[i*OutSize+1]/Sang;
  double Rw=RL[i*OutSize+2]/Sang;
  double Ls=RL[i*OutSize+3]/Sang;
  double Rs=RL[i*OutSize+4]/Sang;
  double Le=RL[i*OutSize+5]/Sang;
  double Re=RL[i*OutSize+6]/Sang;

  double B_res=f[i]*2*M_PI*me*c/e;

  for (int j=0; j<Nz; j++)
  {
   int Nlev=0;

   for (int lr=0; lr<2; lr++)
   {
    int QTfound=(lr==0) ? V[j].Bz1*V[j].Bz<0 : V[j].Bz*V[j].Bz2<0;

    if (QTfound)
    {
     double z0=-V[j].Bz_b[lr]/V[j].Bz_a[lr];
     if (z0!=(V[j].dz/2)) AddLevel(&l, 0, z0, &Nlev, &NlevMax); 
    }
   }

   if (V[j].GR_on) for (int lr=0; lr<2; lr++)
   {
    double B1=(lr==0) ? V[j].B1 : V[j].B2;
    double B2=V[j].B;

    if (B1>0 && B2>0 && B1!=B2)
    {
     int smin=(int)ceil(B_res/max(B1, B2));
     int smax=(int)floor(B_res/min(B1, B2)); 
     smin=max(smin, 2);
     smax=min(smax, V[j].s_max);

     for (int s=smin; s<=smax; s++)
     {
      double z0=(B_res/s-V[j].B_b[lr])/V[j].B_a[lr];
      if (z0!=(V[j].dz/2)) AddLevel(&l, s, z0, &Nlev, &NlevMax); 
     }
    }
   }

   SortLevels(l, Nlev);

   for (int k=0; k<=Nlev; k++)
   {
    double z1=(k==0) ? 0 : l[k-1].z0;
    double z2=(k==Nlev) ? V[j].dz : l[k].z0;
    double dz=z2-z1;
        
    if (dz>0)
    {
     double zc=(z1+z2)/2;
     int lr=(zc<(V[j].dz/2)) ? 0 : 1;
     double Bx=V[j].Bx_a[lr]*zc+V[j].Bx_b[lr];
     double By=V[j].By_a[lr]*zc+V[j].By_b[lr];
     double Bz=V[j].Bz_a[lr]*zc+V[j].Bz_b[lr];
       
     double B=sqrt(sqr(Bx)+sqr(By)+sqr(Bz));
     double theta=(B>0) ? acos(Bz/B) : 0.0;
     double f_B=e*B/me/c/(2.0*M_PI);

     double jXff, kXff, jOff, kOff;
     jXff=jOff=kXff=kOff=0;

     if (V[j].FF_on)
     {
      if (V[j].DEM_on) FindFF_DEM_XO(f[i], theta, V[j].f_p, f_B, T_arr, lnT_arr, DEM_arr+NT*V[j].j_ofs, NT, V[j].ABcode, &jXff, &kXff, &jOff, &kOff);
      else
      {
       FindFF_single(f[i], theta, -1, V[j].f_p, f_B, V[j].T0, V[j].n_e, V[j].ABcode, &jXff, &kXff);
       FindFF_single(f[i], theta,  1, V[j].f_p, f_B, V[j].T0, V[j].n_e, V[j].ABcode, &jOff, &kOff); 
      }
     }

     double jXen, kXen, jOen, kOen;
     jXen=kXen=jOen=kOen=0;

     if (V[j].HHe_on)
     {
      FindNeutralsEffect(f[i], theta, -1, V[j].f_p, f_B, V[j].T0, V[j].n_e, V[j].n_H, V[j].n_He, &jXen, &kXen);
      FindNeutralsEffect(f[i], theta,  1, V[j].f_p, f_B, V[j].T0, V[j].n_e, V[j].n_H, V[j].n_He, &jOen, &kOen);
     }

     double jX=jXff+jXen;
     double kX=kXff+kXen;
     double jO=jOff+jOen;
     double kO=kOff+kOen;
    
     double tauX=-kX*dz;
     double eX=(tauX<700) ? exp(tauX) : 0.0;
     double dIX=(kX==0.0 || tauX>700) ? 0.0 : jX/kX*((1.0-eX) ? 1.0-eX : -tauX);
     double tauO=-kO*dz;
     double eO=(tauO<700) ? exp(tauO) : 0.0; 
     double dIO=(kO==0.0 || tauO>700) ? 0.0 : jO/kO*((1.0-eO) ? 1.0-eO : -tauO);
     
     if (theta>(M_PI/2))
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

    if (k!=Nlev)
    {
     int lr=(l[k].z0<(V[j].dz/2)) ? 0 : 1;
     double Bx=V[j].Bx_a[lr]*l[k].z0+V[j].Bx_b[lr];
     double By=V[j].By_a[lr]*l[k].z0+V[j].By_b[lr];
     double Bz=V[j].Bz_a[lr]*l[k].z0+V[j].Bz_b[lr];
     double dB_dz=fabs(V[j].dB_dz[lr]);
     double dtheta_dz=fabs(V[j].dtheta_dz[lr]); 
    
     if (l[k].s<2) //QT layer
     {
      double a=Lw;
      Lw=Rw;
      Rw=a;

      double B=sqrt(sqr(Bx)+sqr(By)+sqr(Bz));
      double QT=e*e*e*e*e/(32*M_PI*M_PI*me*me*me*me*c*c*c*c)*V[j].n_e*sqr(B)*B/sqr(sqr(f[i]))/dtheta_dz;
      QT=exp(-QT);
      a=Le*QT+Re*(1.0-QT);
      Re=Re*QT+Le*(1.0-QT);
      Le=a;
     }
     else //GR layer
     { 
      double B=B_res/l[k].s;
      double f_B=f[i]/l[k].s;
      double theta=acos(Bz/sqrt(sqr(Bx)+sqr(By)+sqr(Bz)));
      double LB=B/dB_dz;

      double tauX, tauO, I0X, I0O;
      tauX=tauO=I0X=I0O=0;

      if (V[j].DDM_on) FindGR_DDM_XO(f[i], theta, l[k].s, V[j].f_p, f_B, T_arr, lnT_arr, DDM_arr+NT*V[j].j_ofs, NT, LB, &tauX, &I0X, &tauO, &I0O);
      else
      {
       FindGR_single(f[i], theta, -1, l[k].s, V[j].f_p, f_B, V[j].n_e, V[j].T0, LB, &tauX, &I0X);
       FindGR_single(f[i], theta,  1, l[k].s, V[j].f_p, f_B, V[j].n_e, V[j].T0, LB, &tauO, &I0O);
      }

      double eX=exp(-tauX);
      double dIX=I0X*((1.0-eX) ? 1.0-eX : tauX);
      double eO=exp(-tauO);
      double dIO=I0O*((1.0-eO) ? 1.0-eO : tauO);

      if (theta>(M_PI/2))
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
    }
   }
  }

  RL[i*OutSize]=f[i]/1e9;
  RL[i*OutSize+1]=Lw*Sang;
  RL[i*OutSize+2]=Rw*Sang;
  RL[i*OutSize+3]=Ls*Sang;
  RL[i*OutSize+4]=Rs*Sang;
  RL[i*OutSize+5]=Le*Sang;
  RL[i*OutSize+6]=Re*Sang;
 }

 free(l);
 free(V);
 free(f);
 if (lnT_arr) free (lnT_arr);

 return res;
}