#include <stdlib.h>
#include <malloc.h>
#include "Messages.h"
#include "GRMain.h"

extern "C" int GET_MW(int argc, void **argv)
{
 #define InSize 30
 #define RLSize 7

 if (argc<3)
 {
  IDLmsg("GET_MW error: not enough parameters.");
  return -1;
 }
 else
 {
  int NSteps=*((short*)argv[0]);

  if (NSteps<1)
  {
   IDLmsg("GET_MW error: number of nodes must be positive.");
   return -2;
  }
  else
  {
   double *ParmsIn=(double*)argv[1];
   double *RL=(double*)argv[2];

   int Nnu=(int)ParmsIn[18];

   if (Nnu<1)
   {
	IDLmsg("GET_MW error: number of frequencies must be positive.");
	return -3;
   }
   else
   {
    double **Parms=(double**)malloc(sizeof(double*)*NSteps);
    for (int i=0; i<NSteps; i++)
    {
     Parms[i]=(double*)malloc(sizeof(double)*InSize);
     double *p=ParmsIn+i*InSize;
     for (int j=0; j<InSize; j++) Parms[i][j]=p[j];
    }
	
	double *nu=(double*)malloc(sizeof(double)*Nnu);
    double *Le=(double*)malloc(sizeof(double)*Nnu);
    double *Re=(double*)malloc(sizeof(double)*Nnu);
    double *Lw=(double*)malloc(sizeof(double)*Nnu);
    double *Rw=(double*)malloc(sizeof(double)*Nnu);
    double *Ls=(double*)malloc(sizeof(double)*Nnu);
    double *Rs=(double*)malloc(sizeof(double)*Nnu);

	for (int i=0; i<Nnu; i++)
	{
	 double *r=RL+i*RLSize;
	 Lw[i]=r[1];
	 Rw[i]=r[2];
	 Ls[i]=r[3];
	 Rs[i]=r[4];
	 Le[i]=r[5];
	 Re[i]=r[6];
	}

    int res=GRTransfer(NSteps, Parms, nu, Rw, Lw, Rs, Ls, Re, Le);

    for (int i=0; i<Nnu; i++)
    {
     double *r=RL+i*RLSize;
     r[0]=nu[i]/1e9;
     r[1]=Lw[i];
     r[2]=Rw[i];
     r[3]=Ls[i];
     r[4]=Rs[i];
     r[5]=Le[i];
     r[6]=Re[i];
    }

    free(nu);
    free(Le);
    free(Re);
    free(Ls);
    free(Rs);
    free(Lw);
    free(Rw);
    for (int i=0; i<NSteps; i++) free(Parms[i]);
    free(Parms);

    return res;
   }
  }
 }
}