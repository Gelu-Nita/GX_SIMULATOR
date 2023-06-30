#include "IDLinterface.h"
#include "Messages.h"
#include <stdio.h>

#ifndef LINUX
extern "C" __declspec(dllexport) int PyGET_MW_USER(int *Lparms, double *Rparms, double *Parms,
                                                   double *T_arr,double *DEM_arr, double *DDM_arr, 
                                                   double *fzeta_arr, double *Tzeta_arr, double *zeta_arr,
                                                   double *RL)
#else
extern "C" int PyGET_MW_USER(int *Lparms, double *Rparms, double *Parms,
                           double *T_arr,double *DEM_arr, double *DDM_arr, 
                           double *fzeta_arr, double *Tzeta_arr, double *zeta_arr,
                           double *RL)
#endif
{
 void *ARGV[10];
 ARGV[0]=(void*)Lparms;
 ARGV[1]=(void*)Rparms;
 ARGV[2]=(void*)Parms;
 ARGV[3]=(void*)T_arr;
 ARGV[4]=(void*)DEM_arr;
 ARGV[5]=(void*)DDM_arr;
 ARGV[6]=(void*)fzeta_arr;
 ARGV[7]=(void*)Tzeta_arr;
 ARGV[8]=(void*)zeta_arr;
 ARGV[9]=(void*)RL; 

 return GET_MW(10, ARGV);
}


#ifndef LINUX
extern "C" __declspec(dllexport) int PyGET_MW(int *Lparms, double *Rparms, double *Parms,
                                              double *T_arr,double *DEM_arr, double *DDM_arr,
                                              double *RL)
#else
extern "C" int PyGET_MW(int *Lparms, double *Rparms, double *Parms,
                        double *T_arr,double *DEM_arr, double *DDM_arr,
                        double *RL)
#endif
{
 void *ARGV[7];
 ARGV[0]=(void*)Lparms;
 ARGV[1]=(void*)Rparms;
 ARGV[2]=(void*)Parms;
 ARGV[3]=(void*)T_arr;
 ARGV[4]=(void*)DEM_arr;
 ARGV[5]=(void*)DDM_arr;
 ARGV[6]=(void*)RL; 

 return GET_MW(7, ARGV);
}
