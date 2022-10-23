#include <stdlib.h>
#include <math.h>
#include "ExtMath.h"

void DEM_moments(double *T_arr, double *lnT_arr, double *DEM_arr, int N, double *n_avg, double *T_avg)
{
 double *y1=(double*)malloc(sizeof(double)*N);
 double *y2=(double*)malloc(sizeof(double)*N);

 for (int i=0; i<N; i++)
 {
  y1[i]=DEM_arr[i]*T_arr[i];
  y2[i]=y1[i]*T_arr[i];
 }

 double n2_avg=IntTabulated(lnT_arr, y1, N);
 *n_avg=sqrt(n2_avg);
 *T_avg=(n2_avg>0) ? IntTabulated(lnT_arr, y2, N)/n2_avg : 0.0;

 free(y1);
 free(y2);
}

void DDM_moments(double *T_arr, double *lnT_arr, double *DDM_arr, int N, double *n_avg, double *T_avg)
{
 double *y1=(double*)malloc(sizeof(double)*N);
 double *y2=(double*)malloc(sizeof(double)*N);

 for (int i=0; i<N; i++)
 {
  y1[i]=DDM_arr[i]*T_arr[i];
  y2[i]=y1[i]*T_arr[i];
 }

 *n_avg=IntTabulated(lnT_arr, y1, N);
 *T_avg=(*n_avg>0) ? IntTabulated(lnT_arr, y2, N)/(*n_avg) : 0.0;

 free(y1);
 free(y2);
}