#pragma once

#define InSize 15
#define OutSize 7
#define RpSize 3
#define RpSize1 9
#define GpSize 7

int MW_Transfer(int *Lparms, double *Rparms, double *Parms, double *T_arr, double *DEM_arr, double *DDM_arr, double *RL, 
	            int AZ_on, double *fZ_arr, double *TZ_arr, double *Z_arr, int *srange, double *GRparms);
