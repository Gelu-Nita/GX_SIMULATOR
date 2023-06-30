#pragma once
void FindFF_single(double f, double theta, int sigma, double f_p, double f_B, double T0, double n_e, int ABcode,
                   int AZ_on, int NfZ, int NTZ, double *fZ_arr, double *TZ_arr, double *Z_arr,
                   double *j, double *k);
void FindFF_DEM_XO(double f, double theta, double f_p, double f_B, double *T_arr, double *lnT_arr, double *DEM_arr, int NT, int ABcode, 
                   int AZ_on, int NfZ, int NTZ, double *fZ_arr, double *TZ_arr, double *Z_arr,
                   double *jX, double *kX, double *jO, double *kO);
