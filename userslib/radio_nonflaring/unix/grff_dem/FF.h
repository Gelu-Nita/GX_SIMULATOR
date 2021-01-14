#pragma once
void FindFF_single(double f, double theta, int sigma, double f_p, double f_B, double T0, double n_e, int ABcode,
                   double *j, double *k);
void FindFF_DEM_XO(double f, double theta, double f_p, double f_B, double *T_arr, double *lnT_arr, double *DEM_arr, int NT, int ABcode, 
                   double *jX, double *kX, double *jO, double *kO);