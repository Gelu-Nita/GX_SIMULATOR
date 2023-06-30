#pragma once
void FindGR_single(double f, double theta, int sigma, int s, double f_p, double f_B, double n_e, double T0, double LB, 
	               double *tau, double *I0);
void FindGR_DDM_XO(double f, double theta, int s, double f_p, double f_B, double *T_arr, double *lnT_arr, double *DDM_arr, int NT, double LB, 
	               double *tauX, double *I0X, double *tauO, double *I0O);
