#pragma once
#define me 9.1093837015e-28
#define c 2.99792458e10
#define e (1.602176634e-19*c/10)
#define kB 1.380649e-16
#define hPl 6.62607015e-27 //Planck constant h (without bar)
#define hPlbar (hPl/2/M_PI) //Planck constant h_bar
#define em_alpha (1.0/137.035999084) 
#define AU 1.495978707e13 
#define sfu 1e-19
#define ieH 2.1798718e-11 //hydrogen ionization energy (theoretical), erg
#define ieHe12 3.9393356e-11 //helium 1st ionization energy, erg
#define ieHe2 8.71830663945283224e-11 //helium 2nd ionization energy, erg

void FindPlasmaDispersion(double f, double f_p, double f_B, double theta, int sigma,
	                      double *N, double *FZh, double *L, double *T, double *st_out, double *ct_out);
double SahaH(double n0, double T0);
void FindIonizationsSolar(double n0, double T0, double *n_e, double *n_H, double *n_He);