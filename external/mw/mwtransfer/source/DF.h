#include <stdlib.h>

class DF
{
 public:
 int OK, dfzero;
 int N_intervals;
 double *E_x;
 int *logscale;
 double n_b, EPS_mu0;
 virtual void Fp(double p, double p_z, double p_n,
	             double *f, double *df_dp, double *df_dalpha)=0;
 virtual void FE(double E, double mu, 
	             double *f, double *df_dE, double *df_dmu, 
				 double *g1, double *g2)=0;
 DF() 
 {
  OK=0;
  E_x=0;
  logscale=0;
 }
 virtual ~DF() 
 {
  if (E_x) free(E_x);
  if (logscale) free(logscale);
 }
};