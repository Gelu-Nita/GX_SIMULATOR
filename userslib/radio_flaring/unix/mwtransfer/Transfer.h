class Ray
{
 int Npoints;
 int Closed, spl_on;
 double zmin, zmax;
 double *z_arr, *dz_arr, *n0_arr, *Bx_arr, *By_arr, *Bz_arr;
 double *jX_arr, *jO_arr, *kX_arr, *kO_arr;
 double *n0_2arr, *Bx_2arr, *By_2arr, *Bz_2arr;
 double *jX_2arr, *jO_2arr, *kX_2arr, *kO_2arr;
 void RayM(int N, double *z_in, double *n0, double *B, double *theta, double *psi, 
	       double *jX, double *jO, double *kX, double *kO, int clos, int spline_on);
 public:
 int OK;
 double tauXtotal, tauOtotal;
 Ray(int N, double *z_in, double *n0, double *B, double *theta, double *psi, 
	 double *jX, double *jO, double *kX, double *kO, int clos, int spline_on);
 ~Ray();
 int RadiationTransferExact(double nu, double *S_in, double Imax, double MinStep, 
	                        double *S_out);
 void RadiationTransferRL(double nu, double R_in, double L_in, double *RL_out);
 void GetLocalParms(double z, double *n0, double *Bx, double *By, double *Bz, 
	                          double *jX, double *jO, double *kX, double *kO);
};