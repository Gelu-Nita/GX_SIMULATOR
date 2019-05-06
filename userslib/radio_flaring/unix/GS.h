class EmWave
{
 public:
 int Valid, sigma;
 double nu, nu_p, nu_B, theta;
 double ct, st;
 double y, N, N_z, T, L, Zfactor;
 EmWave(double nu, double theta, int sigma, double nu_p, double nu_B, double cst_min);
};

void GS_jk(EmWave *w, DF *df, int ExactBessel, double *j, double *k);
void GS_jk_approx(EmWave *w, DF *df, int Npoints, int Q_on, double *j, double *k);
void FF_jk(EmWave *w, double n0, double T0, double *j, double *k);

void GS_jk_MultiDF(EmWave *w, DF **mdf, int ExactBessel, double *j, double *k);
void GS_jk_approx_MultiDF(EmWave *w, DF **mdf, int Npoints, int Q_on, double *j, double *k);

#define ERR_i 1e-5
#define ERR_s 1e-5
#define S_MIN 1
#define S_MAX 1000000
#define S_MAX_LOCAL 1000000
#define T_MAX 3600.0
#define CST_MIN_DEFAULT 1e-3