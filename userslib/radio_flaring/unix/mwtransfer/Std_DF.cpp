#include <math.h>
#include <float.h>
#include "ExtMath.h"
#include "Astrophys.h"
#include "Std_DF.h"
#include "Messages.h"

#define InSizeF 33

class FFFdf : public Std_DF_energy  //1
{
 public:
 void FE(double E, double *f, double *f_E){};
 void Fp(double p, double *f, double *f_p){};
 FFFdf();
};

FFFdf :: FFFdf()
{
 OK=1;
 dfzero=1;
 n_b=0.0;
 N_intervals=0;
}

//----------------------------------------------------------------------

class THMdf : public Std_DF_energy //2
{
 double A, theta;
 public:
 void FE(double E, double *f, double *f_E);
 void Fp(double p, double *f, double *f_p);
 THMdf(double n0, double *ParmIn);
};

void THMdf :: FE(double E, double *f, double *f_E)
{
 double G=E/mc2+1.0;
 double p=mc*sqrt(sqr(G)-1.0);

 double fp, dfp_dp;
 Fp(p, &fp, &dfp_dp);

 *f=fp*p*m*G;
 *f_E=sqr(m*G)*(dfp_dp+fp/p)+fp*p/(c*c);
}

void THMdf :: Fp(double p, double *f, double *f_p)
{
 double G=sqrt(1.0+sqr(p/mc));
 *f=A/(mc*mc*mc)*exp((1.0-G)/theta);
 *f_p=-(*f)*p/G/theta/(mc*mc);
}

THMdf :: THMdf(double n0, double *ParmIn)
{
 double T0=fabs(ParmIn[2]);

 theta=kB*T0/mc2;
 A=n0/(2.0*M_PI*theta*ExpBesselK(2, 1.0/theta));

 N_intervals=3;
 E_x[0]=0.0;
 E_x[1]=kB*T0;
 E_x[2]=kB*T0*3;
 E_x[3]=kB*T0*710;
 logscale[0]=0;
 logscale[1]=0;
 logscale[2]=1;

 OK=finite(A) && A>=0;
 dfzero=(A==0.0);
 n_b=0.0;
}

//----------------------------------------------------------------------

class PLWdf : public Std_DF_energy //3
{
 double A, delta;
 public:
 void FE(double E, double *f, double *f_E);
 void Fp(double p, double *f, double *f_p);
 PLWdf(double *ParmIn);
};

void PLWdf :: FE(double E, double *f, double *f_E)
{
 *f=A*pow(E, -delta);
 *f_E=(-delta/E)*(*f);
}

void PLWdf :: Fp(double p, double *f, double *f_p)
{
 double G=sqrt(1.0+sqr(p/mc));
 double E=mc2*(G-1.0);

 double fE, dfE_dE;
 FE(E, &fE, &dfE_dE);

 *f=fE/(p*m*G);
 *f_p=(dfE_dE-fE*G*m/sqr(p)*(1.0+sqr(p/G/mc)))/(sqr(m*G));
}

PLWdf :: PLWdf(double *ParmIn)
{
 double E1=ParmIn[6];
 double E2=ParmIn[7];
 delta=ParmIn[9];
 n_b=ParmIn[12];

 A=n_b/(2.0*M_PI)*(delta-1.0)/(pow(E1, 1.0-delta)-pow(E2, 1.0-delta));

 N_intervals=1;
 E_x[0]=E1;
 E_x[1]=E2;
 logscale[0]=1;

 OK=finite(A) && A>=0.0 && E2>E1;
 dfzero=(n_b==0.0);
}

//----------------------------------------------------------------------------

class DPLdf : public Std_DF_energy //4
{
 double Ebr, A1, A2, delta1, delta2;
 public:
 void FE(double E, double *f, double *f_E);
 void Fp(double p, double *f, double *f_p);
 DPLdf(double *ParmIn);
};

void DPLdf :: FE(double E, double *f, double *f_E)
{
 if (E<Ebr)
 {
  *f=A1*pow(E, -delta1);
  *f_E=(-delta1/E)*(*f);
 }
 else
 {
  *f=A2*pow(E, -delta2);
  *f_E=(-delta2/E)*(*f);
 }
}

void DPLdf :: Fp(double p, double *f, double *f_p)
{
 double G=sqrt(1.0+sqr(p/mc));
 double E=mc2*(G-1.0);

 double fE, dfE_dE;
 FE(E, &fE, &dfE_dE);

 *f=fE/(p*m*G);
 *f_p=(dfE_dE-fE*G*m/sqr(p)*(1.0+sqr(p/G/mc)))/(sqr(m*G));
}

DPLdf :: DPLdf(double *ParmIn)
{
 double E1=ParmIn[6];
 double E2=ParmIn[7];
 Ebr=ParmIn[8];
 delta1=ParmIn[9];
 delta2=ParmIn[10];
 n_b=ParmIn[12];

 A1=n_b/(2.0*M_PI)/
	((pow(E1, 1.0-delta1)-pow(Ebr, 1.0-delta1))/(delta1-1.0)+
	 pow(Ebr, delta2-delta1)*(pow(Ebr, 1.0-delta2)-pow(E2, 1.0-delta2))/(delta2-1.0));
 A2=A1*pow(Ebr, delta2-delta1);

 N_intervals=2;
 E_x[0]=E1;
 E_x[1]=Ebr;
 E_x[2]=E2;
 logscale[0]=1;
 logscale[1]=1;

 OK=finite(A1) && A1>=0.0 && finite(A2) && A2>=0.0 && E2>Ebr && Ebr>E1;
 dfzero=(n_b==0.0);
}

//----------------------------------------------------------------------------

class TNTdf : public Std_DF_energy //5
{
 double Ecr, pcr;
 THMdf *thm;
 PLWdf *plw;
 public:
 void FE(double E, double *f, double *f_E);
 void Fp(double p, double *f, double *f_p);
 TNTdf(double n0, double *ParmIn);
 ~TNTdf();
};

void TNTdf :: FE(double E, double *f, double *f_E)
{
 if (E<Ecr) thm->FE(E, f, f_E);
 else if (plw) plw->FE(E, f, f_E);
 else thm->FE(E, f, f_E);
}

void TNTdf :: Fp(double p, double *f, double *f_p)
{
 if (p<pcr) thm->Fp(p, f, f_p);
 else if (plw) plw->Fp(p, f, f_p);
 else thm->Fp(p, f, f_p);
}

TNTdf :: TNTdf(double n0, double *ParmIn)
{
 double T0=fabs(ParmIn[2]);
 double eps=ParmIn[3];
 double E_max=ParmIn[7];
 double delta=ParmIn[9];

 double Eth=kB*T0;
 double Gth=Eth/mc2+1.0;
 double pth=mc*sqrt(sqr(Gth)-1.0);

 pcr=pth/sqrt(eps);
 double Gcr=sqrt(1.0+sqr(pcr/mc));
 Ecr=mc2*(Gcr-1.0);
 
 thm=new THMdf(n0, ParmIn);
 if (E_max>Ecr)
 {
  double fcr, dfcr_dE;
  thm->FE(Ecr, &fcr, &dfcr_dE);
  double Acr=fcr*pow(Ecr, delta);
  n_b=Acr*2.0*M_PI/(delta-1.0)*(pow(Ecr, 1.0-delta)-pow(E_max, 1.0-delta));

  double ParmLoc[InSizeF];
  ParmLoc[6]=Ecr;
  ParmLoc[7]=E_max;
  ParmLoc[9]=delta;
  ParmLoc[12]=n_b;
  plw=new PLWdf(ParmLoc);

  OK=thm->OK && plw->OK;

  E_x[0]=0.0;
  if (Ecr<=(kB*T0))
  {
   N_intervals=2;
   E_x[1]=Ecr;
   E_x[2]=E_max;
   logscale[0]=0;
   logscale[1]=1;
  }
  else if (Ecr<=(kB*T0*3))
  {
   N_intervals=3;
   E_x[1]=kB*T0;
   E_x[2]=Ecr;
   E_x[3]=E_max;
   logscale[0]=logscale[1]=0;
   logscale[2]=1;
  }
  else
  {
   N_intervals=4;
   E_x[1]=kB*T0;
   E_x[2]=kB*T0*3;
   E_x[3]=Ecr;
   E_x[4]=E_max;
   logscale[0]=logscale[1]=0;
   logscale[2]=(E_x[3]/E_x[2]>5.0);
   logscale[3]=1;
  }
 }
 else
 {
  plw=0;
  n_b=0.0;

  OK=thm->OK;

  E_x[0]=0.0;
  if (E_max<=(kB*T0))
  {
   N_intervals=1;
   E_x[1]=E_max;
   logscale[0]=0;
  }
  else if (E_max<=(kB*T0*3))
  {
   N_intervals=2;
   E_x[1]=kB*T0;
   E_x[2]=E_max;
   logscale[0]=logscale[1]=0;
  }
  else
  {
   N_intervals=3;
   E_x[1]=kB*T0;
   E_x[2]=kB*T0*3;
   E_x[3]=E_max;
   logscale[0]=logscale[1]=0;
   logscale[2]=(E_x[3]/E_x[2]>5.0);
  }
 }

 dfzero=thm->dfzero;
}

TNTdf :: ~TNTdf()
{
 if (thm) delete thm;
 if (plw) delete plw;
}

//----------------------------------------------------------------------------

class KAPdf : public Std_DF_energy //6
{
 double A, kappa_p1, kappa_m32_theta;
 public:
 void FE(double E, double *f, double *f_E);
 void Fp(double p, double *f, double *f_p);
 KAPdf(double n0, double *ParmIn);
};

void KAPdf :: FE(double E, double *f, double *f_E)
{
 double G=E/mc2+1.0;
 double p=mc*sqrt(sqr(G)-1.0);

 double fp, dfp_dp;
 Fp(p, &fp, &dfp_dp);

 *f=fp*p*m*G;
 *f_E=sqr(m*G)*(dfp_dp+fp/p)+fp*p/(c*c);
}

void KAPdf :: Fp(double p, double *f, double *f_p)
{
 double G=sqrt(1.0+sqr(p/mc));
 double D=1.0+(G-1.0)/kappa_m32_theta;
 *f=A/(mc*mc*mc)*pow(D, -kappa_p1);
 *f_p=-kappa_p1*(*f)/D/kappa_m32_theta*p/G/(mc*mc);
}

class KappaIntegrand : public IntegrableFunction
{
 public:
 double kappa_m32_theta, kappa_p1;
 double F(double G);
};

double KappaIntegrand :: F(double G)
{
 return G*sqrt(sqr(G)-1.0)*pow(1.0+(G-1.0)/kappa_m32_theta, -kappa_p1);
}

KAPdf :: KAPdf(double n0, double *ParmIn)
{
 double T0=fabs(ParmIn[2]);
 double kappa=ParmIn[4];
 double E_max=ParmIn[7];

 double theta=kB*T0/mc2;
 kappa_p1=kappa+1.0;
 kappa_m32_theta=(kappa-1.5)*theta;

 double G_max=E_max/mc2+1.0;
 KappaIntegrand ki;
 ki.kappa_m32_theta=kappa_m32_theta;
 ki.kappa_p1=kappa_p1;
 int err;
 A=n0/(2.0*M_PI)/qromb(&ki, 1.0, G_max, 1e-6, &err);

 E_x[0]=0.0;
 if (E_max<=(kB*T0))
 {
  N_intervals=1;
  E_x[1]=E_max;
  logscale[0]=0;
 }
 else if (E_max<=(kB*T0*3))
 {
  N_intervals=2;
  E_x[1]=kB*T0;
  E_x[2]=E_max;
  logscale[0]=logscale[1]=0;
 }
 else
 {
  N_intervals=3;
  E_x[1]=kB*T0;
  E_x[2]=kB*T0*3;
  E_x[3]=E_max;
  logscale[0]=logscale[1]=0;
  logscale[2]=(E_x[3]/E_x[2]>5.0);
 }

 OK=finite(A) && A>=0.0;
 dfzero=(A==0.0);
 n_b=0.0;
}

//----------------------------------------------------------------------------

class PLPdf : public Std_DF_energy //7
{
 double A, delta;
 public:
 void FE(double E, double *f, double *f_E);
 void Fp(double p, double *f, double *f_p);
 PLPdf(double *ParmIn);
};

void PLPdf :: FE(double E, double *f, double *f_E)
{
 double G=E/mc2+1.0;
 double p=mc*sqrt(sqr(G)-1.0);

 double fp, dfp_dp;
 Fp(p, &fp, &dfp_dp);

 *f=fp*p*m*G;
 *f_E=sqr(m*G)*(dfp_dp+fp/p)+fp*p/(c*c);
}

void PLPdf :: Fp(double p, double *f, double *f_p)
{
 *f=A*pow(p, -delta);
 *f_p=-delta*(*f)/p;
}

PLPdf :: PLPdf(double *ParmIn)
{
 double E1=ParmIn[6];
 double E2=ParmIn[7];
 delta=ParmIn[9];
 n_b=ParmIn[12];

 double p1=mc*sqrt(sqr(E1/mc2+1.0)-1.0);
 double p2=mc*sqrt(sqr(E2/mc2+1.0)-1.0);

 A=n_b/(2.0*M_PI)*(delta-3.0)/(pow(p1, 3.0-delta)-pow(p2, 3.0-delta));

 N_intervals=1;
 E_x[0]=E1;
 E_x[1]=E2;
 logscale[0]=1;

 OK=finite(A) && A>=0.0 && E2>E1;
 dfzero=(n_b==0.0);
}

//----------------------------------------------------------------------------

class PLGdf : public Std_DF_energy //8
{
 double A, delta;
 public:
 void FG(double G, double *f, double *f_G);
 void FE(double E, double *f, double *f_E);
 void Fp(double p, double *f, double *f_p);
 PLGdf(double *ParmIn);
};

void PLGdf :: FG(double G, double *f, double *f_G)
{
 *f=A*pow(G, -delta);
 *f_G=-delta*(*f)/G;
}

void PLGdf :: FE(double E, double *f, double *f_E)
{
 double G=E/mc2+1.0;

 double fG, dfG_dG;
 FG(G, &fG, &dfG_dG);

 *f=fG/mc2;
 *f_E=dfG_dG/(mc2*mc2);
}

void PLGdf :: Fp(double p, double *f, double *f_p)
{
 double G=sqrt(1.0+sqr(p/mc));

 double fG, dfG_dG;
 FG(G, &fG, &dfG_dG);

 *f=fG/(p*G*mc*mc);
 *f_p=(dfG_dG-fG/G*(sqr(G*mc/p)+1.0))/sqr(G*mc*mc);
}

PLGdf :: PLGdf(double *ParmIn)
{
 double E1=ParmIn[6];
 double E2=ParmIn[7];
 delta=ParmIn[9];
 n_b=ParmIn[12];

 double G1=E1/mc2+1.0;
 double G2=E2/mc2+1.0;

 A=n_b/(2.0*M_PI)*(delta-1.0)/(pow(G1, 1.0-delta)-pow(G2, 1.0-delta));

 N_intervals=1;
 E_x[0]=E1;
 E_x[1]=E2;
 logscale[0]=1;

 OK=finite(A) && A>=0.0 && E2>E1;
 dfzero=(n_b==0.0);
}

//----------------------------------------------------------------------------

class TNPdf : public Std_DF_energy //9
{
 double Ecr, pcr;
 THMdf *thm;
 PLPdf *plp;
 public:
 void FE(double E, double *f, double *f_E);
 void Fp(double p, double *f, double *f_p);
 TNPdf(double n0, double *ParmIn);
 ~TNPdf();
};

void TNPdf :: FE(double E, double *f, double *f_E)
{
 if (E<Ecr) thm->FE(E, f, f_E);
 else if (plp) plp->FE(E, f, f_E);
 else thm->FE(E, f, f_E);
}

void TNPdf :: Fp(double p, double *f, double *f_p)
{
 if (p<pcr) thm->Fp(p, f, f_p);
 else if (plp) plp->Fp(p, f, f_p);
 else thm->Fp(p, f, f_p);
}

TNPdf :: TNPdf(double n0, double *ParmIn)
{
 double T0=fabs(ParmIn[2]);
 double eps=ParmIn[3];
 double E_max=ParmIn[7];
 double delta=ParmIn[9];

 double Eth=kB*T0;
 double Gth=Eth/mc2+1.0;
 double pth=mc*sqrt(sqr(Gth)-1.0);

 pcr=pth/sqrt(eps);
 double Gcr=sqrt(1.0+sqr(pcr/mc));
 Ecr=mc2*(Gcr-1.0);

 double G_max=E_max/mc2+1.0;
 double p_max=mc*sqrt(sqr(G_max)-1.0);
 
 thm=new THMdf(n0, ParmIn);
 if (E_max>Ecr)
 {
  double fcr, dfcr_dp;
  thm->Fp(pcr, &fcr, &dfcr_dp);
  double Acr=fcr*pow(pcr, delta);
  n_b=Acr*2.0*M_PI/(delta-3.0)*(pow(pcr, 3.0-delta)-pow(p_max, 3.0-delta));

  double ParmLoc[InSizeF];
  ParmLoc[6]=Ecr;
  ParmLoc[7]=E_max;
  ParmLoc[9]=delta;
  ParmLoc[12]=n_b;
  plp=new PLPdf(ParmLoc);

  OK=thm->OK && plp->OK;

  E_x[0]=0.0;
  if (Ecr<=(kB*T0))
  {
   N_intervals=2;
   E_x[1]=Ecr;
   E_x[2]=E_max;
   logscale[0]=0;
   logscale[1]=1;
  }
  else if (Ecr<=(kB*T0*3))
  {
   N_intervals=3;
   E_x[1]=kB*T0;
   E_x[2]=Ecr;
   E_x[3]=E_max;
   logscale[0]=logscale[1]=0;
   logscale[2]=1;
  }
  else
  {
   N_intervals=4;
   E_x[1]=kB*T0;
   E_x[2]=kB*T0*3;
   E_x[3]=Ecr;
   E_x[4]=E_max;
   logscale[0]=logscale[1]=0;
   logscale[2]=(E_x[3]/E_x[2]>5.0);
   logscale[3]=1;
  }
 }
 else
 {
  plp=0;
  n_b=0.0;

  OK=thm->OK;

  E_x[0]=0.0;
  if (E_max<=(kB*T0))
  {
   N_intervals=1;
   E_x[1]=E_max;
   logscale[0]=0;
  }
  else if (E_max<=(kB*T0*3))
  {
   N_intervals=2;
   E_x[1]=kB*T0;
   E_x[2]=E_max;
   logscale[0]=logscale[1]=0;
  }
  else
  {
   N_intervals=3;
   E_x[1]=kB*T0;
   E_x[2]=kB*T0*3;
   E_x[3]=E_max;
   logscale[0]=logscale[1]=0;
   logscale[2]=(E_x[3]/E_x[2]>5.0);
  }
 }

 dfzero=thm->dfzero;
}

TNPdf :: ~TNPdf()
{
 if (thm) delete thm;
 if (plp) delete plp;
}

//----------------------------------------------------------------------------

class TNGdf : public Std_DF_energy //10
{
 double Ecr, pcr;
 THMdf *thm;
 PLGdf *plg;
 public:
 void FE(double E, double *f, double *f_E);
 void Fp(double p, double *f, double *f_p);
 TNGdf(double n0, double *ParmIn);
 ~TNGdf();
};

void TNGdf :: FE(double E, double *f, double *f_E)
{
 if (E<Ecr) thm->FE(E, f, f_E);
 else if (plg) plg->FE(E, f, f_E);
 else thm->FE(E, f, f_E);
}

void TNGdf :: Fp(double p, double *f, double *f_p)
{
 if (p<pcr) thm->Fp(p, f, f_p);
 else if (plg) plg->Fp(p, f, f_p);
 else thm->Fp(p, f, f_p);
}

TNGdf :: TNGdf(double n0, double *ParmIn)
{
 double T0=fabs(ParmIn[2]);
 double eps=ParmIn[3];
 double E_max=ParmIn[7];
 double delta=ParmIn[9];

 double Eth=kB*T0;
 double Gth=Eth/mc2+1.0;
 double pth=mc*sqrt(sqr(Gth)-1.0);

 pcr=pth/sqrt(eps);
 double Gcr=sqrt(1.0+sqr(pcr/mc));
 Ecr=mc2*(Gcr-1.0);

 double G_max=E_max/mc2+1.0;
 
 thm=new THMdf(n0, ParmIn);
 if (E_max>Ecr)
 {
  double fcr, dfcr_dE;
  thm->FE(Ecr, &fcr, &dfcr_dE);
  double Acr=fcr*mc2*pow(Gcr, delta);
  n_b=Acr*2.0*M_PI/(delta-1.0)*(pow(Gcr, 1.0-delta)-pow(G_max, 1.0-delta));

  double ParmLoc[InSizeF];
  ParmLoc[6]=Ecr;
  ParmLoc[7]=E_max;
  ParmLoc[9]=delta;
  ParmLoc[12]=n_b;
  plg=new PLGdf(ParmLoc);

  OK=thm->OK && plg->OK;

  E_x[0]=0.0;
  if (Ecr<=(kB*T0))
  {
   N_intervals=2;
   E_x[1]=Ecr;
   E_x[2]=E_max;
   logscale[0]=0;
   logscale[1]=1;
  }
  else if (Ecr<=(kB*T0*3))
  {
   N_intervals=3;
   E_x[1]=kB*T0;
   E_x[2]=Ecr;
   E_x[3]=E_max;
   logscale[0]=logscale[1]=0;
   logscale[2]=1;
  }
  else
  {
   N_intervals=4;
   E_x[1]=kB*T0;
   E_x[2]=kB*T0*3;
   E_x[3]=Ecr;
   E_x[4]=E_max;
   logscale[0]=logscale[1]=0;
   logscale[2]=(E_x[3]/E_x[2]>5.0);
   logscale[3]=1;
  }
 }
 else
 {
  plg=0;
  n_b=0.0;

  OK=thm->OK;

  E_x[0]=0.0;
  if (E_max<=(kB*T0))
  {
   N_intervals=1;
   E_x[1]=E_max;
   logscale[0]=0;
  }
  else if (E_max<=(kB*T0*3))
  {
   N_intervals=2;
   E_x[1]=kB*T0;
   E_x[2]=E_max;
   logscale[0]=logscale[1]=0;
  }
  else
  {
   N_intervals=3;
   E_x[1]=kB*T0;
   E_x[2]=kB*T0*3;
   E_x[3]=E_max;
   logscale[0]=logscale[1]=0;
   logscale[2]=(E_x[3]/E_x[2]>5.0);
  }
 }

 dfzero=thm->dfzero;
}

TNGdf :: ~TNGdf()
{
 if (thm) delete thm;
 if (plg) delete plg;
}

//----------------------------------------------------------------------------

class NDSdf : public Std_DF_energy //50
{
 double A, kT, n2;
 public:
 void FE(double E, double *f, double *f_E);
 void Fp(double p, double *f, double *f_p);
 NDSdf(double n0, double *ParmIn);
};

void NDSdf :: FE(double E, double *f, double *f_E)
{
 double Q=E/kT;
 *f=A*pow(Q, n2)*exp(-Q);
 *f_E=(*f)/kT*(n2/Q-1.0); 
}

void NDSdf :: Fp(double p, double *f, double *f_p)
{
 double G=sqrt(1.0+sqr(p/mc));
 double E=mc2*(G-1.0);

 double fE, dfE_dE;
 FE(E, &fE, &dfE_dE);

 *f=fE/(p*m*G);
 *f_p=(dfE_dE-fE*G*m/sqr(p)*(1.0+sqr(p/G/mc)))/(sqr(m*G));
}

class NDSxFunction : public IntegrableFunction
{
 public:
 double n2, fx;
 double F(double q);
};

double NDSxFunction :: F(double q)
{
 return (q ? pow(q, n2) : 0.0)*exp(-q)-fx;
}

void FindNDSx(double n2, double *x1, double *x2)
{
 NDSxFunction nf;
 nf.n2=n2;
 nf.fx=pow(n2, n2)*exp(-n2)*0.1;

 *x1=BrentRoot(&nf, 0.0, n2,    1e-3);
 *x2=BrentRoot(&nf, n2,  710.0, 1e-3);
}

NDSdf :: NDSdf(double n0, double *ParmIn)
{
 double T0=ParmIn[2];
 double n=ParmIn[4];
 
 n2=n/2;
 kT=kB*T0;
 A=n0/(2.0*M_PI*kT*Gamma(n2+1.0));

 double x1, x2;
 FindNDSx(n2, &x1, &x2);

 if (x1>0.1)
 {
  N_intervals=3;
  E_x[0]=0.0;
  E_x[1]=kT*x1;
  E_x[2]=kT*x2;
  E_x[3]=kT*710;
  logscale[0]=0;
  logscale[1]=0;
  logscale[2]=1;
 }
 else
 {
  N_intervals=2;
  E_x[0]=0.0;
  E_x[1]=kT*x2;
  E_x[2]=kT*710;
  logscale[0]=0;
  logscale[1]=1;
 }

 OK=finite(A) && A>=0.0 && finite(x1) && finite(x2);
 dfzero=(A==0.0); 
 n_b=0.0;
}

//----------------------------------------------------------------------------

class ISOdf : public Std_DF_angle //0, 1
{
 public:
 void Falpha(double mu, double sa, double *f, double *f_alpha);
 void Fmu(double mu, double *f, double *f_mu, double *g1, double *g2);
 double g1short(double mu);
 ISOdf();
};

void ISOdf :: Falpha(double mu, double sa, double *f, double *f_alpha)
{
 *f=0.5;
 *f_alpha=0.0;
}

void ISOdf :: Fmu(double mu, double *f, double *f_mu, double *g1, double *g2)
{
 *f=0.5;
 *f_mu=0.0;
 if (g1) *g1=0.0;
 if (g2) *g2=0.0;
}

double ISOdf :: g1short(double mu)
{
 return 0.0;
}

ISOdf :: ISOdf()
{
 OK=1;
}

//----------------------------------------------------------------------------

class EXPdf : public Std_DF_angle //2
{
 double B, alpha_c, mu_c, dmu;
 public:
 void Falpha(double mu, double sa, double *f, double *f_alpha);
 void Fmu(double mu, double *f, double *f_mu, double *g1, double *g2);
 double g1short(double mu);
 EXPdf(double *ParmIn);
};

void EXPdf :: Falpha(double mu, double sa, double *f, double *f_alpha)
{
 Fmu(mu, f, f_alpha, 0, 0);
 *f_alpha*=(-sa);
}

void EXPdf :: Fmu(double mu, double *f, double *f_mu, double *g1, double *g2)
{
 double amu=fabs(mu);
 double g1loc;
 if (amu<mu_c)
 {
  *f=B;
  *f_mu=0.0;
  if (g1) *g1=0.0;
  if (g2) *g2=0.0;
 }
 else
 {
  *f=B*exp(-(amu-mu_c)/dmu);
  g1loc=-sign(mu)/dmu;
  *f_mu=*f*g1loc;
  if (g1) *g1=g1loc;
  if (g2) *g2=1.0/sqr(dmu);
 }
}

double EXPdf :: g1short(double mu)
{
 double amu=fabs(mu);
 if (amu<mu_c) return 0.0;
 else return -sign(mu)/dmu;
}

EXPdf :: EXPdf(double *ParmIn)
{
 alpha_c=ParmIn[20];
 dmu=ParmIn[22];

 mu_c=fabs(cos(alpha_c));
 B=0.5/(mu_c+dmu-dmu*exp((mu_c-1.0)/dmu));

 EPS_mu0=min(EPS_mu0, dmu/30);
 OK=finite(B) && B>0.0;
}

//----------------------------------------------------------------------------

class GAUdf : public Std_DF_angle //3
{
 double B, alpha_c, mu_c, dmu;
 public:
 void Falpha(double mu, double sa, double *f, double *f_alpha);
 void Fmu(double mu, double *f, double *f_mu, double *g1, double *g2);
 double g1short(double mu);
 GAUdf(double *ParmIn);
};

void GAUdf :: Falpha(double mu, double sa, double *f, double *f_alpha)
{
 Fmu(mu, f, f_alpha, 0, 0);
 *f_alpha*=(-sa);
}

void GAUdf :: Fmu(double mu, double *f, double *f_mu, double *g1, double *g2)
{
 double amu=fabs(mu);
 double g1loc;
 if (amu<mu_c)
 {
  *f=B;
  *f_mu=0.0;
  if (g1) *g1=0.0;
  if (g2) *g2=0.0;
 }
 else
 {
  *f=B*exp(-sqr((amu-mu_c)/dmu));
  g1loc=-2.0*(amu-mu_c)/sqr(dmu)*sign(mu);
  *f_mu=*f*g1loc;
  if (g1) *g1=g1loc;
  if (g2) *g2=4.0*sqr((amu-mu_c)/sqr(dmu))-2.0/sqr(dmu);
 }
}

double GAUdf :: g1short(double mu)
{
 double amu=fabs(mu);
 if (amu<mu_c) return 0.0;
 else return -2.0*(amu-mu_c)/sqr(dmu)*sign(mu);
}

GAUdf :: GAUdf(double *ParmIn)
{
 alpha_c=ParmIn[20];
 dmu=ParmIn[22];

 mu_c=fabs(cos(alpha_c));
 B=0.5/(mu_c+dmu*sqrt(M_PI)/2*Erf((1.0-mu_c)/dmu));

 EPS_mu0=min(EPS_mu0, sqr(dmu)/30);
 OK=finite(B) && B>0.0;
}

//----------------------------------------------------------------------------

class GABdf : public Std_DF_angle //4
{
 double B, alpha_c, mu_c, dmu;
 public:
 void Falpha(double mu, double sa, double *f, double *f_alpha);
 void Fmu(double mu, double *f, double *f_mu, double *g1, double *g2);
 double g1short(double mu);
 GABdf(double *ParmIn);
};

void GABdf :: Falpha(double mu, double sa, double *f, double *f_alpha)
{
 Fmu(mu, f, f_alpha, 0, 0);
 *f_alpha*=(-sa);
}

void GABdf :: Fmu(double mu, double *f, double *f_mu, double *g1, double *g2)
{
 *f=B*exp(-sqr((mu-mu_c)/dmu));
 double g1loc=-2.0*(mu-mu_c)/sqr(dmu);
 *f_mu=*f*g1loc;
 if (g1) *g1=g1loc;
 if (g2) *g2=4.0*sqr((mu-mu_c)/sqr(dmu))-2.0/sqr(dmu);
}

double GABdf :: g1short(double mu)
{
 return -2.0*(mu-mu_c)/sqr(dmu);
}

GABdf :: GABdf(double *ParmIn)
{
 alpha_c=ParmIn[21];
 dmu=ParmIn[22];

 mu_c=cos(alpha_c);
 B=2.0/(sqrt(M_PI)*dmu)/(Erf((1.0-mu_c)/dmu)+Erf((1.0+mu_c)/dmu));

 EPS_mu0=min(EPS_mu0, sqr(dmu)/30);
 OK=finite(B) && B>0.0;
}

//----------------------------------------------------------------------------

class SGAdf : public Std_DF_angle //5
{
 double B, alpha_c, mu_c, dmu, a4;
 public:
 void Falpha(double mu, double sa, double *f, double *f_alpha);
 void Fmu(double mu, double *f, double *f_mu, double *g1, double *g2);
 double g1short(double mu);
 SGAdf(double *ParmIn);
};

void SGAdf :: Falpha(double mu, double sa, double *f, double *f_alpha)
{
 Fmu(mu, f, f_alpha, 0, 0);
 *f_alpha*=(-sa);
}

void SGAdf :: Fmu(double mu, double *f, double *f_mu, double *g1, double *g2)
{
 double d2=sqr(mu-mu_c);
 *f=B*exp(-(d2+a4*sqr(d2))/sqr(dmu));
 double g1loc=-2.0*(mu-mu_c)*(1.0+2.0*a4*d2)/sqr(dmu);
 *f_mu=*f*g1loc;
 if (g1) *g1=g1loc;
 if (g2) *g2=2.0/sqr(sqr(dmu))*
	         (2.0*d2*(1.0-3.0*a4*sqr(dmu)+4.0*a4*d2+4.0*sqr(a4*d2))-sqr(dmu)); 
}

double SGAdf :: g1short(double mu)
{
 double d2=sqr(mu-mu_c);
 return -2.0*(mu-mu_c)*(1.0+2.0*a4*d2)/sqr(dmu);
}

class SGAIntegrand : public IntegrableFunction
{
 public:
 double mu_c, dmu, a4;
 double F(double mu);
};

double SGAIntegrand :: F(double mu)
{
 double d2=sqr(mu-mu_c);
 return exp(-(d2+a4*sqr(d2))/sqr(dmu));
}

SGAdf :: SGAdf(double *ParmIn)
{
 alpha_c=ParmIn[21];
 dmu=ParmIn[22];
 a4=ParmIn[23];

 mu_c=cos(alpha_c);
 SGAIntegrand sgi;
 sgi.mu_c=mu_c;
 sgi.dmu=dmu;
 sgi.a4=a4;
 int err;
 B=1.0/qromb(&sgi, -1, 1, 1e-10, &err);

 EPS_mu0=min(EPS_mu0, sqr(dmu)/30);
 OK=finite(B) && B>0.0;
}

//----------------------------------------------------------------------------

class SINdf : public Std_DF_angle //111
{
 double B, alpha_c, mu_c, N;
 public:
 void Falpha(double mu, double sa, double *f, double *f_alpha);
 void Fmu(double mu, double *f, double *f_mu, double *g1, double *g2);
 double g1short(double mu);
 SINdf(double *ParmIn);
};

void SINdf :: Falpha(double mu, double sa, double *f, double *f_alpha)
{
 double amu=fabs(mu);
 if (amu<=mu_c)
 {
  *f=B;
  *f_alpha=0.0;
 }
 else
 {
  double alpha=(amu<1.0) ? acos(amu) : 0.0;
  double p=alpha/alpha_c*M_PI/2;
  double sp=sin(p);
  double cp=cos(p);
  *f=(p>0.0) ? B*pow(sp, N) : 0.0;
  *f_alpha=(p>0.0) ? B*N*pow(sp, N-1.0)*cp/alpha_c*M_PI/2 : 0.0;
 }
}

void SINdf :: Fmu(double mu, double *f, double *f_mu, double *g1, double *g2)
{
 double amu=fabs(mu);
 if (amu<mu_c)
 {
  *f=B;
  *f_mu=0.0;
  if (g1) *g1=0.0;
  if (g2) *g2=0.0;
 }
 else
 {
  double alpha=(amu<1.0) ? acos(amu) : 0.0;
  double sa=sin(alpha);
  double p=alpha/alpha_c*M_PI/2;
  double sp=sin(p);
  double cp=cos(p);
  *f=(p>0.0) ? B*pow(sp, N) : 0.0;
  *f_mu=(p>0.0) ? -(M_PI/2/alpha_c/sa)*B*N*pow(sp, N-1.0)*cp*sign(mu) : 0.0;
  if (g1) *g1=(p>0.0) ? -(M_PI/2/alpha_c/sa)*N*cp/sp*sign(mu) : 0.0;
  if (g2) *g2=(p>0.0) ? sqr(M_PI/2/alpha_c/sa)*N*((N-1.0)*sqr(cp/sp)-1.0-
	                    2.0*alpha_c/M_PI*cp/sp*mu/sa) : 0.0;
 }
}

double SINdf :: g1short(double mu)
{
 double amu=fabs(mu);
 if (amu<mu_c) return 0.0;
 else
 {
  double alpha=(amu<1.0) ? acos(amu) : 0.0;
  double sa=sin(alpha);
  double p=alpha/alpha_c*M_PI/2;
  double sp=sin(p);
  double cp=cos(p);
  return (p>0.0) ? -(M_PI/2/alpha_c/sa)*N*cp/sp*sign(mu) : 0.0;
 }
}

class SINIntegrand : public IntegrableFunction
{
 public:
 double alpha_c, N;
 double F(double alpha);
};

double SINIntegrand :: F(double alpha)
{
 double p=alpha/alpha_c*M_PI/2;
 return (p>0.0) ? pow(sin(p), N)*sin(alpha) : 0.0;
}

SINdf :: SINdf(double *ParmIn)
{
 alpha_c=ParmIn[20];
 N=ParmIn[23]; //Changed since the previous version!!!                #####

 mu_c=fabs(cos(alpha_c));
 alpha_c=acos(mu_c);

 if (alpha_c==0.0) B=0.5;
 else
 {
  SINIntegrand si;
  si.alpha_c=alpha_c;
  si.N=N;
  int err;
  B=0.5/(mu_c+qromb(&si, 0.0, alpha_c, 1e-10, &err));
 }

 OK=finite(B) && B>0.0; 
}

//----------------------------------------------------------------------------

 void Std_DF :: Fp(double p, double p_z, double p_n, double *f, double *df_dp, double *df_dalpha)
 {
  double f1, f2, df1_dp, df2_dalpha;

  F1->Fp(p, &f1, &df1_dp);

  double mu=(p>0.0) ? p_z/p : 0.0;
  double sa=(p>0.0) ? p_n/p : 0.0;
  if (mu>1.0) mu=1.0;
  if (mu<(-1.0)) mu=-1.0;
  if (sa>1.0) sa=1.0;
  if (sa<(-1.0)) sa=-1.0;
  
  F2->Falpha(mu, sa, &f2, &df2_dalpha);

  *f=f1*f2; 
  *df_dp=df1_dp*f2;
  *df_dalpha=f1*df2_dalpha;
 }

 void Std_DF :: FE(double E, double mu, 
	               double *f, double *df_dE, double *df_dmu, 
		           double *g1, double *g2)
 {
  if (!f) *g1=F2->g1short(mu); //only g1 is needed
  else //calculating all
  {
   double f1, f2, df1_dE, df2_dmu;

   F1->FE(E, &f1, &df1_dE);
   F2->Fmu(mu, &f2, &df2_dmu, g1, g2);

   *f=f1*f2;
   *df_dE=df1_dE*f2;
   *df_dmu=f1*df2_dmu;
  }
 }

 Std_DF :: Std_DF(double n0, double *ParmIn)
 {
  switch (int(ParmIn[17]))
  {
   case FFF: F1=new FFFdf(); //1
	         break;
   case THM: F1=new THMdf(n0, ParmIn); //2
	         break;
   case PLW: F1=new PLWdf(ParmIn); //3
	         break;
   case DPL: F1=new DPLdf(ParmIn); //4
	         break;
   case TNT: F1=new TNTdf(n0, ParmIn); //5
	         break;
   case KAP: F1=new KAPdf(n0, ParmIn); //6
	         break;
   case PLP: F1=new PLPdf(ParmIn); //7
	         break;
   case PLG: F1=new PLGdf(ParmIn); //8
	         break;
   case TNP: F1=new TNPdf(n0, ParmIn); //9
	         break;
   case TNG: F1=new TNGdf(n0, ParmIn); //10
	         break;
   case NDS: F1=new NDSdf(n0, ParmIn); //50
	         break;
   default:  F1=new FFFdf();
  }

  if (!F1->OK) IDLmsg("MWTransfer error: incorrect energy distribution.");
  else
  {
   dfzero=F1->dfzero;
   n_b=F1->n_b;
   N_intervals=F1->N_intervals;
   if (N_intervals>0)
   {
    logscale=(int*)malloc(sizeof(int)*N_intervals);
    E_x=(double*)malloc(sizeof(double)*(N_intervals+1));
    for (int i=0; i<N_intervals; i++) logscale[i]=F1->logscale[i];
    for (int i=0; i<=N_intervals; i++) E_x[i]=F1->E_x[i];
   }
  }
 
  switch (int(ParmIn[19]))
  {
   case ISO: 
   case ISO1: F2=new ISOdf(); //0, 1
	          break;
   case EXP: F2=new EXPdf(ParmIn); //2
	         break;
   case GAU: F2=new GAUdf(ParmIn); //3
	         break;
   case GAB: F2=new GABdf(ParmIn); //4
	         break;
   case SGA: F2=new SGAdf(ParmIn); //5
	         break;
   case SIN: F2=new SINdf(ParmIn); //111
	         break;
   default: F2=new ISOdf();
  }

  if (!F2->OK) IDLmsg("MWTransfer error: incorrect pitch-angle distribution.");
  else EPS_mu0=F2->EPS_mu0;

  OK=F1->OK && F2->OK;
 }

 Std_DF :: ~Std_DF()
 {
  delete F1;
  delete F2;
 }

 //-------------------------------------------------------------------------

Multi_Std_DF :: Multi_Std_DF(double n0, double *ParmIn)
{
 int dftype=(int)ParmIn[17];

 N=(dftype==TPL || dftype==TDP) ? 2 : 1;
 mdf=(DF**)malloc(sizeof(DF*)*(N+1));

 if (dftype==TPL || dftype==TDP)
 {
  double ParmLoc[InSizeF];

  for (int i=0; i<InSizeF; i++) ParmLoc[i]=ParmIn[i];
  ParmLoc[17]=(double)THM;
  ParmLoc[19]=(double)ISO;
  mdf[0]=new Std_DF(n0, ParmLoc);

  for (int i=0; i<InSizeF; i++) ParmLoc[i]=ParmIn[i];
  if (dftype==TPL) ParmLoc[17]=(double)PLW;
  else if (dftype==TDP) ParmLoc[17]=(double)DPL;
  mdf[1]=new Std_DF(n0, ParmLoc);
 }
 else mdf[0]=new Std_DF(n0, ParmIn);

 n_b=0.0;
 OK=1;
 for (int i=0; i<N; i++)
 {
  n_b+=mdf[i]->n_b;
  OK=OK && mdf[i]->OK;
 }

 mdf[N]=0;
}

Multi_Std_DF :: ~Multi_Std_DF()
{
 for (int i=0; i<N; i++) delete mdf[i];
 free(mdf);
}