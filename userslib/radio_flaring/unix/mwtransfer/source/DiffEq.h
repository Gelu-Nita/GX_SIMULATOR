class DEIntegrand
{
 public:
 virtual void F(double x, double *y, double *y1)=0;
 virtual double MaxRatio(double *yerr, double *y)=0;
 virtual double EPS(double *y)=0;
};

void rkqs(double *y, double *dydx, int n, double *x, double htry, double *hdid, double *hnext, 
	      DEIntegrand *F, int *IntError);