#pragma once

class CagmVectorFieldOps;

//------------------------------------------------------------------
class CagmScalarFieldOps
{
friend class CagmVectorFieldOps;

public:
    int N[3];
    REALTYPE_A step[3];
    int * GetDimensions()
    {
        return N;
    }
    static uint32_t GetAllocSize(int *N)
    {
        return sizeof(REALTYPE_A)*N[1]*N[2] + sizeof(CagmScalarFieldOps);
    }

protected:
    REALTYPE_A **field;
    int NphysL[3], NphysH[3];

public:
	CagmScalarFieldOps(int *_N, int *_DphysL = nullptr, int *_DphysH = nullptr);
	CagmScalarFieldOps(const CagmScalarFieldOps&);
	virtual ~CagmScalarFieldOps();

    CagmScalarFieldOps& operator=(const CagmScalarFieldOps&);

	uint32_t setDPhys(int *_DphysL, int *_DphysH);

    //REALTYPE_A getElement(int kx, int ky, int kz);
    REALTYPE_A *getAddress(int kx, int ky, int kz);

    virtual uint32_t SetMargins(CagmScalarFieldOps *source, int *Mmin, int *_DphysL = nullptr, int *_DphysH = nullptr);

    uint32_t SetSteps(REALTYPE_A *_step);
    REALTYPE_A *GetSteps();

	//uint32_t set(CagmScalarFieldOps *a);
	REALTYPE_A derivative(int kx, int ky, int kz, int dir);
    uint32_t div(CagmVectorFieldOps *a);
    uint32_t div31(CagmVectorFieldOps *a);
    uint32_t div42(CagmVectorFieldOps *a);
    uint32_t div41(CagmVectorFieldOps *a);
    uint32_t div5(CagmVectorFieldOps *a);
    uint32_t divScheme(CagmVectorFieldOps *a, int scheme);
    uint32_t dot(CagmVectorFieldOps *a, CagmVectorFieldOps *b, CagmVectorFieldOps *Weight = nullptr);
	uint32_t abs2(CagmVectorFieldOps *a, CagmVectorFieldOps *Weight = nullptr);
	uint32_t abs(CagmVectorFieldOps *a);
	uint32_t projection(CagmVectorFieldOps *a, REALTYPE_A *d);
	uint32_t inv(CagmScalarFieldOps *a);
	uint32_t inv(void);
	uint32_t mult(REALTYPE_A c, CagmScalarFieldOps *a);
	uint32_t mult(REALTYPE_A c);
	uint32_t mult(CagmScalarFieldOps *c, CagmScalarFieldOps *a);
	uint32_t mult(CagmScalarFieldOps *c);
    uint32_t add(CagmScalarFieldOps *a, CagmScalarFieldOps *b);
    uint32_t addPhys(CagmScalarFieldOps *a, CagmScalarFieldOps *b);
    uint32_t add(CagmScalarFieldOps *a);
    uint32_t addPhys(CagmScalarFieldOps *a);
    uint32_t sub(CagmScalarFieldOps *a, CagmScalarFieldOps *b);
    uint32_t sub(CagmScalarFieldOps *a);
    uint32_t neg(CagmScalarFieldOps *a);
    uint32_t neg();
    uint32_t acos();
    uint32_t power(CagmScalarFieldOps *a, REALTYPE_A pw);
    uint32_t power(REALTYPE_A pw);
    uint32_t zero();
    uint32_t zeroZ0();
    uint32_t setZlevel(int level, REALTYPE_A w);
    uint32_t setPlane(CagmScalarFieldOps *plane, int wplane, int from, int to);

	uint32_t LOS(CagmVectorFieldOps *a, REALTYPE_A *dircos);
    static uint32_t rotate2D(CagmScalarFieldOps *ac, CagmScalarFieldOps *as, CagmScalarFieldOps *acn, CagmScalarFieldOps *asn, REALTYPE_A cosz);
    uint32_t sqDiff(CagmScalarFieldOps *a1, CagmScalarFieldOps *a2);
    uint32_t relax(CagmScalarFieldOps *mult, CagmScalarFieldOps *weight);

    REALTYPE_A sum(CagmScalarFieldOps *weight = nullptr);
    REALTYPE_A sumPhys(CagmScalarFieldOps *weight = nullptr);
    REALTYPE_A sumPhysW(CagmScalarFieldOps *weight);
    REALTYPE_A avPhys(CagmScalarFieldOps *weight = nullptr);
    REALTYPE_A maxval(void);
    uint32_t limWeight(int limType, CagmScalarFieldOps *calc, CagmScalarFieldOps *cond);

protected:
	uint32_t Initialize(int *_N, int *_NphysL = nullptr, int *_NphysH = nullptr, REALTYPE_A *_step = nullptr);
  	uint32_t setNPhys(int *_NphysL, int *_NphysH);
    uint32_t Delete();
};

// static void indices(int p, int Np, int q, int Nq, int *p1, int *p2, REALTYPE_A *pf, int *q1, int *q2, REALTYPE_A *qf);

