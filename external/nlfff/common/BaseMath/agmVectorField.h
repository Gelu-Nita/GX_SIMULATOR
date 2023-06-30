#pragma once
#include "stdDefinitions.h"

#include "agmVectorFieldOps.h"
#include "agsFieldsCommon.h"
#include "binUtilities.h"

class CagmRotate3D;
class CagmMetricsLim;
class CagmMetricsCosLim;

class CagmScalarField;

//------------------------------------------------------------------
class CagmVectorField : public CagmVectorFieldOps
{
friend class CagmScalarField;

public:
    static uint32_t GetAllocSize(int *N)
    {
        return 3*sizeof(REALTYPE_A)*N[0]*N[1]*N[2] + sizeof(CagmVectorField) + CagmVectorFieldOps::GetAllocSize(N);
    }

    //uint32_t Get(CagmVectorField& to)
    //{
    //    return to.Copy(*this);
    //}

    uint32_t Copy(const CagmVectorField& from)
    {
        if (from.isRef)
        {
            int ky, kz;
            for (ky = 0; ky < N[1]; ky++)
                for (kz = 0; kz < N[2]; kz++)
                {
                    memcpy(allocFieldX + (ky + kz*N[1])*N[0], from.allocFieldX + (ky + kz*N[1])*N[0], sizeof(REALTYPE_A)*N[0]);
                    memcpy(allocFieldY + (ky + kz*N[1])*N[0], from.allocFieldY + (ky + kz*N[1])*N[0], sizeof(REALTYPE_A)*N[0]);
                    memcpy(allocFieldZ + (ky + kz*N[1])*N[0], from.allocFieldZ + (ky + kz*N[1])*N[0], sizeof(REALTYPE_A)*N[0]);
                }
        }
        else
        {
            memcpy(allocFieldX, from.allocFieldX, sizeof(REALTYPE_A)*N[0]*N[1]*N[2]);
            memcpy(allocFieldY, from.allocFieldY, sizeof(REALTYPE_A)*N[0]*N[1]*N[2]);
            memcpy(allocFieldZ, from.allocFieldZ, sizeof(REALTYPE_A)*N[0]*N[1]*N[2]);
        }

        SetSteps((REALTYPE_A *)from.step);

        return 0;
    }

public:
    REALTYPE_A *allocFieldX,  *allocFieldY,  *allocFieldZ;

protected:
	bool isRef;

protected:
    uint32_t Alloc()
    {
        allocFieldX = new REALTYPE_A[N[0]*N[1]*N[2]];
        allocFieldY = new REALTYPE_A[N[0]*N[1]*N[2]];
        allocFieldZ = new REALTYPE_A[N[0]*N[1]*N[2]];
        memset(allocFieldX, 0, sizeof(REALTYPE_A)*N[0]*N[1]*N[2]);
        memset(allocFieldY, 0, sizeof(REALTYPE_A)*N[0]*N[1]*N[2]);
        memset(allocFieldZ, 0, sizeof(REALTYPE_A)*N[0]*N[1]*N[2]);

        int ky, kz;
        for (ky = 0; ky < N[1]; ky++)
            for (kz = 0; kz < N[2]; kz++)
            {
                fieldX[ky + kz*N[1]] = allocFieldX + (ky + kz*N[1])*N[0];
                fieldY[ky + kz*N[1]] = allocFieldY + (ky + kz*N[1])*N[0];
                fieldZ[ky + kz*N[1]] = allocFieldZ + (ky + kz*N[1])*N[0];
            }

        return 0;
    }

    uint32_t Copy(CbinDataStruct *data)
    {
        data->Copy(allocFieldX, allocFieldY, allocFieldZ);

        REALTYPE_A step[] = {1.0, 1.0, 1.0};
        SetSteps(step);

        return 0;
    }

    uint32_t Copy(REALTYPE_A *Bx, REALTYPE_A *By, REALTYPE_A *Bz)
    {
        CopyComp(Bx, 0);
        CopyComp(By, 1);
        CopyComp(Bz, 2);

        REALTYPE_A step[] = {1.0, 1.0, 1.0};
        SetSteps(step);

        return 0;
    }

    uint32_t Delete()
    {
		if (!isRef)
        {
            delete [] allocFieldX;
            delete [] allocFieldY;
            delete [] allocFieldZ;
        }

        return 0;
    }

public:
	CagmVectorField(int *_N, bool isAlloc = true, int *_DphysL = nullptr, int *_DphysH = nullptr)
        : CagmVectorFieldOps(_N, _DphysL, _DphysH),
          allocFieldX(nullptr),  
          allocFieldY(nullptr),  
          allocFieldZ(nullptr)  
        {
    		isRef = !isAlloc;
            if (isAlloc)
                Alloc();
		}

	CagmVectorField(CbinDataStruct* data)
        : CagmVectorFieldOps(data->GetDimensions()),
          allocFieldX(nullptr),  
          allocFieldY(nullptr),  
          allocFieldZ(nullptr)  
        {
    		isRef = false;
            Alloc();
            Copy(data);
		}

	CagmVectorField(int *N, REALTYPE_A *Bx, REALTYPE_A *By, REALTYPE_A *Bz)
        : CagmVectorFieldOps(N),
          allocFieldX(nullptr),  
          allocFieldY(nullptr),  
          allocFieldZ(nullptr)  
        {
    		isRef = false;
            Alloc();
            Copy(Bx, By, Bz);
		}

    CagmVectorField(const CagmVectorField& from) // copy constructor, creates only solid copy
        : CagmVectorFieldOps(from),
          allocFieldX(nullptr),  
          allocFieldY(nullptr),  
          allocFieldZ(nullptr)  
    {
        isRef = false;
        Alloc();
        Copy(from);
    }

	CagmVectorField(CagmVectorFieldOps *source, int *M, int *Mmin, int *_DphysL = nullptr, int *_DphysH = nullptr)
        : CagmVectorFieldOps(M, _DphysL, _DphysH),
          allocFieldX(nullptr),  
          allocFieldY(nullptr),  
          allocFieldZ(nullptr)  
    {
		isRef = true;
        SetMargins(source, Mmin, _DphysL, _DphysH);

        SetSteps(source->step);
	}

    CagmVectorField(REALTYPE_A *X, REALTYPE_A *Y, REALTYPE_A *Z, int *M, REALTYPE_A *steps = nullptr)
        : CagmVectorFieldOps(M),
        allocFieldX(nullptr),
        allocFieldY(nullptr),
        allocFieldZ(nullptr)
    {
        isRef = true;

        setRefField(X, Y, Z, M);

        if (steps)
            SetSteps(steps);
        else
        {
            REALTYPE_A s[] = { 1.0, 1.0, 1.0 };
            SetSteps(s);
        }
    }

    CagmVectorField& operator=(const CagmVectorField& from) // creates only solid copy
    {
        if (this == &from)
            return *this;

        this->CagmVectorFieldOps::operator=(from);

        Delete();
        Alloc();
        Copy(from);

        return *this;
    }

    uint32_t CopyComp(REALTYPE_A *Bc, int comp)
    {
        REALTYPE_A *c;
        if (comp == 0)
            c = allocFieldX;
        else if (comp == 1)
            c = allocFieldY;
        else if (comp == 2)
            c = allocFieldZ;

        memcpy(c, Bc, N[0]*N[1]*N[2]*sizeof(REALTYPE_A));

        return 0;
    }

    uint32_t GetComp(REALTYPE_A *Bc, int comp)
    {
        REALTYPE_A *c;
        if (comp == 0)
            c = allocFieldX;
        else if (comp == 1)
            c = allocFieldY;
        else if (comp == 2)
            c = allocFieldZ;

        memcpy(Bc, c, N[0]*N[1]*N[2]*sizeof(REALTYPE_A));

        return 0;
    }

    uint32_t Copy(CagmVectorField *source, int *Mmin)
    {
	    int kx, ky, kz;
        for (kz = 0; kz < N[2]; kz++)
            for (ky = 0; ky < N[1]; ky++)
                for (kx = 0; kx < N[0]; kx++)
                {
                    allocFieldX[(ky + kz*N[1])*N[0] + kx] = source->allocFieldX[(ky+Mmin[1] + (kz+Mmin[2])*source->N[1])*source->N[0] + kx+Mmin[0]];
                    allocFieldY[(ky + kz*N[1])*N[0] + kx] = source->allocFieldY[(ky+Mmin[1] + (kz+Mmin[2])*source->N[1])*source->N[0] + kx+Mmin[0]];
                    allocFieldZ[(ky + kz*N[1])*N[0] + kx] = source->allocFieldZ[(ky+Mmin[1] + (kz+Mmin[2])*source->N[1])*source->N[0] + kx+Mmin[0]];
                }

        SetSteps(source->step);

        return 0;
    }

	virtual ~CagmVectorField()
    {
        Delete();
    }

    uint32_t GetFieldAddress(REALTYPE_A **px, REALTYPE_A **py, REALTYPE_A **pz)
    {
        *px = allocFieldX;
        *py = allocFieldY;
        *pz = allocFieldZ;

        return 0;
    }

	uint32_t GetData(CbinDataStruct* data)
    {
        return data->Create(N, allocFieldX, allocFieldY, allocFieldZ);
    }

    uint32_t getThetaMetrics(REALTYPE_A relBound, int stencil, CagmMetricsLim * m);
    uint32_t getDifference(CagmVectorField * init, REALTYPE_A relBound, int stencil, CagmMetricsLim * mabs, CagmMetricsLim * mcos, CagmMetricsCosLim * mcm);
    
    uint32_t crossD(CagmVectorField *a, const CagmVectorField *b);
    uint32_t crossD(CagmVectorField *a);
    uint32_t rotD(CagmVectorField *a);
    uint32_t gradD(CagmScalarField *a);
    uint32_t multD(REALTYPE_A c, CagmVectorField *a);
    uint32_t multD(REALTYPE_A c);
    uint32_t multD(CagmScalarField *c, CagmVectorField *a);
    uint32_t multD(CagmVectorField *a, CagmScalarField *c);
    uint32_t multD(CagmScalarField *c);
    uint32_t addD(CagmVectorField *a, CagmVectorField *b);
    uint32_t addD(CagmVectorField *a);
    uint32_t subD(CagmVectorField *a, CagmVectorField *b);
    uint32_t subD(CagmVectorField *a);
    uint32_t negD(CagmVectorField *a);
    uint32_t negD();
    uint32_t zeroD();
    uint32_t condWeight(int WiegelmannProcCondBase, CagmVectorField *baseField, CagmVectorField *baseWeight, int WiegelmannProcCondBase2, CagmVectorField *baseField2, CagmVectorField *baseWeight2,
        int WiegelmannProcCondAbs, CagmScalarField *absField, CagmScalarField *absWeight, int WiegelmannProcCondAbs2, CagmScalarField *absField2, CagmScalarField *absWeight2,
        int WiegelmannProcCondLOS, CagmScalarField *losField, CagmScalarField *losWeight, int WiegelmannProcCondLOS2, CagmScalarField *losField2, CagmScalarField *losWeight2, 
        CagmRotate3D *rotator);
};
