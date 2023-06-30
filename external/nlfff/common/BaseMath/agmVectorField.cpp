#include "stdDefinitions.h"
#include "agsFieldsCommon.h"
#define _USE_MATH_DEFINES
#include "math.h"

#include "agmVectorField.h"
#include "agmScalarField.h"
#include "agmMetrics.h"

#include "agmRotate3D.h"

#define fidx(kx, ky, kz) (((ky) + (kz)*N[1])*N[0] + (kx))

//-----------------------------------------------------------------------
uint32_t CagmVectorField::getThetaMetrics(REALTYPE_A relBound, int stencil, CagmMetricsLim *m)
{
    int *N = this->GetDimensions();

    CagmVectorField *J = new CagmVectorField(N);
    CagmVectorField *JxB = new CagmVectorField(N);
    J->rotScheme(this, stencil); // rotB
    JxB->cross(J, this); // rotB x B
    CagmScalarField *b = new CagmScalarField(N);
    CagmScalarField *j = new CagmScalarField(N);
    CagmScalarField *jxb = new CagmScalarField(N);
    b->abs(this);
    j->abs(J);
    jxb->abs(JxB);
    REALTYPE_A ts = 0, tjs = 0, tj = 0;
    int kx, ky, kz;
    int cnt = 0;
    int bx[2]; bx[0] = (int)ceil(N[0]*relBound); bx[1] = (int)floor(N[0]*relBound);
    int by[2]; by[0] = (int)ceil(N[1]*relBound); by[1] = (int)floor(N[1]*relBound);
    int bz[2]; bz[0] =                        0; bz[1] = (int)floor(N[2]*relBound);
    for (kz = bz[0]; kz < N[2]-bz[1]; kz++)
        for (ky = by[0]; ky < N[1]-by[1]; ky++)
            for (kx = bx[0]; kx < N[0]-bx[1]; kx++)
            {
                REALTYPE_A bv = b->field[(ky)+(kz)*N[1]][(kx)];
                REALTYPE_A jv = j->field[(ky)+(kz)*N[1]][(kx)];
                REALTYPE_A jxbv = jxb->field[(ky)+(kz)*N[1]][(kx)];

                ts  += jxbv/(jv*bv);
                tjs += jxbv/bv;
                tj  += jv;
                cnt++;
            }

    m->mW->m[0] = asin(ts/cnt)*180/M_PI;
    m->mW->mj[0] = asin(tjs/tj)*180/M_PI;

    delete[] J;
    delete[] JxB;
    delete[] j;
    delete[] b;
    delete[] jxb;

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::getDifference(CagmVectorField *init, REALTYPE_A relBound, int stencil, 
                                     CagmMetricsLim *mabs, CagmMetricsLim *mcos, CagmMetricsCosLim *mcm)
{
    int *N = GetDimensions();
    CagmScalarField *Babs1 = new CagmScalarField(N);
    Babs1->abs(init);
    CagmScalarField *Babs2 = new CagmScalarField(N);
    Babs2->abs(this);

    CagmScalarField *B1B2 = new CagmScalarField(N);
    B1B2->dot(init, this);

    CagmVectorField *J = new CagmVectorField(N);
    J->rotScheme(this, stencil); // rotB
    CagmScalarField *j = new CagmScalarField(N);
    j->abs(J);

    REALTYPE_A ta = 0, tja = 0, tBa = 0, tc = 0, tjc = 0, tBc = 0, tj = 0, tB = 0;
    mcm->mW->c[0] = 1;
    mcm->mW->B4c[0] = 0;
    int kx, ky, kz;
    int cnt = 0;
    int bx[2]; bx[0] = (int)ceil(N[0]*relBound); bx[1] = (int)floor(N[0]*relBound);
    int by[2]; by[0] = (int)ceil(N[1]*relBound); by[1] = (int)floor(N[1]*relBound);
    int bz[2]; bz[0] =                        0; bz[1] = (int)floor(N[2]*relBound);
    for (kz = bz[0]; kz < N[2]-bz[1]; kz++)
        for (ky = by[0]; ky < N[1]-by[1]; ky++)
            for (kx = bx[0]; kx < N[0]-bx[1]; kx++)
            {
                REALTYPE_A B  = Babs2->field[(ky)+(kz)*N[1]][(kx)];
                REALTYPE_A dbv = fabs(Babs1->field[(ky)+(kz)*N[1]][(kx)] - B)/B;
                REALTYPE_A cv = B1B2->field[(ky)+(kz)*N[1]][(kx)]/(Babs1->field[(ky)+(kz)*N[1]][(kx)]*B);
                REALTYPE_A jv = j->field[(ky)+(kz)*N[1]][(kx)];

                tj  += jv;
                tB  += B;
                ta  += dbv;
                tja += dbv*jv;
                tBa += dbv*B;
                tc  += cv;
                tjc += cv*jv;
                tBc += cv*B;
                cnt++;
                if (fabs(cv) < mcm->mW->c[0])
                {
                    mcm->mW->c[0] = fabs(cv);
                    mcm->mW->B4c[0] = Babs2->field[(ky)+(kz)*N[1]][(kx)];
                }
            }
    mabs->mW->m[0] = ta/cnt;
    mabs->mW->mj[0] = tja/tj;
    mabs->mW->mB[0] = tBa/tB;
    mcos->mW->m[0] = tc/cnt;
    mcos->mW->mj[0] = tjc/tj;
    mcos->mW->mB[0] = tBc/tB;

    delete Babs1;
    delete Babs2;
    delete B1B2;
    delete J;
    delete j;

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::crossD(CagmVectorField *a, const CagmVectorField *b)
{
	// check equiv. sizes!
	int kx, ky, kz;
    REALTYPE_A tx, ty, tz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				tx = a->allocFieldY[fidx(kx, ky, kz)]*b->allocFieldZ[fidx(kx, ky, kz)] - a->allocFieldZ[fidx(kx, ky, kz)]*b->allocFieldY[fidx(kx, ky, kz)];
				ty = a->allocFieldZ[fidx(kx, ky, kz)]*b->allocFieldX[fidx(kx, ky, kz)] - a->allocFieldX[fidx(kx, ky, kz)]*b->allocFieldZ[fidx(kx, ky, kz)];
				tz = a->allocFieldX[fidx(kx, ky, kz)]*b->allocFieldY[fidx(kx, ky, kz)] - a->allocFieldY[fidx(kx, ky, kz)]*b->allocFieldX[fidx(kx, ky, kz)];
				allocFieldX[fidx(kx, ky, kz)] = tx;
				allocFieldY[fidx(kx, ky, kz)] = ty;
				allocFieldZ[fidx(kx, ky, kz)] = tz;
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::crossD(CagmVectorField *a)
{
    return crossD(this, a);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::rotD(CagmVectorField *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    REALTYPE_A zy, yz, xz, zx, yx, xy;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
                if (kx == 0)
                {
                    zx = - 3*a->allocFieldZ[fidx(0, ky, kz)] + 4*a->allocFieldZ[fidx(1, ky, kz)] - a->allocFieldZ[fidx(2, ky, kz)];
                    yx = - 3*a->allocFieldY[fidx(0, ky, kz)] + 4*a->allocFieldY[fidx(1, ky, kz)] - a->allocFieldY[fidx(2, ky, kz)];
                }
                else if (kx == N[0]-1)
                {
                    zx = a->allocFieldZ[fidx(N[0]-3, ky, kz)] - 4*a->allocFieldZ[fidx(N[0]-2, ky, kz)] + 3*a->allocFieldZ[fidx(N[0]-1, ky, kz)];
                    yx = a->allocFieldY[fidx(N[0]-3, ky, kz)] - 4*a->allocFieldY[fidx(N[0]-2, ky, kz)] + 3*a->allocFieldY[fidx(N[0]-1, ky, kz)];
                }
                else
                {
                    zx = a->allocFieldZ[fidx(kx+1, ky, kz)] - a->allocFieldZ[fidx(kx-1, ky, kz)];
                    yx = a->allocFieldY[fidx(kx+1, ky, kz)] - a->allocFieldY[fidx(kx-1, ky, kz)];
                }

                if (ky == 0)
                {
                    zy = - 3*a->allocFieldZ[fidx(kx, 0, kz)] + 4*a->allocFieldZ[fidx(kx, 1, kz)] - a->allocFieldZ[fidx(kx, 2, kz)];
                    xy = - 3*a->allocFieldX[fidx(kx, 0, kz)] + 4*a->allocFieldX[fidx(kx, 1, kz)] - a->allocFieldX[fidx(kx, 2, kz)];
                }
                else if (ky == N[1]-1)
                {
                    zy = a->allocFieldZ[fidx(kx, N[1]-3, kz)] - 4*a->allocFieldZ[fidx(kx, N[1]-2, kz)] + 3*a->allocFieldZ[fidx(kx, N[1]-1, kz)];
                    xy = a->allocFieldX[fidx(kx, N[1]-3, kz)] - 4*a->allocFieldX[fidx(kx, N[1]-2, kz)] + 3*a->allocFieldX[fidx(kx, N[1]-1, kz)];
                }
                else
                {
                    zy = a->allocFieldZ[fidx(kx, ky+1, kz)] - a->allocFieldZ[fidx(kx, ky-1, kz)];
                    xy = a->allocFieldX[fidx(kx, ky+1, kz)] - a->allocFieldX[fidx(kx, ky-1, kz)];
                }

                if (kz == 0)
                {
                    xz = - 3*a->allocFieldX[fidx(kx, ky, 0)] + 4*a->allocFieldX[fidx(kx, ky, 1)] - a->allocFieldX[fidx(kx, ky, 2)];
                    yz = - 3*a->allocFieldY[fidx(kx, ky, 0)] + 4*a->allocFieldY[fidx(kx, ky, 1)] - a->allocFieldY[fidx(kx, ky, 2)];
                }
                else if (kz == N[2]-1)
                {
                    xz = a->allocFieldX[fidx(kx, ky, N[2]-3)] - 4*a->allocFieldX[fidx(kx, ky, N[2]-2)] + 3*a->allocFieldX[fidx(kx, ky, N[2]-1)];
                    yz = a->allocFieldY[fidx(kx, ky, N[2]-3)] - 4*a->allocFieldY[fidx(kx, ky, N[2]-2)] + 3*a->allocFieldY[fidx(kx, ky, N[2]-1)];
                }
                else
                {
                    xz = a->allocFieldX[fidx(kx, ky, kz+1)] - a->allocFieldX[fidx(kx, ky, kz-1)];
                    yz = a->allocFieldY[fidx(kx, ky, kz+1)] - a->allocFieldY[fidx(kx, ky, kz-1)];
                }

				allocFieldX[fidx(kx, ky, kz)] = (zy - yz)*0.5;
				allocFieldY[fidx(kx, ky, kz)] = (xz - zx)*0.5;
				allocFieldZ[fidx(kx, ky, kz)] = (yx - xy)*0.5;
			}


    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::gradD(CagmScalarField *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    REALTYPE_A dx, dy, dz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
                if (kx == 0)
                    dx = - 3*a->allocField[fidx(0, ky, kz)] + 4*a->allocField[fidx(1, ky, kz)] - a->allocField[fidx(2, ky, kz)];
                else if (kx == N[0]-1)
                    dx = a->allocField[fidx(N[0]-3, ky, kz)] - 4*a->allocField[fidx(N[0]-2, ky, kz)] + 3*a->allocField[fidx(N[0]-1, ky, kz)];
                else
                    dx = a->allocField[fidx(kx+1, ky, kz)] - a->allocField[fidx(kx-1, ky, kz)];
				allocFieldX[fidx(kx, ky, kz)] = dx*0.5;

                if (ky == 0)
                    dy = - 3*a->allocField[fidx(kx, 0, kz)] + 4*a->allocField[fidx(kx, 1, kz)] - a->allocField[fidx(kx, 2, kz)];
                else if (ky == N[1]-1)
                    dy = a->allocField[fidx(kx, N[1]-3, kz)] - 4*a->allocField[fidx(kx, N[1]-2, kz)] + 3*a->allocField[fidx(kx, N[1]-1, kz)];
                else
                    dy = a->allocField[fidx(kx, ky+1, kz)] - a->allocField[fidx(kx, ky-1, kz)];
				allocFieldY[fidx(kx, ky, kz)] = dy*0.5;

                if (kz == 0)
                    dz = - 3*a->allocField[fidx(kx, ky, 0)] + 4*a->allocField[fidx(kx, ky, 1)] - a->allocField[fidx(kx, ky, 2)];
                else if (kz == N[2]-1)
                    dz = a->allocField[fidx(kx, ky, N[2]-3)] - 4*a->allocField[fidx(kx, ky, N[2]-2)] + 3*a->allocField[fidx(kx, ky, N[2]-1)];
                else
                    dz = a->allocField[fidx(kx, ky, kz+1)] - a->allocField[fidx(kx, ky, kz-1)];
				allocFieldZ[fidx(kx, ky, kz)] = dz*0.5;
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::multD(REALTYPE_A c, CagmVectorField *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				allocFieldX[fidx(kx, ky, kz)] = a->allocFieldX[fidx(kx, ky, kz)]*c;
				allocFieldY[fidx(kx, ky, kz)] = a->allocFieldY[fidx(kx, ky, kz)]*c;
				allocFieldZ[fidx(kx, ky, kz)] = a->allocFieldZ[fidx(kx, ky, kz)]*c;
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::multD(REALTYPE_A c)
{
    return multD(c, this);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::multD(CagmScalarField *c, CagmVectorField *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				allocFieldX[fidx(kx, ky, kz)] = (a->allocFieldX[fidx(kx, ky, kz)]) * (c->allocField[fidx(kx, ky, kz)]);
				allocFieldY[fidx(kx, ky, kz)] = (a->allocFieldY[fidx(kx, ky, kz)]) * (c->allocField[fidx(kx, ky, kz)]);
				allocFieldZ[fidx(kx, ky, kz)] = (a->allocFieldZ[fidx(kx, ky, kz)]) * (c->allocField[fidx(kx, ky, kz)]);
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::multD(CagmVectorField *a, CagmScalarField *c)
{
    return multD(c, a);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::multD(CagmScalarField *c)
{
    return multD(c, this);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::addD(CagmVectorField *a, CagmVectorField *b)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				allocFieldX[fidx(kx, ky, kz)] = a->allocFieldX[fidx(kx, ky, kz)] + b->allocFieldX[fidx(kx, ky, kz)];
				allocFieldY[fidx(kx, ky, kz)] = a->allocFieldY[fidx(kx, ky, kz)] + b->allocFieldY[fidx(kx, ky, kz)];
				allocFieldZ[fidx(kx, ky, kz)] = a->allocFieldZ[fidx(kx, ky, kz)] + b->allocFieldZ[fidx(kx, ky, kz)];
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::addD(CagmVectorField *a)
{
    return addD(this, a);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::subD(CagmVectorField *a, CagmVectorField *b)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				allocFieldX[fidx(kx, ky, kz)] = a->allocFieldX[fidx(kx, ky, kz)] - b->allocFieldX[fidx(kx, ky, kz)];
				allocFieldY[fidx(kx, ky, kz)] = a->allocFieldY[fidx(kx, ky, kz)] - b->allocFieldY[fidx(kx, ky, kz)];
				allocFieldZ[fidx(kx, ky, kz)] = a->allocFieldZ[fidx(kx, ky, kz)] - b->allocFieldZ[fidx(kx, ky, kz)];
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::subD(CagmVectorField *a)
{
    return subD(this, a);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::negD(CagmVectorField *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				allocFieldX[fidx(kx, ky, kz)] = -a->allocFieldX[fidx(kx, ky, kz)];
				allocFieldY[fidx(kx, ky, kz)] = -a->allocFieldY[fidx(kx, ky, kz)];
				allocFieldZ[fidx(kx, ky, kz)] = -a->allocFieldZ[fidx(kx, ky, kz)];
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::negD()
{
    return negD(this);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::zeroD()
{
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				allocFieldX[fidx(kx, ky, kz)] = 0;
				allocFieldY[fidx(kx, ky, kz)] = 0;
				allocFieldZ[fidx(kx, ky, kz)] = 0;
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorField::condWeight(int WiegelmannProcCondBase, CagmVectorField *baseField, CagmVectorField *baseWeight, int WiegelmannProcCondBase2, CagmVectorField *baseField2, CagmVectorField *baseWeight2,
                                  int WiegelmannProcCondAbs, CagmScalarField *absField, CagmScalarField *absWeight, int WiegelmannProcCondAbs2, CagmScalarField *absField2, CagmScalarField *absWeight2,
                                  int WiegelmannProcCondLOS, CagmScalarField *losField, CagmScalarField *losWeight, int WiegelmannProcCondLOS2, CagmScalarField *losField2, CagmScalarField *losWeight2, 
                                  CagmRotate3D *rotator)
{
    int iAbs = (absField && absWeight ? WiegelmannProcCondAbs : 0);
    int iAbs2 = (absField2 && absWeight2 ? WiegelmannProcCondAbs2 : 0);
    int iLOS = (losField && losWeight ? WiegelmannProcCondLOS : 0);
    int iLOS2 = (losField2 && losWeight2 ? WiegelmannProcCondLOS2 : 0);
    int ixyz = (baseField && baseWeight ? WiegelmannProcCondBase : 0);
    int ixyz2 = (baseField2 && baseWeight2 ? WiegelmannProcCondBase2 : 0);
    if (iLOS != 0 || iLOS2 != 0 || iAbs != 0 || iAbs2 != 0)
    {
        CagmScalarField w(this->GetDimensions());
        CagmScalarField Babs(this->GetDimensions());
        Babs.abs(this);

        if (iAbs != 0)
        {
            w = *absWeight;
            if (iAbs == 1)
                w.limWeight(1, &Babs, absField);
            Babs.relax(absField, &w);
        }

        if (iAbs2 == 1)
        {
            w = *absWeight2;
            w.limWeight(-1, &Babs, absField2);
            Babs.relax(absField2, &w);
        }

        if (iLOS != 0 || iLOS2 != 0)
        {
            CagmVectorField *Brot = nullptr;
            bool bRotate = false;
            if (rotator->isEye())
                Brot = this;
            else
            {
                bRotate = true;
                Brot = new CagmVectorField(*this);
                Brot->rotate3D(rotator, true);
            }

            CagmScalarField *proj = new CagmScalarField(N); //////
            proj->projection(this, rotator->vcos); ///////////
            
            CagmScalarField Blos(Brot->GetDimensions());
            CagmScalarField Btr(Brot->GetDimensions());
            CagmScalarField Bv(Brot->GetDimensions());

            Brot->getTransv(&Btr);
            Brot->getComponent(&Blos, PLANE_Z);

            if (iLOS != 0)
            {
                w = *losWeight;
                if (iLOS == 1)
                    w.limWeight(1, &Blos, losField);
                Blos.relax(losField, &w);
            }
            if (iLOS2 == 1)
            {
                w = *losWeight2;
                w.limWeight(-1, &Blos, losField2);
                Blos.relax(losField2, &w);
            }

            CagmScalarField Btrn(Brot->GetDimensions());
            Btrn.sqDiff(&Babs, &Blos);

            Brot->setComponent(&Blos, PLANE_Z);

            Btr.inv();
            Btrn.mult(&Btr);
            Brot->getComponent(&Bv, PLANE_X);
            Bv.mult(&Btrn);
            Brot->setComponent(&Bv, PLANE_X);
            Brot->getComponent(&Bv, PLANE_Y);
            Bv.mult(&Btrn);
            Brot->setComponent(&Bv, PLANE_Y);

            Brot->rotate3D(rotator, false);

            if (bRotate)
            {
                *this = *Brot;
                delete Brot;
            }
        }
        else
        {
            CagmScalarField Babsn(this->GetDimensions());
            Babsn.abs(this);
            Babsn.inv();
            Babs.mult(&Babsn);

            this->mult(&Babs);
        }

        return 0;
    }

    if (ixyz != 0 || ixyz2 != 0)
    {
        CagmScalarField Bv(this->GetDimensions());
        CagmScalarField bv(this->GetDimensions());
        CagmScalarField wv(this->GetDimensions());
        CagmScalarField bv2(this->GetDimensions());
        CagmScalarField wv2(this->GetDimensions());

        this->getComponent(&Bv, PLANE_X);
        if (ixyz != 0)
        {
            baseField->getComponent(&bv, PLANE_X);
            baseWeight->getComponent(&wv, PLANE_X);
            if (ixyz == 1)
                wv.limWeight(1, &bv, &wv);
            Bv.relax(&bv, &wv);
        }
        if (ixyz2 == 1)
        {
            baseField2->getComponent(&bv, PLANE_X);
            baseWeight2->getComponent(&wv, PLANE_X);
            wv.limWeight(-1, &bv, &wv);
            Bv.relax(&bv, &wv);
        }
        this->setComponent(&Bv, PLANE_X);

        this->getComponent(&Bv, PLANE_Y);
        if (ixyz != 0)
        {
            baseField->getComponent(&bv, PLANE_Y);
            baseWeight->getComponent(&wv, PLANE_Y);
            if (ixyz == 1)
                wv.limWeight(1, &bv, &wv);
            Bv.relax(&bv, &wv);
        }
        if (ixyz2 == 1)
        {
            baseField2->getComponent(&bv, PLANE_Y);
            baseWeight2->getComponent(&wv, PLANE_Y);
            wv.limWeight(-1, &bv, &wv);
            Bv.relax(&bv, &wv);
        }
        this->setComponent(&Bv, PLANE_Y);

        this->getComponent(&Bv, PLANE_Z);
        if (ixyz != 0)
        {
            baseField->getComponent(&bv, PLANE_Z);
            baseWeight->getComponent(&wv, PLANE_Z);
            if (ixyz == 1)
                wv.limWeight(1, &bv, &wv);
            Bv.relax(&bv, &wv);
        }
        if (ixyz2 == 1)
        {
            baseField2->getComponent(&bv, PLANE_Z);
            baseWeight2->getComponent(&wv, PLANE_Z);
            wv.limWeight(-1, &bv, &wv);
            Bv.relax(&bv, &wv);
        }
        this->setComponent(&Bv, PLANE_Z);
    }

    return 0;
}
