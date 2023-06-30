#include "stdDefinitions.h"
#include "agsFieldsCommon.h"
#include "math.h"

#include "agmVectorField.h"
#include "agmScalarField.h"

#define fidx(kx, ky, kz) (((ky) + (kz)*N[1])*N[0] + (kx))

//-----------------------------------------------------------------------
static void convWindow(int N, REALTYPE_A * v)
{
    REALTYPE_A m = 0.8;
    int hw = (N - 1) / 2;
    REALTYPE_A d = log(m) / (hw*hw);
    for (int k = 0; k < N; k++)
        v[k] = exp(d*(k - hw)*(k - hw));
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::CreateConvWindow()
{
    REALTYPE_A *a = new REALTYPE_A[N[0]];
    convWindow(N[0], a);
    int kx, ky, kz;
    REALTYPE_A s = 0;
    for (kz = 0; kz < N[0]; kz++)
        for (ky = 0; ky < N[0]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                allocField[fidx(kx, ky, kz)] = a[kx] * a[ky] * a[kz];
                s += allocField[fidx(kx, ky, kz)];
            }
    for (kz = 0; kz < N[0]; kz++)
        for (ky = 0; ky < N[0]; ky++)
            for (kx = 0; kx < N[0]; kx++)
                allocField[fidx(kx, ky, kz)] /= s;

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::divD(CagmVectorField *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    REALTYPE_A dx, dy, dz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                if (kx == 0)
                    dx = - 3*a->allocFieldX[fidx(0, ky, kz)] + 4*a->allocFieldX[fidx(1, ky, kz)] - a->allocFieldX[fidx(2, ky, kz)];
                else if (kx == N[0]-1)
                    dx = a->allocFieldX[fidx(N[0]-3, ky, kz)] - 4*a->allocFieldX[fidx(N[0]-2, ky, kz)] + 3*a->allocFieldX[fidx(N[0]-1, ky, kz)];
                else
                    dx = a->allocFieldX[fidx(kx+1, ky, kz)] - a->allocFieldX[fidx(kx-1, ky, kz)];

                if (ky == 0)
                    dy = - 3*a->allocFieldY[fidx(kx, 0, kz)] + 4*a->allocFieldY[fidx(kx, 1, kz)] - a->allocFieldY[fidx(kx, 2, kz)];
                else if (ky == N[1]-1)
                    dy = a->allocFieldY[fidx(kx, N[1]-3, kz)] - 4*a->allocFieldY[fidx(kx, N[1]-2, kz)] + 3*a->allocFieldY[fidx(kx, N[1]-1, kz)];
                else
                    dy = a->allocFieldY[fidx(kx, ky+1, kz)] - a->allocFieldY[fidx(kx, ky-1, kz)];

                if (kz == 0)
                    dz = - 3*a->allocFieldZ[fidx(kx, ky, 0)] + 4*a->allocFieldZ[fidx(kx, ky, 1)] - a->allocFieldZ[fidx(kx, ky, 2)];
                else if (kz == N[2]-1)
                    dz = a->allocFieldZ[fidx(kx, ky, N[2]-3)] - 4*a->allocFieldZ[fidx(kx, ky, N[2]-2)] + 3*a->allocFieldZ[fidx(kx, ky, N[2]-1)];
                else
                    dz = a->allocFieldZ[fidx(kx, ky, kz+1)] - a->allocFieldZ[fidx(kx, ky, kz-1)];

				allocField[fidx(kx, ky, kz)] = (dx + dy + dz) * 0.5;
            }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::dotD(CagmVectorField *a, CagmVectorField *b)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				allocField[fidx(kx, ky, kz)] = a->allocFieldX[fidx(kx, ky, kz)]*b->allocFieldX[fidx(kx, ky, kz)] +
                                          a->allocFieldY[fidx(kx, ky, kz)]*b->allocFieldY[fidx(kx, ky, kz)] +
                                          a->allocFieldZ[fidx(kx, ky, kz)]*b->allocFieldZ[fidx(kx, ky, kz)];

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::abs2D(CagmVectorField *a)
{
    return dotD(a, a);
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::absD(CagmVectorField *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				allocField[fidx(kx, ky, kz)] = sqrt(a->allocFieldX[fidx(kx, ky, kz)]*a->allocFieldX[fidx(kx, ky, kz)] +
                                               a->allocFieldY[fidx(kx, ky, kz)]*a->allocFieldY[fidx(kx, ky, kz)] +
                                               a->allocFieldZ[fidx(kx, ky, kz)]*a->allocFieldZ[fidx(kx, ky, kz)]);

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::invD(CagmScalarField *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				allocField[fidx(kx, ky, kz)] = 1/a->allocField[fidx(kx, ky, kz)];

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::invD(void)
{
    return invD(this);
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::multD(REALTYPE_A c, CagmScalarField *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				allocField[fidx(kx, ky, kz)] = a->allocField[fidx(kx, ky, kz)]*c;

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::multD(REALTYPE_A c)
{
    return multD(c, this);
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::multD(CagmScalarField *c, CagmScalarField *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				allocField[fidx(kx, ky, kz)] = a->allocField[fidx(kx, ky, kz)]*c->allocField[fidx(kx, ky, kz)];

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::multD(CagmScalarField *c)
{
    return multD(c, this);
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::addD(CagmScalarField *a, CagmScalarField *b)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				allocField[fidx(kx, ky, kz)] = a->allocField[fidx(kx, ky, kz)] + b->allocField[fidx(kx, ky, kz)];

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::addD(CagmScalarField *a)
{
    return addD(this, a);
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::subD(CagmScalarField *a, CagmScalarField *b)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				allocField[fidx(kx, ky, kz)] = a->allocField[fidx(kx, ky, kz)] - b->allocField[fidx(kx, ky, kz)];

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::subD(CagmScalarField *a)
{
    return subD(this, a);
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::negD(CagmScalarField *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				allocField[fidx(kx, ky, kz)] = -a->allocField[fidx(kx, ky, kz)];

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::negD()
{
    return negD(this);
}

//-----------------------------------------------------------------------
uint32_t CagmScalarField::zeroD()
{
    REALTYPE_A wsum = 0;
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
                allocField[fidx(kx, ky, kz)] = 0;

    return 0;
}

