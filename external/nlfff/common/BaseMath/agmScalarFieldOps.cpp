#include "stdDefinitions.h"
#include <math.h>

#include "agsFieldsCommon.h"
#include "agmScalarFieldOps.h"
#include "agmVectorFieldOps.h"

#define fidx(kx, ky, kz) (ky)+(kz)*N[1]][(kx)

//-----------------------------------------------------------------------
CagmScalarFieldOps::CagmScalarFieldOps(int *_N, int *_DphysL, int *_DphysH)
{
    Initialize(_N);

    setDPhys(_DphysL, _DphysH);
}

//-----------------------------------------------------------------------
CagmScalarFieldOps::CagmScalarFieldOps(const CagmScalarFieldOps& from)
{
    Initialize((int *)from.N, (int *)from.NphysL, (int *)from.NphysH, (REALTYPE_A *)from.step);
}

//-----------------------------------------------------------------------
CagmScalarFieldOps::~CagmScalarFieldOps()
{
    Delete();
}

//-----------------------------------------------------------------------
CagmScalarFieldOps& CagmScalarFieldOps::operator=(const CagmScalarFieldOps& from)
{
    if (this == &from)
        return *this;

    Delete();
    Initialize((int *)from.N, (int *)from.NphysL, (int *)from.NphysH, (REALTYPE_A *)from.step);

    return *this;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::Delete()
{
    delete [] field;

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::Initialize(int *_N, int *_NphysL, int *_NphysH, REALTYPE_A *_step)
{
    N[0] = _N[0]; N[1] = _N[1]; N[2] = _N[2];
    field = new REALTYPE_A * [N[1]*N[2]];

    setNPhys(_NphysL, _NphysH);

    if (_step)
        SetSteps(_step);
    else
    {
        step[0] = 1.0;
        step[1] = 1.0;
        step[2] = 1.0;
    }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::setDPhys(int *_DphysL, int *_DphysH)
{
    if (_DphysL)
    {
        NphysL[0] = _DphysL[0]; NphysL[1] = _DphysL[1]; NphysL[2] = _DphysL[2];
    }
    if (_DphysH)
    {
        NphysH[0] = N[0]-_DphysH[0]; NphysH[1] = N[1]-_DphysH[1]; NphysH[2] = N[2]-_DphysH[2];
    }

	return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::setNPhys(int *_NphysL, int *_NphysH)
{
    if (_NphysL)
    {
        NphysL[0] = _NphysL[0]; NphysL[1] = _NphysL[1]; NphysL[2] = _NphysL[2];
    }
    else
    {
        NphysL[0] =    0; NphysL[1] =    0; NphysL[2] =    0;
    }
    if (_NphysH)
    {
        NphysH[0] = _NphysH[0]; NphysH[1] = _NphysH[1]; NphysH[2] = _NphysH[2];
    }
    else
    {
        NphysH[0] = N[0]; NphysH[1] = N[1]; NphysH[2] = N[2];
    }

	return 0;
}

////-----------------------------------------------------------------------
//REALTYPE_A CagmScalarFieldOps::getElement(int kx, int ky, int kz)
//{
//    return field[fidx(kx, ky, kz)];
//}

//-----------------------------------------------------------------------
REALTYPE_A *CagmScalarFieldOps::getAddress(int kx, int ky, int kz)
{
    return &field[fidx(kx, ky, kz)];
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::SetMargins(CagmScalarFieldOps *source, int *Mmin, int *_DphysL, int *_DphysH)
{
    setDPhys(_DphysL, _DphysH);

    int ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            field[ky + kz*N[1]] = source->getAddress(Mmin[0], ky+Mmin[1], kz+Mmin[2]);

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::SetSteps(REALTYPE_A *_step)
{
    step[0] = _step[0];
    step[1] = _step[1];
    step[2] = _step[2];

    return 0;
}

//-----------------------------------------------------------------------
REALTYPE_A * CagmScalarFieldOps::GetSteps()
{
    return step;
}


////-----------------------------------------------------------------------
//uint32_t CagmScalarFieldOps::set(CagmScalarFieldOps *a)
//{
//	// check equiv. sizes!
//	int kx, ky, kz;
//	for (kx = NphysL[0]; kx < NphysH[0]; kx++)
//		for (ky = NphysL[1]; ky < NphysH[1]; ky++)
//			for (kz = NphysL[2]; kz < NphysH[2]; kz++)
//				field[fidx(kx, ky, kz)] = a->field[fidx(kx, ky, kz)];
//
//    return 0;
//}

//-----------------------------------------------------------------------
REALTYPE_A CagmScalarFieldOps::derivative(int kx, int ky, int kz, int dir)
{
    REALTYPE_A d;
    if (dir == 0)
    {
        if (kx == 0)
            d = -3*field[fidx(0, ky, kz)] + 4*field[fidx(1, ky, kz)] - field[fidx(2, ky, kz)];
        else if (kx == N[0]-1)
            d = field[fidx(N[0]-3, ky, kz)] - 4*field[fidx(N[0]-2, ky, kz)] + 3*field[fidx(N[0]-1, ky, kz)];
        else
            d = field[fidx(kx+1, ky, kz)] - field[fidx(kx-1, ky, kz)];
        return d*0.5*step[0];
    }
    else if (dir == 1)
    {
        if (ky == 0)
            d = -3*field[fidx(kx, 0, kz)] + 4*field[fidx(kx, 1, kz)] - field[fidx(kx, 2, kz)];
        else if (ky == N[1]-1)
            d = field[fidx(kx, N[1]-3, kz)] - 4*field[fidx(kx, N[1]-2, kz)] + 3*field[fidx(kx, N[1]-1, kz)];
        else
            d = field[fidx(kx, ky+1, kz)] - field[fidx(kx, ky-1, kz)];
        return d*0.5*step[1];
    }
    else
    {
        if (kz == 0)
            d = -3*field[fidx(kx, ky, 0)] + 4*field[fidx(kx, ky, 1)] - field[fidx(kx, ky, 2)];
        else if (kz == N[2]-1)
            d = field[fidx(kx, ky, N[2]-3)] - 4*field[fidx(kx, ky, N[2]-2)] + 3*field[fidx(kx, ky, N[2]-1)];
        else
            d = field[fidx(kx, ky, kz+1)] - field[fidx(kx, ky, kz-1)];
        return d*0.5*step[2];
    }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::div(CagmVectorFieldOps *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    REALTYPE_A dx, dy, dz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                if (kx == 0)
                    dx = - 3*a->fieldX[fidx(0, ky, kz)] + 4*a->fieldX[fidx(1, ky, kz)] - a->fieldX[fidx(2, ky, kz)];
                else if (kx == N[0]-1)
                    dx = a->fieldX[fidx(N[0]-3, ky, kz)] - 4*a->fieldX[fidx(N[0]-2, ky, kz)] + 3*a->fieldX[fidx(N[0]-1, ky, kz)];
                else
                    dx = a->fieldX[fidx(kx+1, ky, kz)] - a->fieldX[fidx(kx-1, ky, kz)];

                if (ky == 0)
                    dy = - 3*a->fieldY[fidx(kx, 0, kz)] + 4*a->fieldY[fidx(kx, 1, kz)] - a->fieldY[fidx(kx, 2, kz)];
                else if (ky == N[1]-1)
                    dy = a->fieldY[fidx(kx, N[1]-3, kz)] - 4*a->fieldY[fidx(kx, N[1]-2, kz)] + 3*a->fieldY[fidx(kx, N[1]-1, kz)];
                else
                    dy = a->fieldY[fidx(kx, ky+1, kz)] - a->fieldY[fidx(kx, ky-1, kz)];

                if (kz == 0)
                    dz = - 3*a->fieldZ[fidx(kx, ky, 0)] + 4*a->fieldZ[fidx(kx, ky, 1)] - a->fieldZ[fidx(kx, ky, 2)];
                else if (kz == N[2]-1)
                    dz = a->fieldZ[fidx(kx, ky, N[2]-3)] - 4*a->fieldZ[fidx(kx, ky, N[2]-2)] + 3*a->fieldZ[fidx(kx, ky, N[2]-1)];
                else
                    dz = a->fieldZ[fidx(kx, ky, kz+1)] - a->fieldZ[fidx(kx, ky, kz-1)];

				field[fidx(kx, ky, kz)] = (dx*step[0] + dy*step[1] + dz*step[2]) * 0.5;
				//field[fidx(kx, ky, kz)] = (dx + dy + dz) * 0.5;
            }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::div31(CagmVectorFieldOps *a)
{
    // check equiv. sizes!
    int kx, ky, kz;
    REALTYPE_A dx, dy, dz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                if (kx == 0)
                    dx = - 3*a->fieldX[fidx(0, ky, kz)] + 4*a->fieldX[fidx(1, ky, kz)] - a->fieldX[fidx(2, ky, kz)];
                else if (kx == N[0]-1)
                    dx = a->fieldX[fidx(N[0]-3, ky, kz)] - 4*a->fieldX[fidx(N[0]-2, ky, kz)] + 3*a->fieldX[fidx(N[0]-1, ky, kz)];
                else
                    dx = a->fieldX[fidx(kx+1, ky, kz)] - a->fieldX[fidx(kx-1, ky, kz)];

                if (ky == 0)
                    dy = - 3*a->fieldY[fidx(kx, 0, kz)] + 4*a->fieldY[fidx(kx, 1, kz)] - a->fieldY[fidx(kx, 2, kz)];
                else if (ky == N[1]-1)
                    dy = a->fieldY[fidx(kx, N[1]-3, kz)] - 4*a->fieldY[fidx(kx, N[1]-2, kz)] + 3*a->fieldY[fidx(kx, N[1]-1, kz)];
                else
                    dy = a->fieldY[fidx(kx, ky+1, kz)] - a->fieldY[fidx(kx, ky-1, kz)];

                if (kz == 0)
                    dz = - 3*a->fieldZ[fidx(kx, ky, kz)] + 4*a->fieldZ[fidx(kx, ky, kz+1)] - a->fieldZ[fidx(kx, ky, kz+2)];
                else if (kz == 1)
                    dz = a->fieldZ[fidx(kx, ky, kz+1)] - a->fieldZ[fidx(kx, ky, kz-1)];
                else
                    dz = a->fieldZ[fidx(kx, ky, kz-2)] - 4*a->fieldZ[fidx(kx, ky, kz-1)] + 3*a->fieldZ[fidx(kx, ky, kz)];

                field[fidx(kx, ky, kz)] = (dx*step[0] + dy*step[1] + dz*step[2]) * 0.5;
                //field[fidx(kx, ky, kz)] = (dx + dy + dz) * 0.5;
            }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::div42(CagmVectorFieldOps *a)
{
    // check equiv. sizes!
    int kx, ky, kz;
    REALTYPE_A dx, dy, dz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                if (kx == 0)
                    dx = - 3*a->fieldX[fidx(0, ky, kz)] + 4*a->fieldX[fidx(1, ky, kz)] - a->fieldX[fidx(2, ky, kz)];
                else if (kx == N[0]-1)
                    dx = a->fieldX[fidx(N[0]-3, ky, kz)] - 4*a->fieldX[fidx(N[0]-2, ky, kz)] + 3*a->fieldX[fidx(N[0]-1, ky, kz)];
                else
                    dx = a->fieldX[fidx(kx+1, ky, kz)] - a->fieldX[fidx(kx-1, ky, kz)];

                if (ky == 0)
                    dy = - 3*a->fieldY[fidx(kx, 0, kz)] + 4*a->fieldY[fidx(kx, 1, kz)] - a->fieldY[fidx(kx, 2, kz)];
                else if (ky == N[1]-1)
                    dy = a->fieldY[fidx(kx, N[1]-3, kz)] - 4*a->fieldY[fidx(kx, N[1]-2, kz)] + 3*a->fieldY[fidx(kx, N[1]-1, kz)];
                else
                    dy = a->fieldY[fidx(kx, ky+1, kz)] - a->fieldY[fidx(kx, ky-1, kz)];

                if (kz == 0)
                    dz = - 3*a->fieldZ[fidx(kx, ky, kz)] + 4*a->fieldZ[fidx(kx, ky, kz+1)] - a->fieldZ[fidx(kx, ky, kz+2)];
                else if (kz == N[2]-1)
                    dz = a->fieldZ[fidx(kx, ky, kz-2)] - 4*a->fieldZ[fidx(kx, ky, kz-1)] + 3*a->fieldZ[fidx(kx, ky, kz)];
                else if (kz == N[2]-2)
                    dz = a->fieldZ[fidx(kx, ky, kz+1)] - a->fieldZ[fidx(kx, ky, kz-1)];
                else
                    dz = (-2*a->fieldZ[fidx(kx, ky, kz-1)] - 3*a->fieldZ[fidx(kx, ky, kz)] + 6*a->fieldZ[fidx(kx, ky, kz+1)] - a->fieldZ[fidx(kx, ky, kz+2)])/3.0;

                field[fidx(kx, ky, kz)] = (dx*step[0] + dy*step[1] + dz*step[2]) * 0.5;
                //field[fidx(kx, ky, kz)] = (dx + dy + dz) * 0.5;
            }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::div41(CagmVectorFieldOps *a)
{
    // check equiv. sizes!
    int kx, ky, kz;
    REALTYPE_A dx, dy, dz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                if (kx == 0)
                    dx = - 3*a->fieldX[fidx(0, ky, kz)] + 4*a->fieldX[fidx(1, ky, kz)] - a->fieldX[fidx(2, ky, kz)];
                else if (kx == N[0]-1)
                    dx = a->fieldX[fidx(N[0]-3, ky, kz)] - 4*a->fieldX[fidx(N[0]-2, ky, kz)] + 3*a->fieldX[fidx(N[0]-1, ky, kz)];
                else
                    dx = a->fieldX[fidx(kx+1, ky, kz)] - a->fieldX[fidx(kx-1, ky, kz)];

                if (ky == 0)
                    dy = - 3*a->fieldY[fidx(kx, 0, kz)] + 4*a->fieldY[fidx(kx, 1, kz)] - a->fieldY[fidx(kx, 2, kz)];
                else if (ky == N[1]-1)
                    dy = a->fieldY[fidx(kx, N[1]-3, kz)] - 4*a->fieldY[fidx(kx, N[1]-2, kz)] + 3*a->fieldY[fidx(kx, N[1]-1, kz)];
                else
                    dy = a->fieldY[fidx(kx, ky+1, kz)] - a->fieldY[fidx(kx, ky-1, kz)];

                if (kz == 0)
                    dz = - 3*a->fieldZ[fidx(kx, ky, kz)] + 4*a->fieldZ[fidx(kx, ky, kz+1)] - a->fieldZ[fidx(kx, ky, kz+2)];
                else if (kz == 1)
                    dz = a->fieldZ[fidx(kx, ky, kz+1)] - a->fieldZ[fidx(kx, ky, kz-1)];
                else if (kz == N[2]-1)
                    dz = a->fieldZ[fidx(kx, ky, kz-2)] - 4*a->fieldZ[fidx(kx, ky, kz-1)] + 3*a->fieldZ[fidx(kx, ky, kz)];
                else
                    dz = (a->fieldZ[fidx(kx, ky, kz-2)] - 6*a->fieldZ[fidx(kx, ky, kz-1)] + 3*a->fieldZ[fidx(kx, ky, kz)] + 2*a->fieldZ[fidx(kx, ky, kz+1)])/3.0;

                field[fidx(kx, ky, kz)] = (dx*step[0] + dy*step[1] + dz*step[2]) * 0.5;
                //field[fidx(kx, ky, kz)] = (dx + dy + dz) * 0.5;
            }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::div5(CagmVectorFieldOps *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    REALTYPE_A dx, dy, dz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                if (kx == 0)
                    dx = - 25*a->fieldX[fidx(0, ky, kz)] + 48*a->fieldX[fidx(1, ky, kz)] - 36*a->fieldX[fidx(2, ky, kz)] + 16*a->fieldX[fidx(3, ky, kz)] - 3*a->fieldX[fidx(4, ky, kz)];
                else if (kx == 1)
                    dx = -  3*a->fieldX[fidx(0, ky, kz)] - 10*a->fieldX[fidx(1, ky, kz)] + 18*a->fieldX[fidx(2, ky, kz)] -  6*a->fieldX[fidx(3, ky, kz)] +   a->fieldX[fidx(4, ky, kz)];
                else if (kx == N[0]-2)
                    dx = -  a->fieldX[fidx(N[0]-5, ky, kz)] +  6*a->fieldX[fidx(N[0]-4, ky, kz)] - 18*a->fieldX[fidx(N[0]-3, ky, kz)] + 10*a->fieldX[fidx(N[0]-2, ky, kz)] +  3*a->fieldX[fidx(N[0]-1, ky, kz)];
                else if (kx == N[0]-1)
                    dx =  3*a->fieldX[fidx(N[0]-5, ky, kz)] - 16*a->fieldX[fidx(N[0]-4, ky, kz)] + 36*a->fieldX[fidx(N[0]-3, ky, kz)] - 48*a->fieldX[fidx(N[0]-2, ky, kz)] + 25*a->fieldX[fidx(N[0]-1, ky, kz)];
                else
                    dx = - a->fieldX[fidx(kx+2, ky, kz)] + 8*a->fieldX[fidx(kx+1, ky, kz)] - 8*a->fieldX[fidx(kx-1, ky, kz)] + a->fieldX[fidx(kx-2, ky, kz)];

                if (ky == 0)
                    dy = - 25*a->fieldY[fidx(kx, 0, kz)] + 48*a->fieldY[fidx(kx, 1, kz)] - 36*a->fieldY[fidx(kx, 2, kz)] + 16*a->fieldY[fidx(kx, 3, kz)] - 3*a->fieldY[fidx(kx, 4, kz)];
                else if (ky == 1)
                    dy = -  3*a->fieldY[fidx(kx, 0, kz)] - 10*a->fieldY[fidx(kx, 1, kz)] + 18*a->fieldY[fidx(kx, 2, kz)] -  6*a->fieldY[fidx(kx, 3, kz)] +   a->fieldY[fidx(kx, 4, kz)];
                else if (ky == N[1]-2)
                    dy = -  a->fieldY[fidx(kx, N[1]-5, kz)] +  6*a->fieldY[fidx(kx, N[1]-4, kz)] - 18*a->fieldY[fidx(kx, N[1]-3, kz)] + 10*a->fieldY[fidx(kx, N[1]-2, kz)] +  3*a->fieldY[fidx(kx, N[1]-1, kz)];
                else if (ky == N[1]-1)
                    dy =  3*a->fieldY[fidx(kx, N[1]-5, kz)] - 16*a->fieldY[fidx(kx, N[1]-4, kz)] + 36*a->fieldY[fidx(kx, N[1]-3, kz)] - 48*a->fieldY[fidx(kx, N[1]-2, kz)] + 25*a->fieldY[fidx(kx, N[1]-1, kz)];
                else
                    dy = - a->fieldY[fidx(kx, ky+2, kz)] + 8*a->fieldY[fidx(kx, ky+1, kz)] - 8*a->fieldY[fidx(kx, ky-1, kz)] + a->fieldY[fidx(kx, ky-2, kz)];

                if (kz == 0)
                    dz = - 25*a->fieldZ[fidx(kx, ky, 0)] + 48*a->fieldZ[fidx(kx, ky, 1)] - 36*a->fieldZ[fidx(kx, ky, 2)] + 16*a->fieldZ[fidx(kx, ky, 3)] - 3*a->fieldZ[fidx(kx, ky, 4)];
                else if (kz == 1)
                    dz = -  3*a->fieldZ[fidx(kx, ky, 0)] - 10*a->fieldZ[fidx(kx, ky, 1)] + 18*a->fieldZ[fidx(kx, ky, 2)] -  6*a->fieldZ[fidx(kx, ky, 3)] +   a->fieldZ[fidx(kx, ky, 4)];
                else if (kz == N[2]-2)
                    dz = -  a->fieldZ[fidx(kx, ky, N[2]-5)] +  6*a->fieldZ[fidx(kx, ky, N[2]-4)] - 18*a->fieldZ[fidx(kx, ky, N[2]-3)] + 10*a->fieldZ[fidx(kx, ky, N[2]-2)] +  3*a->fieldZ[fidx(kx, ky, N[2]-1)];
                else if (kz == N[2]-1)
                    dz =  3*a->fieldZ[fidx(kx, ky, N[2]-5)] - 16*a->fieldZ[fidx(kx, ky, N[2]-4)] + 36*a->fieldZ[fidx(kx, ky, N[2]-3)] - 48*a->fieldZ[fidx(kx, ky, N[2]-2)] + 25*a->fieldZ[fidx(kx, ky, N[2]-1)];
                else
                    dz = - a->fieldZ[fidx(kx, ky, kz+2)] + 8*a->fieldZ[fidx(kx, ky, kz+1)] - 8*a->fieldZ[fidx(kx, ky, kz-1)] + a->fieldZ[fidx(kx, ky, kz-2)];

				field[fidx(kx, ky, kz)] = (dx*step[0] + dy*step[1] + dz*step[2]) / 12.0;
				//field[fidx(kx, ky, kz)] = (dx + dy + dz) * 0.5;
            }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::divScheme(CagmVectorFieldOps *a, int scheme)
{
    if (scheme == 5)
        return div5(a);
    else
        return div(a);
}
    
//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::dot(CagmVectorFieldOps *a, CagmVectorFieldOps *b, CagmVectorFieldOps *Weight)
{
	// check equiv. sizes!
    REALTYPE_A w = 1;
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                if (Weight)
                    w = Weight->fieldX[fidx(kx, ky, kz)];
				field[fidx(kx, ky, kz)] = a->fieldX[fidx(kx, ky, kz)]*b->fieldX[fidx(kx, ky, kz)] *w;
            }
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                if (Weight)
                    w = Weight->fieldY[fidx(kx, ky, kz)];
				field[fidx(kx, ky, kz)] += a->fieldY[fidx(kx, ky, kz)]*b->fieldY[fidx(kx, ky, kz)] *w;
            }
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                if (Weight)
                    w = Weight->fieldZ[fidx(kx, ky, kz)];
				field[fidx(kx, ky, kz)] += a->fieldZ[fidx(kx, ky, kz)]*b->fieldZ[fidx(kx, ky, kz)] *w;
            }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::abs2(CagmVectorFieldOps *a, CagmVectorFieldOps *Weight)
{
    return dot(a, a, Weight);
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::abs(CagmVectorFieldOps *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				field[fidx(kx, ky, kz)] = sqrt(a->fieldX[fidx(kx, ky, kz)]*a->fieldX[fidx(kx, ky, kz)] +
                                               a->fieldY[fidx(kx, ky, kz)]*a->fieldY[fidx(kx, ky, kz)] +
                                               a->fieldZ[fidx(kx, ky, kz)]*a->fieldZ[fidx(kx, ky, kz)]);

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::projection(CagmVectorFieldOps *a, REALTYPE_A *d)
{
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				field[fidx(kx, ky, kz)] = a->fieldX[fidx(kx, ky, kz)] * d[0] +
                                          a->fieldY[fidx(kx, ky, kz)] * d[1] +
                                          a->fieldZ[fidx(kx, ky, kz)] * d[2];

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::inv(CagmScalarFieldOps *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
                if (a->field[fidx(kx, ky, kz)] != 0)
				    field[fidx(kx, ky, kz)] = 1.0/a->field[fidx(kx, ky, kz)];
                else
				    field[fidx(kx, ky, kz)] = 0;

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::inv(void)
{
    return inv(this);
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::mult(REALTYPE_A c, CagmScalarFieldOps *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				field[fidx(kx, ky, kz)] = a->field[fidx(kx, ky, kz)]*c;

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::mult(REALTYPE_A c)
{
    return mult(c, this);
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::mult(CagmScalarFieldOps *c, CagmScalarFieldOps *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
                if (a->field[fidx(kx, ky, kz)] == 0)
    				field[fidx(kx, ky, kz)] = 0;
                else
				    field[fidx(kx, ky, kz)] = a->field[fidx(kx, ky, kz)]*c->field[fidx(kx, ky, kz)];

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::mult(CagmScalarFieldOps *c)
{
    return mult(c, this);
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::add(CagmScalarFieldOps *a, CagmScalarFieldOps *b)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				field[fidx(kx, ky, kz)] = a->field[fidx(kx, ky, kz)] + b->field[fidx(kx, ky, kz)];

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::addPhys(CagmScalarFieldOps *a, CagmScalarFieldOps *b)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = NphysL[2]; kz < NphysH[2]; kz++)
        for (ky = NphysL[1]; ky < NphysH[1]; ky++)
            for (kx = NphysL[0]; kx < NphysH[0]; kx++)
				field[fidx(kx, ky, kz)] = a->field[fidx(kx, ky, kz)] + b->field[fidx(kx, ky, kz)];

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::add(CagmScalarFieldOps *a)
{
    return add(this, a);
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::addPhys(CagmScalarFieldOps *a)
{
    return addPhys(this, a);
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::sub(CagmScalarFieldOps *a, CagmScalarFieldOps *b)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				field[fidx(kx, ky, kz)] = a->field[fidx(kx, ky, kz)] - b->field[fidx(kx, ky, kz)];

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::sub(CagmScalarFieldOps *a)
{
    return sub(this, a);
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::neg(CagmScalarFieldOps *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				field[fidx(kx, ky, kz)] = -a->field[fidx(kx, ky, kz)];

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::neg()
{
    return neg(this);
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::acos()
{
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				field[fidx(kx, ky, kz)] = ::acos(field[fidx(kx, ky, kz)]);

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::zero()
{
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
                field[fidx(kx, ky, kz)] = 0;

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::zeroZ0()
{
	int kx, ky;
    for (ky = 0; ky < N[1]; ky++)
        for (kx = 0; kx < N[0]; kx++)
            field[fidx(kx, ky, 0)] = 0;

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::setZlevel(int level, REALTYPE_A w)
{
	int kx, ky;
    for (ky = 0; ky < N[1]; ky++)
        for (kx = 0; kx < N[0]; kx++)
            field[ky+level*N[1]][kx] = w;

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::setPlane(CagmScalarFieldOps *plane, int wplane, int from, int to)
{
	int kx, ky, kz;
    if (wplane & PLANE_Z)
    {
        for (ky = NphysL[1]; ky < NphysH[1]; ky++)
            for (kx = NphysL[0]; kx < NphysH[0]; kx++)
			    field[fidx(kx, ky, to)] = plane->field[ky+from*plane->N[1]][kx];
    }
    if (wplane & PLANE_Y)
    {
        for (kz = NphysL[2]; kz < NphysH[2]; kz++)
            for (kx = NphysL[0]; kx < NphysH[0]; kx++)
			    field[fidx(kx, to, kz)] = plane->field[from+kz*plane->N[1]][kx];
    }
    if (wplane & PLANE_X)
    {
        for (kz = NphysL[2]; kz < NphysH[2]; kz++)
            for (ky = NphysL[1]; ky < NphysH[1]; ky++)
			    field[fidx(to, ky, kz)] = plane->field[ky+kz*plane->N[1]][from];
    }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::LOS(CagmVectorFieldOps *a, REALTYPE_A *dircos)
{
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				field[fidx(kx, ky, kz)] =   a->fieldX[fidx(kx, ky, kz)]*dircos[0]
                                          + a->fieldY[fidx(kx, ky, kz)]*dircos[1]
                                          + a->fieldZ[fidx(kx, ky, kz)]*dircos[2]
                ;

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::rotate2D(CagmScalarFieldOps *ac, CagmScalarFieldOps *as, CagmScalarFieldOps *acn, CagmScalarFieldOps *asn, REALTYPE_A cosz)
{
    REALTYPE_A sinz = sqrt(1-cosz*cosz);
	int kx, ky, kz;
    int *N = ac->GetDimensions();
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
				acn->field[fidx(kx, ky, kz)] =  ac->field[fidx(kx, ky, kz)]*cosz + as->field[fidx(kx, ky, kz)]*sinz;
				asn->field[fidx(kx, ky, kz)] = -ac->field[fidx(kx, ky, kz)]*sinz + as->field[fidx(kx, ky, kz)]*cosz;
            }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::sqDiff(CagmScalarFieldOps *a1, CagmScalarFieldOps *a2)
{
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                if ( ::fabs(a1->field[fidx(kx, ky, kz)]) <= ::fabs(a2->field[fidx(kx, ky, kz)]) )
				    field[fidx(kx, ky, kz)] = 0;
                else
				    field[fidx(kx, ky, kz)] = sqrt(a1->field[fidx(kx, ky, kz)]*a1->field[fidx(kx, ky, kz)] - a2->field[fidx(kx, ky, kz)]*a2->field[fidx(kx, ky, kz)]);
            }

    return 0;
}
//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::relax(CagmScalarFieldOps *cond, CagmScalarFieldOps *weight)
{
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				field[fidx(kx, ky, kz)] = weight->field[fidx(kx, ky, kz)]*(cond->field[fidx(kx, ky, kz)] - field[fidx(kx, ky, kz)]) + field[fidx(kx, ky, kz)];

    return 0;
}

//-----------------------------------------------------------------------
REALTYPE_A CagmScalarFieldOps::sum(CagmScalarFieldOps *weight)
{
    REALTYPE_A wsum = 0;
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                if (weight)
    				wsum += field[fidx(kx, ky, kz)] * weight->field[fidx(kx, ky, kz)];
                else
    				wsum += field[fidx(kx, ky, kz)];
            }

    return wsum;
}

//-----------------------------------------------------------------------
REALTYPE_A CagmScalarFieldOps::sumPhys(CagmScalarFieldOps *weight)
{
    REALTYPE_A wsum = 0;
	int kx, ky, kz;
    for (kz = NphysL[2]; kz < NphysH[2]; kz++)
        for (ky = NphysL[1]; ky < NphysH[1]; ky++)
            for (kx = NphysL[0]; kx < NphysH[0]; kx++)
            {
                if (weight)
    				wsum += field[fidx(kx, ky, kz)] * weight->field[fidx(kx, ky, kz)];
                else
    				wsum += field[fidx(kx, ky, kz)];
            }

    return wsum;
}

//-----------------------------------------------------------------------
REALTYPE_A CagmScalarFieldOps::sumPhysW(CagmScalarFieldOps *weight)
{
    REALTYPE_A wsum = 0;
	int kx, ky, kz;
    for (kz = weight->NphysL[2]; kz < weight->NphysH[2]; kz++)
        for (ky = weight->NphysL[1]; ky < weight->NphysH[1]; ky++)
            for (kx = weight->NphysL[0]; kx < weight->NphysH[0]; kx++)
                wsum += field[fidx(kx, ky, kz)] * weight->field[fidx(kx, ky, kz)];

    return wsum;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::power(REALTYPE_A pw)
{
    return power(this, pw);
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::power(CagmScalarFieldOps *a, REALTYPE_A pw)
{
    REALTYPE_A wsum = 0;
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
                field[fidx(kx, ky, kz)] = pow(a->field[fidx(kx, ky, kz)], pw);

    return 0;
}

//-----------------------------------------------------------------------
REALTYPE_A CagmScalarFieldOps::avPhys(CagmScalarFieldOps *weight)
{
    return sumPhys(weight)/((NphysH[0]-NphysL[0]+1)*(NphysH[1]-NphysL[1]+1)*(NphysH[2]-NphysL[2]+1));
}

//-----------------------------------------------------------------------
REALTYPE_A CagmScalarFieldOps::maxval(void)
{
    REALTYPE_A wmax = 0;
	int kx, ky, kz;
    for (kz = NphysL[2]; kz < NphysH[2]; kz++)
        for (ky = NphysL[1]; ky < NphysH[1]; ky++)
            for (kx = NphysL[0]; kx < NphysH[0]; kx++)
                if (field[fidx(kx, ky, kz)] > wmax)
    				wmax = field[fidx(kx, ky, kz)];

    return wmax;
}

//-----------------------------------------------------------------------
uint32_t CagmScalarFieldOps::limWeight(int limType, CagmScalarFieldOps *calc, CagmScalarFieldOps *cond)
{
    if (limType != 0)
    {
	    int kx, ky, kz;
        for (kz = 0; kz < N[2]; kz++)
            for (ky = 0; ky < N[1]; ky++)
                for (kx = 0; kx < N[0]; kx++)
                    if (limType < 0 && calc->field[fidx(kx, ky, kz)] < cond->field[fidx(kx, ky, kz)])
                        field[fidx(kx, ky, kz)] = 0;
                    else if (limType > 0 && calc->field[fidx(kx, ky, kz)] > cond->field[fidx(kx, ky, kz)])
                        field[fidx(kx, ky, kz)] = 0;
    }

    return 0;
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/*
void CagmScalarFieldOps::indices(int p, int Np, int q, int Nq, int *p1, int *p2, REALTYPE_A *pf, int *q1, int *q2, REALTYPE_A *qf)
{
    *p1 = p-1; *p2 = p+1; *pf = 0.5;
    *q1 = q-1; *q2 = q+1; *qf = 0.5;
    if (p == 0)
        *p1 = 0, *p2 = 1, *pf = 1.0;
    else if (p == Np-1)
        *p1 = Np-2, *p2 = Np-1, *pf = 1.0;
    if (q == 0)
        *q1 = 0, *q2 = 1, *qf = 1.0;
    else if (q == Nq-1)
        *q1 = Nq-2, *q2 = Nq-1, *qf = 1.0;
}
*/

// div
/*
    int p1, p2, q1, q2;
    REALTYPE_A pf, qf;
	kx = 0;
	for (ky = 0; ky < N[1]; ky++)
		for (kz = 0; kz < N[2]; kz++)
        {
            indices(ky, N[1], kz, N[2], &p1, &p2, &pf, &q1, &q2, &qf);
			field[fidx(kx, ky, kz)] = (a->fieldX[fidx(1, ky, kz)]-a->fieldX[fidx(0, ky, kz)]) +
                                      (a->fieldY[fidx(kx, p2, kz)]-a->fieldY[fidx(kx, p1, kz)]) * pf +
                                      (a->fieldZ[fidx(kx, ky, q2)]-a->fieldZ[fidx(kx, ky, q1)]) * qf;
        }
	kx = N[0]-1;
	for (ky = 0; ky < N[1]; ky++)
		for (kz = 0; kz < N[2]; kz++)
        {
            indices(ky, N[1], kz, N[2], &p1, &p2, &pf, &q1, &q2, &qf);
			field[fidx(kx, ky, kz)] = (a->fieldX[fidx(N[0]-1, ky, kz)]-a->fieldX[fidx(N[0]-2, ky, kz)]) +
                                      (a->fieldY[fidx(kx, p2, kz)]-a->fieldY[fidx(kx, p1, kz)]) * pf +
                                      (a->fieldZ[fidx(kx, ky, q2)]-a->fieldZ[fidx(kx, ky, q1)]) * qf;
        }
	ky = 0;
	for (kx = 0; kx < N[0]; kx++)
		for (kz = 0; kz < N[2]; kz++)
        {
            indices(kx, N[0], kz, N[2], &p1, &p2, &pf, &q1, &q2, &qf);
			field[fidx(kx, ky, kz)] = (a->fieldX[fidx(p2, ky, kz)]-a->fieldX[fidx(p1, ky, kz)]) * pf +
                                      (a->fieldY[fidx(kx, 1, kz)]-a->fieldY[fidx(kx, 0, kz)]) +
                                      (a->fieldZ[fidx(kx, ky, q2)]-a->fieldZ[fidx(kx, ky, q1)]) * qf;
        }
	ky = N[1]-1;
	for (kx = 0; kx < N[0]; kx++)
		for (kz = 0; kz < N[2]; kz++)
        {
            indices(kx, N[0], kz, N[2], &p1, &p2, &pf, &q1, &q2, &qf);
			field[fidx(kx, ky, kz)] = (a->fieldX[fidx(p2, ky, kz)]-a->fieldX[fidx(p1, ky, kz)]) * pf +
                                      (a->fieldY[fidx(kx, N[1]-1, kz)]-a->fieldY[fidx(kx, N[1]-2, kz)]) +
                                      (a->fieldZ[fidx(kx, ky, q2)]-a->fieldZ[fidx(kx, ky, q1)]) * qf;
        }
	kz = 0;
	for (kx = 0; kx < N[0]; kx++)
		for (ky = 0; ky < N[1]; ky++)
        {
            indices(kx, N[0], ky, N[1], &p1, &p2, &pf, &q1, &q2, &qf);
			field[fidx(kx, ky, kz)] = (a->fieldX[fidx(p2, ky, kz)]-a->fieldX[fidx(p1, ky, kz)]) * pf +
                                      (a->fieldY[fidx(kx, q2, kz)]-a->fieldY[fidx(kx, q1, kz)]) * qf +
                                      (a->fieldZ[fidx(kx, ky, 1)]-a->fieldZ[fidx(kx, ky, 0)]);
        }
	ky = N[1]-1;
	for (kx = 0; kx < N[0]; kx++)
		for (ky = 0; ky < N[1]; ky++)
        {
            indices(kx, N[0], ky, N[1], &p1, &p2, &pf, &q1, &q2, &qf);
			field[fidx(kx, ky, kz)] = (a->fieldX[fidx(p2, ky, kz)]-a->fieldX[fidx(p1, ky, kz)]) * pf +
                                      (a->fieldY[fidx(kx, q2, kz)]-a->fieldY[fidx(kx, q1, kz)]) * qf +
                                      (a->fieldZ[fidx(kx, ky, N[2]-1)]-a->fieldZ[fidx(kx, ky, N[2]-2)]);
        }
*/