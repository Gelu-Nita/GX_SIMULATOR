#include "stdDefinitions.h"
#include <math.h>

#include "agsFieldsCommon.h"
#include "agmVectorFieldOps.h"
#include "agmScalarFieldOps.h"

#include "agmRotate3D.h"

#define fidx(kx, ky, kz) (ky)+(kz)*N[1]][(kx)

//-----------------------------------------------------------------------
CagmVectorFieldOps::CagmVectorFieldOps(int *_N, int *_DphysL, int *_DphysH)
{
    Initialize(_N);

    setDPhys(_DphysL, _DphysH);
}

//-----------------------------------------------------------------------
CagmVectorFieldOps::CagmVectorFieldOps(const CagmVectorFieldOps& from)
{
    Initialize((int *)from.N, (int *)from.NphysL, (int *)from.NphysH, (REALTYPE_A *)from.step);
}

//-----------------------------------------------------------------------
CagmVectorFieldOps::~CagmVectorFieldOps()
{
    Delete();
}

//-----------------------------------------------------------------------
CagmVectorFieldOps& CagmVectorFieldOps::operator=(const CagmVectorFieldOps& from)
{
    if (this == &from)
        return *this;

    Delete();
    Initialize((int *)from.N, (int *)from.NphysL, (int *)from.NphysH, (REALTYPE_A *)from.step);

    return *this;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::Delete()
{
    delete [] fieldX;
    delete [] fieldY;
    delete [] fieldZ;

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::Initialize(int *_N, int *_NphysL, int *_NphysH, REALTYPE_A *_step)
{
	N[0] = _N[0];
	N[1] = _N[1];
	N[2] = _N[2];
    fieldX = new REALTYPE_A * [N[1]*N[2]];
    fieldY = new REALTYPE_A * [N[1]*N[2]];
    fieldZ = new REALTYPE_A * [N[1]*N[2]];

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
uint32_t CagmVectorFieldOps::setDPhys(int *_DphysL, int *_DphysH)
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
uint32_t CagmVectorFieldOps::setNPhys(int *_NphysL, int *_NphysH)
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

//-----------------------------------------------------------------------
REALTYPE_A *CagmVectorFieldOps::getAddress(int v, int kx, int ky, int kz)
{
    if (v == 0)
        return &fieldX[fidx(kx, ky, kz)];
    else if (v == 1)
        return &fieldY[fidx(kx, ky, kz)];
    else
        return &fieldZ[fidx(kx, ky, kz)];
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::SetMargins(CagmVectorFieldOps *source, int *Mmin, int *_DphysL, int *_DphysH)
{
    setDPhys(_DphysL, _DphysH);

    int ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
        {
            fieldX[ky + kz*N[1]] = source->getAddress(0, Mmin[0], ky+Mmin[1], kz+Mmin[2]);
            fieldY[ky + kz*N[1]] = source->getAddress(1, Mmin[0], ky+Mmin[1], kz+Mmin[2]);
            fieldZ[ky + kz*N[1]] = source->getAddress(2, Mmin[0], ky+Mmin[1], kz+Mmin[2]);
        }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::setRefField(REALTYPE_A *X, REALTYPE_A *Y, REALTYPE_A *Z, int *M)
{
    for (int kz = 0; kz < N[2]; kz++)
        for (int ky = 0; ky < N[1]; ky++)
        {
            fieldX[ky + kz*N[1]] = X + (ky + kz*N[1])*N[0];
            fieldY[ky + kz*N[1]] = Y + (ky + kz*N[1])*N[0];
            fieldZ[ky + kz*N[1]] = Z + (ky + kz*N[1])*N[0];
        }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::SetSteps(REALTYPE_A *_step)
{
    step[0] = _step[0];
    step[1] = _step[1];
    step[2] = _step[2];

    return 0;
}

//-----------------------------------------------------------------------
REALTYPE_A * CagmVectorFieldOps::GetSteps()
{
    return step;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::blockF(CagmVectorFieldOps *_B, CagmScalarFieldOps *_n, CagmScalarFieldOps *_w, CagmVectorFieldOps *_gradw)
{
    int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                REALTYPE_A w = _w->field[fidx(kx, ky, kz)];
                REALTYPE_A dw_dx = _gradw->fieldX[fidx(kx, ky, kz)];
                REALTYPE_A dw_dy = _gradw->fieldY[fidx(kx, ky, kz)];
                REALTYPE_A dw_dz = _gradw->fieldZ[fidx(kx, ky, kz)];
                REALTYPE_A Z = _n->field[fidx(kx, ky, kz)];
                REALTYPE_A dZ_dx = _n->derivative(kx, ky, kz, 0);
                REALTYPE_A dZ_dy = _n->derivative(kx, ky, kz, 1);
                REALTYPE_A dZ_dz = _n->derivative(kx, ky, kz, 2);

                //REALTYPE_A dBx_dx = derivative(kx, ky, kz, 0, 0);
            }


    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::cross(CagmVectorFieldOps *a, const CagmVectorFieldOps *b)
{
	// check equiv. sizes!
	int kx, ky, kz;
    REALTYPE_A tx, ty, tz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				tx = a->fieldY[fidx(kx, ky, kz)]*b->fieldZ[fidx(kx, ky, kz)] - a->fieldZ[fidx(kx, ky, kz)]*b->fieldY[fidx(kx, ky, kz)];
				ty = a->fieldZ[fidx(kx, ky, kz)]*b->fieldX[fidx(kx, ky, kz)] - a->fieldX[fidx(kx, ky, kz)]*b->fieldZ[fidx(kx, ky, kz)];
				tz = a->fieldX[fidx(kx, ky, kz)]*b->fieldY[fidx(kx, ky, kz)] - a->fieldY[fidx(kx, ky, kz)]*b->fieldX[fidx(kx, ky, kz)];
				fieldX[fidx(kx, ky, kz)] = tx;
				fieldY[fidx(kx, ky, kz)] = ty;
				fieldZ[fidx(kx, ky, kz)] = tz;
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::cross(CagmVectorFieldOps *a)
{
    return cross(this, a);
}

////-----------------------------------------------------------------------
//CagmVectorFieldOps& CagmVectorFieldOps::operator*(const CagmVectorFieldOps& a)
//{
//    cross(this, &a);
//
//    return *this;
//}
//

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::rot(CagmVectorFieldOps *a)
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
                    zx = - 3*a->fieldZ[fidx(0, ky, kz)] + 4*a->fieldZ[fidx(1, ky, kz)] - a->fieldZ[fidx(2, ky, kz)];
                    yx = - 3*a->fieldY[fidx(0, ky, kz)] + 4*a->fieldY[fidx(1, ky, kz)] - a->fieldY[fidx(2, ky, kz)];
                }
                else if (kx == N[0]-1)
                {
                    zx = a->fieldZ[fidx(N[0]-3, ky, kz)] - 4*a->fieldZ[fidx(N[0]-2, ky, kz)] + 3*a->fieldZ[fidx(N[0]-1, ky, kz)];
                    yx = a->fieldY[fidx(N[0]-3, ky, kz)] - 4*a->fieldY[fidx(N[0]-2, ky, kz)] + 3*a->fieldY[fidx(N[0]-1, ky, kz)];
                }
                else
                {
                    zx = a->fieldZ[fidx(kx+1, ky, kz)] - a->fieldZ[fidx(kx-1, ky, kz)];
                    yx = a->fieldY[fidx(kx+1, ky, kz)] - a->fieldY[fidx(kx-1, ky, kz)];
                }

                if (ky == 0)
                {
                    zy = - 3*a->fieldZ[fidx(kx, 0, kz)] + 4*a->fieldZ[fidx(kx, 1, kz)] - a->fieldZ[fidx(kx, 2, kz)];
                    xy = - 3*a->fieldX[fidx(kx, 0, kz)] + 4*a->fieldX[fidx(kx, 1, kz)] - a->fieldX[fidx(kx, 2, kz)];
                }
                else if (ky == N[1]-1)
                {
                    zy = a->fieldZ[fidx(kx, N[1]-3, kz)] - 4*a->fieldZ[fidx(kx, N[1]-2, kz)] + 3*a->fieldZ[fidx(kx, N[1]-1, kz)];
                    xy = a->fieldX[fidx(kx, N[1]-3, kz)] - 4*a->fieldX[fidx(kx, N[1]-2, kz)] + 3*a->fieldX[fidx(kx, N[1]-1, kz)];
                }
                else
                {
                    zy = a->fieldZ[fidx(kx, ky+1, kz)] - a->fieldZ[fidx(kx, ky-1, kz)];
                    xy = a->fieldX[fidx(kx, ky+1, kz)] - a->fieldX[fidx(kx, ky-1, kz)];
                }

                if (kz == 0)
                {
                    xz = - 3*a->fieldX[fidx(kx, ky, 0)] + 4*a->fieldX[fidx(kx, ky, 1)] - a->fieldX[fidx(kx, ky, 2)];
                    yz = - 3*a->fieldY[fidx(kx, ky, 0)] + 4*a->fieldY[fidx(kx, ky, 1)] - a->fieldY[fidx(kx, ky, 2)];
                }
                else if (kz == N[2]-1)
                {
                    xz = a->fieldX[fidx(kx, ky, N[2]-3)] - 4*a->fieldX[fidx(kx, ky, N[2]-2)] + 3*a->fieldX[fidx(kx, ky, N[2]-1)];
                    yz = a->fieldY[fidx(kx, ky, N[2]-3)] - 4*a->fieldY[fidx(kx, ky, N[2]-2)] + 3*a->fieldY[fidx(kx, ky, N[2]-1)];
                }
                else
                {
                    xz = a->fieldX[fidx(kx, ky, kz+1)] - a->fieldX[fidx(kx, ky, kz-1)];
                    yz = a->fieldY[fidx(kx, ky, kz+1)] - a->fieldY[fidx(kx, ky, kz-1)];
                }

				fieldX[fidx(kx, ky, kz)] = (zy*step[1] - yz*step[2])*0.5;
				fieldY[fidx(kx, ky, kz)] = (xz*step[2] - zx*step[0])*0.5;
				fieldZ[fidx(kx, ky, kz)] = (yx*step[0] - xy*step[1])*0.5;
				//fieldX[fidx(kx, ky, kz)] = (zy - yz)*0.5;
				//fieldY[fidx(kx, ky, kz)] = (xz - zx)*0.5;
				//fieldZ[fidx(kx, ky, kz)] = (yx - xy)*0.5;
			}


    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::rot31(CagmVectorFieldOps *a)
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
                    zx = - 3*a->fieldZ[fidx(0, ky, kz)] + 4*a->fieldZ[fidx(1, ky, kz)] - a->fieldZ[fidx(2, ky, kz)];
                    yx = - 3*a->fieldY[fidx(0, ky, kz)] + 4*a->fieldY[fidx(1, ky, kz)] - a->fieldY[fidx(2, ky, kz)];
                }
                else if (kx == N[0]-1)
                {
                    zx = a->fieldZ[fidx(N[0]-3, ky, kz)] - 4*a->fieldZ[fidx(N[0]-2, ky, kz)] + 3*a->fieldZ[fidx(N[0]-1, ky, kz)];
                    yx = a->fieldY[fidx(N[0]-3, ky, kz)] - 4*a->fieldY[fidx(N[0]-2, ky, kz)] + 3*a->fieldY[fidx(N[0]-1, ky, kz)];
                }
                else
                {
                    zx = a->fieldZ[fidx(kx+1, ky, kz)] - a->fieldZ[fidx(kx-1, ky, kz)];
                    yx = a->fieldY[fidx(kx+1, ky, kz)] - a->fieldY[fidx(kx-1, ky, kz)];
                }

                if (ky == 0)
                {
                    zy = - 3*a->fieldZ[fidx(kx, 0, kz)] + 4*a->fieldZ[fidx(kx, 1, kz)] - a->fieldZ[fidx(kx, 2, kz)];
                    xy = - 3*a->fieldX[fidx(kx, 0, kz)] + 4*a->fieldX[fidx(kx, 1, kz)] - a->fieldX[fidx(kx, 2, kz)];
                }
                else if (ky == N[1]-1)
                {
                    zy = a->fieldZ[fidx(kx, N[1]-3, kz)] - 4*a->fieldZ[fidx(kx, N[1]-2, kz)] + 3*a->fieldZ[fidx(kx, N[1]-1, kz)];
                    xy = a->fieldX[fidx(kx, N[1]-3, kz)] - 4*a->fieldX[fidx(kx, N[1]-2, kz)] + 3*a->fieldX[fidx(kx, N[1]-1, kz)];
                }
                else
                {
                    zy = a->fieldZ[fidx(kx, ky+1, kz)] - a->fieldZ[fidx(kx, ky-1, kz)];
                    xy = a->fieldX[fidx(kx, ky+1, kz)] - a->fieldX[fidx(kx, ky-1, kz)];
                }

                if (kz == 0)
                {
                    xz = - 3*a->fieldX[fidx(kx, ky, kz)] + 4*a->fieldX[fidx(kx, ky, kz+1)] - a->fieldX[fidx(kx, ky, kz+2)];
                    yz = - 3*a->fieldY[fidx(kx, ky, kz)] + 4*a->fieldY[fidx(kx, ky, kz+1)] - a->fieldY[fidx(kx, ky, kz+2)];
                }
                else if (kz == 1)
                {
                    xz = a->fieldX[fidx(kx, ky, kz+1)] - a->fieldX[fidx(kx, ky, kz-1)];
                    yz = a->fieldY[fidx(kx, ky, kz+1)] - a->fieldY[fidx(kx, ky, kz-1)];
                }
                else
                {
                    xz = a->fieldX[fidx(kx, ky, kz-2)] - 4*a->fieldX[fidx(kx, ky, kz-1)] + 3*a->fieldX[fidx(kx, ky, kz)];
                    yz = a->fieldY[fidx(kx, ky, kz-2)] - 4*a->fieldY[fidx(kx, ky, kz-1)] + 3*a->fieldY[fidx(kx, ky, kz)];
                }

                fieldX[fidx(kx, ky, kz)] = (zy*step[1] - yz*step[2])*0.5;
                fieldY[fidx(kx, ky, kz)] = (xz*step[2] - zx*step[0])*0.5;
                fieldZ[fidx(kx, ky, kz)] = (yx*step[0] - xy*step[1])*0.5;
                //fieldX[fidx(kx, ky, kz)] = (zy - yz)*0.5;
                //fieldY[fidx(kx, ky, kz)] = (xz - zx)*0.5;
                //fieldZ[fidx(kx, ky, kz)] = (yx - xy)*0.5;
            }


    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::rot42(CagmVectorFieldOps *a)
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
                    zx = - 3*a->fieldZ[fidx(0, ky, kz)] + 4*a->fieldZ[fidx(1, ky, kz)] - a->fieldZ[fidx(2, ky, kz)];
                    yx = - 3*a->fieldY[fidx(0, ky, kz)] + 4*a->fieldY[fidx(1, ky, kz)] - a->fieldY[fidx(2, ky, kz)];
                }
                else if (kx == N[0]-1)
                {
                    zx = a->fieldZ[fidx(N[0]-3, ky, kz)] - 4*a->fieldZ[fidx(N[0]-2, ky, kz)] + 3*a->fieldZ[fidx(N[0]-1, ky, kz)];
                    yx = a->fieldY[fidx(N[0]-3, ky, kz)] - 4*a->fieldY[fidx(N[0]-2, ky, kz)] + 3*a->fieldY[fidx(N[0]-1, ky, kz)];
                }
                else
                {
                    zx = a->fieldZ[fidx(kx+1, ky, kz)] - a->fieldZ[fidx(kx-1, ky, kz)];
                    yx = a->fieldY[fidx(kx+1, ky, kz)] - a->fieldY[fidx(kx-1, ky, kz)];
                }

                if (ky == 0)
                {
                    zy = - 3*a->fieldZ[fidx(kx, 0, kz)] + 4*a->fieldZ[fidx(kx, 1, kz)] - a->fieldZ[fidx(kx, 2, kz)];
                    xy = - 3*a->fieldX[fidx(kx, 0, kz)] + 4*a->fieldX[fidx(kx, 1, kz)] - a->fieldX[fidx(kx, 2, kz)];
                }
                else if (ky == N[1]-1)
                {
                    zy = a->fieldZ[fidx(kx, N[1]-3, kz)] - 4*a->fieldZ[fidx(kx, N[1]-2, kz)] + 3*a->fieldZ[fidx(kx, N[1]-1, kz)];
                    xy = a->fieldX[fidx(kx, N[1]-3, kz)] - 4*a->fieldX[fidx(kx, N[1]-2, kz)] + 3*a->fieldX[fidx(kx, N[1]-1, kz)];
                }
                else
                {
                    zy = a->fieldZ[fidx(kx, ky+1, kz)] - a->fieldZ[fidx(kx, ky-1, kz)];
                    xy = a->fieldX[fidx(kx, ky+1, kz)] - a->fieldX[fidx(kx, ky-1, kz)];
                }

                if (kz == 0)
                {
                    xz = - 3*a->fieldX[fidx(kx, ky, kz)] + 4*a->fieldX[fidx(kx, ky, kz+1)] - a->fieldX[fidx(kx, ky, kz+2)];
                    yz = - 3*a->fieldY[fidx(kx, ky, kz)] + 4*a->fieldY[fidx(kx, ky, kz+1)] - a->fieldY[fidx(kx, ky, kz+2)];
                }
                else if (kz == N[2]-1)
                {
                    xz = a->fieldX[fidx(kx, ky, kz-2)] - 4*a->fieldX[fidx(kx, ky, kz-1)] + 3*a->fieldX[fidx(kx, ky, kz)];
                    yz = a->fieldY[fidx(kx, ky, kz-2)] - 4*a->fieldY[fidx(kx, ky, kz-1)] + 3*a->fieldY[fidx(kx, ky, kz)];
                }
                else if (kz == N[2]-2)
                {
                    xz = a->fieldX[fidx(kx, ky, kz+1)] - a->fieldX[fidx(kx, ky, kz-1)];
                    yz = a->fieldY[fidx(kx, ky, kz+1)] - a->fieldY[fidx(kx, ky, kz-1)];
                }
                else
                {
                    xz = (-2*a->fieldX[fidx(kx, ky, kz-1)] - 3*a->fieldX[fidx(kx, ky, kz)] + 6*a->fieldX[fidx(kx, ky, kz+1)] - a->fieldX[fidx(kx, ky, kz+2)])/3.0;
                    yz = (-2*a->fieldY[fidx(kx, ky, kz-1)] - 3*a->fieldY[fidx(kx, ky, kz)] + 6*a->fieldY[fidx(kx, ky, kz+1)] - a->fieldY[fidx(kx, ky, kz+2)])/3.0;
                }

                fieldX[fidx(kx, ky, kz)] = (zy*step[1] - yz*step[2])*0.5;
                fieldY[fidx(kx, ky, kz)] = (xz*step[2] - zx*step[0])*0.5;
                fieldZ[fidx(kx, ky, kz)] = (yx*step[0] - xy*step[1])*0.5;
                //fieldX[fidx(kx, ky, kz)] = (zy - yz)*0.5;
                //fieldY[fidx(kx, ky, kz)] = (xz - zx)*0.5;
                //fieldZ[fidx(kx, ky, kz)] = (yx - xy)*0.5;
            }


    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::rot41(CagmVectorFieldOps *a)
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
                    zx = - 3*a->fieldZ[fidx(0, ky, kz)] + 4*a->fieldZ[fidx(1, ky, kz)] - a->fieldZ[fidx(2, ky, kz)];
                    yx = - 3*a->fieldY[fidx(0, ky, kz)] + 4*a->fieldY[fidx(1, ky, kz)] - a->fieldY[fidx(2, ky, kz)];
                }
                else if (kx == N[0]-1)
                {
                    zx = a->fieldZ[fidx(N[0]-3, ky, kz)] - 4*a->fieldZ[fidx(N[0]-2, ky, kz)] + 3*a->fieldZ[fidx(N[0]-1, ky, kz)];
                    yx = a->fieldY[fidx(N[0]-3, ky, kz)] - 4*a->fieldY[fidx(N[0]-2, ky, kz)] + 3*a->fieldY[fidx(N[0]-1, ky, kz)];
                }
                else
                {
                    zx = a->fieldZ[fidx(kx+1, ky, kz)] - a->fieldZ[fidx(kx-1, ky, kz)];
                    yx = a->fieldY[fidx(kx+1, ky, kz)] - a->fieldY[fidx(kx-1, ky, kz)];
                }

                if (ky == 0)
                {
                    zy = - 3*a->fieldZ[fidx(kx, 0, kz)] + 4*a->fieldZ[fidx(kx, 1, kz)] - a->fieldZ[fidx(kx, 2, kz)];
                    xy = - 3*a->fieldX[fidx(kx, 0, kz)] + 4*a->fieldX[fidx(kx, 1, kz)] - a->fieldX[fidx(kx, 2, kz)];
                }
                else if (ky == N[1]-1)
                {
                    zy = a->fieldZ[fidx(kx, N[1]-3, kz)] - 4*a->fieldZ[fidx(kx, N[1]-2, kz)] + 3*a->fieldZ[fidx(kx, N[1]-1, kz)];
                    xy = a->fieldX[fidx(kx, N[1]-3, kz)] - 4*a->fieldX[fidx(kx, N[1]-2, kz)] + 3*a->fieldX[fidx(kx, N[1]-1, kz)];
                }
                else
                {
                    zy = a->fieldZ[fidx(kx, ky+1, kz)] - a->fieldZ[fidx(kx, ky-1, kz)];
                    xy = a->fieldX[fidx(kx, ky+1, kz)] - a->fieldX[fidx(kx, ky-1, kz)];
                }

                if (kz == 0)
                {
                    xz = - 3*a->fieldX[fidx(kx, ky, kz)] + 4*a->fieldX[fidx(kx, ky, kz+1)] - a->fieldX[fidx(kx, ky, kz+2)];
                    yz = - 3*a->fieldY[fidx(kx, ky, kz)] + 4*a->fieldY[fidx(kx, ky, kz+1)] - a->fieldY[fidx(kx, ky, kz+2)];
                }
                else if (kz == 1)
                {
                    xz = a->fieldX[fidx(kx, ky, kz+1)] - a->fieldX[fidx(kx, ky, kz-1)];
                    yz = a->fieldY[fidx(kx, ky, kz+1)] - a->fieldY[fidx(kx, ky, kz-1)];
                }
                else if (kz == N[2]-1)
                {
                    xz = a->fieldX[fidx(kx, ky, kz-2)] - 4*a->fieldX[fidx(kx, ky, kz-1)] + 3*a->fieldX[fidx(kx, ky, kz)];
                    yz = a->fieldY[fidx(kx, ky, kz-2)] - 4*a->fieldY[fidx(kx, ky, kz-1)] + 3*a->fieldY[fidx(kx, ky, kz)];
                }
                else
                {
                    xz = (a->fieldX[fidx(kx, ky, kz-2)] - 6*a->fieldX[fidx(kx, ky, kz-1)] + 3*a->fieldX[fidx(kx, ky, kz)] + 2*a->fieldX[fidx(kx, ky, kz+1)])/3.0;
                    yz = (a->fieldY[fidx(kx, ky, kz-2)] - 6*a->fieldY[fidx(kx, ky, kz-1)] + 3*a->fieldY[fidx(kx, ky, kz)] + 2*a->fieldY[fidx(kx, ky, kz+1)])/3.0;
                }

                fieldX[fidx(kx, ky, kz)] = (zy*step[1] - yz*step[2])*0.5;
                fieldY[fidx(kx, ky, kz)] = (xz*step[2] - zx*step[0])*0.5;
                fieldZ[fidx(kx, ky, kz)] = (yx*step[0] - xy*step[1])*0.5;
                //fieldX[fidx(kx, ky, kz)] = (zy - yz)*0.5;
                //fieldY[fidx(kx, ky, kz)] = (xz - zx)*0.5;
                //fieldZ[fidx(kx, ky, kz)] = (yx - xy)*0.5;
            }


    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::rot5(CagmVectorFieldOps *a)
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
                    zx = - 25*a->fieldZ[fidx(0, ky, kz)] + 48*a->fieldZ[fidx(1, ky, kz)] - 36*a->fieldZ[fidx(2, ky, kz)] + 16*a->fieldZ[fidx(3, ky, kz)] - 3*a->fieldZ[fidx(4, ky, kz)];
                    yx = - 25*a->fieldY[fidx(0, ky, kz)] + 48*a->fieldY[fidx(1, ky, kz)] - 36*a->fieldY[fidx(2, ky, kz)] + 16*a->fieldY[fidx(3, ky, kz)] - 3*a->fieldY[fidx(4, ky, kz)];
                }
                else if (kx == 1)
                {
                    zx = -  3*a->fieldZ[fidx(0, ky, kz)] - 10*a->fieldZ[fidx(1, ky, kz)] + 18*a->fieldZ[fidx(2, ky, kz)] -  6*a->fieldZ[fidx(3, ky, kz)] +   a->fieldZ[fidx(4, ky, kz)];
                    yx = -  3*a->fieldY[fidx(0, ky, kz)] - 10*a->fieldY[fidx(1, ky, kz)] + 18*a->fieldY[fidx(2, ky, kz)] -  6*a->fieldY[fidx(3, ky, kz)] +   a->fieldY[fidx(4, ky, kz)];
                }
                else if (kx == N[0]-2)
                {
                    zx = -  a->fieldZ[fidx(N[0]-5, ky, kz)] +  6*a->fieldZ[fidx(N[0]-4, ky, kz)] - 18*a->fieldZ[fidx(N[0]-3, ky, kz)] + 10*a->fieldZ[fidx(N[0]-2, ky, kz)] +  3*a->fieldZ[fidx(N[0]-1, ky, kz)];
                    yx = -  a->fieldY[fidx(N[0]-5, ky, kz)] +  6*a->fieldY[fidx(N[0]-4, ky, kz)] - 18*a->fieldY[fidx(N[0]-3, ky, kz)] + 10*a->fieldY[fidx(N[0]-2, ky, kz)] +  3*a->fieldY[fidx(N[0]-1, ky, kz)];
                }
                else if (kx == N[0]-1)
                {
                    zx =  3*a->fieldZ[fidx(N[0]-5, ky, kz)] - 16*a->fieldZ[fidx(N[0]-4, ky, kz)] + 36*a->fieldZ[fidx(N[0]-3, ky, kz)] - 48*a->fieldZ[fidx(N[0]-2, ky, kz)] + 25*a->fieldZ[fidx(N[0]-1, ky, kz)];
                    yx =  3*a->fieldY[fidx(N[0]-5, ky, kz)] - 16*a->fieldY[fidx(N[0]-4, ky, kz)] + 36*a->fieldY[fidx(N[0]-3, ky, kz)] - 48*a->fieldY[fidx(N[0]-2, ky, kz)] + 25*a->fieldY[fidx(N[0]-1, ky, kz)];
                }
                else
                {
                    zx = - a->fieldZ[fidx(kx+2, ky, kz)] + 8*a->fieldZ[fidx(kx+1, ky, kz)] - 8*a->fieldZ[fidx(kx-1, ky, kz)] + a->fieldZ[fidx(kx-2, ky, kz)];
                    yx = - a->fieldY[fidx(kx+2, ky, kz)] + 8*a->fieldY[fidx(kx+1, ky, kz)] - 8*a->fieldY[fidx(kx-1, ky, kz)] + a->fieldY[fidx(kx-2, ky, kz)];
                }

                if (ky == 0)
                {
                    zy = - 25*a->fieldZ[fidx(kx, 0, kz)] + 48*a->fieldZ[fidx(kx, 1, kz)] - 36*a->fieldZ[fidx(kx, 2, kz)] + 16*a->fieldZ[fidx(kx, 3, kz)] - 3*a->fieldZ[fidx(kx, 4, kz)];
                    xy = - 25*a->fieldX[fidx(kx, 0, kz)] + 48*a->fieldX[fidx(kx, 1, kz)] - 36*a->fieldX[fidx(kx, 2, kz)] + 16*a->fieldX[fidx(kx, 3, kz)] - 3*a->fieldX[fidx(kx, 4, kz)];
                }
                else if (ky == 1)
                {
                    zy = -  3*a->fieldZ[fidx(kx, 0, kz)] - 10*a->fieldZ[fidx(kx, 1, kz)] + 18*a->fieldZ[fidx(kx, 2, kz)] -  6*a->fieldZ[fidx(kx, 3, kz)] +   a->fieldZ[fidx(kx, 4, kz)];
                    xy = -  3*a->fieldX[fidx(kx, 0, kz)] - 10*a->fieldX[fidx(kx, 1, kz)] + 18*a->fieldX[fidx(kx, 2, kz)] -  6*a->fieldX[fidx(kx, 3, kz)] +   a->fieldX[fidx(kx, 4, kz)];
                }
                else if (ky == N[1]-2)
                {
                    zy = -  a->fieldZ[fidx(kx, N[1]-5, kz)] +  6*a->fieldZ[fidx(kx, N[1]-4, kz)] - 18*a->fieldZ[fidx(kx, N[1]-3, kz)] + 10*a->fieldZ[fidx(kx, N[1]-2, kz)] +  3*a->fieldZ[fidx(kx, N[1]-1, kz)];
                    xy = -  a->fieldX[fidx(kx, N[1]-5, kz)] +  6*a->fieldX[fidx(kx, N[1]-4, kz)] - 18*a->fieldX[fidx(kx, N[1]-3, kz)] + 10*a->fieldX[fidx(kx, N[1]-2, kz)] +  3*a->fieldX[fidx(kx, N[1]-1, kz)];
                }
                else if (ky == N[1]-1)
                {
                    zy =  3*a->fieldZ[fidx(kx, N[1]-5, kz)] - 16*a->fieldZ[fidx(kx, N[1]-4, kz)] + 36*a->fieldZ[fidx(kx, N[1]-3, kz)] - 48*a->fieldZ[fidx(kx, N[1]-2, kz)] + 25*a->fieldZ[fidx(kx, N[1]-1, kz)];
                    xy =  3*a->fieldX[fidx(kx, N[1]-5, kz)] - 16*a->fieldX[fidx(kx, N[1]-4, kz)] + 36*a->fieldX[fidx(kx, N[1]-3, kz)] - 48*a->fieldX[fidx(kx, N[1]-2, kz)] + 25*a->fieldX[fidx(kx, N[1]-1, kz)];
                }
                else
                {
                    zy = - a->fieldZ[fidx(kx, ky+2, kz)] + 8*a->fieldZ[fidx(kx, ky+1, kz)] - 8*a->fieldZ[fidx(kx, ky-1, kz)] + a->fieldZ[fidx(kx, ky-2, kz)];
                    xy = - a->fieldX[fidx(kx, ky+2, kz)] + 8*a->fieldX[fidx(kx, ky+1, kz)] - 8*a->fieldX[fidx(kx, ky-1, kz)] + a->fieldX[fidx(kx, ky-2, kz)];
                }

                if (kz == 0)
                {
                    xz = - 25*a->fieldX[fidx(kx, ky, 0)] + 48*a->fieldX[fidx(kx, ky, 1)] - 36*a->fieldX[fidx(kx, ky, 2)] + 16*a->fieldX[fidx(kx, ky, 3)] - 3*a->fieldX[fidx(kx, ky, 4)];
                    yz = - 25*a->fieldY[fidx(kx, ky, 0)] + 48*a->fieldY[fidx(kx, ky, 1)] - 36*a->fieldY[fidx(kx, ky, 2)] + 16*a->fieldY[fidx(kx, ky, 3)] - 3*a->fieldY[fidx(kx, ky, 4)];
                }
                else if (kz == 1)
                {
                    xz = -  3*a->fieldX[fidx(kx, ky, 0)] - 10*a->fieldX[fidx(kx, ky, 1)] + 18*a->fieldX[fidx(kx, ky, 2)] -  6*a->fieldX[fidx(kx, ky, 3)] +   a->fieldX[fidx(kx, ky, 4)];
                    yz = -  3*a->fieldY[fidx(kx, ky, 0)] - 10*a->fieldY[fidx(kx, ky, 1)] + 18*a->fieldY[fidx(kx, ky, 2)] -  6*a->fieldY[fidx(kx, ky, 3)] +   a->fieldY[fidx(kx, ky, 4)];
                }
                else if (kz == N[2]-2)
                {
                    xz = -  a->fieldX[fidx(kx, ky, N[2]-5)] +  6*a->fieldX[fidx(kx, ky, N[2]-4)] - 18*a->fieldX[fidx(kx, ky, N[2]-3)] + 10*a->fieldX[fidx(kx, ky, N[2]-2)] +  3*a->fieldX[fidx(kx, ky, N[2]-1)];
                    yz = -  a->fieldY[fidx(kx, ky, N[2]-5)] +  6*a->fieldY[fidx(kx, ky, N[2]-4)] - 18*a->fieldY[fidx(kx, ky, N[2]-3)] + 10*a->fieldY[fidx(kx, ky, N[2]-2)] +  3*a->fieldY[fidx(kx, ky, N[2]-1)];
                }
                else if (kz == N[2]-1)
                {
                    xz =  3*a->fieldX[fidx(kx, ky, N[2]-5)] - 16*a->fieldX[fidx(kx, ky, N[2]-4)] + 36*a->fieldX[fidx(kx, ky, N[2]-3)] - 48*a->fieldX[fidx(kx, ky, N[2]-2)] + 25*a->fieldX[fidx(kx, ky, N[2]-1)];
                    yz =  3*a->fieldY[fidx(kx, ky, N[2]-5)] - 16*a->fieldY[fidx(kx, ky, N[2]-4)] + 36*a->fieldY[fidx(kx, ky, N[2]-3)] - 48*a->fieldY[fidx(kx, ky, N[2]-2)] + 25*a->fieldY[fidx(kx, ky, N[2]-1)];
                }
                else
                {
                    xz = - a->fieldX[fidx(kx, ky, kz+2)] + 8*a->fieldX[fidx(kx, ky, kz+1)] - 8*a->fieldX[fidx(kx, ky, kz-1)] + a->fieldX[fidx(kx, ky, kz-2)];
                    yz = - a->fieldY[fidx(kx, ky, kz+2)] + 8*a->fieldY[fidx(kx, ky, kz+1)] - 8*a->fieldY[fidx(kx, ky, kz-1)] + a->fieldY[fidx(kx, ky, kz-2)];
                }

				fieldX[fidx(kx, ky, kz)] = (zy*step[1] - yz*step[2]) / 12.0;
				fieldY[fidx(kx, ky, kz)] = (xz*step[2] - zx*step[0]) / 12.0;
				fieldZ[fidx(kx, ky, kz)] = (yx*step[0] - xy*step[1]) / 12.0;
				//fieldX[fidx(kx, ky, kz)] = (zy - yz)*0.5;
				//fieldY[fidx(kx, ky, kz)] = (xz - zx)*0.5;
				//fieldZ[fidx(kx, ky, kz)] = (yx - xy)*0.5;
			}


    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::rotScheme(CagmVectorFieldOps *a, int scheme)
{
    if (scheme == 5)
        return rot5(a);
    else
        return rot(a);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::grad(CagmScalarFieldOps *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    REALTYPE_A dx, dy, dz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
                if (kx == 0)
                    dx = - 3*a->field[fidx(0, ky, kz)] + 4*a->field[fidx(1, ky, kz)] - a->field[fidx(2, ky, kz)];
                else if (kx == N[0]-1)
                    dx = a->field[fidx(N[0]-3, ky, kz)] - 4*a->field[fidx(N[0]-2, ky, kz)] + 3*a->field[fidx(N[0]-1, ky, kz)];
                else
                    dx = a->field[fidx(kx+1, ky, kz)] - a->field[fidx(kx-1, ky, kz)];

                if (ky == 0)
                    dy = - 3*a->field[fidx(kx, 0, kz)] + 4*a->field[fidx(kx, 1, kz)] - a->field[fidx(kx, 2, kz)];
                else if (ky == N[1]-1)
                    dy = a->field[fidx(kx, N[1]-3, kz)] - 4*a->field[fidx(kx, N[1]-2, kz)] + 3*a->field[fidx(kx, N[1]-1, kz)];
                else
                    dy = a->field[fidx(kx, ky+1, kz)] - a->field[fidx(kx, ky-1, kz)];

                if (kz == 0)
                    dz = - 3*a->field[fidx(kx, ky, 0)] + 4*a->field[fidx(kx, ky, 1)] - a->field[fidx(kx, ky, 2)];
                else if (kz == N[2]-1)
                    dz = a->field[fidx(kx, ky, N[2]-3)] - 4*a->field[fidx(kx, ky, N[2]-2)] + 3*a->field[fidx(kx, ky, N[2]-1)];
                else
                    dz = a->field[fidx(kx, ky, kz+1)] - a->field[fidx(kx, ky, kz-1)];

				fieldX[fidx(kx, ky, kz)] = dx*0.5*step[0];
				fieldY[fidx(kx, ky, kz)] = dy*0.5*step[1];
				fieldZ[fidx(kx, ky, kz)] = dz*0.5*step[2];
				//fieldX[fidx(kx, ky, kz)] = dx*0.5;
				//fieldY[fidx(kx, ky, kz)] = dy*0.5;
				//fieldZ[fidx(kx, ky, kz)] = dz*0.5;
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::grad31(CagmScalarFieldOps *a)
{
    // check equiv. sizes!
    int kx, ky, kz;
    REALTYPE_A dx, dy, dz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                if (kx == 0)
                    dx = - 3*a->field[fidx(0, ky, kz)] + 4*a->field[fidx(1, ky, kz)] - a->field[fidx(2, ky, kz)];
                else if (kx == N[0]-1)
                    dx = a->field[fidx(N[0]-3, ky, kz)] - 4*a->field[fidx(N[0]-2, ky, kz)] + 3*a->field[fidx(N[0]-1, ky, kz)];
                else
                    dx = a->field[fidx(kx+1, ky, kz)] - a->field[fidx(kx-1, ky, kz)];

                if (ky == 0)
                    dy = - 3*a->field[fidx(kx, 0, kz)] + 4*a->field[fidx(kx, 1, kz)] - a->field[fidx(kx, 2, kz)];
                else if (ky == N[1]-1)
                    dy = a->field[fidx(kx, N[1]-3, kz)] - 4*a->field[fidx(kx, N[1]-2, kz)] + 3*a->field[fidx(kx, N[1]-1, kz)];
                else
                    dy = a->field[fidx(kx, ky+1, kz)] - a->field[fidx(kx, ky-1, kz)];

                if (kz == 0)
                    dz = - 3*a->field[fidx(kx, ky, kz)] + 4*a->field[fidx(kx, ky, kz+1)] - a->field[fidx(kx, ky, kz+2)];
                else if (kz == 1)
                    dz = a->field[fidx(kx, ky, kz+1)] - a->field[fidx(kx, ky, kz-1)];
                else
                    dz = a->field[fidx(kx, ky, kz-2)] - 4*a->field[fidx(kx, ky, kz-1)] + 3*a->field[fidx(kx, ky, kz)];

                fieldX[fidx(kx, ky, kz)] = dx*0.5*step[0];
                fieldY[fidx(kx, ky, kz)] = dy*0.5*step[1];
                fieldZ[fidx(kx, ky, kz)] = dz*0.5*step[2];
                //fieldX[fidx(kx, ky, kz)] = dx*0.5;
                //fieldY[fidx(kx, ky, kz)] = dy*0.5;
                //fieldZ[fidx(kx, ky, kz)] = dz*0.5;
            }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::grad42(CagmScalarFieldOps *a)
{
    // check equiv. sizes!
    int kx, ky, kz;
    REALTYPE_A dx, dy, dz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                if (kx == 0)
                    dx = - 3*a->field[fidx(0, ky, kz)] + 4*a->field[fidx(1, ky, kz)] - a->field[fidx(2, ky, kz)];
                else if (kx == N[0]-1)
                    dx = a->field[fidx(N[0]-3, ky, kz)] - 4*a->field[fidx(N[0]-2, ky, kz)] + 3*a->field[fidx(N[0]-1, ky, kz)];
                else
                    dx = a->field[fidx(kx+1, ky, kz)] - a->field[fidx(kx-1, ky, kz)];

                if (ky == 0)
                    dy = - 3*a->field[fidx(kx, 0, kz)] + 4*a->field[fidx(kx, 1, kz)] - a->field[fidx(kx, 2, kz)];
                else if (ky == N[1]-1)
                    dy = a->field[fidx(kx, N[1]-3, kz)] - 4*a->field[fidx(kx, N[1]-2, kz)] + 3*a->field[fidx(kx, N[1]-1, kz)];
                else
                    dy = a->field[fidx(kx, ky+1, kz)] - a->field[fidx(kx, ky-1, kz)];

                if (kz == 0)
                    dz = - 3*a->field[fidx(kx, ky, kz)] + 4*a->field[fidx(kx, ky, kz+1)] - a->field[fidx(kx, ky, kz+2)];
                else if (kz == N[2]-1)
                    dz = a->field[fidx(kx, ky, kz-2)] - 4*a->field[fidx(kx, ky, kz-1)] + 3*a->field[fidx(kx, ky, kz)];
                else if (kz == N[2]-2)
                    dz = a->field[fidx(kx, ky, kz+1)] - a->field[fidx(kx, ky, kz-1)];
                else
                    dz = (-2*a->field[fidx(kx, ky, kz-1)] - 3*a->field[fidx(kx, ky, kz)] + 6*a->field[fidx(kx, ky, kz+1)] - a->field[fidx(kx, ky, kz+2)])/3.0;

                fieldX[fidx(kx, ky, kz)] = dx*0.5*step[0];
                fieldY[fidx(kx, ky, kz)] = dy*0.5*step[1];
                fieldZ[fidx(kx, ky, kz)] = dz*0.5*step[2];
                //fieldX[fidx(kx, ky, kz)] = dx*0.5;
                //fieldY[fidx(kx, ky, kz)] = dy*0.5;
                //fieldZ[fidx(kx, ky, kz)] = dz*0.5;
            }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::grad41(CagmScalarFieldOps *a)
{
    // check equiv. sizes!
    int kx, ky, kz;
    REALTYPE_A dx, dy, dz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                if (kx == 0)
                    dx = - 3*a->field[fidx(0, ky, kz)] + 4*a->field[fidx(1, ky, kz)] - a->field[fidx(2, ky, kz)];
                else if (kx == N[0]-1)
                    dx = a->field[fidx(N[0]-3, ky, kz)] - 4*a->field[fidx(N[0]-2, ky, kz)] + 3*a->field[fidx(N[0]-1, ky, kz)];
                else
                    dx = a->field[fidx(kx+1, ky, kz)] - a->field[fidx(kx-1, ky, kz)];

                if (ky == 0)
                    dy = - 3*a->field[fidx(kx, 0, kz)] + 4*a->field[fidx(kx, 1, kz)] - a->field[fidx(kx, 2, kz)];
                else if (ky == N[1]-1)
                    dy = a->field[fidx(kx, N[1]-3, kz)] - 4*a->field[fidx(kx, N[1]-2, kz)] + 3*a->field[fidx(kx, N[1]-1, kz)];
                else
                    dy = a->field[fidx(kx, ky+1, kz)] - a->field[fidx(kx, ky-1, kz)];

                if (kz == 0)
                    dz = - 3*a->field[fidx(kx, ky, kz)] + 4*a->field[fidx(kx, ky, kz+1)] - a->field[fidx(kx, ky, kz+2)];
                else if (kz == 1)
                    dz = a->field[fidx(kx, ky, kz+1)] - a->field[fidx(kx, ky, kz-1)];
                else if (kz == N[2]-1)
                    dz = a->field[fidx(kx, ky, kz-2)] - 4*a->field[fidx(kx, ky, kz-1)] + 3*a->field[fidx(kx, ky, kz)];
                else
                    dz = (a->field[fidx(kx, ky, kz-2)] - 6*a->field[fidx(kx, ky, kz-1)] + 3*a->field[fidx(kx, ky, kz)] + 2*a->field[fidx(kx, ky, kz+1)])/3.0;

                fieldX[fidx(kx, ky, kz)] = dx*0.5*step[0];
                fieldY[fidx(kx, ky, kz)] = dy*0.5*step[1];
                fieldZ[fidx(kx, ky, kz)] = dz*0.5*step[2];
                //fieldX[fidx(kx, ky, kz)] = dx*0.5;
                //fieldY[fidx(kx, ky, kz)] = dy*0.5;
                //fieldZ[fidx(kx, ky, kz)] = dz*0.5;
            }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::grad5(CagmScalarFieldOps *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    REALTYPE_A dx, dy, dz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
                if (kx == 0)
                    dx = - 25*a->field[fidx(0, ky, kz)] + 48*a->field[fidx(1, ky, kz)] - 36*a->field[fidx(2, ky, kz)] + 16*a->field[fidx(3, ky, kz)] - 3*a->field[fidx(4, ky, kz)];
                else if (kx == 1)
                    dx = -  3*a->field[fidx(0, ky, kz)] - 10*a->field[fidx(1, ky, kz)] + 18*a->field[fidx(2, ky, kz)] -  6*a->field[fidx(3, ky, kz)] +   a->field[fidx(4, ky, kz)];
                else if (kx == N[0]-2)
                    dx = -  a->field[fidx(N[0]-5, ky, kz)] +  6*a->field[fidx(N[0]-4, ky, kz)] - 18*a->field[fidx(N[0]-3, ky, kz)] + 10*a->field[fidx(N[0]-2, ky, kz)] +  3*a->field[fidx(N[0]-1, ky, kz)];
                else if (kx == N[0]-1)
                    dx =  3*a->field[fidx(N[0]-5, ky, kz)] - 16*a->field[fidx(N[0]-4, ky, kz)] + 36*a->field[fidx(N[0]-3, ky, kz)] - 48*a->field[fidx(N[0]-2, ky, kz)] + 25*a->field[fidx(N[0]-1, ky, kz)];
                else
                    dx = - a->field[fidx(kx+2, ky, kz)] + 8*a->field[fidx(kx+1, ky, kz)] - 8*a->field[fidx(kx-1, ky, kz)] + a->field[fidx(kx-2, ky, kz)];

                if (ky == 0)
                    dy = - 25*a->field[fidx(kx, 0, kz)] + 48*a->field[fidx(kx, 1, kz)] - 36*a->field[fidx(kx, 2, kz)] + 16*a->field[fidx(kx, 3, kz)] - 3*a->field[fidx(kx, 4, kz)];
                else if (ky == 1)
                    dy = -  3*a->field[fidx(kx, 0, kz)] - 10*a->field[fidx(kx, 1, kz)] + 18*a->field[fidx(kx, 2, kz)] -  6*a->field[fidx(kx, 3, kz)] +   a->field[fidx(kx, 4, kz)];
                else if (ky == N[1]-2)
                    dy = -  a->field[fidx(kx, N[1]-5, kz)] +  6*a->field[fidx(kx, N[1]-4, kz)] - 18*a->field[fidx(kx, N[1]-3, kz)] + 10*a->field[fidx(kx, N[1]-2, kz)] +  3*a->field[fidx(kx, N[1]-1, kz)];
                else if (ky == N[1]-1)
                    dy =  3*a->field[fidx(kx, N[1]-5, kz)] - 16*a->field[fidx(kx, N[1]-4, kz)] + 36*a->field[fidx(kx, N[1]-3, kz)] - 48*a->field[fidx(kx, N[1]-2, kz)] + 25*a->field[fidx(kx, N[1]-1, kz)];
                else
                    dy = - a->field[fidx(kx, ky+2, kz)] + 8*a->field[fidx(kx, ky+1, kz)] - 8*a->field[fidx(kx, ky-1, kz)] + a->field[fidx(kx, ky-2, kz)];

                if (kz == 0)
                    dz = - 25*a->field[fidx(kx, ky, 0)] + 48*a->field[fidx(kx, ky, 1)] - 36*a->field[fidx(kx, ky, 2)] + 16*a->field[fidx(kx, ky, 3)] - 3*a->field[fidx(kx, ky, 4)];
                else if (kz == 1)
                    dz = -  3*a->field[fidx(kx, ky, 0)] - 10*a->field[fidx(kx, ky, 1)] + 18*a->field[fidx(kx, ky, 2)] -  6*a->field[fidx(kx, ky, 3)] +   a->field[fidx(kx, ky, 4)];
                else if (kz == N[2]-2)
                    dz = -  a->field[fidx(kx, ky, N[2]-5)] +  6*a->field[fidx(kx, ky, N[2]-4)] - 18*a->field[fidx(kx, ky, N[2]-3)] + 10*a->field[fidx(kx, ky, N[2]-2)] +  3*a->field[fidx(kx, ky, N[2]-1)];
                else if (kz == N[2]-1)
                    dz =  3*a->field[fidx(kx, ky, N[2]-5)] - 16*a->field[fidx(kx, ky, N[2]-4)] + 36*a->field[fidx(kx, ky, N[2]-3)] - 48*a->field[fidx(kx, ky, N[2]-2)] + 25*a->field[fidx(kx, ky, N[2]-1)];
                else
                    dz = - a->field[fidx(kx, ky, kz+2)] + 8*a->field[fidx(kx, ky, kz+1)] - 8*a->field[fidx(kx, ky, kz-1)] + a->field[fidx(kx, ky, kz-2)];

				fieldX[fidx(kx, ky, kz)] = dx*step[0] / 12.0;
				fieldY[fidx(kx, ky, kz)] = dy*step[1] / 12.0;
				fieldZ[fidx(kx, ky, kz)] = dz*step[2] / 12.0;
				//fieldX[fidx(kx, ky, kz)] = dx*0.5;
				//fieldY[fidx(kx, ky, kz)] = dy*0.5;
				//fieldZ[fidx(kx, ky, kz)] = dz*0.5;
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::gradScheme(CagmScalarFieldOps *a, int scheme)
{
    if (scheme == 5)
        return grad5(a);
    else
        return grad(a);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::mult(REALTYPE_A c, CagmVectorFieldOps *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				fieldX[fidx(kx, ky, kz)] = a->fieldX[fidx(kx, ky, kz)]*c;
				fieldY[fidx(kx, ky, kz)] = a->fieldY[fidx(kx, ky, kz)]*c;
				fieldZ[fidx(kx, ky, kz)] = a->fieldZ[fidx(kx, ky, kz)]*c;
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::mult(REALTYPE_A c)
{
    return mult(c, this);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::mult(CagmScalarFieldOps *c, CagmVectorFieldOps *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				fieldX[fidx(kx, ky, kz)] = (a->fieldX[fidx(kx, ky, kz)]) * (c->field[fidx(kx, ky, kz)]);
				fieldY[fidx(kx, ky, kz)] = (a->fieldY[fidx(kx, ky, kz)]) * (c->field[fidx(kx, ky, kz)]);
				fieldZ[fidx(kx, ky, kz)] = (a->fieldZ[fidx(kx, ky, kz)]) * (c->field[fidx(kx, ky, kz)]);
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::multPhys(CagmScalarFieldOps *c, CagmVectorFieldOps *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = NphysL[2]; kz < NphysH[2]; kz++)
        for (ky = NphysL[1]; ky < NphysH[1]; ky++)
            for (kx = NphysL[0]; kx < NphysH[0]; kx++)
			{
				fieldX[fidx(kx, ky, kz)] = (a->fieldX[fidx(kx, ky, kz)]) * (c->field[fidx(kx, ky, kz)]);
				fieldY[fidx(kx, ky, kz)] = (a->fieldY[fidx(kx, ky, kz)]) * (c->field[fidx(kx, ky, kz)]);
				fieldZ[fidx(kx, ky, kz)] = (a->fieldZ[fidx(kx, ky, kz)]) * (c->field[fidx(kx, ky, kz)]);
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::multPhys(CagmVectorFieldOps *v, CagmVectorFieldOps *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = NphysL[2]; kz < NphysH[2]; kz++)
        for (ky = NphysL[1]; ky < NphysH[1]; ky++)
            for (kx = NphysL[0]; kx < NphysH[0]; kx++)
			{
				fieldX[fidx(kx, ky, kz)] = (a->fieldX[fidx(kx, ky, kz)]) * (v->fieldX[fidx(kx, ky, kz)]);
				fieldY[fidx(kx, ky, kz)] = (a->fieldY[fidx(kx, ky, kz)]) * (v->fieldY[fidx(kx, ky, kz)]);
				fieldZ[fidx(kx, ky, kz)] = (a->fieldZ[fidx(kx, ky, kz)]) * (v->fieldZ[fidx(kx, ky, kz)]);
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::multPhys(CagmVectorFieldOps *v)
{
    return multPhys(v, this);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::mult(CagmVectorFieldOps *a, CagmScalarFieldOps *c)
{
    return mult(c, a);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::multPhys(CagmVectorFieldOps *a, CagmScalarFieldOps *c)
{
    return multPhys(c, a);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::mult(CagmScalarFieldOps *c)
{
    return mult(c, this);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::multPhys(CagmScalarFieldOps *c)
{
    return multPhys(c, this);
}


//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::mult(CagmVectorFieldOps *a, REALTYPE_A *d)
{
	int kx, ky, kz;
    for (kz = NphysL[2]; kz < NphysH[2]; kz++)
        for (ky = NphysL[1]; ky < NphysH[1]; ky++)
            for (kx = NphysL[0]; kx < NphysH[0]; kx++)
			{
				fieldX[fidx(kx, ky, kz)] = (a->fieldX[fidx(kx, ky, kz)]) * d[0];
				fieldY[fidx(kx, ky, kz)] = (a->fieldY[fidx(kx, ky, kz)]) * d[1];
				fieldZ[fidx(kx, ky, kz)] = (a->fieldZ[fidx(kx, ky, kz)]) * d[2];
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::add(CagmVectorFieldOps *a, CagmVectorFieldOps *b)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				fieldX[fidx(kx, ky, kz)] = a->fieldX[fidx(kx, ky, kz)] + b->fieldX[fidx(kx, ky, kz)];
				fieldY[fidx(kx, ky, kz)] = a->fieldY[fidx(kx, ky, kz)] + b->fieldY[fidx(kx, ky, kz)];
				fieldZ[fidx(kx, ky, kz)] = a->fieldZ[fidx(kx, ky, kz)] + b->fieldZ[fidx(kx, ky, kz)];
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::addPhys(CagmVectorFieldOps *a, CagmVectorFieldOps *b)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = NphysL[2]; kz < NphysH[2]; kz++)
        for (ky = NphysL[1]; ky < NphysH[1]; ky++)
            for (kx = NphysL[0]; kx < NphysH[0]; kx++)
			{
				fieldX[fidx(kx, ky, kz)] = a->fieldX[fidx(kx, ky, kz)] + b->fieldX[fidx(kx, ky, kz)];
				fieldY[fidx(kx, ky, kz)] = a->fieldY[fidx(kx, ky, kz)] + b->fieldY[fidx(kx, ky, kz)];
				fieldZ[fidx(kx, ky, kz)] = a->fieldZ[fidx(kx, ky, kz)] + b->fieldZ[fidx(kx, ky, kz)];
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::add(CagmVectorFieldOps *a)
{
    return add(this, a);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::addPhys(CagmVectorFieldOps *a)
{
    return addPhys(this, a);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::sub(CagmVectorFieldOps *a, CagmVectorFieldOps *b)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				fieldX[fidx(kx, ky, kz)] = a->fieldX[fidx(kx, ky, kz)] - b->fieldX[fidx(kx, ky, kz)];
				fieldY[fidx(kx, ky, kz)] = a->fieldY[fidx(kx, ky, kz)] - b->fieldY[fidx(kx, ky, kz)];
				fieldZ[fidx(kx, ky, kz)] = a->fieldZ[fidx(kx, ky, kz)] - b->fieldZ[fidx(kx, ky, kz)];
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::subPhys(CagmVectorFieldOps *a, CagmVectorFieldOps *b)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = NphysL[2]; kz < NphysH[2]; kz++)
        for (ky = NphysL[1]; ky < NphysH[1]; ky++)
            for (kx = NphysL[0]; kx < NphysH[0]; kx++)
			{
				fieldX[fidx(kx, ky, kz)] = a->fieldX[fidx(kx, ky, kz)] - b->fieldX[fidx(kx, ky, kz)];
				fieldY[fidx(kx, ky, kz)] = a->fieldY[fidx(kx, ky, kz)] - b->fieldY[fidx(kx, ky, kz)];
				fieldZ[fidx(kx, ky, kz)] = a->fieldZ[fidx(kx, ky, kz)] - b->fieldZ[fidx(kx, ky, kz)];
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::sub(CagmVectorFieldOps *a)
{
    return sub(this, a);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::subPhys(CagmVectorFieldOps *a)
{
    return subPhys(this, a);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::neg(CagmVectorFieldOps *a)
{
	// check equiv. sizes!
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				fieldX[fidx(kx, ky, kz)] = -a->fieldX[fidx(kx, ky, kz)];
				fieldY[fidx(kx, ky, kz)] = -a->fieldY[fidx(kx, ky, kz)];
				fieldZ[fidx(kx, ky, kz)] = -a->fieldZ[fidx(kx, ky, kz)];
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::neg()
{
    return neg(this);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::zero()
{
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				fieldX[fidx(kx, ky, kz)] = 0;
				fieldY[fidx(kx, ky, kz)] = 0;
				fieldZ[fidx(kx, ky, kz)] = 0;
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::shift(int n)
{
    int from0 = 0;
    int from = from0;
    int to0 = N[2];
    int to = to0;
    if (n > 0)
        from += n;
    else
        to += n;

	int kx, ky, kz;
    for (kz = from; kz < to; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				fieldX[fidx(kx, ky, kz)] = fieldX[fidx(kx, ky, kz-n)];
				fieldY[fidx(kx, ky, kz)] = fieldY[fidx(kx, ky, kz-n)];
				fieldZ[fidx(kx, ky, kz)] = fieldY[fidx(kx, ky, kz-n)];
			}
    for (kz = from0; kz < from; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				fieldX[fidx(kx, ky, kz)] = 0;
				fieldY[fidx(kx, ky, kz)] = 0;
				fieldZ[fidx(kx, ky, kz)] = 0;
			}
    for (kz = to+1; kz < to0; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				fieldX[fidx(kx, ky, kz)] = 0;
				fieldY[fidx(kx, ky, kz)] = 0;
				fieldZ[fidx(kx, ky, kz)] = 0;
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::setZlevel(int wplane, int level, REALTYPE_A w)
{
	int kx, ky;
    for (ky = 0; ky < N[1]; ky++)
        for (kx = 0; kx < N[0]; kx++)
		{
            if (wplane & PLANE_X)
			    fieldX[ky+level*N[1]][kx] = w;
            if (wplane & PLANE_Y)
			    fieldY[ky+level*N[1]][kx] = w;
            if (wplane & PLANE_Z)
			    fieldZ[ky+level*N[1]][kx] = w;
		}

    return 0;
}


//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::setVector(REALTYPE_A *d)
{
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
				fieldX[fidx(kx, ky, kz)] = d[0];
				fieldY[fidx(kx, ky, kz)] = d[1];
				fieldZ[fidx(kx, ky, kz)] = d[2];
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::getPlane(CagmVectorFieldOps *plane, int wplane, int from, int to)
{
	int kx, ky, kz;
    if (wplane & PLANE_Z)
    {
        for (ky = NphysL[1]; ky < NphysH[1]; ky++)
            for (kx = NphysL[0]; kx < NphysH[0]; kx++)
		    {
			    plane->fieldX[ky+to*plane->N[1]][kx] = fieldX[fidx(kx, ky, from)];
			    plane->fieldY[ky+to*plane->N[1]][kx] = fieldY[fidx(kx, ky, from)];
			    plane->fieldZ[ky+to*plane->N[1]][kx] = fieldZ[fidx(kx, ky, from)];
		    }
    }
    if (wplane & PLANE_Y)
    {
        for (kz = NphysL[2]; kz < NphysH[2]; kz++)
            for (kx = NphysL[0]; kx < NphysH[0]; kx++)
		    {
			    plane->fieldX[to+kz*plane->N[1]][kx] = fieldX[fidx(kx, from, kz)];
			    plane->fieldY[to+kz*plane->N[1]][kx] = fieldY[fidx(kx, from, kz)];
			    plane->fieldZ[to+kz*plane->N[1]][kx] = fieldZ[fidx(kx, from, kz)];
		    }
    }
    if (wplane & PLANE_X)
    {
        for (kz = NphysL[2]; kz < NphysH[2]; kz++)
            for (ky = NphysL[1]; ky < NphysH[1]; ky++)
		    {
			    plane->fieldX[ky+kz*plane->N[1]][to] = fieldX[fidx(from, ky, kz)];
			    plane->fieldY[ky+kz*plane->N[1]][to] = fieldY[fidx(from, ky, kz)];
			    plane->fieldZ[ky+kz*plane->N[1]][to] = fieldZ[fidx(from, ky, kz)];
		    }
    }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::setPlaneComp(CagmVectorFieldOps *plane, int wplane, int wcomp, int from, int to)
{
	int kx, ky, kz;
    if (wplane & PLANE_Z)
    {
        for (ky = NphysL[1]; ky < NphysH[1]; ky++)
            for (kx = NphysL[0]; kx < NphysH[0]; kx++)
		    {
                if (wcomp & PLANE_X)
			        fieldX[fidx(kx, ky, to)] = plane->fieldX[ky+from*plane->N[1]][kx];
                if (wcomp & PLANE_Y)
			        fieldY[fidx(kx, ky, to)] = plane->fieldY[ky+from*plane->N[1]][kx];
                if (wcomp & PLANE_Z)
    			    fieldZ[fidx(kx, ky, to)] = plane->fieldZ[ky+from*plane->N[1]][kx];
		    }
    }
    if (wplane & PLANE_Y)
    {
        for (kz = NphysL[2]; kz < NphysH[2]; kz++)
            for (kx = NphysL[0]; kx < NphysH[0]; kx++)
		    {
                if (wcomp & PLANE_X)
			        fieldX[fidx(kx, to, kz)] = plane->fieldX[from+kz*plane->N[1]][kx];
                if (wcomp & PLANE_Y)
			        fieldY[fidx(kx, to, kz)] = plane->fieldY[from+kz*plane->N[1]][kx];
                if (wcomp & PLANE_Z)
			        fieldZ[fidx(kx, to, kz)] = plane->fieldZ[from+kz*plane->N[1]][kx];
		    }
    }
    if (wplane & PLANE_X)
    {
        for (kz = NphysL[2]; kz < NphysH[2]; kz++)
            for (ky = NphysL[1]; ky < NphysH[1]; ky++)
		    {
                if (wcomp & PLANE_X)
			        fieldX[fidx(to, ky, kz)] = plane->fieldX[ky+kz*plane->N[1]][from];
                if (wcomp & PLANE_Y)
			        fieldY[fidx(to, ky, kz)] = plane->fieldY[ky+kz*plane->N[1]][from];
                if (wcomp & PLANE_Z)
			        fieldZ[fidx(to, ky, kz)] = plane->fieldZ[ky+kz*plane->N[1]][from];
		    }
    }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::setPlane(CagmVectorFieldOps *plane, int wplane, int from, int to)
{
    return setPlaneComp(plane, wplane, PLANE_X + PLANE_Y + PLANE_Z, from, to);
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::getComponent(CagmScalarFieldOps *comp, int wcomp)
{
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
                if (wcomp == PLANE_X)
			        comp->field[fidx(kx, ky, kz)] = fieldX[fidx(kx, ky, kz)];
                else if (wcomp == PLANE_Y)
			        comp->field[fidx(kx, ky, kz)] = fieldY[fidx(kx, ky, kz)];
                else if (wcomp == PLANE_Z)
			        comp->field[fidx(kx, ky, kz)] = fieldZ[fidx(kx, ky, kz)];
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::setComponent(CagmScalarFieldOps *comp, int wcomp)
{
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
			{
                if (wcomp == PLANE_X)
			        fieldX[fidx(kx, ky, kz)] = comp->field[fidx(kx, ky, kz)];
                else if (wcomp == PLANE_Y)
			        fieldY[fidx(kx, ky, kz)] = comp->field[fidx(kx, ky, kz)];
                else if (wcomp == PLANE_Z)
			        fieldZ[fidx(kx, ky, kz)] = comp->field[fidx(kx, ky, kz)];
			}

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::getTransv(CagmScalarFieldOps *a)
{
	int kx, ky, kz;
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
				a->field[fidx(kx, ky, kz)] = sqrt(fieldX[fidx(kx, ky, kz)]*fieldX[fidx(kx, ky, kz)] + fieldY[fidx(kx, ky, kz)]*fieldY[fidx(kx, ky, kz)]);

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::getBounds(CagmVectorFieldOps *boundsx, CagmVectorFieldOps *boundsy, CagmVectorFieldOps *boundsz)
{
	// check equiv. sizes!
    getPlane(boundsx, PLANE_X, NphysL[0], 0);
    getPlane(boundsx, PLANE_X, NphysH[0]-1, 1);
    getPlane(boundsy, PLANE_Y, NphysL[1], 0);
    getPlane(boundsy, PLANE_Y, NphysH[1]-1, 1);
    getPlane(boundsz, PLANE_Z, NphysL[2], 0);
    getPlane(boundsz, PLANE_Z, NphysH[2]-1, 1);

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::setBounds(CagmVectorFieldOps *boundsx, CagmVectorFieldOps *boundsy, CagmVectorFieldOps *boundsz)
{
	// check equiv. sizes!
    setPlane(boundsx, PLANE_X, 0, NphysL[0]);
    setPlane(boundsx, PLANE_X, 1, NphysH[0]-1);
    setPlane(boundsy, PLANE_Y, 0, NphysL[1]);
    setPlane(boundsy, PLANE_Y, 1, NphysH[1]-1);
    setPlane(boundsz, PLANE_Z, 0, NphysL[2]);
    setPlane(boundsz, PLANE_Z, 1, NphysH[2]-1);

    return 0;
}

//-----------------------------------------------------------------------
//static void l_div_position(REALTYPE_A p, int N, int *k1, REALTYPE_A *tk)
#define l_div_position(p, N, k1, tk) \
{ \
    if (p >= REALTYPE_A((N)-1) || fabs(p-(REALTYPE_A((N)-1))) < 1e-5) \
    { \
        k1 = (N)-2; \
        tk = 1; \
    } \
    else \
    { \
        k1 = (int)floor(p); \
        if (k1 < 0) \
        { \
            k1 = 0;  \
            tk = 0; \
        } \
        else \
            tk = (p) - k1; \
    } \
}

//-----------------------------------------------------------------------
//static REALTYPE_A l_get_point(int Ny, REALTYPE_A **tfield, int x1, int y1, int z1, REALTYPE_A tx, REALTYPE_A ty, REALTYPE_A tz)
#define l_get_point(Ny, tfield, x1, y1, z1, tx, ty, tz) \
    ((1-(tz))* ((1-(ty))* ((1-(tx))*tfield[(y1)     +  (z1)   *Ny][x1] + (tx)*tfield[(y1)     +  (z1)   *Ny][x1+1]) + \
                    (ty)* ((1-(tx))*tfield[((y1)+1) +  (z1)   *Ny][x1] + (tx)*tfield[((y1)+1) +  (z1)   *Ny][x1+1]))  \
   +     (tz)* ((1-(ty))* ((1-(tx))*tfield[(y1)     + ((z1)+1)*Ny][x1] + (tx)*tfield[(y1)     + ((z1)+1)*Ny][x1+1]) + \
                    (ty)* ((1-(tx))*tfield[((y1)+1) + ((z1)+1)*Ny][x1] + (tx)*tfield[((y1)+1) + ((z1)+1)*Ny][x1+1]))  \
    )
//{
//    REALTYPE_A B2[2][2];
//    REALTYPE_A B3[2];
//
//    B2[0][0] = (1-tx)*tfield[y1   +  z1   *Ny][x1] + tx*tfield[y1   +  z1   *Ny][x1+1];
//    B2[0][1] = (1-tx)*tfield[y1   + (z1+1)*Ny][x1] + tx*tfield[y1   + (z1+1)*Ny][x1+1];
//    B2[1][0] = (1-tx)*tfield[y1+1 +  z1   *Ny][x1] + tx*tfield[y1+1 +  z1   *Ny][x1+1];
//    B2[1][1] = (1-tx)*tfield[y1+1 + (z1+1)*Ny][x1] + tx*tfield[y1+1 + (z1+1)*Ny][x1+1];
//
//    B3[0] = (1-ty)*B2[0][0] + ty*B2[1][0];
//    B3[1] = (1-ty)*B2[0][1] + ty*B2[1][1];
//
//}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::stretch(CagmVectorFieldOps *src, Interpolator inetrp, REALTYPE_A p1, REALTYPE_A p2, REALTYPE_A p3)
{
    REALTYPE_A cx = (REALTYPE_A)(src->N[0] - 1)/(REALTYPE_A)(N[0] - 1);
    REALTYPE_A cy = (REALTYPE_A)(src->N[1] - 1)/(REALTYPE_A)(N[1] - 1);
    REALTYPE_A cz = (REALTYPE_A)(src->N[2] - 1)/(REALTYPE_A)(N[2] - 1);

    if (inetrp == Lanczos)
    {
        int win = (int)p1;
        int size = (int)p2;
    }

	int kx, ky, kz;
	int x1, y1, z1;
    REALTYPE_A tx, ty, tz;
    for (kz = NphysL[2]; kz < NphysH[2]; kz++)
    {
        l_div_position((kz*cz), (src->N[2]), z1, tz);
        for (ky = NphysL[1]; ky < NphysH[1]; ky++)
        {
            l_div_position(ky*cy, src->N[1], y1, ty);
            for (kx = NphysL[0]; kx < NphysH[0]; kx++)
			{
                l_div_position(kx*cx, src->N[0], x1, tx);
                fieldX[fidx(kx, ky, kz)] = l_get_point(src->N[1], src->fieldX, x1, y1, z1, tx, ty, tz);
                fieldY[fidx(kx, ky, kz)] = l_get_point(src->N[1], src->fieldY, x1, y1, z1, tx, ty, tz);
                fieldZ[fidx(kx, ky, kz)] = l_get_point(src->N[1], src->fieldZ, x1, y1, z1, tx, ty, tz);
            }
        }
    }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::conv(CagmVectorFieldOps *src, CagmScalarFieldOps *win)
{
    int kx, ky, kz;
    int wx, wy, wz;
    int tx;
    int cx = (int)floor((win->NphysL[0] + win->NphysH[0]) / 2);
    int cy = (int)floor((win->NphysL[1] + win->NphysH[1]) / 2);
    int cz = (int)floor((win->NphysL[2] + win->NphysH[2]) / 2);
    //int M = (win->NphysH[0] - win->NphysL[0])*(win->NphysH[1] - win->NphysL[1])*(win->NphysH[2] - win->NphysL[2]);
    for (kz = NphysL[2]+cz; kz < NphysH[2]-cz; kz++)
        for (ky = NphysL[1]+cy; ky < NphysH[1]-cy; ky++)
            for (kx = NphysL[0]+cx; kx < NphysH[0]-cx; kx++)
            {
                REALTYPE_A sx = 0, sy = 0, sz = 0;
                for (wz = win->NphysL[2]; wz < win->NphysH[2]; wz++)
                {
                    for (wy = win->NphysL[1]; wy < win->NphysH[1]; wy++)
                    {
                        for (wx = win->NphysL[0]; wx < win->NphysH[0]; wx++)
                        {
                            tx = kx + wx - cx;
                            sx += src->fieldX[fidx(kx, ky, kz)] * win->field[wy + wz*win->N[1]][wx];
                            sy += src->fieldY[fidx(kx, ky, kz)] * win->field[wy + wz*win->N[1]][wx];
                            sz += src->fieldZ[fidx(kx, ky, kz)] * win->field[wy + wz*win->N[1]][wx];
                        }
                    }
                }
                REALTYPE_A xx = src->fieldX[fidx(kx, ky, kz)];
                fieldX[fidx(kx, ky, kz)] = sx; // / M;
                fieldY[fidx(kx, ky, kz)] = sy; // / M;
                fieldZ[fidx(kx, ky, kz)] = sz; // / M;
            }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::inCube(const REALTYPE_A *coord, const REALTYPE_A absBoundAchieve, const REALTYPE_A relBoundAchieve)
{
    uint32_t out = 0;
    if (coord[0] < relBoundAchieve*N[0] || coord[0] < absBoundAchieve || 
        coord[0] > N[0]-relBoundAchieve*N[0] - 1 || coord[0] > N[0] - 1 - absBoundAchieve)
        out = out | PLANE_X;
    if (coord[1] < relBoundAchieve*N[1] || coord[1] < absBoundAchieve || 
        coord[1] > N[1]-relBoundAchieve*N[1] - 1 || coord[1] > N[1] - 1 - absBoundAchieve)
        out = out | PLANE_Y;
    if (coord[2] < relBoundAchieve*N[2] || coord[2] < absBoundAchieve || 
        coord[2] > N[2]-relBoundAchieve*N[2] - 1 || coord[2] > N[2] - 1 - absBoundAchieve)
        out = out | PLANE_Z;

    return out;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::getPoint(const REALTYPE_A *coord, REALTYPE_A *vect)
{
    uint32_t out = 0;
    if (coord[0] < 0 || coord[0] > N[0]-1)
        out = out | PLANE_X;
    if (coord[1] < 0 || coord[1] > N[1]-1)
        out = out | PLANE_Y;
    if (coord[2] < 0 || coord[2] > N[2]-1)
        out = out | PLANE_Z;

    if (!out)
    {
        int x1, y1, z1;
        REALTYPE_A tx, ty, tz;
        l_div_position(coord[0], N[0], x1, tx);
        l_div_position(coord[1], N[1], y1, ty);
        l_div_position(coord[2], N[2], z1, tz);
        vect[0] = l_get_point(N[1], fieldX, x1, y1, z1, tx, ty, tz);
        vect[1] = l_get_point(N[1], fieldY, x1, y1, z1, tx, ty, tz);
        vect[2] = l_get_point(N[1], fieldZ, x1, y1, z1, tx, ty, tz);
    }

    return out;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::rotate3D(CagmRotate3D *rotator, bool direction)
{
    int kx, ky, kz;
    REALTYPE_A v[3];
    for (kz = 0; kz < N[2]; kz++)
        for (ky = 0; ky < N[1]; ky++)
            for (kx = 0; kx < N[0]; kx++)
            {
                rotator->rotate(fieldX[fidx(kx, ky, kz)], fieldY[fidx(kx, ky, kz)], fieldZ[fidx(kx, ky, kz)], v, v+1, v+2, direction);
                fieldX[fidx(kx, ky, kz)] = v[0];
                fieldY[fidx(kx, ky, kz)] = v[1];
                fieldZ[fidx(kx, ky, kz)] = v[2];
            }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::planeDerivative(int layer, REALTYPE_A *d)
{
    int kx, ky;
    for (ky = 0; ky < N[1]; ky++)
        for (kx = 0; kx < N[0]; kx++)
        {
            int bidx = (ky*N[0] + kx)*9;
            if (kx == 0)
            {
                d[bidx]   = -3 * fieldX[fidx(0, ky, layer)] + 4 * fieldX[fidx(1, ky, layer)] - fieldX[fidx(2, ky, layer)];
                d[bidx+3] = -3 * fieldY[fidx(0, ky, layer)] + 4 * fieldY[fidx(1, ky, layer)] - fieldY[fidx(2, ky, layer)];
                d[bidx+6] = -3 * fieldZ[fidx(0, ky, layer)] + 4 * fieldZ[fidx(1, ky, layer)] - fieldZ[fidx(2, ky, layer)];
            }
            else if (kx == N[0] - 1)
            {
                d[bidx]   = fieldX[fidx(N[0]-3, ky, layer)] - 4 * fieldX[fidx(N[0]-2, ky, layer)] + 3 * fieldX[fidx(N[0]-1, ky, layer)];
                d[bidx+3] = fieldY[fidx(N[0]-3, ky, layer)] - 4 * fieldY[fidx(N[0]-2, ky, layer)] + 3 * fieldY[fidx(N[0]-1, ky, layer)];
                d[bidx+6] = fieldZ[fidx(N[0]-3, ky, layer)] - 4 * fieldZ[fidx(N[0]-2, ky, layer)] + 3 * fieldZ[fidx(N[0]-1, ky, layer)];
            }
            else
            {
                d[bidx]   = fieldX[fidx(kx+1, ky, layer)] - fieldX[fidx(kx-1, ky, layer)];
                d[bidx+3] = fieldY[fidx(kx+1, ky, layer)] - fieldY[fidx(kx-1, ky, layer)];
                d[bidx+6] = fieldZ[fidx(kx+1, ky, layer)] - fieldZ[fidx(kx-1, ky, layer)];
            }

            d[bidx] *= 0.5*step[0];
            d[bidx+3] *= 0.5*step[0];
            d[bidx+6] *= 0.5*step[0];

            if (ky == 0)
            {
                d[bidx+1] = -3 * fieldX[fidx(kx, 0, layer)] + 4 * fieldX[fidx(kx, 1, layer)] - fieldX[fidx(kx, 2, layer)];
                d[bidx+4] = -3 * fieldY[fidx(kx, 0, layer)] + 4 * fieldY[fidx(kx, 1, layer)] - fieldY[fidx(kx, 2, layer)];
                d[bidx+7] = -3 * fieldZ[fidx(kx, 0, layer)] + 4 * fieldZ[fidx(kx, 1, layer)] - fieldZ[fidx(kx, 2, layer)];
            }
            else if (ky == N[1] - 1)
            {
                d[bidx+1] = fieldX[fidx(kx, N[1]-3, layer)] - 4 * fieldX[fidx(kx, N[1]-2, layer)] + 3 * fieldX[fidx(kx, N[1]-1, layer)];
                d[bidx+4] = fieldY[fidx(kx, N[1]-3, layer)] - 4 * fieldY[fidx(kx, N[1]-2, layer)] + 3 * fieldY[fidx(kx, N[1]-1, layer)];
                d[bidx+7] = fieldZ[fidx(kx, N[1]-3, layer)] - 4 * fieldZ[fidx(kx, N[1]-2, layer)] + 3 * fieldZ[fidx(kx, N[1]-1, layer)];
            }
            else
            {
                d[bidx+1] = fieldX[fidx(kx, ky+1, layer)] - fieldX[fidx(kx, ky-1, layer)];
                d[bidx+4] = fieldY[fidx(kx, ky+1, layer)] - fieldY[fidx(kx, ky-1, layer)];
                d[bidx+7] = fieldZ[fidx(kx, ky+1, layer)] - fieldZ[fidx(kx, ky-1, layer)];
            }

            d[bidx+1] *= 0.5*step[1];
            d[bidx+4] *= 0.5*step[1];
            d[bidx+7] *= 0.5*step[1];

            if (layer == 0) // should not be
            {
                d[bidx+2] = -3 * fieldX[fidx(kx, ky, 0)] + 4 * fieldX[fidx(kx, ky, 1)] - fieldX[fidx(kx, ky, 2)];
                d[bidx+5] = -3 * fieldY[fidx(kx, ky, 0)] + 4 * fieldY[fidx(kx, ky, 1)] - fieldY[fidx(kx, ky, 2)];
                d[bidx+8] = -3 * fieldZ[fidx(kx, ky, 0)] + 4 * fieldZ[fidx(kx, ky, 1)] - fieldZ[fidx(kx, ky, 2)];
            }
            else if (layer == N[2] - 1)
            {
                d[bidx+2] = fieldX[fidx(kx, ky, N[2]-3)] - 4 * fieldX[fidx(kx, ky, N[2]-2)] + 3 * fieldX[fidx(kx, ky, N[2]-1)];
                d[bidx+5] = fieldY[fidx(kx, ky, N[2]-3)] - 4 * fieldY[fidx(kx, ky, N[2]-2)] + 3 * fieldY[fidx(kx, ky, N[2]-1)];
                d[bidx+8] = fieldZ[fidx(kx, ky, N[2]-3)] - 4 * fieldZ[fidx(kx, ky, N[2]-2)] + 3 * fieldZ[fidx(kx, ky, N[2]-1)];
            }
            else
            {
                d[bidx+2] = fieldX[fidx(kx, ky, layer+1)] - fieldX[fidx(kx, ky, layer-1)];
                d[bidx+5] = fieldY[fidx(kx, ky, layer+1)] - fieldY[fidx(kx, ky, layer-1)];
                d[bidx+8] = fieldZ[fidx(kx, ky, layer+1)] - fieldZ[fidx(kx, ky, layer-1)];
            }

            d[bidx+2] *= 0.5*step[2];
            d[bidx+5] *= 0.5*step[2];
            d[bidx+8] *= 0.5*step[2];
        }

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagmVectorFieldOps::planeDerivative2(int layer, REALTYPE_A *d, REALTYPE_A *dd)
{
    int kx, ky;
    for (ky = 0; ky < N[1]; ky++)
        for (kx = 0; kx < N[0]; kx++)
        {
            int bidx = (ky*N[0] + kx)*9;
            int didx = (ky*N[0] + kx)*27;
            if (kx == 0)
            {
            }
            else if (kx == N[0] - 1)
            {
            }
            else
            {
                dd[didx   ]   = fieldX[fidx(kx+1, ky, layer)] - 2 * fieldX[fidx(kx, ky, layer)] + fieldX[fidx(kx-1, ky, layer)]; // Bx / dxdx
                dd[didx+ 9]   = fieldY[fidx(kx+1, ky, layer)] - 2 * fieldY[fidx(kx, ky, layer)] + fieldY[fidx(kx-1, ky, layer)]; // By / dxdx
                dd[didx+18]   = fieldZ[fidx(kx+1, ky, layer)] - 2 * fieldZ[fidx(kx, ky, layer)] + fieldZ[fidx(kx-1, ky, layer)]; // Bz / dxdx
                if (ky == 0)
                {
                    dd[didx+ 3] = -3 * d[bidx+1] + 4 * d[bidx+10] - d[bidx+19]; // (Bx/dy)/dx
                    dd[didx+12] = -3 * d[bidx+4] + 4 * d[bidx+13] - d[bidx+22]; // (By/dy)/dx
                    dd[didx+21] = -3 * d[bidx+7] + 4 * d[bidx+16] - d[bidx+25]; // (Bz/dy)/dx
                }
                else if (ky == N[1] - 1)
                {
                    dd[didx+ 3] = d[bidx-17] - 4 * d[bidx-8] + 3 * d[bidx+1]; // (Bx/dy)/dx
                    dd[didx+12] = d[bidx-14] - 4 * d[bidx-5] + 3 * d[bidx+4]; // (By/dy)/dx
                    dd[didx+21] = d[bidx-11] - 4 * d[bidx-2] + 3 * d[bidx+7]; // (Bz/dy)/dx
                }
                else
                {
                    dd[didx+ 3] = d[bidx+10] - d[bidx-8]; // (Bx/dy)/dx
                    dd[didx+12] = d[bidx+13] - d[bidx-5]; // (By/dy)/dx
                    dd[didx+21] = d[bidx+16] - d[bidx-2]; // (Bz/dy)/dx
                }
                if (layer == 0) // should not be
                {
                }
                else if (layer == N[2] - 1)
                {
                }
                else
                {
                    dd[didx+ 6] = d[bidx+11] - d[bidx-7]; // (Bx/dz)/dx
                    dd[didx+15] = d[bidx+14] - d[bidx-4]; // (By/dz)/dx
                    dd[didx+24] = d[bidx+17] - d[bidx-1]; // (Bz/dz)/dx
                }
            }

            dd[didx   ] *= step[0]*step[0];
            dd[didx+ 9] *= step[0]*step[0];
            dd[didx+18] *= step[0]*step[0];

            dd[didx+ 3] *= 0.5*step[0];
            dd[didx+ 1] = dd[didx+3];
            dd[didx+12] *= 0.5*step[0];
            dd[didx+10] = dd[didx+12];
            dd[didx+21] *= 0.5*step[0];
            dd[didx+19] = dd[didx+21];

            dd[didx+ 6] *= 0.5*step[0];
            dd[didx+ 2] = dd[didx+6];
            dd[didx+15] *= 0.5*step[0];
            dd[didx+11] = dd[didx+15];
            dd[didx+24] *= 0.5*step[0];
            dd[didx+20] = dd[didx+24];

            if (ky == 0)
            {
            }
            else if (ky == N[1] - 1)
            {
            }
            else
            {
            }

            if (layer == 0) // should not be
            {
            }
            else if (layer == N[2] - 1)
            {
            }
            else
            {
            }

        }

    return 0;
}

//-----------------------------------------------------------------------
// grad:
/*
    int p1, p2, q1, q2;
    REALTYPE_A pf, qf;
	kx = 0;
	for (ky = 0; ky < N[1]; ky++)
		for (kz = 0; kz < N[2]; kz++)
        {
            CagmScalarFieldOps::indices(ky, N[1], kz, N[2], &p1, &p2, &pf, &q1, &q2, &qf);
			fieldX[fidx(kx, ky, kz)] = (a->field[fidx(1, ky, kz)]-a->field[fidx(0, ky, kz)]);
			fieldY[fidx(kx, ky, kz)] = (a->field[fidx(kx, p2, kz)]-a->field[fidx(kx, p1, kz)]) * pf;
			fieldZ[fidx(kx, ky, kz)] = (a->field[fidx(kx, ky, q2)]-a->field[fidx(kx, ky, q1)]) * qf;
        }
	kx = N[0]-1;
	for (ky = 0; ky < N[1]; ky++)
		for (kz = 0; kz < N[2]; kz++)
        {
            CagmScalarFieldOps::indices(ky, N[1], kz, N[2], &p1, &p2, &pf, &q1, &q2, &qf);
			fieldX[fidx(kx, ky, kz)] = (a->field[fidx(N[0]-1, ky, kz)]-a->field[fidx(N[0]-2, ky, kz)]);
			fieldY[fidx(kx, ky, kz)] = (a->field[fidx(kx, p2, kz)]-a->field[fidx(kx, p1, kz)]) * pf;
			fieldZ[fidx(kx, ky, kz)] = (a->field[fidx(kx, ky, q2)]-a->field[fidx(kx, ky, q1)]) * qf;
        }
	ky = 0;
	for (kx = 0; kx < N[0]; kx++)
		for (kz = 0; kz < N[2]; kz++)
        {
            CagmScalarFieldOps::indices(kx, N[0], kz, N[2], &p1, &p2, &pf, &q1, &q2, &qf);
			fieldX[fidx(kx, ky, kz)] = (a->field[fidx(p2, ky, kz)]-a->field[fidx(p2, ky, kz)]) * pf;
			fieldY[fidx(kx, ky, kz)] = (a->field[fidx(kx, 1, kz)]-a->field[fidx(kx, 0, kz)]);
			fieldZ[fidx(kx, ky, kz)] = (a->field[fidx(kx, ky, q2)]-a->field[fidx(kx, ky, q1)]) * qf;
        }
	ky = N[1]-1;
	for (kx = 0; kx < N[0]; kx++)
		for (kz = 0; kz < N[2]; kz++)
        {
            CagmScalarFieldOps::indices(kx, N[0], kz, N[2], &p1, &p2, &pf, &q1, &q2, &qf);
			fieldX[fidx(kx, ky, kz)] = (a->field[fidx(p2, ky, kz)]-a->field[fidx(p2, ky, kz)]) * pf;
			fieldY[fidx(kx, ky, kz)] = (a->field[fidx(kx, N[1]-1, kz)]-a->field[fidx(kx, N[1]-2, kz)]);
			fieldZ[fidx(kx, ky, kz)] = (a->field[fidx(kx, ky, q2)]-a->field[fidx(kx, ky, q1)]) * qf;
        }
	kz = 0;
	for (kx = 0; kx < N[0]; kx++)
		for (ky = 0; ky < N[1]; ky++)
        {
            CagmScalarFieldOps::indices(kx, N[0], ky, N[1], &p1, &p2, &pf, &q1, &q2, &qf);
			fieldX[fidx(kx, ky, kz)] = (a->field[fidx(p2, ky, kz)]-a->field[fidx(p2, ky, kz)]) * pf;
			fieldY[fidx(kx, ky, kz)] = (a->field[fidx(kx, q2, kz)]-a->field[fidx(kx, q1, kz)]) * qf;
			fieldZ[fidx(kx, ky, kz)] = (a->field[fidx(kx, ky, 1)]-a->field[fidx(kx, ky, 0)]);
        }
	kz = N[2]-1;
	for (kx = 0; kx < N[0]; kx++)
		for (ky = 0; ky < N[1]; ky++)
        {
            CagmScalarFieldOps::indices(kx, N[0], ky, N[1], &p1, &p2, &pf, &q1, &q2, &qf);
			fieldX[fidx(kx, ky, kz)] = (a->field[fidx(p2, ky, kz)]-a->field[fidx(p2, ky, kz)]) * pf;
			fieldY[fidx(kx, ky, kz)] = (a->field[fidx(kx, q2, kz)]-a->field[fidx(kx, q1, kz)]) * qf;
			fieldZ[fidx(kx, ky, kz)] = (a->field[fidx(kx, ky, N[2]-1)]-a->field[fidx(kx, ky, N[2]-2)]);
        }
*/

// rot
/*
    int p1, p2, q1, q2;
    REALTYPE_A pf, qf;
	kx = 0;
	for (ky = 0; ky < N[1]; ky++)
		for (kz = 0; kz < N[2]; kz++)
        {
            CagmScalarFieldOps::indices(ky, N[1], kz, N[2], &p1, &p2, &pf, &q1, &q2, &qf);
			fieldX[fidx(kx, ky, kz)] = (a->fieldZ[fidx(kx, p2, kz)]-a->fieldZ[fidx(kx, p1, kz)]) * pf -
                                       (a->fieldY[fidx(kx, ky, q2)]-a->fieldY[fidx(kx, ky, q1)]) * qf;
			fieldY[fidx(kx, ky, kz)] = (a->fieldX[fidx(kx, ky, q2)]-a->fieldX[fidx(kx, ky, q1)]) * qf -
                                       (a->fieldZ[fidx(1, ky, kz)]-a->fieldZ[fidx(0, ky, kz)]);
			fieldZ[fidx(kx, ky, kz)] = (a->fieldY[fidx(1, ky, kz)]-a->fieldY[fidx(0, ky, kz)]) -
                                       (a->fieldX[fidx(kx, p2, kz)]-a->fieldX[fidx(kx, p1, kz)]) * pf;
        }
	kx = N[0]-1;
	for (ky = 0; ky < N[1]; ky++)
		for (kz = 0; kz < N[2]; kz++)
        {
            CagmScalarFieldOps::indices(ky, N[1], kz, N[2], &p1, &p2, &pf, &q1, &q2, &qf);
			fieldX[fidx(kx, ky, kz)] = (a->fieldZ[fidx(kx, p2, kz)]-a->fieldZ[fidx(kx, p1, kz)]) * pf -
                                       (a->fieldY[fidx(kx, ky, q2)]-a->fieldY[fidx(kx, ky, q1)]) * qf;
			fieldY[fidx(kx, ky, kz)] = (a->fieldX[fidx(kx, ky, q2)]-a->fieldX[fidx(kx, ky, q1)]) * qf -
                                       (a->fieldZ[fidx(N[0]-1, ky, kz)]-a->fieldZ[fidx(N[0]-2, ky, kz)]);
			fieldZ[fidx(kx, ky, kz)] = (a->fieldY[fidx(N[0]-1, ky, kz)]-a->fieldY[fidx(N[0]-2, ky, kz)]) -
                                       (a->fieldX[fidx(kx, p2, kz)]-a->fieldX[fidx(kx, p1, kz)]) * pf;
        }
	ky = 0;
	for (kx = 0; kx < N[0]; kx++)
		for (kz = 0; kz < N[2]; kz++)
        {
            CagmScalarFieldOps::indices(kx, N[0], kz, N[2], &p1, &p2, &pf, &q1, &q2, &qf);
			fieldX[fidx(kx, ky, kz)] = (a->fieldZ[fidx(kx, 1, kz)]-a->fieldZ[fidx(kx, 0, kz)]) -
                                       (a->fieldY[fidx(kx, ky, q2)]-a->fieldY[fidx(kx, ky, q1)]) * qf;
			fieldY[fidx(kx, ky, kz)] = (a->fieldX[fidx(kx, ky, q2)]-a->fieldX[fidx(kx, ky, q1)]) * qf -
                                       (a->fieldZ[fidx(p2, ky, kz)]-a->fieldZ[fidx(p1, ky, kz)]) * pf;
			fieldZ[fidx(kx, ky, kz)] = (a->fieldY[fidx(p2, ky, kz)]-a->fieldY[fidx(p1, ky, kz)]) * pf -
                                       (a->fieldX[fidx(kx, 1, kz)]-a->fieldX[fidx(kx, 0, kz)]);
        }
	ky = N[1]-1;
	for (kx = 0; kx < N[0]; kx++)
		for (kz = 0; kz < N[2]; kz++)
        {
            CagmScalarFieldOps::indices(kx, N[0], kz, N[2], &p1, &p2, &pf, &q1, &q2, &qf);
			fieldX[fidx(kx, ky, kz)] = (a->fieldZ[fidx(kx, N[1]-1, kz)]-a->fieldZ[fidx(kx, N[1]-2, kz)]) -
                                       (a->fieldY[fidx(kx, ky, q2)]-a->fieldY[fidx(kx, ky, q1)]) * qf;
			fieldY[fidx(kx, ky, kz)] = (a->fieldX[fidx(kx, ky, q2)]-a->fieldX[fidx(kx, ky, q1)]) * qf -
                                       (a->fieldZ[fidx(p2, ky, kz)]-a->fieldZ[fidx(p1, ky, kz)]) * pf;
			fieldZ[fidx(kx, ky, kz)] = (a->fieldY[fidx(p2, ky, kz)]-a->fieldY[fidx(p1, ky, kz)]) * pf -
                                       (a->fieldX[fidx(kx, N[1]-1, kz)]-a->fieldX[fidx(kx, N[1]-2, kz)]);
        }
	kz = 0;
	for (kx = 0; kx < N[0]; kx++)
		for (ky = 0; ky < N[1]; ky++)
        {
            CagmScalarFieldOps::indices(kx, N[0], ky, N[1], &p1, &p2, &pf, &q1, &q2, &qf);
			fieldX[fidx(kx, ky, kz)] = (a->fieldZ[fidx(kx, q2, kz)]-a->fieldZ[fidx(kx, q1, kz)]) * qf -
                                       (a->fieldY[fidx(kx, ky, 1)]-a->fieldY[fidx(kx, ky, 0)]);
			fieldY[fidx(kx, ky, kz)] = (a->fieldX[fidx(kx, ky, 1)]-a->fieldX[fidx(kx, ky, 0)]) -
                                       (a->fieldZ[fidx(p2, ky, kz)]-a->fieldZ[fidx(p1, ky, kz)]) * pf;
			fieldZ[fidx(kx, ky, kz)] = (a->fieldY[fidx(p2, ky, kz)]-a->fieldY[fidx(p1, ky, kz)]) * pf -
                                       (a->fieldX[fidx(kx, q2, kz)]-a->fieldX[fidx(kx, q1, kz)]) * qf;
        }
	kz = N[2]-1;
	for (kx = 0; kx < N[0]; kx++)
		for (ky = 0; ky < N[1]; ky++)
        {
            CagmScalarFieldOps::indices(kx, N[0], ky, N[1], &p1, &p2, &pf, &q1, &q2, &qf);
			fieldX[fidx(kx, ky, kz)] = (a->fieldZ[fidx(kx, q2, kz)]-a->fieldZ[fidx(kx, q1, kz)]) * qf -
                                       (a->fieldY[fidx(kx, ky, N[2]-1)]-a->fieldY[fidx(kx, ky, N[2]-2)]);
			fieldY[fidx(kx, ky, kz)] = (a->fieldX[fidx(kx, ky, N[2]-1)]-a->fieldX[fidx(kx, ky, N[2]-2)]) -
                                       (a->fieldZ[fidx(p2, ky, kz)]-a->fieldZ[fidx(p1, ky, kz)]) * pf;
			fieldZ[fidx(kx, ky, kz)] = (a->fieldY[fidx(p2, ky, kz)]-a->fieldY[fidx(p1, ky, kz)]) * pf -
                                       (a->fieldX[fidx(kx, q2, kz)]-a->fieldX[fidx(kx, q1, kz)]) * qf;
        }
*/