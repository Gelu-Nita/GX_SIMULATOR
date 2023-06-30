#pragma once

#include "agmBaseMath.h"

class CagmRotate3D
{
public:
    REALTYPE_A vcos[3];
    REALTYPE_A lat, lon;

protected:
    REALTYPE_A VCS2VAR[3][3], VAR2VCS[3][3];
    REALTYPE_A sinlat, sinlon, coslat, coslon;
    REALTYPE_A eyeTol;

public:
	CagmRotate3D(REALTYPE_A *_vcos, REALTYPE_A _eyeTol = 1e-6)
    {
        init(_vcos, _eyeTol);
    }
	CagmRotate3D(REALTYPE_A _lon, REALTYPE_A _lat, REALTYPE_A _eyeTol = 1e-6)
    {
        init(_lon, _lat, _eyeTol);
    }
	CagmRotate3D()
    {
    }
	virtual ~CagmRotate3D()
    {}

    void copyRotator(CagmRotate3D *rotator)
    {
        memcpy(vcos, rotator->vcos, 3*sizeof(REALTYPE_A));
        lat = rotator->lat;
        lon = rotator->lon;

        memcpy(VCS2VAR, rotator->VCS2VAR, 9 * sizeof(REALTYPE_A));
        memcpy(VAR2VCS, rotator->VAR2VCS, 9 * sizeof(REALTYPE_A));
        sinlat = rotator->sinlat;
        sinlon = rotator->sinlon;
        coslat = rotator->coslat;
        coslon = rotator->coslon;
        eyeTol = rotator->eyeTol;
    }

	virtual void init(REALTYPE_A *_vcos, REALTYPE_A _eyeTol = 1e-6)
    {
        eyeTol = _eyeTol;

        for (int d= 0; d < 3; d++)
            vcos[d] = _vcos[d];

        sinlon = -vcos[1];
        coslon = sqrt(1 - sinlon*sinlon);
        coslat = vcos[2]/coslon;
        sinlat = -vcos[0]/coslon;

        lat = asin(sinlat)*v_180/v_pi_c;
        lon = asin(sinlon)*v_180/v_pi_c;

        createMatrices();
    }
	virtual void init(REALTYPE_A _lon, REALTYPE_A _lat, REALTYPE_A _eyeTol = 1e-6)
    {
        eyeTol = _eyeTol;

        lon = _lon;
        lat = _lat;

        getDirCos(this->lon, this->lat, this->vcos, &(this->sinlon), &(this->coslon), &(this->sinlat), &(this->coslat));

        createMatrices();
    }

    static void getDirCos(REALTYPE_A _lon, REALTYPE_A _lat, REALTYPE_A *_vcos
                        , REALTYPE_A *_sinlon = nullptr, REALTYPE_A *_coslon = nullptr, REALTYPE_A *_sinlat = nullptr, REALTYPE_A *_coslat = nullptr)
    {
        REALTYPE_A sinlon = sin(_lon*v_pi_c / v_180);
        REALTYPE_A coslon = cos(_lon*v_pi_c / v_180);
        REALTYPE_A sinlat = sin(_lat*v_pi_c / v_180);
        REALTYPE_A coslat = cos(_lat*v_pi_c / v_180);
        if (_sinlon)
            *_sinlon = sinlon;
        if (_coslon)
            *_coslon = coslon;
        if (_sinlat)
            *_sinlat = sinlat;
        if (_coslat)
            *_coslat = coslat;

        _vcos[0] = -sinlat*coslon;
        _vcos[1] = -sinlon;
        _vcos[2] = coslat*coslon;
    }

    bool isEye()
    {
        return fabs(vcos[0]) < eyeTol && fabs(vcos[1]) < eyeTol && fabs(1 - vcos[2]) < eyeTol;
    }

    void rotate(REALTYPE_A *v, REALTYPE_A *vr, bool direction)
    {
        if (direction)
            rotateP2V(v, vr);
        else
            rotateV2P(v, vr);
    }

    void rotate(REALTYPE_A x, REALTYPE_A y, REALTYPE_A z, REALTYPE_A *xr, REALTYPE_A *yr, REALTYPE_A *zr, bool direction)
    {
        if (direction)
            rotateP2V(x, y, z, xr, yr, zr);
        else
            rotateV2P(x, y, z, xr, yr, zr);
    }

    void rotateP2V(REALTYPE_A x, REALTYPE_A y, REALTYPE_A z, REALTYPE_A *xr, REALTYPE_A *yr, REALTYPE_A *zr)
    {
        REALTYPE_A xt = VAR2VCS[0][0] * x + VAR2VCS[0][1] * y + VAR2VCS[0][2] * z;
        REALTYPE_A yt = VAR2VCS[1][0] * x + VAR2VCS[1][1] * y + VAR2VCS[1][2] * z;
        REALTYPE_A zt = VAR2VCS[2][0] * x + VAR2VCS[2][1] * y + VAR2VCS[2][2] * z;
        *xr = xt;
        *yr = yt;
        *zr = zt;
    }
    void rotateP2V(REALTYPE_A *v, REALTYPE_A *vr)
    {
        rotateP2V(v[0], v[1], v[2], vr, vr + 1, vr + 2);
    }
    void rotateP2V(REALTYPE_A *v)
    {
        rotateP2V(v[0], v[1], v[2], v, v + 1, v + 2);
    }

    void rotateV2P(REALTYPE_A x, REALTYPE_A y, REALTYPE_A z, REALTYPE_A *xr, REALTYPE_A *yr, REALTYPE_A *zr)
    {
        *xr = VCS2VAR[0][0] * x + VCS2VAR[0][1] * y + VCS2VAR[0][2] * z;
        *yr = VCS2VAR[1][0] * x + VCS2VAR[1][1] * y + VCS2VAR[1][2] * z;
        *zr = VCS2VAR[2][0] * x + VCS2VAR[2][1] * y + VCS2VAR[2][2] * z;
    }
    void rotateV2P(REALTYPE_A *v, REALTYPE_A *vr)
    {
        rotateV2P(v[0], v[1], v[2], vr, vr + 1, vr + 2);
    }
protected:
    void createMatrices()
    {
        VAR2VCS[0][0] = VCS2VAR[0][0] = coslat;
        VAR2VCS[1][0] = VCS2VAR[0][1] = -sinlat*sinlon;
        VAR2VCS[2][0] = VCS2VAR[0][2] = -sinlat*coslon;
        VAR2VCS[0][1] = VCS2VAR[1][0] = 0;
        VAR2VCS[1][1] = VCS2VAR[1][1] = coslon;
        VAR2VCS[2][1] = VCS2VAR[1][2] = -sinlon;
        VAR2VCS[0][2] = VCS2VAR[2][0] = sinlat;
        VAR2VCS[1][2] = VCS2VAR[2][1] = coslat*sinlon;
        VAR2VCS[2][2] = VCS2VAR[2][2] = coslat*coslon;
    }
};
