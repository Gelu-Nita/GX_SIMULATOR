#include "stdDefinitions.h"
#include "LinesTaskQueue.h"
#include "LinesProcessor.h"
#include "agmVectorField.h"
#include "agmVectorFieldLineFuncs.h"

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

#define l_get_point(Ny, tfield, x1, y1, z1, tx, ty, tz) \
    ((1-(tz))* ((1-(ty))* ((1-(tx))*tfield[(y1)     +  (z1)   *Ny][x1] + (tx)*tfield[(y1)     +  (z1)   *Ny][x1+1]) + \
                    (ty)* ((1-(tx))*tfield[((y1)+1) +  (z1)   *Ny][x1] + (tx)*tfield[((y1)+1) +  (z1)   *Ny][x1+1]))  \
   +     (tz)* ((1-(ty))* ((1-(tx))*tfield[(y1)     + ((z1)+1)*Ny][x1] + (tx)*tfield[(y1)     + ((z1)+1)*Ny][x1+1]) + \
                    (ty)* ((1-(tx))*tfield[((y1)+1) + ((z1)+1)*Ny][x1] + (tx)*tfield[((y1)+1) + ((z1)+1)*Ny][x1+1]))  \
    )

bool fcond(void *p, const CagmRKF45Vect& v)
{
    T_Lines *pp = (T_Lines *)p;
    return v.e[0] < pp->absBoundAchieve       || v.e[0]  > (pp->N)[0] - pp->absBoundAchieve - 1 
        || v.e[1] < pp->absBoundAchieve       || v.e[1]  > (pp->N)[1] - pp->absBoundAchieve - 1
        || v.e[2] < pp->absBoundAchieveBottom || v.e[2]  > (pp->N)[2] - pp->absBoundAchieve - 1
        ;
}

uint32_t fdata(void *p, const double /*t*/, const CagmRKF45Vect& v, CagmRKF45Vect& vp)
{
    vp.e[0] = 0; vp.e[1] = 0; vp.e[2] = 0;

    T_Lines *pp = (T_Lines *)p;
    if ((v.e[0] >= 0 && v.e[0]  <= (pp->N)[0] - 1
      && v.e[1] >= 0 && v.e[1]  <= (pp->N)[1] - 1
      && v.e[2] >= 0 && v.e[2]  <= (pp->N)[2] - 1))
    {
        REALTYPE_A field[3];
        ((T_Lines *)p)->vfield->getPoint(((CagmRKF45Vect&)v).v(), field);
        vp.e[0] = field[0] * ((T_Lines *)p)->dir;
        vp.e[1] = field[1] * ((T_Lines *)p)->dir;
        vp.e[2] = field[2] * ((T_Lines *)p)->dir;
        REALTYPE_A n = 1.0/sqrt(vp.e[0]*vp.e[0] + vp.e[1]*vp.e[1] + vp.e[2]*vp.e[2]);
        vp.e[0] *= n;
        vp.e[1] *= n;
        vp.e[2] *= n;
        return 0;
    }

    //return (rc ? 1 : 0);
    return 1;
}

//-----------------------------------------------------------------------------
CLinesProcessor::CLinesProcessor(LQPSupervisor *_supervisor, int _taskID, CagmVectorField *_v, int _dir, REALTYPE_A _step, REALTYPE_A _relErr, REALTYPE_A _absErr
        , REALTYPE_A _boundAchieve, REALTYPE_A _boundAchieveBottom, int _maxLength, int *_passed)
    : supervisor(_supervisor)
      ,v(_v)
      ,dir(_dir)
      ,step(_step)
      ,boundAchieve(_boundAchieve) 
      ,boundAchieveBottom(_boundAchieveBottom)
      ,maxLength(_maxLength)
      ,passed(_passed)
{
    rkf45 = new CagmRKF45(_absErr, _relErr, fdata, 3, nullptr, fcond, boundAchieve);
    coord = new REALTYPE_A[3*maxLength];
}

//-----------------------------------------------------------------------------
CLinesProcessor::~CLinesProcessor()
{
    delete rkf45;
    delete[] coord;
}

//-------------------------------------------------------------------------------------
uint32_t CLinesProcessor::setTaskParams(void *params)
{
    memcpy(point, (REALTYPE_A *)params, 3*sizeof(REALTYPE_A)); 

    return 0;
}

//-----------------------------------------------------------------------------
uint32_t CLinesProcessor::ActionCore()
{
    int lineLength;
    int code;

    if ( !supervisor->needProcessing(queueID) )
            return 0;

    CagmVectorFieldOps::Status s = v->getOneFullLine(rkf45, point, dir, step, boundAchieve, boundAchieveBottom, maxLength, &lineLength, coord, &code);
    uint32_t rc = supervisor->SetResult(queueID, point, coord, lineLength, (int)code, CagmVectorFieldOps::Status::BufferOverload);

    return 0;
}
