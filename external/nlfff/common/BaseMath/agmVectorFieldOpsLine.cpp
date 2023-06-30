#include "stdDefinitions.h"
#include <math.h>

#include "agsFieldsCommon.h"
#include "agmVectorFieldOps.h"
#include "agmRKF45.h"
#include "agmVectorFieldLineFuncs.h"

#define fidx(kx, ky, kz) (ky)+(kz)*N[1]][(kx)

static void v_copyCoord(REALTYPE_A *coord, CagmRKF45Vect *rkfv)
{
    coord[0] = (*rkfv)[0];
    coord[1] = (*rkfv)[1];
    coord[2] = (*rkfv)[2];
}

//-----------------------------------------------------------------------
CagmVectorFieldOps::Status CagmVectorFieldOps::getOneLine(CagmRKF45 *rkf45, CagmRKF45Vect *rkfv, REALTYPE_A step, REALTYPE_A *coord, int maxlen, int *length, CagmRKF45::Status *status, bool noDuplicate)
{
    *length = 0;
    CagmVectorFieldOps::Status outstatus = CagmVectorFieldOps::Status::None;
    REALTYPE_A t = 0;
    REALTYPE_A s = step;
    if (!noDuplicate)
    {
        v_copyCoord(coord, rkfv);
        (*length)++;
    }
    for (int i = 1; i < maxlen; i++)
    {
        if (i >= maxlen)
        {
            outstatus = CagmVectorFieldOps::Status::BufferOverload;
            break;
        }

        *status = rkf45->calculate(t, *rkfv, t + s, false);
        if (*status == CagmRKF45::Status::End || *status == CagmRKF45::Status::EndByCond)
        {
            v_copyCoord(coord+3*(*length), rkfv);
            (*length)++;
        }

        if (*status != CagmRKF45::Status::End)
        {
            outstatus = (*status == CagmRKF45::Status::EndByCond || *status == CagmRKF45::Status::EndNoMove ?
                          CagmVectorFieldOps::Status::Boundary : CagmVectorFieldOps::Status::RKF45Problem);
            break;
        }
    }

    return outstatus;
}

//-----------------------------------------------------------------------
CagmVectorFieldOps::Status CagmVectorFieldOps::getOneFullLine(CagmRKF45 *rkf45, REALTYPE_A *start, int direction, REALTYPE_A step, REALTYPE_A boundAchieve, REALTYPE_A boundAchieveBottom, 
    int maxLength, int *length, REALTYPE_A *coord, int *code)
{
    step = fabs(step);
    int dirc = (direction >= 0 ? 1 : -1);
    CagmRKF45Vect rkfv(3, start);

    CagmVectorFieldOps::Status status;

    *length = 0;
    *code = 0;

    REALTYPE_A B0[3];
    uint32_t rc = getPoint(start, B0);
    if (rc != 0)
    {
        *code = (int)CagmVectorFieldOps::Status::OutOfCube;
        return CagmVectorFieldOps::Status::OutOfCube;
    }

    CagmRKF45::Status rkfstatus;

    T_Lines data;
    data.dir = dirc;
    data.vfield = this;
    data.N = this->N;
    data.fieldX = this->fieldX;
    data.fieldY = this->fieldY;
    data.fieldZ = this->fieldZ;
    data.absBoundAchieve = 0;
    data.absBoundAchieveBottom = 0;
    //data.absBoundAchieve = boundAchieve/10;
    //data.absBoundAchieveBottom = boundAchieveBottom - 1.1*boundAchieve;
    //if (data.absBoundAchieveBottom < data.absBoundAchieve)
    //    data.absBoundAchieveBottom = data.absBoundAchieve;
    data.relBoundAchieve = 0;

    rkf45->reinit(&data);
    int currlen;
    status = getOneLine(rkf45, &rkfv, step, coord, maxLength, &currlen, &rkfstatus);

    bool needBack = (direction == 0);
    bool noDuplicate = false;
    if (currlen > 1)
    {
        if (needBack)
        {
            for (int krev = 0; krev < currlen/2; krev++)
            {
                REALTYPE_A tmp = coord[krev*3];   coord[krev*3]   = coord[(currlen-1-krev)*3];   coord[(currlen-1-krev)*3]   = tmp;
                           tmp = coord[krev*3+1]; coord[krev*3+1] = coord[(currlen-1-krev)*3+1]; coord[(currlen-1-krev)*3+1] = tmp;
                           tmp = coord[krev*3+2]; coord[krev*3+2] = coord[(currlen-1-krev)*3+2]; coord[(currlen-1-krev)*3+2] = tmp;
            }
            noDuplicate = true;
        }
    }
    else
        currlen = 0;

    int rest = maxLength - currlen;
    REALTYPE_A *posc = coord + currlen*3;
    *length += currlen;
    *code = (int)status*100 + (int)rkfstatus;

    if (status & CagmVectorFieldOps::Status::BufferOverload)
        return status;

    if (needBack)
    {
        data.dir = -data.dir;
        rkfv = start;
        rkf45->reinit(&data);
        status = getOneLine(rkf45, &rkfv, step, posc, rest, &currlen, &rkfstatus, noDuplicate);
        if (currlen <= 1)
            currlen = 0;

        *length += currlen;
        *code += ((int)status*100 + (int)rkfstatus)*100;
    }

    return status;
}

////-----------------------------------------------------------------------
//CagmVectorFieldOps::Status CagmVectorFieldOps::getMultiLines(CagmRKF45 *rkf45, REALTYPE_A *start, int ninpt, int direction, REALTYPE_A step, REALTYPE_A tolerance, REALTYPE_A boundAchieve, 
//    int maxResult, int *length, REALTYPE_A *coord, int *codes)
//{
//}

////-----------------------------------------------------------------------
//CagmVectorFieldOps::Status CagmVectorFieldOps::getLine(REALTYPE_A *start, int ninpt, int direction, REALTYPE_A step, REALTYPE_A tolerance, REALTYPE_A boundAchieve, 
//    int maxResult, int *length, REALTYPE_A *coord, int *codes)
//{
//    REALTYPE_A c0[3], B0[3];
//
//    T_Lines data;
//    data.vfield = this;
//    data.absTol = boundAchieve;
//    data.relTol = 0;
//
//    REALTYPE_A relErr = tolerance;
//    REALTYPE_A absErr = 0;
//    CagmRKF45 *rkf45 = new CagmRKF45(absErr, relErr, fdata, 3, &data, fcond, boundAchieve);
//    step = fabs(step);
//    int dirc = (direction >= 0 ? 1 : -1);
//    CagmRKF45Vect rkfv(3);
//
//    CagmVectorFieldOps::Status status;
//
//    REALTYPE_A *posc = coord;
//    int rest = maxResult;
//    int k;
//    for (k = 0; k < ninpt; k++)
//    {
//        length[k] = 0;
//        codes[k] = 0;
//    }
//    for (k = 0; k < ninpt; k++)
//    {
//        REALTYPE_A *vc = start + 3*k;
//        rkfv = vc;
//        uint32_t rc = getPoint(vc, B0);
//        if (rc != 0)
//            continue;
//
//        CagmRKF45::Status rkfstatus;
//
//        status = CagmVectorFieldOps::Status::None;
//        data.dir = dirc;
//        int currlen;
//
//        rkf45->reinit(absErr, relErr, &data);
//        CagmVectorFieldOps::Status st = getOneLine(rkf45, &rkfv, step, posc, rest, currlen, rkfstatus);
//
//        bool needBack = (direction == 0);
//        bool noDuplicate = false;
//        if (currlen > 1)
//        {
//            if (needBack)
//            {
//                for (int krev = 0; krev < currlen/2; krev++)
//                {
//                    REALTYPE_A tmp = posc[krev*3]; posc[krev*3] = posc[(currlen-1-krev)*3]; posc[(currlen-1-krev)*3] = tmp;
//                    tmp = posc[krev*3+1]; posc[krev*3+1] = posc[(currlen-1-krev)*3+1]; posc[(currlen-1-krev)*3+1] = tmp;
//                    tmp = posc[krev*3+2]; posc[krev*3+2] = posc[(currlen-1-krev)*3+2]; posc[(currlen-1-krev)*3+2] = tmp;
//                }
//                noDuplicate = true;
//            }
//
//            rest -= currlen;
//            posc += currlen*3;
//            length[k] += currlen;
//
//            codes[k] = (int)st*1000 + (int)rkfstatus*100;
//        }
//        else
//            currlen = 0;
//
//        if (st == CagmVectorFieldOps::Status::BufferOverload)
//        {
//            status = st;
//            codes[k] = (int)st;
//            break;
//        }
//
//        if (needBack)
//        {
//            data.dir = -data.dir;
//            rkf45->reinit(absErr, relErr, &data);
//            st = getOneLine(rkf45, &rkfv, step, posc, rest, currlen, rkfstatus, noDuplicate);
//            if (currlen <= 1 || st != CagmVectorFieldOps::Status::Boundary)
//                currlen = 0;
//
//            rest -= currlen;
//            posc += currlen*3;
//            length[k] += currlen;
//
//            codes[k] += (int)st*10 + (int)rkfstatus;
//
//            if (st == CagmVectorFieldOps::Status::BufferOverload)
//            {
//                status = st;
//                codes[k] = (int)st;
//                break;
//            }
//        }
//    }
//
//    delete rkf45;
//
//    return status;
//}
//
////-----------------------------------------------------------------------
//CagmVectorFieldOps::Status CagmVectorFieldOps::getLine1(REALTYPE_A *start, int direction, int maxLength, REALTYPE_A step, REALTYPE_A tolerance, REALTYPE_A boundAchieve, 
//    int& length, REALTYPE_A *coord, REALTYPE_A *steps)
//{
//    length = 0;
//    REALTYPE_A c0[3], v0[3];
//    uint32_t rc = getPoint(start, v0);
//    if (rc != 0)
//        return CagmVectorFieldOps::Status::OutOfCube;
//
//    step = fabs(step);
//    REALTYPE_A dir = (direction < 0 ? -1 : 1);
//    if (direction == 0)
//    {
//        REALTYPE_A norm = 1 / sqrt(v0[0]*v0[0] + v0[1] * v0[1] + v0[2] * v0[2]);
//        c0[0] = start[0] + v0[0] * norm*step;
//        c0[1] = start[1] + v0[1] * norm*step;
//        c0[2] = start[2] + v0[2] * norm*step;
//        dir = (inCube(c0) ? -1 : 1);
//    }
//
//    CagmRKF45Vect v(3, start);
//    T_Lines data;
//    data.vfield = this;
//    data.absTol = boundAchieve;
//    data.relTol = 0;
//    data.dir = dir;
//
//    REALTYPE_A relErr = tolerance;
//    REALTYPE_A absErr = 0;
//    CagmRKF45 *rkf45 = new CagmRKF45(absErr, relErr, fdata, 3, &data, fcond, boundAchieve);
//
//    CagmVectorFieldOps::Status outstatus = CagmVectorFieldOps::Status::None;
//    REALTYPE_A t = 0;
//    REALTYPE_A s = step;
//    coord[0] = v[0];
//    coord[1] = v[1];
//    coord[2] = v[2];
//    steps[0] = t;
//    length++;
//    for (int i = 1; i < maxLength; i++)
//    {
//        CagmRKF45::Status status = rkf45->calculate(t, v, t + s, false);
//        if (status == CagmRKF45::Status::End)
//        {
//            coord[3*length  ] = v[0];
//            coord[3*length+1] = v[1];
//            coord[3*length+2] = v[2];
//            steps[length] = t;
//            length++;
//        }
//        else
//        {
//            outstatus = (status == CagmRKF45::Status::EndByCond ? CagmVectorFieldOps::Status::Boundary : CagmVectorFieldOps::Status::RKF45Problem);
//            break;
//        }
//    }
//
//    delete rkf45;
//
//    return outstatus;
//}
