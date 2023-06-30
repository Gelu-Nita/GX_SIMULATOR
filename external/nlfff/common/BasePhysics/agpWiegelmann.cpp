#include "stdDefinitions.h"
#include <math.h>

#include "agpWiegelmann.h"
#include "agmScalarField.h"
#include "agmVectorField.h"
#include "mfoGlobals.h"

//-----------------------------------------------------------------------
CagpWiegelmann::CagpWiegelmann(int *_N, int _taskID)
    : 
      vB(nullptr),
      vgradW(nullptr),
      sW(nullptr),
      vbaseB(nullptr),
      vbaseW(nullptr),
      sabsB(nullptr),
      sabsW(nullptr),
      slosB(nullptr),
      slosW(nullptr),
      vRotB(nullptr),
      Wa(nullptr),
      Wb(nullptr),
      vFt(nullptr),
      v(nullptr),
      s3(nullptr),
      Wa2Wb2(nullptr),
      s2(nullptr),
      s(nullptr),
      vF(nullptr),
      vdircos(nullptr),
      sourceB(nullptr),
      sourceW(nullptr),
      sourceGradW(nullptr),
      outF(nullptr)
{
    for (int i = 0; i < 3; i++)
    {
        N[i] = _N[i];
        dircos[i] = 0;
    }
    dircos[2] = 1;
}

//-----------------------------------------------------------------------
uint32_t CagpWiegelmann::Bind(CagmVectorField *_sourceB, CagmScalarField *_sourceW, CagmVectorField *_sourceGradW,
                           CagmVectorField *_baseField, CagmVectorField *_baseWeight,
                           CagmScalarField *_absField, CagmScalarField *_absWeight, 
                           CagmScalarField *_losField, CagmScalarField *_losWeight, 
                           REALTYPE_A *_vcos,
                           CagmVectorFieldOps *_outF, REALTYPE_A *_Lt)
{
    sourceB = _sourceB;
    sourceW = _sourceW;
    sourceGradW = _sourceGradW;
    baseField = _baseField;
    baseWeight = _baseWeight;
    absField = _absField;
    absWeight = _absWeight;
    losField = _losField;
    losWeight = _losWeight;
    outF = _outF;
    Lt = _Lt;

    if (_vcos)
        for (int i = 0; i < 3; i++)
            dircos[i] = _vcos[i];
    
    return 0;
}

//-----------------------------------------------------------------------
CagpWiegelmann::~CagpWiegelmann()
{
    delete vB;
    delete vgradW;
    delete sW;
    delete vbaseB;
    delete vbaseW;
    delete sabsB;
    delete sabsW;
    delete slosB;
    delete slosW;

    delete vRotB;
    delete Wa;
    delete Wb;
    delete vFt;
    delete v;
    delete s3;
    delete Wa2Wb2;
    delete s2;
    delete s;

    delete vF;

    delete vdircos;
}

//-----------------------------------------------------------------------
uint32_t CagpWiegelmann::Allocate(int *M, REALTYPE_A *steps)
{
    // base
    vB = new CagmVectorField(M, false);
    vB->SetSteps(steps);
    sW = new CagmScalarField(M, false);
    sW->SetSteps(steps);
    vgradW = new CagmVectorField(M, false);
    vgradW->SetSteps(steps);
    if (WiegelmannProcCondType == 1)
    {
        vbaseB = new CagmVectorField(M, false);
        vbaseB->SetSteps(steps);
        vbaseW = new CagmVectorField(M, false);
        vbaseW->SetSteps(steps);
        sabsB = new CagmScalarField(M, false);
        sabsB->SetSteps(steps);
        sabsW = new CagmScalarField(M, false);
        sabsW->SetSteps(steps);
        slosB = new CagmScalarField(M, false);
        slosB->SetSteps(steps);
        slosW = new CagmScalarField(M, false);
        slosW->SetSteps(steps);
    }

    vdircos = new CagmVectorField(M);
    vdircos->SetSteps(steps);
    vdircos->setVector(dircos);

    // intermediate
    vRotB = new CagmVectorField(M);
    vRotB->SetSteps(steps);
    Wa = new CagmVectorField(M);
    Wa->SetSteps(steps);
    Wb = new CagmVectorField(M);
    Wb->SetSteps(steps);
    vFt = new CagmVectorField(M);
    vFt->SetSteps(steps);
    v = new CagmVectorField(M);
    v->SetSteps(steps);
    s3 = new CagmScalarField(M);
    s3->SetSteps(steps);
    Wa2Wb2 = new CagmScalarField(M);
    Wa2Wb2->SetSteps(steps);
    s2 = new CagmScalarField(M);
    s2->SetSteps(steps);
    s = new CagmScalarField(M);
    s->SetSteps(steps);

    // output
    vF = new CagmVectorField(M, false);
    vF->SetSteps(steps);

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagpWiegelmann::setTaskParams(void * params, size_t _queueID)
{
    int Mmin[3], _DphysL[3], _DphysH[3];

    int *ppos = (int *)params;
    Mmin[0] = ppos[0];
    Mmin[1] = ppos[1];
    Mmin[2] = ppos[2];
    _DphysL[0] = ppos[3];
    _DphysL[1] = ppos[4];
    _DphysL[2] = ppos[5];
    _DphysH[0] = ppos[6];
    _DphysH[1] = ppos[7];
    _DphysH[2] = ppos[8];

    // input
    vB->SetMargins(sourceB, Mmin, _DphysL, _DphysH);
    vgradW->SetMargins(sourceGradW, Mmin, _DphysL, _DphysH);
    sW->SetMargins(sourceW, Mmin, _DphysL, _DphysH);
    if (WiegelmannProcCondType == 1 && baseField && baseWeight)
    {
        vbaseB->SetMargins(baseField, Mmin, _DphysL, _DphysH);
        vbaseW->SetMargins(baseWeight, Mmin, _DphysL, _DphysH);
    }
    if (WiegelmannProcCondType == 1 && absField && absWeight)
    {
        sabsB->SetMargins(absField, Mmin, _DphysL, _DphysH);
        sabsW->SetMargins(absWeight, Mmin, _DphysL, _DphysH);
    }
    if (WiegelmannProcCondType == 1 && losField && losWeight)
    {
        slosB->SetMargins(losField, Mmin, _DphysL, _DphysH);
        slosW->SetMargins(losWeight, Mmin, _DphysL, _DphysH);
    }

    // output
    vF->SetMargins(outF, Mmin, _DphysL, _DphysH);

    queueID = _queueID;

    return 0;
}

//-----------------------------------------------------------------------
uint32_t CagpWiegelmann::ActionCore()
{
    s2->abs2(vB);
	s->inv(s2); // s == 1/B^2

    //int *N = vB->GetDimensions();
    //CagmVectorField *FFG = new CagmVectorField(N);
    //FFG->blockF(vB, s, sW, vgradW);

    vRotB->rotScheme(vB, WiegelmannDerivStencil); // rotB
    Wa->cross(vRotB, vB); // rotB x B
    Wa->mult(s);

    s3->divScheme(vB, WiegelmannDerivStencil); // divB
    Wb->mult(s3, vB); // divB * B
    Wb->mult(s);

    Wa2Wb2->abs2(Wa);
    s->abs2(Wb);
    s->mult(WiegelmannWeightDivfree);
    Wa2Wb2->add(s);

    s->mult(Wa2Wb2, s2);
    Lt[queueID] = s->sumPhysW(sW);

    vF->multPhys(Wa2Wb2, vB); // (Wa^2 + Wb^2)*B
    //vF->mult(Wa2Wb2, vB); // (Wa^2 + Wb^2)*B

    vFt->cross(Wa, vB); // Wa x B
    v->rotScheme(vFt, WiegelmannDerivStencil); // rot(Wa x B)
    vF->addPhys(v); // rot(Wa x B) + (Wa^2 + Wb^2)*B

    vFt->cross(vgradW);  // (Wa x B) x gradW

    v->cross(Wa, vRotB);
    vF->subPhys(v); // rot(Wa x B) - Wa x rotB + (Wa^2 + Wb^2)*B

    s->dot(Wb, vB);
    v->gradScheme(s, WiegelmannDerivStencil);
    v->mult(WiegelmannWeightDivfree);
    vF->addPhys(v); // rot(Wa x B) - Wa x rotB + grad(Wb . B) + (Wa^2 + Wb^2)*B

    v->mult(s, vgradW);
    v->mult(WiegelmannWeightDivfree);
    vFt->add(v);  // rot(Wa x B) x gradW + (Wb . B)*gradW

    v->mult(Wb, s3);
    v->mult(WiegelmannWeightDivfree);
    vF->subPhys(v); //  rot(Wa x B) - Wa x rotB + grad(Wb . B) + (Wa^2 + Wb^2)*B - Wb*divB

    vF->multPhys(sW);
    vF->addPhys(vFt); // w*F + rot(Wa x B) - Wa x rotB + grad(Wb . B) - Wb*divB + (Wa^2 + Wb^2)*B

//    delete[] FFG;

    if (WiegelmannProcCondType == 1 && baseField)
    {
        v->subPhys(vB, vbaseB);
        s->abs2(v, vbaseW);
        Lt[queueID] += s->sumPhys();

        v->multPhys(vbaseW);
        vF->subPhys(v);
    }

    if (WiegelmannProcCondType == 1 && absField)
    {
        s3->abs(vB);
        s->sub(s3, sabsB);
        s2->mult(s, s);
        s2->mult(sabsW);
        Lt[queueID] += s2->sumPhys();

        s->mult(sabsW);
        s3->inv();
        s->mult(s3);
        v->mult(vB, s);
        vF->subPhys(v);
    }

    if (WiegelmannProcCondType == 1 && losField)
    {
        s3->projection(vB, dircos);
        s->sub(s3, slosB);
        s2->mult(s, s);
        s2->mult(slosW);
        Lt[queueID] += s2->sumPhys();

        s->mult(slosW);
        v->mult(vdircos, s);
        vF->subPhys(v);
    }

	return 0;
}

////-----------------------------------------------------------------------
//uint32_t CagpWiegelmann::ActionCore()
//{
//    s2->abs2(vB);
//	s->inv(s2); // s2inv == 1/B^2
//
//    vRotB->rot(vB); // rotB
//    Wa->cross(vRotB, vB); // rotB x B
//
//    s3->div(vB); // divB
//    Wb->mult(s3, vB); // divB * B
//
//    Wa->sub(Wb);
//    Wa->mult(s);
//
//    Wa2Wb2->abs2(Wa);// W^2
//
//    s->mult(Wa2Wb2, s2); // functional
//    Lt[queueID] = s->sumPhysArg(sW);
//
//    vF->mult(Wa2Wb2, vB); // W^2*B
//
//    vFt->cross(Wa, vB); // W x B
//    v->rot(vFt); // rot(W x B)
//    vF->add(v); // rot(W x B) + W^2*B
//
//    v->cross(Wa, vRotB);
//    vF->sub(v); // rot(W x B) - W x rotB + W^2*B
//
//    s->dot(Wa, vB);
//    v->grad(s);
//    vF->add(v); // rot(W x B) - W x rotB + grad(W . B) + W^2*B
//
//    v->mult(Wa, s3);
//    vF->sub(v); //  rot(W x B) - W x rotB + grad(W . B) + W*B - W*divB
//
//	return 0;
//}

/*
	sbuf1->abs2(base);
	sbuf1->inv(); // sbuf1 == 1/B^2

	vbuf3->rot(base); // vbuf3 = rotB
	vbuf2->cross(vbuf3, base); // vbuf2 = rotBxB
	sjB2->abs2(vbuf2); // jB^2 = (rotBxB)^2

	sL->mult(sbuf1, sjB2); // L = B^-2 * (rotBxB)^2

	vbuf2->mult(sbuf1); // Wa
	vbuf1->cross(vbuf2, vbuf3); //(Wa)x(rotB)
	vbuf1->neg(); // summator F: vbuf1 = - (Wa)x(rotB) 

	sbuf2->abs2(vbuf2); // Wa^2
	vbuf3->mult(sbuf2, base); // Wa^2.B
	vbuf1->add(vbuf3); // summator F: vbuf1 = - (Wa)x(rotB) + Wa^2.B

	vbuf2->cross(vbuf2, base); // (Wa)xB
	vF->cross(vbuf2, vgradW); // F~ = (Wa)xBxgradw

	vbuf3->rot(vbuf2); // rot((Wa)xB)
	vbuf1->add(vbuf3); // summator F: vbuf1 = rot((Wa)xB) - (Wa)x(rotB) + Wa^2.B

	sbuf2->div(base); // divB
	vbuf3->mult(sbuf2, base); // divB.B
	vbuf2->mult(sbuf1, vbuf3); // Wb

	sbuf1->abs2(vbuf3); // (divB.B)^2
	sL->add(sbuf1); // L = B^-2 * (rotBxB)^2 + (divB.B)^2
	sL->mult(baseW); // L = w * [B^-2 * (rotBxB)^2 + (divB.B)^2]

	sbuf1->abs2(vbuf3); // Wb^2
	vbuf3->mult(sbuf1, base); // Wb^2.B
	vbuf1->add(vbuf3); // summator F: vbuf1 = rot((Wa)xB) - (Wa)x(rotB) + (Wa^2+Wb^2).B

	sbuf1->dot(vbuf2, base); // Wb.B
	vbuf3->grad(sbuf1); // grad(Wb.B)
	vbuf1->add(vbuf3); // summator F: vbuf1 = rot((Wa)xB) - (Wa)x(rotB) +  grad(Wb.B) + (Wa^2+Wb^2).B

	vbuf2->mult(sbuf2); // Wb.divB
	vbuf1->sub(vbuf2); // summator F: vbuf1 = rot((Wa)xB) - (Wa)x(rotB) +  grad(Wb.B) - Wb.divB + (Wa^2+Wb^2).B

	vbuf1->mult(baseW); // w * summator F

	vF->add(vbuf1); // F~ = w*F + (Wa)xBxgradw

	vbuf1->mult(sbuf1, vgradW); // (Wb.B) * gradw
	vF->add(vbuf1); // F~ = w*F + (Wa)xBxgradw + (Wb.B) * gradw
*/