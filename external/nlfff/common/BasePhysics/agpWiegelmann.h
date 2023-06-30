#pragma once

class CagmScalarFieldOps;
class CagmVectorFieldOps;
class CagmScalarField;
class CagmVectorField;

//------------------------------------------------------------------
class CagpWiegelmann
{
protected:
    int N[3];
    REALTYPE_A dircos[3];

    // sources input
    CagmVectorField *sourceB;
    CagmScalarField *sourceW;

    CagmVectorField *baseField;
    CagmVectorField *baseWeight;
    CagmScalarField *absField;
    CagmScalarField *absWeight;
    CagmScalarField *losField;
    CagmScalarField *losWeight;

    // sources calculate
    CagmVectorField *sourceGradW;

    // sources output
    CagmVectorFieldOps *outF;

    // input
    CagmVectorField *vB, *vgradW;
    CagmVectorField *vbaseB, *vbaseW;
    CagmScalarField *sabsB, *sabsW;
    CagmScalarField *slosB, *slosW;
    CagmScalarField *sW;

    // intermediate
  	CagmVectorField *vRotB, *Wa, *Wb, *vFt, *v;
	CagmScalarField *s3, *Wa2Wb2, *s2, *s;

    CagmVectorField *vdircos;

    // output
  	CagmVectorFieldOps *vF;

    size_t queueID;

    REALTYPE_A *Lt;

protected:

public:
    CagpWiegelmann(int *_N, int _taskID); //, CWiegelmannChunkTaskQueue *_queue);
	virtual ~CagpWiegelmann();

	uint32_t Bind(CagmVectorField *_sourceB, CagmScalarField *_sourceW, CagmVectorField *_sourceGradW,
               CagmVectorField *_baseField, CagmVectorField *_baseWeight,
               CagmScalarField *_absField, CagmScalarField *_absWeight, 
               CagmScalarField *_losField, CagmScalarField *_losWeight, 
               REALTYPE_A *_vcos,
               CagmVectorFieldOps *_outF, REALTYPE_A *_Lt);
    virtual uint32_t Allocate(int *M, REALTYPE_A *steps);

    virtual uint32_t setTaskParams(void * params, size_t queueID);
    virtual uint32_t ActionCore();
};
