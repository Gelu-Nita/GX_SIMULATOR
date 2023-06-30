#pragma once

#include "agmScalarField.h"
#include "agmVectorField.h"
#include "agmMetrics.h"
#ifdef __GNUC__
#define __cdecl __attribute__((cdecl))
#define __declspec(dllexport) __attribute__((visibility ("default")))
#endif

void _proceedGlobals(bool bGet = true);

typedef uint32_t (*PROTO_mfoWiegelmannCallback) (REALTYPE_A dstep, int d, int nChunks, int nTasks, 
                                                      int depth, REALTYPE_A ddL, CagmMetrics *_metrics,
                                                      CagmVectorField *, int *stop);

typedef int (*PROTO_utilInitialize) ();
typedef int (*PROTO_utilSetInt) (char *query, int value);
typedef int (*PROTO_utilSetDouble) (char *query, double value);
typedef int (*PROTO_utilGetInt) (char *query, int *result);
typedef int (*PROTO_utilGetDouble) (char *query, double *result);

typedef uint32_t (*PROTO_mfoWiegelmannProc) (CagmVectorField *, CagmScalarField *, 
                                                  CagmVectorField *, CagmVectorField *, CagmVectorField *, CagmVectorField *,
                                                  CagmScalarField *, CagmScalarField *, CagmScalarField *, CagmScalarField *,
                                                  CagmScalarField *, CagmScalarField *, CagmScalarField *, CagmScalarField *, 
                                                  REALTYPE_A *, PROTO_mfoWiegelmannCallback);

typedef int (*PROTO_utilGetVersion) (char *, int);

typedef uint32_t (*PROTO_mfoGetLinesV) (int *N, CagmVectorField *v,
    uint32_t _cond, REALTYPE_A chromoLevel,
    int *Rx, int *Ry, int *Rz,
    int nProc,
    REALTYPE_A step, REALTYPE_A tolerance, REALTYPE_A boundAchieve,
    int *_nLines, int *_nPassed,
    int *_voxelStatus, REALTYPE_A *_physLength, REALTYPE_A *_avField,
    int *_linesLength, int *_codes, int *_globalIdx,
    int *_startIdx, int *_endIdx,
    uint64_t _maxCoordLength, uint64_t *_totalLength, REALTYPE_A *_coords, uint64_t *_linesStart, int *_linesIndex, int *seedIdx);

typedef uint32_t (*PROTO_mfoGetLines) (int *N, double *Bx, double *By, double *Bz,
    uint32_t _cond, REALTYPE_A chromoLevel,
    int *Rx, int *Ry, int *Rz,
    int nProc,
    REALTYPE_A step, REALTYPE_A tolerance, REALTYPE_A boundAchieve,
    int *_nLines, int *_nPassed,
    int *_voxelStatus, REALTYPE_A *_physLength, REALTYPE_A *_avField,
    int *_linesLength, int *_codes, int *_globalIdx,
    int *_startIdx, int *_endIdx,
    uint64_t _maxCoordLength, uint64_t *_totalLength, REALTYPE_A *_coords, uint64_t *_linesStart, int *_linesIndex, int *seedIdx);

typedef int (*PROTO_mfoNLFFF) (int argc, void* argv[]);
typedef int (*PROTO_mfoLines) (int argc, void* argv[]);
typedef int (*PROTO_mfoNLFFFVersion) (int argc, void* argv[]);

extern "C" {
__declspec( dllexport ) uint32_t utilInitialize();
__declspec( dllexport ) int utilSetInt(char *, int);
__declspec( dllexport ) int utilGetInt(char *, int *);
__declspec( dllexport ) int utilSetDouble(char *, REALTYPE_A);
__declspec( dllexport ) int utilGetDouble(char *, REALTYPE_A *);

__declspec( dllexport ) uint32_t mfoWiegelmannProcedure(CagmVectorField *, CagmScalarField *,
                                                     CagmVectorField *, CagmVectorField *, CagmVectorField *, CagmVectorField *,
                                                     CagmScalarField *, CagmScalarField *, CagmScalarField *, CagmScalarField *,
                                                     CagmScalarField *, CagmScalarField *, CagmScalarField *, CagmScalarField *,
                                                     REALTYPE_A *, PROTO_mfoWiegelmannCallback);

__declspec( dllexport ) int utilGetVersion(char *, int);

__declspec(dllexport) uint32_t mfoGetLinesV(int *N,
    CagmVectorField *v,
    uint32_t _cond = 0x3, REALTYPE_A chromoLevel = 0,
    REALTYPE_A  *_seeds = nullptr, int _Nseeds = 0,
    int nProc = 0,
    REALTYPE_A step = 1.0, REALTYPE_A tolerance = 1e-3, REALTYPE_A boundAchieve = 1e-3,
    int *_nLines = nullptr, int *_nPassed = nullptr,
    int *_voxelStatus = nullptr, REALTYPE_A *_physLength = nullptr, REALTYPE_A *_avField = nullptr,
    int *_linesLength = nullptr, int *_codes = nullptr,
    int *_startIdx = nullptr, int *_endIdx = nullptr,
    uint64_t _maxCoordLength = 0, uint64_t *_totalLength = nullptr, REALTYPE_A *_coords = nullptr, uint64_t *_linesStart = nullptr, int *_linesIndex = nullptr, int *seedIdx = nullptr);

__declspec(dllexport) uint32_t mfoGetLines(int *N,
    REALTYPE_A *Bx, REALTYPE_A *By, REALTYPE_A *Bz,
    uint32_t _cond = 0x3, REALTYPE_A chromoLevel = 0,
    REALTYPE_A  *_seeds = nullptr, int _Nseeds = 0,
    int nProc = 0,
    REALTYPE_A step = 1.0, REALTYPE_A tolerance = 1e-3, REALTYPE_A boundAchieve = 1e-3,
    int *_nLines = nullptr, int *_nPassed = nullptr,
    int *_voxelStatus = nullptr, REALTYPE_A *_physLength = nullptr, REALTYPE_A *_avField = nullptr,
    int *_linesLength = nullptr, int *_codes = nullptr,
    int *_startIdx = nullptr, int *_endIdx = nullptr, int *_apexIdx = nullptr,
    uint64_t _maxCoordLength = 0, uint64_t *_totalLength = nullptr, REALTYPE_A *_coords = nullptr, uint64_t *_linesStart = nullptr, int *_linesIndex = nullptr, int *seedIdx = nullptr);

// __declspec(dllexport) uint32_t mfoWiegelmannProcedure(CagmVectorField *, CagmScalarField *, CagmVectorField *, CagmScalarField *, ................);

// IDL interface
__declspec( dllexport ) int mfoNLFFF(int argc, void* argv[]);
__declspec( dllexport ) int mfoLines(int argc, void* argv[]);
__declspec( dllexport ) int mfoNLFFFVersion(int argc, void* argv[]);

}