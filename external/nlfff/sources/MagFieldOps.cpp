// MagFieldOps.cpp : Defines the initialization routines for the DLL.
//

#include "stdDefinitions.h"
#include <ctime>
#include "string.h"

#include "MagFieldOps.h"

#include "mfoGlobals.h"
#include "VersionInfoResource.h"

#include "agmScalarField.h"
#include "agmVectorField.h"

//------------------------------------------------------------------
bool mapIntproceed(bool bGet, std::string name, int &value, int defaultValue = 0)
{
    auto search = mapInt.find(name);
    bool rc = search != mapInt.end();
    if (bGet) // return true, if exist
    {
        if (rc)
            value = search->second;
        else
            value = defaultValue;
    }
    else
    {
        mapInt[name] = defaultValue;
    }

    return rc;
}

//------------------------------------------------------------------
bool mapDoubleproceed(bool bGet, std::string name, double &value, double defaultValue = 0)
{
    auto search = mapDouble.find(name);
    bool rc = search != mapDouble.end();
    if (bGet) // return true, if exist
    {
        if (rc)
            value = search->second;
        else
            value = defaultValue;
    }
    else
    {
        mapDouble[name] = defaultValue;
    }

    return rc;
}

//------------------------------------------------------------------
void _proceedGlobals(bool bGet)
{
    mapIntproceed   (bGet, "synchronous", WiegelmannSynchronous, 1);
    mapIntproceed   (bGet, "show_progress", WiegelmannShowProgress, 1);
    mapDoubleproceed(bGet, "show_progress_time_quant", WiegelmannShowProgressTimeQuant, 1);
    mapIntproceed   (bGet, "protocol_step", WiegelmannProtocolStep, 10);

    mapIntproceed   (bGet, "weight_type", WiegelmannWeightType, SWF_COS);
    mapDoubleproceed(bGet, "weight_bound_size", WiegelmannWeightBound, 0.1);
    mapDoubleproceed(bGet, "weight_relative_divfree", WiegelmannWeightDivfree, 1.0);

    mapIntproceed   (bGet, "timeout_ms", WiegelmannProcTimeoutMS, 60000);

    mapIntproceed   (bGet, "derivative_stencil", WiegelmannDerivStencil, 3);

    mapDoubleproceed(bGet, "step_initial", WiegelmannProcStep0, 0.1); // units of Bav/Fmax
    mapDoubleproceed(bGet, "step_initial_abs", WiegelmannProcUsePrev, 0);
    mapDoubleproceed(bGet, "step_initial_abs_factor", WiegelmannProcPrevStepFactor, 0.8);
    
    mapIntproceed   (bGet, "step_max", WiegelmannProcStepMax, 100000); // max. aval. step
    mapIntproceed   (bGet, "max_iterations", WiegelmannProcMaxSteps, 100000); // max. number of step

    mapDoubleproceed(bGet, "step_increment_init", WiegelmannProcStepIncrInit, 1.1);
    mapDoubleproceed(bGet, "step_increment", WiegelmannProcStepIncrMatr, 1.1);
    mapDoubleproceed(bGet, "step_increment_main", WiegelmannProcStepIncrMain, 1.1);
    mapDoubleproceed(bGet, "step_decrement_init", WiegelmannProcStepDecrInit, 0.9);
    mapDoubleproceed(bGet, "step_decrement", WiegelmannProcStepDecrMatr, 0.9);
    mapDoubleproceed(bGet, "step_decrement_main", WiegelmannProcStepDecrMain, 0.9);
    mapDoubleproceed(bGet, "step_terminate_init", WiegelmannProcStepLimInit, 0.01); // related to init. step
    mapDoubleproceed(bGet, "step_terminate", WiegelmannProcStepLimMatr, 0.01);
    mapDoubleproceed(bGet, "step_terminate_main", WiegelmannProcStepLimMain, 0.01);
    mapDoubleproceed(bGet, "d_functional_value_terminate_init", WiegelmannProcdLStopInit, 1e-4); // dL value ...
    mapDoubleproceed(bGet, "d_functional_value_terminate", WiegelmannProcdLStopMatr, 1e-4); 
    mapDoubleproceed(bGet, "d_functional_value_terminate_main", WiegelmannProcdLStopMain, 1e-4);
    mapIntproceed   (bGet, "d_functional_iterations_terminate_init", WiegelmannProcdLIterInit, 100); // ... during consequently iterations
    mapIntproceed   (bGet, "d_functional_iterations_terminate", WiegelmannProcdLIterMatr, 100);
    mapIntproceed   (bGet, "d_functional_iterations_terminate_main", WiegelmannProcdLIterMain, 100);
    mapDoubleproceed(bGet, "d_functional_value_max_inceasing_init", WiegelmannProcFunctionalLimitInit, 1e-4); // max. rel. avail. L increasing
    mapDoubleproceed(bGet, "d_functional_value_max_inceasing", WiegelmannProcFunctionalLimitMatr, 1e-4);
    mapDoubleproceed(bGet, "d_functional_value_max_inceasing_main", WiegelmannProcFunctionalLimitMain, 1e-4);
    mapDoubleproceed(bGet, "d_functional_stdev_value_init", WiegelmannProcdLStdValInit, 5e-4); // min std(rel. dL) ...
    mapDoubleproceed(bGet, "d_functional_stdev_value", WiegelmannProcdLStdValMatr, 5e-4);
    mapDoubleproceed(bGet, "d_functional_stdev_value_main", WiegelmannProcdLStdValMain, 5e-4);
    mapIntproceed   (bGet, "d_functional_stdev_window_init", WiegelmannProcdLStdWinInit, 11); // ... std window size + 1
    mapIntproceed   (bGet, "d_functional_stdev_window", WiegelmannProcdLStdWinMatr, 11);
    mapIntproceed   (bGet, "d_functional_stdev_window_main", WiegelmannProcdLStdWinMain, 11);

    mapIntproceed   (bGet, "dense_grid_use", WiegelmannMatryoshkaUse, 1);
    mapIntproceed   (bGet, "dense_grid_min_n", WiegelmannMatryoshkaDeepMinN, 25);
    mapDoubleproceed(bGet, "dense_grid_factor", WiegelmannMatryoshkaFactor, 2.0);
    int iWiegelmannMatryoshkaInterpolator;
    mapIntproceed(bGet, "dense_grid_interpolator", iWiegelmannMatryoshkaInterpolator, 0);
    mapDoubleproceed(bGet, "dense_grid_interpolator_p1", WiegelmannMatryoshkaInterpolatorP1, 2.0);
    mapDoubleproceed(bGet, "dense_grid_interpolator_p2", WiegelmannMatryoshkaInterpolatorP2, 8.0);
    mapDoubleproceed(bGet, "dense_grid_interpolator_p3", WiegelmannMatryoshkaInterpolatorP3, 0.0);

    mapIntproceed   (bGet, "add_conditions_mode", WiegelmannProcCondType,  1); // 0 - ignore, 1 - as functional, 2 - as fixed

    mapIntproceed   (bGet, "add_conditions_abs", WiegelmannProcCondAbs,   2);  // 0 - no limit, 1 - no less, 2 - exact
    mapIntproceed   (bGet, "add_conditions_abs_max", WiegelmannProcCondAbs2,  1); // 0 - no limit, 1 - no greater
    mapIntproceed   (bGet, "add_conditions_los", WiegelmannProcCondLOS,   2);
    mapIntproceed   (bGet, "add_conditions_los_max", WiegelmannProcCondLOS2,  1);
    mapIntproceed   (bGet, "add_conditions_xyz", WiegelmannProcCondBase,  2);
    mapIntproceed   (bGet, "add_conditions_xyz_max", WiegelmannProcCondBase2, 1);

    mapIntproceed   (bGet, "chunk_size_max", WiegelmannChunkSizeMax, 100);
    mapIntproceed   (bGet, "chunk_size_opt", WiegelmannChunkSizeOpt, 30);
    mapIntproceed   (bGet, "chunk_tasks", WiegelmannChunkTasks, 0);
    //mapIntproceed   (bGet, "threads_priority", WiegelmannThreadPriority, THREAD_PRIORITY_LOWEST);
    mapIntproceed(bGet, "threads_priority", WiegelmannThreadPriority, 0);

    mapIntproceed   (bGet, "metrics_theta", WiegelmannGetMetricsTheta, 0);
    mapIntproceed   (bGet, "metrics_diff_init", WiegelmannGetMetricsDiffInit, 0);
    mapIntproceed   (bGet, "metrics_diff_prev", WiegelmannGetMetricsDiffPrev, 0);

    if (bGet)
    {
        WiegelmannMatryoshkaInterpolator = (CagmVectorFieldOps::Interpolator)iWiegelmannMatryoshkaInterpolator;
    }
}

//------------------------------------------------------------------
__declspec( dllexport ) uint32_t utilInitialize()
{
    _proceedGlobals(false);
    _proceedGlobals();
    return 0;
}

//------------------------------------------------------------------
__declspec( dllexport ) int utilSetInt(char *query, int value)
{
    bool isNew = mapIntproceed(false, query, value, value);
    _proceedGlobals();
    return (isNew ? 1 : 0);
}

//------------------------------------------------------------------
__declspec( dllexport ) int utilGetInt(char *query, int *result)
{ 
    return mapIntproceed(true, query, *result, 0);
}

//------------------------------------------------------------------
__declspec( dllexport ) int utilSetDouble(char *query, REALTYPE_A value)
{
    bool isNew = mapDoubleproceed(false, query, value, value);
    _proceedGlobals();
    return (isNew ? 1 : 0);
}

//------------------------------------------------------------------
__declspec( dllexport ) int utilGetDouble(char *query, REALTYPE_A *result)
{ 
    return mapDoubleproceed(true, query, *result, 0);
}

//------------------------------------------------------------------
__declspec(dllexport) int utilGetVersion(char *fullvers, int buflength)
{
    std::string s;
    s = VIR_ProductName;
    s += " v.";
    s += VIR_QUOTE_SUBST(VIR_Ver1);
    s += ".";
    s += VIR_QUOTE_SUBST(VIR_Ver2);
    s += ".";
    s += VIR_QUOTE_SUBST(VIR_Ver3);
    s += ".";
    s += VIR_QUOTE_SUBST(VIR_Ver4);
    s += " (";
    s += VIR_QUOTE_SUBST(VIR_T_REV);
    s += VIR_QUOTE_SUBST(VIR_Revision);
    s += "). ";
    s += VIR_QUOTE_SUBST(VIR_COPYRIGHT);
    s += ", ";
    s += VIR_QUOTE_SUBST(VIR_FROM);
    s += VIR_QUOTE_SUBST(VIR_Year);
    s += ", ";
    s += VIR_CompanyName;
    
    strncpy(fullvers, s.c_str(), buflength);

    return (int)s.length();
}
