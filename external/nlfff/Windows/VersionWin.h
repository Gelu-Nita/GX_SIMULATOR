#pragma once
#include "../sources/VersionInfoResource.h"

#define VIR_CONC2(a, b) a##b
#define VIR_CONC2_S(a, b) VIR_CONC2(a, b)
#define VIR_CONC3(a, b, c) a##b##c
#define VIR_CONC3_S(a, b, c) VIR_CONC3(a, b, c)
#define VIR_CONC4D(a, b, c, d) a##.##b##.##c##.##d
#define VIR_CONC4C(a, b, c, d) a##,##b##,##c##,##d

#define VIR_FileDescriptionD VIR_CONC3_S(VIR_T_REV, VIR_Revision, VIR_T_LIBNAME)
#define VIR_FileDescription VIR_QUOTE_SUBST(VIR_FileDescriptionD)

#define VIR_LegalCopyrightD VIR_CONC2_S(VIR_COPYRIGHT, VIR_Year)
#define VIR_LegalCopyright VIR_QUOTE_SUBST(VIR_LegalCopyrightD)

//-------------------------------------------------------------------------
#define VIR_FileVersionD VIR_CONC4D(VIR_Ver1, VIR_Ver2, VIR_Ver3, VIR_Ver4)
#define VIR_FileVersion VIR_QUOTE_SUBST(VIR_FileVersionD)

#define VIR_ProductVersion VIR_FileVersion

#define VIR_FileVersionC VIR_CONC4C(VIR_Ver1, VIR_Ver2, VIR_Ver3, VIR_Ver4)
#define VIR_ProductVersionC VIR_FileVersionC
