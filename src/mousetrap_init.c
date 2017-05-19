#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP mousetrap_boxBlur(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP mousetrap_cleanAngles(SEXP);
extern SEXP mousetrap_computeNorm(SEXP);
extern SEXP mousetrap_distMat(SEXP, SEXP, SEXP);
extern SEXP mousetrap_distMat3d(SEXP, SEXP, SEXP, SEXP);
extern SEXP mousetrap_distMat3dV(SEXP, SEXP, SEXP, SEXP);
extern SEXP mousetrap_distMatV(SEXP, SEXP, SEXP);
extern SEXP mousetrap_gaussBlur(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP mousetrap_gaussBlurSlow(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP mousetrap_getAngleP(SEXP, SEXP);
extern SEXP mousetrap_getAnglesP(SEXP, SEXP);
extern SEXP mousetrap_getAnglesV(SEXP, SEXP);
extern SEXP mousetrap_getAngleV(SEXP, SEXP);
extern SEXP mousetrap_getlast(SEXP);
extern SEXP mousetrap_getLength(SEXP, SEXP);
extern SEXP mousetrap_getLength3d(SEXP, SEXP, SEXP);
extern SEXP mousetrap_getLengths(SEXP, SEXP);
extern SEXP mousetrap_getLengths3d(SEXP, SEXP, SEXP);
extern SEXP mousetrap_getVelocities(SEXP, SEXP);
extern SEXP mousetrap_getVelocities3d(SEXP, SEXP, SEXP);
extern SEXP mousetrap_getVelocity(SEXP, SEXP);
extern SEXP mousetrap_getVelocity3d(SEXP, SEXP, SEXP);
extern SEXP mousetrap_moments_mat(SEXP);
extern SEXP mousetrap_scale_mat(SEXP, SEXP, SEXP);
extern SEXP mousetrap_scale_rows(SEXP, SEXP, SEXP);
extern SEXP mousetrap_sd_mat(SEXP);
extern SEXP mousetrap_select_max(SEXP, SEXP);
extern SEXP mousetrap_spatialize(SEXP, SEXP, SEXP);
extern SEXP mousetrap_spatialize3d(SEXP, SEXP, SEXP, SEXP);
extern SEXP mousetrap_spatialize4d(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP mousetrap_spatializeArray(SEXP, SEXP, SEXP);
extern SEXP mousetrap_spatializeArray3d(SEXP, SEXP, SEXP, SEXP);
extern SEXP mousetrap_spatializeArrayToLong(SEXP, SEXP, SEXP);
extern SEXP mousetrap_spatializeArrayToLong3d(SEXP, SEXP, SEXP, SEXP);
extern SEXP mousetrap_spatializeArrayToLong4d(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP mousetrap_tab(SEXP, SEXP);
extern SEXP mousetrap_tab_mean(SEXP, SEXP, SEXP);
extern SEXP mousetrap_tab_sum(SEXP, SEXP, SEXP);
extern SEXP mousetrap_trajAlign(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP mousetrap_trajAlign3d(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP mousetrap_trans_mat(SEXP, SEXP, SEXP);
extern SEXP mousetrap_trans_rows(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"mousetrap_boxBlur",                 (DL_FUNC) &mousetrap_boxBlur,                 5},
    {"mousetrap_cleanAngles",             (DL_FUNC) &mousetrap_cleanAngles,             1},
    {"mousetrap_computeNorm",             (DL_FUNC) &mousetrap_computeNorm,             1},
    {"mousetrap_distMat",                 (DL_FUNC) &mousetrap_distMat,                 3},
    {"mousetrap_distMat3d",               (DL_FUNC) &mousetrap_distMat3d,               4},
    {"mousetrap_distMat3dV",              (DL_FUNC) &mousetrap_distMat3dV,              4},
    {"mousetrap_distMatV",                (DL_FUNC) &mousetrap_distMatV,                3},
    {"mousetrap_gaussBlur",               (DL_FUNC) &mousetrap_gaussBlur,               5},
    {"mousetrap_gaussBlurSlow",           (DL_FUNC) &mousetrap_gaussBlurSlow,           5},
    {"mousetrap_getAngleP",               (DL_FUNC) &mousetrap_getAngleP,               2},
    {"mousetrap_getAnglesP",              (DL_FUNC) &mousetrap_getAnglesP,              2},
    {"mousetrap_getAnglesV",              (DL_FUNC) &mousetrap_getAnglesV,              2},
    {"mousetrap_getAngleV",               (DL_FUNC) &mousetrap_getAngleV,               2},
    {"mousetrap_getlast",                 (DL_FUNC) &mousetrap_getlast,                 1},
    {"mousetrap_getLength",               (DL_FUNC) &mousetrap_getLength,               2},
    {"mousetrap_getLength3d",             (DL_FUNC) &mousetrap_getLength3d,             3},
    {"mousetrap_getLengths",              (DL_FUNC) &mousetrap_getLengths,              2},
    {"mousetrap_getLengths3d",            (DL_FUNC) &mousetrap_getLengths3d,            3},
    {"mousetrap_getVelocities",           (DL_FUNC) &mousetrap_getVelocities,           2},
    {"mousetrap_getVelocities3d",         (DL_FUNC) &mousetrap_getVelocities3d,         3},
    {"mousetrap_getVelocity",             (DL_FUNC) &mousetrap_getVelocity,             2},
    {"mousetrap_getVelocity3d",           (DL_FUNC) &mousetrap_getVelocity3d,           3},
    {"mousetrap_moments_mat",             (DL_FUNC) &mousetrap_moments_mat,             1},
    {"mousetrap_scale_mat",               (DL_FUNC) &mousetrap_scale_mat,               3},
    {"mousetrap_scale_rows",              (DL_FUNC) &mousetrap_scale_rows,              3},
    {"mousetrap_sd_mat",                  (DL_FUNC) &mousetrap_sd_mat,                  1},
    {"mousetrap_select_max",              (DL_FUNC) &mousetrap_select_max,              2},
    {"mousetrap_spatialize",              (DL_FUNC) &mousetrap_spatialize,              3},
    {"mousetrap_spatialize3d",            (DL_FUNC) &mousetrap_spatialize3d,            4},
    {"mousetrap_spatialize4d",            (DL_FUNC) &mousetrap_spatialize4d,            5},
    {"mousetrap_spatializeArray",         (DL_FUNC) &mousetrap_spatializeArray,         3},
    {"mousetrap_spatializeArray3d",       (DL_FUNC) &mousetrap_spatializeArray3d,       4},
    {"mousetrap_spatializeArrayToLong",   (DL_FUNC) &mousetrap_spatializeArrayToLong,   3},
    {"mousetrap_spatializeArrayToLong3d", (DL_FUNC) &mousetrap_spatializeArrayToLong3d, 4},
    {"mousetrap_spatializeArrayToLong4d", (DL_FUNC) &mousetrap_spatializeArrayToLong4d, 5},
    {"mousetrap_tab",                     (DL_FUNC) &mousetrap_tab,                     2},
    {"mousetrap_tab_mean",                (DL_FUNC) &mousetrap_tab_mean,                3},
    {"mousetrap_tab_sum",                 (DL_FUNC) &mousetrap_tab_sum,                 3},
    {"mousetrap_trajAlign",               (DL_FUNC) &mousetrap_trajAlign,               5},
    {"mousetrap_trajAlign3d",             (DL_FUNC) &mousetrap_trajAlign3d,             6},
    {"mousetrap_trans_mat",               (DL_FUNC) &mousetrap_trans_mat,               3},
    {"mousetrap_trans_rows",              (DL_FUNC) &mousetrap_trans_rows,              3},
    {NULL, NULL, 0}
};

void R_init_mousetrap(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
