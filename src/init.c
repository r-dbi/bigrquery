#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP null_to_na_(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"null_to_na_", (DL_FUNC) &null_to_na_, 1},
  {NULL, NULL, 0}
};

void R_init_bigrquery(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
