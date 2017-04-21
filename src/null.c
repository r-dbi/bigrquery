#include <R.h>
#include <Rinternals.h>

SEXP null_to_na_(SEXP x) {
  SEXP na_vector = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(na_vector, 0, NA_STRING);

  // One element for each row
  int n = Rf_length(x);
  for (int i = 0; i < n; ++i) {
    SEXP row = VECTOR_ELT(x, i);
    SEXP f = VECTOR_ELT(row, 0);

    int p = Rf_length(f);
    for (int j = 0; j < p; ++j) {
      SEXP val = VECTOR_ELT(f, j);

      if (Rf_isNull(val)) {
        SEXP v = PROTECT(Rf_allocVector(VECSXP, 1));
        SET_VECTOR_ELT(v, 0, Rf_duplicate(na_vector));
        SET_VECTOR_ELT(f, j, v);
        UNPROTECT(1);
      } else if  (Rf_isNull(VECTOR_ELT(val, 0))) {
        SET_VECTOR_ELT(val, 0, Rf_duplicate(na_vector));
      }
    }
  }

  UNPROTECT(1);

  return(x);
}
