#ifndef BIGRQUERY_INTEGER64_H
#define BIGRQUERY_INTEGER64_H

#define INT64SXP REALSXP

#define NA_INTEGER64 (0x8000000000000000)

inline int64_t* INTEGER64(SEXP x) {
  return reinterpret_cast<int64_t*>(REAL(x));
}

#endif // BIGRQUERY_INTEGER64_H

// This file is a modified version of
// https://github.com/r-dbi/RPostgres/blob/master/src/integer64.h
