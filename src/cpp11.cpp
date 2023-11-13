// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// BqField.cpp
SEXP bq_parse(std::string meta_s, std::string data_s);
extern "C" SEXP _bigrquery_bq_parse(SEXP meta_s, SEXP data_s) {
  BEGIN_CPP11
    return cpp11::as_sexp(bq_parse(cpp11::as_cpp<cpp11::decay_t<std::string>>(meta_s), cpp11::as_cpp<cpp11::decay_t<std::string>>(data_s)));
  END_CPP11
}
// BqField.cpp
SEXP bq_field_init(std::string json, std::string value);
extern "C" SEXP _bigrquery_bq_field_init(SEXP json, SEXP value) {
  BEGIN_CPP11
    return cpp11::as_sexp(bq_field_init(cpp11::as_cpp<cpp11::decay_t<std::string>>(json), cpp11::as_cpp<cpp11::decay_t<std::string>>(value)));
  END_CPP11
}
// BqField.cpp
SEXP bq_parse_files(std::string schema_path, std::vector<std::string> file_paths, int n, bool quiet);
extern "C" SEXP _bigrquery_bq_parse_files(SEXP schema_path, SEXP file_paths, SEXP n, SEXP quiet) {
  BEGIN_CPP11
    return cpp11::as_sexp(bq_parse_files(cpp11::as_cpp<cpp11::decay_t<std::string>>(schema_path), cpp11::as_cpp<cpp11::decay_t<std::vector<std::string>>>(file_paths), cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<bool>>(quiet)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_bigrquery_bq_field_init",  (DL_FUNC) &_bigrquery_bq_field_init,  2},
    {"_bigrquery_bq_parse",       (DL_FUNC) &_bigrquery_bq_parse,       2},
    {"_bigrquery_bq_parse_files", (DL_FUNC) &_bigrquery_bq_parse_files, 4},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_bigrquery(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
