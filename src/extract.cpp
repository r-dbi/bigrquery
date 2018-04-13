// [[Rcpp::depends(rapidjsonr)]]
#include <Rcpp.h>
#include <stdlib.h>
#include <rapidjson/document.h>



SEXP make_cell(const rapidjson::Value& v) {

  if (v.IsString()) {
    SEXP chr = Rf_mkCharLenCE(v.GetString(), v.GetStringLength(), CE_UTF8);
    return Rf_ScalarString(chr);
  } else if (v.IsArray()) {

    int n = v.Size();
    if (n == 0) {
      return R_NilValue;
    }

    bool leaf = v[0]["v"].IsString();
    if (leaf) { // Array of strings -> character vector
      Rcpp::CharacterVector out(n);

      for (int i = 0; i < n; ++i) {
        out[i] = v[i]["v"].GetString();
      }
      return out;
    } else { // Array of structs -> data frame
      return R_NilValue;
    }
  } else if (v.IsObject()) {

    const rapidjson::Value& f = v["f"];
    int n = f.Size();

    Rcpp::List out(n);
    for (int i = 0; i < n; ++i) {
      out[i] = make_cell(f[i]["v"]);
    }
    return out;
  }

  return R_NilValue;
}

// [[Rcpp::export]]
Rcpp::List bq_tabledata_to_list(Rcpp::RawVector x) {
  std::string data(x.begin(), x.end());
  rapidjson::Document d;
  d.ParseInsitu(&data[0]);

  if (!d.HasMember("rows")) {
    return(Rcpp::List());
  }

  const rapidjson::Value& rows = d["rows"];
  int n = rows.Size();
  int p = rows[0]["f"].Size();
  // Rprintf("[%i x %i]", n, p);

  Rcpp::List out(p);
  for (int j = 0; j < p; ++j) {
    const rapidjson::Value& v = rows[0]["f"][j]["v"];

    // Unsafe in long run: first row might contain a NULL
    if (v.IsString()) {
      out[j] = Rcpp::CharacterVector(n);
    } else if (v.IsArray() || v.IsObject()) {
      out[j] = Rcpp::List(n);
    } else {
      Rcpp::stop("Column %i is not a string, array, or object", j);
    }
  }

  // Fill in values
  for (int i = 0; i < n; ++i) {
    const rapidjson::Value& f = rows[i]["f"];

    for (int j = 0; j < p; ++j) {
      const rapidjson::Value& v = f[j]["v"];

      if (v.IsString()) {
        SEXP cell = Rf_mkCharLenCE(v.GetString(), v.GetStringLength(), CE_UTF8);
        SET_STRING_ELT(VECTOR_ELT(out, j), i, cell);
      } else if (v.IsArray() || v.IsObject()) {
        SEXP cell = make_cell(v);
        SET_VECTOR_ELT(VECTOR_ELT(out, j), i, cell);
      }
    }
  }

  return out;
}
