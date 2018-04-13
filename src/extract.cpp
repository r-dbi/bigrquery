// [[Rcpp::depends(rapidjsonr)]]
#include <Rcpp.h>
#include <stdlib.h>
#include <rapidjson/document.h>

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
    out[j] = Rcpp::CharacterVector(n);
  }

  // Fill in values
  for (int i = 0; i < n; ++i) {
    const rapidjson::Value& f = rows[i]["f"];

    for (int j = 0; j < p; ++j) {
      const rapidjson::Value& v = f[j]["v"];

      if (v.IsString()) {
        SEXP chr = Rf_mkCharLenCE(v.GetString(), v.GetStringLength(), CE_UTF8);
        SET_STRING_ELT(VECTOR_ELT(out, j), i, chr);
      }

    }
  }

  return out;
}
