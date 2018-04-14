// [[Rcpp::depends(rapidjsonr)]]
#include <Rcpp.h>
#include <rapidjson/document.h>

#include <ctime>
#include <stdlib.h>

#if defined(_WIN32) || defined(_WIN64)
#define timegm _mkgmtime
#endif

enum BqType {
  BQ_INTEGER,
  BQ_FLOAT,
  BQ_BOOLEAN,
  BQ_STRING,
  BQ_TIMESTAMP,
  BQ_TIME,
  BQ_DATE,
  BQ_DATETIME,
  BQ_RECORD
};

BqType parse_bq_type(std::string x) {
  if (x == "INTEGER") {
    return BQ_INTEGER;
  } else if (x == "FLOAT") {
    return BQ_FLOAT;
  } else if (x == "BOOLEAN") {
    return BQ_BOOLEAN;
  } else if (x == "STRING") {
    return BQ_STRING;
  } else if (x == "TIMESTAMP") {
    return BQ_TIMESTAMP;
  } else if (x == "TIME") {
    return BQ_TIME;
  } else if (x == "DATE") {
    return BQ_DATE;
  } else if (x == "DATETIME") {
    return BQ_DATETIME;
  } else if (x == "RECORD") {
    return BQ_RECORD;
  } else {
    Rcpp::stop("Unknown type %s", x);
  }
}

class BqField {
private:
  std::string name_;
  BqType type_;
  bool array_;
  std::vector<BqField> fields_;

public:
  BqField(std::string name, BqType type) : name_(name), type_(type), array_(false) {
  }

  BqField(const rapidjson::Value& field) {
    name_ = field["name"].GetString();
    array_ = std::string(field["mode"].GetString()) == "REPEATED";
    type_ = parse_bq_type(field["type"].GetString());

    if (field.HasMember("fields")) {
      const rapidjson::Value& fields = field["fields"];

      rapidjson::Value::ConstValueIterator field = fields.Begin(), end = fields.End();
      for ( ; field != end; ++field) {
        fields_.push_back(BqField(*field));
      }
    }
  }

  SEXP vectorInit(int n, bool array) const {
    if (array) {
      return Rcpp::List(n);
    }

    switch(type_) {
    case BQ_INTEGER:
      return Rcpp::IntegerVector(n);
    case BQ_FLOAT:
      return Rcpp::DoubleVector(n);
    case BQ_BOOLEAN:
      return Rcpp::LogicalVector(n);
    case BQ_STRING:
      return Rcpp::CharacterVector(n);
    case BQ_TIMESTAMP:
    case BQ_DATETIME:
      return Rcpp::DatetimeVector(n, "UTC");
    case BQ_DATE:
      return Rcpp::DateVector(n);
    case BQ_TIME:
      {
        Rcpp::DoubleVector out(n);
        out.attr("class") = "difftime";
        out.attr("units") = "secs";
        return out;
      }
    case BQ_RECORD:
      return Rcpp::List(n);
    }
  }

  SEXP vectorInit(int n) const  {
    return vectorInit(n, array_);
  }

  void vectorSet(SEXP x, int i, const rapidjson::Value& v, bool array) const {
    if (array && type_ != BQ_RECORD) {
      if (!v.IsArray())
        Rcpp::stop("Not an array [1]");

      int n = v.Size();
      SEXP out = vectorInit(n, false);
      for (int j = 0; j < n; ++j) {
        vectorSet(out, j, v[j]["v"], false);
      }

      SET_VECTOR_ELT(x, i, out);
      return;
    }

    switch(type_) {
    case BQ_INTEGER:
      INTEGER(x)[i] = v.IsString() ? atoi(v.GetString()) : NA_INTEGER;
      break;
    case BQ_FLOAT:
      REAL(x)[i] = v.IsString() ? atof(v.GetString()) : NA_REAL;
      break;
    case BQ_BOOLEAN:
      if (v.IsString()) {
        bool is_true = strncmp(v.GetString(), "T", 1) == 0;
        INTEGER(x)[i] = is_true;
      } else {
        INTEGER(x)[i] = NA_LOGICAL;
      }
      break;
    case BQ_STRING:
      if (v.IsString()) {
        SEXP chr = Rf_mkCharLenCE(v.GetString(), v.GetStringLength(), CE_UTF8);
        SET_STRING_ELT(x, i, chr);
      } else {
        SET_STRING_ELT(x, i, NA_STRING);
      }
      break;
    case BQ_TIMESTAMP:
      REAL(x)[i] = v.IsString() ? atof(v.GetString()) : NA_REAL;
      break;
    case BQ_TIME:
      if (v.IsString()) {
        struct tm tm;
        strptime(v.GetString(), "%H:%M:%S", &tm);

        REAL(x)[i] = tm.tm_hour * 3600 + tm.tm_min * 60 + tm.tm_sec;
      } else {
        REAL(x)[i] = NA_REAL;
      }
      break;
    case BQ_DATE:
      if (v.IsString()) {
        Rcpp::Date date(v.GetString());
        REAL(x)[i] = date.getDate();
      } else {
        REAL(x)[i] = NA_REAL;
      }
      break;
    case BQ_DATETIME:
      if (v.IsString()) {
        struct tm tm;
        strptime(v.GetString(), "%Y-%m-%d %H:%M:%S", &tm);
        REAL(x)[i] = timegm(&tm);
      } else {
        REAL(x)[i] = NA_REAL;
      }
      break;
    case BQ_RECORD:
      SET_VECTOR_ELT(x, i, recordValue(v));
      break;
    }
  }

  SEXP recordValue(const rapidjson::Value& v) const {
    int p = fields_.size();

    Rcpp::List out(p);
    Rcpp::CharacterVector names(p);
    out.attr("names") = names;

    if (!array_) {
      const rapidjson::Value& f = v["f"];
      // f is array of fields
      if (!f.IsArray())
        Rcpp::stop("Not array [2]");

      for (int j = 0; j < p; ++j) {
        const BqField& field = fields_[j];
        const rapidjson::Value& vs = f[j]["v"];

        int n = (field.array_) ? vs.Size() : 1;
        SEXP col = field.vectorInit(n, false);
        if (field.array_) {
          for (int i = 0; i < n; ++i) {
            field.vectorSet(col, i, vs[i]["v"], false);
          }
        } else {
          field.vectorSet(col, 0, vs);
        }

        out[j] = col;
        names[j] = field.name_;
      }
    } else {
      // v is array
      int n = v.Size();

      for (int j = 0; j < p; ++j) {
        const BqField& field = fields_[j];
        out[j] = field.vectorInit(n);
        names[j] = field.name_;
      }
      out.attr("class") = "data.frame";
      out.attr("row.names") = Rcpp::IntegerVector::create(NA_INTEGER, -n);

      for (int i = 0; i < n; ++i) {
        const rapidjson::Value& f = v[i]["v"]["f"];
        if (!f.IsArray())
          Rcpp::stop("Not an array [3]");

        for (int j = 0; j < p; ++j) {
          fields_[j].vectorSet(out[j], i, f[j]["v"]);
        }
      }
    }

    return out;
  }

  void vectorSet(SEXP x, int i, const rapidjson::Value& v) const  {
    vectorSet(x, i, v, array_);
  }

};

// [[Rcpp::export]]
SEXP bq_field_init(std::string json, std::string value = "") {
  rapidjson::Document d1;
  d1.Parse(&json[0]);

  BqField field(d1);
  SEXP out = field.vectorInit(1);

  if (value != "") {
    rapidjson::Document d2;
    d2.Parse(&value[0]);

    field.vectorSet(out, 0, d2);
  }

  return out;
}
