#include <fstream>
#include <string>
#include <vector>
#include <grpc/grpc.h>
#include <grpc/support/log.h>
#include <grpcpp/grpcpp.h>
#include "google/cloud/bigquery/storage/v1/stream.pb.h"
#include "google/cloud/bigquery/storage/v1/storage.pb.h"
#include "google/cloud/bigquery/storage/v1/storage.grpc.pb.h"
#include <Rcpp.h>
#include <Rinternals.h>

using google::cloud::bigquery::storage::v1::ReadSession;
using google::cloud::bigquery::storage::v1::BigQueryRead;

// Define a default logger for gRPC
void rgpr_default_log(gpr_log_func_args* args) {
  Rcpp::Rcerr << args->message << std::endl;
}

// Set gRPC default logger
// [[Rcpp::export]]
void bqs_init_logger() {
  gpr_set_log_function(rgpr_default_log);
}

// Set gRPC verbosity level
// [[Rcpp::export]]
void bqs_set_log_verbosity(int severity) {
  //-1 UNSET
  // 0 DEBUG
  // 1 INFO
  // 2 ERROR
  // 3 QUIET
  gpr_set_log_verbosity(static_cast<gpr_log_severity>(severity));
}

// Check gRPC version
// [[Rcpp::export]]
std::string grpc_version() {
  std::string version;
  version += grpc_version_string();
  version += " ";
  version += grpc_g_stands_for();
  return version;
}

//' Simple read file to read configuration from json
std::string readfile(std::string filename)
{
  std::ifstream ifs(filename);
  std::string content( (std::istreambuf_iterator<char>(ifs) ),
                       (std::istreambuf_iterator<char>()    ) );
  return content;
}

//' append std::string at the end of a std::vector<uint8_t> vector
void to_raw(const std::string input, std::vector<uint8_t>* output) {
  output->insert(output->end(), input.begin(), input.end());
}

class BigQueryReadClient {
public:
  BigQueryReadClient(std::shared_ptr<grpc::Channel> channel)
    : stub_(BigQueryRead::NewStub(channel)) {
  }
  void SetClientInfo(const std::string &client_info) {
    client_info_ = client_info;
  }
  ReadSession CreateReadSession(const std::string& project,
                                const std::string& dataset,
                                const std::string& table,
                                const std::string& parent,
                                const std::int64_t& timestamp_seconds,
                                const std::int32_t& timestamp_nanos,
                                const std::vector<std::string>& selected_fields,
                                const std::string& row_restriction
  ) {
    google::cloud::bigquery::storage::v1::CreateReadSessionRequest method_request;
    ReadSession *read_session = method_request.mutable_read_session();
    std::string table_fullname = "projects/" + project + "/datasets/" + dataset + "/tables/" + table;
    read_session->set_table(table_fullname);
    read_session->set_data_format(google::cloud::bigquery::storage::v1::DataFormat::ARROW);
    if (timestamp_seconds > 0 || timestamp_nanos > 0) {
      read_session->mutable_table_modifiers()->mutable_snapshot_time()->set_seconds(timestamp_seconds);
      read_session->mutable_table_modifiers()->mutable_snapshot_time()->set_nanos(timestamp_nanos);
    }
    if (!row_restriction.empty()) {
      read_session->mutable_read_options()->set_row_restriction(row_restriction);
    }
    for (int i = 0; i < int(selected_fields.size()); i++) {
      read_session->mutable_read_options()->add_selected_fields(selected_fields[i]);
    }
    method_request.set_parent("projects/" + parent);
    grpc::ClientContext context;
    context.AddMetadata("x-goog-request-params", "read_session.table=" + table_fullname);
    context.AddMetadata("x-goog-api-client", client_info_);
    ReadSession method_response;

    // The actual RPC.
    grpc::Status status = stub_->CreateReadSession(&context, method_request, &method_response);
    if (!status.ok()) {
      std::string err;
      err += "gRPC method CreateReadSession error -> ";
      err += status.error_message();
      Rcpp::stop(err.c_str());
    }
    return method_response;
  }
  void ReadRows(const std::string stream,
                std::vector<uint8_t>* ipc_stream,
                std::int64_t& n,
                long int& rows_count,
                long int& pages_count) {

    grpc::ClientContext context;
    context.AddMetadata("x-goog-request-params", "read_stream=" + stream);
    context.AddMetadata("x-goog-api-client", client_info_);

    google::cloud::bigquery::storage::v1::ReadRowsRequest method_request;
    method_request.set_read_stream(stream);
    method_request.set_offset(0);

    google::cloud::bigquery::storage::v1::ReadRowsResponse method_response;

    std::unique_ptr<grpc::ClientReader<google::cloud::bigquery::storage::v1::ReadRowsResponse> > reader(
        stub_->ReadRows(&context, method_request));
    if (n >= 0) {
      while (reader->Read(&method_response)) {
        if (method_request.offset() >= n) {
          break;
        }
        to_raw(method_response.arrow_record_batch().serialized_record_batch(), ipc_stream);
        method_request.set_offset(method_request.offset() + method_response.row_count());
        pages_count += 1;
        R_CheckUserInterrupt();
      }
    } else {
      while (reader->Read(&method_response)) {
        to_raw(method_response.arrow_record_batch().serialized_record_batch(), ipc_stream);
        method_request.set_offset(method_request.offset() + method_response.row_count());
        pages_count += 1;
        R_CheckUserInterrupt();
      }
      grpc::Status status = reader->Finish();
      if (!status.ok()) {
        std::string err;
        err += "grpc method ReadRows error -> ";
        err += status.error_message();
        Rcpp::stop(err.c_str());
      }
    }
    rows_count += method_request.offset();
  }
private:
  std::unique_ptr<BigQueryRead::Stub> stub_;
  std::string client_info_;
};

//' @noRd
// [[Rcpp::export]]
Rcpp::List bqs_ipc_stream(std::string project,
                           std::string dataset,
                           std::string table,
                           std::string parent,
                           std::int64_t n,
                           std::string client_info,
                           std::string service_configuration,
                           std::string access_token,
                           std::string root_certificate,
                           std::int64_t timestamp_seconds,
                           std::int32_t timestamp_nanos,
                           std::vector<std::string> selected_fields,
                           std::string row_restriction) {

  std::shared_ptr<grpc::ChannelCredentials> channel_credentials;
  if (access_token.empty()) {
    channel_credentials = grpc::GoogleDefaultCredentials();
  } else {
    grpc::SslCredentialsOptions ssl_options;
    if (!root_certificate.empty()) {
      ssl_options.pem_root_certs = readfile(root_certificate);
    }
    channel_credentials = grpc::CompositeChannelCredentials(
      grpc::SslCredentials(ssl_options),
      grpc::AccessTokenCredentials(access_token));
  }
  grpc::ChannelArguments channel_arguments;
  channel_arguments.SetServiceConfigJSON(readfile(service_configuration));

  BigQueryReadClient client(
      grpc::CreateCustomChannel("bigquerystorage.googleapis.com:443",
                                channel_credentials,
                                channel_arguments));

  client.SetClientInfo(client_info);

  std::vector<uint8_t> schema;
  std::vector<uint8_t> ipc_stream;
  long int rows_count = 0;
  long int pages_count = 0;

  // Retrieve ReadSession
  ReadSession read_session = client.CreateReadSession(project,
                                                      dataset,
                                                      table,
                                                      parent,
                                                      timestamp_seconds,
                                                      timestamp_nanos,
                                                      selected_fields,
                                                      row_restriction);
  // Add schema to IPC stream
  to_raw(read_session.arrow_schema().serialized_schema(), &schema);

  // Add batches to IPC stream
  for (int i = 0; i < read_session.streams_size(); i++) {
    client.ReadRows(read_session.streams(i).name(), &ipc_stream, n, rows_count, pages_count);
  }

  gpr_log(GPR_INFO, "Streamed %ld rows in %ld messages.", rows_count, pages_count);

  Rcpp::List li = Rcpp::List::create(schema, ipc_stream);

  // Return stream
  return li;
}
