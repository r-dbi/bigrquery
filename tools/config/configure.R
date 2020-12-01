# Prepare your package for installation here.
# Use 'define()' to define configuration variables.
# Use 'configure_file()' to substitute configuration values.

# Check OS
win <- .Platform$OS.type == "windows"

# Find RTOOLS40

install_with_pacman <- function(pkg, rtools40, win) {
	arch <- switch(win, "64" = "x86_64", "32" = "i686")
	# try cran mirrors
	pacman <- function() {
		system(sprintf("%s/usr/bin/pacman -Syu", rtools40))
	  system(
	  	sprintf(
	  		"%s/usr/bin/pacman -S --noconfirm mingw-w64-%s-%s",
	  		rtools40,
	  		arch,
	  		pkg
	  	)
	  )
	}
  if (pacman() != 0) {
  	# cran mirrors does not have the package, check bintray (move it to top mirrors)
  	mirrors <- sprintf("%s/etc/pacman.d/mirrorlist.mingw%s", rtools40, win)
  	lines <- readLines(mirrors)
  	b <- grep("bintray", lines)
  	s <- grep("^Server", lines)[1] - 1L
  	linesx <- append(lines[-b], lines[b], s)
  	writeLines(linesx, mirrors)
  	pacman()
  	on.exit(writeLines(lines, mirrors), add = TRUE)
  }
}

if (win) {
	message("*** searching for RTOOLS40 mingw binaries ...", appendLF = FALSE)
	RTOOLS40_ROOT <- gsub("\\\\", "/", Sys.getenv("RTOOLS40_HOME", "c:/rtools40"))
	WIN <- if (.Platform$r_arch == "x64") {"64"} else {"32"}
	MINGW_PREFIX <- paste0("/mingw", WIN)
	BINPREF <- Sys.getenv("BINPREF", paste0(RTOOLS40_ROOT, MINGW_PREFIX, "/bin/"))
	if (dir.exists(BINPREF)) {
		Sys.setenv(PATH = paste(BINPREF, Sys.getenv("PATH"), sep = ";"))
		if (Sys.which("grpc_cpp_plugin") == "") {
			# attempt to install grpc and protoc using msys2
			install_with_pacman("grpc", RTOOLS40_ROOT, WIN)
		}
		message(" OK")
	} else {
		message(" Failed")
	}
}

# Find protoc and plugin to generate grpc/pb C++ code and pkg-config
detect_binary <- function(binary) {
	message(sprintf("*** searching for %s ...", binary), appendLF = FALSE)
	path <- Sys.which(binary)
	if (path == "") {
		message(" Failed")
		stop("Could not find ", binary)
	} else {
		message(" OK")
	}
	path
}

protoc <- detect_binary("protoc")
grpc_cpp_plugin <- detect_binary("grpc_cpp_plugin")
pkg_config <- detect_binary("pkg-config")


# Use packages proto files and determine build order
base_proto_path <- "./inst/protos/"
protos <- dir(base_proto_path, ".proto", recursive = TRUE)

## identify service protos
services <- character()
for (proto in protos) {
	if (
		any(
			grepl("^service ",
						readLines(
							file.path(base_proto_path, proto)
						)
			)
		)
	) {
		services <- c(services, proto)
	}
}

## compile order
compile_order <- function(pbpath, bpath) {
	import_order <- function(pbpath) {
		imports <- lapply(
			pbpath,
			function(path) {
				grep("^import", readLines(path), value = TRUE)
			}
		)
		imports <- gsub("import \"|\";",
										"",
										unique(unlist(imports)))
		if (length(imports) > 0) {
			imports <- file.path(bpath, imports)
			imports <- imports[file.exists(imports)]
			return(unique(c(import_order(imports), imports, pbpath)))
		}
		return()
	}
	p <- gsub(paste0("^", bpath),"", import_order(file.path(bpath, pbpath)))
	gsub("^[\\/]", "", p)
}

protos <- compile_order(services, base_proto_path)

ipath <- base_proto_path
debian_local <- "/usr/local/include/"
if (dir.exists(debian_local)) {
	ipath <- c(debian_local, base_proto_path)
}

message("*** compiling proto files ...")
system(
	sprintf(
		"%s %s --cpp_out=./src %s",
		protoc,
		paste0("-I=", ipath, collapse = " "),
		paste(protos, collapse = " ")))
system(
	sprintf(
		"%s %s --plugin=protoc-gen-grpc=%s --grpc_out=./src %s",
		protoc,
		paste0("-I=", ipath, collapse = " "),
		grpc_cpp_plugin, paste(services, collapse = " ")))

# fix OPTIONAL conflict in field_behavior.pb.h enum
field_behavior <- "src/google/api/field_behavior.pb.h"
if (file.exists(field_behavior)) {
	lines <- readLines(field_behavior)
	x <- grep("^enum FieldBehavior", lines)
	linesx <- c(lines[1:(x - 1)],
							"#undef OPTIONAL",
							lines[x:length(lines)])
	writeLines(linesx, field_behavior)
}


pkg_sources <- sort(dir("./src", ".cpp$|.c$"), decreasing = TRUE)

# define variable for template
define(CPPF = paste(system(sprintf("%s --cflags grpc", pkg_config), intern = TRUE), "-DSTRICT_R_HEADERS"))
define(CXXF = paste("-I.", if (isTRUE(win)) {"-D_WIN32_WINNT=0x600"}))
define(LIBS = sprintf(
	"%s %s",
	system(sprintf("%s --libs protobuf grpc++", pkg_config), intern = TRUE),
	if (isTRUE(win)) {"-lcrypt32 -lws2_32 -limagehlp"} else {""}))
define(TARGETS = paste(
	c(gsub(".proto$", ".pb.o", protos),
		gsub(".proto$", ".grpc.pb.o", services),
		gsub(".cpp$|.c$", ".o", pkg_sources)),
	collapse = " "))
