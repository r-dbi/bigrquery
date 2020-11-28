
// implementation by Gábor Csárdi from the processx package:
// https://github.com/r-lib/processx/blob/master/src/base64.c

#ifndef BASE64_H
#define BASE64_H

#ifdef __cplusplus
extern "C" {
#endif

SEXP base64_encode(SEXP array);
SEXP base64_decode(SEXP array);

#ifdef __cplusplus
}
#endif

#endif /* BASE64_H */
