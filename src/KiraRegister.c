 /* Este arquivo he nessario para rodar os arquivos em C no R, alem a mudanda em NAMESPACE*/
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void R_init_MData(DllInfo* info) {
  R_registerRoutines(info, NULL, NULL, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}

