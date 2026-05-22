#include <R.h>
#include <Rinternals.h>

extern SEXP get_screen_dimensions();

static const R_CallMethodDef CallEntries[] = {
  {"get_screen_dimensions_", (DL_FUNC) &get_screen_dimensions, 0},
  {NULL , NULL, 0}
};

void R_init_tooey(DllInfo *dll) {
  R_registerRoutines(
    dll,      // DllInfo
    NULL,      // .C
    CallEntries,  // .Call
    NULL,      // Fortran
    NULL       // External
  );
  R_useDynamicSymbols(dll, FALSE);
}
