#include <R.h>
#include <Rinternals.h>

extern SEXP get_screen_dimensions();
extern SEXP enable_raw_mode();
extern SEXP disable_raw_mode();

static const R_CallMethodDef CallEntries[] = {
  {"get_screen_dimensions_", (DL_FUNC) &get_screen_dimensions, 0},
  {"enable_raw_mode_", (DL_FUNC) &enable_raw_mode, 0},
  {"disable_raw_mode_", (DL_FUNC) &disable_raw_mode, 1},
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
