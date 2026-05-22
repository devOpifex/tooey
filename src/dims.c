#include <sys/ioctl.h>
#include <stdio.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>

SEXP get_screen_dimensions() 
{
  struct winsize ws;
  ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws);

  SEXP result = PROTECT(allocVector(INTSXP, 2));

  INTEGER(result)[0] = ws.ws_col;
  INTEGER(result)[1] = ws.ws_row;

  UNPROTECT(1);
  return result;
}
