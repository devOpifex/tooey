#include <sys/ioctl.h>
#include <stdio.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>

SEXP get_screen_dimensions()
{
  struct winsize ws;

  // Fall back to a sane default when stdout isn't a terminal (e.g. piped, or
  // under R CMD check), where the ioctl fails and leaves `ws` uninitialised.
  int cols = 80, rows = 24;
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0 && ws.ws_row > 0) {
    cols = ws.ws_col;
    rows = ws.ws_row;
  }

  SEXP result = PROTECT(allocVector(INTSXP, 2));

  INTEGER(result)[0] = cols;
  INTEGER(result)[1] = rows;

  UNPROTECT(1);
  return result;
}
