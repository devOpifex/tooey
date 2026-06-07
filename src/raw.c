#include <termios.h>
#include <unistd.h>
#include <stdlib.h>

#include <R.h>
#include <Rinternals.h>

struct termios orig_termios;

void disable_raw_mode() {
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
}

SEXP enable_raw_mode(SEXP auto_disable) {
  tcgetattr(STDIN_FILENO, &orig_termios);

  if(asLogical(auto_disable)) {
    atexit(disable_raw_mode);
  }

  struct termios raw = orig_termios;
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 1;

  tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);

  return R_NilValue;
}
