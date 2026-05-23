#include <termios.h>
#include <unistd.h>
#include <stdlib.h>

struct termios orig_termios;

void disable_raw_mode() {
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
}

void enable_raw_mode(int auto_disable) {
  tcgetattr(STDIN_FILENO, &orig_termios);
  if(auto_disable) {
    atexit(disable_raw_mode);
  }
  struct termios raw = orig_termios;
  raw.c_lflag &= ~(ECHO | ICANON | ISIG);
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}
