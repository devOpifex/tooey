# Map a CSI/SS3 final letter (no numeric parameters) to a key name, e.g. the
# arrow keys (ESC [ A) and the SS3 function keys (ESC O P). Returns NULL for an
# unrecognised letter so the caller can fall back.
t_csi_letter <- function(ch) {
  switch(
    ch,
    A = "up",
    B = "down",
    C = "right",
    D = "left",
    H = "home",
    F = "end",
    Z = "shift-tab",
    P = "f1",
    Q = "f2",
    R = "f3",
    S = "f4",
    NULL
  )
}

# Map the numeric code of a `ESC [ <n> ~` sequence to a key name (navigation
# and function keys). Returns NULL for an unrecognised code.
t_csi_tilde <- function(num) {
  switch(
    as.character(num),
    "1" = "home",
    "2" = "insert",
    "3" = "delete",
    "4" = "end",
    "5" = "pageup",
    "6" = "pagedown",
    "7" = "home",
    "8" = "end",
    "11" = "f1",
    "12" = "f2",
    "13" = "f3",
    "14" = "f4",
    "15" = "f5",
    "17" = "f6",
    "18" = "f7",
    "19" = "f8",
    "20" = "f9",
    "21" = "f10",
    "23" = "f11",
    "24" = "f12",
    NULL
  )
}

# Translate an xterm modifier code into a key-name prefix. The code is
# `1 + bitmask`, where shift = 1, alt = 2, ctrl = 4 (e.g. 5 -> ctrl,
# 6 -> shift+ctrl). Returns "" for no modifier, else "ctrl-", "ctrl-shift-", etc.
t_csi_modifier <- function(num) {
  if (is.na(num) || num <= 1L) {
    return("")
  }
  m <- num - 1L
  parts <- c(
    if (bitwAnd(m, 4L) > 0L) "ctrl",
    if (bitwAnd(m, 2L) > 0L) "alt",
    if (bitwAnd(m, 1L) > 0L) "shift"
  )
  if (!length(parts)) {
    return("")
  }
  paste0(paste(parts, collapse = "-"), "-")
}

# Parse a CSI (ESC [ ...) or SS3 (ESC O ...) escape sequence into a key name.
# Handles the bare-letter forms (arrows, home/end, shift-tab, F1-F4), the
# numeric `~` forms (insert/delete/page keys, F5-F12), and modified variants
# such as `ESC [ 1 ; 5 C` (ctrl-right). Returns NULL when the sequence is
# unrecognised or incomplete.
t_parse_csi <- function(codes) {
  n <- length(codes)
  if (n < 3) {
    return(NULL)
  }

  # Bare-letter form: ESC [ A / ESC O P. The third byte is the final letter and
  # carries no numeric parameters.
  third <- codes[3]
  if (third >= 65L && third != 126L) {
    return(t_csi_letter(intToUtf8(third)))
  }

  # Parameterised form: ESC [ <num> [ ; <mod> ] <final>, where <final> is `~`
  # or a letter. Walk to the final byte, then split the parameters on `;`.
  rest <- codes[3:n]
  is_final <- rest == 126L |
    (rest >= 65L & rest <= 90L) |
    (rest >= 97L & rest <= 122L)
  final_idx <- which(is_final)[1]
  if (is.na(final_idx)) {
    return(NULL)
  }
  final <- rest[final_idx]

  param_chars <- if (final_idx > 1L) {
    intToUtf8(rest[seq_len(final_idx - 1L)])
  } else {
    ""
  }
  params <- strsplit(param_chars, ";", fixed = TRUE)[[1]]
  num1 <- suppressWarnings(as.integer(params[1]))
  modcode <- if (length(params) >= 2L) {
    suppressWarnings(as.integer(params[2]))
  } else {
    NA_integer_
  }

  key <- if (final == 126L) {
    t_csi_tilde(num1)
  } else {
    t_csi_letter(intToUtf8(final))
  }
  if (is.null(key)) {
    return(NULL)
  }
  paste0(t_csi_modifier(modcode), key)
}

t_parse_key <- function(bytes) {
  if (!length(bytes)) {
    return(NULL)
  }

  codes <- as.integer(bytes)
  n <- length(codes)
  code <- codes[1]

  # Escape sequences (ESC = 27)
  if (code == 27L) {
    # A lone ESC, with nothing following.
    if (n == 1L) {
      return("esc")
    }
    # CSI (ESC [) or SS3 (ESC O): arrows, navigation, and function keys.
    if (codes[2] == 91L || codes[2] == 79L) {
      key <- t_parse_csi(codes)
      return(if (is.null(key)) "esc" else key)
    }
    # ESC followed by any other key is Alt+<key>, e.g. ESC a -> "alt-a".
    rest <- t_parse_key(bytes[-1])
    return(if (is.null(rest)) "esc" else paste0("alt-", rest))
  }

  named <- switch(
    as.character(code),
    "9" = "tab",
    "10" = "enter",
    "13" = "enter",
    "127" = "backspace",
    "0" = "ctrl-space",
    "28" = "ctrl-\\",
    "29" = "ctrl-]",
    "30" = "ctrl-^",
    "31" = "ctrl-_",
    NULL
  )
  if (!is.null(named)) {
    return(named)
  }

  # Control characters: Ctrl-A (1) .. Ctrl-Z (26)
  if (code >= 1L && code <= 26L) {
    return(paste0("ctrl-", intToUtf8(code + 96L)))
  }

  # A leading byte >= 128 starts a multi-byte UTF-8 character; decode the whole
  # character (its length is encoded in the leading byte) rather than a single
  # byte, so accented and wide characters survive.
  if (code >= 128L) {
    len <- if (code >= 240L) {
      4L
    } else if (code >= 224L) {
      3L
    } else if (code >= 192L) {
      2L
    } else {
      1L
    }
    ch <- rawToChar(bytes[seq_len(min(len, n))])
    Encoding(ch) <- "UTF-8"
    return(ch)
  }

  rawToChar(bytes[1])
}
