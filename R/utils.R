# Checks whether each element of x equals the previous element
equals_previous <- function(x) {
  if (length(x) == 0) {
    logical()
  } else {
    c(FALSE, x[-1] == x[-length(x)])
  }
}

# Sets up a progress bar, and returns a data frame 'prog' indicating how many
# progress characters to add at various iterations
progress_setup <- function(n, chr = "^") {
  cat("0", paste0("---------", 1:5), "\n", chr, sep = "")
  prog <- data.frame(i = ceiling(n * 1:50 / 50), n_chr = 1:50)
  prog <- prog[!duplicated(prog$i, fromLast = TRUE), ]
  prog$n_chr <- c(prog$n_chr - c(0, prog$n_chr[-nrow(prog)]))
  prog
}

# Prints the number of progress characters given by 'prog' at iteration i
progress_update <- function(i, prog, chr = "^") {
  i_match <- match(i, prog$i)
  if (!is.na(i_match)) {
    cat(rep(chr, prog$n_chr[i_match]), sep = "")
    if (i_match == nrow(prog)) cat("\n")
  }
}
