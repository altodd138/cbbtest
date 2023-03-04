# Given a vector of times (ordered), and values (factor or numeric), forms
# a "time-dependent" data frame. The value of the variable changes at time s to
# the value x corresponding to s. In other words, x is the value of the variable
# on (s, t], where t is the next time the variable changes, possibly Inf
time_dep <- function(time, value, check = TRUE) {
  # Validate input
  if (check) {
    stopifnot(is.numeric(time))
    stopifnot(is.factor(value) || is.numeric(value))
    stopifnot(all(!is.na(time) & !is.na(value)))
    dat <- data.frame(time, value)
    dat <- dat[order(dat$time), ]
    dat <- dat[!duplicated(dat), ]
    if (any(duplicated(dat$time)))
      stop("some element of 'time' has multiple values")
    if (!is.infinite(dat$time[1]) || dat$time[1] >= 0)
      stop("must provide a value at -Inf (before first finite time)")
  } else {
    dat <- data.frame(time, value)
    dat <- dat[order(dat$time), ]
    dat <- dat[!duplicated(dat), ]
  }

  # Combine times for which the value does not change, and return
  dat <- dat[!equals_previous(dat$value), ]
  class(dat) <- c("time_dep", class(dat))
  dat
}

# Convenient tool for making a binary time-dependent variable with 'value_l' to
# the left of 'time', 'value_r' to the right, and possibly as a factor. We do
# not run the check since the function structures the input to 'time_dep'
bin_time_dep <- function(time, value_l = 0, value_r = 1, levels = NULL) {
  value <- c(value_l, value_r)
  if (!is.null(levels)) value <- factor(value, levels)
  if (is.infinite(time) && time > 0) {
    time_dep(-Inf, value[1], check = FALSE)
  } else {
    time_dep(c(-Inf, time), value, check = FALSE)
  }
}

# Convert a time-dependent data frame to a step function
as.stepfun.time_dep <- function(dat) {
  f_num <- `if`(
    nrow(dat) == 1,
    stepfun(0, rep(as.numeric(dat$value), 2), right = TRUE),
    stepfun(dat$time[-1], as.numeric(dat$value), right = TRUE)
  )
  f <- `if`(
    is.numeric(dat$value),
    \(x) f_num(x),
    \(x) factor(f_num(x), 1:nlevels(dat$value), levels(dat$value))
  )
  list(f = f, f_num = f_num)
}

# Plots the numeric-coded step function of a time-dependent variable
plot.time_dep <- function(dat) {
  plot(as.stepfun(dat)$f_num)
}

# Converts a single-row data frame 'dat' to a long format, using
# time-dependent variables
expand_time_dep <- function(
    dat,
    start_str,
    stop_str,
    time_dep_str,
    check = TRUE,
    progress = FALSE
) {
  # Validate input
  if (check) {
    stopifnot(is.data.frame(dat))
    cols_str <- c(start_str, stop_str, time_dep_str)
    stopifnot(all(is.character(cols_str)))
    if (!all(cols_str %in% names(dat)))
      stop("'dat' must have all of columns 'start_str', 'stop_str', 'time_dep_str'")
    if (length(unique(cols_str)) < length(cols_str))
      stop("overlapping column names in 'start_str', 'stop_str', 'time_dep_str'")
    is_time_dep <- vapply(
      dat[, time_dep_str, drop = FALSE],
      \(x) all(vapply(x, \(y) inherits(y, "time_dep"), NA)),
      NA
    )
    if (!all(is_time_dep))
      stop("columns 'time_dep_str' must be lists of class 'time_dep'")
  }

  # Set up progress bar
  n <- nrow(dat)
  if (progress) {
    cat("Expanding time-dependent variables:\n\n")
    prog <- progress_setup(n)
  }

  # Form list of expanded rows
  names_indep <- setdiff(names(dat), c(start_str, stop_str, time_dep_str))
  t_start <- dat[[start_str]]
  t_stop <- dat[[stop_str]]
  lst <- list()
  for (i in seq_len(n)) {
    if (progress) progress_update(i, prog)
    breaks <- do.call(c, lapply(time_dep_str, \(x) dat[[x]][[i]]$time))
    breaks <- sort(unique(breaks))
    breaks <- breaks[t_start[i] < breaks & breaks < t_stop[i]]

    # Find values of each time-dependent confounder L on each sub-interval
    dat_dep <- data.frame(c(t_start[i], breaks), c(breaks, t_stop[i]))
    names(dat_dep) <- c(start_str, stop_str)
    for (name in time_dep_str) {
      f <- as.stepfun(dat[[name]][[i]])$f
      dat_dep[[name]] <- f(dat_dep[[stop_str]])
    }

    # Combine with time-independent variables and return
    dat_indep <- dat[i, names_indep, drop = FALSE]
    lst[[i]] <- cbind(dat_indep, dat_dep)
  }

  # Bind and return in original column order
  do.call(rbind, lst)[, names(dat)]
}
