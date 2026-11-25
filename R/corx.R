#' Compute and format a correlation matrix
#'
#' Compute ordinary or partial correlations and format them for an APA-style
#' table. Unrounded coefficients, p-values, and observation counts are also
#' available separately.
#'
#' @param data A data frame or matrix with at least three rows. Selected columns
#'   must be numeric or integer. Numeric columns with value labels are accepted;
#'   character and factor columns are not.
#' @param x Column selection for the rows of the result. The default, `NULL`,
#'   selects all columns. Use bare names, quoted names, or tidyselect helpers
#'   such as `where(is.numeric)`. For a character vector stored in `cols`, use
#'   `tidyselect::all_of(cols)`. An explicit selection must match a column.
#'   Use `c(Label = column)` to label a variable without renaming the input.
#' @param y Column selection for the columns of the result, using the same syntax
#'   as `x`. The default, `NULL`, uses the selection in `x`. An explicit
#'   selection must match a column. Labels on the two axes may differ.
#' @param z Column selection for control variables, using the same syntax as
#'   `x`. The default, `NULL`, computes ordinary correlations. Selected controls
#'   are removed from both axes of the result.
#' @param method Correlation method: `"pearson"` (the default), `"spearman"`,
#'   or `"kendall"`.
#' @param stars Numeric p-value thresholds, listed from largest to smallest.
#'   Each threshold a p-value falls strictly below adds one star. Use `NULL`
#'   or `numeric(0)` to omit stars.
#' @param p_adjust Multiple-testing adjustment passed to `stats::p.adjust()`:
#'   `"none"` (the default), `"holm"`, `"hochberg"`, `"hommel"`,
#'   `"bonferroni"`, `"BH"`, `"BY"`, or `"fdr"`.
#' @param round Number of decimal places in the formatted table. The default
#'   is 2. This does not round the stored coefficients or p-values.
#' @param remove_lead Logical. If `TRUE` (the default), omit leading zeros from
#'   formatted coefficients (for example, `.42` instead of `0.42`).
#' @param triangle Which part of a square matrix with the same variables in the
#'   same order on both axes to show in the formatted table: `"lower"`,
#'   `"upper"`, or `NULL` (the default) for the full table. The stored numeric
#'   matrices are not cropped.
#' @param caption Optional character string printed above the table and used as
#'   the default plot title.
#' @param note Optional character string printed below the table. If `NULL`,
#'   a note is generated from the significance thresholds in `stars`.
#' @param describe Descriptive statistics to append for each row variable.
#'   Use `FALSE` (the default) for none, `TRUE` for the mean and standard
#'   deviation, or a named list of functions that each accept a column and
#'   return one value. You can also select the built-in summaries
#'   `mean`, `sd`, `var`, `median`, `iqr`, `skewness`, `kurtosis`, and `n`
#'   (the number of nonmissing values), for example `c(M = mean, n)`.
#'   The built-in summaries remove missing values from each variable
#'   independently; custom functions handle missing values themselves.
#' @param grey_nonsig Logical. If `TRUE` (the default), print coefficients
#'   without significance stars in grey when stars are enabled and
#'   `describe = FALSE`. This affects console output only.
#' @param call_only Logical. If `TRUE`, return the matched call and its
#'   environments for debugging, without computing correlations. The default
#'   is `FALSE`.
#'
#' @details
#' Ordinary Pearson correlations use the two-sided t test with `n - 2` degrees
#' of freedom. Ordinary rank correlations use the asymptotic p-values from
#' `stats::cor.test()`. Partial correlations use `ppcor::pcor.test()`.
#'
#' **Missing data.** Each pair uses only the rows observed for both variables
#' and all controls. The `n` matrix records this count, even when it is zero.
#' Ordinary correlations based on fewer than two observations have missing
#' coefficients and p-values. With exactly two observations, ordinary Pearson
#' and Spearman correlations have a coefficient but no p-value. Partial tests
#' require at least three observations beyond the number of controls (two for
#' Kendall); otherwise both the coefficient and p-value are missing.
#'
#' **Multiple comparisons.** The adjustment is applied to distinct unordered
#' variable pairs in the matrix. Self-correlations are excluded and their
#' p-values are left unchanged. If a pair appears in both orientations, both
#' cells receive the same adjusted p-value. The display selected by `triangle`
#' does not change the set of comparisons.
#'
#' @returns A list of class `corx` with the following components:
#'   * `call`: The matched function call.
#'   * `apa`: The formatted character matrix.
#'   * `r`: The unrounded matrix of correlation coefficients.
#'   * `p`: The matrix of p-values, adjusted if requested.
#'   * `n`: The matrix of observation counts.
#'   * `caption`, `note`: The text displayed with the table.
#'
#'   With `call_only = TRUE`, the result is an unclassed list containing the
#'   matched `call`, the function's evaluation environment (`env`), and the
#'   calling environment (`parent_env`).
#' @seealso [corx-methods], [print.corx()], [plot.corx()], [plot_mds()], [to_table()],
#'   [to_clipboard()].
#' @examples
#' fit <- corx(mtcars, x = c(mpg, cyl, disp), y = c(drat, qsec),
#'             z = wt, caption = "Controlling for weight",
#'             describe = list(M = function(x) mean(x, na.rm = TRUE)))
#' fit
#' coef(fit)
#' fit$p
#'
#' corx(iris[, 1:4], describe = c(median, IQR = iqr, n), triangle = "lower")
#' @md
#' @export

corx <-
  function(
    data,
    x = NULL,
    y = NULL,
    z = NULL,
    method = c("pearson", "spearman", "kendall"),
    stars = c(0.05, 0.01, 0.001),
    p_adjust = c(
      "none",
      "holm",
      "hochberg",
      "hommel",
      "bonferroni",
      "BH",
      "BY",
      "fdr"
    ),
    round = 2,
    remove_lead = TRUE,
    triangle = NULL,
    caption = NULL,
    note = NULL,
    describe = FALSE,
    grey_nonsig = TRUE,
    call_only = FALSE
  ) {
    call <- match.call()
    env <- environment()

    parent_env <- sys.frame(sys.parent())
    if (call_only) {
      return(list(call = call, env = env, parent_env = parent_env))
    }

    if (nrow(data) < 3) {
      stop("At least three rows of data are required.")
    }

    if (methods::is(data, "matrix")) {
      data <- data.frame(data, check.names = FALSE)
    }

    # Locations identify the source columns; their names label the output axes.
    x <- select_columns(data, rlang::enquo(x), stats::setNames(seq_along(data), names(data)))
    y <- select_columns(data, rlang::enquo(y), x)
    z <- select_columns(data, rlang::enquo(z), NULL, allow_empty = TRUE)
    if (length(z)) {
      z <- unique(z)
      x <- x[!x %in% z]
      y <- y[!y %in% z]
    } else {
      z <- NULL
    }

    if (!length(x) || !length(y)) {
      stop("Can't partial out the entirety of x or y")
    }

    # check classes are appropriate
    check_classes(
      data[, unique(c(x, y, z)), drop = FALSE],
      c("numeric", "integer"),
      "All classes must be numeric."
    )

    if (!is.null(triangle)) {
      triangle <- match.arg(triangle, c("lower", "upper"))
      if (!identical(unname(x), unname(y))) {
        stop("Triangular tables require the same variables in the same order on both axes.")
      }
    }
    method <- match.arg(method[1L], c("pearson", "spearman", "kendall"))
    p_adjust <- p_adjust[1]

    cors <- cormat_list(
      data = data,
      x = x,
      y = y,
      z = z,
      method = method,
      p_adjust = p_adjust
    )

    pres_matrix <- apa_matrix(
      r_matrix = cors$r, #get apa matrix
      p_matrix = cors$p,
      self = outer(unname(x), unname(y), "=="),
      stars = stars,
      round = round,
      remove_lead = remove_lead,
      triangle = triangle
    )

    # describe function ----------------------------------------------------

    # allow shortcuts

    all_desc <- list(
      mean = function(x) mean(x, na.rm = T),
      sd = function(x) stats::sd(x, na.rm = T),
      var = function(x) stats::var(x, na.rm = T),
      median = function(x) stats::median(x, na.rm = T),
      iqr = function(x) stats::IQR(x, na.rm = T),
      skewness = function(x) moments::skewness(x, na.rm = T),
      kurtosis = function(x) moments::kurtosis(x, na.rm = T),
      n = function(x) digits(length(stats::na.omit(x)), 0)
    )

    describe_name <- character()
    describe_expr <- call$describe
    run_select <- !is.null(describe_expr) &&
      !identical(describe_expr, quote(T)) &&
      !identical(describe_expr, quote(TRUE)) &&
      !identical(describe_expr, quote(F)) &&
      !identical(describe_expr, quote(FALSE))

    if (run_select) {
      describe_name <-
        tryCatch(
          {
            # allow lists to be sent to tidyselect
            eval_select_names(
              names(all_desc),
              {{ describe }},
              strict = FALSE
            )
          },
          error = function(e) character()
        )
    }

    if (length(describe_name) > 0) {
      # if vars were found

      if (length(describe_name) != (length(call$describe) - 1)) {
        # check if all vars were found
        describe_name <- eval_select_names(
          names(all_desc),
          {{ describe }},
          strict = TRUE
        )
      }

      describe <- all_desc[describe_name] # set describe to all_desc
      names(describe) <- names(describe_name) # rename as needed
    }

    if (!identical(describe, F)) {
      # if describe is selected
      if (identical(describe, T)) {
        # if it is equal to true
        describe = list(
          # define default describe functions
          "M" = function(x) {
            mean(x, na.rm = TRUE)
          },
          "SD" = function(x) {
            stats::sd(x, na.rm = TRUE)
          }
        )
      }

      orig_names <- colnames(pres_matrix)
      pres_matrix <- data.frame(pres_matrix)

      for (i in seq_along(describe)) {
        # then apply describe function to data
        safe_round <- function(x, round) {
          if (methods::is(x, "numeric")) {
            return(digits(x, round))
          }
          x
        }

        pres_matrix[[ncol(pres_matrix) + 1L]] <-
          unlist(lapply(seq_along(x), function(var) {
            val <- describe[[i]](data[[x[[var]]]])
            safe_round(val, round)
          }))
      }
      pres_matrix <- as.matrix(pres_matrix)
      colnames(pres_matrix) <- c(orig_names, names(describe))
    }

    if (!is.null(triangle)) {
      # if triangle change names -------

      nums <- seq_along(rownames(pres_matrix))
      rownames(pres_matrix) <- paste0(nums, ". ", rownames(pres_matrix))
      colnames(pres_matrix)[1:length(nums)] <- nums
      if (length(nums) > 1L) {
        omit <- if (triangle == "lower") length(nums) else 1L
        pres_matrix <- pres_matrix[, -omit, drop = FALSE]
      }
    }

    # add in note --------------------------------------------

    if (is.null(note)) {
      note <- lapply(seq_along(stars), function(s) {
        temp_stars <- paste(rep("*", s), collapse = "")
        paste0(temp_stars, " p < ", stars[s])
      })

      note <- paste(note, collapse = "; ")
    }

    c_matrix <- list(
      call = call,
      apa = pres_matrix,
      r = cors$r,
      p = cors$p,
      n = cors$n,
      caption = caption,
      note = note
    )
    class(c_matrix) <- "corx"
    attr(c_matrix, "method") <- method
    attr(c_matrix, "triangle") <- triangle
    attr(c_matrix, "grey_nonsig") <- grey_nonsig
    attr(c_matrix, "stars") <- stars
    attr(c_matrix, "round") <- round
    attr(c_matrix, "describe") <- describe

    c_matrix
  }

#' partial_n_matrix
#'
#' Calculate complete observations for a crosstab + a third variable
#' @param data data.frame or matrix
#' @param x rownames
#' @param y colnames
#' @param z partial variable vector

partial_n_matrix <- function(data, x, y, z) {
  mx <- matrix(nrow = length(x), ncol = length(y))
  rownames(mx) <- x
  colnames(mx) <- y

  for (row in rownames(mx)) {
    for (col in colnames(mx)) {
      mx[row, col] <- sum(stats::complete.cases(data[, c(row, col, z)]))
    }
  }

  as.matrix(mx)
}

#' apa matrix
#'
#' Creates an apa matrix
#' @param r_matrix correlation coefficient matrix
#' @param p_matrix p-value matrix
#' @param stars a vector of pvalue stars
#' @param round How many digits to round to?
#' @param remove_lead a logical. Should leading zeros be removed?
#' @param triangle can select lower upper or NULL
#' @param self logical matrix identifying comparisons of a variable with itself

apa_matrix <- function(
  r_matrix,
  p_matrix,
  stars,
  round,
  remove_lead,
  triangle,
  self
) {
  f_matrix <- r_matrix
  f_matrix[] <- digits(f_matrix, round)

  f_matrix[self] <- " - "

  if (!is.null(triangle)) {
    if (triangle == "lower") {
      f_matrix[upper.tri(r_matrix)] <- ""
      p_matrix[upper.tri(p_matrix)] <- 1
    }
    if (triangle == "upper") {
      f_matrix[lower.tri(r_matrix)] <- ""
      p_matrix[lower.tri(p_matrix)] <- 1
    }
  }

  s_matrix <- star_matrix(p_matrix, stars = stars)
  s_matrix[self] <- ""

  f_matrix[] <- paste0(f_matrix, s_matrix)

  if (remove_lead) {
    f_matrix[] <-
      gsub("0\\.", ".", f_matrix)
  } #remove leading zeros if requested

  return(f_matrix)
}


#' Print a formatted correlation table
#'
#' Print the original call, the formatted table, and its caption and note.
#'
#' @param x A [corx()] object.
#' @param ... Unused; included for compatibility with the `print()` generic.
#' @returns `NULL`, invisibly. The table is written to the console.
#' @seealso [corx-methods], [to_table()].
#' @md
#' @export
print.corx <- function(x, ...) {
  apa <- x$apa

  text <- utils::capture.output(print(apa, quote = F, right = T))
  width <- max(nchar(text))
  header <- text[1]

  grey <- attr(x, "grey_nonsig")
  star_call <- attr(x, "stars")

  if (length(star_call) > 0 & grey & identical(attr(x, "describe"), F)) {
    # make nonsig grey

    if (attr(x, "round") != 0) {
      # if no decimal places change regex
      patt <- "(-)?[0-9]?\\.[0-9]{1,}(?![\\*0-9])" # possible negative, then a possible 0-9 character, then a decimal
    } else {
      # then more 0-9 characters (at least one), but not followed by any number of stars!
      patt <- "-?[0-1](?![\\*\\.0-9]{1,})" # different pattern for round = 0 (even though no one will ever use that setting)
    }

    gr <- gregexpr(patt, text, perl = T) # get match locations
    mat <- regmatches(text, gr)
    regmatches(text, gr) <- lapply(mat, function(x) crayon::silver(x)) # replace with silver text
  }

  text <- gsub("\\bNA\\b", crayon::red("NA"), text) # make NAs red
  text <- gsub("\\*", crayon::yellow("*"), text) # make stars yellow
  text <- gsub("\\ - ", crayon::silver(" - "), text) # make dashes silver

  text <- text[-1] # remove header
  bar <- paste(rep(crayon::silver("-"), width), collapse = "") # create a bar same length as table
  temp_note <- paste("Note.", x$note) # get note ready

  final_text <- paste(
    c(
      crayon::blue(utils::capture.output(x$call)), # call
      "", # then an empty line
      x$caption, # table caption
      bar, # a bar
      header, # a header
      bar, # a bar
      text, # table contents
      bar, # final bar
      temp_note, # the note
      ""
    ),
    collapse = "\n"
  ) # all separated with line breaks
  cat(final_text)
}

#' Extract results from a correlation table
#'
#' Use `coef()` to extract unrounded coefficients, or use `summary()` and
#' `as.data.frame()` to extract the formatted table.
#'
#' @rdname corx-methods
#' @aliases corx-methods
#' @param object,x A [corx()] object.
#' @param ... Unused; included for compatibility with the generics.
#' @param digits,quantile.type Unused arguments to the `summary()` method.
#'   To change rounding, set `round` when calling [corx()].
#' @returns `coef()` returns the numeric coefficient matrix. `summary()` and
#'   `as.data.frame()` return the formatted table as a data frame, with the
#'   table's original column names.
#' @examples
#' fit <- corx(mtcars[, 1:3])
#' coef(fit)
#' summary(fit)
#' as.data.frame(fit)
#' @md
#' @export
coef.corx <- function(object, ...) object$r # coef returns r matrix

#' digits
#'
#' Consistent rounding for strings
#' @param x number to round
#' @param n number of digits

digits <- function(x, n = 2) {
  x <- round(x, n)
  # Many coefficients have the same printed value (at most 201 at two decimals).
  values <- unique(as.vector(x))
  labels <- sapply(values, function(i) {
    if (is.na(i)) return(NA)
    format(round(as.numeric(as.character(i)), n), nsmall = n, trim = TRUE)
  })
  x[] <- labels[match(x, values)]
  x
}

#' Plot a correlation matrix
#'
#' Plot the coefficients and p-values stored in a [corx()] object using
#' `ggcorrplot::ggcorrplot()`.
#'
#' @param x A [corx()] object.
#' @param ... Named arguments passed to `ggcorrplot::ggcorrplot()`. Use `type`
#'   or `title` to override the defaults. The coefficient and p-value matrices
#'   are supplied from `x`; do not pass `corr` or `p.mat`.
#' @details The plot uses the `triangle` setting from the original call, or
#'   `"full"` if none was supplied. The caption in the original call supplies
#'   the default title.
#' @returns A ggplot object.
#' @seealso [plot_mds()].
#' @examples
#' fit <- corx(mtcars[, 1:4])
#' plot(fit, type = "lower", title = "Motor Trend car data")
#' @md
#' @export

plot.corx <- function(x, ...) {
  elip <- list(...)
  if (is.null(elip$title)) elip$title <- if (is.null(x$caption)) "" else x$caption
  if (is.null(elip$type)) {
    tri <- attr(x, "triangle", exact = TRUE)
    if (is.null(tri) && is.character(x$call$triangle)) tri <- x$call$triangle
    elip$type <- if (is.null(tri)) "full" else tri
  }
  elip[['corr']] <- x$r # I'm setting arguments which will be used in do.call
  elip[['p.mat']] <- x$p # We include the p.matrix for signifance rules in ggcorrplot

  do.call(ggcorrplot::ggcorrplot, elip)
}

#' @rdname corx-methods
#' @export
summary.corx <- function(object, ..., digits, quantile.type) {
  name_mat <- colnames(object$apa)
  obj <- data.frame(object$apa)
  names(obj) <- name_mat
  obj
}

#' @rdname corx-methods
#' @export
as.data.frame.corx <- function(x, ...) {
  name_mat <- colnames(x$apa)
  obj <- data.frame(x$apa)
  names(obj) <- name_mat
  obj
}

#' check_classes
#'
#' check all classes are as expected
#' @param data the data object
#' @param ok_classes a vector of allowed classes
#' @param stop_message a character string provided to users if error triggers.
#' @param stop should the variable stop, or create a warning?

check_classes <- function(data, ok_classes, stop_message, stop = TRUE) {
  v_is <- function(x, classes) {
    if (!is.object(x) && inherits(x, classes)) return(TRUE)
    x <- labelled::remove_labels(x)
    any(vapply(classes, function(y) methods::is(x, y), logical(1)))
  }

  class_ok <- vapply(data, v_is, logical(1), classes = ok_classes)
  if (all(class_ok)) return(invisible(NULL))

  bad_cols <- names(data)[!class_ok]
  bad_index <- which(!class_ok)
  bad_classes <- vapply(data[!class_ok], function(x) {
    paste(abbreviate(class(x), 3), collapse = ",")
  }, character(1))
  script <- paste(
    glue::glue("[{bad_index}] '{bad_cols}' <{bad_classes}>"),
    collapse = ", "
  )

  if (stop) {
    stop(stop_message, " ", script, ".", call. = FALSE)
  } else {
    warning(stop_message, " ", script, ".", call. = FALSE)
  }
}


#' star_matrix
#'
#' Replaces p-values with stars
#' @param m matrix of p-values
#' @param stars a vector of p-value thresholds to replace with stars

star_matrix <- function(m, stars) {
  get_stars <- function(p, stars) {
    if (is.na(p)) {
      p <- 1
    }
    n_stars <- sum(p < stars)
    paste(rep("*", n_stars), collapse = "")
  }

  s_matrix <- m
  s_matrix[] <- sapply(m, function(p) {
    get_stars(p, stars = stars)
  })
  s_matrix
}
