#' Extract a formatted correlation table
#'
#' Extract the formatted table from a [corx()] result, optionally adding
#' p-values on a separate row beneath each row of coefficients.
#'
#' @param corx A [corx()] object.
#' @param include_p Logical. If `FALSE` (the default), return the formatted
#'   table as stored in the object. If `TRUE`, add rows for the stored
#'   p-values, which are adjusted if requested in [corx()].
#' @returns If `include_p = FALSE`, the formatted character matrix. Otherwise,
#'   a data frame with alternating coefficient and p-value rows. Descriptive
#'   statistics have no corresponding p-value; those cells are missing.
#' @seealso [corx-methods], [to_clipboard()].
#' @examples
#' fit <- corx(mtcars[, 1:4])
#' to_table(fit)
#' to_table(fit, include_p = TRUE)
#' @md
#' @export

to_table <- function(corx, include_p = FALSE) {
  apa <- corx$apa
  if (!include_p) return(apa)

  # Older saved objects stored options only in the call.
  method <- attr(corx, "method", exact = TRUE)
  triangle <- attr(corx, "triangle", exact = TRUE)
  if (is.null(method)) {
    method <- corx$call$method
    triangle <- corx$call$triangle
    if (!is.null(triangle) && !is.character(triangle)) {
      stop("Recreate the corx object to recover its triangle setting.")
    }
  }
  if (is.null(method)) method <- "pearson"
  label <- switch(as.character(method), pearson = "Pearson's r",
                  spearman = "Spearman's rho", kendall = "Kendall's tau")
  if (is.null(label)) stop("Recreate the corx object to recover its correlation method.")

  p <- corx$p
  if (!is.null(triangle) && ncol(p) > 1L) {
    # Triangular tables label coefficient columns by their original positions.
    if (is.null(dim(apa))) apa <- matrix(apa, ncol = 1L, dimnames = list(names(apa), "1"))
    cols <- as.integer(colnames(apa)[seq_len(ncol(p) - 1L)])
    p <- p[, cols, drop = FALSE]
  }
  p[] <- round_p(p, leading.zero = FALSE, simplify = 1, simple_digits = 0)
  coef_cells <- apa[, seq_len(ncol(p)), drop = FALSE]
  hidden <- coef_cells %in% c("", " - ")
  p[hidden] <- coef_cells[hidden]

  rows <- seq_len(nrow(apa))
  out <- apa[rep(rows, each = 2L), , drop = FALSE]
  out[2L * rows, ] <- NA_character_
  out[2L * rows, seq_len(ncol(p))] <- p
  out <- data.frame(row = rep(rownames(apa), each = 2L),
                    info = rep(c(label, "p-value"), length(rows)),
                    out, row.names = NULL, check.names = FALSE, stringsAsFactors = FALSE)
  out[[1L]][2L * rows] <- ""
  names(out)[1:2] <- ""
  out
}

round_p <- function(p, n = 3, stars = c(), leading.zero = FALSE, apa_threshold = 0.001, simplify = .1,
                    simple_digits = 2){
  rounded = digits(p,n)
  out <- lapply(seq_along(rounded), function(x){

    if(!is.na(rounded[x])){
    #message(x)
    original = p[x]
    r_original = rounded[x]
    r = rounded[x]

    if(as.numeric(r) == 0){
      r = strsplit(r,split="")[[1]]
      r[length(r)] = 1
      r = paste(r,collapse = "")
    }

    #  add stars --------------
    stars_to_add = c()
    if(!is.null(stars)){
     stars_to_add <- lapply(stars,function(s){
       if(as.numeric(original) < s){
         return("*")
       }else{
         return(NA)
       }
      })

     stars_to_add <-
       paste(stats::na.omit(unlist(stars_to_add)), collapse = "")

    }

    if(r_original < as.numeric(r)){
      r = paste0("< ",r)
    }

    if(original < apa_threshold){
      r = paste0("< ", apa_threshold)
    }

    if(original >= simplify){
      r = digits(original, n = simple_digits)
    }

    r <- paste0(r,stars_to_add)
    if(!leading.zero) r <- gsub("0\\.", ".", r)

    return(r)

    }else{
      NA
    }
  })
  unlist(out)
}
