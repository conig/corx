#' Extract a formatted correlation table
#'
#' Extract the formatted table from a [corx()] result, optionally adding
#' p-values on a separate row beneath each row of coefficients.
#'
#' @param corx A [corx()] object.
#' @param include_p Logical. If `FALSE` (the default), return the formatted
#'   table as stored in the object. If `TRUE`, add rows for the stored
#'   p-values, which are adjusted if requested in [corx()].
#' @details For Pearson correlations, leave `method` at its default in
#'   [corx()] when using `include_p = TRUE`. Explicitly setting
#'   `method = "pearson"` currently causes an error in that output mode.
#'   Use the full table rather than triangular formatting when adding
#'   p-values to a two-variable table.
#' @returns If `include_p = FALSE`, the stored formatted table: usually a
#'   character matrix (see `triangle` in [corx()]). Otherwise, a data frame with
#'   alternating coefficient and p-value rows.
#' @seealso [corx-methods], [to_clipboard()].
#' @examples
#' fit <- corx(mtcars[, 1:4])
#' to_table(fit)
#' to_table(fit, include_p = TRUE)
#' @md
#' @export

to_table <- function(corx, include_p = FALSE){

  if(is.null(corx$call$method)) {
    method <- "Pearson's r"
  } else{
    if (corx$call$method == "spearman") {
      method <- "Spearman's rho"
    }
    if (corx$call$method == "kendall") {
      method <- "Kendall's tau"
    }
  }

  if(!include_p){
    return(corx$apa)
  }

  p_values <- corx$p
  p_values[] <- round_p(p_values, leading.zero = FALSE,
                        simplify = 1, simple_digits = 0)

  if(!is.null(corx$call$triangle)){
    if(corx$call$triangle == "lower"){
      p_values[!lower.tri(p_values)] <- ""
      p_values <- p_values[,-ncol(p_values)]
    }
    if(corx$call$triangle == "upper"){
      p_values[lower.tri(p_values)] <- ""
      p_values <- p_values[,-ncol(p_values)]
    }

  }

    out <- lapply(seq_len(nrow(corx$apa)), function(i){

      first_line <- data.frame(row = rownames(corx$apa)[i],
                          info = method)

      first_line <- cbind(first_line, as.data.frame(t(corx$apa[i,])))

      p_val.i <- p_values[i,]
      p_val.i[corx$apa[i,] == " - "] <- " - "

      second_line <- data.frame(row = "", info = "p-value")

      second_line <- cbind(second_line, as.data.frame(t(p_val.i)))
      colnames(second_line) <- colnames(first_line)

      rbind(first_line, second_line)
    })


  out <- do.call(rbind, out)
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

digits <- function(x, n = 2) {
  x = round(x, n)
  x[] = sapply(x, function(i) {
         ifelse(!is.na(i), trimws(format(round(as.numeric(as.character(i)), n), nsmall = n)),NA)
      })
  return(x)
}
