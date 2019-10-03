#' corx
#'
#' Creates an object of class "corx". This class of object is a list which contains an APA formatted table, and matricies of: correlation coefficients, p-values, and observations. Methods provided for functions 'plot' and 'coef'.
#' @param data A data.frame or matrix
#' @param x a vector of character names
#' @param y a vector of character names
#' @param method One of "pearson", "spearman", or "kendall"
#' @param partial a character vector of names. Used to perform partial correlation.
#' @param round Number of digits in printing
#' @param stars a numeric vector. Stars are added to summary if p-value is lower than each value.
#' @param remove_lead a bool. if true, leading zeros are removed in summaries
#' @param triangle one of "lower", "upper" or NULL \(default\)
#' @param caption table caption
#' @param note table note
#' @param describe a list of functions with names or a logical. If functions are supplied to describe, a new column will be appended the apa matrix for each argument in the list. If TRUE is supplied, means and standard deviation is appended with na.rm = T
#' @param ... additional arguments
#' @examples
#' cor_mat <- corx(mtcars, x = c(mpg,cyl,disp),
#'    y = c(wt,drat,disp,qsec), partial = wt,
#'    round = 2, stars = c(0.05),
#'    describe = list("mean" = function(x) mean(x,na.rm=TRUE)))
#' cor_mat
#' coef(cor_mat)
#' plot(cor_mat)
#' @return A list of class 'corx'.
#' @export corx

# data = mtcars
# x = c("mpg")
# y = c("cyl")
# method = c("pearson", "spearman", "kendall")
# partial = NULL
# stars = c(0.05)
# round = 2
# remove_lead = T
# triangle = NULL
# caption = NULL
# note = NULL
# describe = T

corx <-
  function(data,
           x = NULL,
           y = NULL,
           method = c("pearson", "spearman", "kendall"),
           partial = NULL,
           stars = c(0.05),
           round = 2,
           remove_lead = T,
           triangle = NULL,
           caption = NULL,
           note = NULL,
           describe = F,
           ...) {

    call = match.call()

    #logical checks -------

    if (!class(describe) %in% c("list", "logical")) {
      stop(
        "describe must be supplied a list of functions (with names), or a logical e.g. 'TRUE'",
        call. = F
      )
    }

    classes = unlist(lapply(data, class))
    class_ok = classes %in% c("numeric","integer")
    bad_cols = names(data)[!class_ok]
    bad_classes = classes[!class_ok]
    script = paste(glue::glue("'{bad_cols}' [{bad_classes}]"),collapse = ", ")


    if(!all(class_ok)){
      stop("All classes must be numeric: ", script,".")
    }

    # allow object names ----------------------
    x = as.character(call$x)
    if(length(x)>1) x <- x[-1]

    y = as.character(call$y)
    if(length(y)>1) y <- y[-1]

    partial = as.character(call$partial)
    if(length(partial) > 1) partial <- partial[-1]
    if(length(partial) == 0) partial <- NULL

    if (length(x)==0) { #if no x
      x = names(data) # x is the name of the dataset
    }else{
    }
    if (length(y)==0) { #if no y
      y = names(data) #make y equal to x
    }else{
    }

    check_names(data, c(x, y, partial))

    if(!is.null(partial)){ #remove partialed out variable
      x = x[!x %in% partial]
      y = y[!y %in% partial]
    }

    method = method[1] # take the first method if a vector

    if (is.null(partial)) { # get correlations
      cors = psych::corr.test(data[, y], data[, x], method = method, adjust = "none")
      cors$n = psych::pairwiseCount(data[,y], data[,x])

      r_matrix = as.matrix(cors$r)
      p_matrix = as.matrix(cors$p)
      n_matrix = as.matrix(cors$n)

      colnames(r_matrix) = x ;colnames(n_matrix) = x ;colnames(p_matrix) = x
      rownames(r_matrix) = y ;rownames(n_matrix) = y ;rownames(p_matrix) = y

    } else{
      cors = partial_matrix(data, x, y, method, partial)

      r_matrix = cors$r
      p_matrix = cors$p
      n_matrix = cors$n

    }

    pres_matrix = apa_matrix(r_matrix,
                                      p_matrix,
                                      stars,
                                      round,
                                      remove_lead,
                                      triangle)

    # describe function ------------------------------

    if (!identical(describe, F)) {
      if (identical(describe, T)) {
        describe = list(
          "M" = function(x)
            mean(x, na.rm = TRUE),
          "SD" = function(x)
            stats::sd(x, na.rm = TRUE)
        )
      }

      data = data.frame(data)
      y = make.names(y)

      orig_names = colnames(pres_matrix)
      pres_matrix = data.frame(pres_matrix)

      for (i in seq_along(describe)) {
        pres_matrix[[names(describe)[i]]] =
          unlist(lapply(seq_along(y), function(var) {
            val = describe[[i]](data[, y[[var]]])
            digits(val, round)
          }))
      }
      pres_matrix = as.matrix(pres_matrix)
      colnames(pres_matrix)[seq_along(orig_names)] = orig_names
    }

    if(!is.null(triangle)){ # if triangle change names ----

      nums = seq_along(rownames(pres_matrix))
      rownames(pres_matrix) = paste0(nums,". ", rownames(pres_matrix))
      colnames(pres_matrix)[1:length(nums)] = nums
      pres_matrix = pres_matrix[,-length(nums)]
    }

    # add in note --------------------------------------------

    if(is.null(note)){

    note = lapply(seq_along(stars), function(s){

      temp_stars = paste(rep("*",s), collapse = "")
      paste0(temp_stars, " p < ", stars[s])
    })

    note = paste(note, collapse = "; ")
    }

    c_matrix = list(
      call = call,
      apa = pres_matrix,
      r = r_matrix,
      p = p_matrix,
      n = n_matrix,
      caption = caption,
      note = note
    )
    class(c_matrix) = "corx"
    return(c_matrix)
  }



#' apa matrix
#'
#' Creates an apa matrix
#' @param r_matrix correlation coefficient matrix
#' @param p_matrix p-value matrix
#' @param stars a vector of pvalue stars
#' @param round How many digits to round to?
#' @param remove_lead a bool. Should leading zeros be removed?
#' @param triangle can select lower upper or NULL

apa_matrix = function(r_matrix,
                      p_matrix,
                      stars,
                      round,
                      remove_lead,
                      triangle) {
  f_matrix = r_matrix
  f_matrix[] = digits(f_matrix , round)
  row_names = matrix(rownames(r_matrix), nrow(r_matrix), ncol = ncol(r_matrix))
  col_names = matrix(colnames(r_matrix), nrow = nrow(r_matrix), ncol = ncol(r_matrix), byrow = T)

  f_matrix[row_names == col_names] = " - "

  #diag(f_matrix)

  if (!is.null(triangle)) {
    if (triangle == "lower") {
      f_matrix[upper.tri(r_matrix)] = ""
      p_matrix[upper.tri(p_matrix)] = 1
    }
    if (triangle == "upper") {
      f_matrix[lower.tri(r_matrix)] = ""
      p_matrix[lower.tri(p_matrix)] = 1
    }
  }

  get_stars = function(p, stars) {
    n_stars = sum(p < stars)
    paste(rep("*", n_stars), collapse = "")
  }

  s_matrix = p_matrix
  s_matrix[] =  sapply(p_matrix, function(p)
    get_stars(p, stars))

  s_matrix[row_names == col_names] = ""

  f_matrix[] = paste0(f_matrix, s_matrix)

  if (remove_lead)
    f_matrix[] <-
    gsub("0\\.", ".", f_matrix) #remove leading zeros if requested

  return(f_matrix)
}


#' print.corx
#' @param x object
#' @param ... extra arguments
#' @export
print.corx = function(x,...){

apa = x$apa
text = utils::capture.output(print(apa, quote = F, right = T))
width = max(nchar(text))
text = gsub("\\*",crayon::yellow("*"),text)
text = gsub("\\ - ",crayon::silver(" - "),text)
header = text[1]
text = text[-1]
bar = paste(rep(crayon::silver("-"), width),collapse = "")
temp_note = paste("Note.",x$note)


final_text = paste(c(
  crayon::blue(utils::capture.output(x$call)),
  "",
  x$caption,
  bar,
  header,
  bar,
  text,
  bar,
  temp_note,
  ""
),
collapse = "\n")
cat(final_text)
}

#' @export
coef.corx = function(object, ...) object$r

#' digits
#'
#' Consistent rounding for strings
#' @param x number to round
#' @param n number of digits

digits = function(x, n = 2) {
  x = round(x, n)
  x[] = sapply(x, function(i) {
    ifelse(!is.na(i), trimws(format(round(as.numeric(as.character(i)), n), nsmall = n)),NA)
  })
  return(x)
}

# set classes

#' S3 class corx
#' @exportClass corx

#' plot.corx
#' @param x a value
#' @param y a value
#' @param ... other arguments
#' @export

plot.corx = function(x, y, ...){
  tri = x$call$triangle
  if(is.null(tri)) tru = "full"
  ggcorrplot::ggcorrplot(x$r, type = tri,...)
}

check_names = function (x, vars) {
  vars = unique(vars)

  if(!is.matrix(x)){
  name_data = names(x)
  }else{
    name_data = colnames(x)
  }


  error_names = vars[!vars %in% name_data]

  find_name = function(n) {
    prob_name = name_data[agrep(n, name_data)]

    if (length(prob_name) == 1) {
      return(glue::glue("'{n}' ['{prob_name}'?]"))
    } else{
      return(n)
    }


  }

  error_names = unlist(lapply(error_names, find_name))

  ifelse(length(error_names) > 1, stem <- "names", stem <- "name")

  mess1 = glue::glue(
    "{length(error_names)} {stem} could not be found: {paste(error_names, collapse = ', ')}."
  )

  if (length(error_names) > 0) {
    stop(mess1, call. = F)
  }
}

#' @export
summary.corx = function(object,... , digits, quantile.type){
  data.frame(object$apa)
}

