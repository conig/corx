#' corx
#'
#' Creates an object of class "corx". This function calculates correlation matricies. It stores effect sizes, p-values, the number of pairwise observations, and a formatted correlation matrix in a list. Partial correlations can be calculated if the 'partial' argument is assigned. Methods are exported for the generic functions 'print', 'plot', summary, data.frame and 'coef'.
#' @param data A data.frame or matrix
#' @param x a vector of rownames. Defaults to all
#' @param y a vector of colnames. Defaults to all
#' @param method One of "pearson", "spearman", or "kendall"
#' @param partial a vector of colnames. Control variables to be used in partial correlations - defaults to NULL
#' @param round Number of digits in printing
#' @param stars a numeric vector. This argument defines cut-offs for p-value stars. Multiple numbers can be given if you want multiple stars
#' @param remove_lead a bool. if TRUE \(the default\), leading zeros are removed in summaries
#' @param triangle one of "lower", "upper" or NULL \(the default\)
#' @param caption table caption. Will be passed to plots
#' @param note table note
#' @param grey_nonsig a bool. Should nonsig values be grey in output? Nonsig values are identified by the lack of a star using regex
#' @param describe a list of functions with names or a logical. If functions are supplied to describe, a new column will be appended the apa matrix for each item in the list. If TRUE is supplied, means and standard deviation is appended \(Missing values listwise deleted\)
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
           grey_nonsig = T,
           ...) {

    call = match.call()

    if(nrow(data) < 3){
      stop("Can't calculate p-values with fewer than four rows of data.")
    }

    #logical checks -------

    if (!class(describe) %in% c("list", "logical")) {
      stop(
        "describe must be supplied a list of functions (with names), or a logical e.g. 'TRUE'",
        call. = F
      )
    }

    check_classes(data, c("numeric","integer"), "All classes must be numeric.")

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
      cors = psych::corr.test(data[, x], data[, y], method = method, adjust = "none")
      cors$n = psych::pairwiseCount(data[,x], data[,y])

      r_matrix = as.matrix(cors$r)
      p_matrix = as.matrix(cors$p)
      n_matrix = as.matrix(cors$n)

      colnames(r_matrix) = y ;colnames(n_matrix) = y ;colnames(p_matrix) = y
      rownames(r_matrix) = x ;rownames(n_matrix) = x ;rownames(p_matrix) = x

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
    attr(c_matrix, "grey_nonsig") = grey_nonsig
    attr(c_matrix, "stars") = stars
    attr(c_matrix, "round") = round
    attr(c_matrix, "describe") = describe

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
    if(is.na(p)) p <- 1
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
header = text[1]

grey = attr(x, "grey_nonsig")
star_call = attr(x, "stars")

if(length(star_call) > 0 & grey & identical(attr(x, "describe"), F)){# make nonsig grey

  if(attr(x, "round") != 0 ){ # if no decimal places change regex
    patt = "(-)?[0-9]?\\.[0-9]{1,}(?![\\*0-9])"
  }else{
    patt = "-?[0-1](?![\\*\\.0-9]{1,})"
  }

  gr = gregexpr(patt,text, perl = T)
  mat = regmatches(text,gr)
  regmatches(text,gr) = lapply(mat, function(x) crayon::silver(x))
}

text = gsub("\\bNA\\b",crayon::red("NA"), text) #make NA red
text = gsub("\\*",crayon::yellow("*"),text)
text = gsub("\\ - ",crayon::silver(" - "),text)


#text = gsub("\\_"," \033[90m",text)
#text = gsub("\\|","\033[39m ",text)

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
#' @param x a corx object
#' @param ... other arguments to ggcorrplot::ggcorrplot
#' @export

plot.corx = function(x, ...){
  call = match.call()
  elip <- list(...)

  tri = x$call$triangle
  if(is.null(tri)) tri = "full"
  if(!is.null(call$type)) tri = call$type

  caption = x$call$caption
  if(is.null(caption)) caption = ""
  if(!is.null(call$title)) caption = call$title

  elip[['title']] = caption
  elip[['type']] = tri
  elip[['corr']] = x$r

  do.call(ggcorrplot::ggcorrplot, elip)
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
  name_mat = colnames(object$apa)
  obj = data.frame(object$apa)
  names(obj) = name_mat
  return(obj)
}

#' @export
as.data.frame.corx = function(x,...){
  name_mat = colnames(x$apa)
  obj = data.frame(x$apa)
  names(obj) = name_mat
  return(obj)
}

#' check_classes
#'
#' check all classes are as expected
#' @param data the data object
#' @param ok_classes a vector of allowed classes
#' @param stop_message a character string provided to users if error triggers.

check_classes = function(data, ok_classes, stop_message) {
  classes = lapply(data, class)
  class_ok = classes %in% ok_classes
  bad_cols = names(data)[!class_ok]
  bad_index = which(names(data) %in% bad_cols)
  bad_classes = lapply(classes[!class_ok], function(x) paste(abbreviate(x,3), collapse = ","))
  script = paste(glue::glue("[{bad_index}] '{bad_cols}' <{bad_classes}>"), collapse = ", ")

  if (!all(class_ok)) {
    stop(stop_message," ", script, ".", call. = F)
  }
}









