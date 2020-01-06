#' corx
#'
#' Creates an object of class 'corx'. This function calculates correlation matrices. It stores effect sizes, p-values, the number of pairwise observations, and a formatted correlation matrix in a list. The argument 'z' allows for control variables to be assigned. If z does not equal NULL, partial correlations are performed. Methods are exported for the generic functions 'print', 'plot', 'summary', 'data.frame' and, 'coef'.
#' @param data A data.frame or matrix
#' @param x a vector of rownames. Defaults to all
#' @param y a vector of colnames. If not supplied, y is set to x.
#' @param z a vector of colnames. Control variables to be used in partial correlations - defaults to NULL
#' @param method a string. One of "pearson", "spearman", or "kendall"
#' @param round a scalar. Number of digits in printing
#' @param stars a numeric vector. This argument defines cut-offs for p-value stars.
#' @param remove_lead a logical. if TRUE (the default), leading zeros are removed in summaries
#' @param triangle one of "lower", "upper" or NULL (the default)
#' @param caption table caption. Passed to plots
#' @param note table note
#' @param describe a list of functions. If functions are supplied to describe, new columns will be bound to the 'APA matrix' for each function in the list. Describe also accepts a variety of shortcuts. If describe is set to TRUE, mean and standard deviation are returned for all row variables. Describe can accept a character vector to call the following descriptive functions: c('mean','sd','var','median','iqr','skewness','kurtosis'). These shortcuts are powered by 'tidyselect'. Skewness and kurtosis are calculated using the 'moments' package. All functions retrieved with shortcuts remove missing values.
#' @param grey_nonsig a logical. Should non-significant values be grey in output? This argument does nothing if describe is not set to FALSE
#' @details 'corx' constructs intercorrelation matrices using 'psych::corr.test'. P-values attained are not adjusted for multiple comparisons. The argument z can be used to specify control variables. If control variables are specified, partial correlations are calculated using 'ppcor::ppcor.test'. Asymmetrical correlation matrices can be constructed using the arguments 'x' and 'y'. The arguments 'x', 'y', and 'z' are powered by 'tidyselect::vars_select'.
#' @examples
#' cor_mat <- corx(mtcars, x = c(mpg,cyl,disp), y = c(wt,drat,disp,qsec),
#'            z = wt, round = 2, stars = c(0.05),
#'            caption = "Controlling for weight" ,
#'            describe = list("mean" = function(x) mean(x,na.rm=TRUE)))
#' cor_mat
#' coef(cor_mat)
#' cor_mat$p
#' plot(cor_mat)
#' cor_2 <- corx(iris[,-5], describe = c(median, IQR = iqr, kurt = kurtosis),
#'          note = "Using shortcuts to select describe functions", triangle = "lower")
#' cor_2
#' @return A list of class 'corx' which includes:
#' \itemize{
#'  \item{"call"}{ The call}
#'  \item{"apa"}{ An 'APA' formatted correlation matrix with significance stars}
#'  \item{"r"}{ Raw correlation coefficients}
#'  \item{"p"}{ Raw p-values}
#'  \item{"n"}{ Pairwise observations}
#'  \item{"caption"}{ Object caption}
#'  \item{"note"}{ Object note}
#' }
#' @export corx

corx <-
  function(data,
           x = NULL,
           y = NULL,
           z = NULL,
           method = c("pearson", "spearman", "kendall"),
           stars = c(0.05),
           round = 2,
           remove_lead = TRUE,
           triangle = NULL,
           caption = NULL,
           note = NULL,
           describe = FALSE,
           grey_nonsig = TRUE) {


    call = match.call()
    env = environment()
    parent_env = sys.frame(sys.parent())
    #return(list(call = call, env = env, parent_env = parent_env))

    if(nrow(data) < 3){
      stop("Can't calculate p-values with fewer than four rows of data.")
    }

    #logical checks -------

    # if (!class(describe) %in% c("list", "logical")) {
    #   stop(
    #     "describe must be supplied a list of functions (with names), or a logical e.g. 'TRUE'",
    #     call. = F
    #   )
    # }

    # select vars and check names ----------------------

    x = tidyselect::vars_select(colnames(data), {{x}}, .strict = F)
    y = tidyselect::vars_select(colnames(data), {{y}}, .strict = F)
    z = tidyselect::vars_select(colnames(data), {{z}}, .strict = F)

    # allow rename within select
    rename_if_needed = function(data, x){
      if(any(names(x) != x) & length(x) > 0){
        rename_vars = x[ names(x) != x]
        colnames(data)[colnames(data) %in% rename_vars] = names(rename_vars)
      }
      return(data)
    }

    data = rename_if_needed(data, x)
    data = rename_if_needed(data, y)
    data = rename_if_needed(data, z)

    if(length(x) > 0) x <- names(x)
    if(length(y) > 0) y <- names(y)
    if(length(z) > 0) z <- names(z)

    # --

    check_for_vec = function(names, sym, env){

      if(length(names) == 0 & !is.null(sym)){

        if(as.character(sym) %in% ls(envir = env)){
          #message("getting vec")
          names = get(as.character(sym))
        }

      } else{
        #message("names not length 0 or sym was null")
      }

      return(names)
    }

    x = check_for_vec(x, call$x, parent_env)
    y = check_for_vec(y, call$y, parent_env)
    z = check_for_vec(z, call$z, parent_env)

    # message(length(x))

    get_input = function(x){ # grab plain text input
      x = as.character(x)
      if(length(x)>1) x <- x[-1]
      return(x)
    }
    x_orig = get_input(call$x)
    y_orig = get_input(call$y)
    z_orig = get_input(call$z)

    to_check = c()

    if(length(x) == 0 & length(x_orig) > 0){ # did the user try to get a var and failed
      to_check = c(to_check, x_orig) # check what the deal is
    }
    if(length(y) == 0 & length(y_orig) > 0){
      to_check = c(to_check, y_orig)
    }
    if(length(z) == 0 & length(z_orig) > 0){
      to_check = c(to_check, z_orig)
    }

    check_names(data, unique(to_check))


    #check_names(data, c(x, y, z)) # check all names are present

    if(length(x) == 0){
      x = names(data)
    }

    if(length(y) == 0){
      y = x
    }

    if(length(z) == 0){
      z = NULL
    }

    if(length(z) > 0){ #remove partialed out variable from x and y
      x = x[!x %in% z]
      y = y[!y %in% z]
    }

    if(length(x) == 0 | length(y) == 0) stop("Can't partial out the entirety of x or y")

    # check classes are appropriate
    check_classes(data[,unique(c(x,y,z))], c("numeric","integer"), "All classes must be numeric.")

    method = method[1] # take the first method in case more than one supplied

    if (length(z) == 0) { # if no partial set
      cors = psych::corr.test(data[, x], data[, y], method = method, adjust = "none") # standard cors
      cors$n = psych::pairwiseCount(data[,x], data[,y])

      r_matrix = as.matrix(cors$r)
      p_matrix = as.matrix(cors$p)
      n_matrix = as.matrix(cors$n)

      colnames(r_matrix) = y ;colnames(n_matrix) = y ;colnames(p_matrix) = y # sometimes col/row names get stripped
      rownames(r_matrix) = x ;rownames(n_matrix) = x ;rownames(p_matrix) = x # so I force them back on

    } else{

      cors = partial_matrix(data, x = x, y = y, method = method, partial = z) # if partial not null use different method

      r_matrix = cors$r
      p_matrix = cors$p
      n_matrix = cors$n

    }

    pres_matrix = apa_matrix(r_matrix, #get apa matrix
                                      p_matrix,
                                      stars,
                                      round,
                                      remove_lead,
                                      triangle)

    # describe function ----------------------------------------------------

    # allow shortcuts

    all_desc = list(mean = function(x) mean(x, na.rm=T),
                    sd = function(x) stats::sd(x, na.rm=T),
                    var = function(x) stats::var(x, na.rm = T),
                    median = function(x) stats::median(x, na.rm = T),
                    iqr = function(x) stats::IQR(x, na.rm = T),
                    skewness = function(x) moments::skewness(x, na.rm = T),
                    kurtosis = function(x) moments::kurtosis(x, na.rm =T)
                    )

    tryCatch({ # allow lists to be sent to tidyselect
    describe_name = tidyselect::vars_select(names(all_desc), {{describe}}, .strict = F)
    }, error = function(e) assign("describe_name", c(), envir = env)) # assign empty vec if error

    if(length(describe_name) > 0){ # if vars were found

      if(length(describe_name) != (length(call$describe) -1)){ # check if all vars were found
      describe_name = tidyselect::vars_select(names(all_desc), {{describe}}, .strict = T)
      }

      describe = all_desc[describe_name] # set describe to all_desc
      names(describe) = names(describe_name) # rename as needed
    }

    if (!identical(describe, F)) { # if describe is selected
      if (identical(describe, T)) { # if it is equal to true
        describe = list( # define default describe functions
          "M" = function(x)
            mean(x, na.rm = TRUE),
          "SD" = function(x)
            stats::sd(x, na.rm = TRUE)
        )
      }

      data = data.frame(data)
      x = make.names(x)

      orig_names = colnames(pres_matrix)
      pres_matrix = data.frame(pres_matrix)

      for (i in seq_along(describe)) { # then apply describe function to data
        pres_matrix[[names(describe)[i]]] =
          unlist(lapply(seq_along(x), function(var) {
            val = describe[[i]](data[, x[[var]]])
            digits(val, round)
          }))
      }
      pres_matrix = as.matrix(pres_matrix)
      colnames(pres_matrix)[seq_along(orig_names)] = orig_names
    }

    if(!is.null(triangle)){ # if triangle change names -------

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
#' @param remove_lead a logical. Should leading zeros be removed?
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

  s_matrix = star_matrix(p_matrix, stars = stars)
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
    patt = "(-)?[0-9]?\\.[0-9]{1,}(?![\\*0-9])" # possible negative, then a possible 0-9 character, then a decimal
  }else{                                        # then more 0-9 characters (at least one), but not followed by any number of stars!
    patt = "-?[0-1](?![\\*\\.0-9]{1,})" # different pattern for round = 0 (even though no one will ever use that setting)
  }

  gr = gregexpr(patt,text, perl = T) # get match locations
  mat = regmatches(text,gr)
  regmatches(text,gr) = lapply(mat, function(x) crayon::silver(x)) # replace with silver text
}

text = gsub("\\bNA\\b", crayon::red("NA"), text) # make NAs red
text = gsub("\\*", crayon::yellow("*"),text) # make stars yelloe
text = gsub("\\ - ", crayon::silver(" - "),text) # make dashes silver

text = text[-1] # remove header
bar = paste(rep(crayon::silver("-"), width),collapse = "") # create a bar same length as table
temp_note = paste("Note.",x$note) # get note ready



final_text = paste(c(
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
collapse = "\n") # all separated with line breaks
cat(final_text)
}

#' @export
coef.corx = function(object, ...) object$r # coef returns r matrix

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

  elip[['title']] = caption # elip is designed
  elip[['type']] = tri      # To be a call
  elip[['corr']] = x$r      # I'm setting arguments which will be used in do.call
  elip[['p.mat']] = x$p     # We include the p.matrix for signifance rules in ggcorrplot

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

#' star_matrix
#'
#' Replaces p-values with stars
#' @param m matrix of p-values
#' @param stars a vector of p-value thresholds to replace with stars

star_matrix = function(m, stars) {
  get_stars = function(p, stars) {
    if (is.na(p))
      p <- 1
    n_stars = sum(p < stars)
    paste(rep("*", n_stars), collapse = "")
  }

  s_matrix = m
  s_matrix[] =  sapply(m, function(p)
    get_stars(p, stars = stars))
  return(s_matrix)
}








