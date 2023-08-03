#' corx
#'
#' Calculates correlations matrices. Relevant values are stored in a list with methods for easy retrieval and formatting in publication ready tables.
#'
#' @param data data.frame or matrix
#' @param x a vector of rownames. Defaults to all
#' @param y a vector of colnames. If not supplied, y is set to x.
#' @param z a vector of variable names. Control variables to be used in partial correlations - defaults to NULL
#' @param method character. One of "pearson", "spearman", or "kendall"
#' @param round numeric. Number of digits in printing
#' @param stars a numeric vector. This argument defines cut-offs for p-value stars.
#' @param p_adjust character. What adjustment for multiple tests should be used? One of "none" (default), "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", or "fdr"
#' @param remove_lead logical. if TRUE (the default), leading zeros are removed in summaries
#' @param triangle character. one of "lower", "upper" or NULL (the default)
#' @param caption character. table caption. Passed to plots
#' @param note character. Text for a table note
#' @param describe list of named functions. If functions are supplied to describe, new columns will be bound to the 'APA matrix' for each function in the list. Describe also accepts a variety of shortcuts. If describe is set to TRUE, mean and standard deviation are returned for all row variables. Describe can accept a character vector to call the following descriptive functions: c('mean','sd','var','median','iqr','skewness','kurtosis'). These shortcuts are powered by 'tidyselect'. Skewness and kurtosis are calculated using the 'moments' package. All functions retrieved with shortcuts remove missing values.
#' @param grey_nonsig logical. Should non-significant values be grey in output? This argument does nothing if describe is not set to FALSE
#' @param call_only logical. For debugging, if TRUE only the call is returned
#' @details
#' Constructs correlation matrices using 'stats::cor.test' unless z is specified. When z is specified ppcor::ppcor.test is used instead. Character and factor variables are not accepted. To prevent errors, users must first convert all variables to numeric.
#'
#' ## Partial correlations:
#'
#' Supplying the argument z will call ppcor::pcor.test the correlation pair are supplied to arguments x and y. The vector of z given to corx is passed to argument z in pcor.test.
#'
#' ##  Missing data:
#'
#' Observations containing missing data required to complete a correlation or partial correlation are automatically removed.
#'
#' ## P-adjust:
#'
#' P-values attained can be adjusted for multiple comparisons by using the 'p_adjust' argument. This calls the function stats::p.adjust. When a matrix is symmetrical, p-values are only adjusted for unique comparisons. When a correlation matrix is not symmetrical, all comparisons are assumed to be unique.

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
#'  * "call" The call which if evaluated reproduces the object
#'  * "apa" An 'APA' formatted correlation matrix with significance stars
#'  * "r" Raw correlation coefficients
#'  * "p" p-values
#'  * "n" Pairwise observations
#'  * "caption" Object caption
#'  * "note" Object note
#' @export

corx <-
  function(data,
           x = NULL,
           y = NULL,
           z = NULL,
           method = c("pearson", "spearman", "kendall"),
           stars = c(0.05,0.01,0.001),
           p_adjust = c("none", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr"),
           round = 2,
           remove_lead = TRUE,
           triangle = NULL,
           caption = NULL,
           note = NULL,
           describe = FALSE,
           grey_nonsig = TRUE,
           call_only = FALSE) {


    call <- match.call()
    env <- environment()

    parent_env <- sys.frame(sys.parent())
    if(call_only) return(list(call = call, env = env, parent_env = parent_env))

    if(nrow(data) < 3){
      stop("Can't calculate p-values with fewer than four rows of data.")
    }

    if(methods::is(data, "matrix")) data <- data.frame(data, check.names = FALSE)

    x <- tidyselect::vars_select(colnames(data), {{x}}, .strict = TRUE)
    y <- tidyselect::vars_select(colnames(data), {{y}}, .strict = TRUE)
    z <- tidyselect::vars_select(colnames(data), {{z}}, .strict = TRUE)

    # allow rename within select
    data <- rename_if_needed(data, x)
    data <- rename_if_needed(data, y)
    data <- rename_if_needed(data, z)

    if(length(x) > 0) x <- names(x)
    if(length(y) > 0) y <- names(y)
    if(length(z) > 0) z <- names(z)

    if(length(x) == 0){
      x <- names(data)
    }

    if(length(y) == 0){
      y <- x
    }

    if(length(z) == 0){
      z <- NULL
    }

    if(length(z) > 0){ # remove partialled out variable from x and y
      x <- x[!x %in% z]
      y <- y[!y %in% z]
    }

    if(length(x) == 0 | length(y) == 0) stop("Can't partial out the entirety of x or y")

    # check classes are appropriate
    check_classes(data[,unique(c(x,y,z))], c("numeric","integer"), "All classes must be numeric.")


    method   <- method[1] # take the first method in case more than one supplied
    p_adjust <- p_adjust[1]

    cors <- cormat_list(
      data = data,
      x = x,
      y = y,
      z = z,
      method = method,
      p_adjust = p_adjust
    )

    pres_matrix <- apa_matrix(cors$r, #get apa matrix
                             cors$p,
                             stars,
                             round,
                             remove_lead,
                             triangle)

    # describe function ----------------------------------------------------

    # allow shortcuts

    all_desc <- list(mean = function(x) mean(x, na.rm=T),
                    sd = function(x) stats::sd(x, na.rm=T),
                    var = function(x) stats::var(x, na.rm = T),
                    median = function(x) stats::median(x, na.rm = T),
                    iqr = function(x) stats::IQR(x, na.rm = T),
                    skewness = function(x) moments::skewness(x, na.rm = T),
                    kurtosis = function(x) moments::kurtosis(x, na.rm =T),
                    n = function(x) digits(length(stats::na.omit(x)),0)
    )

    tryCatch({ # allow lists to be sent to tidyselect
      describe_name <- tidyselect::vars_select(names(all_desc), {{describe}}, .strict = F)
    }, error = function(e) assign("describe_name", c(), envir = env)) # assign empty vec if error

    if(length(describe_name) > 0){ # if vars were found

      if(length(describe_name) != (length(call$describe) -1)){ # check if all vars were found
        describe_name <- tidyselect::vars_select(names(all_desc), {{describe}}, .strict = T)
      }

      describe <- all_desc[describe_name] # set describe to all_desc
      names(describe) <- names(describe_name) # rename as needed
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

      data <- data.frame(data)
      x <- make.names(x)

      orig_names <- colnames(pres_matrix)
      pres_matrix <- data.frame(pres_matrix)

      for (i in seq_along(describe)) { # then apply describe function to data
        safe_round <- function(x, round){
          if(methods::is(x, "numeric")){
            return(digits(x, round))
          }
          x
        }

        pres_matrix[[names(describe)[i]]] <-
          unlist(lapply(seq_along(x), function(var) {
            val <- describe[[i]](data[, x[[var]]])
            safe_round(val, round)
          }))
      }
      pres_matrix <- as.matrix(pres_matrix)
      colnames(pres_matrix)[seq_along(orig_names)] <- orig_names
    }

    if(!is.null(triangle)){ # if triangle change names -------

      nums <- seq_along(rownames(pres_matrix))
      rownames(pres_matrix) <- paste0(nums,". ", rownames(pres_matrix))
      colnames(pres_matrix)[1:length(nums)] <- nums
      pres_matrix <- pres_matrix[,-length(nums)]
    }

    # add in note --------------------------------------------

    if(is.null(note)){

      note <- lapply(seq_along(stars), function(s){

        temp_stars <- paste(rep("*",s), collapse = "")
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

partial_n_matrix <- function(data, x, y, z){

  mx <- matrix(nrow = length(x), ncol = length(y))
  rownames(mx) <- x
  colnames(mx) <- y

  for(row in rownames(mx)){
    for(col in colnames(mx)){
      mx[row,col] <- sum(stats::complete.cases(data[,c(row, col, z)]))
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

apa_matrix <- function(r_matrix,
                      p_matrix,
                      stars,
                      round,
                      remove_lead,
                      triangle) {
  f_matrix <- r_matrix
  f_matrix[] <- digits(f_matrix , round)
  row_names <- matrix(rownames(r_matrix), nrow(r_matrix), ncol = ncol(r_matrix))
  col_names <- matrix(colnames(r_matrix), nrow = nrow(r_matrix), ncol = ncol(r_matrix), byrow = T)

  f_matrix[row_names == col_names] <- " - "

  #diag(f_matrix)

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
  s_matrix[row_names == col_names] <- ""

  f_matrix[] <- paste0(f_matrix, s_matrix)

  if (remove_lead)
    f_matrix[] <-
    gsub("0\\.", ".", f_matrix) #remove leading zeros if requested

  return(f_matrix)
}


#' print.corx
#' @param x object
#' @param ... extra arguments
#' @export
print.corx <- function(x,...){

  apa <- x$apa

  text <- utils::capture.output(print(apa, quote = F, right = T))
  width <- max(nchar(text))
  header <- text[1]

  grey <- attr(x, "grey_nonsig")
  star_call <- attr(x, "stars")

  if(length(star_call) > 0 & grey & identical(attr(x, "describe"), F)){# make nonsig grey

    if(attr(x, "round") != 0 ){ # if no decimal places change regex
      patt <- "(-)?[0-9]?\\.[0-9]{1,}(?![\\*0-9])" # possible negative, then a possible 0-9 character, then a decimal
    }else{                                        # then more 0-9 characters (at least one), but not followed by any number of stars!
      patt <- "-?[0-1](?![\\*\\.0-9]{1,})" # different pattern for round = 0 (even though no one will ever use that setting)
    }

    gr <- gregexpr(patt,text, perl = T) # get match locations
    mat <- regmatches(text,gr)
    regmatches(text,gr) <- lapply(mat, function(x) crayon::silver(x)) # replace with silver text
  }

  text <- gsub("\\bNA\\b", crayon::red("NA"), text) # make NAs red
  text <- gsub("\\*", crayon::yellow("*"),text) # make stars yelloe
  text <- gsub("\\ - ", crayon::silver(" - "),text) # make dashes silver

  text <- text[-1] # remove header
  bar <- paste(rep(crayon::silver("-"), width),collapse = "") # create a bar same length as table
  temp_note <- paste("Note.",x$note) # get note ready



  final_text <- paste(c(
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
coef.corx <- function(object, ...) object$r # coef returns r matrix

#' digits
#'
#' Consistent rounding for strings
#' @param x number to round
#' @param n number of digits

digits <- function(x, n = 2) {
  x <- round(x, n)
  x[] <- sapply(x, function(i) {
    ifelse(!is.na(i), trimws(format(round(as.numeric(as.character(i)), n), nsmall = n)),NA)
  })
  x
}

#' plot.corx
#' @param x a corx object
#' @param ... other arguments to ggcorrplot::ggcorrplot
#' @export

plot.corx <- function(x, ...){
  call <- match.call()
  elip <- list(...)

  tri <- x$call$triangle
  if(is.null(tri)) tri <- "full"
  if(!is.null(call$type)) tri <- call$type

  caption <- x$call$caption
  if(is.null(caption)) caption <- ""
  if(!is.null(call$title)) caption <- call$title

  elip[['title']] <- caption # elip is designed
  elip[['type']] <- tri      # To be a call
  elip[['corr']] <- x$r      # I'm setting arguments which will be used in do.call
  elip[['p.mat']] <- x$p     # We include the p.matrix for signifance rules in ggcorrplot

  do.call(ggcorrplot::ggcorrplot, elip)
}

#' @export
summary.corx <- function(object,... , digits, quantile.type){
  name_mat <- colnames(object$apa)
  obj <- data.frame(object$apa)
  names(obj) <- name_mat
  obj
}

#' @export
as.data.frame.corx <- function(x,...){
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

  v_is <- function(x, classes)
    any(sapply(classes, function(y) {
      x <- labelled::remove_labels(x)
      methods::is(x, y)
    }))

  classes <- lapply(data, class)

  class_ok <- sapply(data, function(x) v_is(x, ok_classes))
  bad_cols <- names(data)[!class_ok]
  bad_index <- which(names(data) %in% bad_cols)
  bad_classes <- sapply(classes[!class_ok], function(x) paste(abbreviate(x,3), collapse = ","))
  script <- paste(glue::glue("[{bad_index}] '{bad_cols}' <{bad_classes}>"), collapse = ", ")

  if (!all(class_ok)) {
    if(stop){
      stop(stop_message," ", script, ".", call. = F)
    }else{
      warning(stop_message," ", script, ".", call. = F)
    }
  }
}



#' star_matrix
#'
#' Replaces p-values with stars
#' @param m matrix of p-values
#' @param stars a vector of p-value thresholds to replace with stars

star_matrix <- function(m, stars) {
  get_stars <- function(p, stars) {
    if (is.na(p))
      p <- 1
    n_stars <- sum(p < stars)
    paste(rep("*", n_stars), collapse = "")
  }

  s_matrix <- m
  s_matrix[] <-  sapply(m, function(p)
    get_stars(p, stars = stars))
  s_matrix
}

#' rename if needed
#'
#' Renames columns
#' @param data data object
#' @param x a character vector. If named, columns will be renamed

  rename_if_needed <- function(data, x) {
      rename_vars <- x[names(x) != x]
      for (i in seq_along(rename_vars)) {
        if (names(x)[i] != x[i]) {
          colnames(data)[colnames(data) == x[i]] <- names(rename_vars[rename_vars == x[i]])
        }
      }

      data
    }
