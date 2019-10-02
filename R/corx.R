#' corx
#'
#' Create a correlation matrix
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
#' @param ... additional arguments
#' @export corx

# data = mtcars
# x = NULL
# y = NULL
# method = c("pearson", "spearman", "kendall")
# partial = NULL
# stars = c(0.05)
# round = 2
# remove_lead = T
# triangle = NULL
# caption = NULL
# note = NULL

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
           ...) {

    call = match.call()

    if (is.null(x)) { #if no x
      x = names(data) # x is the name of the dataset
    }
    if (is.null(y)) { #if no y
      y = names(data) #make y equal to x
    }

    if(!is.null(partial)){
      x = x[!x %in% partial]
      y = y[!y %in% partial]
    }

    method = method[1] # take the first method if a vector

    all_vars = unique(c(x,y))

    combos = t(utils::combn(all_vars,2)) # get all combos
    duplis = cbind(all_vars, all_vars)

    results = data.frame(rbind(combos, duplis), stringsAsFactors = F)
    names(results) = c("Var1","Var2")

    cors = lapply(seq_along(results$Var1), function(r) { # calc results
      #message(r)
      get_cor(data,
              results$Var1[r],
              results$Var2[r],
              method = method,
              partial = partial)
    })

    cors = do.call(rbind, cors) #combine into data.frame

    results = data.frame(cbind(results, cors)) # add to table

    r_matrix = par_matrix(results, x, y, "r") # extract matricies
    p_matrix = par_matrix(results, x, y, "p")
    n_matrix = par_matrix(results, x, y, "n")
    pres_matrix = apa_matrix(r_matrix,
                                      p_matrix,
                                      stars,
                                      round,
                                      remove_lead,
                                      triangle)

    if(is.null(note)){

    note = lapply(seq_along(stars), function(s){

      temp_stars = paste(rep("*",s), collapse = "")
      paste0(temp_stars, " p < ", stars[s])
    })

    note = paste(note, collapse = "; ")
    }

    c_matrix = list(call = call,apa = pres_matrix, r = r_matrix, p = p_matrix, n = n_matrix, caption = caption,
                    note = note)
    class(c_matrix) = "corx"
    return(c_matrix)
  }

#' get_cor
#'
#' A flexible correlation function
#' @param data data
#' @param x variable 1
#' @param y variable 2
#' @param method correlation method
#' @param partial control for anything?

get_cor = function(data, x, y, method, partial) {

  data = data.frame(data)
  x = make.names(x)
  y = make.names(y)
  if (length(partial) == 0) {
    data = stats::na.omit(data[,c(x,y)])

    result = stats::cor.test(data[, x],
                    data[, y],
                      method = method)
    r = result$estimate
    if(x==y) r <- 1
    p = result$p.value
    n = nrow(data)

  } else {
    partial_data = stats::na.omit(data[, c(x, y, partial)])
    n = nrow(partial_data)

    if(x != y){
    result = ppcor::pcor.test(partial_data[, x], partial_data[,
                                                       y], partial_data[, partial], method = method)
    r = result$estimate
    p = result$p.value
    }else{
      r = 1
      p = 0
    }

  }
  return(data.frame(r = r,p = p, n = n))
}

#' par_matrix
#'
#' This function is used to construct final matricies
#' @param results results dataset
#' @param x one set of variables
#' @param y another set of variables
#' @param par the parameter to build with

par_matrix = function(results, x, y, par){
  m = matrix(nrow = length(y), ncol = length(x))
  rownames(m) = y
  colnames(m) = x

  contains_var = function(r,c){
    r_var = rownames(m)[r]
    c_var = colnames(m)[c]

    bool = unlist(lapply(seq_along(results$Var1),function(i){
      Var1_in =  all(c(results$Var1[i], results$Var2[i]) %in% c(r_var,c_var) &
                       c(r_var,c_var) %in% c(results$Var1[i], results$Var2[i]))
    }))

    return(bool)

  }

  for(r in seq_along(rownames(m))){
    for(c in seq_along(colnames(m))){
 #message(r); message(c)
      rows = contains_var(r,c)

      m[r,c] = results[rows, par]

    }
  }

  return(m)
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

apa_matrix = function(r_matrix, p_matrix, stars, round, remove_lead, triangle) {
  f_matrix = r_matrix
  f_matrix[] = NA

  for (r in seq_along(rownames(r_matrix))) {
    for (c in seq_along(colnames(r_matrix))) {
#message(r);message(c)
      col1 = rownames(r_matrix)[r]
      col2 = colnames(r_matrix)[c]

      if(col1 == col2){
        f_matrix[r,c] = "-"
      }else{

      temp_r = r_matrix[r,c]
      temp_p = p_matrix[r,c]

      n_stars = sum(temp_p < stars)
      add_stars = paste(rep("*", n_stars), collapse = "")

      f_matrix[r,c] = paste0(digits(temp_r,round),add_stars)

      if(!is.null(triangle)) {
        if (triangle == "lower" & c > r) {
          f_matrix[r, c] = ""
        }

        if (triangle == "upper" & r > c) {
          f_matrix[r, c] = ""
        }
      }


      }

    }
  }

  if(remove_lead) f_matrix[] <- gsub("0\\.",".",f_matrix) #remove leading zeros if requested

if(!is.null(triangle)){

  nums = seq_along(rownames(f_matrix))
  rownames(f_matrix) = paste0(nums,". ", rownames(f_matrix))
  colnames(f_matrix) = nums
  f_matrix = f_matrix[,-length(nums)]
}


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
  temp_note
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
  corrplot::corrplot(x$r, method = "square")
}
