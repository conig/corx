
#' partial_matrix
#'
#' Creates matricies of partial correlations including r, n, and p
#' @param data the data object
#' @param x horizontal variables
#' @param y veritical variables
#' @param method the methos
#' @param partial variables to partial out

partial_matrix <- function(data, x, y, method, partial){
call = match.call()
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

  return(list(call = call, r = r_matrix, p = p_matrix, n = n_matrix))

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
  m = matrix(nrow = length(x), ncol = length(y))
  rownames(m) = x
  colnames(m) = y

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
