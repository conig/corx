
#' partial_matrix
#'
#' Creates matrices of partial correlations including r, n, and p
#' @param data the data object
#' @param x rownames
#' @param y colnames
#' @param method the method
#' @param partial variables to partial out

partial_matrix <- function(data, x, y, method, partial){
call = match.call() # always nice to save the call
  all_vars = unique(c(x,y)) # grab all unique vars
  combos = t(utils::combn(all_vars,2)) # get all combos
  duplis = cbind(all_vars, all_vars) # add in the duplicates

  results = data.frame(rbind(combos, duplis), stringsAsFactors = F)
  names(results) = c("Var1","Var2")

  cors = lapply(seq_along(results$Var1), function(r) { # calc results
    #message(r)
    get_cor(data, # get partial correlation for each unique combination
            x = results$Var1[r],
            y = results$Var2[r],
            method = method,
            partial = partial)
  })

  cors = do.call(rbind, cors) #combine into data.frame

  results = data.frame(cbind(results, cors)) # add to table

  all_results = par_matrix(results,x,y)

  r_matrix = all_results$r # extract matrices
  p_matrix = all_results$p
  n_matrix = all_results$n

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

  partial_data = stats::na.omit(data[, c(x, y, partial)])
  n = nrow(partial_data)

  if (x != y) {

      result = ppcor::pcor.test(partial_data[, x], partial_data[,
                                                                y], partial_data[, partial], method = method)
    r = result$estimate
    p = result$p.value
  } else{
    r = 1
    p = 0
  }


  return(data.frame(r = r, p = p, n = n))
}

#' par_matrix
#'
#' This function is used to construct final matrices
#' @param results results dataset
#' @param x one set of variables
#' @param y another set of variables

par_matrix = function(results, x, y){
  m = matrix(nrow = length(x), ncol = length(y)) # create resultant matrix[x,y]
  rownames(m) = x # x is rows
  colnames(m) = y # y is cols

  r_mat = m # create empty matrices ready to recieve results
  p_mat = m
  n_mat = m

  contains_var = function(r,c){ # for a supplied index get the row which contains the relevant result
    r_var = rownames(m)[r] # identify the x col
    c_var = colnames(m)[c] # identify the y col

    bool = unlist(lapply(seq_along(results$Var1),function(i){ # find the row which stores the relevant result
      Var1_in =  all(c(results$Var1[i], results$Var2[i]) %in% c(r_var,c_var) &
                       c(r_var,c_var) %in% c(results$Var1[i], results$Var2[i]))
    }))

    return(bool)

  }

  for(r in seq_along(rownames(m))){ # for every rowname
    for(c in seq_along(colnames(m))){ # and every colname
      #message(r); message(c)
      rows = contains_var(r,c) #find the row index which stores the relevant result

      r_mat[r,c] = results[rows, "r"] # extract the requested parameter.
      p_mat[r,c] = results[rows, "p"]
      n_mat[r,c] = results[rows, "n"]

    }
  }

  return(list(r = r_mat, p = p_mat, n = n_mat))
}
