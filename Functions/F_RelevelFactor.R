RelevelFactor <- function(x, map, Na.group = NA, order = NULL, result = 'new.factor') {
  
  if (!is.factor(x)) {stop('The provided variable is not a factor')}
  
  old.levels <- levels(x)
  matr <- unlist(map)
  
  if (length(matr) != length(levels(x))) {stop('The provided number of levels differs form the factor levels!', call. = FALSE)}
  if (is.null(attributes(matr)$names))  {
    
    warning('The provided levels are matched by position !', call. = FALSE)
    y <- cbind(c(old.levels, NA), c(matr, Na.group))
    colnames(y) <- c('Old.Levels', 'New.Levels')
    levels(x) <- c(matr, Na.group)
    x[is.na(x)] <- Na.group
    
  } else {
    
    if (!setequal(levels(x), old.levels)) {stop('The provided levels do not match the factor levels!', call.= FALSE)}
    
    
    matr <- cbind(names(matr), matr)
    levels(x) <- c(matr[match(levels(x), matr[,1]),2], Na.group)
    x[is.na(x)] <- Na.group
    
    if(!is.null(order)) {x <- factor(x, levels = unique(c(order, Na.group)))}
    
    y <- rbind(matr, c(NA, Na.group))
    colnames(y) <- c('Old.Levels', 'New.Levels')
  }
  
  
  # if (!is.null(order)) {
  #   
  #   # order <- order[!is.na(order)]
  #   # 
  #   # if (any(order %% 1 != 0) | max(order) > length(levels(x))  | length(order) != (length(levels(x)))) {
  #   #   
  #   #   warning(paste('Incorrect order argument! Expecting integer vector of length', length(levels(x))
  #   #                 ,'No ordering has been made!'), call. = FALSE)
  #   #   
  #   # } else {
  #   #   
  #   #   
  #   # }
  #   
  #   x <- factor(as.character(x), levels = levels(order))
  # }
  
  if (result == 'relevel.matrix') 
  { names(y) <- NULL
  
  return(y)
  
  } else {
    return(x)
    
  }
}

# as.character(factor(1:2, levels = c(1:2), labels = c('Az', 'Ti')))
# 
# 
# 
# match(c('Az', 'ti', 'toi'),  c('ti', 'Az', 'toi'))
