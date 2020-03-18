jenks_natural <- function(data, var, breaks,round_test){
  
  # data <- copy(iris)
  # var <- "Petal.Length"
  # breaks <- 5
  
  # conver df to data.table  
  setDT(data)
  
  # name of new column
  newvar <- paste0(var,"_jenks")
  
  # calculate jenks natural breaks
  data[, paste0(newvar) := as.character(cut(get(var), breaks= getJenksBreaks(get(var), breaks), include.lowest = TRUE, dig.lab=3)) ]
  
  # Edit factor text
  data[, paste0(newvar) := str_replace_all(get(newvar), "\\[|\\(|\\]", "") ]
  data[, paste0(newvar) := stri_replace_all_regex(get(newvar), "[,]", " - ") ]
  
  # round
  if(round_test == TRUE){
    
    data[, paste0(newvar) := paste(head(str_split(get(newvar)," - ") %>% unlist() %>% as.numeric() %>% round(2))[1],
                                   "-",
                                   head(str_split(get(newvar)," - ") %>% unlist() %>% as.numeric() %>% round(2))[2]),by=id_hex]
    }
  
  # get factor labels
  jenks_labels  <- data[, get(newvar)]  %>% table %>% names() %>% sort(decreasing = F) 
  
  # recode variable
  data[, paste0(newvar) := factor(get(newvar), levels = jenks_labels)]
  
  return(data)
}
