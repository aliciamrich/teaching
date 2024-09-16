
fix.strings <-  function(df) {
  df <- df %>%
    mutate(across(where(is.character), ~str_remove_all(.x, fixed("'")))) %>%
    mutate(across(where(is.character), ~str_remove_all(.x, fixed("[")))) %>%
    mutate(across(where(is.character), ~str_remove_all(.x, fixed("]")))) %>%
    mutate(across(where(is.character), ~str_trim(.x, "both"))) %>%
    mutate(across(where(is.character), ~str_squish(.x)))
  return(df)
}

export.list <- function(df, filename) {
  write.table(df,
              paste0(params$local, "/dataframes/", filename, ".txt"),
              sep       = "\t",
              quote     = FALSE,
              row.names = FALSE,
              col.names = FALSE)
}


backup.df  <- function(df, filename) {
  write.table(df,
              paste0(params$local, "/dataframes/", filename, "_", Sys.Date(), ".tsv"),
              sep       = "\t",
              quote     = FALSE,
              row.names = FALSE)
}


since.start <- function(date.col, units) {
  as.numeric(as.period(interval(ymd(params$day1), date.col), unit = units), units)
}


check.duplicates <- function(df2, df, group) {
  df2 <- df %>%
    group_by(group) %>%
    filter(n() > 1) %>%
    ungroup()
}

read.tables <- function(file) {
  data <- read.table(file, 
                     header = TRUE, 
                     sep = "\t", 
                     stringsAsFactors = FALSE) %>%
    tibble()
  return(data)
}

read.summaries <- function(file) {
  
  data <- readr::read_lines(file) %>%
    tibble() %>%
    separate(everything(), into = c("key", "value"), sep = "=", fill = "right", extra = "merge") %>%
    pivot_wider(names_from = "key", values_from = "value")
  return(data)
}

scanBams <- function(file){
  data <- scanBam(file)
  return(data)
}

read.recent.version.csv <- function(directory, pattern) {
  files             <- list.files(path       = paste0(params$local, "/", directory, "/"), 
                                  pattern    = paste0(pattern, "\\d{4}-\\d{1,2}-\\d{1,2}\\.csv"), 
                                  full.names = TRUE)
  dates             <- gsub(".*_(\\d{4}-\\d{1,2}-\\d{1,2})\\.csv", "\\1", files)
  dates             <- as.Date(dates, format = "%Y-%m-%d")
  most_recent_index <- which.max(dates)
  most_recent_file  <- files[most_recent_index]
  data              <- read.csv(most_recent_file, header = TRUE)
  
  return(data)
}


read.recent.version.tsv <- function(directory, pattern) {
  files             <- list.files(path       = paste0(params$local, "/", directory, "/"), 
                                  pattern    = paste0(pattern, "\\d{4}-\\d{1,2}-\\d{1,2}\\.tsv"), 
                                  full.names = TRUE)
  dates             <- gsub(".*_(\\d{4}-\\d{1,2}-\\d{1,2})\\.tsv", "\\1", files)
  dates             <- as.Date(dates, format = "%Y-%m-%d")
  most_recent_index <- which.max(dates)
  most_recent_file  <- files[most_recent_index]
  data              <- read.table(most_recent_file, sep = "\t", header = T)
  
  return(data)
}

open.job <- function(name, mem, hrs, cpus) {
  chunk   <- paste("paste to cluster shell:\n\nsrun --partition=guest --nodes=1 --ntasks-per-node=1",
                   paste0(" --job-name=", 
                          name, 
                          " --mem=", 
                          mem, 
                          "GB --time=", 
                          hrs, 
                          ":00:00 --cpus-per-task=", 
                          cpus),
                   "--pty $SHELL\n\n")
  
  if(knitr::is_html_output()) {
    output <- knit_print(asis_output(paste("<div class='swan-chunk'>",  chunk,  "</div>") ) )
    
  } else {
    
    output <- chunk
  }
  
  return(output)
}

load.pkg <- function(pkg) {
  chunk <- paste("paste to cluster shell:\n\ncd", params$work_dir, "\nmodule load", pkg, "\n\n")
  
  if(knitr::is_html_output()) {
    output <- knit_print(asis_output(paste("<div class='swan-chunk'>",  chunk,  "</div>") ) )
    
    
  } else {
    output <- chunk
  }
  
  return(output)
}
