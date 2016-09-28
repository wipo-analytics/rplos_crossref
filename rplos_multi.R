#work in progress function
plos_multi <- function(q) {
  library(plyr) #for ldply
  library(dplyr) #for pipes, select and filter
  library(rplos)
  res <- lapply(q, function(x) searchplos(x, limit = 0)) %>%
    ldply("[[", 1) %>% #get meta from the lists
    select(numFound) %>% #select numFound column of meta
    filter(numFound == max(numFound)) %>% #filter on max numFound
    print() #print max value of numFound
  df <- lapply(q, function(x) searchplos(x, fl = c('id', 'author', 'publication_date', 'title', 'abstract'), limit = res)) %>%
    setNames(s) %>% 
    ldply("[[", 2)
}
