#' @title Calculate total number of records for a search of plos
#' @description Displays the total number of results for a query of PLOS using rplos::searchplos. Useful for establishing the total number of results for a query and creating a value to use in searchplos limit = . See also plos_multi which includes plos_record and then retrieves the records. 
#' @param q a single search terms or multiple search terms. 
#'
#' @return numeric
#' @export
#' @importFrom plyr ldply
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom rplos searchplos
#' @examples \dontrun{s <- c('"synthetic biology"', '"synthetic genomics"', '"synthetic genome"', '"synthetic genomes"')}
#' \dontrun{r <- plos_records(s)}
plos_records <- function(q) {
  library(plyr) #for ldply
  library(dplyr) #for pipes, select and filter
  library(rplos)
  lapply(q, function(x) searchplos(x, limit = 0)) %>%
    ldply("[[", 1) %>% #get meta from the lists
    select(numFound) %>% #select numFound column of meta
    filter(numFound == max(numFound)) %>% #filter on max numFound
    print() #print max value of numFound
}
