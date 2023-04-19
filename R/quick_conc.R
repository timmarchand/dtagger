#' @title Quick concordancing of pattern matches or index locations
#' @description Lightweight concordancing function to return key words in context (KWIC) in a tidy format.
#' @param x a character vector of tokenized strings, or a single string
#' @param index a character vector of regex pattern to match, or a numeric vector to use as index of matches
#' @param n an integer, to specify the number of context tokens either side of the matched node
#' @param tokenize a logical, to tokenize the text first or not
#' @param separated a logical, to separate the context tokens or not
#' @return A tibble containing:
#' * case - a case number for the match found.
#' * left - objects immediately adjacent (up to n) to the left of the matched node.
#' In case of `separated = TRUE`, the left  are separated into left(n):left1
#' * match - the matched search item, as defined by the `index` argument.
#' * right - tokens immediately adjacent (up to n) to the right of the matched node.
#' In case of `separated = TRUE`, the right tokens are separated into right1:right(n).
#' * index - the index row position of matched result from the input data frame.
#' @rawNamespace import(data.table, except = c(first,last,between, transpose))
#' @importFrom data.table data.table shift
#' @importFrom tidyselect all_of
#' @import tibble
#' @import dplyr
#' @export
#'
#' @examples
#' x <- c("The", "cat", "sat", "on", "the", "mat")
#' index <- c("cat", "sat")
#' quick_conc(x, index, n = 2)
#' x <- "The dog barked loudly, alerting the neighbors of potential danger.
#' A nearby park seemed like the perfect spot for the dog and
#' it quickly made its way there."
#' quick_conc(x, index = "dog", n = 3, tokenize = TRUE, separated = TRUE)
#' quick_conc(x, index = c(4,8,12), tokenize = TRUE)
#'
quick_conc <- function(x, index, n = 5, tokenize = FALSE, separated = FALSE){
      if(tokenize){x <-  base::unlist(base::strsplit(x, "\\s|(?=[?!,.])", perl = TRUE))}
       DT <- data.table::data.table(x)
        data.table::setnames(DT, "x", "match")
        DT[, token_id := .I]


          cols_left <- base::paste0("left",n:1)
          cols_right <- base::paste0("right",1:n)

          for(i in n:1){
            DT[, base::paste0("left",i) := data.table::shift(match, n = i, type = "lag")]
          }

          for(i in 1:n){
            DT[, base::paste0("right",i) := data.table::shift(match, n = i, type = "lead")]
          }

            if(is.character(index)) {
            DT[match %like% base::paste(index, collapse = "|")][,
            case := .I] -> result
        }

            if(is.numeric(index)) {
            DT[index][,case := .I] -> result}

          if(separated){result <- tibble::as_tibble(result) %>%
            dplyr::select(case, token_id, tidyselect::all_of(cols_left),match,tidyselect::all_of(cols_right))
          return(result)}

          result <- result[,left := base::do.call(paste, c(.SD, sep = " ")), .SDcols = cols_left][,
           right := base::do.call(paste, c(.SD, sep = " ")), .SDcols = cols_right][,
           list(case,token_id,left,match,right)] %>%
          tibble::as_tibble()

        return(result)
  }
