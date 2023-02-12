#' @title Add Stanford _ST tags
#'
#' @description Adds Stanford (_ST) part of speech tags to a character vector of text.
#' @details This function adds Stanford Tags _ST to a vector of text. The function works by tokenizing the text first,
#' and then annotating the tokenized text using \code{udpipe::udpipe_annotate}.
#' Requires a udpipe_model to be present, so if an error occurs, try running \code{init_udpipe_model()} first.
#' @param x A character vector containing the text to be tagged.
#' @param mdl A model object of class \code{udpipe_model}.
#' @param st_hesitation Logical. Experimental feature - should hesitation markers be excluded before tagging? Set to TRUE to exclude.
#' Regex for the hesitation markers are the same as the default for \code{dtag_hesitation}, but can be set using the regex argument.
#' @param tokenized Logical. Set to TRUE in case text has already been tokenized.
#' @return A character vector containing the tagged text.
#'
#' @export
#'
#' @importFrom  udpipe udpipe_annotate


add_st_tags <- function(x, mdl = udmodel, st_hesitation = FALSE, tokenized = FALSE, ...){
  # check to see if udpipe is loaded, load as required
   stopifnot("Udpipe model not loaded. Initialise first with init_udpipe_model()" = exists("udmodel"))


  # correction for missing spaces after commas and full stops

  x <- {{x}} %>%
              str_squish %>%
              str_replace_all("(\\w(\\.|,))(\\w)", "\\1 \\2") %>%
              str_split("\\s|(?=[?!,.])") %>%
              map(~str_subset(.x, ".+")) %>%
              pluck(1)
 # # tag and extract st_hesitation markers
if(st_hesitation){

 st_hesitations_extracted <-
   dtag_hesitation(x,...) %>%
    enframe() %>%
    group_split(st_hesitation = str_detect(value, "HSTN")) %>%
    map(~select(.x, -st_hesitation))

x <- st_hesitations_extracted[[1]] %>% deframe
nms <- st_hesitations_extracted[[1]] %>% pull(name)

if(length(st_hesitations_extracted) < 2){st_hesitations_extracted[2] <- list(c())}
}

if(tokenized){
  x <- str_flatten(x, " ") %>%
    map_chr(~str_replace_all(.x, "\\s([.,;:!?])", "\\1"))
}

  st_tagged <- udpipe::udpipe_annotate({{mdl}}, x, parser = "none") %>%
    as_tibble() %>%
    transmute(tagged = str_c(token,xpos,sep = "_")) %>%
    pull(tagged)


  if(st_hesitation){
   st_tagged<- st_tagged %>%
     enframe() %>%
     mutate(name = nms) %>% # replace name with original index position
   bind_rows(st_hesitations_extracted[[2]]) %>%
   arrange(name) %>%
   pull(value) }

    return(st_tagged)
  }
