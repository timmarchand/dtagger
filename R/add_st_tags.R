#' Add ST Tags to Text
#'
#' The `add_st_tags` function is designed to process and annotate text using the Universal Dependencies
#' (UD) model with the udpipe package. It allows users to tokenize and tag text with part-of-speech (Stanford) tags,
#' and to extract and handle hesitation markers. The function provides options for controlling the parsing,
#' tokenizer type, and handling of flattened input.
#'
#' @param x A character vector of input text to be processed.
#' @param mdl A udpipe model to use for processing the text. The default is the udmodel.
#' @param st_hesitation A logical value indicating whether or not to extract hesitation markers
#' from the input text. If `TRUE`, the function will extract hesitation markers and return them separately.
#' Default is `FALSE`.
#' @param flattened A logical value indicating if the input text is flattened. If `FALSE`, i.e. if the character string is in
#' tokenized form, the function will flatten the text before processing. Default is `TRUE`.

#' @param skip_parse A logical value determining if the function should skip parsing
#' and only return tokenized and tagged text. If `FALSE`, the function returns the
#' full UD model when parsing. Default is `TRUE`.
#' @param ... Additional arguments to be passed to the `udpipe_annotate()` function.
#' For example:
#'
#' `tokenizer = "horizontal"` to force the `udpipe_annotate` function to tokenize on tokens
#'  separated by white spaces. This will combine words and trailing punctuation marks, unless
#'  they have been separated by white space previously.
#'
#' `tokenizer = "vertical"` to force the `udpipe_annotate` function to tokenize on tokens separated by
#' new line breaks.  This can be useful if you want the tokenizer to recognise multi-word
#' entities as a single token, or avoid separating hyphenated words etc.
#'
#' @return If `skip_parse` is `FALSE`, the function returns a tibble with the full udpipe model when parsing.
#' If `st_hesitation` is `TRUE` (experimental), the function returns a character vector of tokenized and tagged
#' text with hesitation markers extracted and handled separately.
#' Otherwise, the function returns a character vector of tokenized and tagged text.
#'
#' @export
#'
#'
#' @importFrom  udpipe udpipe_annotate
#' @examples
#' \dontrun{
#' # Example text:
#' text <- "This is an example sentence to be tagged"
#' # Example speech, tokenized:
#' speech <- c("I","don't", "know" ,  "erm" ,",", "whether" , "to" ,
#' "include" ,"hesitation" , "markers", ".")
#' # Initiate udpipe model
#' init_udpipe_model()
#' # Tag text
#' add_st_tags(text)
#' # Tag speech
#' add_st_tags(speech, st_hesitation = TRUE, tokenized = TRUE)
#' text <- "I'm in a part-time job, at the moment."
#' text2 <- "I'm\nin\na\npart-time\njob\n,\n\nat the moment\n.\n"
#' # tokenizes using default model - may separate some hyphenated words
#' add_st_tags(text)
#' # tokenizes on whitespaces - punctuation marks can be lumped in with words
#' add_st_tags(text, tokenizer = "horizontal")
#' # tokenizes on user-defined line breaks - possible to capture multi-word expressions
#'  add_st_tags(text2, tokenizer = "vertical") }

add_st_tags <- function(x,
                        mdl = udmodel,
                        st_hesitation = FALSE,
                        flattened = TRUE,
                        skip_parse = TRUE,
                        ...){

    if(!flattened){x <- d_flatten_text(x)}

  # check to see if udpipe is loaded, load as required
   stopifnot("Udpipe model not loaded. Initialise first with init_udpipe_model()" = exists("udmodel"))

   ## return full udpipe model when parsing
  if(!skip_parse) {full_tbl <- udpipe::udpipe_annotate({{mdl}}, x, tagger = "default",
                                       parser = "default",
                                       ...) %>%
    as_tibble() %>%
    mutate(st = str_c(token,xpos,sep = "_"), .after = xpos)

  return(full_tbl)}

 # tag and extract st_hesitation markers
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



  st_tagged <- udpipe::udpipe_annotate({{mdl}}, x, tagger = "default",
                                       parser = "none",
                                       ...) %>%
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
