#' dtag_folder
#'
#' This function takes in a path and an optional argument for the number of texts to be analyzed. It then reads the text files in the folder, adds stylistic and multidimensional analysis tags, and returns a list of dataframes containing the tagged texts, individual and corpus-level scores for each dimension of the text, and word counts.
#'
#' @param path A character string denoting the folder containing the text files.
#' @param n An optional argument denoting the maximum number of text files to be analyzed.
#' @return A list of dataframes containing the tagged texts, individual and corpus-level scores for each dimension of the text, and word counts.
#' @importFrom fs dir_info
#' @importFrom readtext readtext
#' @export
#'
#' @examples
#' \dontrun{
#' dtag_folder("path_to_folder")}
dtag_folder <- function(path, n = NULL){
negative_tags <- c("NN", "AWL", "PIN", "TTR", "JJ", "TIME", "PLACE", "RB")

ALL_corpora <-  path %>%
  fs::dir_info(recurse = TRUE) %>%
  # target only folders with $$
  filter(str_detect(path, "\\$\\$")) %>%
  # pull the path column
  pull(path) %>%
  map_df(~readtext::readtext(.x, docvarsfrom = "filepaths", dvsep = "\\$\\$")) %>%
  # create corpus var from filepath and add wc (word count)
  transmute(corpus = str_extract(docvar2, "(?<=texts/)[^/]+"),
            doc_id = str_remove(doc_id, "\\.txt$"),
            text)

if(!is.null(n)){ALL_corpora <- ALL_corpora[1:n,]}

ALL <- ALL_corpora %>%
  mutate(st_tags = map(text, ~add_st_tags(.x, hesitation =FALSE), .progress = "(1/4) Tagging ST tags")) %>%
  mutate(mda_tags = map(st_tags, ~add_mda_tags(.x, hesitation = TRUE), .progress = "(2/4) Tagging MDA tags")) %>%
  log_midpipe(message("(3/4) Compiling the tagged text")) %>%
  mutate(tagged_text = map(mda_tags, ~str_flatten(.x, " ")) %>%
                        map_chr(~str_replace_all(.x, "\\s([.,;:!?])" , "\\1"))) %>%
  mutate(wordcount = map_int(tagged_text, ~str_count(.x, "[A-Za-z']+_")))

## get awl ttr scores for each doc_id
awl_ttr_output <- ALL %>% pluck("st_tags") %>%
 map( ~ add_awl_ttr(.x)) %>%
        set_names(str_c(ALL$corpus, ALL$doc_id, sep = "&&&"))

awl_ttr <- awl_ttr_output %>%
  map(~pivot_longer(.x, cols = everything(), names_to = "feature", names_transform = ~str_replace(.x, "(.+)", "<\\1>"), values_to = "value")) %>%
  map_df(~as_data_frame(.x), .id = "doc_id" ) %>%
  separate(doc_id, into = c("corpus", "doc_id"), sep = "&&&")


ALL_Dscores <- map_df(mda_tags, ~ALL %>%
         mutate(
                count = str_count(tagged_text, .x),
                value = (count * 100) / wordcount,
                feature = .x), .progress = "(4/4) Counting tags") %>%
  tibble()  %>%
select(corpus,doc_id,feature,value) %>%
  arrange(doc_id, feature) %>%
  bind_rows(awl_ttr) %>%
  left_join(biber_base) %>%
  mutate(zscore = ((value - biber_mean) / biber_sd)) %>%
  mutate(dscore = case_when(feature %in% negative_tags ~ -zscore,
                            TRUE ~ zscore)) %>%
  select(corpus, doc_id, dimension, feature, detail, value, zscore,dscore, biber_mean, biber_sd) %>%
  arrange(corpus, doc_id, dimension, feature)

document_dimension_scores <- ALL_Dscores %>%
  summarise(dimension_score = sum(dscore, na.rm = TRUE), .by = c(corpus, doc_id, dimension)) %>%
    arrange(corpus, doc_id, dimension) %>%
  pivot_wider(names_from = "dimension", values_from = "dimension_score") %>%
  select(-Other)

corpus_dimension_scores <- document_dimension_scores %>% summarise(across(where(is.numeric), mean), .by = corpus)



l <- list( corpus_dimension_scores = corpus_dimension_scores,
           document_dimension_scores =  document_dimension_scores,
           dimension_tags = ALL_Dscores,
           tokenized_tags = ALL %>%  unnest(cols = c("st_tags", "mda_tags")) %>% select(corpus, doc_id, st_tags, mda_tags) %>% arrange(corpus, doc_id),
           texts = ALL %>% tibble() %>%  select(corpus, doc_id, raw_text = text, tagged_text, wordcount) %>% arrange(corpus, doc_id))

return(l)
}
