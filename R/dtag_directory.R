#' dtag_directory
#' @description Tag a directory of folders of plain text files with <MDA> tags
#' @details The target texts to be tagged should be placed in a directory of folders
#' with $$ prefixed on the folder names. The function will then read in any text
#' files from the target folders, and retrieve the folder names as the "corpus" variable.
#'
#' If the texts have already been tagged with Stanford _ST tags, choose the option \code{ST = TRUE}.
#'
#' Otherwise, the function add_st_tags() will run over the texts,
#' for which it is necessary to have a udpipe model loaded. See \code{\link{add_st_tags}} for details.
#'
#' The function then adds multidimensional analysis <MDA> tags, and calculates Dimension scores
#' based on the Biber 1988 standard. Note that some of the tags from the original study can be excluded
#' from the analysis, with the `exclude` argument.
#'
#' If the argument \code{deflated = TRUE}, the function also returns Dimension scores
#'  calculated without using the low mean frequency features from Biber's original study,
#' following the MAT tagger algorithm (Nini 2019).
#'
#' The function returns a list of tibbles including the tagged texts, individual and
#' corpus-level scores for each dimension of the text and word counts. If the function detects
#' more than one corpus folder (folders prefixed with $$), it will also return the result of
#' post-hoc significance tests. This is a set of confidence intervals
#' on the differences between the means of the dimension scores based on the Studentized range statistic,
#' Tukey's ‘Honest Significant Difference’ method.
#'
#' @param path A character string denoting the folder containing the target folders (at any level).
#' @param n An optional argument denoting the maximum number of text files to be analyzed.
#' @param ST Logical argument denoting whether the text files have _ST tags included already.
#' @param deflated Logical argument. If TRUE (default), in addition to the normal results, the function
#' returns the dimension scores with "deflated" results,
#' which means rare features from Biber's original study (mean freq < 0.1) are removed from the Dimension
#' score calculations.
#' @param exclude A character vector of dimension tags you don't want to include in the analysis.
#' Note that the dimension tags should be quoted inside angular brackets. For example:
#'
#' (1) If some of the word counts in the texts are below 400, you may want to exclude
#' type token ratios from the analysis with `exclude = "<TTR>"`.
#'
#' (2) Some of the tags (such as <WZPRES> and <GER>)  were manually checked in the original Biber study, but
#' are automatically tagged here.  You can exclude some of these tags by naming them in the exclude character
#' vector such as `exclude = c("<GER>", "<WZPRES>")`.
#'
#' (3) To exclude all of tags manually checked by Biber, use the argument `exclude = "<MANUAL>"`
#' This is the same as the argument:
#' `exclude = c("<DEMP>", "<GER>", "<PASTP>", "<PRESP>", "<SERE>", "<THAC>", "<THVC>", "<TOBJ>", "<TSUB>", "<WZPAST>", "<WZPRES>")`
#'
#' (4) You can combine (1) and (3) with `exclude = c("<MANUAL>" , "<TTR>")`
#'
#'
#' @param ... Additional arguments to be passed on.
#' @return A list of data frames containing:
#'
#' \bold{corpus_dimension_scores}
#' * corpus - name of corpus folder
#' * corpus_text_type - closest text type for average corpus dimensions
#' * most_common_text_type - the mode of the closest text type for the documents within the corpus folder
#' * Dimension scores - calculated scores for Dimension1 ~ Dimension6
#'
#' \bold{document_dimension_scores}
#' * corpus - name of corpus folder
#' * doc_id - name of text file
#' * Dimension scores - calculated scores for Dimension1 ~ Dimension6
#' * closest_text_type - closest matching text type for each doc_id, based on Biber 1989
#' * dimension_tags
#' * dimension - Dimension1 ~ Dimension6 from Biber 1988 for each feature
#' * feature - the <MDA> tag or AWL or TTR
#' * detail - brief description of the feature
#' * count - number of times the feature is counted in text
#' * value - in case of <MDA> tag, normailsed frequency per 100 tokens
#' * z-score - value scaled to the biber_mean and biber_sd
#' * d-score - same as z-score, but with the sign of negative dimension features reversed
#' * biber_mean and biber_sd for each feature, based on Biber 1988
#'
#' \bold{tokenized_tags}
#' * corpus - name of corpus folder
#' * doc_id - name of text file
#' * st - text tokenized on each _ST tag
#' * mda - text tokenized on each <MDA> tag
#'
#' \bold{texts}
#' * corpus - name of corpus folder
#' * doc_id - name of text file
#' * raw_text - untagged, flattened text for each doc_id
#' * tagged_text - flattened text with _ST and <MDA> tags for each doc_id
#' * wordcount - number of non-punctuation tokens found in text
#'
#'  \bold{Tukey_hsd}
#' * dimension - the dimension for pairwise comparison
#' * contrast - the corpora under pairwise comparison
#' * null.value - the expected difference in means after aov (zero)
#' * estimate - the difference in means after aov
#' * conf.low - the 95% familywise lower confidence level
#' * conf.high - the 95% familywise upper confidence level
#' * p.value -  significance test
#' @importFrom fs dir_info
#' @importFrom readtext readtext
#' @importFrom broom tidy
#' @importFrom tidyr pivot_longer
#' @export
#' @references
#'  1. Biber, D. (1988). Variation across Speech and Writing. Cambridge: Cambridge University Press. doi:10.1017/CBO9780511621024
#'  2. Biber, D. (1989). A typology of English texts. , 27(1), 3-44. https://doi.org/10.1515/ling.1989.27.1.3
#'  3. Nini, A. (2019). The Multi-Dimensional Analysis Tagger. In Berber Sardinha, T. &  Veirano Pinto M. (eds), Multi-Dimensional Analysis: Research Methods and Current  Issues, 67-94, London; New York: Bloomsbury Academic.
#' @examples
#' \dontrun{
#' dtag_directory("path_to_directory")}
dtag_directory <- function(path, n = NULL, ST = FALSE, deflated = TRUE, exclude = NULL, ...){


tags_to_count <- c("<AMP>", "<ANDC>", "<BEMA>", "<CAUS>", "<CONT>", "<DEMP>",
"<DPAR>", "<EMPH>", "<FPP1>", "<HDG>", "<INPR>", "<JJ>", "<NN>",
"<PIN>", "<PIT>", "<POMD>", "<PRIV>", "<PROD>", "<SERE>", "<SPP2>",
"<STPR>", "<THATD>", "<VPRT>", "<WHCL>", "<WHQU>", "<XX0>",
"<PEAS>", "<PRESP>", "<PUBV>", "<SYNE>", "<TPP3>", "<VBD>", "<NOMZ>",
"<PHC>", "<PIRE>", "<PLACE>", "<RB>", "<TIME>", "<WHOBJ>", "<WHSUB>",
"<COND>", "<NEMD>", "<PRMD>", "<SPAU>", "<SUAV>", "<BYPA>",
"<CONJ>", "<OSUB>", "<PASS>", "<PASTP>", "<WZPAST>", "<DEMO>",
"<THAC>", "<THVC>", "<TOBJ>", "<CONC>", "<DWNT>", "<EX>", "<GER>",
"<HSTN>", "<PRED>", "<QUAN>", "<QUPR>", "<SMP>", "<SPIN>", "<TO>",
"<TSUB>", "<VBN>", "<WZPRES>")

exclude <- {{exclude}}
if(any(str_detect(exclude, "<MANUAL>"))){ exclude <- c(exclude, "<DEMP>", "<GER>", "<PASTP>",
                                                  "<PRESP>", "<SERE>", "<THAC>", "<THVC>",
                                                  "<TOBJ>", "<TSUB>", "<WZPAST>", "<WZPRES>")}

# negative_tags <- c("<NN>", "<AWL>", "<PIN>", "<TTR>",
#                    "<JJ>", "<TIME>", "<PLACE>", "<RB>")

ALL_corpora <-
  path %>%
  # target only folders with $$ prefix
  fs::dir_info(recurse = TRUE, regexp = "\\$\\$.*txt") %>%
  # pull the path column
  pull(path) %>%
  # use $$ prefix as separator to grab subfolder (corpus) name
  map_df(~readtext::readtext(.x,
                             docvarsfrom = "filepaths",
                             dvsep = "\\$\\$")) %>%
  # extarct corpus var from filepath
  transmute(corpus = str_extract(docvar2, "^[^/]+"),
            doc_id = str_remove(doc_id, "\\.txt$"),
            text)

if(!is.null(n)){ALL_corpora <- ALL_corpora[1:n,]}
if(ST == TRUE){ALL <- ALL_corpora %>%
  mutate(st = map(text, ~base::unlist(base::strsplit(.x, "\\s", perl = TRUE))))}

if(ST == FALSE){
ALL <- ALL_corpora %>%
  mutate(st = map(text,
                       ~add_st_tags(.x, st_hesitation = FALSE),
                       .progress = "(1/4) Tagging ST tags")) }

ALL <- ALL %>%
  mutate(mda = map(st,
                        ~add_mda_tags(.x, mda_hesitation = TRUE),
                        .progress = "(2/4) Tagging MDA tags")) %>%
  log_midpipe(message("(3/4) Compiling the tagged text")) %>%
  mutate(tagged_text = map(mda, ~str_flatten(.x, " ")) %>%
                        map_chr(~str_replace_all(.x, "\\s([.,;:!?])" , "\\1"))) %>%
  mutate(wordcount = map_int(tagged_text, ~str_count(.x, "[A-Za-z']+_"))) %>%
  tibble()

## get awl ttr scores for each doc_id
awl_ttr <- ALL %>%
          pluck("st") %>%
          map( ~ add_awl_ttr(.x)) %>%
          set_names(str_c(ALL$corpus, ALL$doc_id, sep = "&&&")) %>%
          map(~pivot_longer(.x, cols = everything(),
                            names_to = "feature",
                            names_transform = ~str_replace(.x, "(.+)", "<\\1>"),
                            values_to = "value")) %>%
        map_df(~as_tibble(.x), .id = "doc_id" ) %>%
        separate(doc_id, into = c("corpus", "doc_id"), sep = "&&&")


ALL_Dscores <- map_df(tags_to_count, ~ALL %>%
            mutate(
                count = str_count(tagged_text, .x),
                value = (count * 100) / wordcount,
                feature = .x), .progress = "(4/4) Counting tags") %>%
            tibble()  %>%
            select(corpus,doc_id,feature,count,value) %>%
            arrange(doc_id, feature) %>%
            bind_rows(awl_ttr) %>%
            filter(!feature %in% exclude) %>%
            left_join(biber_base) %>%
            mutate(zscore = ((value - biber_mean) / biber_sd)) %>%
            mutate(dscore = if_else(loading == "negative",  -zscore, zscore)) %>%
            select(corpus, doc_id, dimension, feature, detail, count,value, zscore,dscore, biber_mean, biber_sd) %>%
            arrange(corpus, doc_id, dimension, feature)

document_dimension_scores <- ALL_Dscores %>%
  summarise(dimension_score = sum(dscore, na.rm = TRUE),
            .by = c(corpus, doc_id, dimension)) %>%
            arrange(corpus, doc_id, dimension) %>%
            pivot_wider(names_from = "dimension", values_from = "dimension_score") %>%
            select(-Other)


document_dimension_scores_deflated <- ALL_Dscores %>%
    filter(biber_mean >= 0.1) %>%
  summarise(dimension_score = sum(dscore, na.rm = TRUE),
            .by = c(corpus, doc_id, dimension)) %>%
            arrange(corpus, doc_id, dimension) %>%
            pivot_wider(names_from = "dimension", values_from = "dimension_score") %>%
            select(-Other)

Tukey_hsd <- Tukey_hsd_deflated <-  NULL
if(document_dimension_scores %>%
   distinct(corpus) %>%
   nrow > 1){

  Tukey_hsd <- document_dimension_scores %>%
   pivot_longer(contains("Dimension"), names_to = "dimension",
               names_transform = list(dimension = as.factor)) %>%
group_by(dimension) %>%
  nest() %>%
  mutate(model = map(data, ~lm(value ~ corpus, .x))) %>%
  summarise(aov = map(model, ~stats::aov(.x)),
         Tukey = map(aov, ~ stats::TukeyHSD(.x))) %>%
    mutate(Tukey = map(Tukey, ~broom::tidy(.x))) %>%
    select(dimension, Tukey) %>%
  unnest(Tukey) %>%
    select(-term)


Tukey_hsd_deflated <- document_dimension_scores_deflated %>%
   pivot_longer(contains("Dimension"), names_to = "dimension",
               names_transform = list(dimension = as.factor)) %>%
group_by(dimension) %>%
  nest() %>%
  mutate(model = map(data, ~lm(value ~ corpus, .x))) %>%
  summarise(aov = map(model, ~stats::aov(.x)),
         Tukey = map(aov, ~ stats::TukeyHSD(.x))) %>%
    mutate(Tukey = map(Tukey, ~broom::tidy(.x))) %>%
    select(dimension, Tukey) %>%
  unnest(Tukey)%>%
    select(-term)
}

document_dimension_scores <- list(document_dimension_scores) %>%
                              add_closest_text_type()


document_dimension_scores_deflated <- list(document_dimension_scores_deflated) %>%
                              add_closest_text_type()


 corpus_dimension_scores <- document_dimension_scores %>%
                                map(~.x %>% summarise(most_common_text_type = find_mode(closest_text_type),
                                across(where(is.numeric), mean), .by = corpus)) %>%
                                add_closest_text_type(by = "corpus") %>%
                                  map(~.x %>%
                                  relocate(corpus_text_type = closest_text_type, .after = corpus))

corpus_dimension_scores_deflated <- document_dimension_scores_deflated %>%
                                map(~.x %>% summarise(most_common_text_type = find_mode(closest_text_type),
                                across(where(is.numeric), mean), .by = corpus)) %>%
                                add_closest_text_type(by = "corpus") %>%
                                  map(~.x %>%
                                  relocate(corpus_text_type = closest_text_type, .after = corpus))


l <- list( corpus_dimension_scores = pluck(corpus_dimension_scores, 1),
           corpus_dimension_scores_deflated = pluck(corpus_dimension_scores_deflated, 1),
           document_dimension_scores =  pluck(document_dimension_scores,1),
           document_dimension_scores_deflated =  pluck(document_dimension_scores_deflated,1),
           dimension_tags = ALL_Dscores,
           tokenized_tags = ALL %>%
                            unnest(cols = c("st", "mda")) %>%
                            select(corpus, doc_id, st, mda) %>%
                            arrange(corpus, doc_id),
           texts = ALL %>%
             select(corpus, doc_id, raw_text = text, tagged_text, wordcount) %>%
             arrange(corpus, doc_id),
           Tukey_hsd = Tukey_hsd,
           Tukey_hsd_deflated = Tukey_hsd_deflated)

if(!deflated){l$corpus_dimension_scores_deflated <- NULL
                      l$document_dimension_scores_deflated <- NULL
                      l$Tukey_hsd_deflated  <- NULL}


return(l)
}
