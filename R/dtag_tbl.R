#' @title dtag_tbl
#' @description Compute the Biber-style dimension scores of a data frame.
#' @details This function adds multidimensional analysis <MDA> tags to a data frame.
#' The data frame should contain one column to identify input id, and another for the
#' text that has been tagged with _ST tags, and flattened into non-tokenized form.
#'
#' After tagging the text, the function then calculates Dimension scores based on the
#' Biber 1988 standard, and approximates the closest text type as per Biber 1989 text
#' classification.
#'
#' @param tbl A data frame with at least one column as input id  and another for the _ST tagged text.
#' @param input A column for the input id (defaults to 1st position, but can be named as e.g. "colname1").
#' @param text A column for the text (defaults to 2nd position, but can be named as e.g. "colname12").
#' The text should be tagged with _ST tags, and in the flattened, not tokenized form.
#' @param tokenized Logical. The default is FALSE, in which case the function tokenizes the text
#' with \code{str_split(text, " ")}. Set to TRUE if text is already tokenized and in a list column.
#' @param ttr Maximum number of tokens to consider for TTR, defaults to 400.
#' @param deflated Logical. If TRUE (default), Dimension scores
#' are calculated without using the low mean frequency features from Biber's original study,
#' following the MAT tagger algorithm (Nini 2019).
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
#' @return A tibble containing:
#' * wordcount - number of non-punctuation tokens found in text
#' * dimension - Dimension1 ~ Dimension6 from Biber 1988 for each feature
#' * feature - the <MDA> tag or AWL or TTR
#' * detail - brief description of the feature
#' * count - number of times the feature is counted in text
#' * value - in case of <MDA> tag, normalized frequency per 100 tokens
#' * z-score - value scaled to the biber_mean and biber_sd
#' * d-score - same as z-score, but with the sign of negative dimension features reversed
#' * biber_mean and biber_sd for each feature, based on Biber 1988
#' * closest matching text type for each input, based on Biber 1989
#' @importFrom tidyr hoist separate
#' @importFrom tidyr nest
#' @export
#' @references
#'  1. Biber, D. (1988). Variation across Speech and Writing. Cambridge: Cambridge University Press. doi:10.1017/CBO9780511621024
#'  2. Biber, D. (1989). A typology of English texts. , 27(1), 3-44. https://doi.org/10.1515/ling.1989.27.1.3
#'   3. Nini, A. (2019). The Multi-Dimensional Analysis Tagger. In Berber Sardinha, T. &  Veirano Pinto M. (eds), Multi-Dimensional Analysis: Research Methods and Current  Issues, 67-94, London; New York: Bloomsbury Academic.
dtag_tbl <- function(tbl, input = 1, text = 2, tokenized = FALSE, ttr = 400, deflated = TRUE, exclude = NULL){

     stopifnot("The input must be in the form of a data frame, with input id in col1 and text in col2." = is.data.frame(tbl))

input <- pull(tbl,{{input}})
st <- pull(tbl,{{text}})
ttr <- {{ttr}}

     stopifnot("The text doesn't appear to have any _ST tags.\nConsider using the add_st_tags() function on the text column first with:\n
               tbl <-  mutate(tbl, text = map(text, add_st_tags) %>% map_chr(d_flatten))" = str_detect(d_flatten(st), "_\\W|_\\w"))

if(tokenized == FALSE){text <- text %>%
                        str_split("\\s")}



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

result_list <- list()

  for (i in  1:nrow(tbl)){
    text = str_split_1(st[i], " ")
    tagged_text = add_mda_tags(text, mda_hesitation = FALSE) %>% d_flatten()
    words <- tagged_text %>% stringr::str_extract_all("\\w+(?=_)") %>% unlist
    ttr <- pmin(ttr, length(words))

    AWL <- (nchar(words) %>% sum) / length(words)
    TTR <- (words[1:ttr] %>% unique %>% length)*100 / ttr

    awl_ttr <- tibble(input = input[i],
                      st_text = st[i],
                    mda_text = tagged_text,
                    wordcount = length(words),
                    feature = c("<AWL>", "<TTR>"),
                    value = c(AWL,TTR))

    interim <- tibble(input = input[i],
                      st_text = st[i],
                    mda_text = tagged_text,
                    wordcount = length(words))


 result   <-  map_df(tags_to_count, ~ interim %>%
         mutate(
                count = str_count(tagged_text, .x),
                value = (count * 100) / wordcount,
                feature = .x), .progress = "(4/4) Counting tags") %>%
  tibble() %>%
   arrange(input, feature) %>%
   bind_rows(awl_ttr) %>%
     filter(!feature %in% exclude) %>%
  left_join(biber_base, by = "feature") %>%
  mutate(zscore = ((value - biber_mean) / biber_sd)) %>%
           mutate(dscore = if_else(loading == "negative",  -zscore, zscore)) %>%
  select(input, st_text, mda_text, wordcount, dimension, feature, detail, count,value, zscore,dscore, biber_mean, biber_sd) %>%
  arrange(input, dimension, feature)


result  <- result %>%
                          tidyr::nest(dimension_tags = c(dimension, feature, detail, count,value, zscore,dscore, biber_mean, biber_sd)) %>%
                         mutate(dimension_scores = map(dimension_tags, ~ .x %>%
                                                         {if(deflated == TRUE) filter(., biber_mean >= 0.1) else .} %>%
                                                        summarise(dimension_score = sum(dscore, na.rm = TRUE),
                                                                  .by = c(dimension)) %>%
                                                                    arrange(dimension) %>%
                                                  pivot_wider(names_from = "dimension", values_from = "dimension_score") %>% select(-Other)),
                                  dimension_scores = add_closest_text_type(dimension_scores, by = NULL))


    result_list[[i]] <-  result

  }


bind_rows(result_list) %>%
  tidyr::hoist(dimension_scores, "closest_text_type") %>%
  relocate(closest_text_type, .after = wordcount)
  }

