
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dtagger

<!-- badges: start -->
<!-- badges: end -->

The dtagger package is an R package that replicates Biber’s (1988)
Multidimensional analysis and Nini’s (2019) Multi-Dimensional Analysis
Tagger (MAT) tool. The most important function is dtag_directory, which
tags a directory of folders of plain text files with MDA tags.

## Installation

You can install the development version of dtagger from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("timmarchand/dtagger")
```

## Usage `dtag_directory`

The main function in dtagger is `dtag_directory`, which tags a directory
of plain text files with MDA tags. The tagged text is then used to
calculate dimension scores based on Biber’s (1988) standard.

``` r
library(dtagger)
## basic example code
dtag_directory(path = "path_to_folder", n = NULL, ST = FALSE, deflated = TRUE)
```

### Arguments

- `path`: A character string denoting the folder containing the target
  folders (at any level).

- `n`: An optional argument denoting the maximum number of text files to
  be analyzed.

- `ST`: Logical argument denoting whether the text files have \_ST tags
  included already.

- `deflated`: Logical argument. If TRUE (default), returns the dimension
  scores with “deflated” results, which means rare features from Biber’s
  original study (mean freq \< 0.1) are removed from the Dimension score
  calculations.

### Returns

The function returns a list of tibbles including the tagged texts,
individual and corpus-level scores for each dimension of the text and
word counts.

If the function detects more than one corpus folder (folders prefixed
with \$\$), it will also return the result of post-hoc significance
tests.

This is a set of confidence intervals on the differences between the
means of the dimension scores based on the Studentized range statistic,
Tukey’s ‘Honest Significant Difference’ method.

- corpus_dimension_scores: A tibble containing the name of the corpus
  folder, the closest text type for average corpus dimensions, the mode
  of the closest text type for the documents within the corpus folder,
  and the calculated scores for Dimension1 \~ Dimension6.

- document_dimension_scores: A tibble containing the name of the corpus
  folder, the name of the text file, the calculated scores for
  Dimension1 \~ Dimension6, the closest matching text type for each
  doc_id, based on Biber (1989), and the MDA tags.

- dimension_tags: A tibble containing the corpus name, doc_id, the
  Dimension1 \~ Dimension6 scores, the MDA tags, and additional
  information for each MDA tag.

- tokenized_tags: A tibble containing the corpus name, doc_id, the text
  tokenized on each \_ST tag, and the text tokenized on each <MDA> tag.

- Tukey_hsd: A tibble containing the dimension for pairwise comparison,
  the corpora under pairwise comparison, the expected difference in
  means after aov (zero), the difference in means after aov, the 95%
  familywise lower confidence level, the 95% familywise upper confidence
  level, and the significance test.

### References

1.  Biber, D. (1988). Variation across Speech and Writing. Cambridge:
    Cambridge University Press. <doi:10.1017/CBO9780511621024>

2.  Biber, D. (1989). A typology of English texts. , 27(1), 3-44.
    <https://doi.org/10.1515/ling.1989.27.1.3>

3.  Nini, A. (2019). The Multi-Dimensional Analysis Tagger. In Berber
    Sardinha, T. & Veirano Pinto M. (eds), Multi-Dimensional Analysis:
    Research Methods and Current Issues, 67-94, London; New York:
    Bloomsbury Academic.

# Other functions

The `dtag_directory` function is a wrapper for a number of other
‘dtagger’ functions that are also accessible in the package, including
`add_st_tags` and `add_mda_tags`, the latter of which is also modular in
structure, allowing users to check and modify the algorithms behind the
scenes.

Other utility functions that might be useful beyond the multidimensional
analysis include, `quick_conc`, for fast KWIC (Key Word in Context)
concordancing, `conc_by_tag` to display KWIC results for search results
based on tag matches, and `missing_tags`, to identify where tags
produced by one tagger are not found in the output of another.

# Add tag functions

## Add Stanford \_ST tags with `add_st_tags`

``` r

 # Example text:
 text <- "This is an example sentence to be tagged"
 # Example speech, tokenized:
 speech <- c("I","don't", "know" ,  "erm" ,",", "whether" , "to" ,
 "include" ,"hesitation" , "markers", ".")
 # Initiate udpipe model
 init_udpipe_model()
 # Tag text
 add_st_tags(text)
 # Tag speech
 add_st_tags(speech, st_hesitation = TRUE, tokenized = TRUE)
```

The `add_st_tags` function is designed to process and annotate text
using the Universal Dependencies (UD) model with the udpipe package. It
allows users to tokenize and tag text with part-of-speech (Stanford)
tags, and to extract and handle hesitation markers. The function
provides options for controlling the parsing, tokenizer type, and
handling of flattened input.

### Arguments

- x A character vector of input text to be processed.

- mdl A udpipe model to use for processing the text. The default is the udmodel.

- st_hesitation A logical value indicating whether or not to extract hesitation markers

  from the input text. If `TRUE`, the function will extract hesitation
  markers and return them separately. Default is `FALSE`.

- flattened A logical value indicating if the input text is flattened. If `FALSE`, i.e. if the character string is in tokenized form, the function will flatten the text before processing. Default is `TRUE`.

- skip_parse A logical value determining if the function should skip parsing

  and only return tokenized and tagged text. If `FALSE`, the function
  returns the full UD model when parsing. Default is `TRUE`.

- … Additional arguments to be passed to the `udpipe_annotate()`
  function. For example:

– `tokenizer = "horizontal"` to force the `udpipe_annotate` function to
tokenize on tokens separated by white spaces. This will combine words
and trailing punctuation marks, unless they have been spearated by white
space previously.

–`tokenizer = "vertical"` to force the `udpipe_annotate` function to
tokenize on tokens separated by new line breaks. This can be useful if
you want the tokenizer to recognise multi-word entities as a single
token, or avoid separating hyphenated words.

### Returns

The function returns a character vector of tokenized and tagged text.

If `skip_parse` is `FALSE`, the function returns a tibble with the full
udpipe model when parsing.

If `st_hesitation` is `TRUE` (experimental), the function returns a
character vector of tokenized and tagged text with hesitation markers
extracted and handled separately.

### **Dependencies**

The function requires the **`udpipe`** package to be installed and
loaded. The **`udmodel`** object must also be present in the global
environment.

## Add MDA tags with `add_mda_tags()`

This function adds <MDA> tags to a character vector of text. It assumes
that the input text has already been tokenized and tagged with Stanford
POS (\_ST) tags. The function works by applying a series of tagging
rules to the text. The rules can be accessed and adjusted by referring
to their individual function code files

### Usage

``` r
add_mda_tags(x, mda_hesitation = TRUE, progress = FALSE,...)
```

### Arguments

- x: A character vector containing the text to be tagged.

- mda_hesitation: If TRUE, hesitation markers are extracted from the
  text. Experimental feature - should hesitation markers be excluded
  before tagging? Regex for the hesitation markers is the same as the
  default for dtag_hesitation, but can be set using the regex argument.
  See `dtag_hesitation` for details.

- progress: If TRUE, a progress message is printed.

`...`: Additional arguments to pass on.

### Examples

Here’s a basic example of how to use the add_mda_tags() function:

``` r
text <- c("I_PPSS", "have_VB", "a_DET", "dog_NN")
add_mda_tags(text)
```

Here’s a more detailed example that shows how to tag text with both \_ST
and <MDA> tags:

``` r
# Generate some text
text <- "This example is short and sweet. This means that not all the tags will
have been included, which is why this is really only a guide and it should
be used with that in mind. Otherwise, I think it may lead to disappointment."

# Load udpipe model into the global environment for _ST tagging
init_udpipe_model()

# Add Stanford tags to text
text <- add_st_tags(text)

# Add <MDA> tags
add_mda_tags(text)
```

## Add a Table of ST, MDA and Other UD Tags with `add_tag_tbl`

The `add_tag_tbl` function is a wrapper around the `add_st_tags` and
`add_mda_tags` functions that also retains all the Universal Dependency
tags produced by the udpipe model.

### Usage

``` r

# Process text with the add_tag_tbl function
text <- "This is a sample sentence."
result <- add_tag_tbl(text)
```

### Arguments

- x A character vector of input text to be processed.

- … Additional arguments to be passed to the `add_st_tags()` function.

## Returns

A tibble with the original text annotated with ST tags and MDA tags. The
output columns includes: - id columns (`doc_id`, `paragraph_id`,
`sentence_id` etc.)

- udpipe output (`token`, `upos`, `xpos`, `dep_rel`, etc)

- both `st` and `mda` tags.

# Concordancing functions

## Quick concordancing with `quick_conc`

**`quick_conc`** is a lightweight concordancing function that returns
key words in context (KWIC) in a tidy format. Given a character vector
of tokenized strings, **`x`**, and either a character vector of regex
patterns to match (**`index`**) or a numeric vector to use as index of
matches, **`quick_conc`** returns a tibble with the contextualized text
shown around the matched node.

### Usage

``` r
quick_conc(x, index, n = 5, tokenize = FALSE, separated = FALSE)
```

### Arguments

- `x`: a character vector of tokenized strings, or a single string

- `index`: a character vector of regex patterns to match, or a numeric
  vector to use as index of matches

- `n`: an integer, to specify the number of context tokens either side
  of the matched node

- `tokenize`: a logical, to tokenize the text first or not

- `separated`: a logical, to separate the context tokens or not

### Examples

``` r
x <- c("The", "cat", "sat", "on", "the", "mat")
    index <- c("cat", "sat")
    quick_conc(x, index, n = 2)
    
x <- "The dog barked loudly, alerting the neighbors of potential danger. A nearby park seemed like the perfect spot for the dog and it quickly made its way there."
    quick_conc(x, index = "dog", n = 3, tokenize = TRUE, separated = TRUE)
    quick_conc(x, index = c(4,8,12), tokenize = TRUE)
```

## Concordancing by tags with `conc_by_tag`

**`conc_by_tag`** allows for fine-grained concordance searches of tagged
text and is typically used with output from `udpipe::udpipe_annotate`
and `dtagger::dtag_tbl` , `dtagger::dtag_directory` or
`dtagger::add_tag_tbl` functions.

The function takes a relational data frame containing the text to
concordance as input, along with the name of the column containing the
text to concatenate, the name of the column containing the tags to
match, the tag to match within the tag column, and identifying details
such as corpus, doc_id etc. The function can take up to two tag inputs,
for example matching all upos == “ADJ” tags and dep_rel == “amod” tags,
and seeing the resulting keywords in context.

## Usage

``` r
 # Process text with the add_tag_tbl function (assuming add_mda_tags function is defined)
 text <- c(doc1 = "This is a simple sentence with a specific keyword.",
           doc2 = "Is this one more complex or simpler?")
 data <- add_tag_tbl(text)

 # Run conc_by_tag function with specified tags and matches
 conc_by_tag(
   data,
   what = "token",
   tag = "xpos",
   match = "^JJ$",
   cols = c("doc_id", "lemma"),
   tag2 = "dep_rel",
   match2 = "^amod$"
 )
 conc_by_tag(
   data,
   what = "token",
   tag = "xpos",
   match = "JJ",
   cols = c("doc_id", "dep_rel"),
   separated = TRUE,
   n = 3
 )
```

The **`conc_by_tag`** function produces concordance lines of text from
**`data`** by finding up to two tag matches in tokenized text. The input
should be a dataframe with a column for tokens in tokenized form, and
separate columns for tags, document and corpus details. Typically, the
function can be used with output from **`udpipe::udpipe_annotate`** and
**`dtagger::dtag_tbl`** or **`dtagger::add_tab_tbl`** functions.

The concordancer can take up to two tag inputs, for example matching all
**`upos == "ADJ"`** tags and **`dep_rel == "amod"`** tags, and seeing
the resulting key words in context.

The output is a data frame of KWIC format, containing the specified
match at the centre, and left and right windows of the concordancing.

## Arguments

- `data`: A relational data frame containing the text to concordance.

- `what`: The name of the column containing the text to concatenate.
  Default is “token”.

- `tag`: The name of the column containing the tags to match. Default is
  “mda_tags”.

- `match`: The tag to match within the **`tag`** column.

- `cols`: The names of the columns to include in the output. By default,
  these will include corpus, doc_id, sentence, but need to be changed if
  the inputting df has different columns.

- `tag2`: The name of the second column containing the tags to match
  (optional).

- `match2`: The second tag to match within the **`tag2`** column
  (optional).

- `...`: Additional arguments to be passed onto
  **`dtagger::quick_conc`**.

## Returns

-  `case` - a case number for the match found.

-  `left` - objects immediately adjacent (up to n) to the left of the matched node,

  as defined by the `what` argument (default is token).

In case of `separated = TRUE`, the left are separated into left(n):left1

-  match - the matched search item, as defined by the `match` argument.

-  right - tokens immediately adjacent (up to n) to the right of the matched node,

  as defined by the `what` argument (default is token).

In case of `separated = TRUE`, the right tokens are separated into
right1:right(n).

-  index - the index row position of matched result from the input data frame.

-  other cols - as defined by the `tag`, `tag2` and `cols` arguments.

## Compare tagging output with `missing_tags`

The **`missing_tags`** function is used to compare the tagging results
of two character vectors in tokenised form. The function produces
concordance lines of where tags in the first vector are missing from the
same index position in the second vector. Both inputs should be exactly
the same length.

For the function to work as expected, the two vectors should be of equal
length and alligned by token.

## Example

``` r

# create two example vectors
# vec1 tagged with ewt udpipe English model
vec1 <- c("This_DT", "is_VBZ", "a_DT", "test_NN", "sentence_NN", "with_IN", 
"tags_NNS", "._.") 

# vec2 tagged with line udpipe English model

vec2 <- c("This_DEM-SG", "is_PRES", "a_IND-SG", "test_SG-NOM", "sentence_SG-NOM", 
NA, "tags_PL-NOM", "._Period") 

# find tags in vec1 that are missing in vec2
missing_tags(vec1, vec2, regex = "DT")

# find tags in vec2 that are missing in vec1
missing_tags(vec2, vec1, regex = "Period")

# find tags in vec1 that are missing in vec2 but with different tag searches
missing_tags(vec1, vec2, regex1 = "DT", regex2 = "IND-SG")
```
