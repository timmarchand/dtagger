#' Initialize Model
#'
#' @param lang The language to use for the model. Default is 'english'.
#' @import udpipe
#' @return None
#' @export
#'
#' @examples
#' init_udpipe_model()
#'
init_udpipe_model <- function(lang = 'english')
{
  udmodel <<- udpipe::udpipe_download_model(language = lang)
  udmodel <<- udpipe::udpipe_load_model(file = udmodel$file_model)
}
