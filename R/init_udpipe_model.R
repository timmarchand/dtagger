#' Initialize UDPipe Model
#'
#' @param lang The language to use for the model. Default is 'english'.
#' @importFrom udpipe udpipe_download_model udpipe_load_model
#' @return None
#' @export
#'
#' @examples
#' \dontrun{init_udpipe_model()}
#'
init_udpipe_model <- function(lang = 'english')
{
  udmodel <<- udpipe::udpipe_download_model(language = lang)
  udmodel <<- udpipe::udpipe_load_model(file = udmodel$file_model)
}
