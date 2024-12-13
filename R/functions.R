
#' Title
#'
#' @param data
#'
#' @return A data.frame/tibble.
#'
descriptive_stats <- function(data) {
    data %>%
    dplyr::group_by(metabolite) %>%
        dplyr::summarise(dplyr::across(
            value,
            base::list(
                mean = mean,
                sd = sd
            )
        )) %>%
        dplyr::mutate(dplyr::across(
            dplyr::where(is.numeric),
            ~ base::round(.x, digits = 1)
        ))
}
