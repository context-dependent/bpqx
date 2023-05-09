#' List surveys accessible with saved api token
list_surveys <- function() {
    base_url <- qx_url("surveys")
    req <- qx_req(base_url)

    f <- function(o) {
        rsp <- req |>
            httr2::req_url_query(offset = o) |>
            httr2::req_perform()
        b <- httr2::resp_body_json(rsp)
        surveys <- b$result$elements
        next_page <- b$result$nextPage

        if (is.null(next_page)) {
            return(surveys)
        } else {
            return(c(surveys, f(o + 100)))
        }
    }

    f(0) |>
        purrr::transpose() |>
        purrr::map(unlist) |>
        tibble::as_tibble()
}
