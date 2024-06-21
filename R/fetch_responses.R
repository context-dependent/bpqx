#' Fetch responses for a given survey
#'
#' @param survey_id
#'   String: a unique identifier for the target survey
#' @export
fetch_responses <- function(survey_id) {
    req_base <- qx_req("surveys", survey_id, "export-responses")
    rsp_progress <- req_base |>
        httr2::req_body_json(list(format = "spss")) |>
        httr2::req_perform() |>
        httr2::resp_body_json()

    pb <- progress::progress_bar$new(
        format = glue::glue("fetch {survey_id} [:bar] :percent :eta"),
        total = 100
    )
    progress <- 0
    req_check <- req_base |>
        httr2::req_url_path_append(rsp_progress$result$progressId)

    while (progress < 100) {
        pb$tick()
        rsp_check <- req_check |>
            httr2::req_perform() |>
            httr2::resp_body_json()

        progress <- rsp_check$result$percentComplete
    }

    tdir <- tempdir()
    tfile <- tempfile(tmpdir = tdir)

    req_base |>
        httr2::req_url_path_append(rsp_check$result$fileId, "file") |>
        httr2::req_perform(path = tfile)

    sav <- unzip(tfile, exdir = tdir)

    d <- haven::read_spss(sav) |> haven::as_factor()

    unlink(tfile)
    unlink(tdir)
    unlink(sav)

    d
}
