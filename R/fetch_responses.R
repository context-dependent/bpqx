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
        format = "  fetching :what [:bar] (:spin) :percent",
        total = 100,
        show_after = 0.3
    )
    prog <- 0
    req_check <- req_base |>
        httr2::req_url_path_append(rsp_progress$result$progressId)

    while (prog < 100) {
        rsp_check <- req_check |>
            httr2::req_perform() |>
            httr2::resp_body_json()
        
        new_prog <- rsp_check$result$percentComplete
        delta <- new_prog - prog

        if(delta > 0) {
            pb$update(new_prog / 100, tokens = list(what = survey_id))
        } else {
            pb$tick(tokens = list(what = survey_id))
        }
        
        prog <- new_prog
    }

    pb$terminate()

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
