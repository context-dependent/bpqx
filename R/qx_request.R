#' Create a request url for httr2::request
#' @param ...
#'   strings, beginning with the endpoint,
#'   representing the additional pathing following ...API/v3
qx_req <- function(...) {
    req <- httr2::request(qx_url(...))
    req |>
        httr2::req_headers(
            "X-API-TOKEN" = api_token(),
            "Content-Type" = "application/json",
            "Accept" = "*/*",
            "accept-encoding" = "gzip, deflate"
        ) |>
        httr2::req_error(body = qx_code)
}

qx_url <- function(...) {
    path <- paste0(unlist(list(...)), collapse = "/")
    glue::glue("https://{data_center()}.qualtrics.com/API/v3/{path}")
}

qx_code <- function(res) {
    interp <- switch(as.character(res$status),
        `200` = "Success: 200",
        `401` =
            c(
                "Qualtrics API reported an authentication error (401):",
                "You may not have the required authorization",
                "Please check your API key and base URL."
            ),
        `403` =
            c(
                "Qualtrics API reported an forbidden error (403):",
                "You may have a valid API key that lacks API query permissions",
                "Please check your settings and/or talk to your administrators."
            ),
        `400` =
            c(
                "Qualtrics API reported a bad request error (400):",
                "Please report this on https://github.com/context-dependent/bpqx"
            ),
        `404` =
            c(
                "Qualtrics API reported a not found error (404):",
                "Please check if you are using the correct survey ID."
            ),
        `413` =
            c(
                "Qualtrics API reported a 413 error:",
                "The request body was likely too large.",
                "Can also occur when a multipart/form-data request is malformed."
            ),
        `429` =
            c(
                "Qualtrics API reported a 429 error:",
                "You have reached the concurrent request limit."
            ),
        `500` =
            c(
                "After 4 attempts, Qualtrics API reported a temporary internal server error (500):",
                "Please contact Qualtrics Support or retry your query",
                glue::glue("instanceId: {httr::content(res)$meta$error$instanceId}"),
                glue::glue("errorCode: {httr::content(res)$meta$error$errorCode}")
            ),
        `503` =
            c(
                "After 4 attempts, Qualtrics API reported a temporary internal server error (503):",
                "Please contact Qualtrics Support or retry your query",
                glue::glue("instanceId: {httr::content(res)$meta$error$instanceId}"),
                glue::glue("errorCode: {httr::content(res)$meta$error$errorCode}")
            ),
        `504` =
            c(
                "After 4 attempts, Qualtrics API reported a gateway timeout error (504):",
                "Please contact Qualtrics Support or retry your query",
                glue::glue("instanceId: {httr::content(res)$meta$error$instanceId}"),
                glue::glue("errorCode: {httr::content(res)$meta$error$errorCode}")
            ),
        # Default response for unknown status code:
        c(
            glue::glue("Qualtrics API reported the atypical status code {res$status_code}"),
            "A dictionary of status codes can be found here: https://developer.mozilla.org/en-US/docs/Web/HTTP/Status",
            "Please check your request, and report at https://github.com/ropensci/qualtRics/issues if reoccurring:"
        )
    )

    return(interp)
}
