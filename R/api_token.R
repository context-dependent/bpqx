#' Attempt to retrieve Qualtrics API token and data center from keyring
#' If not found, will prompt user to enter API token and data center
#' @rdname auth
#' @param reset_api_token If TRUE, will prompt user to enter new API token
#' @param custom_data_center If not NULL, will set data center to this value
auth <- function(reset_api_token = FALSE, custom_data_center = NULL) {
    list(
        api_token = get_set_api_token(reset = reset_api_token), 
        data_center = get_set_data_center(custom_data_center)
    )
}

#' Retrieve Qualtrics API token
#' @rdname auth
get_set_api_token <- function(reset) {
    if (!("bpqx-api-token" %in% keyring::key_list()[[1]]) || reset) {
        keyring::key_set_with_value(
            "bpqx-api-token",
            password = getPass::getPass(
                msg = "Enter your Qualtrics API Token > ",
                forcemask = TRUE
            )
        )
    }

    keyring::key_get("bpqx-api-token")
}

#' Retrieve Data Center
#' @rdname auth
#' @importFrom rlang "%||%"
get_set_data_center <- function(custom_value = NULL) {
    if (
        !("bpqx-data-center" %in% keyring::key_list()[[1]]) || 
        !is.null(custom_value)
    ) {
        data_center <- custom_value %||% "blueprintade.yul1"
        keyring::key_set_with_value(
            "bpqx-data-center",
            password = data_center
        )
    }

    keyring::key_get("bpqx-data-center")
}

