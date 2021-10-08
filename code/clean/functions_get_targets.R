#' Obtain estimates or true versions of key quantities as a 
#' function of sample size
#' 
#' @param .data Data simulated from simulate_data_from_sem (or a subset)
#' @param sample_sizes Sample sizes for samples used to fit model (target sample sizes)
#' @param with_replacement Sample from .data with replacement?
#' @return When .data is the full population of data, result is the true 
#'         sampling distribution for the supplied target sample sizes
#'         When .data is a sample of data from the population, result is the
#'         (upstrap-)estimated sampling distribution for the supplied target 
#'         sample sizes
get_quantities_from_data <- function(.data, sample_sizes, iters, with_replacement) {
    ## Extract the exposure and outcome variable
    exposure <- attr(.data, "exposure")
    outcome <- attr(.data, "outcome")

    ## Extract the adjustment set
    adj_set <- attr(.data, "adjustment_set")

    ## Create model formula
    mod_formula_part_adjset <- paste(adj_set, collapse = "+")
    mod_formula_part_right <- paste(exposure, "+", mod_formula_part_adjset)
    mod_formula <- as.formula(paste(outcome, "~", mod_formula_part_right))

    ## Loop over sample sizes
    results_by_sample_size <- lapply(sample_sizes, function(n) {
        ## Perform many iterations
        replicate(iters, {
            ## Obtain sample of size n (target sample sizes)
            data_resampled <- dplyr::slice_sample(.data, n = n, replace = with_replacement)

            ## Fit model and extract p-value
            mod <- lm(mod_formula, data = data_resampled)
            results <- tidy(mod) %>%
                filter(term==exposure)
            estim <- results$estimate
            se <- results$std.error
            pval <- results$p.value
            c(estim = estim, se = se, pval = pval)
        })
    })
    names(results_by_sample_size) <- sample_sizes

    results_by_sample_size
}

