## `ggdag::simulate_data()` won't work for my purposes because it 
## doesn't give me the path coefficients. Need to write my own with 
## `dagitty::exogenousVariables()` and `dagitty::topologicalOrdering()`.

#' Generate coefficients for a linear SEM from a given DAG
#' 
#' @param dag A DAG of class \code{dagitty}
#' @param effect_size The coefficient on the exposure variable in the 
#'                    structural equation for the outcome
#' @return A list of information containing information that defines a 
#'         linear SEM (without specification of SDs)
generate_sem_from_graph <- function(dag, effect_size = NULL) {
    exo <- dagitty::exogenousVariables(dag)
    topo <- dagitty::topologicalOrdering(dag)
    topo <- tibble(node = names(topo), order = unlist(topo))
    topo <- topo %>%
        mutate(exo = node %in% exo) %>%
        filter(!exo) %>%
        select(-exo) %>%
        arrange(order)

    ## Define exposure and outcome. Get smallest minimal adjustment set.
    ## If there is a tie for smallest, choose the first one.
    exposure <- dagitty::exposures(dag)
    outcome <- dagitty::outcomes(dag)
    which_adj_set <- dagitty::adjustmentSets(dag) %>% lengths() %>% which.min()
    adj_set <- dagitty::adjustmentSets(dag)[[which_adj_set]]

    ## Loop over endogenous nodes in topological order
    parents_list <- vector("list", nrow(topo))
    coeffs_list <- vector("list", nrow(topo))
    for (i in seq_len(nrow(topo))) {
        ## Get parents of this node
        this_node <- topo$node[i]
        node_parents <- dagitty::parents(dag, this_node)

        ## Generate random coefficients
        coeffs <- runif(length(node_parents), min = -1, max = 1)
        names(coeffs) <- node_parents

        ## Store node parents and coefficients
        parents_list[[i]] <- node_parents
        coeffs_list[[i]] <- coeffs
    }
    names(coeffs_list) <- topo$node
    sem <- list(
        exo = exo,
        endo = tibble(topo, parents = parents_list, coeffs = coeffs_list),
        exposure = exposure,
        outcome = outcome,
        adjustment_set = adj_set
    )

    if (!is.null(effect_size)) {
        ## Set the causal effect to be effect_size
        sem$endo$coeffs[[outcome]][exposure] <- effect_size
    }

    sem
}

#' Simulate a dataset from a given SEM
#' 
#' @param sem Result from generate_sem_from_graph
#' @param n Size of dataset to simulate
#' @param noise_sd SD of the error term for the outcome
#' @return A dataset arising from the supplied SEM and noise SD
simulate_data_from_sem <- function(sem, n, noise_sd) {
    ## Argument checking
    stopifnot(all(c("exo", "endo") %in% names(sem)))
    stopifnot(is(sem$exo, "character"))
    stopifnot(is(sem$endo, "tbl"))
    stopifnot(identical(lengths(sem$endo$parents), unname(lengths(sem$endo$coeffs))))

    ## Simulate exogenous variables
    exo <- sem$exo
    sim_data <- matrix(
        rnorm(length(exo)*n, mean = 0, sd = 1),
        ncol = length(exo)
    )
    colnames(sim_data) <- exo
    sim_data <- as_tibble(sim_data)

    ## Loop over endogenous variables and simulate them
    endo <- sem$endo
    for (i in seq_len(nrow(endo))) {
        this_node <- endo$node[i]
        node_parents <- endo$parents[[i]]
        coeffs <- matrix(endo$coeffs[[i]], ncol = 1)
        if (this_node==sem$outcome) {
            sim_data[[this_node]] <- as.numeric(as.matrix(sim_data[,node_parents]) %*% coeffs) + rnorm(n, mean = 0, sd = noise_sd)
        } else {
            sim_data[[this_node]] <- as.numeric(as.matrix(sim_data[,node_parents]) %*% coeffs) + rnorm(n, mean = 0, sd = 1)
        }
    }

    ## Save adjustment set, exposure, outcome variable info as attributes
    attr(sim_data, "exposure") <- sem$exposure
    attr(sim_data, "outcome") <- sem$outcome
    attr(sim_data, "adjustment_set") <- sem$adjustment_set

    sim_data
}
