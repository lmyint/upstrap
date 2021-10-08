run_simulation <- function(dag, noise_sd, curve_sample_sizes, train_sample_sizes, train_reps = 100, iters_truth, iters_upstrap) {
    ## Generate SEM for the DAG where causal effect is 0.01
    sem <- generate_sem_from_graph(dag, effect_size = 0.01)

    ## Generate huge superpopulation of data
    superpop_size <- 1e6
    true_data_big <- simulate_data_from_sem(sem = sem, n = superpop_size, noise_sd = noise_sd)

    ## Get true versions of quantities of interest
    print(system.time({
    truth <- get_quantities_from_data(.data = true_data_big, sample_sizes = curve_sample_sizes, iters = iters_truth, with_replacement = FALSE)
    }))

    ## Get upstrap estimates
    print(system.time({
    estimates_upstrap <- lapply(train_sample_sizes, function(train_size) {
        replicate(train_reps, {
            train_data <- dplyr::slice_sample(true_data_big, n = train_size)
            get_quantities_from_data(.data = train_data, sample_sizes = curve_sample_sizes, iters = iters_upstrap, with_replacement = TRUE)
        }, simplify = FALSE)
    })
    }))

    list(truth = truth, estimates_upstrap = estimates_upstrap)
}


## ============================================================================
## Set up axes of the sensitivity analysis
## dag_simple{1,2,3,4} are simple confounder triangles with 1,2,4,8 confounders

dag_simple1 <- dagitty("dag {
bb=\"0,0,1,1\"
A [exposure,pos=\"0.100,0.500\"]
C1 [pos=\"0.500,0.200\"]
Y [outcome,pos=\"0.900,0.500\"]
A -> Y
C1 -> A
C1 -> Y
}")

dag_simple2 <- dagitty("dag {
bb=\"0,0,1,1\"
A [exposure,pos=\"0.100,0.500\"]
C1 [pos=\"0.250,0.200\"]
C2 [pos=\"0.750,0.200\"]
Y [outcome,pos=\"0.900,0.500\"]
A -> Y
C1 -> A
C1 -> Y
C2 -> A
C2 -> Y
}")

dag_simple3 <- dagitty("dag {
bb=\"0,0,1,1\"
A [exposure,pos=\"0.100,0.500\"]
C1 [pos=\"0.250,0.200\"]
C2 [pos=\"0.400,0.200\"]
C3 [pos=\"0.550,0.200\"]
C4 [pos=\"0.700,0.200\"]
Y [outcome,pos=\"0.900,0.500\"]
A -> Y
C1 -> A
C1 -> Y
C2 -> A
C2 -> Y
C3 -> A
C3 -> Y
C4 -> A
C4 -> Y
}")

dag_simple4 <- dagitty("dag {
bb=\"0,0,1,1\"
A [exposure,pos=\"0.100,0.500\"]
C1 [pos=\"0.150,0.200\"]
C2 [pos=\"0.250,0.200\"]
C3 [pos=\"0.350,0.200\"]
C4 [pos=\"0.450,0.200\"]
C5 [pos=\"0.550,0.200\"]
C6 [pos=\"0.650,0.200\"]
C7 [pos=\"0.750,0.200\"]
C8 [pos=\"0.850,0.200\"]
Y [outcome,pos=\"0.900,0.500\"]
A -> Y
C1 -> A
C1 -> Y
C2 -> A
C2 -> Y
C3 -> A
C3 -> Y
C4 -> A
C4 -> Y
C5 -> A
C5 -> Y
C6 -> A
C6 -> Y
C7 -> A
C7 -> Y
C8 -> A
C8 -> Y
}")

dag_complex_custom <- dagitty("dag {
bb=\"0,0,1,1\"
A [exposure,pos=\"0.248,0.537\"]
X1 [pos=\"0.275,0.293\"]
X2 [pos=\"0.401,0.304\"]
X3 [pos=\"0.532,0.299\"]
X4 [pos=\"0.651,0.287\"]
X5 [pos=\"0.613,0.085\"]
X6 [pos=\"0.720,0.199\"]
Y [outcome,pos=\"0.579,0.526\"]
A -> Y
X1 -> A
X1 -> X2
X1 -> X3 [pos=\"0.403,0.107\"]
X1 -> X4 [pos=\"0.462,0.003\"]
X1 -> Y
X2 -> A
X2 -> X3
X2 -> Y
X3 -> A
X3 -> Y
X4 -> A
X4 -> Y
X5 -> X3
X5 -> X4
X5 -> X6
X6 -> X4
X6 -> Y [pos=\"0.758,0.378\"]
}")

dag_shrier <- dagitty("dag {
Coach [pos=\"-4.392,-7.906\"]
ConnectiveTissueDisorder [pos=\"3.494,-5.099\"]
ContactSport [pos=\"-1.157,2.548\"]
FitnessLevel [pos=\"-1.489,-4.530\"]
Genetics [pos=\"2.022,-7.906\"]
Injury [outcome,pos=\"4.969,8.605\"]
IntraGameProprioception [pos=\"-1.328,7.141\"]
NeuromuscularFatigue [pos=\"1.501,-1.235\"]
PreGameProprioception [pos=\"-3.638,-1.235\"]
PreviousInjury [pos=\"-4.293,4.175\"]
TeamMotivation [pos=\"-7.175,-0.950\"]
TissueWeakness [pos=\"4.236,1.857\"]
WarmUpExercises [exposure,pos=\"-7.000,8.650\"]
Coach -> FitnessLevel
Coach -> TeamMotivation
ConnectiveTissueDisorder -> NeuromuscularFatigue
ConnectiveTissueDisorder -> TissueWeakness
ContactSport -> IntraGameProprioception
ContactSport -> PreviousInjury
FitnessLevel -> NeuromuscularFatigue
FitnessLevel -> PreGameProprioception
Genetics -> ConnectiveTissueDisorder
Genetics -> FitnessLevel
Genetics -> NeuromuscularFatigue
IntraGameProprioception -> Injury
NeuromuscularFatigue -> Injury
NeuromuscularFatigue -> IntraGameProprioception
PreGameProprioception -> WarmUpExercises
PreviousInjury -> WarmUpExercises
TeamMotivation -> PreviousInjury
TeamMotivation -> WarmUpExercises
TissueWeakness -> Injury
WarmUpExercises -> Injury
WarmUpExercises -> IntraGameProprioception
}")

list_dags <- list(
    dag_simple1,
    dag_simple2,
    dag_simple3,
    dag_simple4,
    dag_complex_custom,
    dagitty::getExample("Sebastiani"),
    dagitty::getExample("Polzer"),
    dag_shrier,
    dagitty::getExample("Kampen")
)


## Parameter combinations for the full simulation study
parameter_combos <- expand.grid(
    dag_index = seq_along(list_dags),
    noise_sd = c(0.01, 0.1, 1, 10),
    stringsAsFactors = FALSE
)
