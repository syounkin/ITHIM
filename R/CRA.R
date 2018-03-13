CRA <- function(meanlog.baseline = NA,
                sdlog.baseline = 1,
                p0.baseline = 0.5,
                meanlog.scenario = meanlog.baseline,
                sdlog.scenario = sdlog.baseline,
                n = 5,
                B = 1e3,
                P,
                Q,
                R = function(x) exp(-x),
                type = "parametric"){

    if(type == "parametric"){

        TA.sample.baseline <- rlnorm(n = B, meanlog = meanlog.baseline, sdlog = sdlog.baseline)
        runif <- runif(n = B)

        TA.baseline <- ifelse(runif < p0.baseline, 0, TA.sample)

        P <- quantile(TA.baseline, probs = (1:n)/n)

        TA.sample.scenario <- rlnorm(n = B, meanlog = meanlog.scenario, sdlog = sdlog.scenario)
        runif <- runif(n = B)

        TA.scenario <- ifelse(runif < p0.scenario, 0, TA.sample)

        Q <- quantile(TA.scenario, probs = (1:n)/n)

    }

    return((sum(R(P)) - sum(Q(P)))/sum(R(P)))
}
