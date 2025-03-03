cofs <- data.frame()
for (i in unique(exp3_het$Participant)) {
    for (j in 1:20) {
        reg <- lm(y ~ x, data = exp3_het[exp3_het$Participant == i, ][-j, ])
        row <- data.frame(
            Participant = i,
            intercept = coefficients(reg)[1],
            slope = coefficients(reg)[2]
        )
        cofs <- rbind(cofs, row)
    }
}

ggplot(cofs, aes(intercept)) +
    geom_histogram(fill = "cornflowerblue", color = "black") +
    facet_wrap(~Participant) +
    theme_classic() +
    ggtitle("Histogram of leave-one-out intercepts for each participant")
ggsave("intercepts.pdf")
# qqnorm(cofs$intercept)
# qqline(cofs$intercept)
# shapiro.test(cofs$intercept)

ggplot(cofs, aes(slope)) +
    geom_histogram(fill = "cornflowerblue", color = "black") +
    facet_wrap(~Participant) +
    theme_classic() +
    ggtitle("Histogram of leave-one-out slopes for each participant")
ggsave("slopes.pdf")
# qqnorm(cofs$slope)
# qqline(cofs$slope)
# shapiro.test(cofs$slope)

ggplot(cofs, aes(intercept, slope, fill = Participant)) +
    geom_point(color = "black", pch = 21) +
    theme_classic() +
    # theme(legend.position = "none") +
    ggtitle("Scatterplot of leave-one-out intercept against slope.")
ggsave("intercepts_slopes.pdf")



# Joint hierarchical posterior 

hierarchical_model <- function(x, random, hypermu, hypersigma, mu, sigma) {
    # Combine hypotheses and data
    bayesdata <- data.frame(
        hypermu = rep(hypermu, each = length(hypersigma) * length(mu) * length(sigma) * length(x)),
        hypersigma = rep(hypersigma, times = length(hypermu), each = length(mu) * length(sigma) * length(x)),
        mu = rep(mu, times = length(hypermu) * length(hypersigma), each = length(sigma) * length(x)),
        sigma = rep(sigma, times = length(hypermu) * length(hypersigma) * length(mu), each = length(x)),
        group = rep(random, times = length(hypermu) * length(hypersigma) * length(mu) * length(sigma)),
        data = rep(x, times = length(hypermu) * length(hypersigma) * length(mu) * length(sigma))
    )
    ndata <- length(x) / length(unique(random))

    # Calculate individual likelihoods

    bayesdata$prior <- (1 / bayesdata$sigma) * exp(
        -(((bayesdata$data - bayesdata$mu)^2) / (2 * bayesdata$sigma^2))
    )

    bayesdata <- bayesdata |> 
    group_by(hypermu, hypersigma, mu, sigma, group) |> 
    summarise(prior = prod(prior))

    bayesdata <- bayesdata |> 
    group_by(hypermu, hypersigma, mu, group) |> 
    summarise(prior = sum(prior))

    bayesdata$prior <- bayesdata$prior * (1 / bayesdata$hypersigma) * exp(-(((bayesdata$mu - bayesdata$hypermu)^2) / (2 * bayesdata$hypersigma^2)))

    priors <- bayesdata |>
        group_by(hypermu, hypersigma, group) |>
        summarise(prior = sum(prior))

    likelihood <- priors |>
        group_by(hypermu, hypersigma) |>
        summarise(likelihood = prod(prior))

    likelihood$posterior <- likelihood$likelihood / sum(likelihood$likelihood)

    return(likelihood)
}
bci <- function(df, posterior = posterior, ci = 0.95) {
    cindex <- c()
    interval <- df[FALSE, ]
    while (sum(interval$posterior) < ci) {
        index <- which(df$posterior == max(df$posterior[!1:nrow(df) %in% cindex]))
        interval <- rbind(interval, df[index, ])
        cindex[length(cindex) + 1] <- index
    }
    return(c(min(cindex), max(cindex)))
}

# INTERCEPT

# Define hypotheses
hypermu <- seq(50, 350, 5)
hypersigma <- seq(10, 300, 10)
mu <- seq(50, 350, 10)
sigma <- seq(10, 300, 10)

likelihood <- hierarchical_model(
    cofs$intercept,
    cofs$Participant,
    hypermu,
    hypersigma,
    mu,
    sigma
    )


likelihood |>
    ggplot(aes(hypersigma, hypermu, fill = posterior)) +
    geom_tile() +
    scale_fill_gradient2(
        "Posterior probability",
        low = "black",
        mid = "blue",
        high = "white",
        midpoint = (max(likelihood$posterior * 0.5))
    ) +
    theme_classic() +
    labs(y = expression(mu), x = expression(sigma), title = "Joint PDF for intercept") +
    coord_flip()


marginal_mu <- likelihood |>
    group_by(hypermu) |>
    summarise(posterior = sum(posterior))
int_mu_mode <- marginal_mu[marginal_mu$posterior == max(marginal_mu$posterior),]

ggplot(marginal_mu, aes(hypermu, posterior)) +
    geom_path() +
    theme_classic() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
        x = expression(mu),
        y = "Posterior probability",
        title = paste0(
            "Marginal PDF of mu for intercept. ",
            "Mode = ", int_mu_mode$hypermu,
            " (",
            marginal_mu$hypermu[bci(marginal_mu)][1],
            "-",
            marginal_mu$hypermu[bci(marginal_mu)][2],
            ")"
            )
    ) +
    geom_vline(
        xintercept = marginal_mu$hypermu[bci(marginal_mu)],
        color = "red",
        linewidth = 1,
        linetype = 2
    ) +
    theme(title = element_text(size = 10))
ggsave("marginal_mu_intercept.pdf")

marginal_sigma <- likelihood |>
    group_by(hypersigma) |>
    summarise(posterior = sum(posterior))

ggplot(marginal_sigma, aes(hypersigma, posterior)) +
    geom_path() +
    theme_classic() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
        x = expression(sigma),
        y = "Posterior probability",
        title = "Marginal PDF of sigma for intercept"
    ) +
    geom_vline(
        xintercept = marginal_sigma$hypersigma[bci(marginal_sigma)],
        color = "red",
        linewidth = 1,
        linetype = 2
    )






# SLOPE

# Define hypotheses
hypermu <- seq(0.1, 2, 0.05)
hypersigma <- seq(0.05, 1, 0.05)
mu <- seq(0.1, 3, 0.1)
sigma <- seq(0.1, 2, 0.1)

likelihood <- hierarchical_model(
    cofs$slope,
    cofs$Participant,
    hypermu,
    hypersigma,
    mu,
    sigma
    )


likelihood |>
    ggplot(aes(hypersigma, hypermu, fill = posterior)) +
    geom_tile() +
    scale_fill_gradient2(
        "Posterior probability",
        low = "black",
        mid = "blue",
        high = "white",
        midpoint = (max(likelihood$posterior * 0.5))
    ) +
    theme_classic() +
    labs(y = expression(mu), x = expression(sigma), title = "Joint posterior pdf of slope") +
    coord_flip()
ggsave("jointpdf_slope.pdf")


marginal_mu <- likelihood |>
    group_by(hypermu) |>
    summarise(posterior = sum(posterior))
slope_mu_mode <- marginal_mu[marginal_mu$posterior == max(marginal_mu$posterior),]

ggplot(marginal_mu, aes(hypermu, posterior)) +
    geom_path() +
    theme_classic() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
        x = expression(mu),
        y = "Posterior probability",
        title = paste0(
            "Marginal PDF of mu for slope. ",
            "Mode = ", slope_mu_mode$hypermu,
            " (",
            marginal_mu$hypermu[bci(marginal_mu)][1],
            "-",
            marginal_mu$hypermu[bci(marginal_mu)][2],
            ")"
            )
    ) +
    geom_vline(
        xintercept = marginal_mu$hypermu[bci(marginal_mu)],
        color = "red",
        linewidth = 1,
        linetype = 2
    ) +
    theme(title = element_text(size = 10))
ggsave("marginal_mu_slope.pdf")

marginal_sigma <- likelihood |>
    group_by(hypersigma) |>
    summarise(posterior = sum(posterior))

ggplot(marginal_sigma, aes(hypersigma, posterior)) +
    geom_path() +
    theme_classic() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
        x = expression(sigma),
        y = "Posterior probability",
        title = "Marginal PDF of sigma for slope"
    ) +
    geom_vline(
        xintercept = marginal_sigma$hypersigma[bci(marginal_sigma)],
        color = "red",
        linewidth = 1,
        linetype = 2
    )






