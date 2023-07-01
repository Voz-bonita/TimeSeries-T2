pacman::p_load("purrr", "glue", "kableExtra")

format_tab <- function(df, caption, ...) {
    tabela <- kable(
        df,
        caption = caption,
        booktabs = T,
        ...
    ) %>%
        kable_styling(
            latex_options = c('striped', 'hold_position'),
            full_width = F
        )
    return(tabela)
}

box_cox__ <- function(x, lambda) {
    if (lambda == 0) {
        return(log(x))
    }
    return((x^lambda - 1) / lambda)
}

box_cox <- function(x, lambda, path_to_plot) {
    lambda <- as.vector(lambda)
    n_plots <- length(lambda)
    transforms <- purrr::map(lambda, ~ box_cox__(x, .))
    png(path_to_plot)
    par(mfrow = c(3, ceiling(n_plots / 3)))
    for (i in 1:n_plots) {
        plot(transforms[[i]], main = glue::glue("lambda = {lambda[i]}"))
    }
    dev.off()
    return(transforms)
}

diff_series <- function(x, lag) {
    diffs <- x %>% ndiffs()
    if (diffs) {
        x <- x %>% diff(diffs)
        sdiffs <- x %>% nsdiffs()
    } else {
        sdiffs <- x %>% nsdiffs()
    }
    if (sdiffs) {
        x <- x %>% diff(nsdiffs, lag = lag)
    }

    return(list("ts" = x, diff_simples = diffs, diff_sasonal = sdiffs))
}

residuals_analysis <- function(model, path_to_plot) {
    residuals <- model[["residuals"]]

    png(path_to_plot)
    par(mfrow = c(2, 2))
    plot(residuals)
    qqnorm(residuals)
    qqline(residuals)
    acf(residuals)
    pacf(residuals)
    dev.off()

    trend_test <- kpss.test(residuals, null = "Trend")
    independence_test <- Box.test(residuals, lag = 15, type = "Ljung-Box")
    normality_test <- shapiro.test(residuals)
    testes_df <- data.frame(
        "Teste" = c("KPSS", "Ljung-Box", "Shapiro-Wilk"), 
        "Estatística do Teste" = c(trend_test[["statistic"]], independence_test[["statistic"]], normality_test[["statistic"]]),
        "p-valor" = c(trend_test[["p.value"]], independence_test[["p.value"]], normality_test[["p.value"]])
    )
    return(testes_df)
}
