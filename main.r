if (!require("pacman")) {
    install.packages("pacman")
}
pacman::p_load("forecast", "Mcomp", "tseries", "ggplot2", "ggpubr")
source("auxiliar_functions.r", encoding = "UTF-8")

LAG_MAP <- c("MONTHLY" = 12)

ids <- c(2119, 2539)
id_escolhido <- ids[2]
serie_escolhida <- M3[[id_escolhido]]
serie_escolhida %>% plot()
treinamento <- serie_escolhida$x
teste <- serie_escolhida$xx
lag <- LAG_MAP[[serie_escolhida$period]]
horizonte <- serie_escolhida$h

# Item a)
mstl(treinamento) %>% plot()

# Item b)
kpss.test(treinamento, null = "Trend")
out_diff_trein <- treinamento %>% diff_series(lag = lag)
c(out_diff_trein$diff_simples, out_diff_trein$diff_sasonal)
treinamento_estacionario <- out_diff_trein$ts

treinamento_estacionario %>%
    autoplot() +
    geom_line(linewidth = 1) +
    theme_bw()

par(mfrow = c(1, 2))
acf(treinamento_estacionario)
pacf(treinamento_estacionario)

box_cox(treinamento,
    lambda = seq(-3, 3, 1)[-5],
    path_to_plot = "assets/box_cox_escolha.png"
)
treinamento_transformado <- box_cox__(treinamento, lambda = 0)
treinamento_transformado %>% kpss.test(treinamento, null = "Trend")
out_diff_trein_trans <- treinamento_transformado %>% diff_series(lag = lag)
c(out_diff_trein_trans$diff_simples, out_diff_trein_trans$diff_sasonal)
trein_trans_estac <- out_diff_trein_trans$ts

par(mfrow = c(1, 2))
acf(trein_trans_estac)
pacf(trein_trans_estac)

# A FAZER: Escolher os modelos baseados nos gráficos ou rodar loops para valores pequenos de p, q, P, Q

# Item c)
ets_models <- c("AAA", "MAA", "MAM")
model_selection_ets(ets_models, treinamento) %>%
    format_tab("", format = "latex", digits = 3)
ets_mod <- ets(treinamento, model = "MAM")
ets_mod_boxcox <- ets(treinamento, model = "AAA", lambda = -.24)
min(box_cox__(treinamento, lambda = -0.24))
# Item d)

residuals_analysis(ets_mod, "assets/ets_mod.png") %>% format_tab("", digits = 3, "latex")
residuals_analysis(ets_mod_boxcox, "assets/ets_mod_boxcox.png") %>% format_tab("", digits = 3, "latex")
# A FAZER: Incluir os modelos ARIMA

# Item e)

# item f)

df_teste_plot <- data.frame(
    "x" = as.vector(time(teste)),
    "y" = as.vector(teste)
)
p3 <- forecast(ets_mod, h = horizonte, level = 95, bootstrap = TRUE) %>%
    prediction_plot() +
    geom_line(
        data = df_teste_plot, aes(x = `x`, y = `y`),
        color = "red", linewidth = 0.5
    ) +
    scale_y_continuous(breaks = seq(4000, 16000, 4000)) +
    coord_cartesian(ylim = c(3800, 16000))

p4 <- forecast(ets_mod_boxcox, h = horizonte, level = 95) %>%
    prediction_plot() +
    geom_line(
        data = df_teste_plot, aes(x = `x`, y = `y`),
        color = "red", linewidth = 0.5
    )

ggarrange(p3, p4) %>%
    ggsave(filename = "assets/predictions.png", .)


# item g)
arima_mod <- Arima(
    y = treinamento, order = c(1, 1, 3), seasonal = c(1, 0, 1)
)
arima_mod_boxcox <- Arima(
    y = treinamento, order = c(1, 1, 2), seasonal = c(0, 0, 2), lambda = -0.24
)
models <- list(
    "ARIMA (manual)" = arima_mod,
    "ARIMA (manual)(Box-Cox)" = arima_mod_boxcox,
    "ETS (manual)" = ets_mod,
    "ETS (manual)(Box-Cox)" = ets_mod_boxcox,
    "auto.arima" = auto.arima,
    "ses" = ses, "holt" = holt,
    "ets" = ets, "stlf" = stlf,
    "bats" = bats, "tbats" = tbats
)
MAEs <- map2_dbl(models, names(models), ~ MAE(
    model = .x, train = treinamento, test = teste, h = horizonte, name = .y
))
data.frame("Implementação" = names(MAEs), "MAE" = unname(MAEs)) %>%
    format_tab(
        glue("Erro médio absoluto para as predições de {horizonte} passos a frente."),
        digits = 2, format = "latex"
    )