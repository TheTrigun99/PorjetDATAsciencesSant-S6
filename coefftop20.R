library(multiview)
fit_cort <- readRDS("uuncontretous_cort.rds")
fit_midl <- readRDS("uuncontretous_midl.rds")
fit_dipg <- readRDS("uuncontretous_dipg.rds")

fit_midl$lambda.min
library(ggplot2)

print(coef_ordered(fit_dipg, s = "lambda.1se")[, c("view_col", "coef")])
top20 <- head(coef_ordered(fit_dipg, s = "lambda.1se")[, c("view_col", "coef")], 30)
top20$couleur <- ifelse(top20$view_col == 'A_23_P205428', "rouge", "bleu")
ggplot(top20, aes(x = reorder(view_col, coef), y = coef)) +
  geom_col(aes(fill = couleur)) +
  coord_flip() +scale_fill_manual(values = c("rouge" = "red", "bleu" = "steelblue"))+
  labs(title = "Top variables pour prédire 'dipg'",
       x = "Variable", y = "Coefficient") +
  theme_minimal()+theme(legend.position = "none")
