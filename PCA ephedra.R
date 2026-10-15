## 7. PCA (post hoc) - computation, scree plot (A) and loading plot (B) ----
pca_df <- data.frame("Y%" = Ephedra$Yield, "TPC" = Ephedra$TPC, "RSA%" = Ephedra$RSA,
                     check.names = FALSE)
rownames(pca_df) <- Ephedra$Run                 # original run numbers label the dots
pca <- prcomp(pca_df, center = TRUE, scale. = TRUE)

## PCA signs are arbitrary; flip them so the plot looks like your Figure 5
## (TPC/RSA% pointing up-left, Y% pointing down-left).
f1 <- if (pca$rotation["TPC", 1] > 0) -1 else 1
f2 <- if (pca$rotation["Y%", 2] > 0) -1 else 1
pca$rotation[, 1] <- f1 * pca$rotation[, 1];  pca$x[, 1] <- f1 * pca$x[, 1]
pca$rotation[, 2] <- f2 * pca$rotation[, 2];  pca$x[, 2] <- f2 * pca$x[, 2]

print(pca)
print(summary(pca))
eig.val <- get_eigenvalue(pca)
print(eig.val)

## A. scree plot
p_scree <- fviz_eig(pca, addlabels = TRUE, ylim = c(0, 100)) +
  labs(title = NULL, tag = "A")

## B. loading plot (cos2 colour scale)
p_var <- fviz_pca_var(pca, col.var = "cos2",
                      gradient.cols = c("red", "blue", "green"), repel = TRUE) +
  labs(title = NULL, tag = "B")
print(p_scree); print(p_var)

## cos2 of the variables (extra figure)
pca_var <- get_pca_var(pca)
png("PCA_corrplot.png", width = 1400, height = 1000, res = 250)
corrplot(pca_var$cos2, is.corr = FALSE)
dev.off()
corrplot(pca_var$cos2, is.corr = FALSE)

## 8. BIPLOT (C) ----------------------------
p_bi_plain <- fviz_pca_biplot(pca, repel = TRUE, col.var = "steelblue", col.ind = "red",
                              labelsize = 5) +
  labs(title = NULL, tag = "C")
print(p_bi_plain)
ggplot2::ggsave("Fig_PCA_biplot_plain.png", p_bi_plain, width = 9, height = 6, dpi = 300)

panel_plain <- gridExtra::arrangeGrob(p_scree, p_var, p_bi_plain,
                                      layout_matrix = rbind(c(1, 2), c(3, 3)))
ggplot2::ggsave("Fig_PCA_panel_plain.png", panel_plain, width = 10, height = 10, dpi = 300)

##OPTIIONAL
## 9. BIPLOT (C) - WITH THE BEST-RUN AREAS (convex hulls) ---------------------
## The n_top best runs of each response are outlined with their convex hull
## (blue = highest Y%, yellow = highest TPC, red = highest RSA%).
n_top  <- 5                                       # number of best runs per response
scores <- as.data.frame(pca$x[, 1:2]); names(scores) <- c("PC1", "PC2")
hull_of <- function(values) {
  idx <- order(values, decreasing = TRUE)[1:n_top]
  pts <- scores[idx, ]
  pts[chull(pts$PC1, pts$PC2), ]
}
p_bi_hull <- p_bi_plain +
  geom_polygon(data = hull_of(Ephedra$Yield), aes(x = PC1, y = PC2),
               fill = "steelblue", colour = "black", alpha = 0.30, inherit.aes = FALSE) +
  geom_polygon(data = hull_of(Ephedra$TPC), aes(x = PC1, y = PC2),
               fill = "yellow", colour = "black", alpha = 0.30, inherit.aes = FALSE) +
  geom_polygon(data = hull_of(Ephedra$RSA), aes(x = PC1, y = PC2),
               fill = "red", colour = "black", alpha = 0.25, inherit.aes = FALSE)
print(p_bi_hull)
ggplot2::ggsave("Fig_PCA_biplot_hull.png", p_bi_hull, width = 9, height = 6, dpi = 300)

panel_hull <- gridExtra::arrangeGrob(p_scree, p_var, p_bi_hull,
                                     layout_matrix = rbind(c(1, 2), c(3, 3)))
ggplot2::ggsave("Fig_PCA_panel_hull.png", panel_hull, width = 10, height = 10, dpi = 300)

cat("\nDone. Figures were saved in:", getwd(), "\n")



