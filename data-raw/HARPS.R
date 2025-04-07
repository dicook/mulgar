library(tidyverse)
library(Rtsne)

harps_results <- read_csv("data/HARPS_GTO_processed_with_cluster_label.csv")
#harps <- read_csv("data/HARPS_GTO_data_new.csv")
#harps <- harps %>%
#  mutate(cluster = harps_results$cluster)
#harps %>% count(pop)

# This is a bit awkward
selected_columns <- colnames(harps_results)[grepl("Mg|Al|Si|Ca|TiI|fe|Cu|Zn|Sr|Y|ZrII|Ce|Ba",colnames(harps_results))]
selected_columns_new <- selected_columns[! selected_columns %in% c('Y_tsne_teffcut40', 'Y_tsne_teffcut40_old', 'Y_tsne_teffcut40_errlim_mc', "Y_tsne_teffcut40_errlim_mc_old", "Y_tsne_teffcut40_nofeh_mc", "Y_tsne_teffcut40_mc", "Yg_sig", "vYg", "vYg_sig", "[Fe/H]_C11", "Yg", "nCu", "nZn" , "nSr", "nY", "nZrII", "nBa", "nCe", "nAl", "nMg", "nSi", "nCa", "[Fe/H]_T13")]
selected_columns_new <- selected_columns_new %>% append("cluster")
selected_columns_new <- selected_columns_new[selected_columns_new %in% c("feh", "AlMg", "TiIFe", "SiFe", "MgFe", "ZnFe", "CuFe", "YMg", "BaFe", "CeFe", "ZrBa", "SrBa", "cluster", "ZrIIFe", "CaFe", "AlFe", "YFe", "ZrFe", "SrFe")] ###Some what correct
harps <- harps_results %>% select(all_of(selected_columns_new))
summary(harps)

set.seed(90)
harps_fit20 <- harps %>%
  select(where(is.numeric)) %>%
  Rtsne(perplexity = 20)
harps_fit30 <- harps %>%
  select(where(is.numeric)) %>%
  Rtsne(perplexity = 30)
harps_fit40 <- harps %>%
  select(where(is.numeric)) %>%
  Rtsne(perplexity = 40)
harps_fit <- harps_fit20
harps_fit <- harps_fit30
harps_fit <- harps_fit40
harps_tsne <- as_tibble(harps_fit$Y) %>%
  rename(tsne1 = V1, tsne2 = V2) %>%
  mutate(cluster = harps$cluster)

# Match colours, approx
# unique(harps_tsne$cluster)

ggplot(harps_tsne, aes(x=tsne1, y=tsne2,
                       colour = cluster,
                       shape = cluster,
                       fill = cluster)) +
  geom_point(size=3, alpha=0.8) +
  scale_shape_manual(values=c("Thin Disc"=16,
                              "Thick Disc I"=24,
                              "Thick Disc II"=25,
                              "Outer Thin Disc"=22,
                              "Inner Disc I"=24,
                              "Inner Disc II"=25,
                              "Inner Disc III"=24,
                              "Transition group"=16,
                              "Young local disc"=16,
                              "Debris candidate"=18,
                              "[s/Fe]-enhanced"=18,
                              "Extreme-Ti star"=16,
                              "Low-[Mg/Fe] star"=22,
                              "High-[Al/Mg] star"=16)) +
  scale_colour_manual(values=c("Thin Disc"="grey30",
                               "Thick Disc I"="hotpink",
                               "Thick Disc II"="purple",
                               "Outer Thin Disc"="forestgreen",
                               "Inner Disc I"="red1",
                               "Inner Disc II"="red3",
                               "Inner Disc III"="gold",
                               "Transition group"="springgreen3",
                               "Young local disc"="grey",
                               "Debris candidate"="maroon",
                               "[s/Fe]-enhanced"="burlywood",
                               "Extreme-Ti star"="green",
                               "Low-[Mg/Fe] star"="navy",
                               "High-[Al/Mg] star"="royalblue")) +
  scale_fill_manual(values=c("Thin Disc"="grey30",
                               "Thick Disc I"="hotpink",
                               "Thick Disc II"="purple",
                               "Outer Thin Disc"="forestgreen",
                               "Inner Disc I"="red1",
                               "Inner Disc II"="red3",
                               "Inner Disc III"="gold",
                               "Transition group"="springgreen3",
                               "Young local disc"="grey",
                               "Debris candidate"="maroon",
                               "[s/Fe]-enhanced"="burlywood",
                               "Extreme-Ti star"="green",
                               "Low-[Mg/Fe] star"="navy",
                               "High-[Al/Mg] star"="royalblue")) +
  theme_bw() + theme(aspect.ratio=1)

library(liminal)
limn_tour_link(
  tour_data = harps,
  embed_data = harps_tsne[,1:2],
  cols = feh:ZrBa,
  color = cluster,
  morph = "center"
)

# Using a tour
clrs <- case_when(
  harps$cluster == "Thin Disc" ~ "grey30",
  harps$cluster == "Thick Disc I" ~ "hotpink",
  harps$cluster == "Thick Disc II" ~ "purple",
  harps$cluster == "Outer Thin Disc" ~ "forestgreen",
  harps$cluster == "Inner Disc I" ~ "red1",
  harps$cluster == "Inner Disc II" ~ "red3",
  harps$cluster == "Inner Disc III" ~ "gold",
  harps$cluster == "Transition group" ~ "springgreen3",
  harps$cluster == "Young local disc" ~ "grey",
  harps$cluster == "Debris candidate" ~ "maroon",
  harps$cluster == "[s/Fe]-enhanced" ~ "burlywood",
  harps$cluster == "Extreme-Ti star" ~ "green",
  harps$cluster == "Low-[Mg/Fe] star" ~ "navy",
  harps$cluster == "High-[Al/Mg] star" ~ "royalblue")
shp <- case_when(
  harps$cluster == "Thin Disc" ~ 16,
  harps$cluster == "Thick Disc I" ~ 24,
  harps$cluster == "Thick Disc II" ~ 25,
  harps$cluster == "Outer Thin Disc" ~ 22,
  harps$cluster == "Inner Disc I" ~ 24,
  harps$cluster == "Inner Disc II" ~ 25,
  harps$cluster == "Inner Disc III" ~ 24,
  harps$cluster == "Transition group" ~ 16,
  harps$cluster == "Young local disc" ~ 16,
  harps$cluster == "Debris candidate" ~ 18,
  harps$cluster == "[s/Fe]-enhanced" ~ 18,
  harps$cluster == "Extreme-Ti star" ~ 16,
  harps$cluster == "Low-[Mg/Fe] star" ~ 22,
  harps$cluster == "High-[Al/Mg] star" ~ 16)


library(tourr)
animate_xy(harps[,1:17], col = clrs, pch = shp,
           half_range=1)
animate_sage(harps[,1:17], col = clrs, pch = shp,
           half_range=1, R=1)
harps_sub <- harps %>%
  filter(cluster %in% c("Thin Disc", "Thick Disc I", "Thick Disc II"))
clrs_sub <- clrs[harps$cluster %in% c("Thin Disc", "Thick Disc I", "Thick Disc II")]
shp_sub <- shp[harps$cluster %in% c("Thin Disc", "Thick Disc I", "Thick Disc II")]
proj <- animate_xy(harps_sub[,1:17],
           tour_path=guided_tour(lda_pp(harps_sub$cluster)),
           col = clrs_sub,
           pch = shp_sub,
           half_range=0.6,
           rescale = FALSE)
best_proj <- proj$basis[[1058]]
best_proj_val <- proj$index_val[1058]

harps_best_proj <- as.matrix(harps_sub[,1:17]) %*%
  best_proj
colnames(harps_best_proj) <- c("PP1", "PP2")
harps_best_proj <- as_tibble(harps_best_proj)
harps_best_proj$cluster <- harps_sub$cluster
ggplot(harps_best_proj) +
  geom_point(aes(x=PP1, y=PP2,
                 colour=cluster,
                 shape=cluster,
                 fill=cluster), size=3, alpha=0.8) +
  scale_shape_manual(values=c("Thin Disc"=16,
                              "Thick Disc I"=24,
                              "Thick Disc II"=25)) +
  scale_colour_manual(values=c("Thin Disc"="grey30",
                               "Thick Disc I"="hotpink",
                               "Thick Disc II"="purple")) +
  scale_fill_manual(values=c("Thin Disc"="grey30",
                             "Thick Disc I"="hotpink",
                             "Thick Disc II"="purple")) +
  #xlim(c(-0.3, 0.5)) +
  #ylim(c(-0.3, 0.6)) +
  theme_bw() + theme(aspect.ratio=1)
animate_xy(harps_sub[,1:17],
           tour_path=radial_tour(best_proj, mvar=c(10,13)),
           col = clrs_sub,
           pch = shp_sub,
           half_range=0.6,
           rescale = FALSE)
ggplot(harps_sub) +
  geom_point(aes(x=MgFe, y=TiIFe,
                 colour=cluster,
                 shape=cluster,
                 fill=cluster), size=3, alpha=0.8) +
  scale_shape_manual(values=c("Thin Disc"=16,
                              "Thick Disc I"=24,
                              "Thick Disc II"=25)) +
  scale_colour_manual(values=c("Thin Disc"="grey30",
                               "Thick Disc I"="hotpink",
                               "Thick Disc II"="purple")) +
  scale_fill_manual(values=c("Thin Disc"="grey30",
                             "Thick Disc I"="hotpink",
                             "Thick Disc II"="purple")) +
  #xlim(c(-0.3, 0.5)) +
  #ylim(c(-0.3, 0.6)) +
  theme_bw() + theme(aspect.ratio=1)

ggplot(harps_sub) +
  geom_point(aes(x=ZnFe, y=CaFe,
                 colour=cluster,
                 shape=cluster,
                 fill=cluster), size=3, alpha=0.8) +
  scale_shape_manual(values=c("Thin Disc"=16,
                              "Thick Disc I"=24,
                              "Thick Disc II"=25)) +
  scale_colour_manual(values=c("Thin Disc"="grey30",
                               "Thick Disc I"="hotpink",
                               "Thick Disc II"="purple")) +
  scale_fill_manual(values=c("Thin Disc"="grey30",
                             "Thick Disc I"="hotpink",
                             "Thick Disc II"="purple")) +
  #xlim(c(-0.3, 0.5)) +
  #ylim(c(-0.3, 0.6)) +
  theme_bw() + theme(aspect.ratio=1)
animate_xy(harps_sub[,c(3, 10, 12, 13)], tour_path=guided_tour(lda_pp(harps_sub$cluster)),
           col = clrs_sub, pch = shp_sub,
           half_range=1)

render_gif(harps_sub[,1:17],
           radial_tour(best_proj, mvar=c(10,13)),
           display_xy(col = clrs_sub, pch = shp_sub,
                      half_range=1),
           gif_file = "harps_manual.gif")

