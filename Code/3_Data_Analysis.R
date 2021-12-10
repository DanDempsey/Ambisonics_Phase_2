##### Xperi Data Analysis
##### Daniel Dempsey

### Load libraries
library(readr)
library(dplyr)
library(lme4)
library(xtable)
library(emmeans)
library(lmerTest)
library(ggplot2)

### Load data
xp_dat <- read_csv('Data/new_dat.csv', col_types = 'cffdd') %>%
  filter( !(stimulus %in% c("Anchor", "Reference")) ) %>%
  filter( !(test == 'test_13') ) # test 13 is excluded due to too many values close to boundary

setwd( 'Output/rmANOVA' )

### Set section titles for future plots
section_titles <- paste0( 'Section ', 1:7, ':\n', 
                          c('Localization Accuracy - single static source',
                            'Localization Accuracy - multiple static sources',
                            'Locatedness', 'Timbre', 'Spatial Impression/Envelopment',
                            'Naturalness/Presence',
                            'Localization Accuracy - single source, off-centre listener position') )


### ANOVA by sections
test_type_split <- split( xp_dat, xp_dat$section )

mm <- Map(lmer, data = test_type_split, MoreArgs = list(formula = result ~ stimulus + (1|subject)))
aov <- lapply(mm, anova)
p_vals <- sapply( aov, function(x) { x$'Pr(>F)' } )
#xtable( p_vals, digits = 3 )
### We're setting significance level at 0.01
### All were significant except for 4 and 5

# QQ-plot of residuals
for (i in seq_along(mm)) {
  pdf(paste0("1_One_Way/QQ_1w_", names(mm)[i], ".pdf"))
  res <- resid(mm[[i]])
  res_std <- ( res - mean(res) ) / sd(res)
  title_index <- as.numeric(names(mm)[i])
  qqnorm(res_std, pch = 20, col = "blue",
         main = section_titles[title_index])
  grid()
  abline(0, 1, lty = 2, col = "red")
  dev.off()
}

# Pairwise tests
em <- lapply(mm, emmeans, specs = list(pairwise ~ stimulus))

# Plot EMM CI's
for (i in seq_along(em)) {
  title_index <- as.numeric(names(em)[i])
  plot(em[[i]]) + ggtitle(section_titles[title_index]) + 
    scale_x_continuous(breaks=seq(0, 100, 20), limits = c(0, 100)) +
    xlab("Score") + ylab("Decoder")
  ggsave(paste0("1_One_Way/CI_1w_", names(em[i]), ".pdf"))
}

# Plot pairwise comparisons
for (i in seq_along(em)) {
  title_index <- as.numeric(names(em)[i])
  pwpp(em[[i]]$`emmeans of stimulus`, plim = 0, ylab = 'Decoder') + ggtitle(section_titles[title_index]) +
    #geom_vline(xintercept = 0.05, linetype="dashed", color = "blue") +
    geom_vline(xintercept = 0.01, linetype="dashed", color = "red")
  ggsave(paste0("1_One_Way/PWPP_1w_", names(em[i]), ".pdf"))
}

### Two-way ANOVAs
mm2w <- Map(lmer, data = test_type_split[-1], MoreArgs = list(formula = result ~ stimulus * signal_type + (1|subject)))
aov2w <- lapply(mm2w, anova)
p_vals_2w_list <- lapply( aov2w, function(x) { as.matrix(x['Pr(>F)']) } )
p_vals_2w <- t( do.call( 'cbind', p_vals_2w_list ) )
rownames( p_vals_2w ) <- paste0( 'Section_', c(2, 4:7) )
#xtable( p_vals_2w, digits = 3 )

# QQ-plot of residuals
for (i in seq_along(mm2w)) {
  pdf(paste0("2_Two_Way/QQ_2w_", names(mm)[i], ".pdf"))
  res <- resid(mm2w[[i]])
  res_std <- ( res - mean(res) ) / sd(res)
  title_index <- as.numeric(names(mm2w)[i])
  qqnorm(res_std, pch = 20, col = "blue",
         main = section_titles[title_index])
  grid()
  abline(0, 1, lty = 2, col = "red")
  dev.off()
}

# Pairwise tests
em_2w <- lapply(mm2w, emmeans, specs = list(pairwise ~ stimulus + signal_type))

# Plot EMM CI's
for (i in seq_along(em_2w)) {
  title_index <- as.numeric(names(em_2w)[i])
  plot(em_2w[[i]]) + ggtitle(section_titles[title_index]) + 
    scale_x_continuous(breaks=seq(0, 100, 20), limits = c(0, 100)) +
    xlab("Score") + ylab("Decoder")
  ggsave(paste0("2_Two_Way/CI_2w_", names(em_2w[i]), ".pdf"))
}

# Plot pairwise comparisons
for (i in seq_along(em_2w)) {
  title_index <- as.numeric(names(em_2w)[i])
  pwpp(em_2w[[i]]$`emmeans of stimulus`) + ggtitle(section_titles[title_index]) +
    ylab("Decoder") + 
    geom_vline(xintercept = 0.01, linetype="dashed", color = "red")
  ggsave(paste0("2_Two_Way/PWPP_2w_", names(em_2w[i]), ".pdf"))
}

### Split up Section 1
sec_1_split <- split( test_type_split[[1]], test_type_split[[1]]$source )

mm_sec1 <- Map(lmer, data = sec_1_split, MoreArgs = list(formula = result ~ stimulus * signal_type + (1|subject)))
aov_sec1 <- lapply(mm_sec1, anova)
p_vals_sec1_list <- lapply( aov_sec1, function(x) { as.matrix(x['Pr(>F)']) } )
p_vals_sec1 <- t( do.call( 'cbind', p_vals_sec1_list ) )
rownames( p_vals_sec1 ) <- names( sec_1_split )
p_vals_2w_all <- rbind( p_vals_2w, p_vals_sec1 )[ c(6:7, 1:5), ]
xtable( p_vals_2w_all, digits = 3 )

# QQ-plot of residuals
for (i in seq_along(mm_sec1)) {
  pdf(paste0("Section_1/QQ_sec1_", names(mm)[i], ".pdf"))
  res <- resid(mm_sec1[[i]])
  res_std <- ( res - mean(res) ) / sd(res)
  plot_title <- paste0( section_titles[1], ' - ', names(mm_sec1)[i] )
  qqnorm(res_std, pch = 20, col = "blue",
         main = plot_title)
  grid()
  abline(0, 1, lty = 2, col = "red")
  dev.off()
}

# Pairwise tests
em_sec1 <- lapply(mm_sec1, emmeans, specs = list(pairwise ~ stimulus + signal_type))

# Plot EMM CI's
for (i in seq_along(em_sec1)) {
  plot_title <- paste0( section_titles[1], ' - ', names(mm_sec1)[i] )
  plot(em_sec1[[i]]) + ggtitle(plot_title) + 
    scale_x_continuous(breaks=seq(0, 100, 20), limits = c(0, 100)) +
    xlab("Score") + ylab("Decoder")
  ggsave(paste0("Section_1/CI_sec1_", names(em_sec1[i]), ".pdf"))
}

# Plot pairwise comparisons
for (i in seq_along(em_sec1)) {
  plot_title <- paste0( section_titles[1], ' - ', names(mm_sec1)[i] )
  pwpp(em_sec1[[i]]$`emmeans of stimulus`) + ggtitle(plot_title) +
    ylab("Decoder") + 
    geom_vline(xintercept = 0.01, linetype="dashed", color = "red")
  ggsave(paste0("Section_1/PWPP_sec1_", names(em_sec1[i]), ".pdf"))
}

### Split up Section 2
sec_2_split <- split( test_type_split[[2]], test_type_split[[2]]$signal_type )

mm_sec2 <- Map(lmer, data = sec_2_split, MoreArgs = list(formula = result ~ stimulus + (1|subject)))
aov_sec2 <- lapply(mm_sec2, anova)

p_vals_sec2 <- sapply( aov_sec2, function(x) { x$'Pr(>F)' } )
p_vals_all <- data.frame( p_vals = c( p_vals, p_vals_sec2 )[ c(1:2, 8:7, 3:6) ] )
xtable( p_vals_all, digits = 3 )

# QQ-plot of residuals
for (i in seq_along(mm_sec2)) {
  pdf(paste0("Section_2/QQ_sec2_", names(mm)[i], ".pdf"))
  res <- resid(mm_sec2[[i]])
  res_std <- ( res - mean(res) ) / sd(res)
  plot_title <- paste0( section_titles[2], ' - ', names(mm_sec1)[i] )
  qqnorm(res_std, pch = 20, col = "blue",
         main = plot_title)
  grid()
  abline(0, 1, lty = 2, col = "red")
  dev.off()
}

# Pairwise tests
em_sec2 <- lapply(mm_sec2, emmeans, specs = list(pairwise ~ stimulus))

# Plot EMM CI's
for (i in seq_along(em_sec2)) {
  plot_title <- paste0( section_titles[2], ' - ', names(mm_sec1)[i] )
  plot(em_sec2[[i]]) + ggtitle(plot_title) + 
    scale_x_continuous(breaks=seq(0, 100, 20), limits = c(0, 100)) +
    xlab("Score") + ylab("Decoder")
  ggsave(paste0("Section_2/CI_sec2_", i, ".pdf"))
}

# Plot pairwise comparisons
for (i in seq_along(em_sec2)) {
  plot_title <- paste0( section_titles[2], ' - ', names(mm_sec1)[i] )
  pwpp(em_sec2[[i]]$`emmeans of stimulus`) + ggtitle(plot_title) +
    ylab("Decoder") + 
    geom_vline(xintercept = 0.01, linetype="dashed", color = "red")
  ggsave(paste0("Section_2/PWPP_sec2_", i, ".pdf"))
}

