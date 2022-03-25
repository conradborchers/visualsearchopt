library(targets)
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(tidyr)
library(tibble)
library(stringr)
library(latex2exp)
library(sjPlot)
library(forcats)

# H1

# Use treatment contrasts to compare each group to the first group.
options(contrasts = c("contr.treatment","contr.poly"))  # The default

d <- tar_read(H1)

m <- lmer(mfd.log ~ (sem|id) + (1|cohort:id) + tsplit, data = d,
          control = lmerControl(optimizer ="Nelder_Mead"))

summary(m)

tab_model(
  m,
  pred.labels = c('Intercept', 'Late Trial Stage'),
  dv.labels = c('Log Mean Fixation Duration'),
  string.ci = "95% CI",
  #show.aic = TRUE,
  file = './tables/h1_mfd_log.html'
) 

car::Anova(m)

ref <- summary(m)$coefficients
ref

ref[1,1] %>% exp()
ref[2,1] %>% exp()

m <- glmer(nfix ~ (sem|id) + (1|cohort:id) + tsplit, data = d, family = poisson)

tab_model(
  m,
  pred.labels = c('Intercept', 'Late Trial Stage'),
  dv.labels = c('N Fixations (Poisson Link)'),
  string.ci = "95% CI",
  #show.aic = TRUE,
  file = './tables/h1_nfix_log.html'
) 

saveRDS(summary(m), "./figures/nfix-model.rds")

summary(m)

ref <- summary(m)$coefficients
ref

ref[1,1] %>% exp()
ref[2,1] %>% exp()

# H2

d <- tar_read(H2)

## One Interaction

m1 <- glmer(bef ~ (sem|id) + (1|cohort:id) + (1|bildaoi) + apd*tsplit, data = d, family = binomial, nAGQ = 0)

tab_model(
  m1,
  pred.labels = c('Intercept', 'Avg Pupil Diameter', 'Late Trial Stage', 'Average Pupil Diameter x Late Trial Stage'),
  dv.labels = c('Correct Anomaly Marking (Binomial Link)'),
  string.ci = "95% CI",
  #show.aic = TRUE,
  file = './tables/h2_main_simple_model.html'
) 

saveRDS(summary(m1), "./figures/bef-simple-model.rds")

## Two Interactions

m2 <- glmer(bef ~ (sem|id) + (1|cohort:id) + (1|bildaoi) + apd*tsplit + mfd*tsplit, 
            data = d, family = binomial, nAGQ=0)

tab_model(
  m2,
  pred.labels = c('Intercept', 'Avg Pupil Diameter', 'Late Trial Stage', 'Mean Fixation Duration', 'Average Pupil Diameter x Late Trial Stage', 'Mean Fixation Duration x Late Trial Stage'),
  dv.labels = c('Correct Anomaly Marking (Binomial Link)'),
  string.ci = "95% CI",
  #show.aic = TRUE,
  file = './tables/h2_main_complex_model.html'
) 

saveRDS(summary(m2), "./figures/bef-complex-model.rds")

## LRT Model Comparison

saveRDS(anova(m1, m2), "./figures/bef-model-lrt.rds")

## POST HOC MODELS ###

source("./R/functions_anon.R")

d <- tar_read(d_timeslice_3)
d %>% 
  filter(str_detect(stimulus, "B0[3,4,5,7,8]$")) %>% 
  aggregate_all_trial_master() %>% 
  clean_and_fill_B() %>% 
  clean_final_B() -> dd

m <- lmer(log(mfd) ~ (sem|id) + (1|cohort:id) + tsplit, data = dd,
          control = lmerControl(optimizer ="Nelder_Mead"))

tab_model(
  m,
  pred.labels = c('Intercept', 'Center Trial Stage', 'Late Trial Stage'),
  dv.labels = c('Log Mean Fixation Duration'),
  string.ci = "95% CI",
  #show.aic = TRUE,
  file = './tables/h1_mfd_log_posthoc.html'
) 

car::Anova(m)

ref <- summary(m)$coefficients
ref

ref[1,1] %>% exp()
ref[2,1] %>% exp()

saveRDS(list(summary(m), car::Anova(m)), "./figures/mfd-model-posthoc.rds")

m <- glmer(nfix ~ (sem|id) + (1|cohort:id) + tsplit, data = dd, family = poisson)

tab_model(
  m,
  pred.labels = c('Intercept', 'Center Trial Stage', 'Late Trial Stage'),
  dv.labels = c('N Fixations (Poisson Link)'),
  #string.est = '$\\beta$', # does not work
  string.ci = "95% CI",
  #show.aic = TRUE,
  file = './tables/h1_nfix_log_posthoc.html'
) 

saveRDS(summary(m), "./figures/nfix-model-posthoc.rds")

# Diagnostic performance

# Use treatment contrasts to compare each group to the first group.
options(contrasts = c("contr.treatment","contr.poly"))

d <- tar_read(clean_bef_3)
d %>% 
  filter(str_detect(bildaoi, "B0[3,4,5,7,8]")) -> d2

m1 <- glmer(bef ~ (sem|id) + (1|cohort:id) + (1|bildaoi) + apd*tsplit, data = d2, family = binomial, nAGQ = 0)

tab_model(
  m1,
  pred.labels = c('Intercept', 'Avg Pupil Diameter', 'Center Trial Stage', 'Late Trial Stage', 'Average Pupil Diameter x Center Trial Stage', 'Average Pupil Diameter x Late Trial Stage'),
  dv.labels = c('Correct Anomaly Marking (Binomial Link)'),
  string.ci = "95% CI",
  #show.aic = TRUE,
  file = './tables/h2_main_simple_model_posthoc.html'
) 

summary(m1)

saveRDS(summary(m1), "./figures/bef-simple-model-posthoc.rds")

m2 <- glmer(bef ~ (sem|id) + (1|cohort:id) + (1|bildaoi) + apd*tsplit + mfd*tsplit, 
            data = d2, family = binomial, nAGQ=0)

tab_model(
  m2,
  pred.labels = c('Intercept', 'Avg Pupil Diameter', 'Center Trial Stage', 'Late Trial Stage', 'Mean Fixation Duration', 'Average Pupil Diameter x Center Trial Stage', 'Average Pupil Diameter x Late Trial Stage', 'Mean Fixation Duration x Center Trial Stage', 'Mean Fixation Duration x Late Trial Stage'),
  dv.labels = c('Correct Anomaly Marking (Binomial Link)'),
  string.ci = "95% CI",
  #show.aic = TRUE,
  file = './tables/h2_main_complex_model_posthoc.html'
) 

saveRDS(summary(m2), "./figures/bef-complex-model-posthoc.rds")

saveRDS(anova(m1, m2), "./figures/bef-model-lrt-posthoc.rds")

# Time Plot Fixation Measures 

d <- tar_read(plot_gla)

pd <- d %>%
  group_by(tsplit) %>%
  summarise(
    apd=mean(apd, na.rm=T),
    mfd=mean(mfd, na.rm=T),
    nfix=mean(nfix, na.rm=T),
    fps=mean(fps, na.rm=T)
  ) 

p1 <- pd %>% 
  ggplot(aes(tsplit, nfix, group=1)) +
  geom_point(size=4) + geom_line(size=2) + theme_bw() +
  xlab("Time Slice (9 x 10 s)") + ylab("N Fixations") +
  theme(text=element_text(size=24)) +
  ylim(c(0, 30))

p2 <- pd %>% 
  ggplot(aes(tsplit, mfd, group=1)) +
  geom_point(size=4) + geom_line(size=2) + theme_bw() +
  xlab("Time Slice (9 x 10 s)") + ylab("MFD in Seconds") +
  theme(text=element_text(size=24)) +
  ylim(c(0.35, 1))

p3 <- pd %>% 
  ggplot(aes(tsplit, fps, group=1)) +
  geom_point(size=4) + geom_line(size=2) + theme_bw() +
  xlab("Time Slice (9 x 10 s)") + ylab("Fixations / Second") +
  theme(text=element_text(size=24)) +
  ylim(c(0, 3))

p4 <- pd %>% 
  ggplot(aes(tsplit, apd, group=1)) +
  geom_point(size=4) + geom_line(size=2) + theme_bw() +
  xlab("Time Slice (9 x 10 s)") + ylab(TeX("Standardized $\\Delta$APD")) +
  theme(text=element_text(size=24)) +
  ylim(c(-0.25, 0.25))

pdf("./figures/fig1-initial-4by4.pdf", width=14, height=14)
gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)
dev.off()

# Decay

## Creation

d <- tar_read(plot_decay)

options(contrasts=c('contr.sum','contr.poly'))  # effect coding

d$trial <- factor(d$trial)
m <- glmer(bef ~ (sem|id) + (1|cohort:id) + (1|bildaoi) + trial, data = d, family = binomial, nAGQ = 0)
saveRDS(list(summary(m), m@beta), "./figures/trial-posthoc-fit.rds")

d <- readRDS("./figures/trial-posthoc-fit.rds")

pd <- data.frame(trial=1:10, beta=d[[2]], odds=d[[2]] %>% exp()) 

pdf("./figures/fig2-OPT-diff.pdf", width=14)
pd %>% 
  ggplot(aes(trial, beta, label=round(odds, 2))) +
  geom_bar(stat='identity') +
  geom_text(size = 8, position = position_stack(vjust = 0.5), color="white") +
  coord_flip() + 
  theme_bw() + 
  theme(text=element_text(size=24)) +
  scale_x_continuous(breaks=1:10, trans='reverse') + 
  xlab("Trial/OPT") + ylab(TeX("$\\beta$ (OPT Easiness)"))
dev.off()

# Repeated Model Tests for 3 Trial Stages and for difficult OPTs

options(contrasts = c("contr.treatment","contr.poly"))  # Dummy Coding

# 4x4 OPT split plot

d <- tar_read(clean_fix_9)

d$hard <- ifelse(d$trial %in% c(3,4,5,7,8), 1, 0)

d %>%
  mutate(fps=nfix/10) %>% 
  select(id, sem, tsplit, apd, mfd, nfix, fps, hard) %>% 
  unite(session, id, sem) %>% 
  group_by(tsplit, hard) %>%
  summarise(
    apd=mean(apd, na.rm=T),
    mfd=mean(mfd, na.rm=T),
    nfix=mean(nfix, na.rm=T),
    fps=mean(fps, na.rm=T)
  ) %>% 
  ungroup() %>% 
  arrange(hard) -> pd

pd %>% 
  ggplot(aes(tsplit, nfix, color=factor(hard))) +
  geom_line(aes(group=paste0(hard)), size=2, show.legend = F) +
  geom_point(size=4) + theme_bw() +
  xlab("Time Slice (9 x 10 s)") + ylab("N Fixations") +
  ylim(c(0, 30)) + 
  scale_color_manual(labels = c("0"="Easy", "1"="Hard"), values = c("0"="blue", "1"="red")) + 
  theme(legend.position="top") +
  theme(text=element_text(size=12)) + 
  labs(color="OPT") +
  theme(text=element_text(size=24)) +
  guides(color = guide_legend(override.aes = list(size=7))) -> p1  

pd %>% 
  ggplot(aes(tsplit, mfd, color=factor(hard))) +
  geom_line(aes(group=paste0(hard)), size=2, show.legend = F) +
  geom_point(size=4) + theme_bw() +
  xlab("Time Slice (9 x 10 s)") + ylab("MFD in Seconds") +
  ylim(c(0.35, 1)) + 
  scale_color_manual(labels = c("0"="Easy", "1"="Hard"), values = c("0"="blue", "1"="red")) + 
  theme(legend.position="top") +
  theme(text=element_text(size=12)) + 
  labs(color="OPT") +
  theme(text=element_text(size=24)) +
  guides(color = guide_legend(override.aes = list(size=7))) -> p2

pd %>% 
  ggplot(aes(tsplit, fps, color=factor(hard))) +
  geom_line(aes(group=paste0(hard)), size=2, show.legend = F) +
  geom_point(size=4) + theme_bw() +
  xlab("Time Slice (9 x 10 s)") + ylab("Fixations / Second") +
  ylim(c(0, 3)) + 
  scale_color_manual(labels = c("0"="Easy", "1"="Hard"), values = c("0"="blue", "1"="red")) + 
  theme(legend.position="top") +
  theme(text=element_text(size=12)) + 
  labs(color="OPT") +
  theme(text=element_text(size=24)) +
  guides(color = guide_legend(override.aes = list(size=7))) -> p3

pd %>% 
  ggplot(aes(tsplit, apd, color=factor(hard))) +
  geom_line(aes(group=paste0(hard)), size=2, show.legend = F) +
  geom_point(size=4) + theme_bw() +
  xlab("Time Slice (9 x 10 s)") + ylab(TeX("Standardized $\\Delta$APD")) +
  ylim(c(-0.25, 0.25)) + 
  scale_color_manual(labels = c("0"="Easy", "1"="Hard"), values = c("0"="blue", "1"="red")) + 
  theme(legend.position="top") +
  theme(text=element_text(size=12)) + 
  labs(color="OPT") +
  theme(text=element_text(size=24)) +
  guides(color = guide_legend(override.aes = list(size=7))) -> p4

pdf("./figures/fig3-splitted-4by4.pdf", width=14, height=14)
gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)
dev.off()

# AOI count per difficulty level 

d <- targets::tar_read(H2)

d %>%
  distinct(bildaoi) %>%
  separate(bildaoi, into=c("bild","aoi"), sep="A") %>%
  group_by(bild) %>%
  summarise(n_aoi=n()) %>%
  ungroup() %>%
  mutate(hard=ifelse(str_detect(bild,"03|04|05|07|08"),1,0)) %>%
  group_by(hard) %>%
  summarise(n_aoi=mean(n_aoi))
