## Title:     Welfare Retrenchments and Government Support:
##            Evidence from a Natural Experiment
##
##            Erik Gahner Larsen
##            erikgahner@gmail.com
##
## Data:      Publicly available at: http://www.europeansocialsurvey.org/download.html?file=ESS6DK&c=DK&y=2012
##            (free login required)
##            Data for the Danish National Election Study can be ordered at: http://dda.dk/catalogue/27067
##            (required in order to produce Figure A.1)

# Load packages
library("tidyverse")
library("gridExtra")
library("MatchIt")
library("rdrobust")
library("stargazer")
library("RItools")
library("rms")
library("lm.beta")
library("optmatch")

# Set random seed
set.seed(2149573)

# Load data
ess <- read_csv("data-ess.csv")
dnes <- read_csv("data-dnes.csv")

# Figure 1: Coverage
## Create data frame with numbers of articles
coverage <- data.frame(
  week = 1:13,
  su = c(2, 4, 0, 0, 3, 2, 3, 19, 12, 2, 3, 0, 0),
  reform = c(14, 14, 6, 4, 13, 18, 13, 32, 31, 16, 22, 15, 2)
)

## Create figure on media coverage
ggplot(coverage, aes(x = week, y = su)) +
  geom_vline(linetype = "dashed", aes(xintercept = 7.5), colour = "grey40") +
  geom_line() +
  geom_point(size = 6, colour = "white") +
  geom_point(size = 3, shape = 1) +
  ylab("Number of articles") +
  scale_x_continuous("Week number (in 2013)", breaks = 1:14) +
  theme_minimal(base_size = 11.5)

## Save figure as eps
ggsave("fig-coverage.eps", height = 4, width = 6)

# Preprocessing: matching, nearest
ess_m <- ess %>% 
  # Select variables to data frame
  select(stfgov, stflife, stfeco, stfdem, reform, education, edulevel, 
         male, age, society, polinterest, tv, religious, ideology, family, 
         prosp, absdate, difdate, pdwrk, unempl, hswrk) %>% 
  # Remove observations with missing values
  na.omit() %>% 
  # Create Treatment variable
  mutate(Treatment = ifelse(reform == 1, "Treated", "Control"))

## Specifcy treatment function
treat_f <- reform ~ education + male + age + edulevel + society + polinterest + tv + religious + ideology

## Estimate binomial model of the treatment function
fit <- glm(treat_f, family = binomial, data = ess_m)

## Estimate propensity scores
ess_m <- ess_m %>% 
  mutate(pscores = predict(fit, type = "response"))

## Use nearest neighbor matching
ess_matched <- matchit(treat_f, method = "nearest", caliper = .1, data = ess_m)

### Get info on matched and unmatched cases
ess_matched

### Get matched data
ess_matched_data <- match.data(ess_matched)

# Uncomment line to save file (the file was saved with the R session in sessionInfo.txt)
#ess_matched_data %>% write_csv("data-ess_matched.csv")
ess_matched_data <- read_csv("data-ess_matched.csv")

# Create figure 2: Distribution of outcome
ggplot(ess_matched_data, aes(stfgov)) +
  geom_bar(colour = "black", fill = "gray90") +
  ylab("") +
  scale_x_continuous("Government satisfaction", breaks = 0:10) +
  theme_minimal()

## Save as .eps
ggsave("fig-outcome.eps", height = 4, width = 4)

## Get mean and standard deviation
mean(ess_matched_data$stfgov)
sd(ess_matched_data$stfgov)

## Estimate models reported in Table 1
reg_m_1 <- lm(stfgov ~ reform, data = ess_matched_data)
reg_m_2 <- lm(stfgov ~ reform + male + age + edulevel + society + polinterest + tv + religious + ideology, data = ess_matched_data)
reg_m_3 <- lm(stfgov ~ reform * education, data = ess_matched_data)
reg_m_4 <- lm(stfgov ~ reform * education + male + age + edulevel + society + polinterest + tv + religious + ideology, data = ess_matched_data)

## Save Table 1
writeLines(capture.output(
  stargazer(reg_m_1, reg_m_2, reg_m_3, reg_m_4,
            align = TRUE, column.sep.width = "0pt",
            no.space = TRUE, digits = 2, model.numbers = FALSE,
            single.row = TRUE, notes.align = "l",
            covariate.labels = c("Reform", "Male", "Age", "Education level",
                                 "Subjective class", "Pol. interest", "Pol. news", "Religiosity",
                                 "Ideology", "Education", "Reform*education"),
            keep.stat = c("n", "rsq"),
            column.labels = c("Average effect", "Average effect, w. covariates", 
                              "Conditional effect", "Conditional effect, w. covariates"),
            model.names = FALSE,
            type = "html"
  )), "table-1.htm")

# Estimate alernative measures and models
reg_life <- lm(stflife ~ reform * education + male + age + edulevel + society + polinterest + tv + religious + ideology, data = ess_matched_data)
reg_eco <- lm(stfeco ~ reform * education + male + age + edulevel + society + polinterest + tv + religious + ideology, data = ess_matched_data)
reg_dem <- lm(stfdem ~ reform * education + male + age + edulevel + society + polinterest + tv + religious + ideology, data = ess_matched_data)

## Save Table 2
writeLines(capture.output(
  stargazer(reg_life, reg_eco, reg_dem,
            align = TRUE, column.sep.width = "0pt",
            no.space = TRUE, digits = 2, model.numbers = FALSE,
            font.size = "scriptsize", single.row = TRUE,
            notes.align = "l", 
            covariate.labels = c("Reform", "Education", "Male", "Age", "Education level", "Subjective class", 
                                 "Pol. interest", "Pol. news", "Religiosity", "Ideology", "Reform*education"
            ),
            keep.stat = c("n", "rsq"),
            column.labels = c("Life", "Economy", "Democracy"),
            model.names = FALSE,
            type = "html")), "table-2.htm")

# Estimate models using alternative proximity measures
reg_edulevel <- lm(stfgov ~ reform * education + reform * edulevel + male + age + society + polinterest + tv + religious + ideology + education, data = ess_matched_data)
reg_family <- lm(stfgov ~ reform * education + reform * family + male + age + society + polinterest + tv + religious + ideology + education, data = ess_matched_data)
reg_pdwrk <- lm(stfgov ~ reform * education + reform * pdwrk + male + age + society + polinterest + tv + religious + ideology, data = ess_matched_data)
reg_full <- lm(stfgov ~ reform * education + reform * edulevel + reform * family + reform * pdwrk + male + age + society + polinterest + tv + religious + ideology, data = ess_matched_data)

## Save Table 3
writeLines(capture.output(
  stargazer(reg_edulevel, reg_family, reg_pdwrk, reg_full,
            align = TRUE, column.sep.width = "0pt", no.space = TRUE,
            digits = 2, model.numbers = FALSE, font.size = "scriptsize", 
            single.row = TRUE, notes.align = "l",
            covariate.labels = c("Reform", "Education", "Education level",
                                 "Family", "Paid work", "Male",
                                 "Age", "Subjective class", "Pol. interest",
                                 "Pol. news", "Religiosity", "Ideology", "Reform*Education",
                                 "Reform*Education level", "Reform*Family", "Reform*Paid work"
            ),
            keep.stat = c("n", "rsq"),
            column.labels = c("Education level", "Family", "Paid work", "Full model"),
            model.names = FALSE,
            type = "html"
  )), "table-3.htm")

# Supplementary Material
## Supplementary Material A

dnes <- dnes %>% 
  mutate(policy_label = case_when(
    policy == "cashbenefits" ~ "Cash benefits",
    policy == "childcare" ~ "Child care",
    policy == "culture" ~ "Culture",
    policy == "defense" ~ "Defense",
    policy == "education" ~ "Education",
    policy == "environment" ~ "Environment",
    policy == "foreignaid" ~ "Foreign aid",
    policy == "healthcare" ~ "Health care",
    policy == "highways" ~ "Highways",
    policy == "homecare" ~ "Home care",
    policy == "immigration" ~ "Immigration",
    policy == "pension" ~ "Pension",
    policy == "police" ~ "Police",
    policy == "unemployment" ~ "Unemployment",
    TRUE ~ policy
  ))

## Create Figure A.1
pdf('fig-spending.pdf', height = 9, width = 8)
ggplot(dnes, aes(x = spending)) +
  geom_bar(fill = c(rep(c("#E02427", "#FCBE75", "#399F34"), 14))) +
  facet_wrap( ~ policy_label) +
  ylab("") +
  scale_x_continuous(name = "", breaks = 1:3, labels = c("Too much", "Suitable", "Too little")) +
  theme_minimal()
dev.off()

## Supplementary Material B

### Create and save Figure B.1
fig_cov_reform <- ggplot(coverage, aes(x = week, y = reform)) +
  geom_vline(linetype = "dashed", aes(xintercept = 7.5), colour = "grey40") +
  ggtitle("A: Reform coverage") +
  geom_line() +
  geom_point(size = 6, colour = "white") +
  geom_point(size = 3, shape = 1) +
  ylab("Number of articles, reforms") +
  scale_x_continuous("Week", breaks = 1:14) +
  theme_minimal(base_size = 11.5)

fig_cov_rel <- ggplot(coverage, aes(x = week, y = su / reform)) +
  geom_vline(linetype = "dashed", aes(xintercept = 7.5), colour = "grey40") +
  ggtitle("B: Relative coverage") +
  geom_line() +
  geom_point(size = 6, colour = "white") +
  geom_point(size = 3, shape = 1) +
  scale_y_continuous("Relative education reform coverage", breaks = seq(0, 0.6, 0.1), labels = paste0(seq(0, 60, 10), "%")) +
  scale_x_continuous("", breaks = 1:14) +
  theme_minimal(base_size = 11.5)

pdf('fig-coverage-rel.pdf', height = 4, width = 10)
grid.arrange(fig_cov_reform, fig_cov_rel, ncol = 2)
dev.off()


## Supplementary Material C

### Create Figure C.1 with dates of interview
pdf('fig-dates.pdf', height = 4, width = 6)
ggplot(ess, aes(x = as.Date(date))) +
  geom_histogram(fill = "gray90",
                 colour = "black",
                 bins = 25) +
  ylab("Frequency") + xlab("Date of interview") +
  geom_vline(linetype = "dashed",
             aes(xintercept = as.Date("2013-02-19")),
             colour = "grey40") +
  theme_minimal()
dev.off()

## Supplementary Material E
### Save plot for PS for unmatched data
plot_unmatched <- ggplot(ess_m, aes(x = pscores, linetype = Treatment, fill = Treatment, colour = Treatment)) +
  geom_line(stat = "density", size = 1) +
  scale_color_manual(values = c("grey70", "black")) +
  ggtitle("Unmatched") +
  ylab("") +
  theme(legend.position = "bottom", plot.title = element_text(size = 12)) +
  scale_x_continuous("", limits = c(0, 1)) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal')

### Save plot for PS for matched data
plot_matched <- ggplot(ess_matched_data, aes(x = distance, linetype = Treatment, 
                                             fill = Treatment, colour = Treatment)) +
  geom_line(stat = "density", size = 1) +
  scale_color_manual(values = c("grey70", "black")) +
  ggtitle("Matched") +
  ylab("") +
  theme(legend.position = "bottom", plot.title = element_text(size = 12)) +
  scale_x_continuous("", limits = c(0, 1)) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.position = 'bottom',
    legend.direction = 'horizontal'
  )

### Save Figure E.1 with propensity scores
pdf('fig-ps.pdf', height = 4, width = 8)
grid.arrange(plot_unmatched, plot_matched, ncol = 2)
dev.off()


### Get standardized mean differences for matched and unmatched groups
xB_unmatched <- xBalance(treat_f, data = ess_m, report = c("all")) %>% as.data.frame()
std_unmatched <- xB_unmatched %>% select(results.std.diff.unstrat) 

xB_matched <- xBalance(treat_f, data = ess_matched_data, report = c("all")) %>% as.data.frame()
std_matched <- xB_matched %>% select(results.std.diff.unstrat) 

balance_df <- data.frame(
    covariate = row.names(xB_matched),
    unmatched = std_unmatched[row.names(xB_unmatched) %in% row.names(xB_matched),],
    matched = std_matched$results.std.diff.unstrat
  )

b_df_l <- balance_df %>% 
  pivot_longer(c("unmatched", "matched"), names_to = "Treatment", values_to = "dif") %>% 
  mutate(Treatment = ifelse(Treatment == "matched", "Matched", "Unmatched"))

### Save Figure E.2
pdf('fig-balance.pdf', height = 4, width = 6)
ggplot(b_df_l,  aes(x = covariate, y = dif, linetype = Treatment, fill = Treatment, colour = Treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "gray90") +
  scale_x_discrete("", 
                   limits = c("ideology", "religious", "tv", "polinterest",
                              "society", "edulevel", "education", "age", "male"),
                   labels = c("tv" = "Pol. news", "society" = "Subjective class", "religious" = "Religiosity",
                              "polinterest" = "Pol. interest", "male" = "Male", "education" = "Education",
                              "ideology" = "Ideology", "edulevel" = "Education level", "age" = "Age")) +
  geom_point(size = 2) +
  scale_color_manual(values = c("black", "grey70")) +
  coord_flip() +
  ylab("Standardized mean difference") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.position = 'top',
        legend.direction = 'horizontal')
dev.off()

## Supplementary Material F

### Descriptive statistics, Table F.1
writeLines(capture.output(
  stargazer(as.data.frame(select(ess, stfgov,
                   reform, education, male, age, edulevel,
                   society, polinterest, tv, religious, ideology)),
    covariate.labels = c(
      "Government satisfaction", "Reform", "Education",
      "Male", "Age", "Education level", "Subjective class",
      "Political interest", "Political news", "Religiosity", "Ideology" ),
    title = "Summary statistics, unmatched data", summary = TRUE,
    type = "html")), "table-descriptive-full.htm")

### Descriptive statistics, Table F.2
writeLines(capture.output(
  stargazer(as.data.frame(select(ess_matched_data, stfgov,
                                 reform, education, male, age, edulevel,
                                 society, polinterest, tv, religious, ideology)),
    covariate.labels = c("Government satisfaction", "Reform", "Education",
      "Male", "Age", "Education level", "Subjective class",
      "Political interest", "Political news", "Religiosity", "Ideology"),
    title = "Summary statistics, matched data",
    summary = TRUE,
    type = "html")), "table-descriptive-matched.htm")

### Correlation matrices (not reported in main text or supplementary material -- but always fun to look at)
#### Unmatched data
ess %>% 
  select(stfgov, reform, education, male, age, edulevel,
         society, polinterest, tv, religious, ideology) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  round(2)

#### Matched data
ess_matched_data %>% 
  select(stfgov, reform, education, male, age, edulevel,
         society, polinterest, tv, religious, ideology) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  round(2)

## Supplementary Material G

### Estimate models for Table G.1
reg_full_1 <- lm(stfgov ~ reform, data = ess)
reg_full_2 <- lm(stfgov ~ reform + male + age + edulevel + society + polinterest + tv + religious + ideology, data = ess)
reg_full_3 <- lm(stfgov ~ reform * education, data = ess)
reg_full_4 <- lm(stfgov ~ reform * education + male + age + edulevel + society + polinterest + tv + religious + ideology, data = ess)

### Save Table G.1
writeLines(capture.output(
  stargazer(reg_full_1, reg_full_2, reg_full_3, reg_full_4,
    align = TRUE, column.sep.width = "0pt",
    no.space = TRUE, digits = 2, model.numbers = FALSE, single.row = TRUE,
    notes.align = "l", 
    covariate.labels = c("Reform", "Education", "Male", "Age", "Education level",
      "Subjective class", "Pol. interest", "Pol. news", "Religiosity",
      "Ideology", "Reform*education"),
    keep.stat = c("n", "rsq"),
    column.labels = c("(1)", "(2)", "(3)", "(4)"),
    model.names = FALSE,
    type = "html")), "table-results-full.htm")

### Estimate models for Table G.2
treat_f_int <- reform ~ education * male + education * age + education * edulevel + education *
  society + education * polinterest + education * tv + education * religious + education * ideology
ess_int <- ess %>% 
  # Select variables to data frame
  select(stfgov, stflife, stfeco, stfdem, reform, education, edulevel, 
         male, age, society, polinterest, tv, religious, ideology, family, 
         prosp, absdate, difdate, pdwrk, unempl, hswrk) %>% 
  # Remove observations with missing values
  na.omit() %>% 
  # Create Treatment variable
  mutate(Treatment = ifelse(reform == 1, "Treated", "Control"))

ess_matched_int <- matchit(treat_f_int, method = "nearest", caliper = .1, data = ess_int)
ess_matched_int
ess_matched_int_data <- match.data(ess_matched_int)

reg_m_1_int <- lm(stfgov ~ reform, data = ess_matched_int_data)
reg_m_2_int <- lm(stfgov ~ reform + male + age + edulevel + society + polinterest + tv + religious + ideology, data = ess_matched_int_data)
reg_m_3_int <- lm(stfgov ~ reform * education, data = ess_matched_int_data)
reg_m_4_int <- lm(stfgov ~ reform * education + male + age + edulevel + society + polinterest + tv + religious + ideology, data = ess_matched_int_data)

### Save Table G.2
writeLines(capture.output(
  stargazer(
    reg_m_1_int, reg_m_2_int, reg_m_3_int, reg_m_4_int,
    align = TRUE, column.sep.width = "0pt", no.space = TRUE,
    digits = 2, model.numbers = FALSE, single.row = TRUE, notes.align = "l",
    covariate.labels = c("Reform", "Male", "Age", "Education level", "Subjective class",
                         "Pol. interest", "Pol. news", "Religiosity", "Ideology", "Education", "Reform*education"
    ),
    keep.stat = c("n", "rsq"), column.labels = c("(1)", "(2)", "(3)", "(4)"), model.names = FALSE,
    type = "html"
  )), "table-matchint.htm")

### Estimate models for Figure G.1
match_dif_cal <- function(x) {
  ess_matched_temp <- match.data(matchit(treat_f, method = "nearest", caliper = x, data = ess_m))
  
  ess_matched_temp %>% 
    lm(stfgov ~ reform * education + male + age + edulevel + society + polinterest + tv + religious + ideology, data = .) %>% 
    broom::tidy() %>% 
    filter(str_detect(term, "reform:education")) %>% 
    transmute(cali = x, est = estimate, se = std.error)
}

cal_df <- map_df(seq(0.01, 0.25, 0.01), match_dif_cal)

### Save Figure G.1
pdf('fig-calest.pdf', height = 4, width = 8)
ggplot(cal_df, aes(x = cali, y = est)) +
  geom_hline(yintercept = 0, col = "gray50") +
  ylab("Effect parameter for 'Reform × education'") +
  xlab("Caliper") +
  theme_minimal() +
  geom_errorbar(aes(ymin = est - 1.645 * se, ymax = est + 1.645 * se), width = 0, size = 0.8) +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), width = 0) +
  geom_point(size = 6, colour = "white") +
  geom_point(size = 3, shape = 1) +
  theme(legend.title = element_blank())
dev.off()

### Matching procedure with optimal and full matching
m_optimal <- matchit(treat_f, method = "optimal", data = ess_m)
m_optimal
m_optimal_data <- match.data(m_optimal)
reg_optimal_1 <- lm(stfgov ~ reform * education, data = m_optimal_data)
reg_optimal_2 <- lm(stfgov ~ reform * education + male + age + edulevel + society + polinterest + tv + religious + ideology, data = m_optimal_data)

m_full <- matchit(treat_f, method = "full", data = ess_m)
m_full
m_full_data <- match.data(m_full)
reg_full_1 <- lm(stfgov ~ reform * education, data = m_full_data)
reg_full_2 <- lm(stfgov ~ reform * education + male + age + edulevel + society + polinterest + tv + religious + ideology, data = m_full_data)

### Create Table G.3
writeLines(capture.output(
  stargazer(
    reg_optimal_1, reg_optimal_2, reg_full_1, reg_full_2,
    align = TRUE, column.sep.width = "0pt", no.space = TRUE, digits = 2,
    model.numbers = FALSE, single.row = TRUE, notes.align = "l",
    covariate.labels = c("Reform", "Education", "Male", "Age",
                         "Education level", "Subjective class", "Pol. interest", "Pol. news",
                         "Religiosity", "Ideology", "Reform*education"),
    keep.stat = c("n", "rsq"), column.labels = c("(1)", "(2)", "(3)", "(4)"), model.names = FALSE,
    type = "html")), "table-matching-optfull.htm")

## Supplementary Material H
### Estimate models with time trend control
reg_tt_1 <- lm(stfgov ~ reform + absdate, data = ess_matched_data)
reg_tt_2 <- lm(stfgov ~ reform + absdate + male + age + edulevel + society + polinterest + tv + religious + ideology, data = ess_matched_data)
reg_tt_3 <- lm(stfgov ~ reform * education + absdate, data = ess_matched_data)
reg_tt_4 <- lm(stfgov ~ reform * education + absdate + male + age + edulevel + society + polinterest + tv + religious + ideology, data = ess_matched_data)

### Save Table H.1
writeLines(capture.output(
  stargazer(
    reg_tt_1, reg_tt_2, reg_tt_3, reg_tt_4,
    align = TRUE, column.sep.width = "0pt", no.space = TRUE, digits = 2,
    model.numbers = FALSE, single.row = TRUE, notes.align = "l",
    covariate.labels = c("Reform", "Education", "Time trend", "Male",
                         "Age", "Education level", "Subjective class", "Pol. interest",
                         "Pol. news", "Religiosity", "Ideology", "Reform*education"),
    keep.stat = c("n", "rsq"),
    column.labels = c("(1)", "(2)", "(3)", "(4)"),
    model.names = FALSE,
    type = "html")), "table-ttc.htm")


## Supplementary Material I

### Estimate RDD models
rd_full <- rdrobust(ess$stfgov, ess$difdate, all = TRUE)
rd_short <- rdrobust(ess$stfgov[ess$difdate < 45 & ess$difdate > -45], 
                     ess$difdate[ess$difdate < 45 & ess$difdate > -45], 
                     all = TRUE)

### Save Figure I.1
pdf('fig-rdd.pdf', height = 4, width = 6)
rdplot(
  y = ess$stfgov[ess$difdate < 45 & ess$difdate > -45],
  x = ess$difdate[ess$difdate < 45 & ess$difdate > -45],
  title = "",
  y.label = "Government satisfaction",
  x.label = "Days",
  col.dots = "#afafaf",
  col.lines = "blue",
  y.lim = c(2, 8)
)
dev.off()

## Supplementary Material J
### Create data frame for announcement effects
ess_rs_df <- data.frame(day = -7:-21)

for (i in ess_rs_df$day) {
  ess_matched_data$reform_t <- ifelse(ess_matched_data$difdate > i & ess_matched_data$difdate < 0, 1, 0)
  ess_rs_df$est[ess_rs_df$day == i] <- coef(summary(lm(stfgov ~ reform_t * education + male + age + edulevel + society + polinterest + tv + religious + ideology, data = ess_matched_data)))["reform_t:education", "Estimate"]
  ess_rs_df$se[ess_rs_df$day == i] <- coef(summary(lm(stfgov ~ reform_t * education + male + age + edulevel + society + polinterest + tv + religious + ideology, data = ess_matched_data)))["reform_t:education", "Std. Error"]
}

### Save Figure J.1
pdf('fig-announcement.pdf', height = 4, width = 8)
ggplot(ess_rs_df, aes(x = day, y = est)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = est - se * 1.96, ymax = est + se * 1.96),
                colour = "black",
                width = .01) +
  geom_point(size = 6, colour = "white") +
  geom_point(size = 3, shape = 1) +
  theme_minimal(base_size = 11.5) +
  ylab("Effect estimate") +
  xlab("Distance to reform, days")
dev.off()


## Supplementary Material K

### Estimate prospective family proximity models
reg_pfamily_1 <- lm(stfgov ~ reform * education + reform * prosp, data = ess_matched_data)
reg_pfamily_2 <- lm(stfgov ~ reform * education + reform * prosp + male + age + society + polinterest + tv + religious + ideology, data = ess_matched_data)
reg_pfamily_3 <- lm(stfgov ~ reform * education + reform * prosp + male + age + society + polinterest + tv + religious + ideology + education, data = ess_matched_data)

### Save Table K.1
writeLines(capture.output(
  stargazer(reg_pfamily_1, reg_pfamily_2, reg_pfamily_3,
    align = TRUE, column.sep.width = "0pt", no.space = TRUE, digits = 2,
    model.numbers = FALSE, font.size = "scriptsize", single.row = TRUE, notes.align = "l", 
    covariate.labels = c("Reform", "Education", "Prospective family", "Male", "Age",
                         "Subjective class", "Pol. interest", "Pol. news", "Religiosity",
                         "Ideology", "Reform*Education", "Reform*Prospective family"),
    keep.stat = c("n", "rsq"),
    column.labels = c("(1)", "(2)", "(3)"),
    model.names = FALSE, type = "html"
  )), "table-proxpfamily.htm")


### Estimate models on socioeconomic proximities
reg_pdwrk <- lm(stfgov ~ reform * education + reform * pdwrk + male + age + society + polinterest + tv + religious + ideology + education, data = ess_matched_data)
reg_unempl <-lm(stfgov ~ reform * education + reform * unempl + male + age + society + polinterest + tv + religious + ideology + education, data = ess_matched_data)
reg_hswrk <- lm(stfgov ~ reform * education + reform * hswrk + male + age + society + polinterest + tv + religious + ideology + education, data = ess_matched_data)
reg_alljobs <- lm(stfgov ~ reform * education + reform * pdwrk + reform * unempl + reform * hswrk + male + age + society + polinterest + tv + religious + ideology + education, data = ess_matched_data)

### Save Table K.2
writeLines(capture.output(
  stargazer(reg_pdwrk, reg_unempl, reg_hswrk, reg_alljobs,
    align = TRUE, column.sep.width = "0pt", no.space = TRUE,
    digits = 2, model.numbers = FALSE, font.size = "scriptsize",
    single.row = TRUE, notes.align = "l",
    covariate.labels = c("Reform", "Education", "Paid work", "Unemployed", "Housework",
                         "Male", "Age", "Subjective class", "Pol. interest", "Pol. news", 
                         "Religiosity", "Ideology", "Reform*Education", "Reform*Paid work", 
                         "Reform*Unemployed", "Reform*Housework" ),
    keep.stat = c("n", "rsq"), column.labels = c("(1)", "(2)", "(3)", "(4)"), model.names = FALSE,
    type = "html")), "table-proxwork.htm")

## Supplementary Material L

### Estimate models with standardized regression coefficients
reg_m_1_beta <- lm.beta(lm(stfgov ~ reform, data = ess_matched_data))
reg_m_2_beta <- lm.beta(lm(stfgov ~ reform + male + age + edulevel + society + polinterest + tv + religious + ideology, data = ess_matched_data))
reg_m_3_beta <- lm.beta(lm(stfgov ~ reform * education, data = ess_matched_data))
reg_m_4_beta <- lm.beta(lm(stfgov ~ reform * education + male + age + edulevel + society + polinterest + tv + religious + ideology, data = ess_matched_data))

### Save Table L.1
writeLines(capture.output(
  stargazer(reg_m_1_beta, reg_m_2_beta, reg_m_3_beta, reg_m_4_beta,
    coef = list(
      reg_m_1_beta$standardized.coefficients,
      reg_m_2_beta$standardized.coefficients,
      reg_m_3_beta$standardized.coefficients,
      reg_m_4_beta$standardized.coefficients
    ),
    star.cutoffs = NA, align = TRUE, column.sep.width = "0pt",
    no.space = TRUE, digits = 2, model.numbers = FALSE, single.row = TRUE, notes.align = "l",
    covariate.labels = c("Reform", "Male", "Age", "Education level", "Subjective class",
                         "Pol. interest", "Pol. news", "Religiosity", "Ideology",
                         "Education", "Reform*education"),
    keep.stat = c("n", "rsq"), column.labels = c("(1)", "(2)", "(3)", "(4)"),
    model.names = FALSE, type = "html")), "table-stdcoef.htm")

## Supplementary Material M

### Specify treatment function
treat_f_edu <- education ~ reform + reform * male + reform * age + reform * edulevel + reform * society + reform * polinterest + reform * tv + reform * religious + reform * ideology

### Repeat matching procedure as above
ess_edu <- ess %>% 
  # Select variables to data frame
  select(stfgov, stflife, stfeco, stfdem, reform, education, edulevel, 
         male, age, society, polinterest, tv, religious, ideology, family, 
         prosp, absdate, difdate, pdwrk, unempl, hswrk) %>% 
  # Remove observations with missing values
  na.omit() %>% 
  # Create Treatment variable
  mutate(Treatment = ifelse(reform == 1, "Treated", "Control"))

ess_matched_edu <- matchit(treat_f_edu, method = "nearest", caliper = .3, data = ess_edu)
ess_matched_edu
ess_matched_edu_data <- match.data(ess_matched_edu)

xB_unmatched <- as.data.frame(xBalance(treat_f_edu, data = ess_m, report = c("all")))
std_unmatched <- xB_unmatched[, "results.std.diff.unstrat"]
xB_matched <- as.data.frame(xBalance(treat_f_edu, data = ess_matched_edu_data, report = c("all")))
std_matched <- xB_matched[, "results.std.diff.unstrat"]

balance_df <- data.frame(
    covariate = row.names(xB_matched),
    unmatched = std_unmatched[row.names(xB_unmatched) %in% row.names(xB_matched)],
    matched = std_matched
  )

b_df_l <- balance_df %>% 
  pivot_longer(c("unmatched", "matched"), names_to = "Treatment", values_to = "dif") %>% 
  mutate(Treatment = ifelse(Treatment == "matched", "Matched", "Unmatched"))

### Create Figure M.1
pdf('fig-balance-edu.pdf', height = 5, width = 6)
ggplot(b_df_l, aes(x = covariate, y = dif, linetype = Treatment, fill = Treatment, colour = Treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "gray90") +
  scale_x_discrete("", limits = c("reform:ideology", "reform:religious", "reform:tv",
                                  "reform:polinterest", "reform:society", "reform:edulevel", "reform:age", 
                                  "reform:male", "ideology", "religious", "tv", "polinterest", "society",
                                  "edulevel", "reform", "age", "male"),
    labels = c("tv" = "Pol. news", "society" = "Subjective class", "religious" = "Religiosity",
               "polinterest" = "Pol. interest", "male" = "Male", "reform" = "Reform", 
               "ideology" = "Ideology", "edulevel" = "Education level", "age" = "Age",
               "reform:male" = "Reform × Male", "reform:age" = "Reform × Age", "reform:edulevel" = "Reform × Education level",
               "reform:society" = "Reform × Subjective class", "reform:polinterest" = "Reform × Pol. interest", 
               "reform:tv" = "Reform × Pol. news", "reform:religious" = "Reform × Religiosity", 
               "reform:ideology" = "Reform × Ideology")) +
  geom_point(size = 2) +
  scale_color_manual(values = c("black", "grey70")) +
  coord_flip() +
  ylab("Standardized mean difference") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.position = 'top',
    legend.direction = 'horizontal'
  )
dev.off()

### Estimate models for Table M.1
reg_m_edu_1 <- lm(stfgov ~ reform * education + male * education + age * education + edulevel * education + society * education + polinterest * education + tv * education + religious * education + ideology * education, data = ess_matched_data)
reg_m_edu_2 <- lm(stfgov ~ reform * education, data = ess_matched_edu_data)
reg_m_edu_3 <- lm(stfgov ~ reform * education + male + age + edulevel + society + polinterest + tv + religious + ideology, data = ess_matched_edu_data)
reg_m_edu_4 <- lm(stfgov ~ reform * education + male * education + age * education + edulevel * education + society * education + polinterest * education + tv * education + religious * education + ideology * education, data = ess_matched_edu_data)

### Save Table M.1
writeLines(capture.output(
  stargazer(reg_m_edu_1, reg_m_edu_2, reg_m_edu_3, reg_m_edu_4,
    align = TRUE, column.sep.width = "0pt", no.space = TRUE, digits = 2,
    model.numbers = FALSE, font.size = "scriptsize", single.row = TRUE, notes.align = "l",
    covariate.labels = c("Reform", "Education", "Male", "Age", "Education level", "Subjective class",
                         "Pol. interest", "Pol. news", "Religiosity", "Ideology", "Reform*Education",
                         "Education*Male", "Education*Age", "Education*Education level", "Education*Subjective class",
                         "Education*Pol. interest", "Education*Pol. news", "Education*Religiosity", "Education*Ideology"),
    keep.stat = c("n", "rsq"), column.labels = c("(1)", "(2)", "(3)", "(4)"), model.names = FALSE, type = "html"
  )), "table-edureg.htm")

# Create and save sessionInfo.txt
writeLines(capture.output(sessionInfo()), "sessionInfo_2021.txt")