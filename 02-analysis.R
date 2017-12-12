## Title:     Welfare Retrenchments and Government Support:
##            Evidence from a Natural Experiment
## 
##            Erik Gahner Larsen
##            E.G.Larsen@kent.ac.uk
##        
## Data:      Publicly available at: http://www.europeansocialsurvey.org/download.html?file=ESS6DK&c=DK&y=2012
##            (free login required)
##            Data for the Danish National Election Study can be ordered at: http://dda.dk/catalogue/27067
##            (required in order to produce Figure A.1)

# Load packages
library("rio")
library("tidyr")
library("ggplot2")
library("gridExtra")
library("MatchIt")
library("rdrobust")
library("stargazer")
library("RItools")
library("rms")
library("lm.beta")

# Set random seed 
set.seed(2149573)

# Load data
ess <- import("data-ess.csv")
dnes <- import("data-dnes.csv")

# Figure 1: Coverage
## Create data frame with numbers of articles
coverage <- data.frame( 
  week = 1:13,
  su = c(2,4,0,0,3,2,3,19,12,2,3,0,0),
  reform = c(14,14,6,4,13,18,13,32,31,16,22,15,2)
)

## Create figure on media coverage
ggplot(coverage, aes(x=week, y=su)) + 
  geom_vline(linetype="dashed", aes(xintercept=7.5), colour="grey40") +
  geom_line() +
  geom_point(size = 6, colour = "white") + 
  geom_point(size = 3, shape = 1) +
  ylab("Number of articles") +
  scale_x_continuous("Week number (in 2013)", breaks=1:14) +
  theme_minimal(base_size = 11.5) 

## Save figure as eps
ggsave("fig-coverage.eps", height = 4, width = 6)

# Preprocessing: matching, nearest
## Select variables to data frame
ess.m <- ess[c("stfgov", "stflife", "stfeco", "stfdem", "reform", "education", "edulevel", "male", "age", "edulevel", "society", "polinterest", "tv", "religious", "ideology", "family", "prosp", "absdate", "difdate", "pdwrk", "unempl", "hswrk")]

## Remove observations with missing values
ess.m <- na.omit(ess.m)

## Specifcy treatment function
treat.f <- reform ~ education + male + age + edulevel + society + polinterest + tv + religious + ideology

## Create Treatment variable
ess.m$Treatment <- NA
ess.m$Treatment <- ifelse(ess.m$reform == 1, "Treated", "Control")

## Estimate binomial model of the treatment function
fit <- glm(treat.f, family=binomial, data=ess.m)

## Estimate propensity scores
ess.m$pscores <- predict(fit, type="response")

## Use nearest neighbor matching
ess.matched <- matchit(treat.f, method = "nearest", caliper = .1, data = ess.m)

### Get info on matched and unmatched cases
ess.matched

### Get matched data
ess.matched.data <- match.data(ess.matched)

# Create figure 2: Distribution of outcome
ggplot(ess.matched.data, aes(stfgov)) +
  geom_bar(colour="black", fill="gray90") +
  ylab("") +
  scale_x_continuous("Government satisfaction", breaks=0:10) + 
  theme_minimal()

## Save as .eps
ggsave("fig-outcome.eps", height = 4, width = 4)

## Get mean and standard deviation
mean(ess.matched.data$stfgov)
sd(ess.matched.data$stfgov)

## Estimate models reported in Table 1
reg.m.1 <- lm(stfgov ~ reform, data=ess.matched.data)
reg.m.2 <- lm(stfgov ~ reform + male + age + edulevel + society + polinterest + tv + religious + ideology, data=ess.matched.data)
reg.m.3 <- lm(stfgov ~ reform*education, data=ess.matched.data)
reg.m.4 <- lm(stfgov ~ reform*education + male + age + edulevel + society + polinterest + tv + religious + ideology, data=ess.matched.data)

## Save Table 1
writeLines(capture.output(
  stargazer(reg.m.1, reg.m.2, reg.m.3, reg.m.4,
            align = TRUE,
            column.sep.width = "0pt",
            no.space = TRUE,
            digits= 2,
            model.numbers = FALSE,
            single.row = TRUE,
            notes.align = "l",
            covariate.labels =c("Reform",  
                                "Male", "Age", "Education level", "Subjective class", "Pol. interest", 
                                "Pol. news", "Religiosity", "Ideology", "Education", "Reform*education"),
            keep.stat = c("n", "rsq"),
            column.labels=c("Average effect", "Average effect, w. covariates", "Conditional effect", "Conditional effect, w. covariates"),
            model.names=FALSE, type="html"
  )), "table-1.htm")


# Estimate alernative measures and models

reg.life <- lm(stflife ~ reform*education + male + age + edulevel + society + polinterest + tv + religious + ideology, data=ess.matched.data)
reg.eco <- lm(stfeco ~ reform*education + male + age + edulevel + society + polinterest + tv + religious + ideology, data=ess.matched.data)
reg.dem <- lm(stfdem ~ reform*education + male + age + edulevel + society + polinterest + tv + religious + ideology, data=ess.matched.data)

## Save Table 2
writeLines(capture.output(
  stargazer(reg.life, reg.eco, reg.dem,
            align = TRUE,
            column.sep.width = "0pt",
            no.space = TRUE,
            digits= 2,
            model.numbers = FALSE,
            font.size = "scriptsize",
            single.row = TRUE,
            notes.align = "l",
            covariate.labels =c("Reform", "Education", 
                                "Male","Age", "Education level", "Subjective class", "Pol. interest", 
                                "Pol. news","Religiosity", "Ideology","Reform*education"),
            keep.stat = c("n", "rsq"),
            column.labels=c("Life", "Economy", "Democracy"),
            model.names=FALSE, type="html"
  )), "table-2.htm")

# Estimate models using alternative proximity measures
reg.edulevel <- lm(stfgov ~ reform*education + reform*edulevel + male + age + society + polinterest + tv + religious + ideology + education, data=ess.matched.data)
reg.family <- lm(stfgov ~ reform*education + reform*family + male + age + society + polinterest + tv + religious + ideology + education, data=ess.matched.data)
reg.pdwrk <- lm(stfgov ~ reform*education + reform*pdwrk + male + age + society + polinterest + tv + religious + ideology, data=ess.matched.data)
reg.full <- lm(stfgov ~ reform*education + reform*edulevel + reform*family + reform*pdwrk + male + age + society + polinterest + tv + religious + ideology, data=ess.matched.data)

## Save Table 3
writeLines(capture.output(
  stargazer(reg.edulevel, reg.family, reg.pdwrk, reg.full,
            align = TRUE,
            column.sep.width = "0pt",
            no.space = TRUE,
            digits= 2,
            model.numbers = FALSE,
            font.size = "scriptsize",
            single.row = TRUE,
            notes.align = "l",
            covariate.labels =c("Reform", "Education", "Education level", "Family", "Paid work",
                                "Male", "Age", "Subjective class", "Pol. interest", 
                                "Pol. news", "Religiosity", "Ideology",
                                "Reform*Education", "Reform*Education level", "Reform*Family", "Reform*Paid work"),
            keep.stat = c("n", "rsq"),
            column.labels=c("Education level", "Family", "Paid work", "Full model"),
            model.names=FALSE, type="html"
  )), "table-3.htm")

# Supplementary Material

## Supplementary Material A

dnes$policy <- as.factor(dnes$policy)
levels(dnes$policy) <-  c("Cash benefits", "Child care", "Culture", "Defense", "Education", 
                               "Environment", "Foreign aid", "Health care", "Highways", "Home care",
                               "Immigration", "Pension", "Police", "Unemployment")

trecol <- c("#E02427", "#FCBE75", "#399F34")

## Create Figure A.1
pdf('fig-spending.pdf', height=9, width=8)
ggplot(dnes, aes(x=spending)) + 
  geom_bar(fill=c(rep(trecol, 14))) +
  facet_wrap(~ policy) + 
  ylab("") +
  scale_x_continuous(name="", breaks=1:3, labels=c("Too much", "Suitable", "Too little")) +
  theme_minimal()
dev.off()

## Supplementary Material B

### Create and save Figure B.1
fig.cov.reform <- ggplot(coverage, aes(x=week, y=reform)) + 
  geom_vline(linetype="dashed", aes(xintercept=7.5), colour="grey40") +
  ggtitle("A: Reform coverage") +
  geom_line() +
  geom_point(size = 6, colour = "white") + 
  geom_point(size = 3, shape = 1) +
  ylab("Number of articles, reforms") +
  scale_x_continuous("Week", breaks=1:14) +
  theme_minimal(base_size = 11.5) 

fig.cov.rel <- ggplot(coverage, aes(x=week, y=su/reform)) + 
  geom_vline(linetype="dashed", aes(xintercept=7.5), colour="grey40") +
  ggtitle("B: Relative coverage") +
  geom_line() +
  geom_point(size = 6, colour = "white") + 
  geom_point(size = 3, shape = 1) +
  scale_y_continuous("Relative education reform coverage", 
                     breaks=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6), 
                     labels=c("0%","10%","20%","30%","40%","50%","60%")) +
  scale_x_continuous("", breaks=1:14) +
  theme_minimal(base_size = 11.5) 

pdf('fig-coverage-rel.pdf', height=4, width=10)
grid.arrange(fig.cov.reform, fig.cov.rel, ncol=2)
dev.off()


## Supplementary Material C

### Create Figure C.1 with dates of interview
pdf('fig-dates.pdf', height=4, width=6)
ggplot(ess, aes(x=as.Date(date))) +
  geom_histogram(fill="gray90", colour="black", bins=25) +
  ylab("Frequency") + xlab("Date of interview") + 
  geom_vline(linetype="dashed", aes(xintercept=as.Date("2013-02-19")), colour="grey40") +
  theme_minimal()
dev.off()



## Supplementary Material E

### Save plot for PS for unmatched data
plot.unmatched <- ggplot(ess.m, aes(x=pscores, linetype=Treatment, fill = Treatment, colour = Treatment)) + 
  geom_line(stat = "density", size=1) +
  scale_color_manual(values=c("grey70", "black")) +
  ggtitle("Unmatched") +
  ylab("") +
  theme(legend.position="bottom", plot.title = element_text(size = 12)) +
  scale_x_continuous("", limits=c(0,1)) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal')

### Save plot for PS for matched data
plot.matched <- ggplot(ess.matched.data, aes(x=distance, linetype=Treatment, fill = Treatment, colour = Treatment)) + 
  geom_line(stat = "density", size=1) +
  scale_color_manual(values=c("grey70", "black")) +
  ggtitle("Matched") +
  ylab("") +
  theme(legend.position="bottom", plot.title = element_text(size = 12)) +
  scale_x_continuous("", limits=c(0,1)) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.key = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal')

### Save Figure E.1 with propensity scores
pdf('fig-ps.pdf', height=4, width=8)
grid.arrange(plot.unmatched, plot.matched, ncol=2)
dev.off()


### Get standardized mean differences for matched and unmatched groups
xB.unmatched <- xBalance(treat.f, data=ess.m, report=c("all"))
xB.unmatched <- as.data.frame(xB.unmatched)
std.unmatched <- xB.unmatched[,"results.std.diff.unstrat"]
xB.matched <- xBalance(treat.f, data=ess.matched.data, report=c("all"))
xB.matched <- as.data.frame(xB.matched)
std.matched <- xB.matched[,"results.std.diff.unstrat"]
balance.df <- data.frame(covariate = row.names(xB.matched), unmatched=std.unmatched[row.names(xB.unmatched) %in% row.names(xB.matched)], matched=std.matched)
b.df.l <- gather(balance.df, Treatment, dif, c(unmatched, matched))
b.df.l[b.df.l$Treatment == "unmatched",]$Treatment <- "Unmatched"
b.df.l[b.df.l$Treatment == "matched",]$Treatment <- "Matched"

### Save Figure E.2
pdf('fig-balance.pdf', height=4, width=6)
ggplot(b.df.l, aes(x=covariate, y=dif, linetype=Treatment, fill = Treatment, colour = Treatment)) + 
  geom_hline(yintercept=0, linetype="dashed", colour="gray90") +
  scale_x_discrete("", 
                   limits = c("ideology", "religious", "tv", "polinterest", "society", "edulevel", "education", "age", "male"), 
                   labels = c("tv" = "Pol. news", 
                              "society" = "Subjective class", 
                              "religious" = "Religiosity", 
                              "polinterest" = "Pol. interest", 
                              "male" = "Male", 
                              "education" = "Education",
                              "ideology" = "Ideology",
                              "edulevel" = "Education level",
                              "age" = "Age")) +
  geom_point(size=2) +
  scale_color_manual(values=c("black", "grey70")) +
  coord_flip() +
  ylab("Standardized mean difference") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(),
        legend.position = 'top',
        legend.direction = 'horizontal')
dev.off()

lrm(treat.f, data=ess)
lrm(treat.f, data=ess.matched.data)

## Supplementary Material F

### Descriptive statistics, Table F.1
writeLines(capture.output(
  stargazer(ess[c("stfgov", "reform", "education", "male", "age", "edulevel", "society", "polinterest", "tv", "religious", "ideology")],
            covariate.labels = c("Government satisfaction", "Reform", "Education", "Male", "Age", "Education level", "Subjective class", "Political interest", "Political news", "Religiosity", "Ideology"),
            title = "Summary statistics, unmatched data",
            summary = TRUE, type="html"
  )), "table-descriptive-full.htm")

### Descriptive statistics, Table F.2
writeLines(capture.output(
  stargazer(ess.matched.data[c("stfgov", "reform", "education", "male", "age", "edulevel", "society", "polinterest", "tv", "religious", "ideology")],
            covariate.labels = c("Government satisfaction", "Reform", "Education", "Male", "Age", "Education level", "Subjective class", "Political interest", "Political news", "Religiosity", "Ideology"),
            title = "Summary statistics, matched data",
            summary = TRUE, type="html"
  )), "table-descriptive-matched.htm")

### Correlation matrices (not reported in main text or supplementary material -- but always fun to look at)
stargazer(cor(ess[c("stfgov", "reform", "education", "male", "age", "edulevel", "society", "polinterest", "tv", "religious", "ideology")], use="pairwise.complete.obs"),
          title = "Correlation matrix, unmatched data",
          digits = 2,
          float.env = "sidewaystable"
)

stargazer(cor(ess.matched.data[c("stfgov", "reform", "education", "male", "age", "edulevel", "society", "polinterest", "tv", "religious", "ideology")], use="pairwise.complete.obs"),
          title = "Correlation matrix, matched data",
          digits = 2,
          float.env = "sidewaystable"
)

## Supplementary Material G

### Estimate models for Table G.1
reg.full.1 <- lm(stfgov ~ reform, data=ess)
reg.full.2 <- lm(stfgov ~ reform + male + age + edulevel + society + polinterest + tv + religious + ideology, data=ess)
reg.full.3 <- lm(stfgov ~ reform*education, data=ess)
reg.full.4 <- lm(stfgov ~ reform*education + male + age + edulevel + society + polinterest + tv + religious + ideology, data=ess)


### Save Table G.1
writeLines(capture.output(
  stargazer(reg.full.1, reg.full.2, reg.full.3, reg.full.4,
            align = TRUE,
            column.sep.width = "0pt",
            no.space = TRUE,
            digits= 2,
            model.numbers = FALSE,
            single.row = TRUE,
            notes.align = "l",
            covariate.labels =c("Reform", "Education", 
                                "Male","Age", "Education level", "Subjective class", "Pol. interest", 
                                "Pol. news","Religiosity", "Ideology","Reform*education"),
            keep.stat = c("n", "rsq"),
            column.labels=c("(1)", "(2)", "(3)", "(4)"),
            model.names=FALSE, type="html"
  )), "table-results-full.htm")

### Estimate models for Table G.2
treat.f.int <- reform ~ education*male + education*age + education*edulevel + education*society + education*polinterest + education*tv + education*religious + education*ideology
ess.int <- ess[c("stfgov", "stflife", "stfeco", "stfdem", "reform", "education", "edulevel", "male", "age", "edulevel", "society", "polinterest", "tv", "religious", "ideology", "family", "prosp", "absdate", "pdwrk", "unempl", "hswrk")]
ess.int <- na.omit(ess.int)
ess.int$Treatment <- NA
ess.int$Treatment <- ifelse(ess.int$reform == 1, "Treated", "Control")
ess.matched.int <- matchit(treat.f.int, method = "nearest", caliper=.1, data = ess.int)
ess.matched.int
ess.matched.int.data <- match.data(ess.matched.int)

reg.m.1.int <- lm(stfgov ~ reform, data=ess.matched.int.data)
reg.m.2.int <- lm(stfgov ~ reform + male + age + edulevel + society + polinterest + tv + religious + ideology, data=ess.matched.int.data)
reg.m.3.int <- lm(stfgov ~ reform*education, data=ess.matched.int.data)
reg.m.4.int <- lm(stfgov ~ reform*education + male + age + edulevel + society + polinterest + tv + religious + ideology, data=ess.matched.int.data)

### Save Table G.2
writeLines(capture.output(
  stargazer(reg.m.1.int, reg.m.2.int, reg.m.3.int, reg.m.4.int, 
            align = TRUE,
            column.sep.width = "0pt",
            no.space = TRUE,
            digits= 2,
            model.numbers = FALSE,
            single.row = TRUE,
            notes.align = "l",
            covariate.labels =c("Reform",  
                                "Male", "Age", "Education level", "Subjective class", "Pol. interest", 
                                "Pol. news", "Religiosity", "Ideology", "Education", "Reform*education"),
            keep.stat = c("n", "rsq"),
            column.labels=c("(1)", "(2)", "(3)", "(4)"),
            model.names=FALSE, type="html"
  )), "table-matchint.htm")


### Estimate models for Figure G.1
cal.df <- data.frame(
  cali = c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.2,0.21,0.22,0.23,0.24,0.25),
  est = NA,
  se = NA
)
for (i in cal.df$cali) {
  ess.matched.temp <- matchit(treat.f, method = "nearest", caliper=i, data = ess.m)
  cal.df$est[cal.df$cali == i] <- coef(summary(lm(stfgov ~ reform*education + male + age + edulevel + society + polinterest + tv + religious + ideology, data=match.data(ess.matched.temp))))["reform:education","Estimate"]
  cal.df$se[cal.df$cali == i] <- coef(summary(lm(stfgov ~ reform*education + male + age + edulevel + society + polinterest + tv + religious + ideology, data=match.data(ess.matched.temp))))["reform:education","Std. Error"]  
}

### Save Figure G.1

pdf('fig-calest.pdf', height=4, width=8)
ggplot(cal.df, aes(x = cali, y = est)) + 
  geom_hline(yintercept=0, col="gray50") +
  ylab("Effect parameter for 'Reform × education'") +
  xlab("Caliper") +
  theme_minimal() +
  geom_errorbar(aes(ymin=est-1.645*se, ymax=est+1.645*se), width=0, size=0.8) +
  geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se), width=0) +
  geom_point(size = 6, colour = "white") + 
  geom_point(size = 3, shape = 1) +
  theme(legend.title=element_blank())
dev.off()

### Matching procedure with optimal and full matching
m.optimal <- matchit(treat.f, method = "optimal", data = ess.m)
m.optimal
m.optimal.data <- match.data(m.optimal)
reg.optimal.1 <- lm(stfgov ~ reform*education, data=m.optimal.data)
reg.optimal.2 <- lm(stfgov ~ reform*education + male + age + edulevel + society + polinterest + tv + religious + ideology, data=m.optimal.data)

m.full <- matchit(treat.f, method = "full", data = ess.m)
m.full
m.full.data <- match.data(m.full)
reg.full.1 <- lm(stfgov ~ reform*education, data=m.full.data)
reg.full.2 <- lm(stfgov ~ reform*education + male + age + edulevel + society + polinterest + tv + religious + ideology, data=m.full.data)

### Create Table G.3
writeLines(capture.output(
  stargazer(reg.optimal.1, reg.optimal.2, reg.full.1, reg.full.2,
            align = TRUE,
            column.sep.width = "0pt",
            no.space = TRUE,
            digits= 2,
            model.numbers = FALSE,
            single.row = TRUE,
            notes.align = "l",
            covariate.labels =c("Reform", "Education", 
                                "Male","Age", "Education level", "Subjective class", "Pol. interest", 
                                "Pol. news","Religiosity", "Ideology","Reform*education"),
            keep.stat = c("n", "rsq"),
            column.labels=c("(1)", "(2)", "(3)", "(4)"),
            model.names=FALSE, type="html"
  )), "table-matching-optfull.htm")


## Supplementary Material H
### Estimate models with time trend control
reg.tt.1 <- lm(stfgov ~ reform + absdate, data=ess.matched.data)
reg.tt.2 <- lm(stfgov ~ reform + absdate + male + age + edulevel + society + polinterest + tv + religious + ideology, data=ess.matched.data)
reg.tt.3 <- lm(stfgov ~ reform*education + absdate, data=ess.matched.data)
reg.tt.4 <- lm(stfgov ~ reform*education + absdate + male + age + edulevel + society + polinterest + tv + religious + ideology, data=ess.matched.data)

### Save Table H.1
writeLines(capture.output(
  stargazer(reg.tt.1, reg.tt.2, reg.tt.3, reg.tt.4,
            align = TRUE,
            column.sep.width = "0pt",
            no.space = TRUE,
            digits= 2,
            model.numbers = FALSE,
            single.row = TRUE,
            notes.align = "l",
            covariate.labels =c("Reform", "Education", "Time trend",
                                "Male","Age", "Education level", "Subjective class", "Pol. interest", 
                                "Pol. news","Religiosity", "Ideology","Reform*education"),
            keep.stat = c("n", "rsq"),
            column.labels=c("(1)", "(2)", "(3)", "(4)"),
            model.names=FALSE, type="html"
  )), "table-ttc.htm")


## Supplementary Material I

### Estimate RDD models
rd.full <- rdrobust(ess$stfgov, ess$difdate, all=TRUE)
rd.short <- rdrobust(ess$stfgov[ess$difdate < 45 & ess$difdate > -45], ess$difdate[ess$difdate < 45 & ess$difdate > -45], all=TRUE)

### Save Figure I.1
pdf('fig-rdd.pdf', height=4, width=6)
rdplot(y = ess$stfgov[ess$difdate < 45 & ess$difdate > -45], 
       x = ess$difdate[ess$difdate < 45 & ess$difdate > -45], 
       title = "",
       y.label = "Government satisfaction",
       x.label = "Days",
       col.dots = "#afafaf",
       type.dots = 1,
       col.lines = "blue",
       y.lim=c(2,8))
dev.off()


## Supplementary Material J

### Create data frame for announcement effects
ess.rs.df <- data.frame(
  day = -7:-21,
  pval = NA
)

for (i in ess.rs.df$day) {
  ess.matched.data$reform.t <- ifelse(ess.matched.data$difdate > i & ess.matched.data$difdate < 0, 1, 0)
  ess.rs.df$est[ess.rs.df.b$day == i] <- coef(summary(lm(stfgov ~ reform.t*education + male + age + edulevel + society + polinterest + tv + religious + ideology, data=ess.matched.data)))["reform.t:education","Estimate"]
  ess.rs.df$se[ess.rs.df.b$day == i] <- coef(summary(lm(stfgov ~ reform.t*education + male + age + edulevel + society + polinterest + tv + religious + ideology, data=ess.matched.data)))["reform.t:education","Std. Error"]
}

### Save Figure J.1
pdf('fig-announcement.pdf', height=4, width=8)
ggplot(ess.rs.df, aes(x=day, y=est)) +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymin=est-se*1.96, ymax=est+se*1.96), colour="black", width=.01) +
  geom_point(size = 6, colour = "white") + 
  geom_point(size = 3, shape = 1) +
  theme_minimal(base_size = 11.5) +
  ylab("Effect estimate") +
  xlab("Distance to reform, days")
dev.off()  


## Supplementary Material K

### Estimate prospective family proximity models
reg.pfamily.1 <- lm(stfgov ~ reform*education + reform*prosp, data=ess.matched.data)
reg.pfamily.2 <- lm(stfgov ~ reform*education + reform*prosp + male + age + society + polinterest + tv + religious + ideology, data=ess.matched.data)
reg.pfamily.3 <- lm(stfgov ~ reform*education + reform*prosp + male + age + society + polinterest + tv + religious + ideology + education, data=ess.matched.data)

### Save Table K.1
writeLines(capture.output(
  stargazer(reg.pfamily.1, reg.pfamily.2, reg.pfamily.3,
            align = TRUE,
            column.sep.width = "0pt",
            no.space = TRUE,
            digits= 2,
            model.numbers = FALSE,
            font.size = "scriptsize",
            single.row = TRUE,
            notes.align = "l",
            covariate.labels =c("Reform", "Education", "Prospective family",
                                "Male","Age",  "Subjective class", "Pol. interest", 
                                "Pol. news","Religiosity", "Ideology", "Reform*Education", "Reform*Prospective family"),
            keep.stat = c("n", "rsq"),
            column.labels=c("(1)", "(2)", "(3)"),
            model.names=FALSE, type="html"
  )), "table-proxpfamily.htm")


### Estimate models on socioeconomic proximities
reg.pdwrk <- lm(stfgov ~ reform*education + reform*pdwrk + male + age + society + polinterest + tv + religious + ideology + education, data=ess.matched.data)
reg.unempl <- lm(stfgov ~ reform*education + reform*unempl + male + age + society + polinterest + tv + religious + ideology + education, data=ess.matched.data)
reg.hswrk <- lm(stfgov ~ reform*education + reform*hswrk + male + age + society + polinterest + tv + religious + ideology + education, data=ess.matched.data)
reg.alljobs <- lm(stfgov ~ reform*education + reform*pdwrk + reform*unempl + reform*hswrk + male + age + society + polinterest + tv + religious + ideology + education, data=ess.matched.data)

### Save Table K.2
writeLines(capture.output(
  stargazer(reg.pdwrk, reg.unempl, reg.hswrk, reg.alljobs,
            align = TRUE,
            column.sep.width = "0pt",
            no.space = TRUE,
            digits= 2,
            model.numbers = FALSE,
            font.size = "scriptsize",
            single.row = TRUE,
            notes.align = "l",
            covariate.labels =c("Reform", "Education", "Paid work", "Unemployed", "Housework",
                                "Male","Age",  "Subjective class", "Pol. interest", 
                                "Pol. news","Religiosity", "Ideology", 
                                "Reform*Education", "Reform*Paid work", "Reform*Unemployed", "Reform*Housework"),
            keep.stat = c("n", "rsq"),
            column.labels=c("(1)", "(2)", "(3)", "(4)"),
            model.names=FALSE, type="html"
  )), "table-proxwork.htm")

## Supplementary Material L

### Estimate models with standardized regression coefficients
reg.m.1.beta <- lm.beta(lm(stfgov ~ reform, data=ess.matched.data))
reg.m.2.beta <- lm.beta(lm(stfgov ~ reform + male + age + edulevel + society + polinterest + tv + religious + ideology, data=ess.matched.data))
reg.m.3.beta <- lm.beta(lm(stfgov ~ reform*education, data=ess.matched.data))
reg.m.4.beta <- lm.beta(lm(stfgov ~ reform*education + male + age + edulevel + society + polinterest + tv + religious + ideology, data=ess.matched.data))

### Save Table L.1
writeLines(capture.output(
  stargazer(reg.m.1.beta, reg.m.2.beta, reg.m.3.beta, reg.m.4.beta,
            coef = list(
              reg.m.1.beta$standardized.coefficients,
              reg.m.2.beta$standardized.coefficients,
              reg.m.3.beta$standardized.coefficients,
              reg.m.4.beta$standardized.coefficients),
            star.cutoffs = NA,
            align = TRUE,
            column.sep.width = "0pt",
            no.space = TRUE,
            digits= 2,
            model.numbers = FALSE,
            single.row = TRUE,
            notes.align = "l",
            covariate.labels =c("Reform", 
                                "Male", "Age", "Education level", "Subjective class", "Pol. interest", 
                                "Pol. news", "Religiosity", "Ideology", "Education", "Reform*education"),
            keep.stat = c("n", "rsq"),
            column.labels=c("(1)", "(2)", "(3)", "(4)"),
            model.names=FALSE, type="html"
  )), "table-stdcoef.htm")

## Supplementary Material M

### Specify treatment function
treat.f.edu <- education ~ reform + reform*male + reform*age + reform*edulevel + reform*society + reform*polinterest + reform*tv + reform*religious + reform*ideology

### Repeat matching procedure as above
ess.edu <- ess[c("stfgov", "stflife", "stfeco", "stfdem", "reform", "education", "edulevel", "male", "age", "edulevel", "society", "polinterest", "tv", "religious", "ideology", "family", "prosp", "absdate", "pdwrk", "unempl", "hswrk")]
ess.edu <- na.omit(ess.edu)
ess.edu$Treatment <- NA
ess.edu$Treatment <- ifelse(ess.edu$reform == 1, "Treated", "Control")
ess.matched.edu <- matchit(treat.f.edu, method = "nearest", caliper=.3, data = ess.edu)
ess.matched.edu
ess.matched.edu.data <- match.data(ess.matched.edu)

xB.unmatched <- xBalance(treat.f.edu, data=ess.m, report=c("all"))
xB.unmatched <- as.data.frame(xB.unmatched)
std.unmatched <- xB.unmatched[,"results.std.diff.unstrat"]
xB.matched <- xBalance(treat.f.edu, data=ess.matched.edu.data, report=c("all"))
xB.matched <- as.data.frame(xB.matched)
std.matched <- xB.matched[,"results.std.diff.unstrat"]
balance.df <- data.frame(covariate = row.names(xB.matched), unmatched=std.unmatched[row.names(xB.unmatched) %in% row.names(xB.matched)], matched=std.matched)
b.df.l <- gather(balance.df, Treatment, dif, c(unmatched, matched))
b.df.l[b.df.l$Treatment == "unmatched",]$Treatment <- "Unmatched"
b.df.l[b.df.l$Treatment == "matched",]$Treatment <- "Matched"


### Create Figure M.1
pdf('fig-balance-edu.pdf', height=5, width=6)
ggplot(b.df.l, aes(x=covariate, y=dif, linetype=Treatment, fill = Treatment, colour = Treatment)) + 
  geom_hline(yintercept=0, linetype="dashed", colour="gray90") +
  scale_x_discrete("", 
                   limits = c("reform:ideology", 
                              "reform:religious", 
                              "reform:tv", "reform:polinterest", "reform:society", "reform:edulevel", "reform:age", "reform:male", "ideology", "religious", "tv", "polinterest", "society", "edulevel", "reform", "age", "male"), 
                   labels = c("tv" = "Pol. news", 
                              "society" = "Subjective class", 
                              "religious" = "Religiosity", 
                              "polinterest" = "Pol. interest", 
                              "male" = "Male", 
                              "reform" = "Reform",
                              "ideology" = "Ideology",
                              "edulevel" = "Education level",
                              "age" = "Age",
                              "reform:male" = "Reform × Male",
                              "reform:age" = "Reform × Age",
                              "reform:edulevel" = "Reform × Education level",
                              "reform:society" = "Reform × Subjective class",
                              "reform:polinterest" = "Reform × Pol. interest",
                              "reform:tv" = "Reform × Pol. news",
                              "reform:religious" = "Reform × Religiosity",
                              "reform:ideology" = "Reform × Ideology"
                   )) +
  geom_point(size=2) +
  scale_color_manual(values=c("black", "grey70")) +
  coord_flip() +
  ylab("Standardized mean difference") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), legend.title = element_blank(),
        legend.key = element_blank(),
        legend.position = 'top',
        legend.direction = 'horizontal')
dev.off()


### Estimate models for Table M.1
reg.m.edu.1 <- lm(stfgov ~ reform*education + male*education + age*education + edulevel*education + society*education + polinterest*education + tv*education + religious*education + ideology*education, data=ess.matched.data)
reg.m.edu.2 <- lm(stfgov ~ reform*education, data=ess.matched.edu.data)
reg.m.edu.3 <- lm(stfgov ~ reform*education + male + age + edulevel + society + polinterest + tv + religious + ideology, data=ess.matched.edu.data)
reg.m.edu.4 <- lm(stfgov ~ reform*education + male*education + age*education + edulevel*education + society*education + polinterest*education + tv*education + religious*education + ideology*education, data=ess.matched.edu.data)

### Save Table M.1
writeLines(capture.output(
  stargazer(reg.m.edu.1, reg.m.edu.2, reg.m.edu.3, reg.m.edu.4,
            align = TRUE,
            column.sep.width = "0pt",
            no.space = TRUE,
            digits= 2,
            model.numbers = FALSE,
            font.size = "scriptsize",
            single.row = TRUE,
            notes.align = "l",
            covariate.labels =c("Reform", "Education", "Male", "Age", "Education level", 
                                "Subjective class", "Pol. interest", "Pol. news","Religiosity", "Ideology", 
                                "Reform*Education", "Education*Male", "Education*Age", "Education*Education level", 
                                "Education*Subjective class", "Education*Pol. interest", 
                                "Education*Pol. news","Education*Religiosity", "Education*Ideology"),
            keep.stat = c("n", "rsq"),
            column.labels=c("(1)", "(2)", "(3)", "(4)"),
            model.names=FALSE, type="html"
  )), "table-edureg.htm")

# Create and save sessionInfo.txt
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")