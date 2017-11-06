library(collegeIncome)
data(college)
library(matahari)

dance_start(value = FALSE, contents = FALSE)

# Each observation is a major
nrow(college)==length(unique(college$major))

# Rank uniformity and discreteness

all(sort(college$rank) == 1:nrow(college))

# Incomplete cases
library(dplyr)
NACols <- names(which(colSums(is.na(college)) > 0))
college %>%
    filter(!complete.cases(.)) %>%
    select(major_category, major_code, one_of(NACols))

# helper function to identify invalid percents
is.wrong <- function(x, min=0, max=1) is.na(x) | is.infinite(x) | (is.numeric(x) & (x < min | x > max))

# column with invalid percents
(wrongCols <- names(which(colSums(sapply(college[, grep("^perc_",names(college), value = TRUE)], is.wrong))>0)))

# Observations with invalid percents
wrongRows <- apply(sapply(wrongCols, function(col) is.wrong(college[, col])), 1, any)
college %>%
    filter(wrongRows) %>%
    select(rank, major_category, major_code, one_of(wrongCols))

# Shortening category name
college <- college %>%
    mutate(category = factor(abbreviate(major_category, minlength=3)))

# Imputing wrong percents with their category median (of median earning) values
college <- college
for(col in seq_along(wrongCols)) {
    wrongColRows <- is.wrong(college[, wrongCols[col]])
    college[wrongColRows, wrongCols[col]] <- 
        median(college[!wrongColRows, wrongCols[col]])
}

# Former wrong rows after imputation
college %>%
    filter(wrongRows) %>%
    select(rank, major_category, major_code, one_of(wrongCols))

# The initial hypothesis model
fit <- lm(rank ~ category, college)
anova(fit)

# Removing intercept yields higher F-statistic and "same" pval, i.e., beyond machine epsilon
fit <- lm(rank ~ category - 1, college)
anova(fit)

# Residual plots
par(mfrow=c(3,2))
plot(fit, which = 1:6, cex.id = 1, col="dark gray")

# Influence measurements
measures<- influence.measures(fit)
summary(measures)

# Influential observations
influential <- unname(apply(measures$is.inf, 1, any))
college %>%
    select(category, major_code, major, total, sample_size, median, rank) %>%
    filter(influential)

# Fit nested models challenging different binary variables from all kind of percentages
fit1 <- lm(rank ~ 0 + category, college)
fit2 <- lm(rank ~ 0 + category + I(college$perc_men > .5), college)
fit3 <- lm(rank ~ 0 + category + I(college$perc_women > .5), college)
fit4 <- lm(rank ~ 0 + category + I(perc_employed > .5), college)
fit5 <- lm(rank ~ 0 + category + I(perc_unemployed > .5), college)
fit6 <- lm(rank ~ 0 + category + I(perc_employed_fulltime > .5), college)
fit7 <- lm(rank ~ 0 + category + I(perc_employed_parttime > .5), college)
fit8 <- lm(rank ~ 0 + category + I(perc_employed_fulltime_yearround > .5), college)
fit9 <- lm(rank ~ 0 + category + I(!perc_employed_fulltime_yearround > .5), college)
fit10 <- lm(rank ~ 0 + category + I(perc_college_jobs > .5), college)
fit11 <- lm(rank ~ 0 + category + I(perc_non_college_jobs > .5), college)
fit12 <- lm(rank ~ 0 + category + I(perc_low_wage_jobs > .5), college)
fit13 <- lm(rank ~ 0 + category + I(!perc_low_wage_jobs > .5), college)
fit14 <- lm(rank ~ 0 + category * I(perc_non_college_jobs > .5), college)
fit15 <- lm(rank ~ 0 + category : I(perc_non_college_jobs > .5), college)

# ANOVA results
anova(fit1, fit2)
anova(fit1, fit3)
anova(fit1, fit4)
anova(fit1, fit5)
anova(fit1, fit6)
anova(fit1, fit7)
anova(fit1, fit8)
anova(fit1, fit9)
anova(fit1, fit10) #least worst
anova(fit1, fit11)
anova(fit1, fit12)
anova(fit1, fit13)
anova(fit1, fit14)
anova(fit1, fit15)

dance_save("college_major_analysis.rds")
