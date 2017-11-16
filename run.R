library(matahari)

dance_start(value = FALSE, contents = FALSE)

library(collegeIncome)
data(college)

# Each observation is a major
nrow(college)==length(unique(college$major))

# Incomplete cases
library(dplyr)
NACols <- names(which(colSums(is.na(college)) > 0))
college %>%
    filter(!complete.cases(.)) %>%
    select(major_category, major_code, one_of(NACols))

# helper function to identify invalid percents
is.wrong <- function(x, min=0, max=1)
    is.na(x) | is.infinite(x) | (is.numeric(x) & (x < min | x > max))

# column with invalid percents
columnScope <- grep("^perc_",names(college), value = TRUE)
(wrongCols <- names(which(colSums(sapply(college[, columnScope], is.wrong))>0)))

# Observations with invalid percents
wrongRows <- apply(sapply(wrongCols, 
                          function(col) is.wrong(college[, col])),
                   1, any)
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
fit1 <- lm(median ~ category, college)
summary(fit1)
anova(fit1)

# Residual plots
par(mfrow=c(3,2))
plot(fit1, which = 1:6, cex.id = 1, col="dark gray")

# Influence measurements
inf.measures <- influence.measures(fit1)
summary(inf.measures)

# Influential observations
influential <- unname(apply(inf.measures$is.inf, 1, any))
college %>%
    select(category, major_code, major, total, sample_size, median, rank) %>%
    filter(influential)

# What-if removing influential observations?
college2 <- college %>% filter(!influential)
anova(lm(median ~ category, college2))

# adding other terms
fit2 <- lm(median ~ category + perc_women, college)
fit3 <- lm(median ~ category + perc_college_jobs, college)
fit4 <- lm(median ~ category + perc_low_wage_jobs, college)
anova(fit1, fit2)
anova(fit1, fit3)
anova(fit1, fit4)

dance_save("college_major_analysis.rds")
