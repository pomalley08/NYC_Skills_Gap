# Burning Glass xCase
# Patrick O'Malley
# 4/7/17

# This is the script use to manipulate and visualize the burning glass data

# Load packages ---------------------------------------------------------------
# Distribution fitting package
install.packages("fitdistplus")
library(fitdistrplus)
# Includes several data analysis packages
install.packages("tidyverse")
library(tidyverse)
# String wrapping package
install.packages("stringr")
library(stringr)
# Text visualization package
install.packages("wordcloud")
library(wordcloud)

# Bring the data generated in mySQL into R ------------------------------------
jobs         <- read.csv("jobs.csv", na.strings = "\\N")
jobs         <- tbl_df(jobs)
certs        <- read.csv("certs.csv", na.strings = "\\N")   
certs        <- tbl_df(certs)
counties     <- read.csv("counties.csv", na.strings = "\\N") 
counties     <- tbl_df(counties)
degrees      <- read.csv("degrees.csv", na.strings = "\\N")  
degrees      <- tbl_df(degrees)
exp_levels   <- read.csv("exp_levels.csv", na.strings = "\\N")  
exp_levels   <- tbl_df(exp_levels)
jobs_certs   <- read.csv("jobs_certs.csv", na.strings = "\\N") 
jobs_certs   <- tbl_df(jobs_certs)
jobs_degrees <- read.csv("jobs_degrees.csv", na.strings = "\\N") 
jobs_degrees <- tbl_df(jobs_degrees)
jobs_majors  <- read.csv("jobs_majors.csv", na.strings = "\\N")  
jobs_majors  <- tbl_df(jobs_majors)
jobs_skills  <- read.csv("jobs_skills.csv", na.strings = "\\N")  
jobs_skills  <- tbl_df(jobs_skills)
majors       <- read.csv("majors.csv", na.strings = "\\N") 
majors       <- tbl_df(majors)
msas         <- read.csv("msas.csv", na.strings = "\\N") 
msas         <- tbl_df(msas)
occupations  <- read.csv("occupations.csv", na.strings = "\\N")  
occupations  <- tbl_df(occupations)
skills       <- read.csv("skills.csv", na.strings = "\\N")  
skills       <- tbl_df(skills)
titles       <- read.csv("titles.csv", na.strings = "\\N")  
titles       <- tbl_df(titles)
states       <- read.csv("states.csv", na.strings = "\\N")  
states       <- tbl_df(states)
soc_level    <- read.csv("SOCLevel.csv")
soc_level    <- tbl_df(soc_level)
industries   <- read.csv("industries.csv")
industries   <- tbl_df(industries)

# Check data integrity
str(jobs)
str(certs)
str(counties)
str(degrees)
str(exp_levels)
str(jobs_certs)
str(jobs_degrees)
str(jobs_majors)
str(jobs_skills)
str(majors)
str(msas)
str(occupations)
str(skills)
str(titles)
str(states)
str(soc_level)
str(industries)

# Initial data exploration -----------------------------------------------------
names(jobs)
jobs %>% distinct(JobID) %>% summarise(n())
# 167,464 unique job ids
jobs %>% tally()
# 189,461 observations in jobs table, so some JobIDs are duplicated
jobs %>% group_by(JobID) %>% summarise(n = n()) %>% arrange(desc(n)) %>% slice(1:3)
filter(jobs, JobID == 37810953921)
filter(jobs, JobID == 490321858)
filter(jobs, JobID == 490773800)
# duplicate JobIDs have different posting durations so assume they represent different jobs

# Using only NYC data
levels(msas$MSA_Name)
# NYC MSC is "New York-Northern New Jersey-Long Island NY-NJ-PA (Metropolitan Statistical Area)"
msas[which(msas$MSA_Name %in% "New York-Northern New Jersey-Long Island NY-NJ-PA (Metropolitan Statistical Area)"), ]
# MSC_ID for NYC is 234
names(jobs)
nyc <- filter(jobs, MSA_ID == 234)
nyc <- tbl_df(nyc)   
str(nyc)
(nyc_jobs_count <- as.numeric(nyc %>% tally()))
table(nyc$County_ID)
# 42,672 jobs for the NYC MSA across 23 counties

# Which states are represented in the data? In what percentages? 
nyc %>% distinct(State_ID) %>% left_join(states)
# There are 3 states represented: NJ, NY, PA
(state_perc <- nyc %>% 
        group_by(State_ID) %>% 
        summarise(n = n(), Percent = round(n / nyc_jobs_count * 100, 2)) %>% 
                left_join(states))
# NY: 71.57%, NJ: 28.01%, PA: 0.42%

# What is the distribution of posting duration? 
names(nyc)
ggplot(data = nyc) + geom_histogram(mapping = aes(x = Posting_Duration), binwidth = 10) +
        ggtitle("Posting Duration Distribution")
descdist(nyc$Posting_Duration)
# appears beta distribution will provide a good fit

# Is the posting duration distribution similar across states?
(nyc_states <- nyc %>% left_join(states))
ggplot(data = nyc_states) + 
               geom_histogram(mapping = aes(x = Posting_Duration), binwidth = 10) +
               facet_grid(State_Name ~ ., scales = "free") +
        ggtitle("Posting Duration Distribution Per State")
# histograms all appear similar with heavy right skews
nyc_states %>% group_by(State_ID) %>% distinct(State_Name)
nj_pd <- nyc %>% filter(State_ID == 35) %>% dplyr::select(Posting_Duration)
descdist(nj_pd$Posting_Duration)
# NJ has a beta dist
ny_pd <- nyc %>% filter(State_ID == 38) %>% dplyr::select(Posting_Duration)
descdist(ny_pd$Posting_Duration)
# NY has a beta dist
pa_pd <- nyc %>% filter(State_ID == 42) %>% dplyr::select(Posting_Duration)
descdist(pa_pd$Posting_Duration)
# PA has a beta dist

# Is the posting duration distribution similar across occupations?
nyc %>% distinct(Occupation_Code) %>% summarise(n = n())
# 562 different occupations, will randomly sample for 2 different ones
(distinct_occs <- nyc %>% 
                left_join(occupations) %>% 
                distinct(Occupation_Code, Occupation_Name))
set.seed(1)
sample_n(distinct_occs, 2)
# look at Paralegal & Valet
nyc_occs <- nyc %>% left_join(occupations)
ggplot(data = filter(nyc_occs, Occupation_Code %in% c(23201100, 53602100))) + 
        geom_histogram(mapping = aes(x = Posting_Duration), binwidth = 10) +
        facet_grid(Occupation_Name ~ ., scales = "free") +
        ggtitle("Posting Duration Distribution Per Occupation")
# Both histograms are right skewed while Valet has a large spike around 100 days
# this matches our observations of statewide posting durations 
para_pd <- nyc %>% filter(Occupation_Code == 23201100) %>% dplyr::select(Posting_Duration)
descdist(para_pd$Posting_Duration)
# Paralegal may have an exponential dist, requires testing to confirm
# check the qq plot for exponential dist
ggplot(data = para_pd) + 
        geom_qq(aes(sample = Posting_Duration), distribution = stats::qexp) +
        ggtitle("Q-Q Plot For Paralegal Posting Duration", 
                subtitle = "Against Exponential Distribution") +
        geom_abline(intercept = 0, slope = 30)
# Check with ks.test
# estimate the parameters
fit1 <- fitdistr(para_pd$Posting_Duration, "exponential") 
# goodness of fit test
ks.test(para_pd$Posting_Duration, "pexp", fit1$estimate) 
# p-value = 0.66 > 0.05 -> distribution not refused

valet_pd <- nyc %>% filter(Occupation_Code == 53602100) %>% dplyr::select(Posting_Duration)
descdist(valet_pd$Posting_Duration)
# Valet has a beta dist

# Determine criteria for indicating a "skills gap" -----------------------------

# What are the top listed occupation names as percent of total listings?
(top_occ <- nyc_occs %>% group_by(Occupation_Name) %>% 
        filter(!is.na(Occupation_Name)) %>% 
        summarise(n = n(), Percent = n / nyc_jobs_count * 100) %>% 
        arrange(desc(Percent)) %>% 
        slice(1:25))
# top is Retail Sales Associate at 5.39% followed by Registered Nurse at 2.80%

# Which occupation names have the longest durations?
(long_dur <- nyc_occs %>% group_by(Occupation_Name) %>% 
        summarise(n = n(), avg_dur = round(mean(Posting_Duration, na.rm = TRUE), 2)) %>% 
        arrange(desc(avg_dur)) %>%
        slice(1:25))
# filter for occupations with over 3 listings to remove one offs affecting the data
(long_dur <- nyc_occs %>% group_by(Occupation_Name) %>% 
        summarise(n = n(), avg_dur = round(mean(Posting_Duration, na.rm = TRUE), 2)) %>%
        filter(n > 3) %>% 
        arrange(desc(avg_dur)) %>%
        slice(1:25))
# top is Optometrist at 94 days and next is Telemarketer at 88 days on average
# look at Optometrist distribution        
ggplot(data = filter(nyc_occs, Occupation_Name %in% "Optometrist"), aes(x = Posting_Duration)) + geom_histogram()

# In which fields or industries is there the greatest unfilled demand (by your definition)?
# choose unfilled demand to be median posting dur longer than 15 days and over 1000 listings
# industries are determined by looking at SOC values, determine SOC level for jobs
jobs <- jobs %>% mutate(SOC_level = floor(Occupation_Code / 100000))
nyc <- nyc %>% mutate(SOC_level = floor(Occupation_Code / 100000))
jobs <- jobs %>% mutate(industry = floor(Occupation_Code / 1000000))
nyc <- nyc %>% mutate(industry = floor(Occupation_Code / 1000000))
head(nyc$SOC_level)
names(soc_level)
job_fields <- nyc %>% left_join(soc_level, by = c("SOC_level" = "SOCLevel")) %>% 
        left_join(industries, by = c("industry" = "Major.Group"))
names(job_fields)
job_fields <- rename(job_fields, industry_name = Name)

# confirm SOC  and industry added as expected
job_fields %>% 
        dplyr::select(Occupation_Code, industry, industry_name, SOC_level, SOC.Name) %>% 
        filter(SOC_level == 272)
# SOC 272 is Entertainers and performers as expected and industry 27 is Arts, Design...

str(job_fields)

top_industries <- job_fields %>% group_by(industry_name) %>% 
        summarise(Median_Dur = median(Posting_Duration), n = n()) %>% 
        filter(!is.na(industry_name), Median_Dur > 15, n > 1050) %>% 
        arrange(desc(n))

# top unfilled industires are: 
# 1. Management with 6103 listings
# 2. Sales and Related with 5564 listings
# 3. Computer and Mathematical with 5108 listings
# 4. Office and Administrative Support with 4455 listings
# 5. Business and Financial Operations with 4121 listings
# 6. Healthcare Practitioners and Technical with 3423 listings


# Within these fields, for which positions is there the greatest unfilled demand (by your definition)?
# look at occupation names with highest number of listings        
top_fields <- job_fields %>% 
        filter(industry_name %in% top_industries$industry_name,  
               Posting_Duration > 75)

top_fields <- droplevels(top_fields)
table(top_fields$industry_name)
top_fields <- top_fields %>% left_join(occupations)

top_positions <- top_fields %>% 
        group_by(industry_name, Occupation_Code) %>% 
        summarise(n = n()) %>% 
        arrange(industry_name, desc(n)) %>% 
        slice(1:3) %>% 
        left_join(occupations)

top_fields %>% filter(industry_name %in% "Sales and Related") %>% 
        group_by(Occupation_Name) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n))
        
# top unfilled positions per in demand industries are:

# -- Management --
# 1. Financial Manager 
# 2. Program Manager
# 3. Store Manager

# -- Computer and Mathematical --
# 1. Software Developer / Engineer
# 2. Business Intelligence Analyst
# 3. IT Project Manager

# -- Healthcare Practitioners and Technical --
# 1. Registered Nurse 
# 2. Physician
# 3. Nurse Practitioner

# -- Business and Financial Operations --
# 1. Business / Management Analyst
# 2. Auditor
# 3. Account Manager / Representative

# -- Office and Administrative Support --
# 1. Stocking Clerk / Sales Floor Support
# 2. Customer Service Representative
# 3. Warehouse / Inventory Associate

# -- Sales and Related --
# 1. Retail Sales Associate
# 2. Cashier
# 3. Sales Representative


# Review in demand positions ---------------------------------------------------

# What are the demographics of these positions—specifically:
# What level of education is required to perform these jobs?

top_pos <- nyc %>% filter(Occupation_Code %in% top_positions$Occupation_Code) %>% 
        left_join(jobs_degrees) %>% left_join(degrees) %>% left_join(occupations)
names(top_pos)

ggplot(data = filter(top_pos, !Degree_Level %in% "Unknown"), 
       mapping = aes(x = Degree_Level)) + geom_bar() + coord_flip() +
        facet_wrap(~ Occupation_Name, nrow = 3, scales = "free_x") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) + 
        ggtitle("Level of Education for Top Unfilled Occupations") + ylab("") + xlab("")

# all different levels of education are required for the different occupations

# Which particular skills are required to perform these jobs? ------------------

top_skills <- nyc_occs %>% 
        filter(Occupation_Code %in% top_positions$Occupation_Code) %>% 
        left_join(jobs_skills) %>% left_join(skills) 

top_temp <- nyc_occs %>% 
        filter(Occupation_Code %in% top_positions$Occupation_Code) %>% 
        left_join(jobs_skills) %>% left_join(skills) %>% 
        group_by(Skill_Name) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n))

require(RColorBrewer)
pal <- brewer.pal(9, "Greens")

wordcloud(top_temp$Skill_Name, top_temp$n, max.words = 50, random.order = FALSE,
          scale = c(2,.25), colors = pal, use.r.layout = TRUE)
title(main = "Top Skills Across All In Demand Positions")
# Top skills across positions
# 1. Planning
# 2. Communication Skills
# 3. Writing
# 4. Organizational Skills
# 5. Problem Solving

# Broken down by Occupation
# -- Management --
# 1. Financial Manager 
top_temp <- nyc_occs %>% 
        filter(Occupation_Code == 11303100) %>% 
        left_join(jobs_skills) %>% left_join(skills) %>% 
        group_by(Skill_Name) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n))

pal <- brewer.pal(9, "Blues")
wordcloud(top_temp$Skill_Name, top_temp$n, max.words = 50, random.order = FALSE,
          scale = c(2,.25), colors = pal, use.r.layout = TRUE)
title(main = "Top Skills for Financial Managers")

# 2. Program Manager
top_temp <- nyc_occs %>% 
        filter(Occupation_Code == 11919900) %>% 
        left_join(jobs_skills) %>% left_join(skills) %>% 
        group_by(Skill_Name) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n))

pal <- brewer.pal(9, "Reds")
wordcloud(top_temp$Skill_Name, top_temp$n, max.words = 50, random.order = FALSE,
          scale = c(2,.25), colors = pal, use.r.layout = TRUE)
title(main = "Top Skills for Program Managers")

# -- Computer and Mathematical --
# 1. Software Developer / Engineer
top_temp <- nyc_occs %>% 
        filter(Occupation_Code == 15113100) %>% 
        left_join(jobs_skills) %>% left_join(skills) %>% 
        group_by(Skill_Name) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n))

pal <- brewer.pal(9, "Oranges")
wordcloud(top_temp$Skill_Name, top_temp$n, max.words = 50, random.order = FALSE,
          scale = c(2,.25), colors = pal, use.r.layout = TRUE)
title(main = "Top Skills for Software Developers")

# 2. Business Intelligence Analyst 
top_temp <- nyc_occs %>% 
        filter(Occupation_Code == 15119993) %>% 
        left_join(jobs_skills) %>% left_join(skills) %>% 
        group_by(Skill_Name) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n))

pal <- brewer.pal(9, "Purples")
wordcloud(top_temp$Skill_Name, top_temp$n, max.words = 50, random.order = FALSE,
          scale = c(2,.25), colors = pal, use.r.layout = TRUE)
title(main = "Top Skills for Business Intelligence Analysts")

# -- Healthcare Practitioners and Technical --
# 1. Registered Nurse 
top_temp <- nyc_occs %>% 
        filter(Occupation_Code == 29114100) %>% 
        left_join(jobs_skills) %>% left_join(skills) %>% 
        group_by(Skill_Name) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n))

pal <- brewer.pal(9, "BuGn")
wordcloud(top_temp$Skill_Name, top_temp$n, max.words = 50, random.order = FALSE,
          scale = c(2,.25), colors = pal, use.r.layout = TRUE)
title(main = "Top Skills for Business Intelligence Analysts")

# 2. Physician
top_temp <- nyc_occs %>% 
        filter(Occupation_Code == 29106200) %>% 
        left_join(jobs_skills) %>% left_join(skills) %>% 
        group_by(Skill_Name) %>% 
        summarise(n = n()) %>% 
        filter(!is.na(Skill_Name)) %>% 
        arrange(desc(n))

pal <- brewer.pal(9, "OrRd")

wordcloud(top_temp$Skill_Name, top_temp$n, max.words = 50, random.order = FALSE,
          scale = c(2,.25), colors = pal, use.r.layout = TRUE)

title(main = "Top Skills for Physicians")

# -- Food Preparation and Serving Related --
# 1. Cook
top_temp <- nyc_occs %>% 
        filter(Occupation_Code == 35201100) %>% 
        left_join(jobs_skills) %>% left_join(skills) %>% 
        group_by(Skill_Name) %>% 
        summarise(n = n()) %>% 
        filter(!is.na(Skill_Name)) %>% 
        arrange(desc(n))

pal <- brewer.pal(9, "PuBu")
wordcloud(top_temp$Skill_Name, top_temp$n, max.words = 50, random.order = FALSE,
          scale = c(2,1), colors = pal, use.r.layout = TRUE)

title(main = "Top Skills for Cooks")

# 2. Bartender
top_temp <- nyc_occs %>% 
        filter(Occupation_Code == 35301100) %>% 
        left_join(jobs_skills) %>% left_join(skills) %>% 
        group_by(Skill_Name) %>% 
        summarise(n = n()) %>% 
        filter(!is.na(Skill_Name)) %>% 
        arrange(desc(n))

pal <- brewer.pal(9, "YlGn")
wordcloud(top_temp$Skill_Name, top_temp$n, max.words = 50, random.order = FALSE,
          scale = c(2,1), colors = pal, use.r.layout = TRUE)
title(main = "Top Skills for Bartenders")

# How many years of experience are required to perform these jobs? -------------
top_pos <- top_pos %>% left_join(exp_levels)
names(top_pos)
ggplot(data = filter(top_pos, !Experience_Level %in% "Not available"), 
       mapping = aes(x = Experience_Level)) + geom_bar() + coord_flip() +
        facet_wrap(~ Occupation_Name, nrow = 2, scales = "free_x") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) + 
        ggtitle("Experience Level for Top Unfilled Occupations")+ ylab("") + xlab("")

# Are there any special certifications required? -------------------------------

# Broken down by Occupation
# -- Management --
# 1. Financial Manager 
top_temp <- nyc_occs %>% 
        filter(Occupation_Code == 11303100) %>% 
        left_join(jobs_certs) %>% left_join(certs) %>% 
        group_by(Cert_Name) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n)) %>% 
        slice(1:10)

ggplot(data = top_temp) + geom_bar(mapping = aes(x = Cert_Name, y = n), 
                                   stat = "identity") + coord_flip() +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
        ggtitle("Certifications for Financial Managers") + ylab("") + xlab("")

# 2. Program Manager
top_temp <- nyc_occs %>% 
        filter(Occupation_Code == 11919900) %>% 
        left_join(jobs_certs) %>% left_join(certs) %>% 
        group_by(Cert_Name) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n)) %>% 
        slice(1:10)

ggplot(data = top_temp) + geom_bar(mapping = aes(x = Cert_Name, y = n), 
                                   stat = "identity") + coord_flip() +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
        ggtitle("Certifications for Program Managers") + ylab("") + xlab("")

# -- Computer and Mathematical --
# 1. Software Developer / Engineer
top_temp <- nyc_occs %>% 
        filter(Occupation_Code == 15113100) %>% 
        left_join(jobs_certs) %>% left_join(certs) %>% 
        group_by(Cert_Name) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n)) %>% 
        slice(1:10)

ggplot(data = top_temp) + geom_bar(mapping = aes(x = Cert_Name, y = n), 
                                   stat = "identity") + coord_flip() +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
        ggtitle("Certifications for Software Developers") + ylab("") + xlab("")

# 2. Business Intelligence Analyst 
top_temp <- nyc_occs %>% 
        filter(Occupation_Code == 15119993) %>% 
        left_join(jobs_certs) %>% left_join(certs) %>% 
        group_by(Cert_Name) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n)) %>% 
        slice(1:10)

ggplot(data = top_temp) + geom_bar(mapping = aes(x = Cert_Name, y = n), 
                                   stat = "identity") + coord_flip() +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
        ggtitle("Certifications for Business Intelligence Analysts") + ylab("") + xlab("")

# -- Healthcare Practitioners and Technical --
# 1. Registered Nurse 
top_temp <- nyc_occs %>% 
        filter(Occupation_Code == 29114100) %>% 
        left_join(jobs_certs) %>% left_join(certs) %>% 
        group_by(Cert_Name) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n)) %>% 
        slice(1:10)

ggplot(data = top_temp) + geom_bar(mapping = aes(x = Cert_Name, y = n), 
                                   stat = "identity") + coord_flip() +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
        ggtitle("Certifications for Registered Nurse") + ylab("") + xlab("")

# 2. Physician
top_temp <- nyc_occs %>% 
        filter(Occupation_Code == 29106200) %>% 
        left_join(jobs_certs) %>% left_join(certs) %>% 
        group_by(Cert_Name) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n)) %>% 
        slice(1:10)

ggplot(data = top_temp) + geom_bar(mapping = aes(x = Cert_Name, y = n), 
                                   stat = "identity") + coord_flip() +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
        ggtitle("Certifications for Physician") + ylab("") + xlab("")

# -- Food Preparation and Serving Related --
# 1. Cook
top_temp <- nyc_occs %>% 
        filter(Occupation_Code == 35201100) %>% 
        left_join(jobs_certs) %>% left_join(certs) %>% 
        group_by(Cert_Name) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n)) %>% 
        slice(1:10)

ggplot(data = top_temp) + geom_bar(mapping = aes(x = Cert_Name, y = n), 
                                   stat = "identity") + coord_flip() +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
        ggtitle("Certifications for Cook") + ylab("") + xlab("")

# 2. Bartender
top_temp <- nyc_occs %>% 
        filter(Occupation_Code == 35301100) %>% 
        left_join(jobs_certs) %>% left_join(certs) %>% 
        group_by(Cert_Name) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n)) %>% 
        slice(1:10)

ggplot(data = top_temp) + geom_bar(mapping = aes(x = Cert_Name, y = n), 
                                   stat = "identity") + coord_flip() +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
        ggtitle("Certifications for Bartender") + ylab("") + xlab("")

# Review salary for top positions ----------------------------------------------



# Compare other regions --------------------------------------------------------

