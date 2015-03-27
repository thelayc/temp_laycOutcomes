# Load libraries
library(dplyr)
library(stringr)
library(laycUtils)
library(laycEnrollment)
library(gmodels)

# Load data-------
list.files('../temp_data/fy14', full.names = TRUE) 

enroll_path <- ('../temp_data/fy14/raw_enrollment_report.txt')
pos_path <- ('../temp_data/fy14/raw_pos_report.txt')
tp_path <- ('../temp_data/fy14/raw_touchpoint_report_detailed.txt')
asmt_path <- ('../temp_data/fy14/raw_assessment_report.txt')
ref_path <- ('../temp_data/fy14/raw_referrals_report.txt')
job_path <- ("../temp_data/fy14/raw_job_report.txt")
ptype_path <- ('../temp_data/fy14/program_type.txt')

enroll <- load_txt(enroll_path)
enroll <- laycUtils::clean_data(enroll)

ptype <- load_txt(ptype_path)
ptype <- laycUtils::clean_data(ptype)

tp <- load_txt(tp_path)
tp <- laycUtils::clean_data(tp)
tp_col <- colnames(tp)
tp_col[1] <- 'subject_id'
colnames(tp) <- tp_col

job <- load_txt(job_path)
job <- laycUtils::clean_data(job)

pos <- load_txt(pos_path)
pos <- clean_data(pos)

#ppm <- load_txt("./input/ppm_tp.txt")

# OPTIONAL: choose specific programs --------------------------------------
ss_workforce <- str_detect(enroll$program_name, '^ss -')
ss_workforce <- unique(enroll$program_name[ss_workforce])
ss_workforce <- ss_workforce[!ss_workforce %in% c("ss - college 101 (disabled)", "ss - full circle brotherhood")]

pos %>% filter(program_name %in% ss_workforce) -> pos

# Get active workforce participants ---------------------------------------

start <- '07/01/2012'
end <- '06/30/2014'
enroll <- clean_enroll(enroll, ptype, 
                       start_date = start,
                       end_date = end)
# Overall
enroll %>%
  filter(program_name %in% ss_workforce) %>%
  select(subject_id) %>%
  distinct %>%
  count

# Breakdown by program
enroll %>%
  filter(program_name %in% ss_workforce) %>%
  group_by(program_name) %>%
  summarise(n = length(unique(subject_id))) ->
  temp

sum(temp$n)

# get case management youth with service activity
pos %>%
  filter(program_name == "ss - case management") %>%
  select(subject_id) %>%
  distinct %>%
  count
  

# Get dual enrollments ---------------------------------------------
# enroll %>%
#   filter(program_name %in% ss_workforce) %>%
#   select(subject_id, program_name) %>%
#   distinct ->
#   dual_enroll
# 
# test <- as.data.frame(table(dual_enroll$subject_id, dual_enroll$program_name))
# CrossTable(dual_enroll$program_name, dual_enroll$program_name,
#            prop.t = TRUE, prop.chisq = FALSE)

# Case management
enroll %>%
  filter(program_name == "ss - case management") %>%
  select(subject_id) %>%
  distinct ->
  cm

# Summer internship
enroll %>%
  filter(program_name == "ss - summer internship") %>%
  select(subject_id) %>%
  distinct ->
  summer

# ged
enroll %>%
  filter(program_name == "ss - ged") %>%
  select(subject_id) %>%
  distinct ->
  ged

# Job placement
enroll %>%
  filter(program_name == "ss - job placement") %>%
  select(subject_id) %>%
  distinct ->
  job

# counseling
enroll %>%
  filter(program_name == "ss - counseling") %>%
  select(subject_id) %>%
  distinct ->
  counseling

# JRT
enroll %>%
  filter(program_name == "ss - job readiness") %>%
  select(subject_id) %>%
  distinct ->
  jrt

# ccorps
enroll %>%
  filter(program_name == "ss - ccorps projects") %>%
  select(subject_id) %>%
  distinct ->
  ccorps

count(inner_join(cm, ged)) # cm vs ged
count(inner_join(cm, jrt)) 
count(inner_join(cm, job))
count(inner_join(cm, summer))
count(inner_join(cm, counseling))
count(inner_join(cm, ccorps))
count(inner_join(ccorps, counseling))
count(inner_join(ccorps, ged))
count(inner_join(ccorps, job))
count(inner_join(ccorps, jrt))
count(inner_join(ccorps, summer))
count(inner_join(counseling, ged))
count(inner_join(counseling, job))
count(inner_join(counseling, jrt))
count(inner_join(counseling, summer))
count(inner_join(ged, job))
count(inner_join(ged, jrt))
count(inner_join(ged, summer))
count(inner_join(job, jrt))
count(inner_join(job, summer))
count(inner_join(jrt, summer))

# Get active JRT participants ---------------------------------------------

enroll %>%
  filter(program_name == "ss - job readiness") %>%
  select(subject_id) %>%
  distinct %>%
  count

# Get active Job Placement participants -----------------------------------

enroll %>%
  filter(program_name == 'ss - job placement') %>%
  select(subject_id) %>%
  distinct %>%
  count

# Case management

# Get active GED participants --------------------------------------------
enroll %>%
  filter(program_name == 'ss - ged') %>%
  select(subject_id) %>%
  distinct %>%
  count

# Get active Conservation Corps participants -----------------------------
enroll %>%
  filter(program_name == "ss - ccorps projects") %>%
  select(subject_id) %>%
  distinct %>%
  count

# Get Soft skills increase ------------------------------------------------
tp %>%
  filter(tp_name == "jrt pre-/post test" & program_name == "ss - job readiness") ->
  jrt_tp

jrt_tp %>%
  group_by(subject_id) %>%
  mutate(first = min(date),
         last = max(date)) ->
  jrt_tp

jrt_tp$prepost[jrt_tp$date == jrt_tp$first] <- 'pre'
jrt_tp$prepost[jrt_tp$date == jrt_tp$last] <- 'post'



 jrt_tp %>%
  select(subject_id, answer_id, question_id, question_short, answer_weight, prepost) %>%
  filter(stringr::str_detect(question_short, '^q')) %>%
  mutate(answer_weight = as.numeric(answer_weight)) %>%
  group_by(subject_id, answer_id) %>%
  mutate(score = sum(answer_weight)) %>%
  ungroup() %>%
  select(subject_id, prepost, score) %>%
  distinct %>%
  #group_by(subject_id) %>%
  #mutate(check = length(unique(prepost))) -> test
  filter(!is.na(prepost)) %>%
  tidyr::spread(prepost, score) %>%
  mutate(change = post - pre) %>%
  filter(!is.na(change)) ->
  jrt_tp

jrt_tp$change_ord[jrt_tp$change > 0] <- 'positive'
jrt_tp$change_ord[jrt_tp$change == 0] <- 'no change'
jrt_tp$change_ord[jrt_tp$change < 0] <- 'negative'

table(jrt_tp$change_ord)          

# Sat for GED exam (OLD) --------------------------------------------
pos$pos_name %>% str_replace_all('\\*', '') -> pos$pos_name
pos$pos_name %>% str_replace_all(' ', '_') -> pos$pos_name
to_keep <- c("ged_total_score")

pos %>%
  filter(stringr::str_detect(pos_name, '^ged')) %>%
  filter(pos_name %in% to_keep) %>%
  mutate(pos_value = as.numeric(pos_value)) %>%
  filter(pos_value > 0) %>%
  select(subject_id) %>%
  distinct %>%
  count

# Sat for GED exam (NEW) --------------------------------------------
## To Review: NEW GED
# math <- str_detect(pos$pos_name, 'math')
# pos$pos_name[math] <- 'ged_math'
# language <- str_detect(pos$pos_name, 'language')
# pos$pos_name[language] <- 'ged_language'
# science <- str_detect(pos$pos_name, 'science')
# pos$pos_name[science] <- 'ged_science'
# social_studies <- str_detect(pos$pos_name, 'social_studies')
# pos$pos_name[social_studies] <- 'ged_social_studies'
# 
# pos$pos_name %>% str_replace_all('\\*', '') -> pos$pos_name
# pos$pos_name %>% str_replace_all(' ', '_') -> pos$pos_name
# 
# 
# to_remove <- c("ged_total_score", "ged_average_score", "ged_academy_attendance")


# pos %>%
#   filter(stringr::str_detect(pos_name, '^ged')) %>%
#   filter(!pos_name %in% to_remove) %>%
#   mutate(taken = 1) %>%
#   select(subject_id, date,pos_name, taken) %>%
#   distinct %>%
#   tidyr::spread(pos_name, taken) %>%
#   mutate(sumrow = rowSums(.[3:8], na.rm = TRUE)) %>%
#   group_by(subject_id) %>%
#   summarise_each(funs(max(., na.rm = TRUE))) %>%
#   mutate(ged_sections = sum(sumrow)) %>%
#   ungroup %>%
#   select(subject_id, ged_sections) %>%
#   distinct %>%
#   mutate(ged_taken = ifelse(ged_sections >= 4, 1, 0)) %>%
#   filter(ged_taken > 0) %>%
#   distinct %>%
#   count
  


select(subject_id) %>%
  distinct %>%
  count

# Get GED diploma ---------------------------------------------------------
enroll %>%
  filter(program_name == 'ss - ged') %>%
  filter(successfully_completed == 'yes') %>%
  select(subject_id) %>%
  distinct %>%
  count

# Get Job placement  -----------------------------------------------
enroll %>%
  filter(program_name %in% ss_workforce) %>%
  select(subject_id) %>%
  distinct %>%
  inner_join(job, by = 'subject_id') %>%
  mutate(job_start = lubridate::mdy(job_start),
         program_start = lubridate::mdy(program_start)) %>%
  filter(program_start < job_start) %>%
  distinct %>%
  count

# From job placement
enroll %>%
  filter(program_name == 'ss - job placement') %>%
  select(subject_id) %>%
  distinct %>%
  inner_join(job, by = 'subject_id') %>%
  mutate(job_start = lubridate::mdy(job_start),
         program_start = lubridate::mdy(program_start)) %>%
  filter(program_start < job_start) %>%
  distinct %>%
  count

# Job placement: ccorps
enroll %>%
  filter(program_name %in% c('ss - ccorps projects')) %>%
  select(subject_id) %>%
  distinct %>%
  inner_join(job, by = 'subject_id') %>%
  mutate(job_start = lubridate::mdy(job_start),
         program_start = lubridate::mdy(program_start)) %>%
  filter(program_start < job_start) %>%
  distinct %>%
  count

# From Summer internship
enroll %>%
  filter(program_name %in% c('ss - summer internship')) %>%
  select(subject_id) %>%
  distinct %>%
  inner_join(job, by = 'subject_id') %>%
  mutate(job_start = lubridate::mdy(job_start),
         program_start = lubridate::mdy(program_start)) %>%
  filter(program_start < job_start) %>%
  distinct %>%
  count

# Get Job retention  -----------------------------------------------
enroll %>%
  filter(program_name == 'ss - job placement') %>%
  select(subject_id) %>%
  distinct %>%
  inner_join(job, by = 'subject_id') %>%
  mutate(job_start = lubridate::mdy(job_start),
         program_start = lubridate::mdy(program_start)) %>%
  filter(program_start < job_start) %>%
  mutate(days_employed = as.numeric(days_employed)) %>%
  filter(days_employed > 90) %>%
  distinct %>%
  count

# Job retention conservation corps
enroll %>%
  filter(program_name %in% c('ss - ccorps projects')) %>%
  select(subject_id) %>%
  distinct %>%
  inner_join(job, by = 'subject_id') %>%
  mutate(job_start = lubridate::mdy(job_start),
         program_start = lubridate::mdy(program_start)) %>%
  filter(program_start < job_start) %>%
  mutate(days_employed = as.numeric(days_employed)) %>%
  filter(days_employed > 90) %>%
  distinct %>%
  count

# Job retention for summer internship
enroll %>%
  filter(program_name %in% c('ss - summer internship')) %>%
  select(subject_id) %>%
  distinct %>%
  inner_join(job, by = 'subject_id') %>%
  mutate(job_start = lubridate::mdy(job_start),
         program_start = lubridate::mdy(program_start)) %>%
  filter(program_start < job_start) %>%
  mutate(days_employed = as.numeric(days_employed)) %>%
  filter(days_employed > 90) %>%
  distinct %>%
  count


