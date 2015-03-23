# Load libraries
library(dplyr)
library(stringr)
library(laycUtils)
library(laycEnrollment)

# Load data
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
job <- load_txt(job_path)
job <- laycUtils::clean_data(job)
ppm <- load_txt("./input/ppm_tp.txt")

# Get youth in workforce
start <- '01/01/2008'
end <- '09/30/2014'
enroll <- clean_enroll(enroll, ptype, 
                       start_date = start,
                       end_date = end)

# Get youth in job placement
enroll %>%
  filter(str_detect(program_name, 'job placement')) ->
  job_ready

job_ready$id %>% unique %>% length

# Get youth in GED
enroll %>%
  filter(str_detect(program_name, 'ged')) ->
  ged

ged$id %>% unique %>% length
