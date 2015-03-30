devtools::install_github("hrbrmstr/waffle", ref="cran")
library(extrafont)
library(waffle)
# GED
ged <- c(`No GED` = 7, `GED`=13)
waffle(ged, rows = 2, 
       colors=c("#E6E6E6", "#8A0829"),
       use_glyph="child",
       glyph_size = 24,
       size = 1,
       reverse = TRUE,
       title = "13 out of 20 participants who took the test obtained their GED\n\n(65% pass rate)\n") + 
  theme(
    plot.title = element_text(family = 'Calibri', face = 'bold'),
    legend.position = 'none')

ggsave(filename = '../temp_data/ged.png', width = 25, height = 15, units = 'cm')

waffle(ged, rows = 2, 
       colors=c("#E6E6E6", "#8A0829"),
       #use_glyph="child",
       #glyph_size = 24,
       size = 1,
       reverse = TRUE,
       xlab = '1 square = 1 youth',
       title = "13 out of 20 participants who took the test obtained their GED\n\n(65% pass rate)\n") + 
  theme(
    plot.title = element_text(family = 'Calibri', face = 'bold'),
    axis.title.x = element_text(family = 'Calibri', face = 'bold'),
    legend.position = 'none')

ggsave(filename = '../temp_data/ged2.png', width = 25, height = 15, units = 'cm')

# Employment
job <- c(`No GED` = 30, `GED`=48)
waffle(job, rows = 5, 
       colors=c("#E6E6E6", "#8A0829"),
       use_glyph="child",
       glyph_size = 15,
       size = 1,
       reverse = TRUE,
       title = "48 out of 78 participants who were ready for employment obtained a job\n\n(61%)\n") + 
  theme(
    plot.title = element_text(family = 'Calibri', face = 'bold'),
    legend.position = 'none')

ggsave(filename = '../temp_data/job.png', width = 25, height = 15, units = 'cm')

waffle(job, rows = 5, 
       colors=c("#E6E6E6", "#8A0829"),
       #use_glyph="child",
       #glyph_size = 15,
       size = 1,
       reverse = TRUE,
       xlab = '1 square = 1 youth',
       title = "48 out of 78 participants who were ready for employment obtained a job\n\n(61%)\n") + 
  theme(
    plot.title = element_text(family = 'Calibri', face = 'bold'),
    axis.title.x = element_text(family = 'Calibri', face = 'bold'),
    legend.position = 'none')

ggsave(filename = '../temp_data/job2.png', width = 25, height = 15, units = 'cm')

# Retention
retention <- c(`No GED` = 5, `GED`= 43)
waffle(retention, rows = 3, 
       colors=c("#E6E6E6", "#8A0829"),
       use_glyph="child",
       glyph_size = 15,
       size = 1,
       reverse = TRUE,
       title = "43 out of 48 participants who obtained a job remained employed for more than 90 days\n\n(89%)\n") + 
  theme(
    plot.title = element_text(family = 'Calibri', face = 'bold'),
    legend.position = 'none')

ggsave(filename = '../temp_data/retention.png', width = 25, height = 15, units = 'cm')

waffle(retention, rows = 3, 
       colors=c("#E6E6E6", "#8A0829"),
       #use_glyph="child",
       #glyph_size = 15,
       size = 1,
       reverse = TRUE,
       xlab = '1 square = 1 youth',
       title = "43 out of 48 participants who obtained a job remained employed for more than 90 days\n\n(89%)\n") + 
  theme(
    plot.title = element_text(family = 'Calibri', face = 'bold'),
    axis.title.x = element_text(family = 'Calibri', face = 'bold'),
    legend.position = 'none')

ggsave(filename = '../temp_data/retention2.png', width = 25, height = 15, units = 'cm')

# Summer internship
summer <- c(`No GED` = 8, `GED`= 149)
waffle(summer, rows = 6, 
       colors=c("#E6E6E6", "#8A0829"),
       use_glyph="child",
       glyph_size = 10,
       size = 0.5,
       reverse = TRUE,
       title = "149 out of 157 participants who enrolled in Summer Program obtained an internship\n\n(95%)\n") + 
  theme(
    plot.title = element_text(family = 'Calibri', face = 'bold'),
    legend.position = 'none')

ggsave(filename = '../temp_data/summer.png', width = 25, height = 15, units = 'cm')

waffle(summer, rows = 6, 
       colors=c("#E6E6E6", "#8A0829"),
       #use_glyph="child",
       #glyph_size = 10,
       size = 0.5,
       reverse = TRUE,
       xlab = '1 square = 1 youth',
       title = "149 out of 157 participants who enrolled in Summer Program obtained an internship\n\n(95%)\n") + 
  theme(
    plot.title = element_text(family = 'Calibri', face = 'bold'),
    axis.title.x = element_text(family = 'Calibri', face = 'bold'),
    legend.position = 'none')

ggsave(filename = '../temp_data/summer2.png', width = 25, height = 15, units = 'cm')
