library(tidyverse)

grade_file_dir <- glue::glue(
  "/Users/sap_c/OneDrive - University of Florida/\\
  Teaching/STA522_spring2021/2021/graded_final_exam/"
)
grade_file <- glue::glue(
  "{grade_file_dir}/\\
  gc_2211_13531_fullgc_2021-05-17-07-51-46.xlsx"
)
dat_grade <- readxl::read_xlsx(grade_file)

is_between <- function(x, lower, upper) {
  (lower <= x) & (x <= upper)
}

determine_letter_grade <- function(score) {
  dplyr::case_when(
    # A = 90 or above
    is_between(score, 90, 100) ~ "A", 
    # 
    # A- = 85-89
    is_between(score, 85, 89) ~"A-",
    # 
    # B+ = 78-84
    is_between(score, 78, 84) ~"B+",
    # 
    # B = 69-77
    is_between(score, 69, 77) ~"B",
    # 
    # B- = 64-68
    is_between(score, 64, 68) ~"B-",
    # 
    # C+ = 59-63
    is_between(score, 59, 63) ~"C+",
    # 
    # C = 54-58
    is_between(score, 54, 63) ~"C",
    # 
    # C- = 49-53
    is_between(score, 49, 53) ~"C-",
    # 
    # D+ = 44-48
    is_between(score, 44, 48) ~"D+",
    # 
    # D = 39-43
    # 
    # D- = 34-38
    is_between(score, 34, 43) ~"D",
    # 
    # F = 32 or below
    is_between(score, 0, 33) ~"F",
    
    TRUE ~ NA_character_
  )
}

# A = 90 or above
# 
# A- = 85-89
# 
# B+ = 78-84
# 
# B = 69-77
# 
# B- = 64-68
# 
# C+ = 59-63
# 
# C = 54-58
# 
# C- = 49-53
# 
# D+ = 44-48
# 
# D = 39-43
# 
# D- = 34-38
# 
# F = 33 or below

dat_grade_1 <- dat_grade %>% 
  mutate(
    temp_Exam1 = `Exam 1 (**Webcam**) - Requires Respondus LockDown Browser [Total Pts: 135 Score] |1497552`,
    temp_Exam2 = `Exam 2 (**Webcam**) - Requires Respondus LockDown Browser [Total Pts: 105 Score] |1509640`,
    temp_Final = `Final Exam (**Webcam**) - Requires Respondus LockDown Browser [Total Pts: 160 Score] |1520945`
  ) %>% 
  mutate(
    Final_rewt = temp_Final * 100 / 150,
    `Cumulative Score` = (
      0.3 * temp_Exam1 + 
        0.3 * temp_Exam2 + 
        0.4 * Final_rewt
    ) %>% 
      ceiling(),
    `Final Score` = (
      pmax(`Cumulative Score`, Final_rewt) + 1
    ) %>% 
      pmin(100) %>% 
      ceiling(),
    `Letter Grade` = determine_letter_grade(`Final Score`)
  )  %>%
  rename(`Final Score (Percent)` = `Final Score`) %>% 
  select(`Last Name`, `First Name`, Username, `Student ID`,
         `Final Exam (**Webcam**) - Requires Respondus LockDown Browser [Total Pts: 160 Score] |1520945`,
         `Cumulative Score`, `Final Score (Percent)`, `Letter Grade`)

dat_grade_1 %>% select(`First Name`, `Last Name`, `Cumulative Score`, `Final Score (Percent)`, `Letter Grade`)

dat_grade_1 %>% 
  write_csv(
    glue::glue(
      "{grade_file_dir}/final_grades_calculated.csv"
    )
  )
