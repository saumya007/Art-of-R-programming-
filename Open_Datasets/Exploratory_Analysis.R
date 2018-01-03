# exploring OULAD dataset . 
library("tidyverse")
library("oulad")
library("tibble")
library("dplyr")
library("ggplot2")
# assessment data set.

#conversion to tibble and refining the data set from data frames and conversion to 
# proper data type for columns.
assessment1 <- as_tibble(assessment)


assessment1 %>%
  group_by(assessment_type)%>%
  filter(!is.na(date))%>%
  summarise(sum_types = n()) %>%
  

assessment1 %>%
  group_by(code_presentation)%>%
  filter(!is.na(code_presentation))%>%
  summarise(sum_codes = n())

assessment1 %>%
  group_by(code_module)%>%
  filter(!is.na(code_module))%>%
  summarise(sum_codes = n())

assessment1 %>%
  group_by(assessment_type, code_presentation)%>%
  filter(!is.na(code_presentation), assessment_type == "CMA")%>%
  summarise(sum_codes = n())

assessment1 %>%
  group_by(assessment_type, code_presentation)%>%
  filter(!is.na(code_presentation), assessment_type == "TMA")%>%
  summarise(sum_codes = n())

assessment1 %>%
  group_by(assessment_type, code_presentation)%>%
  filter(!is.na(code_presentation), assessment_type == "Exam")%>%
  summarise(sum_codes = n())
assessment1 %>%
  group_by(assessment_type) %>%
  ggplot(mapping = aes(x = assessment_type, group = interaction(code_module, code_presentation),y  = date)) +
  geom_line(mapping = aes(color = code_presentation))+
  geom_point(mapping = aes(color = code_module))

# student . student registration , student assessment and student vle 

# student ..
View(student)
student1 <- as_tibble(student)

student1%>%
  group_by(code_module)%>%
  summarise(n = n_distinct(id_student))%>%
  ggplot(mapping = aes(y = n, x = code_module, group = 1))+
  geom_line(mapping = aes(color = n))+
  geom_point(mapping = aes(color = n))# students taking a particular course

# students  in a particular
# semester
student1%>%
  group_by(code_presentation)%>%
  summarise(n = n_distinct(id_student))%>%
  ggplot(mapping = aes(y = n, x = code_presentation,group = 1))+
  geom_line(mapping = aes(color = n))+
  geom_point(mapping = aes(color = n))  

# students taking a particular course in a particular
# semester
student1%>%
  group_by(code_module, code_presentation)%>%
  summarise(n = n_distinct(id_student))%>%
  ggplot(mapping = aes(y = code_module, x = code_presentation, group = 1))+
  geom_count(mapping = aes(size = n, color = n)) 

# gender based demographic of students taking a course in a semester
student1%>%
  group_by(code_module, code_presentation, gender)%>%
  summarise(n = n_distinct(id_student))%>%
  ggplot(mapping = aes(y = code_module, x = code_presentation, group = 1))+
  geom_count(mapping = aes(size = n, color = gender)) 

# students belonging to certain region
student1%>%
  group_by(code_module, code_presentation, region)%>%
  summarise(n = n_distinct(id_student))%>%
  ggplot(mapping = aes(y = code_module, x = code_presentation, group = 1))+
  geom_count(mapping = aes(size = n, color = region)) 

# students in certain age groups ..
student1%>%
  group_by(code_module, code_presentation, age_band)%>%
  summarise(n = n_distinct(id_student))%>%
  ggplot(mapping = aes(y = code_module, x = code_presentation, group = 1))+
  geom_count(mapping = aes(size = n, color = age_band)) 

  # highest education of students ..
  
  student1%>%
    group_by(code_module, code_presentation, highest_education)%>%
    summarise(n = n_distinct(id_student))%>%
  ggplot(mapping = aes(y = code_module, x = code_presentation, group = 1))+
    geom_count(mapping = aes(size = n,color = highest_education))
  
  # mappingas per number of previous attempts ..
  
  student1%>%
    group_by(code_module, code_presentation, highest_education)%>%
    summarise(n = n_distinct(id_student))%>%
    ggplot(mapping = aes(y = code_module, x = code_presentation, group = 1))+
    geom_count(mapping = aes(size = n,color = highest_education))

  # relationship between imd band and number of previous attempts ..
  student1%>%
    group_by(code_module, code_presentation, imd_band)%>%
    filter(!is.na(imd_band))%>%
    summarise(n = n_distinct(num_of_prev_attempts))
    ggplot(mapping = aes(y = imd_band, x = num_of_prev_attempts, group = 1))+
    geom_boxplot(mapping = aes(color = n)) # no strong inference from here.. 
  
  # relation between imd_band and final_result ..
    student1%>%
      group_by(imd_band, final_result) %>%
      summarise(n = n_distinct(id_student)) %>%
      ungroup() %>%
      group_by(imd_band) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ungroup() %>%
      ggplot(aes(imd_band, pct))+
      geom_col(aes(fill = final_result)) # no strong inference from here.. 
    

  # analysing vle
    # anaylysis of the type of contents and amount of each of them
    vle1 <- as_tibble(vle)
    vle1 %>% 
      filter(!is.na(week_from), !is.na(week_to))%>%
      ggplot(aes(activity_type))+
      geom_bar(aes(fill = activity_type))
  # analysis of the type of contents and its course duration by student.
    vle1 %>% 
      filter(!is.na(week_from), !is.na(week_to))%>%
      mutate(duration = week_to - week_from)%>%
      ggplot(aes(activity_type, duration))+
      geom_boxplot(aes(color = activity_type))
# lower value indicates the course has to be completed in the same week.
    
# performing joins to analyse further data ..
    #student and course join ..
    # primary key for course is c(code_module, code_presentation)
    # primary key for student is id_student.
    course %>%
      summarise(!n_distinct(c(code_module, code_presentation)))
    course1 <- as_tibble(course)
    course1
    student_course <- left_join(student1,course1, by = c("code_module", "code_presentation"))
    student_course
  
    # cannot derive value from assessment , student_assessment, student_registration as
    # a standalone data set. Need to join with other data sets 