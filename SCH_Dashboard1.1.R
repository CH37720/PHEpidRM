
pacman::p_load(knitr, here, flexdashboard, tidyverse,
               DT, scales, plotly, patchwork,  readr, stringr,
               forcats, memisc, pyramid, janitor, paletteer,
               ggsci, pwalk, rpivotTable, highcharter, htmltools,
               viridis, outbreaks, ggrepel, ggthemes, zoo, options(scipen=999),table1)

# Load the dataset
Hypottowndata20 <- read_csv("https://github.com/CH37720/PHEpidRM/edit/main/Hypottowndata20.csv")

#data prep1
Hypottowndata21 <- Hypottowndata20 %>% mutate(Agegr = cut(Ageyrc, breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54,59, 64, Inf),
                                                          labels = c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49','50-54', '55-59', '60-64', '>64')))
Hypottowndata21T<-Hypottowndata21 %>% subset(Assess_month == "June")

#Define-specific version of date()
date1<- format(Sys.time(), "%b %d,%Y")
dateRDLM <- paste("June 30,2025")
dateRP <- paste("January - June, 2025")

# Define district names
District<-c("1", "2", "3")

# Town x Dashboard

# Create age sex pyramid of the hh members of the town

pyramidT<-function(Hypottowndata21T){
  # Create new subset
  pyramid_data <- 
    Hypottowndata21T %>% 
    
    # Count total cases by age group and gender
    count(Agegr, Sex, name = "total") %>%  
    
    # Create new columns for x-axis values on the plot
    mutate(
      # New column with axis values - convert male counts to negative
      axis_counts = ifelse(Sex == "Male", -total, total),
      # New column for percentage axis values
      axis_percent = round(100 * (axis_counts / nrow(Hypottowndata21T)), 
                           digits = 1))
  Hypottown_pyramid <- 
    ggplot() +
    
    geom_col(data = pyramid_data, #specify data to graph
             aes(
               x = Agegr,    # indicate x variable
               y = axis_counts,  # indicate NEGATED y variable
               fill = Sex))  +   # fill by sex
    theme_light() +
    coord_flip()
  
  max_count <- max(pyramid_data$total)
  
  custom_axes <- 
    
    # Use previous graph
    Hypottown_pyramid +
    
    # Adjust y-axis (total count)  
    scale_y_continuous(
      
      # Specify limit of y-axis using max value and making positive and negative
      limits = c(-max_count, max_count),
      
      # Specify the spacing between axis labels
      breaks = scales::breaks_width(4),
      
      # Make axis labels absolute so male labels appear positive
      labels = abs)
  
  custom_labels <- 
    
    # Start with previous demographic pyramid
    custom_axes +
    
    # Adjust the labels
    labs(
      title = paste("Town X households by Age and Sex", dateRDLM),
      subtitle = "Analysis of a Hypothetical Community Health Data",
      x = "Age Group",
      y = "Count", 
      fill = "Sex",
      caption = stringr::str_glue("Data are from a Hypothetical data for Exercise \nn = {nrow(Hypottowndata21T)}"))
  
  custom_color_theme <- 
    
    # Use previous graph
    custom_labels +
    
    # Designate colors and legend labels manually
    scale_fill_manual(
      
      # Select color of sex fill
      values = c("Female" = "lightblue",
                 "Male" = "darkblue"))+
    
    # Adjust theme settings
    theme(
      axis.line = element_line(colour = "black"), # make axis line black
      plot.title = element_text(hjust = 0.5),     # center title
      plot.subtitle = element_text(hjust = 0.5),  # center subtitle
      plot.caption = element_text(hjust = 0,      # format caption text
                                  size = 11, 
                                  face = "italic")) 
  print(custom_color_theme)
}
pyramidT(Hypottowndata21T)

# build chart for Known_Sicknesss of the hh members of the town

chartknsT<-function(Hypottowndata21T){
  Sumbydistkndxd <- Hypottowndata21T %>% group_by(District, Known_Sick) %>% summarise(n = n())
  
  Percbydistkndxd  <- Sumbydistkndxd %>%
    mutate(percentage = round((n / sum(n))*100, 1))
  
  #Group Bar Chart
  Known_Sickness_Chart<- ggplot(Percbydistkndxd, aes(x = District, y = percentage, fill = Known_Sick)) +
    geom_col(position = "dodge") +
    labs(title = paste("Town X Household Members' with Known Sickness by district, Country X", dateRDLM),
         subtitle = stringr::str_glue("Analysis of a Hypothetical Community Health Data for Exercise \nn = {nrow(Hypottowndata21T)} (Hypottowndata20.csv)"),
         x= "District",
         y= "Household Members, %") +
    theme(axis.text.x = element_text(angle = 90))
  
  Known_Sickness_Chart
}
chartknsT(Hypottowndata21T)

# build chart for current Sicknesss (symptoms) of the hh members of the town

chartcurskT<-function(Hypottowndata21T){
  
  Sumbydistcurskd <- Hypottowndata21T %>% group_by(District, current_sick) %>% summarise(n = n())
  
  Percbydistcurskd  <- Sumbydistcurskd %>%
    mutate(percentage = round((n / sum(n))*100, 1))
  
  #Group Bar Chart
  Current_Sickness_Chart<- ggplot(Percbydistcurskd, aes(x = District, y = percentage, fill = current_sick)) +
    geom_col(position = "dodge") +
    labs(title = paste("Town-X Household Members' with Current Sickness(Symtom) by district, Country X", dateRDLM),
         subtitle = stringr::str_glue("Analysis of a Hypothetical Community Health Data for Exercise \nn = {nrow(Hypottowndata21T)} (Hypottowndata20.csv)"),
         x= "District",
         y= "Household Members, %") +
    theme(axis.text.x = element_text(angle = 90))
  
  Current_Sickness_Chart
}
chartcurskT(Hypottowndata21T)

# build chart for the hh members of the town Sought Health Facility 

chartsouhfT<-function(Hypottowndata21T){
  
  distsouhfild <-Hypottowndata21T %>% filter(current_sick != "No")
  
  Sumbydistsouhfd <-distsouhfild %>% group_by(District, sought_hf) %>% summarise(n=n())
  
  Percbydistsouhfd  <- Sumbydistsouhfd %>%
    mutate(percentage = round((n / sum(n))*100, 1))
  
  #Group Bar Chart
  Sought_hf_Chart<- ggplot(Percbydistsouhfd, aes(x = District, y = percentage, fill = sought_hf)) +
    geom_col(position = "dodge") +
    labs(title = paste("Town X Household Members Sought Health Facility for the Current Sickness by district, Country X", dateRDLM),
         subtitle = stringr::str_glue("Analysis of a Hypothetical Community Health Data for Exercise \nn = {nrow(distsouhfild)} (Hypottowndata20.csv)"),
         x= "District",
         y= "Household Members, %") +
    theme(axis.text.x = element_text(angle = 90)) +
    coord_flip()
  Sought_hf_Chart
}
chartsouhfT(Hypottowndata21T)
  
### Chart - Main Perceived community health problem (Main_PCHP) of the hh members of the town

chartchpT<-function(Hypottowndata21T){
  Sumbydistchpfild <-Hypottowndata21T %>% filter(Relation_to_hhh == "Household Head")
  
  Sumbydistchpd <-Sumbydistchpfild %>% group_by(District, Main_PCHP) %>% summarise(n=n())
  
  Percbydistchpd  <- Sumbydistchpd %>%
    mutate(percentage = round((n / sum(n))*100, 1)) %>%
    arrange(by=District, desc(percentage))
  
  #Group Bar Chart
  Main_PCHP_Chart <-
    ggplot(Percbydistchpd, aes(x = District, y = percentage,
                               fill = Main_PCHP)) +
    geom_col(position = "dodge") +
    labs(title = paste("Town X, District Household Members'Main Perceived Community Health Problem, Country X", dateRDLM),
         subtitle = stringr::str_glue("Analysis of a Hypothetical Community Health Data for Exercise \nn = {nrow(Sumbydistchpfild)} (Hypottowndata20.csv)"),
         x= "District",
         y= "Household Members, %") +
    theme(axis.text.x = element_text(angle = 90)) +
    coord_flip()
  Main_PCHP_Chart
}
chartchpT(Hypottowndata21T)

# Town x: trend Analysis

#Summary Table for Town X
#Summary table of Town X community health and health related problems by month, Country x
stabT<- function(Hypottowndata21){
  dfstabT<- Hypottowndata21 %>% 
    mutate(Known_Sick= factor(Known_Sick, levels= c("Heart problem", "Hypertension", "Diabetes Mellitus", "Cancer", "Asthma (CORP)", "Other")),
           current_sick= factor(current_sick, levels = c("Only Fever ", "Only Cough", "Fever and Cough", "Fever and Rash", "Other")),
           sought_hf= factor(sought_hf, levels = c("Yes", "No")),
           Main_PCHP= factor(Main_PCHP, levels = c("Drinking water problem", "Sanitation problem", "Food insecurity", "Air pollution","Respiratory problem", "Sound pollution in the residential area", "Theft of resources impacting health", "Other")),
           Assess_month= factor(Assess_month, levels = c("January", "February", "March", "April", "May", "June")))
  
  caption<-paste("Trend of community health and health related problems by month,Town X, Country x:", dateRP)
  footnote<- "Assess-Month= The Month of data collection and reporting; Known_Sick= Known Sickness; Current_Sick= Current Sickness(symptom); sough_hf=Sought health facility; Main_PCHP= Main perceived communinuty health problem
  (Data Source: Author, 2025. A Hypothetical Community Health Data for Exercise (Hypottowndata20.csv))"
  
  StabT<-table1(~ Known_Sick + current_sick + sought_hf + Main_PCHP | Assess_month,
                data=dfstabT, na.is.category = F, render.missing=NULL, topclass="Rtable1-zebra", overall=F, caption=caption, footnote=footnote)
  
  print(StabT)
}
stabT(Hypottowndata21)


# Display plot of trend of Known Sickness in a town by the assessment months 
#on the same respondent households of the community

lplotknsT<-function(Hypottowndata21){
  Sumbydistkndxd <- Hypottowndata21 %>% group_by(Assess_month, Known_Sick) %>%
    summarise(n = n())
  
  Percbydistkndxd  <- Sumbydistkndxd %>%
    mutate(percentage = round((n / sum(n))*100, 1)) 
  
  Percbydistkndxd2<- Percbydistkndxd %>% mutate(Assess_month = factor(Assess_month,
                                                                      levels = c("January", "February",
                                                                                 "March", "April", "May", "June")))
  #line plot
  Known_Sickness_plot<- ggplot(Percbydistkndxd2, aes(x = Assess_month,
                                                     y = percentage, color = Known_Sick, group = Known_Sick)) +
    geom_line(linewidth = 1) + 
    geom_point() + 
    scale_color_discrete() +
    labs(title = paste("Trend of known Sickness repoted by respondent houshold members of town X, Country X:", dateRP),
         subtitle = stringr::str_glue("Analysis of a Hypothetical Community Health Data for Exercise (Hypottowndata20.csv)"),
         x= "Month",
         y= "Household Members, %") +
    theme_classic()
  Known_Sickness_plot
}
lplotknsT(Hypottowndata21)

# Trend of current Sicknesss (symptoms) of the hh members of the town

lplotcurskT<-function(Hypottowndata21){
  
  Sumbydistcurskd <- Hypottowndata21 %>% group_by(Assess_month, current_sick) %>% summarise(n = n())
  
  Percbydistcurskd  <- Sumbydistcurskd %>%
    mutate(percentage = round((n / sum(n))*100, 1))
  
  Percbydistcurskd2<- Percbydistcurskd %>% mutate(Assess_month = factor(Assess_month,
                                                                        levels = c("January", "February",
                                                                                   "March", "April", "May", "June")))
  #line plot
  current_Sickness_plot<- ggplot(Percbydistcurskd2, aes(x = Assess_month,
                                                        y = percentage, color = current_sick, group = current_sick)) +
    geom_line(linewidth = 1) + 
    geom_point() + 
    scale_color_discrete() +
    labs(title = paste("Trend of current sickness repoted by respondent houshold members of town X, Country X:", dateRP),
         subtitle = stringr::str_glue("Analysis of a Hypothetical Community Health Data for Exercise (Hypottowndata20.csv)"),
         x= "Month",
         y= "Household Members, %") +
    theme_classic()
  current_Sickness_plot
}
lplotcurskT(Hypottowndata21)

# Trend of the hh members of the town Sought Health Facility 

lplotsouhfT<-function(Hypottowndata21){
  
  distsouhfild <-Hypottowndata21 %>% filter(current_sick != "No")
  
  Sumbydistsouhfd <-distsouhfild %>% group_by(Assess_month, sought_hf) %>% summarise(n=n())
  
  Percbydistsouhfd  <- Sumbydistsouhfd %>%
    mutate(percentage = round((n / sum(n))*100, 1))
  
  Percbydistsouhfd2<- Percbydistsouhfd %>% mutate(Assess_month = factor(Assess_month,
                                                                        levels = c("January", "February",
                                                                                   "March", "April", "May", "June")))
  #line plot
  Sought_hf_plot<- ggplot(Percbydistsouhfd2, aes(x = Assess_month,
                                                 y = percentage, color = sought_hf, group = sought_hf)) +
    geom_line(linewidth = 1) + 
    geom_point() + 
    scale_color_discrete() +
    labs(title = paste("Trend of respondent houshold members sought health facility, town X, Country X:", dateRP),
         subtitle = stringr::str_glue("Analysis of a Hypothetical Community Health Data for Exercise (Hypottowndata20.csv)"),
         x= "Month",
         y= "Household Members, %") +
    theme_classic()
  Sought_hf_plot
}
lplotsouhfT(Hypottowndata21)

### Trend of Main Perceived community health problem (Main_PCHP) of the hh members of the town

lplotpchpT<-function(Hypottowndata21){
  Sumbydistchpfild <-Hypottowndata21 %>% filter(Relation_to_hhh == "Household Head")
  
  Sumbydistchpd <-Sumbydistchpfild %>% group_by(Assess_month, Main_PCHP) %>% summarise(n=n())
  
  Percbydistchpd  <- Sumbydistchpd %>%
    mutate(percentage = round((n / sum(n))*100, 1))
  
  Percbydistchpd2<- Percbydistchpd %>% mutate(Assess_month = factor(Assess_month,
                                                                    levels = c("January", "February",
                                                                               "March", "April", "May", "June")))
  #line plot
  Main_PCHP_plot<- ggplot(Percbydistchpd2, aes(x = Assess_month,
                                               y = percentage, color = Main_PCHP, group = Main_PCHP)) +
    geom_line(linewidth = 1) + 
    geom_point() + 
    scale_color_discrete() +
    labs(title = paste("Trend of perceived main community health problem of respondent houshold members, town X, Country X:", dateRP),
         subtitle = stringr::str_glue("Analysis of a Hypothetical Community Health Data for Exercise (Hypottowndata20.csv)"),
         x= "Month",
         y= "Household Members, %") +
    theme_classic()
  Main_PCHP_plot
}
lplotpchpT(Hypottowndata21)


# District 1 Dashboard

# Chart -1.1 Sex

crsexpie <- function(district_name) {
  
  subtowndata0 <- subset(Hypottowndata21T, District == district_name)
  
  Sumbysexdata0 <- subtowndata0 %>% group_by(Sex) %>% summarise(n = n())
  
  Percentage_data0 <- Sumbysexdata0 %>% 
    mutate(percentage = round((n / sum(n))*100, 1))
  
  percentage_pie0 <-
    ggplot(Percentage_data0, aes(x = " ", y=percentage,fill=Sex)) +
    geom_col() +
    coord_polar(theta = "y")+
    geom_text(aes(label = paste(percentage, "\ %")),
              position = position_stack(vjust = 0.5), # Center the label
              color = "white",
              fontface = "bold") +
    labs(title = paste("Sex of persons in the households of district", district_name, "town X, country X", dateRDLM),
         subtitle = stringr::str_glue("Analysis of data \nn = {nrow(subtowndata0)} from a hypothetical data (hypottowndata20)")) +
    theme_void()+
    scale_fill_viridis_d()
}

#create pie of sex distribution for district 1
piesexd1<-function(Hypottowndata21T){
  plots<- lapply(District[[1]], crsexpie)
  #Print the plot
  for(i in seq_along(plots)){print(plots[[1]])}
}
piesexd1(Hypottowndata21T)
  
# Chart-1.2 Known Sickness

crKnSbarch <- function(district_name) {
  
  subtowndata0 <- subset(Hypottowndata21T, District == district_name)
  
  
  Sumbydistkndxd0 <- subtowndata0 %>% group_by(Known_Sick) %>% summarise(n = n())
  
  Percbydistkndxd0  <- Sumbydistkndxd0 %>%
    mutate(percentage = round((n / sum(n))*100, 1))
  
  #Group Bar Chart
  Known_Sickness_Chart<- ggplot(Percbydistkndxd0, aes(x = " " , y = percentage, fill = Known_Sick)) +
    geom_col(position = "dodge") +
    labs(title = paste("District", district_name, "Household Members' with Known Sickness, Town X, Country X", dateRDLM),
         subtitle = stringr::str_glue("Analysis of a Hypothetical Community Health Data for Exercise \nn = {nrow(subtowndata0)} (Hypottowndata20.csv)"),
         x= "Known_Sickness",
         y= "Household Members, %") +
    theme(axis.text.x = element_text(angle = 90)) 
}

#create bchart of known sickness distribution for district 1
barchKnSd1<-function(Hypottowndata21T){
  bcharts<- lapply(District[[1]], crKnSbarch)
  #Print the bchart
  for(i in seq_along(bcharts)){print(bcharts[[1]])}
}
barchKnSd1(Hypottowndata21T)

# Chart-1.3 Current Sickness (symptoms)

curSbarch <- function(district_name) {
  
  subtowndata0 <- subset(Hypottowndata21T, District == district_name)
  
  
  Sumbydistcurskd0 <- subtowndata0 %>% group_by(current_sick) %>% summarise(n = n())
  
  Percbydistcurskd0  <- Sumbydistcurskd0 %>%
    mutate(percentage = round((n / sum(n))*100, 1))
  
  #Group Bar Chart
  Current_Sickness_Chart<- ggplot(Percbydistcurskd0, aes(x = " ", y = percentage, fill = current_sick)) +
    geom_col(position = "dodge") +
    labs(title = paste("District", district_name, "Household Members' with Current Sickness, Town X, Country X", dateRDLM),
         subtitle = stringr::str_glue("Analysis of a Hypothetical Community Health Data for Exercise \nn = {nrow(subtowndata0)} (Hypottowndata20.csv)"),
         x= paste("District", district_name),
         y= "Household Members, %") +
    theme(axis.text.x = element_text(angle = 90)) +
    coord_flip()
}

#create bchart of current sickness (symptoms) distribution for district 1
barcurSd1<-function(Hypottowndata21T){
  curSbarchs<- lapply(District[[1]], curSbarch)
  #Print the bchart
  for(i in seq_along(curSbarchs)){print(curSbarchs[[1]])}
}
barcurSd1(Hypottowndata21T)
  
# Chart -1.4 Sought Health Facility

souhfbarch <- function(district_name) {
  
  subtowndata0 <- subset(Hypottowndata21T, District == district_name)
  
  distsouhfild0 <-subset(subtowndata0, current_sick != "No")
  
  Sumbydistsouhfd0 <-distsouhfild0 %>% group_by(sought_hf) %>% summarise(n=n())
  
  Percbydistsouhfd0  <- Sumbydistsouhfd0 %>%
    mutate(percentage = round((n / sum(n))*100, 1))
  
  #Group Bar Chart
  Sought_hf_Chart<- ggplot(Percbydistsouhfd0, aes(x = " ", y = percentage, fill = sought_hf)) +
    geom_col(position = "dodge") +
    labs(title = paste("District", district_name, "Household Members' Sought Health Facility for the Current Sickness, Town X, Country X", dateRDLM),
         subtitle = stringr::str_glue("Analysis of a Hypothetical Community Health Data for Exercise \nn = {nrow(distsouhfild0)} (Hypottowndata20.csv)"),
         x= paste("District", district_name),
         y= "Household Members, %") +
    theme(axis.text.x = element_text(angle = 90))
}

#create bchart of sought health facility distribution for district 1
barsouhfd1<-function(Hypottowndata21T){
  souhfbarchs<- lapply(District[[1]], souhfbarch)
  #Print the bchart
  for(i in seq_along(souhfbarchs)){print(souhfbarchs[[1]])}
}
barsouhfd1(Hypottowndata21T)

# Chart - 1.5 Main Perceived community health problem (Main_PCHP)

MPCHPbarch <- function(district_name) {
  
  subtowndata0 <- subset(Hypottowndata21T, District == district_name)
  
  Sumbydistchpfild0 <-subtowndata0  %>% filter(Relation_to_hhh == "Household Head")
  
  Sumbydistchpd0 <-Sumbydistchpfild0 %>% group_by(Main_PCHP) %>% summarise(n=n())
  
  Percbydistchpd0  <- Sumbydistchpd0 %>%
    mutate(percentage = round((n / sum(n))*100, 1)) %>%
    arrange(by=Main_PCHP, desc(percentage))
  
  # Group Bar Chart
  Main_PCHP_Chart <-
    ggplot(Percbydistchpd0, aes(x = " ", y = percentage,
                                fill = Main_PCHP)) +
    geom_col(position = "dodge") +
    labs(title = paste("District", district_name, "Household Members' Main Perceived Community Health Problem,Town X, Country X", dateRDLM),
         subtitle = stringr::str_glue("Analysis of a Hypothetical Community Health Data for Exercise \nn = {nrow(Sumbydistchpfild0)} (Hypottowndata20.csv)"),
         x= paste("District", district_name),
         y= "Household Members, %") +
    theme(axis.text.x = element_text(angle = 90)) +
    coord_flip()
}

#create bchart of MPCHP distribution for district 1
barMPCHPd1<-function(Hypottowndata21T){
  MPCHPbarchs<- lapply(District[[1]], MPCHPbarch)
  #Print the bchart
  for(i in seq_along(MPCHPbarchs)){print(MPCHPbarchs[[1]])}
}
barMPCHPd1(Hypottowndata21T)
 
# District 1 trend analysis
  
##Summary table of District 1
#Summary table of each District community health and health related problems by month, Town X, Country x
  stabD<- function(district_name){
    dfstabD<- subset(Hypottowndata21, District == district_name) %>% 
      mutate(Known_Sick= factor(Known_Sick, levels= c("Heart problem", "Hypertension", "Diabetes Mellitus", "Cancer", "Asthma (CORP)", "Other")),
             current_sick= factor(current_sick, levels = c("Only Fever", "Only Cough", "Fever and Cough", "Fever and Rash", "Other")),
             sought_hf= factor(sought_hf, levels = c("Yes", "No")),
             Main_PCHP= factor(Main_PCHP, levels = c("Drinking water problem", "Sanitation problem", "Food insecurity", "Air pollution","Respiratory problem", "Sound pollution in the residential area", "Theft of resources impacting health", "Other")),
             Assess_month= factor(Assess_month, levels = c("January", "February", "March", "April", "May", "June")))
    caption<-paste("Trend of community health and health related problems by month,District", district_name,"Town X, Country x:", dateRP)
    footnote<- "Assess-Month= The Month of data collection and reporting; Known_Sick= Known Sickness; Current_Sick= Current Sickness(symptom); sough_hf=Sought health facility; Main_PCHP= Main perceived communinuty health problem
  (Data Source: Author, 2025. A Hypothetical Community Health Data for Exercise (Hypottowndata20.csv))"
    
    StabD<-table1(~ Known_Sick + current_sick + sought_hf + Main_PCHP | Assess_month,
                  data=dfstabD, na.is.category = F, render.missing=NULL,  topclass="Rtable1-zebra", overall=F, caption=caption, footnote=footnote)
  }
#Summary table of District 1 community health and health related problems by month, Town X, Country x
stabD1<-function(Hypottowndata21){
  stabs<- lapply(District[[1]], stabD)
  #Print the line plot
  print(stabs[[1]])
}
stabD1(Hypottowndata21) 
  
  
# Function for trend of Known Sickness of district hhs
  
  lplotKnSD <- function(district_name) {
    
    subtowndata0 <- subset(Hypottowndata21, District == district_name)
    
    
    Sumbydistkndxd0 <- subtowndata0 %>% group_by(Assess_month, Known_Sick) %>% summarise(n = n())
    
    Percbydistkndxd0  <- Sumbydistkndxd0 %>%
      mutate(percentage = round((n / sum(n))*100, 1))
    
    Percbydistkndxd2<- Percbydistkndxd0 %>% mutate(Assess_month = factor(Assess_month,
                                                                         levels = c("January", "February",
                                                                                    "March", "April", "May", "June")))
    #line plot
    Known_Sickness_plot<- ggplot(Percbydistkndxd2, aes(x = Assess_month,
                                                       y = percentage, color = Known_Sick, group = Known_Sick)) +
      geom_line(linewidth = 1) + 
      geom_point() + 
      scale_color_discrete() +
      labs(title = paste("Trend of known Sickness repoted by respondent houshold members of District", district_name, "Town X, Country X:", dateRP),
           subtitle = stringr::str_glue("Analysis of a Hypothetical Community Health Data for Exercise (Hypottowndata20.csv)"),
           x= "Month",
           y= "Household Members, %") +
      theme_classic()
    Known_Sickness_plot
  }

#Trend of known sickness of district 1
lplotKnSD1<-function(Hypottowndata21){
  lplots<- lapply(District[[1]], lplotKnSD)
  #Print the line plot
  print(lplots[[1]])
}
lplotKnSD1(Hypottowndata21)

# line plot-1.3 Current Sickness (symptoms)

lplotcurSD <- function(district_name) {
  
  subtowndata0 <- subset(Hypottowndata21, District == district_name)
  
  
  Sumbydistcurskd0 <- subtowndata0 %>% group_by(Assess_month, current_sick) %>% summarise(n = n())
  
  Percbydistcurskd0  <- Sumbydistcurskd0 %>%
    mutate(percentage = round((n / sum(n))*100, 1))
  
  Percbydistcurskd2<- Percbydistcurskd0 %>% mutate(Assess_month = factor(Assess_month,
                                                                         levels = c("January", "February",
                                                                                    "March", "April", "May", "June")))
  #line plot
  current_sickness_plot<- ggplot(Percbydistcurskd2, aes(x = Assess_month,
                                                        y = percentage, color = current_sick, group = current_sick)) +
    geom_line(linewidth = 1) + 
    geom_point() + 
    scale_color_discrete() +
    labs(title = paste("Trend of current sickness repoted by respondent houshold members of District", district_name, "Town X, Country X:", dateRP),
         subtitle = stringr::str_glue("Analysis of a Hypothetical Community Health Data for Exercise (Hypottowndata20.csv)"),
         x= "Month",
         y= "Household Members, %") +
    theme_classic()
  current_sickness_plot
}

#Trend of current sickness of district 1
lplotcurSD1<-function(Hypottowndata21){
  lplots<- lapply(District[[1]], lplotcurSD)
  #Print the line plot
  print(lplots[[1]])
}
lplotcurSD1(Hypottowndata21)


# line plot -1.4 Sought Health Facility

lplotsouhfD <- function(district_name) {
  
  subtowndata0 <- subset(Hypottowndata21, District == district_name)
  
  distsouhfild0 <-subset(subtowndata0, current_sick != "No")
  
  Sumbydistsouhfd0 <-distsouhfild0 %>% group_by(Assess_month, sought_hf) %>% summarise(n=n())
  
  Percbydistsouhfd0  <- Sumbydistsouhfd0 %>%
    mutate(percentage = round((n / sum(n))*100, 1))
  
  Percbydistsouhfd2<- Percbydistsouhfd0 %>% mutate(Assess_month = factor(Assess_month,
                                                                         levels = c("January", "February",
                                                                                    "March", "April", "May", "June")))
  #line plot
  sought_hf_plot<- ggplot(Percbydistsouhfd2, aes(x = Assess_month,
                                                 y = percentage, color = sought_hf, group = sought_hf)) +
    geom_line(linewidth = 1) + 
    geom_point() + 
    scale_color_discrete() +
    labs(title = paste("Trend of respondent houshold members sought health facility, District", district_name, "Town X, Country X:", dateRP),
         subtitle = stringr::str_glue("Analysis of a Hypothetical Community Health Data for Exercise (Hypottowndata20.csv)"),
         x= "Month",
         y= "Household Members, %") +
    theme_classic()
  sought_hf_plot
}

#Trend of sought health facility of district 1
lplotsouhfD1<-function(Hypottowndata21){
  lplots<- lapply(District[[1]], lplotsouhfD)
  #Print the line plot
  print(lplots[[1]])
}
lplotsouhfD1(Hypottowndata21)


# line plot - 1.5 Main Perceived community health problem (Main_PCHP)

lplotMPCHPD <- function(district_name) {
  subtowndata0 <- subset(Hypottowndata21, District == district_name)
  Sumbydistchpfild0 <-subtowndata0  %>% filter(Relation_to_hhh == "Household Head")
  Sumbydistchpd0 <-Sumbydistchpfild0 %>% group_by(Assess_month, Main_PCHP) %>% summarise(n=n())
  Percbydistchpd0  <- Sumbydistchpd0 %>%
    mutate(percentage = round((n / sum(n))*100, 1))
  Percbydistchpd2<-  Percbydistchpd0 %>% mutate(Assess_month = factor(Assess_month,
                                                                      levels = c("January", "February",
                                                                                 "March", "April", "May", "June")))
  #line plot
  Main_PCHP_plot<- ggplot(Percbydistchpd2, aes(x = Assess_month,
                                               y = percentage, color = Main_PCHP, group = Main_PCHP)) +
    geom_line(linewidth = 1) + 
    geom_point() + 
    scale_color_discrete() +
    labs(title = paste("Trend of respondent houshold members perceived main community health problem, District", district_name, "Town X, Country X:", dateRP),
         subtitle = stringr::str_glue("Analysis of a Hypothetical Community Health Data for Exercise (Hypottowndata20.csv)"),
         x= "Month",
         y= "Household Members, %") +
    theme_classic()
  Main_PCHP_plot
}

#Trend of Main_PCHP of district 1
lplotMPCHPD1<-function(Hypottowndata21){
  lplots<- lapply(District[[1]], lplotMPCHPD)
  #Print the line plot
  print(lplots[[1]])
}
lplotMPCHPD1(Hypottowndata21)


# District 2 Dashboard

### Chart - 2.1 Sex

#create pie of sex distribution for district 2
piesexd2<-function(Hypottowndata21T){
  plots2<- lapply(District, crsexpie)
  #Print each plot
  print(plots2[[2]])}
piesexd2(Hypottowndata21T)

# Chart-2.2 Known Sickness 
#create bchart of known sickness distribution for district 2
barchKnSd2<-function(Hypottowndata21T){
  bcharts2<- lapply(District, crKnSbarch)
  #Print the bchart
  print(bcharts2[[2]])}
barchKnSd2(Hypottowndata21T)

# Chart-2.3 Current Sickness (symptoms)

#create bchart of current sickness (symptoms) distribution for district 2
barcurSd2<-function(Hypottowndata21T){
  curSbarchs2<- lapply(District, curSbarch)
  #Print the bchart
  print(curSbarchs2[[2]])
}
barcurSd2(Hypottowndata21T)

# Chart -2.4 Sought Health Facility

#create bchart of sought health facility distribution for district 2
barsouhfd2<-function(Hypottowndata21T){
  souhfbarchs2<- lapply(District, souhfbarch)
  #Print the bchart
  print(souhfbarchs2[[2]])
}
barsouhfd2(Hypottowndata21T)

### Chart - 2.5 Main Perceived community health problem (Main_PCHP)

#create bchart of MPCHP distribution for district 2
barMPCHPd2<-function(Hypottowndata21T){
  MPCHPbarchs2<- lapply(District, MPCHPbarch)
  #Print the bchart
  print(MPCHPbarchs2[[2]])}
barMPCHPd2(Hypottowndata21T)


# District 2 Trend Analysis

#Summary table of District 2 community health and health related problems by month, Town X, Country x
stabD2<-function(Hypottowndata21){
  stabs<- lapply(District, stabD)
  #Print the line plot
  print(stabs[[2]])
}
stabD2(Hypottowndata21)


# line plot-2.2 Known Sickness 
#create line plot of known sickness of district 2
lplotKnSD2<-function(Hypottowndata21){
  lplots<- lapply(District, lplotKnSD)
  #Print the line plot
  print(lplots[[2]])
}
lplotKnSD2(Hypottowndata21)

# line plot-2.3 Current Sickness (symptoms)

#create line plot of current sickness (symptoms) of district 2
lplotcurSD2<-function(Hypottowndata21){
  lplots<- lapply(District, lplotcurSD)
  #Print the line plot
  print(lplots[[2]])
}
lplotcurSD2(Hypottowndata21)

# line plot -2.4 Sought Health Facility

#create line plot of sought health facility of district 2
lplotsouhfD2<-function(Hypottowndata21){
  lplots<- lapply(District, lplotsouhfD)
  #Print the line plot
  print(lplots[[2]])
}
lplotsouhfD2(Hypottowndata21)

### line plot - 2.5 Main Perceived community health problem (Main_PCHP)

#create line plot of MPCHP of district 2
lplotMPCHPD2<-function(Hypottowndata21){
  lplots<- lapply(District, lplotMPCHPD)
  #Print the line plot
  print(lplots[[2]])
}
lplotMPCHPD2(Hypottowndata21)


# District 3 Dashboard

### Chart -3.1 Sex

#create pie of sex distribution for district 3
piesexd3<-function(Hypottowndata21T){
  plots3<- lapply(District, crsexpie)
  #Print each plot
  print(plots3[[3]])}
piesexd3(Hypottowndata21T)

#Chart-3.2 Known Sickness

#create bchart of known sickness distribution for district 3
barchKnSd3<-function(Hypottowndata21T){
  bcharts3<- lapply(District, crKnSbarch)
  #Print the bchart
  print(bcharts3[[3]])}
barchKnSd3(Hypottowndata21T)

# Chart-3.3 Current Sickness (symptoms)

#create bchart of current sickness (symptoms) distribution for district 3
barcurSd3<-function(Hypottowndata21T){
  curSbarchs3<- lapply(District, curSbarch)
  #Print the bchart
  print(curSbarchs3[[3]])}
barcurSd3(Hypottowndata21T)

# Chart -3.4 Sought Health Facility

#create bchart of sought health facility distribution for district 3
barsouhfd3<-function(Hypottowndata21T){
  souhfbarchs3<- lapply(District, souhfbarch)
  #Print the bchart
  print(souhfbarchs3[[3]])}
barsouhfd3(Hypottowndata21T)

# Chart - 3.5 Main Perceived community health problem (Main_PCHP)

#create bchart of MPCHP distribution for district 3
barMPCHPd3<-function(Hypottowndata21T){
  MPCHPbarchs3<- lapply(District, MPCHPbarch)
  #Print the bchart
  print(MPCHPbarchs3[[3]])}

barMPCHPd3(Hypottowndata21T)


# District 3 Trend Analysis

#Summary table of District 3 community health and health related problems by month, Town X, Country x
stabD3<-function(Hypottowndata21){
  stabs<- lapply(District, stabD)
  #Print the line plot
  print(stabs[[3]])
}
stabD3(Hypottowndata21)


# line plot-3.2 Known Sickness 

#create line plot of known sickness of district 3
lplotKnSD3<-function(Hypottowndata21){
  lplots<- lapply(District, lplotKnSD)
  #Print the line plot
  print(lplots[[3]])
}
lplotKnSD3(Hypottowndata21)

# line plot-3.2 Current Sickness (symptoms)

#create line plot of current sickness (symptoms) of district 3
lplotcurSD3<-function(Hypottowndata21){
  lplots<- lapply(District, lplotcurSD)
  #Print the line plot
  print(lplots[[3]])
}
lplotcurSD3(Hypottowndata21)

# line plot -3.2 Sought Health Facility

#create line plot of sought health facility of district 3
lplotsouhfD3<-function(Hypottowndata21){
  lplots<- lapply(District, lplotsouhfD)
  #Print the line plot
  print(lplots[[3]])
}
lplotsouhfD3(Hypottowndata21)

### line plot - 3.2 Main Perceived community health problem (Main_PCHP)

#create line plot of MPCHP of district 3
lplotMPCHPD3<-function(Hypottowndata21){
  lplots<- lapply(District, lplotMPCHPD)
  #Print the line plot
  print(lplots[[3]])
}
lplotMPCHPD3(Hypottowndata21)

#Pivot Table: Pivot table of the recent month ( dateRDLM ), data of household members in town X, country X 

pt1<-function(Hypottowndata21T){
  
  pt1<- (rpivotTable(Hypottowndata21T,
                     aggrigatorName = "Count",
                     cols= "Agegr",
                     rows= "Sex",
                     renderName= "Heatmap"))
  pt1
}
pt1(Hypottowndata21T)

# Data Table

### Chart - Data Table of My Hypothetical Community Health Data of Town-X with its three Districts (named as 1, 2 and 3), Country x, dateRDLM


mydatatable<-function(Hypottowndata20){
  datatable<-datatable(Hypottowndata20,
                       caption = "My Hypothetical Community Health Data",
                       rownames = T,
                       filter = "top",
                       extensions = "Buttons", options = list(
                         dom ='Bfrtip',
                         buttons = c('copy', 'print', "csv")
                       ))
  datatable
}

mydatatable(Hypottowndata20)
