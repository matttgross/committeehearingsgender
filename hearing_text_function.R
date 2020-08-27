
library(tidyverse)
library(tidytext)
library(rebus)
library(zoo)


#Create Function

hearing_text <- function(file, committee)
{
  hearing<-readLines(file)
  
  #Remove Empty Lines
  empty_lines = grepl('^\\s*$', hearing)
  hearing = hearing[! empty_lines]
  
  #Remove Some Things Not for Analysis
  remove_pattern<-or("\\[GRAPHIC\\(S\\) NOT AVAILABLE IN TIFF FORMAT\\]", "\\[The information follows:\\]", "\\[GRAPHICS NOT AVAILABLE IN TIFF FORMAT\\]", "\\[Applause+\\.\\]", "\\[Witnesses sworn+\\.\\]", "\\[laughter\\]", "\\[all\\]", "\\[" %R% one_or_more(WRD) %R% "\\]")
  hearing<-str_remove(hearing, pattern=remove_pattern)
  
  
  #Create Dataframe
  hearing_df <- as.data.frame(hearing, stringsAsFactors = FALSE)
  
  #Tokenize and Add Line Numbers
  
  hearing_speech <- hearing_df %>%
    unnest_tokens(output=speech, input=hearing, token="lines", to_lower = FALSE)
  
  hearing_speech<- hearing_speech %>%
    mutate(line_number = row_number())
  
  #Get Names
  rebus_pattern <- or("Chairman " %R% capture(one_or_more(WRD)) %R% DOT %R% " ", "Mr" %R% DOT %R% " " %R% capture(one_or_more(WRD)) %R% DOT %R% " ", "Mrs" %R% DOT %R% " " %R% capture(one_or_more(WRD)) %R% DOT %R% " ", "Ms" %R% DOT %R% " " %R% capture(one_or_more(WRD)) %R% DOT %R% " ", "Chairwoman " %R% capture(one_or_more(WRD)) %R% DOT %R% " ", "Dr" %R% DOT %R% " " %R% capture(one_or_more(WRD)) %R% DOT %R% " ", "General" %R% DOT %R% " " %R% capture(one_or_more(WRD)) %R% DOT %R% " ", "Admiral" %R% capture(one_or_more(WRD)) %R% DOT %R% " ", "Ambassador" %R% capture(one_or_more(WRD)) %R% DOT %R% " ", "Secretary" %R% DOT %R% " " %R% capture(one_or_more(WRD)) %R% DOT %R% " ", capture("The Chairman" %R% DOT %R% " "))
  name<-str_match(hearing_speech$speech, pattern=rebus_pattern)
  
  df_name<-as.data.frame(name, stringsAsFactors = FALSE)
  
  #Create last_name and gender
  df_name<-df_name %>% mutate(last_name=case_when(
    is.na(V2)==FALSE ~ V2,
    is.na(V3)==FALSE ~ V3,
    is.na(V4)==FALSE ~ V4,
    is.na(V5)==FALSE ~ V5,
    is.na(V6)==FALSE ~ V6,
    is.na(V7)==FALSE ~ V7,
    is.na(V8)==FALSE ~ V8,
    is.na(V9)==FALSE ~ V9,
    is.na(V10)==FALSE ~ V10,
    is.na(V11)==FALSE ~ V11,
    is.na(V12)==FALSE ~ "The Chairman"),
    gender=
      case_when(
        is.na(V2)==FALSE ~ "M",
        is.na(V3)==FALSE ~ "M",
        is.na(V4)==FALSE ~ "F",
        is.na(V5)==FALSE ~ "F",
        is.na(V6)==FALSE ~ "F",
        is.na(V7)==FALSE ~ "M",
        is.na(V8)==FALSE ~ "M",
        is.na(V9)==FALSE ~ "M",
        is.na(V9)==FALSE ~ "F",
        is.na(V10)==FALSE ~ "M",
        is.na(V11)==FALSE ~ "M",
        is.na(V12)==FALSE ~ "M"),
    committee = committee)
  
  
  
  #Carry Forward last_name and gender
  df_name$last_name<-na.locf(df_name$last_name)
  df_name$gender<-na.locf(df_name$gender)
  
  #Add Line Numbers and Join
  df_joined<-df_name %>%
    mutate(line_number = row_number()) %>%
    left_join(hearing_speech)
  
  
  #Remove Names
  rm_name_pattern <- or("The Chairman" %R% DOT %R% " ","Chairman " %R% one_or_more(WRD) %R% DOT %R% " ","Mr" %R% DOT %R% " " %R% one_or_more(WRD) %R% " ", "Mrs" %R% DOT %R% " " %R% one_or_more(WRD) %R% DOT %R% " ", "Ms" %R% DOT %R% " " %R% one_or_more(WRD) %R% DOT %R% " ", "Chairwoman " %R% one_or_more(WRD) %R% DOT %R% " ", "Dr" %R% DOT %R% " " %R% one_or_more(WRD) %R% DOT %R% " " )
  df_joined$speech<- str_remove(df_joined$speech, pattern=rm_name_pattern)
  
  #Unnest words and lower case
  hearing_words<-df_joined %>%
    unnest_tokens(output=word, input=speech, token="words")
  
  hearing_words$last_name<-tolower(hearing_words$last_name)
  
  #Delete Extra Columns
  hearing_words <- hearing_words %>% select(-c(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12))
  
  #Create line_word column 
  d <- hearing_words
  line_word <- rownames(d)
  rownames(d) <- NULL
  hearing_words <- cbind(line_word,d)
  return(hearing_words)
}




