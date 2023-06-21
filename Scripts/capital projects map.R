library(rvest)
library(tidyverse)
library(data.table)
library(rstudioapi)
library(ggmap)

page_url = "https://www.fortworthtexas.gov/projects"

## Get first page of capital projects and total number of projects
page_main <- read_html(page_url)

pages_count<- html_elements(page_main, ".seamless-pagination-info")

total_pages <- strsplit(html_text2(pages_count)," ")[[1]][4]

total_pages <- list(1:as.numeric(total_pages))

links <- html_elements(page_main, ".works-list-container")

articles <- html_elements(links, "a")

URL <- articles %>% html_attr("href")

projects <- data.frame(URL)

rm(links, articles, URL, page_url)

print("Gathering all the projects")

## Complete a list of URLS for all ongoing projects
for(i in 2:length(total_pages[[1]])){
  
  print(paste("Getting page",i))
  page <- read_html(paste0("https://www.fortworthtexas.gov/projects?dlv_OC%20CL%20Public%20Works%20and%20Projects=(pageindex=", i, ")"))
  links <- html_elements(page, ".works-list-container")
  articles <- html_elements(links, "a")
  URL <- articles %>% html_attr("href")
  projects <- rbind(projects, data.frame(URL))
  rm(links, articles, URL,page)
}

projects <- as.data.table(projects)

##prep additional columns for final dataframe
projects$title <- NA
projects$type <- NA
projects$value <- NA
projects$details <- NA
projects$location <- NA
projects$district <- NA
projects$completion.date <- NA
projects$project.number <- NA
projects$status <- NA
projects$coordinates <- NA

##loop through each project to get it's information
for(k in 1:nrow(projects)){
  
print(k)  
  
page <- read_html(projects$URL[k])

project.title <- page %>% html_elements(".oc-page-title") %>% html_text()

project.details <- page %>% html_elements(".project-details-list") %>% html_elements("span") %>% html_text2()

project.type <- project.details[2]

project.value <- project.details[4]
  
## Get project info
project.info <- page %>% html_elements(".grid") %>% html_elements("p") %>% html_text2()
print("Get project info")

## Get location and district info
for (i in 1:length(project.info)){
  
  if(grepl("View Map", project.info[i])==TRUE){
  project.location <- gsub("View Map","",project.info[i])
  }
  
  if(grepl("Council District",project.info[i])==TRUE){
    p_split <- strsplit(project.info[i], " ")
    project.district <- parse_number(p_split[[1]])
    project.district <- project.district[!is.na(project.district)]
  }
  
  if(grepl("Council Districts",project.info[i])==TRUE){
    p_split <- strsplit(project.info[i], " ")
    project.district <- parse_number(p_split[[1]])
    project.district <- project.district[!is.na(project.district)]
    project.district <- paste(project.district,collapse = ", " )
  }
  
}

project.info
## Get Coordinates
print("Get coordinates")
project.coordinates <- page %>% html_elements(".gmap") %>% html_attr("data-params") %>% strsplit(":")

if(length(project.coordinates)>0){
project.coordinates <- strsplit(project.coordinates[[1]][6],"\"")
project.coordinates <- gsub('"', "", project.coordinates[[1]][2])
project.coordinates <- gsub('}', "", project.coordinates)
project.coordinates <- gsub(' ', "", project.coordinates)
}

##Get Side box details
print("sidebox")
side_values <- page %>% html_elements(".col-m-4")

check <- side_values %>% html_elements(xpath = "./div")
check

project.status <- side_values %>% html_elements(xpath = "./div[2]/div[1]" ) %>% html_text(trim = TRUE)
project.status <- gsub("\r","",project.status)
project.status <- gsub("\n",", ",project.status)


project.complete <- side_values %>% html_elements(xpath = "./div[3]/div[1]") %>% html_text(trim = TRUE)
project.complete <- gsub("\r","",project.complete)
project.complete <- gsub("\n",", ",project.complete)

project.id <- side_values %>% html_elements(xpath = "./div[5]/div[1]") %>% html_text(trim = TRUE)


##Add values to projects dataframe
print("add to")
projects$title[k] <- project.title

projects$type[k] <- project.type

projects$value[k] <- project.value

projects$details[k] <- project.info[1]

if(exists("project.location")){
projects$location[k] <- project.location
}

if(exists("project.district")){
projects$district[k] <- project.district
}

if(length(project.complete)>0){
projects$completion.date[k] <- project.complete
}

if(length(project.id)>0){
projects$project.number[k] <- project.id
}

projects$status[k] <- project.status

if(length(project.coordinates)>0){
projects$coordinates[k] <- project.coordinates
}

## Clear before next iteration
print("removing")
rm(page, project.title, project.title, project.details, project.type, project.value, 
   project.location, project.district, project.coordinates, project.info, side_values, 
   project.complete, project.number, project.status, side_values, p_split)
}



for(i in 1:nrow(projects)){
projects$latitude[i] = strsplit(projects$coordinates[i],",")[[1]][1]
projects$latitude[i] <- as.numeric(projects$latitude[i])
projects$longitude[i] = strsplit(projects$coordinates[i],",")[[1]][2]
projects$longitude[i] <- as.numeric(projects$longitude[i])
}

map_data <- projects[!is.na(projects$coordinates),]
map_data$latitude <- as.numeric(map_data$latitude)
map_data$longitude <- as.numeric(map_data$longitude)
map_data$Id <- rownames(map_data)
map_data[is.na(map_data)] <- "Unknown"
map_data$Date_updated <- Sys.Date()

nomap_data <- projects[is.na(projects$coordinates),]
nomap_data$Id <- rownames(nomap_data)
nomap_data[is.na(nomap_data)] <- "Unknown"


write.csv(map_data,"C:/Users/patar/OneDrive/Documents/Fort Worth Capital Projects/Fort_Worth_Capital_Projects/www/map_data.csv", row.names = FALSE)
write.csv(nomap_data,"C:/Users/patar/OneDrive/Documents/Fort Worth Capital Projects/Fort_Worth_Capital_Projects/www/nomap_data.csv",row.names = FALSE)
