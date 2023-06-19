library(blastula)
library(purrr)
library(quarto)
#setwd("C:/Users/John/Desktop/triggerise")

# cred file
#create_smtp_creds_file(file = "murume",
#                       user="murume2195@outlook.com",
#                       provider = "outlook")


#loading in some sample facility data
facility_data=read.csv("sample facility data.csv")

#rendering my facilities document for each report
#make a function to render the two reports
render_facility=function(facility){
  quarto::quarto_render(
    input="tr_facilities.qmd",
    execute_params = list(Facility=facility),
    output_file = glue::glue("{facility}.html")
  )
}

#now applying this function
unique(facility_data$Facility) |>
  as.character() |>
  purrr::walk(render_facility)

#now we have the two reports rendered, and since they 
#are html, we are going to remove the first line using the
#function below
html_files <- paste0(unique(facility_data$Facility), ".html")

# Loop through each HTML file
for (file in html_files) {
  # Read the contents of the HTML file
  report <- readLines(file)
  
  # Remove the doctype declaration from the report contents
  report <- report[-1]
  
  # Write the modified report contents back to the file
  writeLines(report, file)
}

#now we need to add email data for each facility from our facility
#data set
email_data <- data.frame(unique(facility_data$Facility),unique(facility_data$Facility_Email))
colnames(email_data)=c("Facility","Email")

#Now I am going to make those two reports email objects
#using render_email
# Create empty list to store email objects
email_objects <- list()
# Iterate over client names
for (facility in email_data$Facility) {
  # Generate file path for the report HTML file
  report_file <- paste0(facility, ".html")
  #render email for the report file
  email <- render_email({
    path = report_file
  })
  # Add the email object to the list
  email_objects[[facility]] <- email
}

#We will now send the emails taking each object and 
#sending it using corresponding email on the email data set
for (i in 1:nrow(email_data)){
  smtp_send(email_objects[[i]],
            from = "murume2195@outlook.com",
            to = as.character(email_data$Email[[i]]),
            subject = paste0("Monthly Risk Report on ",
                             Sys.Date()),
            credentials = creds_file("murume"))
  print(paste("Email sent to",as.character(email_data$Facility[i])))
}
