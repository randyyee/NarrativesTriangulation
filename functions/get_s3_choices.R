library(aws.s3)
library(readxl)
library(paws)

get_s3_choices <- function(type)
{
  
  Sys.setenv(AWS_PROFILE = "AWS-SANDBOX-SYSTEM_NARRATIVES", AWS_DEFAULT_REGION = "us-east-2",
             AWS_REGION = "us-east-2")
  
  svc <- secretsmanager()
  
  see <- svc$get_secret_value(
    SecretId = "system_narratives_1AOdf2WRb7"
  )
  
  see <- fromJSON(see$SecretString)
  
  
  Sys.setenv(AWS_ACCESS_KEY_ID = see$aws_access_key, 
             AWS_SECRET_ACCESS_KEY = see$aws_secret_access_key,
             AWS_PROFILE = "AWS-SANDBOX-SYSTEM_NARRATIVES", 
             AWS_DEFAULT_REGION = "us-east-2",
             AWS_REGION = "us-east-2")
  
  rm(see)
  rm(svc)
  
  # Lists all of bucket contents
  choices <- aws.s3::get_bucket(bucket = "sandbox.pepfar.data.data-extracts")
  
  # get just path names
  choices <- lapply(choices, "[[", 1)
  
  # decide if narratives or mer
  if (type == "narratives")
  {
    choices <- choices[grepl("^Narratives|^merged", choices)]
  } else
  {
    choices <- choices[grepl("^MER", choices)]
  }
  
  # get just file names
  cleaned_choices <- lapply(choices, function(x) gsub(".*\\/", "", x))
  
  # make dataframe of file names and path names
  choices <- do.call(rbind, Map(data.frame, file_names = cleaned_choices, 
                                path_names = choices, stringsAsFactors = FALSE))
  
  # filter just files that end in txt or xlsx or csv
  choices <- choices[grepl("txt$|xlsx$|csv$", choices$file_names), ]
  
  # reset row names
  rownames(choices) <- NULL
  
  return(choices)
  
}

