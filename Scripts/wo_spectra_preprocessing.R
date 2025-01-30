source('./Scripts/.PACKAGES.R')

# get data from NERC output
sd <- read.csv("./Data/NERC_smoothed_Spectra_2022.csv")

a <- sd %>%
  pivot_longer(
    cols = -X,  # Replace 'observation_id' with the actual name of your ID column
    names_to = "wavelength",  # Name of the new column that will hold the wavelength names
    values_to = "value"       # Name of the new column that will hold the corresponding values
  )

# make variable names from id
b  <- extract(a, X, into = c("date", "treatment", "clone", "clone_num", "branch", "rep"), 
               "([[:digit:]]+)_([[:alnum:]]+)_([[:alnum:]]+)_([[:alnum:]]+)_([[:alpha:]]+).([[:alnum:]]+)", remove=FALSE)

# get date and and days since 
b$date <- as.Date(b$date, format = '%d%m%Y')
b$days <- round(as.numeric(difftime( b$date, '2022-06-27', units = 'days')), digits =0)   

colnames(b)[1] <- 'id'

## get the time of each spectral measurement
# Set the path
folder_path <- './Spectraprocessing/Data'  # Replace with your folder path
# Get the list of files in the folder
file_names <- list.files(path = folder_path, full.names = TRUE)
# Get file information
file_info <- file.info(file_names)


# Create a dataframe with filenames and timestamps
spec_times <- data.frame(
  id = str_sub(rownames(file_info), (str_length(folder_path)+2), -5),  # Get the filenames
  collection_time = file_info$mtime    # modified time
)

# combine times to the specral data 
c <- left_join(b, spec_times, by = 'id')


# average raw spectra
d <- c %>%
  group_by(treatment, wavelength, clone, days, date, branch, clone_num) %>% 
  summarise_at(vars("value",'collection_time'), median) 


# tidy miss labled clone
d[d == 1546] <- '1564'

# remove weird day
e <- d %>% 
  filter(days != 25)

write.csv(e, './Data/tidy_averaged_spectra.csv')
