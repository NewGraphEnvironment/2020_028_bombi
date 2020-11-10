##lets bring our photos and get filename on git as well as lat, longs for the ones that have geo info

##need to add the gpslat long columns if they do not exist - hence the tibble::add_column
# https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist

source('R/packages.R')
source('R/functions.R')

files <- paste0('data/photos/',list.files('data/photos'))


photo_meta <- exifr::read_exif(path = files) %>% 
  purrr::set_names(., nm = tolower(names(.))) %>% 
  mutate(url  = paste0('https://raw.githubusercontent.com/NewGraphEnvironment/2020_028_bombi/master/', 
                       sourcefile))

#GPSLatitude and GPSLongitude datetimecreated


##write to a csv
write.csv(photo_meta, file = 'data/photo_meta.csv', row.names = F) ##moved this up a level




