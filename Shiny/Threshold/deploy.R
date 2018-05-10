# install.packages('rsconnect')

rsconnect::setAccountInfo(name='bus464',
                          token='65FC79697D68BFF31255CC25031B7207',
                          secret='2eOxYLIoycCuyo9QOdAQ/GkpvkaTkLlsKyRpbc5e')

library(rsconnect)
rsconnect::deployApp('/Volumes/Untitled/Users/Marcus/Dropbox/Skole (Selective Sync Conflict)/NHH/Master/V18/BUS464/BUS464 Vizualization in R/Shiny/Threshold')
