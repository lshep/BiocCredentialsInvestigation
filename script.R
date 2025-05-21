## Command Line Bash

## Get BiocCredentials user list
curl -u <admin email>:<admin password> https://git.bioconductor.org/BiocCredentials/api/biocusers/ > biocusers.json

## Copy gitolite config file of currently active packages
cp ~/BioconductorPackages/PkgManagement/gitolite-admin/conf/packages.conf .


## In R

library(jsonlite)
library(stringr)
library(dplyr)

## Load json file of BiocCredentials
json_file = "biocusers.json"
json_data = fromJSON(json_file,flatten=TRUE)

## unknown github ids
length(which(is.na(json_data$github_username)))
## [1] 669

## Load and configure packages.conf file to map currently active package maintainers
pkgs_conf = "packages.conf"
conn = file(pkgs_conf, open="r")
lines = str_trim(readLines(conn))
close(conn)
df = data.frame(pkg="", usernames="")
pkg = ""
for(l in lines){
    if(startsWith(l, "repo")){
        pkg = tail(strsplit(l, "/")[[1]],1)
    }
    if (startsWith(l, "RW devel")) {
        user_string <- sub("RW devel =\\s*", "", l)
        usernames <- strsplit(user_string, "\\s+")[[1]]
        df = rbind(df,data.frame(pkg, usernames))
    }
}
df = df[-1,]

df$usernames <- as.character(df$usernames)
json_data$username <- as.character(json_data$username)

# Merge on usernames and username to get github_username
df_merged_username <- merge(df, json_data,
                   by.x = "usernames", by.y = "username", all.x = TRUE)

df_merged_pkg <- df_merged_username %>% arrange(pkg)%>% select(pkg, everything())

## how many packages don't have a github id
## however this includes @bioconductor_writers
## which is a shorthand for all the core team 
> length(which(is.na(df_merged_pkg$github_username)))
[1] 815
> length(which(is.na(df_merged_username$github_username)))
[1] 815

# remove @bioconductor_writers
> length(setdiff(which(is.na(df_merged_username$github_username)),
                 which(df_merged_username$usernames == "@bioconductor_writers")))
[1] 705

# but this could include potentially a package that has a maintainer with a
# valid maintainer with github_id and maintainer without a valid github_id
