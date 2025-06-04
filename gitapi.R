
##  Notes: 

curl -X PUT \
  -H "Authorization: token YOUR_GITHUB_TOKEN" \
  -H "Accept: application/vnd.github+json" \
  https://api.github.com/repos/OWNER/REPO/collaborators/USERNAME \
  -d '{"permission":"push"}'


Parameters:

    OWNER: Owner of the repo

    REPO: Repository name

    USERNAME: GitHub username of the person you're adding

    permission: Set to "push" for write access (other options: "pull", "admin", "maintain", "triage")


Response:

    201 Created: User invited.

    204 No Content: User is already a collaborator.

    422 Unprocessable Entity: Invalid username or you don't have permission.


####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################


###  chat gpt helping draft R script

library(httr)
library(jsonlite)

# === CONFIGURATION ===
github_token <- "ghp_your_token_here"
owner <- "your-username-or-org"
repo <- "your-repo"
username_to_add <- "collaborator-username"
permission <- "push"  # "pull", "push", "admin", "maintain", "triage"

# === Construct URL ===
url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/collaborators/", username_to_add)

# === Set headers and body ===
headers <- add_headers(
  Authorization = paste("token", github_token),
  Accept = "application/vnd.github+json"
)

body <- toJSON(list(permission = permission), auto_unbox = TRUE)

# === Make PUT request ===
response <- PUT(url, headers, body = body)

# === Handle response ===
status <- status_code(response)
if (status == 201) {
  cat("Invitation sent to", username_to_add, "\n")
} else if (status == 204) {
  cat("", username_to_add, "is already a collaborator.\n")
} else {
  cat("Failed to add collaborator. Status code:", status, "\n")
  print(content(response, as = "parsed"))
}


#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################

## manual single test

library(httr)
library(jsonlite)

github_token <- Sys.getenv("GITHUB_PAT_COLLAB")
owner <- "lshep"
repo <- "LoriTestPkg"         # LoriTestPkg3  LoriTestPkg2
username_to_add <- "jwokaty"
permission <- "push"          # "pull", "push", "admin", "maintain", "triage"

# === Construct URL ===
url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/collaborators/", username_to_add)

# === Set headers and body ===
headers <- add_headers(
  Authorization = paste("token", github_token),
  Accept = "application/vnd.github+json"
)

body <- toJSON(list(permission = permission), auto_unbox = TRUE)

# === Make PUT request ===
response <- PUT(url, headers, body = body)

# === Handle response ===
status <- status_code(response)
if (status == 201) {
  cat("Invitation sent to", username_to_add, "\n")
} else if (status == 204) {
  cat("", username_to_add, "is already a collaborator.\n")
} else {
  cat("Failed to add collaborator. Status code:", status, "\n")
  print(content(response, as = "parsed"))
}





#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################

library(httr)
library(jsonlite)
library(dplyr)

github_token <- Sys.getenv("GITHUB_PAT_COLLAB")
owner <- "lshep"
permission <- "push"              ## "pull", "push", "admin", "maintain", "triage"
github_token <- Sys.getenv("GITHUB_PAT_COLLAB")


## Create mock tibble similar to that generated mapping pages and github_id(s)
##   if known
   
pkg = c("LoriTestPkg", "LoriTestPkg3", "LoriPkgTest2")
github_usernames = c("jwokaty", NA_character_,"jwokaty, vjcitn")
mock_df = tibble(pkg, github_usernames)
test_df = as.data.frame(mock_df)
none_assigned = c()
pkg_repo = c()
api_status = c()
github_id = c()
constructed_url = c()

for (i in seq_len(nrow(test_df))) {
    repo <- test_df$pkg[i]
    usernames <- test_df$github_usernames[i]
    if (!is.na(usernames)) {
        split_usernames <- trimws(strsplit(usernames, ",")[[1]])
        for (username in split_usernames) {
            ## === Construct URL ===
            url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/collaborators/", username)
            ## === Set headers and body ===
            headers <- add_headers(
                Authorization = paste("token", github_token),
                Accept = "application/vnd.github+json"
            )
            body <- toJSON(list(permission = permission), auto_unbox = TRUE)

            ## === Make PUT request ===
            response <- PUT(url, headers, body = body)
            ## === Handle response ===
            status <- status_code(response)
            pkg_repo = c(pkg_repo, repo)
            github_id = c(github_id, username)
            api_status = c(api_status, status)
            constructed_url = c(constructed_url, url)
        }
    }else{
        message(repo, " does not have a valid github_id")
        none_assigned = c(none_assigned, repo)
        pkg_repo = c(pkg_repo, repo)
        github_id = c(github_id, NA_character_)
        api_status = c(api_status, 404)
        constructed_url = c(constructed_url, NA_character_)
    } 
}
