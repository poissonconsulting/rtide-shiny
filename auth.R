#dropbox
if(!file.exists('helpers/droptoken.rds')){
  token <- rdrop2::drop_auth()
  saveRDS(object = token, file = "helpers/droptoken.rds")
}

gmailr::use_secret_file("helpers/poisson-shiny-mail-auth.json")
