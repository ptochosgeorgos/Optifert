# ==============================================================================
# SCRIPT: 02_mirror_lysimeter_database.R
# ZWECK:  mirroring database contents to github repo
# ==============================================================================

library("purrr")

srcdir <- "~/mnt/Data-Work-RE/27_Natural_Resources-RE/273_GSF_Work/NährstoffManagementSysteme/Drittmittelprojekte/2024_32er_Lysimeter/04_Database"
trgdir <- "data/Lysimeter"

# Delete old versions
fold <- list.files(trgdir, full.names = TRUE)
file.remove(fold)

# Mirror updated files
flist  <- list.files(srcdir, pattern="*.csv", full.names = TRUE)
file.copy(flist, trgdir)

# Rename to convention

fname <-list.files(trgdir, full.names = TRUE)
fname_new <- map_chr(
  list.files(trgdir), 
  ~ paste0(trgdir,"/Lysimeter_",.)
  )

file.rename(fname, fname_new)



