library(haven)     # for reading .dta files
library(writexl)   # for writing .xlsx files

# Import the Stata .dta file
dta_data <- read_dta("data/00_manually-downloaded/WB_PFR_Baseline_2011/BEN_2011_PFRIE-BL_v01_M_Stata8/VILLAGES_CODES.dta")

# Export to Excel
write_xlsx(dta_data, "data/00_manually-downloaded/adm4_codes.xlsx")

parcels <- st_drop_geometry(parcel_panel)


