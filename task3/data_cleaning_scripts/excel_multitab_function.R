

# Hurdle: Data file is Excel
# Fix:  Use readxl package

library(readxl)


# Hurdle: multiple sheets in source data
# Solution: use re-usable function to read any number of sheets from Excel

read_excel_allsheets <-
    function(filename, tibble = TRUE) {
        sheets <- readxl::excel_sheets(filename)
        x <- lapply(sheets,
                    function(X) readxl::read_excel(filename, sheet = X))
        if(!tibble) x <- lapply(x, as.data.frame)
        names(x) <- sheets
        x
    }

# Use function to read any multi-sheet Excel file
raw_data <- read_excel_allsheets("seabirds.xls")

