# User sets the proper working directory. Only things in directory should be desired PDF's.

setwd("O:/0950/Orwork/ISOPDF/Hand Input/Batch_2") # Set the location of PDF's to be read
dest_folder <- "O:/0950/Orwork/ISOPDF/Hand Input/R Output/"  # Where the output of this program is saved

# check.packages function: install and load multiple R packages.
# Check to see if packages are installed. Install them if they are not, then load them into the R session.

##################################################

#Function 1 - ensures correct packages are installed. Installs them if needed. 
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

####################################################

# put relevant required package here and it will install if not currently installed on computer
packages<-c("tabulizer","pdfsearch","pdftools", "dplyr", "mgsub", "testit")
check.packages(packages)

# activating package to local environment
library(dplyr)
library(tabulizer)
library(pdfsearch)
library(pdftools)
library(mgsub)

#########################################################

# function to remove all commas and spaces and coerce columns to numeric. It also makes sure a number
# in a format such as (5,460) is recognized as a negative number.
# Function 2
clean <- function(ttt) {
  as.numeric(mgsub(string=as.character(ttt), pattern = c("\\$", "\\.", ",", "\\)", "\\("), replacement = c("", "","","","-")))
}

###########################################################

#Function 3
# function used to generate a reliable line code field.
line_code <- function(exhibit) 
  {
  # First we check if the first column has been delimited like "1." or if it's like "1. Bonds"
  # The former can be coerced as is to a numeric, and the latter cannot. 
  # End result either way is we create a new column that represents the first number in each row.
  
  if (has_warning(as.numeric(exhibit[1,1]))) {
      col_1 <- sapply(strsplit(exhibit[,1],split = " "),'[',1)
      }
  else {col_1 <- exhibit[,1]}
  
# So far this has handled the two types of situations of V1 having either "1." or "1. Bonds"
# This next line of code avoids the rare case when the company accidentally didn't
# put a space in betweeen the line code and the description
 
  for(i in 1:length(col_1)) {

    if(has_warning(as.numeric(col_1[i]))
       &length(regmatches(col_1[i], regexpr(pattern="\\d+\\.\\d?",text=col_1[i]))!=0)
       )
    {
    col_1[i] <- regmatches(col_1[i], regexpr(pattern="\\d+\\.\\d?",text=col_1[i]))
    }
    next
}
 
  col_1 <- as.numeric(col_1)
 
 # See if Line 2.1 is on 2nd column and if that cell is not blank. Also checks for income statement reliably. 
 # Also checks 36.2 for Page3 reliably.
 #Also checks 
 if ((!has_warning(as.numeric(sapply(strsplit(as.character(exhibit[3,2]),split = " "),'[',1)))
     | !has_warning(as.numeric(sapply(strsplit(as.character(exhibit[(nrow(exhibit)-2),2]),split=" "),'[',1))) 
     | !has_warning(as.numeric(sapply(strsplit(as.character(exhibit[(nrow(exhibit)-1),2]),split=" "),'[',1))))               
     & (!((exhibit[3,2]=="") %in% TRUE) & !is.na(exhibit[3,2]))
 ) {
   
   col_2 <- sapply(strsplit(as.character(exhibit[,2]),split = " "),'[',1)
   
   for(i in 1:length(col_2)) {
     
     if(has_warning(as.numeric(col_2[i]))
       &length(regmatches(col_2[i], regexpr(pattern="\\d+\\.\\d?",text=col_2[i]))!=0)) 
       {
       col_2[i] <- regmatches(col_2[i], regexpr(pattern="\\d+\\.\\d?",text=col_2[i]))
     }
     next
   }
   
   col_2 <- as.numeric(col_2)
   for (i in 1:length(col_1)){
     if(is.na(col_1)[i]){
      col_1[i] <- col_2[i]
    }
    next
 }
}   
   
 # What this part does is assign a line code for lines that are NA based on prior valid lines
 for (i in 1:length(col_1)) {
   if (!is.na(col_1[i])) 
     {
    col_1[i] <- col_1[i]
   }
   else {
     col_1[i] <- col_1[i-1]
   }
   next
 } 
 
   col_1<<-col_1 #Finally we save the line code column into the global environment. 

    }

###############################################
# Function 3a - A simplified version of line_code, intended for page14 only. 

line_code_p14 <- function(exhibit) 
{
  # First we check if the first column has been delimited like "1." or if it's like "1. Bonds"
  # The former can be coerced as is to a numeric, and the latter cannot. 
  # End result either way is we create a new column that represents the first number in each row.
  
  if (has_warning(as.numeric(exhibit[1,1]))) {
    col_1 <- sapply(strsplit(exhibit[,1],split = " "),'[',1)
  }
  else {col_1 <- exhibit[,1]}
  
  # So far this has handled the two types of situations of V1 having either "1." or "1. Bonds"
  # This next line of code avoids the rare case when the company accidentally didn't
  # put a space in betweeen the line code and the description
  
  for(i in 1:length(col_1)) {
    
    if(has_warning(as.numeric(col_1[i]))
       &length(regmatches(col_1[i], regexpr(pattern="\\d+\\.\\d?",text=col_1[i]))!=0)
    )
    {
      col_1[i] <- regmatches(col_1[i], regexpr(pattern="\\d+\\.\\d?",text=col_1[i]))
    }
    next
  }
  
  col_1 <- as.numeric(col_1)
  
  # See if Line 2.1 is on 2nd column and if that cell is not blank. Also checks for income statement reliably. 
  # Also checks 36.2 for Page3 reliably.
  #Also checks 
  
  
  # What this part does is assign a line code for lines that are NA based on prior valid lines
  for (i in 1:length(col_1)) {
    if (!is.na(col_1[i])) 
    {
      col_1[i] <- col_1[i]
    }
    else {
      col_1[i] <- col_1[i-1]
    }
    next
  } 
  
  col_1<<-col_1 #Finally we save the line code column into the global environment. 
  
}



#########################################################################

# Function 4 - Locate in-line numbers

# This function should basically scan all cells in a data frame for a certain pattern, and then create a new vector
# that contains the matching text. The text to look for is basically just a $, and then everything appearing afterward in that
# same cell. This should get us what we really care about, namely the written premium in-line numbers and the RCG tax. 

inline <- function(exhibit) {
  
# input will be the scanned page as already obtained.
  
  inline_values <- data.frame(col1=1,col2=1) #Setting this up this way seems to fix the issue of the function failing due
                                          #assumption it is a factor variable instead of character.
  
for (i in 1:nrow(exhibit)) {
    for (j in 1:ncol(exhibit)) {
      
      inline_values <- rbind(inline_values,as.character(regmatches(exhibit[i,j], regexpr(pattern="\\$.+", text = exhibit[i,j]))))
      
      next
}
  next

}
   inline_values_p4 <<- inline_values[2:5,1]  
      
      }
##################################################################

# Plan for this file:
# 1. User first places PDF's into designated folder
# 2. Algorithm R will follow will be to first obtain the correct page numbers to extract, as 
# unfortunately pages definitely vary between PDF's due to the length of Notes and Disclosures.
# 3. The identified correct pages are saved as a vector correct_page_nos.
# 4. correct_page_nos is passed to the pdf_test command, which extracts the tables from a given PDF object. Area has been predetermined to be uniform once correct pages are selected. 
# 5. Each page is cleaned up and saved as its own dataframe.
# 6. 


# setwd("C:/Users/nated/Desktop/Data Science/work")



# 

file_num <- length(dir()) # How many files are in the folder. This info is passed to the loop. 


for (i in 1:file_num) {  
  
test_file <- dir()[i] # Setting the ith PDF as the one being dissected.

naic_num <- sapply(strsplit(test_file,,split = " "),'[',1)
naic_row <- matrix(c("NAIC Number",naic_num),ncol=2)

# testing
# test_file <- dir()[1]
# Detecting appropriate pages
pdftext <- pdf_text(test_file)
page2 <- grep("Bonds",pdftext)[1] #Asset Page
page3 <- page2 + 1 #Liability Page
page4 <- page3 + 1 #Income Statement
page5 <- page4 + 1 #Cash Flow Statement
page14 <- grep("Years in",pdftext)[1] #Part3 Loss and LAE Schedule
correct_page_nos <- c(page2,page3,page4,page5,page14) #temporary assignment while we work on first two pages

# correct_page_nos <- c(page3,page3,page4,page5,page14) #correct pages of PDF to be passed to next function

# The difficult part of this procedure is reliably identifying the exact coordinates that should be
# used to denote the area from which the PDF's are scanned. It is not workable to allow R to auto-
# -matically find it, because this breaks down every time. Instead we will use functions to locate
# reliable words from which we can reasonably extrapolate the correct X/Y coordinates. Note that
# the area from locate_areas used by extract_tables is of type (top,left,bottom,right) and each of 
# so top left corner is (0,0) and bottom right corner is (612,1008), so therefore the correct
# syntax would be (0,0,1008,612)

 page2_data <- pdf_data(test_file)[page2][[1]]
 page2_top <- with(page2_data,subset(y,text == "1.")) 
 page2_left <- with(page2_data,subset(x,text == "1.")) - 5
 page2_bottom <- with(page2_data,subset(y,text == "28.")) + 3 + with(page2_data,subset(height,text == "28."))
 page2_right  <- 590
 
 page2_area <- c(page2_top,page2_left,page2_bottom,page2_right)



 page3_data <- pdf_data(test_file)[page3][[1]]
 page3_top <- with(page3_data,subset(y,text == "1.")) 
 page3_left <- with(page3_data,subset(x,text == "1.")) - 5
 page3_bottom <- with(page3_data,subset(y,text == "38.")) + 3 + with(page3_data,subset(height,text == "38."))
 page3_right  <- 590
 
 page3_area <- c(page3_top,page3_left,page3_bottom,page3_right)

 page4_data <- pdf_data(test_file)[page4][[1]]
 page4_top <- with(page4_data,subset(y,text == "1.")) 
 page4_left <- with(page4_data,subset(x,text == "1.")) - 5
 page4_bottom <- with(page4_data,subset(y,text == "39.")) + 3 + with(page4_data,subset(height,text == "39."))
 page4_right  <- 590
 
 page4_area <- c(page4_top,page4_left,page4_bottom,page4_right)
 
 page5_data <- pdf_data(test_file)[page5][[1]]
 page5_top <- with(page5_data,subset(y,text == "1.")) 
 page5_left <- with(page5_data,subset(x,text == "1.")) - 5
 page5_bottom <- with(page5_data,subset(y,text == "19.2")) + 3 + with(page5_data,subset(height,text == "19.2"))
 page5_right  <- 590
 
 page5_area <- c(page5_top,page5_left,page5_bottom,page5_right)
 
 page14_data <- pdf_data(test_file)[page14][[1]]
 
 page14_top <- with(page14_data,subset(y,text == "1."))[1] 
 page14_left <- with(page14_data,subset(x,text == "1."))[1] 
 page14_bottom <- with(page14_data,subset(y,text == "7.")) + 3 + with(page14_data,subset(height,text == "7."))
 page14_right  <- 1000
 
 page14_area <- c(page14_top,page14_left,page14_bottom,page14_right)
 
 
 #page14b_top <- with(page14_data,subset(y,text == "5."))[1]+5+with(page14_data,subset(height,text == "5."))[1]
 #page14b_left <- with(page14_data,subset(x,text == "1."))[1] 
# page14b_bottom <- with(page14_data,subset(y,text == "6.")) + 3 + with(page14_data,subset(height,text == "6."))
 #page14b_right  <- 1000
 
# page14b_area <- c(page14b_top,page14b_left,page14b_bottom,page14b_right)
 
 
 # Extracting all pages at once and putting them into one object 
pdf_test <- extract_tables(test_file, output = "data.frame", pages = correct_page_nos, 
                           area = list(page2_area,page3_area,page4_area, page5_area,page14_area), 
                           guess = FALSE, header=FALSE )

#pdf_test_p14 <- extract_tables(test_file, output = "data.frame", pages = page14, columns = list(c(50, 150, 250)),
#                               guess = FALSE, header = FALSE)


# Creating asset page

p2 <- pdf_test[[1]]


line_code(p2) # generates line_code vector, assigned to "col_1"

p2 <- p2[,(ncol(p2)-3):ncol(p2)] # Guarantees we return only the data
#Cleans out extraneous characters from data and replaces "(" with "-")
p2[] <- sapply(p2,clean)
p2[is.na(p2)] <- 0

p2 <- cbind(as.numeric(col_1),p2) #Appends line code as first column

# Assigns correct column names
names(p2) <-  c("Line_Code","Admitted_Assets","Nonadmitted_Assets","Net_Admit_Assets","PY_NET_Asset")



p2 <- filter(p2,!(Line_Code %in% c(2.0, 3.0,4.0,15.0,16.0)))
p2 <- group_by(p2,Line_Code)

p2 <- summarize(p2,Admitted_Assets = sum(Admitted_Assets), Nonadmitted_Assets = sum(Nonadmitted_Assets),
                Net_Admit_Assets = sum(Net_Admit_Assets),PY_NET_Asset = sum(PY_NET_Asset))

#Assigns consistent row names
row.names(p2) <- c("1. Bonds", "2.1 Preferred Stocks", "2.2 Common Stocks", "3.1 First liens",
                  "3.2 Other than first liens", "4.1 Prop occupied by company", 
                  "4.2 Prop held for prod of income", "4.3 Prop held
                   for sale", "5. Cash", "6. Contract loans", "7. Derivatives", "8. Other invested assets",
                   "9. Receivables for Securities", "10. Sec lending", "11. Agg write in for C&IA", 
                   "12. Subtotal,C&IA",
                   "13. Title plants", "14. Investment income due", "15.1 Uncollected prmeiums", "15.2 Deferred
                   Premiums","15.3 Accrued Retro Premiums", "16.1 Amounts recoverable from reinsurers", 
                   "16.2 Funds held by or deposited with reinsured companies", 
                   "16.3 Other amounts receivable under reinsurance contracts",
                   "17. Amounts receivable relating to uninsured plans",
                   "18.1 Tax Recoverable", "18.2 Net DTA", "19. GF receivable", "20. Electronic data equipment", 
                   "21. Furniture and equipment", "22. Net adjustment for forex", "23. Receivables from parent", 
                   "24. Health care and other amounts receivable", "25. Agg write ins all other", 
                   "26. Total Assets excluding separate accounts(12-25)", "27. Sep, Seg, and Protected", 
                   "28. Total Assets"
                   )

# View(p2)

######################################

# Formatting page 3 - Liability Page
p3 <- pdf_test[[2]]

line_code(p3) # generates line_code vector, assigned to "col_1"

p3 <- p3[,(ncol(p3)-1):ncol(p3)] # Guarantees we return only the data
#Cleans out extraneous characters from data and replaces "(" with "-")
p3[] <- sapply(p3,clean)
p3[is.na(p3)] <- 0

p3 <- cbind(as.numeric(col_1),p3) #Appends line code as first column

# Assigns correct column names
names(p3) <-  c("Line_Code","Current_YR","Prior_YR")



p3 <- filter(p3,!(Line_Code %in% c(11.0,36.0)))
p3 <- group_by(p3,Line_Code)

p3 <- summarize(p3, Current_YR = sum(Current_YR), Prior_YR = sum(Prior_YR))

row.names(p3) <- c("1. Losses","2. Reinsurance Payable", "3. LAE", "4. Commissions payable", 
                   "5. Other expenses", "6. non-income Taxes", "7.1 Federal income tax","7.2 Net DTL", 
                   "8. Borrowed Money", "9. Unearned Premium Reserve", "10. Advance Premium",
                   "11.1 Stockholder dividends", "11.2 PH Dividends", "12. Ceded Re Premiums payable",
                   "13. Funds held for reinsurance treaties", "14. Amounts witheld", "15. Remittances",
                   "16. Provision for reinsurance", "17. Net forex adjustments", "18. Drafts outstanding",
                   "19. Payable to parents", "20. Derivatives", "21. Payable for sec", 
                   "22. Payable for sec lending", "23. Uninsured plan liab", "24. Cap notes",
                   "25. Agg liab write in", "26. Total liabilities excl. prot cells", "27. Prot cells",
                   "28. Total Liabilities", "29. Agg write in for special funds", "30. Common stock",
                   "31. Preferred Stock", "32. Agg write ins other than special", "33. Surplus notes",
                   "34. Gross paid in and contributed surplus", "35. Unassigned funds (surplus)", 
                   "36.1 Less common treasury stock", "36.2 Less preferred treasury stock",
                   "37. Policyholders' surplus", "38. Totals")

# View(p3)

##############################################

# Formatting Page 4 - Income Statement

p4 <- pdf_test[[3]]
inline(p4) # reads the in-line WP items in 1.1-1.4
line_code(p4) # generates line_code vector, assigned to "col_1"

p4 <- p4[,(ncol(p4)-2):ncol(p4)] # Guarantees we return only the data
#Cleans out extraneous characters from data and replaces "(" with "-")
p4[] <- sapply(p4,clean)
p4[is.na(p4)] <- 0

p4 <- cbind(as.numeric(col_1),p4) #Appends line code as first column

# Assigns correct column names
names(p4) <-  c("Line_Code","Current_YR","Prior_YR_to_date","Prior_YR_total")



p4 <- filter(p4,!(Line_Code %in% c(1.0,2.0,32.0,33.0)))
p4 <- group_by(p4,Line_Code)

p4 <- summarize(p4, Current_YR = sum(Current_YR), Prior_YR = sum(Prior_YR_to_date), Prior_YR_full_year = sum(Prior_YR_total))

row.names(p4) <- c("1.1 Direct EP", "1.2 Assumed EP", "1.3 Ceded EP", "1.4 Net EP", "2.1 Direct LI", "2.2 Assumed LI",
                   "2.3 Ceded LI", "2.4 Net LI", "3. LAE Incurred", "4. OUE Incurred", "5. Agg W/I for U/W", 
                   "6. Total U/W deduction (2-5 sum)", "7. Net income protected cell", "8. Net underwriting gain/loss (1-6+7",
                   "9. NIIE", "10. Net realized capital gain", "11. Net investment gain", "12. Net gain from agents", 
                   "13. Service charges", "14. Agg W/I for misc inc", "15. Total OI (12-14)", "16. Net income before PHD",
                   "17. PHD", "18. Net income before tax", "19. Taxes", "20. Net income", "21. Surplus PY", "22. Net income",
                   "23. Net transfers to protected cells", "24. Change in unreal cap g", "25. chg in unreal forex", 
                   "26. Chg in Deferred tax", "27. Chg in nonadmitted assets", "28. Chg in provision for re", "29. Chg in SN",
                   "30. Surplus +- Protected Cells", "31. Cuulative accounting chg", "32.1 Capital Paid in", 
                   "32.2 Capital transferred from surplus", "32.3 Capital transferred to surplus", "33.1 Surplus paid in",
                   "33.2 Surplus transferred to capital", "33.3 Surplus transferred from capital", "34. Net remittances",
                   "35. Stockholder dividends", "36. Chg in treasury stock", "37. Agg W/I for surplus", "38. Chg in surplus",
                   "39. Policyholders' surplus")

# View(p4)

inline_values_p4[] <- sapply(inline_values_p4,clean)
inline_values_p4[is.na(inline_values_p4)] <- 0
inline_values_p4 <- as.numeric(inline_values_p4)
names(inline_values_p4) <- c("Direct WP", "Assumed WP", "Ceded WP", "Net WP")

# View(inline_values_p4)
# setwd("C:/Users/i53642/Desktop/Data Science/Coursera")

#####################################################

# Formatting Cash Flow Exhibit (p5)
p5 <- pdf_test[[4]]
line_code(p5) # generates line_code vector, assigned to "col_1"

p5 <- p5[,(ncol(p5)-2):ncol(p5)] # Guarantees we return only the data
#Cleans out extraneous characters from data and replaces "(" with "-")
p5[] <- sapply(p5,clean)
p5[is.na(p5)] <- 0

p5 <- cbind(as.numeric(col_1),p5) #Appends line code as first column

# Assigns correct column names
names(p5) <-  c("Line_Code","Current_YR","Prior_YR_to_date","Prior_YR_total")



p5 <- filter(p5,!(Line_Code %in% c(12.0,13.0,16.0,19.0)))
p5 <- group_by(p5,Line_Code)

p5 <- summarize(p5, Current_YR = sum(Current_YR), Prior_YR = sum(Prior_YR_to_date), Prior_YR_full_year = sum(Prior_YR_total))
row.names(p5) <- c("1. Prems collected net of reinsurance", "2. Net inv income", "3. Misc income",
                   "4. TOTAL (1-3)", "5. Benefit and loss payments", 
                   "6. Net transfers to Sep, Seg, Protected", "7. Commissions, exp, and agg w/i", 
                   "8. Dividends paid to Policyholders", "9. Federal and foreign income tax", 
                   "10. TOTAL (5-9)", "11. Net cash from operations (4 minus 10)", "12.1 Bonds",
                   "12.2 Stocks", "12.3 Mortgage loans", "12.4 Real Estate", "12.5 Other invested",
                   "12.6 Net gains on cash", "12.7 Misc proceeds", 
                   "12.8 TOTAL inv proceeds (12.1-12.7)", "13.1 Bonds",
                   "13.2 Stocks", "13.3 Mortgage loans", "13.4 Real Estate", "13.5 Other invested",
                   "13.6 Misc applications", 
                   "13.8 TOTAL acquired (13.1-13.6)", "14. Net chg contract loans", 
                   "15. Net cash from investments", "16.1 Surp notes", 
                   "16.2 Capital and paid in surplus, less treasury", "16.3 Borrowed funds", 
                   "16.4 Net deposits on contracts", "16.5 Dividends to stockholders", 
                   "16.6 Other cash provided", "17. Net cash from financing", 
                   "18. Net change in cash", "19.1 Beginning year cash", "19.2 End of period cash")
# View(p5)

############################################

# Obtaining Part 3 information. Note this has to be done in 3 chunks because the extract_table
# function failed to properly differentiate columns due to the "XXX" being centered while other 
# numbers are aligned right. Since in some cases there is no overlap between the two types of fields
# R treats it as two columns when it's really one column. The workaround to this is to read Part3
# in 3 separate rounds, stripping the row with XXX to its own area. 

p14 <- pdf_test[[5]]
line_code_p14(p14)

#There are two possible outcomes observed so far with respect to Part 3. Either R will correctly
# identify the the "XXX" and numbers above and below it are on same column or it will treat it as two
# separate columns. The below code handles the case in which R does it correctly. 

if (ncol(p14)<20) {
  
p14 <- p14[,(ncol(p14)-12):ncol(p14)] # Guarantees we return only the data

}

if (ncol(p14)>20) {
  
  p14 <- p14[,(ncol(p14)-20):ncol(p14)] # Guarantees we return only the data
  p14 <- p14[,c(2,4,6,8,9,10,12,13,14,15,17,19,21)]
  }
  

#Cleans out extraneous characters from data and replaces "(" with "-")
p14[] <- sapply(p14,clean)
p14[is.na(p14)] <- 0
p14 <- cbind(as.numeric(col_1),p14) #Appends line code as first column
names(p14) <- c("Line_Code","PY_Case_Loss_LAE", "PY_IBNR", "Total_PY", "CY_on_prior",
                "CY_prior_unreported", "Total_2019_pay", "Q.S._Known_Case",
                "Q.S._Reopened", "Q.S._IBNR", "Total_Q.S._LLAER", "PY_Case_Dev",
                "PY_IBNR_Dev", "PY_Total_Dev")

p14 <- group_by(p14,Line_Code)

p14 <- summarize(p14, PY_Case_Loss_LAE = sum(PY_Case_Loss_LAE), PY_IBNR = sum(PY_IBNR), Total_PY = sum(Total_PY),
                  CY_on_prior = sum(CY_on_prior),CY_prior_unreported = sum(CY_prior_unreported),
                  Total_2019_pay = sum(Total_2019_pay),
                  Q.S._Known_Case = sum(Q.S._Known_Case),Q.S._Reopened = sum(Q.S._Reopened),
                  Q.S._IBNR = sum(Q.S._IBNR),Total_Q.S._LLAER = sum(Total_Q.S._LLAER),
                  PY_Case_Dev = sum(PY_Case_Dev),
                  PY_IBNR_Dev = sum(PY_IBNR_Dev), PY_Total_Dev = sum(PY_Total_Dev))

row.names(p14) <- c("1. 2016 and prior", "2. 2017", "3. Sub 2017 + Prior", "4. 2018", 
                   "5. Sub 2018 + Prior", "6. 2019", "7. Totals")

#########################################
#Processing Line 6 only
########################


# View(p14)

cat("Hand Input #",i,"\n",file = paste(dest_folder,"HInput.csv",sep=""),sep="", append = TRUE)
write.table(naic_row,file = paste(dest_folder,"HInput.csv",sep=""),sep=",",row.names=FALSE, col.names = FALSE, append = TRUE)
write.table(p2,file = paste(dest_folder,"HInput.csv",sep=""), sep=",", col.names = NA, row.names = TRUE, append = TRUE)
write.table(p3,file = paste(dest_folder,"HInput.csv",sep=""), sep=",", col.names = NA, row.names = TRUE, append = TRUE)
write.table(p4,file = paste(dest_folder,"HInput.csv",sep=""), sep=",", col.names = NA, row.names = TRUE, append = TRUE)
write.table(p5,file = paste(dest_folder,"HInput.csv",sep=""), sep=",", col.names = NA, row.names = TRUE, append = TRUE)
write.table(p14,file = paste(dest_folder,"HInput.csv",sep=""), sep=",", col.names = NA, row.names = TRUE, append = TRUE)
cat("\n",file = paste(dest_folder,"HInput.csv",sep=""),sep="", append = TRUE)
next

}

