# Hand_Input_Project

Current status of the R Script:
Testing was completely successful for all files in Batch1 folder.
However, in Batch2 it failed to properly process the Part3 table (variable 'p14') for several PDF's. 
The specific error was artificially creating columns that did not actually exist.


Currently, the logic handles only two processing scenarios for Page14:
1) Treats the 13 data columns as 13 columns (occurs when the XXX and numbers overlap and there isn't a lot of margin on either side of values)
2) Splits the "XXX" and numerical portions into separate columns. Occurs when the "XXX" and other row values do not overlap in the x position closely enough. Currently the code is equipped to handle this scenario by essentially picking only the columns that have the data in them and ignoring the XXX columns. This only works if it is split predictably in a uniform fashion. 
3) 



Recommended update:
1) Update possible logic for handling p14 so it more robustly handles various possibilities. 
  a. One thing to explore is to see what would happen if you captured last row of text for column headers. I am not very confident this would help things - probably would end up harming more than helping to be honest. 
  b. 
