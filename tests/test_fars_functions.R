
#------------------------------------------------------------------------------------------------------------------------------------------------
#
# TESTING for function 'fars_read'
#
# > filename ="C://Users//cjmsn//Desktop//Coursera//3 Master Soft Dev in R - Johns Hopkins//3 Building R Packages//Week2 - Documentation//fars_data//data//accident_2013.csv.bz2"
# [1] fars_read(filename)
# A tibble: 30,202 x 50
#STATE ST_CASE VE_TOTAL VE_FORMS PVH_INVL  PEDS PERNOTMVIT PERMVIT PERSONS COUNTY  CITY   DAY MONTH  YEAR DAY_WEEK  HOUR MINUTE
#<int>   <int>    <int>    <int>    <int> <int>      <int>   <int>   <int>  <int> <int> <int> <int> <int>    <int> <int>  <int>
#  1     1   10001        1        1        0     0          0       8       8    115     0     6     1  2013        1     0     55
#  2     1   10002        2        2        0     0          0       2       2     55  1670     3     1  2013        5    21     24
#  3     1   10003        1        1        0     0          0       1       1     89  1730     6     1  2013        1    11     45
# etc...
#
#------------------------------------------------------------------------------------------------------------------------------------------------



#---------------------------------------------------
# TESTING for function 'make_filename'
# (requires 'testthat' package)
#
# > make_filename(99)   --> expected  [1] "accident_99.csv.bz2"
# > make_filename(2002) --> expected  [1] "accident_2002.csv.bz2"
# > make_filename(11.2) --> expected  [1] "accident_11.csv.bz2"
# > make_filename(002)  --> expected  [1] "accident_2.csv.bz2"
#---------------------------------------------------
test_functions <- function() {

  #---------------------------------------------------
  # TESTING for function 'make_filename'
  #---------------------------------------------------
  cat("TESTING FUNCTION: 'make_filename' 4 TIMES","\n")

  if (equals(make_filename(99),"accident_99.csv.bz2"))
    cat(" 1 PASSED make_filename(99) ","\n")
  else
    cat(" 1 Failed make_filename(99) ","\n")

  if (equals(make_filename(2002),"accident_2002.csv.bz2"))
    cat(" 2 PASSED make_filename(2002) ","\n")
  else
    cat(" 2 Failed make_filename(2002) ","\n")

  if (equals(make_filename(11.2),"accident_11.csv.bz2"))
    cat(" 3 PASSED make_filename(11.2) ","\n")
  else
    cat(" 3 Failed make_filename(11.2) ","\n")

  if (equals(make_filename(002),"accident_2.csv.bz2"))
    cat(" 4 PASSED make_filename(002) ","\n")
  else
    cat(" 4 Failed make_filename(002) ","\n")
}

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}



#-------------------------------------------------------------
#
# TESTING for function 'fars_read_years'
#
# > fars_read_years(2013)
# [[1]]
#  A tibble: 30,202 x 2
#  MONTH  year
#  <int> <dbl>
#     1     1  2013
#     2     1  2013
#     3     1  2013
# ... with 30,046 more rows
#
#-------------------------------------------------------------



#---------------------------------------------------------------
#
# TESTING for function 'fars_summarize_years'
#
# > fars_summarize_years(2013)
# [[1]]
# > fars_summarize_years(c(2013:2015))
# A tibble: 12 x 4
# MONTH `2013` `2014` `2015`
# * <int>  <int>  <int>  <int>
#   1     1   2230   2168   2368
#   2     2   1952   1893   1968
#   3     3   2356   2245   2385
#   4     4   2300   2308   2430
#   5     5   2532   2596   2847
#   6     6   2692   2583   2765
#   7     7   2660   2696   2998
#   8     8   2899   2800   3016
#   9     9   2741   2618   2865
#   10    10   2768   2831   3019
#   11    11   2615   2714   2724
#   12    12   2457   2604   2781
#
#---------------------------------------------------------------


