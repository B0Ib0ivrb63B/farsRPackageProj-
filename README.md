# farsRPackageProj
[![Travis-CI Build Status](https://travis-ci.org/B0Ib0ivrb63B/farsRPackageProj-.svg?branch=master)](https://travis-ci.org/B0Ib0ivrb63B/farsRPackageProj-)

title: "Into to the farsRPackageProj"<br/>
author: "CJ Muller"<br/>
date: 04-29-2018


## Purpose

This is an R Markdown document for the package 'farsRPackageProj'. This is a demenstration of knittr and R markdown 'vignette' in fulfillment of the course objectives for '<span style="color:blue">Coursera course 'Master Soft Dev in R by Johns Hopkins, course # 3 Building R Packages, week 4</span>'.


## Introduction and Description

The file 'fars_functions.R' are a set of 5 functions provided by Coursera to demonstrate how to plot observations for fatalities seen on the US highway system.  Data are from the US National Highway Traffic Safety Administration's Fatality Analysis Reporting System, which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes. See: <http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)>
 
## Functions provided

The following 5 functions are used to plot these datasets on a png state map based on latitude and longitude of the observations:


1. **fars_read**(filename) - function to read a semicolon (;) delimited file (including csv, tsv, .gz, .bz2, or .zip) into a tibble, for example:

```
          filename ="accident_2013.csv.bz2"
          fars_read(filename)
```

2. **make_filename**(year) - utility function used by the function'fars_read_years' to create a string to be used as a filename of the format 'accident_(year).csv.bz2', for example:

```
          make_filename(99)
```

3. **fars_read_years**(year) - function to read the US National Highway Traffic Safety data by individual file year, placing them into a tibble (dataframe), for example:

```
          fars_read_years(2013)
          fars_read_years(c(2013:2015))
```

4. **fars_summarize_years**(year) - function to read the US National Highway Traffic Safety data by one or more years and summarize number of observations for each month  into a tibble (dataframe) 12 x (number of years specified), for example:

```
          fars_summarize_years(2013)
          fars_summarize_years(c(2013:2015))
```    

5. **fars_map_state**(year) - function to read the US National Highway Traffic Safety data and plot plot the of observations for the given year on a png state map' based on latitude and longitude of the observation, (Alabama = 1, Wyoming = 50, etc), for example:

```
          fars_map_state(1,2013)  # Alabama
          fars_map_state(12,2014) # Florida
```  


## Sample data sets included

The file 'fars_data.zip' has been included as a smaple data set in the 'data-raw' folder of the package. It contain data for 3 years work of fatalities:

* accident_2013.csv.bz2
* accident_2014.csv.bz2
* accident_2015.csv.bz2
