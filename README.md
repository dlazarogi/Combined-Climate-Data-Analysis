# Combined-Climate-Data-Analysis

The Combined Climate Data Analysis tool is a Shiny app designed to send geographic coordinates and obtain environmental data related to light, its duration, its quality, as well as temperature data for academic purposes.

For correct operation, the data is entered in the latitude and longitude boxes, separating each value by a comma. 3 examples,<b> 63.27N,16.01E</b>; <b>63.82N20.26E</b>;<b>66.70N22.53E</b> will be indicated as

<b>Enter Latitudes (comma-separated):</b>

63.27,63.82,66.70

<b>Enter Longitudes (comma-separated):</b>

16.01,20.26,22.53

The public repositories used in the query are:

* suncalc library in R to calculate Day Lenght plot according to the latitude.

* NASA Prediction Of Worldwide Energy Resources (POWER) database. Specifically a recalculation of the daily Averaged Daily CERES All Sky Surface Shortwave Downward Irradiance. The total solar irradiance incident (direct plus diffuse) on a horizontal plane at the surface of the earth under all sky conditions. An alternative term for the total solar irradiance is the "Global Horizontal Irradiance" or GHI.

* National Oceanic and Atmospheric Administration (NOAA) database to obtain temperature data.

You can select a Year Range from one to several years. The only limitation is a query to an unfinished year. The recomendation is not to extend the range to unfinished years, as not all the data will be available in some of the databases.


# Usage
The Shiny app has included checkups for dependencies. Any initial usage will confirm all dependencies are installed, otherwise, they will be installed and confirmation for updates will be displayed specifically in your system. If some dependency is not properly installed it will be reported

![Screenshot 2024-08-24 at 10-40-26 Combined Climate Data Analysis](https://github.com/user-attachments/assets/d507882c-3e9f-4c25-90bc-0e01a6eaf967)

When running the app with the default coordinates, The following plots will be generated:

<b>Day length attending to latitude</b>

![Screenshot 2024-09-02 at 13-19-30 Combined Climate Data Analysis](https://github.com/user-attachments/assets/c65f45ca-d9be-4714-81ba-923237573beb)


<b>Irradinace plot</b>

<img width="878" alt="2" src="https://github.com/user-attachments/assets/c838eb94-d3b9-4ed8-af4e-a48df943097f">

<b>Temperature plot</b>

<img width="878" alt="3" src="https://github.com/user-attachments/assets/5b50eeb4-3ca6-4197-b8bf-d868898b408b">


You will be able to download the tables in excel format, individual plots at high resolution and a PDF document with all the plots.

# R version and platform

R version 4.4.1 (2024-06-14) -- "Race for Your Life"

platform       x86_64-pc-linux-gnu  
