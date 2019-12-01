# Bluescope-Engineering
Dashboards and analysis for engineering business at Bluescope

## Purpose
Created for an hoc analysis and visuals for monthly reporting pack. Engineering has been somewhat of a blackhole for our organization until a year ago. Prior to my current role as analyst for the engineering group, there was no single finance person tasked with preparing/analyzing their metrics and tracking them against annual targets. Engineering prepared their own metrics and self-assessed their performance. 

Working with historical data is difficult because the business did not record all the data in one place (if they did at all). I've created a tidy table of historical data going back two years and would like to go back a couple more years to better understand some of the organizational changes we've made to this production team.

The work in this project consists of visuals for the monthly reporting pack, dashboards put together for the regional managers on a weekly basis, and ad hoc analysis. The analysis piece is mostly around engineering capacity as well as lead time calculations. I should rename each file to indicate what type of script it is now that I think about it. Will add that to the list of things to do.

## Data

I really didn't run into any difficulties with the data until I started working on lead times. The lead times analysis was difficult because it dealt with dates. The challenge was more about learning how to use lubridate package than it was anything data-related. Lubridate makes working with dates very simple.

## To Do List

-[] figure out system for running necessary reports from BI, saving down to local folder. 

      *This will allow me to start scheduling specific R scripts 
      
-[] resize the MBR charts so they are final report size

-[] see if there's a way to take the MBR charts that get saved and add to excel spreadsheet

      *another option would be to add MBR charts to final PDF
      
-[] rename scripts to indicate what type they are