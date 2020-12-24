# hhlamba
This Poster was designed for:
1. Tourists at NYC
2. People living in NYC
3. NYPD 
4. NYC311

To see the data in action you can also visit the Dashboards I created:


<p>
  <a href="https://public.tableau.com/profile/hhlamba#!/vizhome/NYC311IncidentsDashboard/Dashboard1" target="_blank">Tableau Dashboard</a>  
</p>

<p>
  <a href="https://app.powerbi.com/view?r=eyJrIjoiOWU4NDZjNjAtNDM2NS00OWM4LTlmNTgtNjZiOGMzNDU5NmQxIiwidCI6IjQyNzhhNDAyLTFhOWUtNGViOS04NDE0LWZmYjU1YTVmY2YxZSIsImMiOjN9" target="_blank">Power Bi Dashboard</a>  
</p>


Poster:
<p align="center">
  <img src="https://github.com/hhlamba/NYC311-Issues-in-the-Borough/blob/main/Output/Resource-Planning-Strategies.jpg" width="500" title="NYC311 Awareness Strategy" border="1">
</p>

I found this Dataset on Kaggle [NYC311](https://www.kaggle.com/sherinclaudia/nyc311-2010), I never heard of NYC311 before and started finding what is it all about. NYC311 is a number that you call when you have any issue while you are in NYC. It's kind of 911 but mainly for non-emergencies only. NYC311 could help you with information also.


The main idea for choosing this dataset was, could I rely on NYC311 to respond to my issues, the answer was in this Dataset. I started exploring the data set by some single dimentional plots that showed me the shape of the data.

Plot 1: Shows the Bourough that has most problems : 
            Observation: Brooklyn has most number of issue calls
                         Brooklyn > Queens > Manhattan > Bronx > Staten Island.
                         
Plot 2: Shows the top 4 Issues for which NYC311 got calls : 
            Observation: Noise is the most called issue
                         Noise > Blocked Driverway > Illegal Parking > Derelict Vehicle

Now digging deep in time series analysis.

Plot 3: What part of the year were each borough actively calling
            Observation: According to the dataset, all boroughs were highly active throughout the year, except Queens(mid-March to mid-September) & Staten Island(mid-March to mid-October) 

Done with the year, now I was analyzing the daily calling rate for Top 4 Issues.

Plot 4: Weekly spread of the Top 4 issues
            Observation: As we work from Monday to Friday, Blocked Driveway and Derelict Vehicle were higher than Noise Complaints
                         But as weekend came closer its party time, hence the Noise complaints increased from Friday to Sunday and makes it the Top Complaint.

Plot 5: Hourly spread of these Issues
            Observation: Similarly we leave for work in the morning and come back in the evening, and we might face the Blocked driveway or illegal parking issues, as we have to leave for work and there is something thats obstructing you from getting there.
                         But night time is filled with Noise complaints only, this number starts to increase drastically from 8:00 p.m and decreases back at 3:00 in the morning.

Now it was time to justify the reliability of NYC311.

Plot 6: Average issues resolution hours for the Top 4 issues.
            Observation: It takes less than 2.33 hours to resolve any issue on an average.

Plot 7: Ratio of Open to Closed Complaints in the entire year.
            Observation: 99% of the issues out of 0.365 million issues are Closed



Hence, this could be helpful to justify NYC311's performance, so anyone who is travelling to NYC or is a resident of NYC could use this to resolve their problems.

But how would this help NYPD and NYC311?
They could use this poster(or create a similar dashboard) to visualize the spread of issues with respect to Boroughs and time, week or month of the year. This could help them manage their resources better and resolve the issues quickly, hence reducing the average time and would help gain and retain the user base.


You can also visit my Dashboards to see the data in action:

<p align="center">
  <a href="https://public.tableau.com/profile/hhlamba#!/vizhome/NYC311IncidentsDashboard/Dashboard1" target=_blank>Tableau Dashboard</a>  
</p>


<p align="center">
  <img src="https://github.com/hhlamba/NYC311-Issues-in-the-Borough/blob/main/Output/Tableau.png" width="500" title="NYC311 Awareness Strategy" border="1">
</p>


<p align="center">
  <a href="https://app.powerbi.com/view?r=eyJrIjoiOWU4NDZjNjAtNDM2NS00OWM4LTlmNTgtNjZiOGMzNDU5NmQxIiwidCI6IjQyNzhhNDAyLTFhOWUtNGViOS04NDE0LWZmYjU1YTVmY2YxZSIsImMiOjN9" target=_blank>Power Bi Dashboard</a>  
</p>

<p align="center">
  <img src="https://github.com/hhlamba/NYC311-Issues-in-the-Borough/blob/main/Output/Power%20Bi.png" width="500" title="NYC311 Awareness Strategy" border="1">
</p>

