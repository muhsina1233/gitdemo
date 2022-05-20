drop database if exists Flights;
create database if not exists Flights;
use Flights;
-- 1.Create a Table Flights with schemas of Table

create table if not exists flights(ID INT,YEAR INT,MONTH INT,DAY INT,DAY_OF_WEEK INT,
AIRLINE varchar(20),FLIGHT_NUMBER INT,TAIL_NUMBER varchar(20),ORIGIN_AIRPORT varchar(20),
DESTINATION_AIRPORT varchar(20),SCHEDULED_DEPARTURE INT,DEPARTURE_TIME INT,DEPARTURE_DELAY INT,
TAXI_OUT INT,WHEELS_OFF INT,SCHEDULED_TIME INT,ELAPSED_TIME INT,AIR_TIME INT,DISTANCE INT,
WHEELS_ON INT,TAXI_IN INT,SCHEDULED_ARRIVAL INT,ARRIVAL_TIME INT,ARRIVAL_DELAY INT,	
DIVERTED INT,CANCELLED INT,CANCELLATION_REASON TEXT,AIR_SYSTEM_DELAY INT,SECURITY_DELAY INT,
AIRLINE_DELAY INT,LATE_AIRCRAFT_DELAY INT,WEATHER_DELAY INT);

-- 2.Insert all records into flights table. Use dataset Flights_Delay.csv

SET GLOBAL local_infile = true;

LOAD DATA LOCAL INFILE 'D:/Flights_Delay.csv' INTO 
TABLE flights FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n' IGNORE 1 ROWS;

-- 3. Average Arrival delay caused by airlines 
select AIRLINE, avg(ARRIVAL_DELAY) as avg_delay from flights group by AIRLINE order by avg_delay ;

-- 4.Display the Day of Month with AVG Delay [Hint: Add Count() of Arrival & Departure Delay]
select MONTH, avg(DEPARTURE_DELAY+ARRIVAL_DELAY) as AVG_DELAY from flights group by MONTH order by MONTH;

-- 5.Analysis for each month with total number of cancellations.
select distinct(MONTH), count(CANCELLED) from flights group by MONTH;

-- 6.Find the airlines that make maximum number of cancellations
select AIRLINE, count(CANCELLED) as cancel from flights group by AIRLINE order by cancel desc;


-- 7.Finding the Busiest Airport [Hint: Find Count() of origin airport and destination airport]
select ORIGIN_AIRPORT ,  count(*) as Count_air from flights group by ORIGIN_AIRPORT order by Count_air DESC;
select DESTINATION_AIRPORT ,  count(*) as Count_air from flights group by DESTINATION_AIRPORT order by Count_air DESC;

-- 8.Find the airlines that make maximum number of Diversions [Hint: Diverted = 1 indicate Diversion]
select AIRLINE, sum(DIVERTED) as diverted from flights group by AIRLINE order by diverted desc;

-- 9.Finding all diverted Route from a source to destination Airport & which route is the most diverted route.
select ORIGIN_AIRPORT, DESTINATION_AIRPORT , count(DIVERTED)
from flights where DIVERTED=1 group by ORIGIN_AIRPORT, DESTINATION_AIRPORT;

-- 10.Finding all Route from origin to destination Airport & which route got delayed. 
select ORIGIN_AIRPORT , DESTINATION_AIRPORT, ARRIVAL_DELAY from flights order by ARRIVAL_DELAY desc;


-- 11.Finding the Route which Got Delayed the Most [Hint: Route include Origin Airport and Destination Airport, Group By Both ]
select ORIGIN_AIRPORT, DESTINATION_AIRPORT , sum(ARRIVAL_DELAY) as delay
from flights group by ORIGIN_AIRPORT, DESTINATION_AIRPORT order by delay desc;

-- 12.  Finding AIRLINES with its total flight count, total number of flights arrival delayed by more than 30 Minutes, % of such flights delayed by more than 30 minutes when it is not Weekends with minimum count of flights from Airlines by more than 10. Also Exclude some of Airlines 'AK', 'HI', 'PR', 'VI' and arrange output in descending order by % of such count of flights.
select AIRLINE, count(*) as tot from flights where AIR_TIME>30;
