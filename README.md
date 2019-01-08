# GoogleTravelTime

___
UPDATE:

As of 2019.01.07 Google has changed some of its functioning regarding API Keys. While this App still works you'll need to enter credit card information (on Google Site, of course) to get an API Key from Google.
___

Calculates driving travel time and plot polylines using a coordinate matrix of multiple points.

The idea behind this script is to answer the question: to which places can I get in X minutes? or which places are at X minutes from a central point?
Basically, it takes advantage of Google API inside R, using a Shiny environment in order to calculate hundreds of routes from a specific central point.

You provide a start or origin point, then select the number of points you want to compute (which are transformed into a matrix of coordinates) and then the Google API calculates how much time it takes to drive to every single of those points. Then, using Leaflet a plot is created with the polylines of every route calculated before. Each polyline color represents the time that it takes to get from point A to point B, all of them separated in different Layers inside the map.

Currently, this App is hosted at: https://cbmapp.shinyapps.io/ShinyTravelTime/.

A few changes have been made:
- Now you can choose the start point everywhere on the world by giving the correct pair of Latitude and Longitude values.
- The matrix size can be plotted as a rectangle into the map and you can change it's size.
- The points are plotted along with the matrix size.
- Improve the colors of routes.
- Added a Download button. It works dependent to reactives so pressing it runs the whole code. Beware with your API keys.

Future changes:
- Improve the way the user chooses the origin point, and possibly the zooming.

**DISCLAIMER**: If you came from r/MapPorn or https://cbmapp.shinyapps.io/RouteTimes/ you'll find these scripts doesn't work exactly like the examples exposed on the website. The script that do those maps are of my personal use only (since I use some tricks with google API's keys), but similar results can be achieved using the scripts in this repository. You can also translate the shiny app to normal code to improve waiting times and add functionality.
