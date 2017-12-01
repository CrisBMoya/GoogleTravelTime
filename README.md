# GoogleTravelTime
Calculates driving travel time and plot polylines using a coordinate matrix of multiple points.

The idea behind this script is to answer the question: to which places can I get in X minutes? or which places are at X minutes from a central point?
Basically it takes advantage of Google API inside R, using a Shiny environment in order to calculate houndreds of routes from an specificn central point.

You provide a start or origin point, then select the number of points you want to compute (which are transformed into a matrix of coordinates) and then the Google API calculates how much time it takes to drive to every single of those points. Then, using Leaflet a plot is created with the polylines of every route calculated before. Each polyline color represent the time that it takes to get from point A to point B, all of them separated in different Layers inside the map.

Currently, this App is hosted at: https://cbmapp.shinyapps.io/ShinyTravelTime/.

A few changes has been made:
-Now you can choose the start point everywhere on the world by giving the correct pair of Latitude and Longitude values.
-The matrix size can be plotted as a rectangle into the map and you can change it's size. The specific points inside the matrix arend displayed due to the heavy loading that implies plotting a big number of markers into the map.

Future changes:
-Plot the matrix of points, possibly using dots, not markers.
-Add a feautre to download the map results whether in PDF or HTML format.
-Improve the way the user choose the origint point, and possibly the zooming.

