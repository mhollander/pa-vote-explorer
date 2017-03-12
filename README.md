PA Vote Explorer
======================================================
This project explores Pennsylvania's election data to find state reps who may be vulnerable to challenge.  There are obviously many measures of what constitutes a __vulnerable__ elected official.  This project assumes that a representative is vulnerable if the presidential candidate from the party opposite the state rep won the election in 2016 in the state rep's district.  For example, a candidate from Senatorial District X would be vulnerable if that candidate is from party A and the presidential candidate from party B won the election in District X.

The map presents two ways of measuring how vulnerable a candidate is.  A vulnerable candidate can be seen as more vulnerable if she only won her district by a small margin.  Alternatively, a vunerable candidate can be seen as more vulnerable if the presdiential candidate from the opposite party won by a large margin.  The following candidates would all be seen as vulnerable for various reasons:
| State Rep Party | Win Margin | Presidential Winner Party | Win Margin
| --------------- | ---------- | ------------------------- | ---------- |
| R | 2% | D | 2% |
| R | 90% | D | 50% |
| D | 2% | R | 50% |
| D | 90% | R | 2% |

The application is written in R/Shiny.

The vote data comes from the Open Elections Project's [PA specific data](https://github.com/openelections/openelections-data-pa).  The project uses both the 2016 data and the 2014 data (since only half of state senators are voted on every 2 years).

The shapefiles come from the [official Pennsylvania redistricting website](http://www.redistricting.state.pa.us/maps/).