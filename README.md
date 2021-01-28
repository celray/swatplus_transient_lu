# SWAT+ Transient Land Use (SWAT+ TLU)

This is the repo for developing land use update alternatives. 
We have implemented a way to change HRU areas using the hru.con file

# How it works

The code looks for hru_yyyy.con (where 'yyyy' is the 'current year') before executing the current year. If it finds the .con file, it reads that updates HRU areas.
