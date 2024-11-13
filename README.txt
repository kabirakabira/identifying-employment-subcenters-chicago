Hello and thank you for downloading our code package on identifying employment subcenters in the Chicago Metro Area.

To run this code on your local system, please follow the instructions below.

Option 1:
Create a new R project (a .Rproj file) in the same directory as the downloaded files. Run the scripts through this R project.

Option 2:
Simply set your working directory to the same directory as the downloaded files.

Essentially, your working directory should have the following file structure:

* working directory
|
*-------scripts
	|
	*------00 Directory Setup and Data Downloads.R
	|
	*------01 Master File.R
	|
	*------Commuting Flows.R
	|
	*------Density Peaks.R
	|
	*------Double Thresholds.R
	|
	*------Positive Residuals.R
	|
	*------Spatial Autocorrelation.R
|
*-------Rproj file (OPTIONAL - see options above)
|
*-------README.txt
|
*-------Miscellaneous files (other git files, etc)

The only script you need to run is the "01 Master File.R". It will run all the other scripts.

Note that (through running the Master File and therefore the 00 Setup file), a new folder will be created
in your working directory called "data". Within this folder, the subfolder "output" will create all the produced shapefiles
and observation tables.

------ NOTE ON REPLICATING THIS FOR OTHER METRO AREAS -------
To adjust this code to work for other metro areas, the only script that will require editing is the "00 Directory Setup and Data Downloads.R" file.
This file downloads the shapefile for Census Tracts in the study area as well as the LEHD data on Origin-Destination and Workplace Characteristics.
The specified downloads will have to be edited to comply with the new region. However, once that has been achieved, the entire project should run by
simply running the "01 Master File.R" script again.

Note that doing a complete re-run may require you to manually delete the newly created "data" folder. In other words, please ensure that 
prior to re-running the Master file, your working directory looks exactly like the file structure pictured above.

Thank you!
- David, Matt, Kabir, Max, Nooshin, and Elly