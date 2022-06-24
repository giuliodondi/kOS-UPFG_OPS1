@LAZYGLOBAL OFF.

//Launch Settings

GLOBAL vesselfilename is "Shuttle_RS_25D".     //this is the name of the vessel file to load


GLOBAL target_orbit IS LEXICON (	
								"periapsis",30,
								"apoapsis",220,
								"inclination",52,
								"Cutoff Altitude",112,
								"end",0								//don't remove this
).


// uncomment this line to trigger automatically an engine failure. Alternatively shutdown manually one of the engines 
//GLOBAL engine_failure_time IS 150.


//change this to the best suitable site based on launch inclination
GLOBAL TAL_site is "Zaragoza".
//GLOBAL TAL_site is "Nearest".


GLOBAL logdata Is false.


CD("0:/UPFG_OPS1").
run ops1_launch.
