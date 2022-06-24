@LAZYGLOBAL OFF.

//Launch Settings

GLOBAL vesselfilename is "Shuttle_RS_25D".     //this is the name of the vessel file to load


//for Vandenberg launches
//GLOBAL target_orbit IS LEXICON (	
//								"periapsis",130,
//								"apoapsis",160,
//								"inclination",-104,
//								"Cutoff Altitude",145,
//								//"Longitude of Periapsis",-120		//if circular orbit will be set to arbitrary value
//																	// set it 2 or 3 degrees to the EAST of its intended long because of the earth's rotation.
//								"end",0								//don't remove this
//).

GLOBAL target_orbit IS LEXICON (	
								"periapsis",30,
								"apoapsis",160,
								"inclination",-104,
								"Cutoff Altitude",120,
								//"Longitude of Periapsis",-120		//if circular orbit will be set to arbitrary value
																	// set it 2 or 3 degrees to the EAST of its intended long because of the earth's rotation.
								"end",0								//don't remove this
).


// uncomment this line to trigger automatically an engine failure. Alternatively shutdown manually one of the engines 
//GLOBAL engine_failure_time IS 150.


GLOBAL TAL_site is "Mataveri".


GLOBAL logdata Is false.


CD("0:/UPFG_OPS1").
run ops1_launch.