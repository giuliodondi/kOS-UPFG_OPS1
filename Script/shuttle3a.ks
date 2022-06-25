@LAZYGLOBAL OFF.


//Launch Settings

//for Vandenberg launches
//GLOBAL target_orbit IS LEXICON (	
//								"periapsis",130,
//								"apoapsis",160,
//								"inclination",-104,
//								"Cutoff Altitude",145,
//								"end",0	
//).

GLOBAL target_orbit IS LEXICON (	
								"periapsis",30,
								"apoapsis",160,
								"inclination",-104,
								"Cutoff Altitude",120,
								"end",0								//don't remove this
).


// uncomment this line to trigger automatically an engine failure. Alternatively shutdown manually one of the engines 
//GLOBAL engine_failure_time IS 150.


//change this to the best suitable site based on launch inclination
GLOBAL TAL_site is "Mataveri".


GLOBAL logdata Is false.


RUNPATH("0:/UPFG_OPS1/ops1_launch").
