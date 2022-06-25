@LAZYGLOBAL OFF.

//	Load libraries
RUNPATH("0:/Shuttle_entrysim/landing_sites").
RUNPATH("0:/Libraries/misc_library").	
RUNPATH("0:/Libraries/maths_library").	
RUNPATH("0:/Libraries/navigation_library").	
RUNPATH("0:/UPFG_OPS1/ops1_interface").
RUNPATH("0:/UPFG_OPS1/ops1_vehicle_library").
RUNPATH("0:/UPFG_OPS1/ops1_targeting_library").
RUNPATH("0:/UPFG_OPS1/ops1_upfg_library").
RUNPATH("0:/UPFG_OPS1/ops1_abort_library").


//Launch Settings

GLOBAL target_orbit IS LEXICON (	
								"periapsis",30,
								"apoapsis",220,
								"inclination",35,
								"Cutoff Altitude",112,
								"end",0								//don't remove this
).


// uncomment this line to trigger automatically an engine failure. Alternatively shutdown manually one of the engines 
//GLOBAL engine_failure_time IS 150.
//GLOBAL engine_failure_time IS 410.


//change this to the best suitable site based on launch inclination
GLOBAL TAL_site is "Zaragoza".


GLOBAL logdata Is false.


RUNPATH("0:/UPFG_OPS1/ops1_launch").
