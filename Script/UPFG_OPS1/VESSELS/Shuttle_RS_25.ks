
GLOBAL vehicle IS LEXICON(
					"name","Space Shuttle - RS25",
					"SRB_time",120,
					"SSME",LEXICON(
							"isp",452,
							"thrust",2090,
							"flow",471.48,
							"minThrottle",0.67
						)
).
GLOBAL events IS LIST(
					LEXICON("time",1,"type", "action","action",{TOGGLE AG1.})	//activates fuel cells
).

					






