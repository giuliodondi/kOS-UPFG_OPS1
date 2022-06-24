
GLOBAL vehicle IS LEXICON(
					"name","Space Shuttle - RS25D",
					"SRB_time",120,
					"SSME",LEXICON(
							"isp",453,
							"thrust",2319.9,
							"flow",522.2162,
							"minThrottle",0.65
						)
).
GLOBAL events IS LIST(
					LEXICON("time",1,"type", "action","action",{TOGGLE AG1.}),	//activates fuel cells
					LEXICON("time",262,"type", "action","action",{LOCAL englist IS SHIP:PARTSDUBBED("ShuttleSSME"). englist[ROUND(RANDOM(),0)]:SHUTDOWN.}),
					LEXICON("time",350,"type", "roll","angle",0)
).

					






