GLOBAL g0 IS 9.80665. 


GLOBAL vehiclestate IS LEXICON(
	"cur_stg", 1,
	"staging_time", 0,
	"staging_in_progress", FALSE,
	"m_burn_left", 0,
	"avg_thr", average_value_factory(6)
).


//VEHICLE INITIALISATION FUNCTION 


function initialise_shuttle {
	CLEARSCREEN.

	RUNPATH("0:/Libraries/resources_library").	

	
	FUNCTION add_resource {
		parameter lexx.
		parameter reslist.
		
		
		for res in reslist {
			IF NOT lexx:HASKEY(res) {lexx:ADD(res,0).}
		}
		RETURN lexx.
	}

	


	
	//hard-coded initialisation of shuttle vehicle
	
	
	//not necessary per se but a useful check to see if we're flying a DECQ shuttle
	LOCAL ssme_count IS SHIP:PARTSDUBBED("ShuttleSSME"):LENGTH.
	IF (ssme_count<>3) {
		PRINT ("ERROR! THE VEHICLE SEEMS TO HAVE THE WRONG NUMBER OF SSMEs") AT (1,40).
		LOCAL X IS 1/0.
	}
	
	vehicle["SSME"]:ADD(
		"active",ssme_count
	).
	
	//	In case user accidentally entered throttle as percentage instead of a fraction
	IF vehicle["SSME"]["minThrottle"] > 1.0	{ SET vehicle["SSME"]["minThrottle"] TO vehicle["SSME"]["minThrottle"]/100. }
	
	local veh_res IS res_dens_init(
		add_resource(
			LEXICON(),
			LIST("LqdHydrogen","LqdOxygen")
		)
	).
	
	
	//measure total mass less the SRBs and clamps
	
	LOCAL et_part IS get_ext_tank_part().
	
	
	LOCAL stack_mass IS 0.
	FOR p IN getShuttleParts() {
		set stack_mass to stack_mass + p:mass*1000.
	}
	
	LOCAL total_prop_mass IS get_prop_mass(
		LEXICON(
			"resources",veh_res,
			"ext_tank",et_part
		)
	).
	
	LOCAL stack_empty_mass IS stack_mass - total_prop_mass.	
	
	
	//prepare stages list
	
	
	vehicle:ADD("stages",LIST()).
	
	//zeroth stage 
	vehicle["stages"]:ADD(0).
	
	//stage1 - SRB
	
	LOCAL stage1_burned_mass IS vehicle["SRB_time"] * ssme_count * vehicle["SSME"]["flow"].
	
	LOCAL stage2InitialMass IS stack_mass - stage1_burned_mass.
	
	
	LOCAL new_stg_1 IS LEXICON(
		"m_initial",	stack_mass,
		"m_final",	stage2InitialMass,
		"m_burn",	stage1_burned_mass,
		"staging", LEXICON (
			"type","time",
			"ignition",	TRUE
		),
		"ign_t", 0,
		"Tstage",vehicle["SRB_time"],
		"Throttle",1,
		"minThrottle",vehicle["SSME"]["minThrottle"],
		"engines",	
			LEXICON(
				"thrust", ssme_count*vehicle["SSME"]["thrust"]*1000, 
				"isp", vehicle["SSME"]["isp"], 
				"flow",ssme_count*vehicle["SSME"]["flow"], 
				"resources",LIST("LqdHydrogen","LqdOxygen")
		),
		"ext_tank",et_part,
		"resources",veh_res,
		"mode", 1
	).

	vehicle["stages"]:ADD(new_stg_1).
	
	
	//stage 2 - SSME CONSTANT T
	
	LOCAL new_stg_2 IS LEXICON(
		"m_initial",	stage2InitialMass,
		"m_final",	stack_empty_mass,
		"m_burn",	stage2InitialMass - stack_empty_mass,
		"staging", LEXICON (
			"type","glim",
			"ignition",	FALSE
		),
		"glim", 3,
		"ign_t", 0,
		"Tstage",0,
		"Throttle",1,
		"minThrottle",vehicle["SSME"]["minThrottle"],
		"engines", new_stg_1["engines"],
		"ext_tank",et_part,
		"resources",veh_res,
		"mode", 1
	).
	
	//will the stage exceed the g limit?
	LOCAL x IS glim_t_m(new_stg_2).
	If x[0] <= 0 {
		PRINT ("ERROR! THE VEHICLE WILL NEVER EXCEED THE 3G ACCELERATION LIMIT. VERIFY PAYLOAD MASS WITHIN LIMITS") AT (1,40).
		LOCAL X IS 1/0.
	}
	
	LOCAL stage3InitialMass IS x[1].
	SET new_stg_2["Tstage"] TO x[0].
	SET new_stg_2["m_final"] TO stage3InitialMass.
	SET new_stg_2["m_burn"] TO new_stg_2["m_initial"] - stage3InitialMass.
	
	
	vehicle["stages"]:ADD(new_stg_2).
	
	//stage 3 - SSME CONSTANT G until depletion or violation
	
	LOCAL new_stg_3 IS LEXICON(
		"m_initial",	stage3InitialMass,
		"m_final",	stack_empty_mass,
		"m_burn", stage3InitialMass - stack_empty_mass,
		"staging", LEXICON (
					"type","minthrot",
					"ignition",	FALSE
		),
		"glim",new_stg_2["glim"],
		"ign_t", 0,
		"Tstage",0,
		"Throttle",1,
		"minThrottle",new_stg_2["minThrottle"],
		"throt_mult",0,
		"engines",	new_stg_2["engines"],
		"ext_tank",et_part,
		"resources",veh_res,
		"mode", 2
	).
	
	SET new_stg_3["throt_mult"] TO new_stg_3["glim"]*g0/new_stg_3["engines"]["thrust"].
	
	LOCAL y IS const_G_t_m(new_stg_3).
	SET new_stg_3["Tstage"] TO y[0].
	LOCAL stage4InitialMass IS y[1].
	
	If stage4InitialMass <= 0 {
		//no fourth stage to be added
		SET new_stg_3["staging"]["type"] TO "depletion".
	} ELSE {
		SET new_stg_3["m_final"] TO stage4InitialMass.
		SET new_stg_3["m_burn"] TO new_stg_3["m_initial"] - stage4InitialMass.
	}
	
	vehicle["stages"]:ADD(new_stg_3).
	
	
	IF (stage4InitialMass>0) {
	
		//stage 4 - SSME CONSTANT T at minimum throttle
	
		LOCAL new_stg_4 IS LEXICON(
			"m_initial",	stage4InitialMass,
			"m_final",	stack_empty_mass,
			"m_burn",	stage4InitialMass - stack_empty_mass,
			"staging", LEXICON (
				"type","depletion",
				"ignition",	FALSE
			),
			"ign_t", 0,
			"Tstage",0,
			"Throttle",vehicle["SSME"]["minThrottle"],
			"engines", new_stg_1["engines"],
			"tankparts",et_part,
			"resources",veh_res,
			"mode", 1
		).
		
		SET new_stg_4["Tstage"] TO new_stg_4["m_burn"]/(new_stg_4["engines"]["flow"] * new_stg_4["Throttle"]).
	
		vehicle["stages"]:ADD(new_stg_4).
	} 



	//final vehicle parameters
	
	vehicle:ADD("ign_t", 0).
	vehicle:ADD("launchTimeAdvance", 300).
	vehicle:ADD("roll",180).
	vehicle:ADD("preburn",5.1).
	vehicle:ADD(
		"handover",
		LEXICON("time", 1000)
	).
	vehicle:REMOVE("Ext_Tank_Part").
	vehicle:REMOVE("SRB_time").

	
	WHEN (SHIP:Q > 0.28) THEN {
		addMessage("THROTTLING DOWN").
		SET vehicle["stages"][1]["Throttle"] TO 0.75.
	}
	
	PRINT " INITIALISATION COMPLETE" AT (0,3).
	
}

FUNCTION debug_vehicle {
	IF EXISTS("0:/vehicledump.txt") {
		DELETEPATH("0:/vehicledump.txt").
	}
	
	log vehicle:dump() to "0:/vehicledump.txt".
	
	until false{
		wait 0.1.
	}
}



//returns the list of parts making up the Orbiter and External Tank
function getShuttleParts {

	function removeChildrenPartsRecursively {
		parameter partslist.
		parameter part.
		
		local partchildren is part:children:copy.
		
		if partchildren:length > 0 {
			for p in partchildren {
			
				removeChildrenPartsRecursively(
					partslist,
					p 
				).
			
			}
		}
		
		partslist:remove(partslist:find(part)).
		return.

	}
	
	
	local et is ship:partsdubbed("ShuttleExtTank")[0].

	local shuttleParts is ship:parts:copy.

	removeChildrenPartsRecursively(
		shuttleParts,
		et
	).

	shuttleParts:add(et).
	
	return shuttleParts.
}








									//CONTROL FUNCTIONS



//open-loop pitch profile for first stage ascent
FUNCTION pitch {
	PARAMETER v.
	PARAMETER v0.
	PARAMETER scale.			 
	
	LOCAL default IS 90.

	LOCAL out IS default.
	
	IF v>v0 {
		
		LOCAL p1 IS -0.0048.
		LOCAL p2 IS 28.8.
		LOCAL p3 IS 26300.
		LOCAL q1 IS 3.923.
		
		LOCAL x IS v + 400.391 - v0.
	
		SET out TO (p1*x^2 + p2*x + p3)/(x + q1).
		
		SET out TO out*(1 + scale*(1 - out/default)).
		
		LOCAL bias IS out - surfacestate["vdir"].
		
		SET out TO out + 0.8*bias.
		
		
	}

	RETURN CLAMP(out,0,default).

}



//compute net thrust vector as thrust-weighted average of engines position 
//relative to the ship raw frame
//obtain the difference between fore vector and thrust vector.
//then, given input reference "fore" and "up" vectors, rotate in that frame
FUNCTION thrustrot {

	PARAMETER ref_fore.
	PARAMETER ref_up.
	
	local norm is VCRS(ref_fore,ref_up).

	LOCAL thrvec IS v(0,0,0).
	local offs is v(0,0,0).
	LOCAL thr is 0.
	
	list ENGINES in all_eng.
	FOR e IN all_eng {
		IF e:ISTYPE("engine") {
			IF e:IGNITION {
				SET thr TO thr + (e:THRUST).
				//set x to x + 1.
				local v is -e:POSITION:NORMALIZED*e:THRUST.
				set offs to offs + v.
			}
		}
	}	
	set thrvec to (offs/thr):NORMALIZED .
	local ship_fore IS SHIP:FACING:VECTOR:NORMALIZED.
	
	LOCAL newthrvec IS rodrigues(ref_fore,norm,-VANG(ship_fore,thrvec)):NORMALIZED*thrvec:MAG.
	
	RETURN ref_fore - newthrvec.
}




//	Returns a kOS direction for given aim vector, reference up vector and roll angle.
//corrects for thrust offset
FUNCTION aimAndRoll {
	DECLARE PARAMETER aimVec.	//	Expects a vector
	DECLARE PARAMETER upVec.	//	Expects a vector
	DECLARE PARAMETER rollAng.	//	Expects a scalar
			 
	LOCAL steerVec IS aimVec.
	
	LOCAL topVec IS VXCL(steerVec,upVec):NORMALIZED.
	SET topVec TO rodrigues(topVec, steerVec, rollAng).
	
	//if the target aiming vector is too far away in yaw calculate an intermediate vector
	//project the target vector in the ship vertical plane, rotate this towards the target by 3 degrees in the yaw plane
	//LOCAL thrustvec IS thrust_vec().
	//IF VANG(thrustvec,steerVec) > 5 {
	//	LOCAL steerVecproj IS VXCL(SHIP:FACING:STARVECTOR,steerVec).
	//	
	//	LOCAL aimnorm Is VCRS(steerVec, steerVecproj).
	//	
	//	SET steerVec TO rodrigues(steerVecproj,aimnorm,-5).
	//
	//}
	
	LOCAL thrustCorr IS thrustrot(steerVec,topVec).
	
	LOCAL outdir IS LOOKDIRUP(steerVec + thrustCorr, topVec).


	//clearvecdraws().
	//arrow(topVec,"topVec",v(0,0,0),30,0.05).
	//arrow(aimVec,"aimVec",v(0,0,0),30,0.05).
	//arrow(steerVec,"steerVec",v(0,0,0),30,0.05).
	//arrow(thrustCorr,"thrustCorr",v(0,0,0),30,0.05).

	RETURN outdir.
}

//given current vehicle fore vector, computes where the thrust is pointing
FUNCTION thrust_vec {
	RETURN SHIP:FACING:VECTOR:NORMALIZED - thrustrot(SHIP:FACING:FOREVECTOR,SHIP:FACING:TOPVECTOR).
}




//	Throttle controller
FUNCTION throttleControl {

	local stg IS get_stage().
	local throtval is stg["Throttle"].
	
	LOCAL minthrot IS 0.
	IF stg:HASKEY("minThrottle") {
		SET minthrot TO stg["minThrottle"].
	}
	
	IF stg["mode"] = 2   {
		SET throtval TO stg["throt_mult"]*SHIP:MASS*1000.
		SET stg["Throttle"] TO throtval.
		SET usc["lastthrot"] TO throtval.
	}
	
	SET throtval TO MIN(1,(throtval - minthrot)/(1 - minthrot)).			
	
	RETURN MAX(0.005,throtval).
}






									//VEHICLE PERFORMANCE & STAGING FUNCTIONS


FUNCTION get_stage {
	RETURN vehicle["stages"][vehiclestate["cur_stg"]].
}



FUNCTION events_handler {

	local met is TIME:SECONDS - vehicle["ign_t"].

	local x IS events:LENGTH.
	
	local rem_list IS LIST().

	//FROM {LOCAL k IS 0.} UNTIL k >= x STEP { SET k TO k+1.} DO{
	
	LOCAL k IS 0.
	FOR evt IN events {
		
		IF met>evt["time"]  {
			IF evt["type"]="jettison" {
				TOGGLE AG8.
				IF evt:HASKEY("mass") {
					FROM { LOCAL i IS vehiclestate["cur_stg"]. } UNTIL i > (vehicle["stages"]:LENGTH - 1)  STEP { SET i TO i+1. } DO {
						SET vehicle["stages"][i]["m_initial"] TO vehicle["stages"][i]["m_initial"] - evt["mass"].
						SET vehicle["stages"][i]["m_final"] TO vehicle["stages"][i]["m_final"] - evt["mass"].
					}
				}
				rem_list:ADD(k).
			}
			ELSE IF evt["type"]="roll" {
								
				IF ABS(evt["angle"] - vehicle["roll"])<5 {
					set vehicle["roll"] TO evt["angle"].
					rem_list:ADD(k).
					SET STEERINGMANAGER:MAXSTOPPINGTIME TO 0.2.
					
				} ELSE {
					SET STEERINGMANAGER:MAXSTOPPINGTIME TO 0.8.
					local rollsign is SIGN( unfixangle( evt["angle"] - vehicle["roll"] ) ).
					set vehicle["roll"] TO fixangle(vehicle["roll"] + rollsign*5).
				} 
			}
			ELSE IF evt["type"]="action" { 
				IF evt:HASKEY("action") {
					evt["action"]:call().
				}
				rem_list:ADD(k).
			}
				
			
			
		}
		SET k TO k+1.
	}
	
	LOCAL items_removed IS 0.
	FOR e IN rem_list {
		events:REMOVE(e - items_removed).
		SET items_removed TO items_removed + 1.
	}
}




//calculates when the g limit will be violated and the vehicle mass at that moment
//returns (0,0) if the g-lim is never reached
FUNCTION glim_t_m {
	PARAMETER stg.
	local out is LIST(0,0).
	
	local mbreak is stg["engines"]["thrust"]/(stg["glim"]*g0).
	IF mbreak > stg["m_final"]  {
		SET out[1] TO mbreak.
		SET out[0] TO (stg["m_initial"] - mbreak)/(stg["engines"]["flow"] * stg["Throttle"]).
	}
	RETURN out.
}


//given a constant g stage calculates the burn time until the lower throttle limit will be reached and the vehicle mass at that moment
FUNCTION const_G_t_m {
	PARAMETER stg.
	local out is LIST(0,0).

	local red_isp is stg["engines"]["isp"]/stg["glim"].
	
	//compute burn time until  we deplete the stage.	
	
	LOCAL maxtime IS red_isp * LN(1 + stg["m_burn"]/stg["m_final"] ).
	
	//compute burn time until  we reach minimum throttle.	
	LOCAL limtime IS - red_isp * LN(stg["minThrottle"]).

	//calculate mass of the fuel burned until violation
	LOCAL mviol IS stg["m_initial"]*CONSTANT:E^(- limtime/red_isp).
	
	IF mviol > stg["m_final"]  {
		SET out[1] TO mviol.
		SET out[0] TO limtime.
	} ELSE {
		SET out[0] TO maxtime.
	}
	
		
	RETURN out.
}


//measures current total engine thrust an isp, as well as theoretical max engine thrust at this altitude

FUNCTION get_thrust_isp {

	LOCAL thr is 0.
	LOCAL iisspp IS 0.
	LOCAL maxthr is 0.			   
	
	list ENGINES in all_eng.
	FOR e IN all_eng {
		IF e:ISTYPE("engine") {
			IF e:IGNITION {
				SET thr TO thr + (e:THRUST * 1000).
				SET iisspp TO iisspp + e:isp*(e:THRUST * 1000).
				SET maxthr TO maxthr + (e:AVAILABLETHRUST*1000).									
			}
		}
	}	
	
	vehiclestate["avg_thr"]:update(thr).
	
	RETURN LIST(vehiclestate["avg_thr"]:average(),iisspp,maxthr).
}



FUNCTION get_TWR {
	RETURN vehiclestate["avg_thr"]:average()/(1000*SHIP:MASS*g0).
}



FUNCTION get_prop_mass {
	PARAMETER stg.
	
	local reslist is stg["resources"].
	local prop_mass IS 0.
	
	FOR tkres In stg["ext_tank"]:RESOURCES {
		FOR res IN reslist:KEYS {
			IF tkres:NAME = res {
				set prop_mass TO prop_mass + tkres:amount*reslist[res].
			}
	
		}
	}
	set prop_mass to prop_mass*1000.
    RETURN prop_mass.
}


//measures everything about the current state of the vehicle, including instantaneous thrust
//thrust only averaged over if staging is not in progress
FUNCTION getState {
	
	LOCAL deltat IS surfacestate["MET"].
	SET surfacestate["MET"] TO TIME:SECONDS. 
	SET deltat TO surfacestate["MET"] - deltat.

	//measure position and orbit parameters
	
	IF ops_mode >1 {set v to SHIP:PROGRADE:VECTOR.}
	ELSE {set v to SHIP:SRFPROGRADE:VECTOR.}
	
	SET surfacestate["hdir"] TO compass_for(v,SHIP:GEOPOSITION ).
	SET surfacestate["vdir"] TO 90 - VANG(v, SHIP:UP:VECTOR).
	SET surfacestate["pitch"] TO 90 - VANG(SHIP:FACING:VECTOR, SHIP:UP:VECTOR).	
	SET surfacestate["az"] TO compass_for(SHIP:FACING:VECTOR,SHIP:GEOPOSITION ).
	SET surfacestate["alt"] TO SHIP:ALTITUDE.
	SET surfacestate["vs"] TO SHIP:VERTICALSPEED.
	SET surfacestate["hs"] TO SHIP:VELOCITY:SURFACE:MAG.
	
	SET orbitstate["velocity"] TO vecYZ(SHIP:ORBIT:VELOCITY:ORBIT).
	SET orbitstate["radius"] TO vecYZ(SHIP:ORBIT:BODY:POSITION)*-1.
	
	IF DEFINED events {	events_handler().}
	
	//measure and compute vehicle performance parameters
	
	LOCAL cur_stg IS get_stage().
	
	LOCAL x IS get_thrust_isp().
	LOCAL avg_thrust is x[0].
	LOCAL avg_isp is x[1].

	IF NOT (vehiclestate["staging_in_progress"]) {
		
		LOCAL current_m IS SHIP:MASS*1000.
		local res_left IS get_prop_mass(cur_stg).
		
		
		IF (vehiclestate["cur_stg"]=1) {
		
			//do it here so we bypass the check during later stages
			IF (surfacestate:HASKEY("q") AND surfacestate["alt"] > 100 ) {
				check_maxq(SHIP:Q).
			}
		
			SET cur_stg["Tstage"] TO cur_stg["Tstage"] - deltat.
			
		} ELSE IF (vehiclestate["cur_stg"]=2) {
		
			update_stage2(current_m, res_left).
	
		} ELSE IF (vehiclestate["cur_stg"]=3) {
			
			update_stage3(current_m, res_left).
		
		} ELSE IF (vehiclestate["cur_stg"]=4) {
			
			update_stage4(current_m, res_left).
		}
	
	}
}

FUNCTION update_stage2 {
	PARAMETER m_initial.
	PARAMETER res_left.
	PARAMETER stg2 IS vehicle["stages"][2].

	SET stg2["m_initial"] TO m_initial.
	SET stg2["m_burn"] TO res_left.
	SET stg2["m_final"] TO m_initial - res_left.
	
	IF (stg2["staging"]["type"]="depletion") {
		
		LOCAL red_flow IS stg2["engines"]["thrust"] * stg2["throttle"]/(stg2["engines"]["isp"]*g0).
		SET stg2["Tstage"] TO stg2["m_burn"]/red_flow.	
		
	} ELSE IF (stg2["staging"]["type"]="glim") {
		//assume the g limit will be exceeded 
		
		LOCAL x IS glim_t_m(stg2).
		
		SET stg2["Tstage"] TO x[0]. 
		SET stg2["m_final"] TO x[1]. 
		SET stg2["m_burn"] TO m_initial - x[1].
		
		LOCAL stg3_m_initial IS x[1].
		LOCAL stg3_m_burn IS res_left - stg2["m_burn"].
		
		update_stage3(stg3_m_initial, stg3_m_burn).
	}

}



FUNCTION update_stage3 {
	PARAMETER m_initial.
	PARAMETER res_left.
	PARAMETER stg3 IS vehicle["stages"][3].
	
	SET stg3["m_initial"] TO m_initial.
	SET stg3["m_burn"] TO res_left.
	SET stg3["m_final"] TO m_initial - res_left.
	
	IF stg["mode"]=1 {
		//constant thrust depletion stage, only used for late ATO aborts
		
		LOCAL red_flow IS stg3["engines"]["thrust"] * stg3["throttle"]/(stg3["engines"]["isp"]*g0).
		SET stg3["Tstage"] TO stg3["m_burn"]/red_flow.
	
	} ELSE IF stg["mode"]=2 {
	
		LOCAL x IS const_G_t_m(stg3).
		SET stg3["Tstage"] TO x[0].

		IF (stg3["staging"]["type"]="minthrot") {
			//there is a fourth stage, assume there will be a violation of minimum throttle 
			
			SET stg3["m_final"] TO x[1].
			LOCAL stg4_m_initial IS x[1].
			
			SET stg3["m_burn"] TO m_initial - x[1].
			LOCAL stg4_m_burn IS res_left - stg3["m_burn"].

			update_stage4(stg4_m_initial, stg4_m_burn).
			
		}
	
	}
}


FUNCTION update_stage4 {
	PARAMETER m_initial.
	PARAMETER res_left.
	PARAMETER stg4 IS vehicle["stages"][4].
	
	//assume it's a depletion stage
	
	
	SET stg4["m_initial"] TO m_initial.
	SET stg4["m_burn"] TO res_left.
	SET stg4["m_final"] TO m_initial - res_left.
	
	LOCAL red_flow IS stg4["engines"]["thrust"] * stg4["throttle"]/(stg4["engines"]["isp"]*g0).
	SET stg4["Tstage"] TO stg4["m_burn"]/red_flow.
	
}



FUNCTION increment_stage {
	
	SET vehiclestate["staging_time"] TO TIME:SECONDS.
	
	LOCAL j IS vehiclestate["cur_stg"].
	
	SET vehiclestate["cur_stg"] TO j + 1.
			
	SET vehicle["stages"][j] TO 0.
	
	SET vehicle["stages"][j+1]["ign_t"] TO vehiclestate["staging_time"].
	
	IF ops_mode=2 {
		SET usc["lastthrot"] TO stg["Throttle"].
	}
	
	vehiclestate["avg_thr"]:reset().
	
	WHEN TIME:SECONDS > vehiclestate["staging_time"] + 0.5 THEN {
		addMessage("STAGING SEQUENCE COMPLETE").
		SET vehiclestate["staging_in_progress"] TO FALSE.
	}

}


FUNCTION srb_staging {
	IF vehiclestate["staging_in_progress"] {RETURN.}

	IF (vehicle["stages"][vehiclestate["cur_stg"]]["Tstage"] <= 3 ) {
		SET vehiclestate["staging_in_progress"] TO TRUE.
		SET control["steerdir"] TO "kill".
		addMessage("STAND-BY FOR SRB SEP").
		
		
		WHEN (get_TWR()<1) THEN {
		
			wait until stage:ready.
			STAGE.
		
			increment_stage().
			
			SET vehicle["handover"]["time"] TO vehiclestate["staging_time"] - vehicle["ign_t"] + 5.
		}
	}
	

}

FUNCTION ssme_staging {
	IF vehiclestate["staging_in_progress"] {RETURN.}
	
	IF 

	IF (vehicle["stages"][vehiclestate["cur_stg"]]["Tstage"] <= 3) {
		SET vehiclestate["staging_in_progress"] TO TRUE.
		addMessage("STAND-BY FOR STAGING").
		
		WHEN (vehicle["stages"][vehiclestate["cur_stg"]]["Tstage"] <=0.0005) THEN {
			increment_stage().
		}
	}

}


FUNCTION is_flameout_imminent {

	LOCAL j IS vehiclestate["cur_stg"].
	
	RETURN ( (vehicle["stages"][j]["Tstage"] <= 3) AND (j = vehicle["stages"]:LENGTH) ). 

}



//simple function to check if vehicle is past maxq
FUNCTION check_maxq {
	PARAMETER newq.
	
	IF (newq >=  surfacestate["q"] ) {
		SET surfacestate["q"] TO newq.
	} ELSE {
		addMessage("VEHICLE HAS REACHED MAX-Q").
		surfacestate:REMOVE("q").
		WHEN (SHIP:Q < 0.95*newq) THEN {
			addMessage("THROTTLING UP").
			SET vehicle["stages"][1]["Throttle"] TO 1.
		}
	}

}






//		SHUTTLE-SPECIFIC FUNCTIONS 





//return the ET part to read fuel quantities
FUNCTION get_ext_tank_part {
	RETURN SHIP:PARTSDUBBED(vehicle["Ext_Tank_Part"])[0].
}




FUNCTION disable_TVC {
	FOR ssme IN SHIP:PARTSDUBBED("ShuttleSSME") {
		IF ssme:ISTYPE("engine") {
			ssme:GIMBAL:DOACTION("lock gimbal", TRUE).
		}
	}
}



//close umbilical doors
FUNCTION close_umbilical {
	SHIP:PARTSDUBBED("ShuttleEngMount")[0]:GETMODULE("ModuleAnimateGeneric"):doaction("toggle et door", true).
}




FUNCTION OMS_dump {
	PARAMETER type.
	PARAMETER state.
	
	IF (type = "oms") {
		FOR oms IN SHIP:PARTSDUBBED("ShuttleEngineOMS") {
			IF (state="start") {
				oms:ACTIVATE.
			} ELSE IF (state="stop") {
				oms:SHUTDOWN.
			}
		}
	}

}

//coudn active SSMEs and compare with expected number.
FUNCTION SSME_out {

	//get SSME parameters from vehicle struct snd known number of engines 
	LOCAL SSMEcount IS 0.
	
	FOR e IN SHIP:PARTSDUBBED("ShuttleSSME") {
		IF e:IGNITION {
			SET SSMEcount TO SSMEcount + 1.
		}
	}
	
	IF (SSMEcount < vehicle["SSME"]["active"]) {
		SET diff TO  vehicle["SSME"]["active"] - SSMEcount.
	
		FROM {LOCAL k IS 1.} UNTIL k > (vehicle["stages"]:LENGTH - 1) STEP { SET k TO k+1.} DO{
			SET vehicle["stages"][k]["engines"]["thrust"] TO SSMEcount*vehicle["SSME"]["thrust"]*1000.
			SET vehicle["stages"][k]["engines"]["flow"] TO SSMEcount*vehicle["SSME"]["flow"].
		}
		
		SET vehicle["SSME"]["active"] TO SSMEcount.
		
		RETURN TRUE.
	}

	RETURN FALSE.
}

FUNCTION SSME_flameout {
	FOR e IN SHIP:PARTSDUBBED("ShuttleSSME") {
		IF e:FLAMEOUT {
			RETURN true.
		}
	}
	RETURN FALSE.
}
