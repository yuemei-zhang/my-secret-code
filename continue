%MACRO Num_Var_Binning
		(	Input_DSN=, 							 	/*input data set name*/
			Target =  , 										/*target binary variables*/
			InputVar_List = , 							/*input continuous variables list, separated by blanks*/			
			Max_Missing_Portion =0.1 , 			/* how big of the  portiton of missing obs out of total obs is allowed; 0~1*/
			Initial_Bin_Num = ,						/*Inital bin number, recommeded:100 or 50 or 20 or 16 or 8*/
			Inital_Bin_Method = , 					/* Equal Width(Bucket) or Equal Height(quantitle).  1: Equal Width; 2: Equal Height*/
			Max_Bin_Num = , 							/*the maximum number of bins are allowed to form, 10~15*/
			ChiSquare_Sign_Level = ,				/* signicance level for the independence chi-square test */
			Info_Value_Threshold =, 				/*  information threshould for variable selection */
			Output_DSN_Detail =,					/*Output dataset name, detailed info   */
			Output_DSN_Smry =,					/*Output dataset name, summary info  */
			Output_DSN_Sel_Detail=,
			Output_DSN_Sel_Smry =				/*Output dataset name, summary info */
		);
	
	*1.target variable check - missing value, boolean 0-1 ;
		PROC SQL NOPRINT;
				SELECT COUNT(*) INTO :MissTargetCnt FROM &Input_DSN WHERE &Target IS MISSING;
				SELECT MIN(&Target), MAX(&Target) INTO :MinTargetValue, :MaxTargetValue FROM &Input_DSN;
		QUIT;
		%LET MissTargetCnt = &MissTargetCnt;
		%LET MinTargetValue = &MinTargetValue;
		%LET MaxTargetValue = &MaxTargetValue;
		
		%IF &MissTargetCnt>0 OR &MinTargetValue NE 0 OR &MaxTargetValue NE 1 %THEN
			%DO;
					%PUT target variable(&Target) is not binary type or have missing values, please check;
					%RETURN;
			%END;
	
	*2.retrieve the input variable list one by one;
		%LET i =1;
		%DO %WHILE (%SCAN(&InputVar_List, &i) NE );
				%GLOBAL InputVar&i;
				%LET InputVar&i = %SCAN(&InputVar_List, &i);
				%LET i = %EVAL(&i+ 1);		
		%END;
		%GLOBAL InputVarCnt;
		%LET InputVarCnt = %EVAL(&i - 1);


	*total observations;
	%GLOBAL All_Obs_Cnt;
	DATA _NULL_;
			SET &Input_DSN(KEEP=&Target) NOBS=temp_x;
			IF _N_ = 1 THEN CALL SYMPUT("All_Obs_Cnt", TRIM(LEFT(temp_x)));
	RUN;		
	%LET All_Obs_Cnt = &All_Obs_Cnt;
	
			
	*3. binning the Input variable list one by one;
	%LET k=1;
	%DO k=1 %TO &InputVarCnt;
			PROC SQL NOPRINT;
					*Missing Value check;	
					%LET MissXvar_Cnt = 0;					
										
					*count missing values;
					SELECT COUNT(*) INTO :MissXvar_Cnt FROM &Input_DSN WHERE &&InputVar&k IS MISSING;				
					%LET MissXvar_Cnt =&MissXvar_Cnt;					
					
					*check whether the missing input observation > allowed missing part;
					%IF %SYSEVALF(&MissXvar_Cnt/&All_Obs_Cnt)>&Max_Missing_Portion %THEN %DO;																										
							%PUT ERROR:  Input Variable  &&InputVar&k  has missing value (&MissXvar_Cnt observations, accounting for %SUBSTR(%SYSEVALF(100*&MissXvar_Cnt/&All_Obs_Cnt),1,2)% of total population);
							%RETURN;
					%END;								
					
					*although the missing part is within the allowed range, it needs to put the warning to the log;
					%IF &MissXvar_Cnt>0 %THEN %DO;
							%PUT WARNING:  Input Variable  &&InputVar&k  has missing value (&MissXvar_Cnt observations, , accounting for %SUBSTR(%SYSEVALF(100*&MissXvar_Cnt/&All_Obs_Cnt),1,2)% of total population);
					%END;
			QUIT;
			
			*initial binning generation;					
			%IF &Inital_Bin_Method = 1 %THEN %DO;  /*equal width, missing part is a special width*/
					*Maximum & Minimum value for each of the X variable;
					%LET Var_X_Max = ;
					%LET Var_X_Min = ;	
					PROC SQL NOPRINT;
							SELECT MIN(&&InputVar&k), MAX(&&InputVar&k) INTO :Var_X_Min, :Var_X_Max FROM &Input_DSN;  /*excluding missing observations*/								
					QUIT;
					%LET Var_X_Max=&Var_X_Max;
					%LET Var_X_Min=&Var_X_Min;
					
					*with missing observations;
					%IF  &MissXvar_Cnt>0 %THEN %DO;
							*missing observation will be taken as an initial bin;
							*calculate the "width" for the intial binning;
							%LET Init_Width=%SYSEVALF((&Var_X_Max-&Var_X_Min)/(&Initial_Bin_Num-1));  /* initial_bin_num -1, deducting the "missing" bin;*/
							
							DATA temp_ds_bin;
									SET &Input_DSN(KEEP=&Target &&InputVar&k);
									IF  &&InputVar&k = . THEN Bin =1;
									ELSE IF &&InputVar&k = &Var_X_Min THEN Bin =2;
									ELSE IF &&InputVar&k = &Var_X_Max THEN Bin =&Initial_Bin_Num;
									ELSE	Bin =CEIL((&&InputVar&k -&Var_X_Min)/&Init_Width)+1;
							RUN;
					%END;
					
					*without missing observations;
					%IF  &MissXvar_Cnt=0 %THEN %DO;
							%LET Init_Width=%SYSEVALF((&Var_X_Max-&Var_X_Min)/(&Initial_Bin_Num));  						
							DATA temp_ds_bin;
									SET &Input_DSN(KEEP=&Target &&InputVar&k);
									IF &&InputVar&k = &Var_X_Min THEN Bin =1;
									ELSE IF &&InputVar&k = &Var_X_Max THEN Bin =&Initial_Bin_Num;
									ELSE Bin =CEIL((&&InputVar&k - &Var_X_Min)/&Init_Width);
							RUN;
					%END;						
			%END;
			
			%IF &Inital_Bin_Method = 2 %THEN %DO;  /*equal height, missing part is a special height*/
					%IF  &MissXvar_Cnt>0 %THEN %DO;
							PROC RANK DATA=&Input_DSN(KEEP=&Target &&InputVar&k) GROUPS=%EVAL(&Initial_Bin_Num-1) OUT=temp_ds_bin;
									VAR &&InputVar&k;
									RANKS Bin;
							RUN;
							
							DATA temp_ds_bin;
									SET temp_ds_bin;
									IF Bin = . THEN Bin=1;
									ELSE Bin = Bin+2;
							RUN;
					%END;
					
					%IF  &MissXvar_Cnt=0 %THEN %DO;
							PROC RANK DATA=&Input_DSN(KEEP=&Target  &&InputVar&k) GROUPS=&Initial_Bin_Num OUT=temp_ds_bin;
									VAR &&InputVar&k;
									RANKS Bin;
							RUN;
							
							DATA temp_ds_bin;
									SET temp_ds_bin;
									Bin=Bin+1;									
							RUN;
					%END;							
			%END;
			
			*get the range for each of the bin;
			PROC SQL NOPRINT;
					CREATE TABLE temp_bin_limits AS
					SELECT Bin, MIN(&&InputVar&k) AS Bin_LowerLimit, MAX(&&InputVar&k) AS Bin_UpperLimit 
						FROM temp_ds_bin 
						GROUP BY Bin
						ORDER BY Bin;						
			QUIT;
			
			*calculate the 'Event' and 'NonEvent' count for each of the bin;
			PROC SORT DATA=temp_ds_bin(KEEP=&Target Bin) OUT=temp_bin_smry;
					BY Bin;
			RUN;
			
			DATA temp_bin_smry;
					SET temp_bin_smry;
					BY Bin;
					
					*initialize;
					IF FIRST.Bin THEN DO;
							Total_Cnt = .;
							Event_Cnt = .;			/* Event:Target=1*/
							NonEvent_Cnt = .;		/*NonEvent:Targe=0*/				
					END;
					
					Total_Cnt +1;
					Event_Cnt +(&Target =1);
					NonEvent_Cnt +(&Target =0);
					
					IF LAST.Bin;
					DROP &Target;
			RUN;			
			
		*merge the min/max with the cross tabulation;					
		DATA temp_bin_smry;
			MERGE temp_bin_smry
						temp_bin_limits;
			BY Bin;
		RUN;
		
		*calculate the total Events and Nonevents;
		%LET Event_Total = ;
		%LET NonEvent_Total =  ;
		PROC SQL NOPRINT;
				SELECT SUM(Event_Cnt),  SUM(NonEvent_Cnt) INTO :Event_Total,  :NonEvent_Total FROM temp_bin_smry;
		QUIT;
		%LET Event_Total = &Event_Total;
		%LET NonEvent_Total = &NonEvent_Total;		
		
		*in case of ZERO COUNT event or ZERO COUNT nonevent;
		*collapsing in ZERO COUNT case;
		*make sure the last observation has non-zero count for both event and nonevent;
		%LET Precondition =1;
		%DO %WHILE (&Precondition=1);
				PROC SQL NOPRINT;
						SELECT MAX(Bin) INTO :LastObsIndex FROM temp_bin_smry;																		
						%LET LastObsIndex = &LastObsIndex;
						
						SELECT Total_Cnt, Event_Cnt, NonEvent_Cnt,  Bin_UpperLimit INTO: Last_Total, : Last_Event, : Last_NonEvent, :Last_Upper FROM temp_bin_smry WHERE Bin=&LastObsIndex;
						%LET Last_Total = &Last_Total;
						%LET Last_Event = &Last_Event;
						%LET Last_NonEvent = &Last_NonEvent;
						%LET Last_Upper = &Last_Upper;
						
						%IF &Last_Event = 0 OR &Last_NonEvent = 0 %THEN %DO;
								UPDATE temp_bin_smry
										SET
												Total_Cnt = Total_Cnt + &Last_Total,
												Event_Cnt= Event_Cnt + &Last_Event ,
												NonEvent_Cnt = NonEvent_Cnt + &Last_NonEvent,
												Bin_UpperLimit = &Last_Upper
										WHERE Bin=%EVAL(&LastObsIndex-1);
								
								DELETE  FROM temp_bin_smry WHERE Bin=&LastObsIndex;
						%END;
						
						%IF &Last_Event > 0 AND &Last_NonEvent > 0 %THEN %DO;
								%LET Precondition =0;
						%END;
				QUIT;
		%END;	
							
		
		DATA temp_bin_smry;
				SET temp_bin_smry;
				BY Bin;
				
				RETAIN Total Event 0 NonEvent 0 Lower 0  Upper 0  New_Start_Ind 0;
				
				*collapse;
				Total_Cnt = SUM(Total, Total_Cnt);
				Event_Cnt = SUM(Event, Event_Cnt);
				NonEvent_Cnt = SUM(NonEvent, NonEvent_Cnt);
				
				*range initialization;
				IF _N_ =1 OR New_Start_Ind=1 THEN DO;
						Lower=Bin_LowerLimit;
						Upper=Bin_UpperLimit;		
				END;
							
				IF	Event_Cnt=0 OR NonEvent_Cnt=0 THEN DO;
						Total = Total_Cnt;
						Event = Event_Cnt;
						NonEvent = NonEvent_Cnt;		
						New_Start_Ind =0;
						Lower=MIN(Bin_LowerLimit, Lower);
						Upper=MAX(Bin_UpperLimit, Upper);											
				END;
				ELSE DO;
						Bin_LowerLimit = MIN(Lower, Bin_LowerLimit);
						Bin_UpperLimit = MAX(Upper, Bin_UpperLimit);
						Total=0;
						Event=0;
						NonEvent=0;
						Lower=0;
						Upper=0;
						New_Start_Ind =1;				
						OUTPUT;
				END;
				
				DROP Total Event NonEvent Lower Upper New_Start_Ind;
		RUN;
		
		*final preparation;
		DATA temp_bin_smry;
	 		SET temp_bin_smry;
	 		Bin_Index = _N_;
		RUN;		
		
			
		*binning algorithms -- chi merge;		
		%LET ChiSquare_threshold = %SYSFUNC(QUANTILE(CHISQUARE, %SYSEVALF(1-&ChiSquare_Sign_Level),1));
		
		%LET Split_Stop_Ind = 0;  /*two conditions: the maximum bins allowed and minimum chi-squared allowed;*/
		%DO %UNTIL (&Split_Stop_Ind = 1);							
				*retrieve the current number of bins;
				%LET Current_Bin_Nr = ;
				PROC SQL NOPRINT;
						SELECT MAX(Bin_Index) INTO :Current_Bin_Nr FROM temp_bin_smry;
				QUIT;
				%LET Current_Bin_Nr = &Current_Bin_Nr;
				
				*store the minimu chisquare value and associated index to split;
				%LET BestIndex_ToSplit =0 ;
				%LET BestChiSqure_Value =10000 ;  /*10000 is an arbitrary big number*/
				
				*calculate the chi-squre statistics between each of the adjacent pairs of bins;
				%LET i=1;
				%DO i=1 %TO %EVAL(&Current_Bin_Nr - 1);			
						*count the event/nonevent/total of each pair of adjacent bins;			
						PROC SQL NOPRINT;
								SELECT SUM(Event_Cnt) , SUM(NonEvent_Cnt), SUM(Total_Cnt) INTO :Event_1, :NonEvent_1, :Total_1 FROM temp_bin_smry WHERE Bin_Index = &i;
								SELECT SUM(Event_Cnt) , SUM(NonEvent_Cnt), SUM(Total_Cnt) INTO :Event_2, :NonEvent_2, :Total_2 FROM temp_bin_smry WHERE Bin_Index = %EVAL(&i+1);
						QUIT;							
						
						*remove the leading/trailing blanks;							
						%LET Event_1 = &Event_1;
						%LET NonEvent_1 = &NonEvent_1;
						%LET Total_1 = &Total_1;
						%LET Event_2 = &Event_2;
						%LET NonEvent_2 = &NonEvent_2;
						%LET Total_2 = &Total_2;						
						
						*calculate the totals;
						%LET Total_Event = %EVAL(&Event_1 + &Event_2);
						%LET Total_NonEvent = %EVAL(&NonEvent_1 + &NonEvent_2);						
						%LET Total_All = %EVAL(&Total_Event + &Total_NonEvent);
						
						*calculate the ChiSquare Value;
						%LET ChiSquare_&i = %SYSEVALF((&Event_1 - (&Total_1*&Total_Event)/&Total_All)**2/(&Total_1*&Total_Event/&Total_All) + (&NonEvent_1 - (&Total_1*&Total_NonEvent/&Total_All))**2/(&Total_1*&Total_NonEvent/&Total_All) +
																		   (&Event_2 - (&Total_2*&Total_Event)/&Total_All)**2/(&Total_2*&Total_Event/&Total_All) + (&NonEvent_2 - (&Total_2*&Total_NonEvent/&Total_All))**2/(&Total_2*&Total_NonEvent/&Total_All));
						
						*%PUT Event_1 = &Event_1  NonEvent_1 = &NonEvent_1  Event_2 = &Event_2  NonEvent_2 = &NonEvent_2  ChiSquare = &&ChiSquare_&i;				
						%IF %SYSEVALF(&BestChiSqure_Value > &&ChiSquare_&i) %THEN %DO;
								%LET BestChiSqure_Value = &&ChiSquare_&i;
								%LET BestIndex_ToSplit=&i;
						%END;
				%END;
				
				*merge the pair with the lowest ChiSquare value;
				%IF %SYSEVALF(&BestChiSqure_Value<= &ChiSquare_threshold) OR %SYSEVALF(&Current_Bin_Nr>&Max_Bin_Num) %THEN %DO;
						PROC SQL NOPRINT;
								UPDATE temp_bin_smry
								SET Bin_Index = Bin_Index - 1
								WHERE Bin_Index > &BestIndex_ToSplit;																
						QUIT;													
				%END;
				
				*proceed to split or not;
				%IF %SYSEVALF(&BestChiSqure_Value> &ChiSquare_threshold) AND %SYSEVALF(&Current_Bin_Nr<=&Max_Bin_Num) %THEN %DO;
						%LET Split_Stop_Ind = 1;
				%END;
		
		%END;
	
		DATA temp_bin_smry;
				FORMAT  Input_Var $32.;
				SET temp_bin_smry;
				Input_Var = "&&InputVar&k";	
				
				FORMAT Bin_Event_Rate Event_Rate NonEvent_Rate WOE IV 8.4;						
				Bin_Event_Rate = Event_Cnt/Total_Cnt;
				Event_Rate = Event_Cnt/&Event_Total;
				NonEvent_Rate = NonEvent_Cnt/&NonEvent_Total;
				WOE = LOG((Event_Cnt/NonEvent_Cnt)/(&Event_Total/&NonEvent_Total));
				IV = (Event_Rate - NonEvent_Rate)*LOG(Event_Rate/NonEvent_Rate);										
		RUN;
	
		*binning for one variable is done, and ready for the next variable in the list;
		%IF &k=1 %THEN %DO;
				DATA &Output_DSN_Detail;					
						SET  temp_bin_smry;											
				RUN;						
		%END;
		
		%IF &K>1 	%THEN %DO;
				DATA &Output_DSN_Detail;
						SET &Output_DSN_Detail						
						temp_bin_smry;						
				RUN;
		%END;	
		
		*summarize ;
		PROC SORT DATA= &Output_DSN_Detail OUT=&Output_DSN_Smry;
				BY Input_Var Bin_Index Bin;
		RUN;
		
		DATA &Output_DSN_Smry(RENAME= (Total=Total_Cnt Event=Event_Cnt NonEvent=NonEvent_Cnt Lower=Bin_LowerLimit Upper=Bin_UpperLimit ));
				SET &Output_DSN_Smry(DROP=Bin_Event_Rate Event_Rate NonEvent_Rate WOE IV);
				BY Input_Var Bin_Index Bin;
			
				RETAIN   Lower Upper;
				IF FIRST.Bin_Index THEN DO;
						Total =0;
						Event=0;
						NonEvent=0;
						Lower=Bin_LowerLimit;
						Upper=Bin_UpperLimit;	
				END;
			
				Total +Total_Cnt;
				Event +Event_Cnt;
				NonEvent + NonEvent_Cnt;
				Lower= (Lower>< Bin_LowerLimit);
				Upper = MAX(Upper, Bin_UpperLimit );
			
				FORMAT Bin_Event_Rate Event_Rate NonEvent_Rate WOE IV 8.4;						
				Bin_Event_Rate = Event/Total;
				Event_Rate = Event/&Event_Total;
				NonEvent_Rate = NonEvent/&NonEvent_Total;
				WOE = LOG((Event/NonEvent)/(&Event_Total/&NonEvent_Total));
				IV = (Event_Rate - NonEvent_Rate)*LOG(Event_Rate/NonEvent_Rate);			
							
				IF LAST.Bin_Index;
				DROP Total_Cnt Event_Cnt  NonEvent_Cnt Bin_LowerLimit Bin_UpperLimit   Bin;
		RUN;
	
		PROC SQL NOPRINT;
				CREATE TABLE temp_iv_all AS
						SELECT Input_Var, SUM(IV) AS IV_VAR FROM &Output_DSN_Smry GROUP BY Input_Var HAVING SUM(IV)>=&Info_Value_Threshold;
		QUIT;
		
		PROC SORT DATA=temp_iv_all NODUPKEY;
				BY Input_Var;
		RUN;
			
		PROC SORT DATA=&Output_DSN_Detail;
				BY Input_Var;
		RUN;
		PROC SORT DATA=&Output_DSN_Smry;
				BY Input_Var;
		RUN;
		DATA &Output_DSN_Sel_Detail;
				MERGE &Output_DSN_Detail
							temp_iv_all(IN=a);
				BY Input_Var;
				
				IF a;
		RUN;
		
		DATA &Output_DSN_Sel_Smry;
				MERGE &Output_DSN_Smry
							temp_iv_all(IN=a);
				BY Input_Var;
				
				IF a;
		RUN;
	%END;
%MEND;


