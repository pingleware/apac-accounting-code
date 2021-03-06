      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	 ******   **	 **  ********  *
      *   **	**     **     **    **	**    **  ***	 **	**     *
      *   **	       **     **    **	**    **  ** *	 **	**     *
      *    ******      **     *******	**    **  **  *  **	**     *
      * 	**     **     **	**   ***  **   * **	**     *
      *   **	**     **     **	 ******   **	***	**     *
      *    ******      **     **	      **  **	 **	**     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *  R E A D   I M P O R T	 F I L E   A N D   U P D A T E	 T H E *
      *  S T O C K   Q U A N T I T I E S   O F	 E X I S T I N G       *
      *  S T O C K   I T E M S.					       *
      *                                                                *
      *     VERSION 8.06 - August 2002				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPQADJ.
000030 AUTHOR.         J.W.LEMMON (CSS).
000040 DATE-WRITTEN.   JANUARY 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2002
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090                 CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000140 COPY STOCK.SL.

	   SELECT STKQNT  ASSIGN DISK
000180			  STATUS WS-STATUS
000190                    ACCESS SEQUENTIAL
000200                    ORGANIZATION LINE SEQUENTIAL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

000300 COPY STOCK.FDE.

000290 FD  STKQNT    LABEL RECORD STANDARD
000300		     VALUE OF FILE-ID "STKQUANT.UPD".
       01  QNT-REC1.
	   03  QNT-ITEM     PIC  X(14).
	   03  QNT-QUANT    PIC S9(07)V9(04) COMP-3.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001700 77  WS-DISC	    PIC 999V99	  COMP-3.
001710 77  WS-SUB           PIC 9(04)     COMP-5.
001710 77  WS-S1            PIC 9(04)     COMP-5.
001720 77  WS-S2            PIC 9(04)     COMP-5.
001730 77  WS-S3            PIC 9(04)     COMP-5.
001740 77  WS-S4            PIC 9(04)     COMP-5.
001750 77  WS-S5            PIC 9(04)     COMP-5.
001760 77  WS-S6            PIC 9(04)     COMP-5.
001770 77  WS-S7            PIC 9(04)     COMP-5.
001780 77  WS-S8            PIC 9(04)     COMP-5.
001790 77  WS-IXS           PIC 9(04)     COMP-5.
001800 77  WS-IXS1          PIC 9(04)     COMP-5.
001810 77  WS-IXS2          PIC 9(04)     COMP-5.
001820 77  WS-IXS3          PIC 9(04)     COMP-5.
001830 77  WS-IXS4          PIC 9(04)     COMP-5.
001900 77  WS-RECOVER	    PIC 9(06)	  COMP-5.
001900 77  WS-RECORDS       PIC 9(06)     COMP-5.
001910 77  WS-TRANS         PIC 9(04)     COMP-5 VALUE 1.
001920 77  WS-KEYSTR        PIC 9(04)     COMP-5.
001930 77  WS-GST-RATE      PIC S9(03)V99 COMP-3.
001940 77  WS-MONTHS        PIC S9(01)V99 COMP-3.
001960 77  WS-KEY1          PIC 9(04)     COMP-5.
001980 77  WS-ITM           PIC X(14).
       77  WS-ETYPE         PIC X(01).
002020 77  WS-OPTION	    PIC X(01).
002020 77  WS-SKIP          PIC X(01).
002040 77  WS-ADJ           PIC X(01).
002070 77  WS-ERROR         PIC 9(01).
002080 77  WS-IND           PIC 9(01)     VALUE 0.
002090 77  WS-INDB          PIC 9(01)     VALUE 0.
002100 77  WS-INDP          PIC 9(01)     VALUE 0.
002110 77  WS-INDS          PIC 9(01)     VALUE 0.
002120 77  WS-AMEND         PIC 9(01)     VALUE 0.
      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC  X(78).
               05  FILLER  PIC X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
               05  FILLER      PIC  X(39).
002450     03  WS-MID-LNE.
002460         05  WS-MCHR PIC X(01) OCCURS 80.
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
002450	   03  WS-MID-LNE2.
002460         05  WS-MCH  PIC X(01) OCCURS 80.
	   03  WS-MID78 REDEFINES WS-MID-LNE2.
	       05  FILLER      PIC  X(01).
	       05  WS-BLNK78   PIC  X(78).
	       05  FILLER      PIC  X(01).
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

002330 COPY WS.WS.

002330 COPY CHAIN.WS.

002350 01  W02-FID.

       COPY APACFIDS.ID.

000010	   03  W02-STOCKF.
000030	       05  FILLER      PIC X(05) VALUE "STCK_".
000040	       05  W02-EXT22   PIC X(03) VALUE "CSS".
000050         05  FILLER      PIC X(04) VALUE ".APC".

       COPY W10.STK.

       COPY W12.WS.

003700 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340         05  W25-WHOLE   PIC 9(05).
004350         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
003750     03  W25-SCOST      PIC S9(09)V99 COMP-3.
003760     03  W25-PCOST      PIC S9(09)V99 COMP-3.
003760     03  W25-PCST       PIC S9(09)V99 COMP-3.
003760     03  W25-PTOT       PIC S9(09)V99 COMP-3.
003770     03  W25-TSELL      PIC S9(09)V99 COMP-3.
003770     03  W25-TOTAL      PIC S9(09)V99 COMP-3.
003780     03  W25-VALUE      PIC S9(09)V99 COMP-3.
003780     03  W25-ADJUST     PIC S9(09)V99 COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

003800 COPY W50.WS.

004090	   03  W80-DEL.
004100         05  W80-2N      PIC 9(02).
006270 01  W85-PASS.
006280     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY       PIC  9(02)    COMP-3.
006300     03  W85-MARG        PIC S9(02)V99.
004110 01  W90-BAL             PIC S9(07)V99 COMP-3.
004120 01  W95-COMP            PIC X(40).
005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.
      /
004130 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY S99.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

      /
007730 PROCEDURE DIVISION.
007740 AA000-MAIN              SECTION.
007750 AA000-INIT.
007760       PERFORM ZA000-INIT.
007780	     PERFORM BA000.

       AA49.
	     CLOSE STOCK.
	     STOP RUN.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY STOCK.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.

       COPY STOCK.WR.

       COPY STOCK.1ST.

      /
      *    ****    C L E A R   S T O C K   O R D E R S
      *
035140 BA000	       SECTION.
035150 BA00.
035160	     DISPLAY CLR-SCREEN.
	     DISPLAY "STOCK QUANTITY - IMPORT/EXPORT" AT 0226
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     MOVE " "		 TO WS-OPTION.

035180 BA05.
	     MOVE "'E'xport or 'I'mport quantities  [ ]"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
036230	   IF NOT (WS-OPTION = "E" OR "I")
036240	       GO TO BA05.
	   IF WS-OPTION = "E"
	       PERFORM BB000
	   ELSE
	       PERFORM BC000.

       BA999.
	     EXIT.

       BB000	       SECTION.
       BB00.
035160	     DISPLAY CLR-SCREEN.
	     DISPLAY "EXPORT STOCK QUANTITIES" AT 0254
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.

       BB05.
035170	     MOVE "Run EXPORT routine (Y/N)  [ ]"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
036230	   IF NOT (WS-OPTION = "Y" OR "N")
036240	       GO TO BB05.
035270     IF WS-OPTION = "N"
035280	       GO TO BB999.
	     OPEN OUTPUT STKQNT.
035360       MOVE "    !"        TO STK-CODE.
035370       PERFORM START-AT-STOCK-CODE THRU READ-STOCK-EXIT.
035460     IF WS-F-ERROR = 22
035470	       GO TO BB995.
035380	     DISPLAY "Stock Record :" AT 1112.
035380	     DISPLAY "Export Record:" AT 1312.
035390 BB10.
035450	     PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
035460     IF WS-F-ERROR = 22
035470	       GO TO BB995.
035440	     DISPLAY STK-CODE AT 1128 WITH FOREGROUND-COLOR 11
		     " " STK-DESC WITH FOREGROUND-COLOR 14.
	   IF NOT (STK-IND = 0)
	       UNLOCK STOCK
	       GO TO BB10.
	   IF STK-QUANT = ZERO
035720	       GO TO BB10.
	     MOVE STK-CODE	 TO QNT-ITEM.
	     MOVE STK-QUANT	 TO QNT-QUANT.
035440	     DISPLAY STK-CODE AT 1328
		     WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     WRITE QNT-REC1.
	     GO TO BB10.

       BB995.
	     CLOSE STKQNT.

035730 BB999.
035740       EXIT.

       BC000	       SECTION.
       BC00.
035160	     DISPLAY CLR-SCREEN.
	     DISPLAY "IMPORT STOCK QUANTITIES" AT 0254
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.

       BC05.
035170	     MOVE "Run IMPORT routine (Y/N)  [ ]"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
036230	   IF NOT (WS-OPTION = "Y" OR "N")
036240	       GO TO BC05.
035270     IF WS-OPTION = "N"
035280	       GO TO BC999.
	     OPEN INPUT STKQNT.
035380	     DISPLAY "Export Record:" AT 1112.
035380	     DISPLAY "Stock Record :" AT 1312.

035390 BC10.
	     READ STKQNT
		  AT END GO TO BC995.
035440	     DISPLAY QNT-ITEM AT 1128
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
035360	     MOVE QNT-ITEM	 TO STK-CODE.
035450	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
035460     IF WS-F-ERROR = 22
035470	       GO TO BC10.
035440	     DISPLAY STK-CODE AT 1328 WITH FOREGROUND-COLOR 11
		     " " STK-DESC WITH FOREGROUND-COLOR 14.
	     ADD QNT-QUANT	 TO STK-QUANT.
	     PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
	     GO TO BC10.

       BC995.
	     CLOSE STKQNT.

035730 BC999.
035740       EXIT.

      /
044130 ZA000-INIT	       SECTION.
044140 ZA000-OPEN.
	     ACCEPT W12-T-YMD FROM DATE.
	     MOVE W12-YY	 TO W12-YEAR.
	     MOVE W12-MM	 TO W12-MONTH.
	     MOVE W12-DD	 TO W12-DAY.
	     MOVE W12-TODAY	 TO TODAY-DDMMYY.
	     PERFORM ZA55 THRU ZA60.
044150	     DISPLAY CLR-SCREEN.
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    S E T   U P   T H E   F U N C T I O N   K E Y S
      *
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *
      *    ESC, F1 to F10 keys
      *
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PAGE-UP AND PAGE-DOWN KEYS
      *
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
	     MOVE 64		 TO MOUSE-FUNC.
	     MOVE 1		 TO MOUSE-PARAM.
	     CALL X"AF" USING MOUSE-FUNC
			      MOUSE-PARAM.
	   IF MOUSE-FUNC NOT = 255
	       MOVE "Y"		 TO MOUSE-ATTACHED
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM
	   ELSE
	       GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      * 	   A S	 F U N C T I O N   K E Y
      *
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.



	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.


      *
      *    ****    P A R A M E T E R   F I L E
      *
       ZA00A.
	     MOVE "PARAM"	 TO AFID-KEY.

       ZA00-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO ZA00-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
	     EXIT.

       ZA00A-CONTINUE.
	     MOVE AFID-PATH	 TO W02-PARAM.




044300	     OPEN I-O STOCK.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 22       TO WS-F-ERROR
                   GO TO ZA50.
044330     IF WS-STATUS NOT = "00"
044340         MOVE 22           TO WS-F-ERROR
050210         PERFORM OPEN-ERROR.
	     GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
039850 ZA50.
	   IF WS-F-ERROR = 22
	       MOVE "STOCK file" TO WS-FILE.
039980	     DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.
       ZA55.
041220       MOVE 1              TO WS-S1.
021870       MOVE SPACES         TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (18) WS-TCHR (49)
021940                              WS-TCHR (60) WS-TCHR (70).
021950       MOVE WS-G3          TO WS-MCHR (18) WS-MCHR (49)
021960                              WS-MCHR (60) WS-MCHR (70)
                                    WS-MCHR (1)  WS-MCHR (80) 
                                    WS-MCH  (1)  WS-MCH  (80).
021970       MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
021980                              WS-BCHR (60) WS-BCHR (70).
      *      MOVE WS-COMPANY	 TO WS-TOP-COMP.
044790 ZA200.
044800       DISPLAY "Files locked - Try later" AT 2312
044810                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
044820       DISPLAY "Press any key" AT 2512 
044830		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
044840	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
044870 ZA999-EXIT.
044880       EXIT.
      /
044890 ZB000-ERROR	       SECTION.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041850	   IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX.
041880     IF WS-STATUS = "10"
041890         MOVE "End of FILE" TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950         MOVE "Invalid record number" TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980         MOVE "DISK full" TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040         MOVE "FILE locked" TO WS-STAT-MESSAGE.
045380       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
045390       DISPLAY "Status " AT 1212 
                      WS-STATUS WITH FOREGROUND-COLOR 11
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
045400     IF WS-STATUS NOT = "94" 
045410         DISPLAY "Key    " AT 1412 
                        WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170	     STOP RUN.
