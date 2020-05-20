      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   *******     ****    *******	 ******    *****   **	 **    *
      *   **	**   **  **   **    **	**    **  **   **  ***	 **    *
      *   **	**  **	  **  **   **	**	  **   **  ****  **    *
      *   *******   ********  ******	**	  **   **  ** ** **    *
      *   **	    **	  **  **   **	**	  **   **  **  ****    *
      *   **	    **	  **  **    **	**    **  **   **  **	***    *
      *   **	    **	  **  **    **	 ******    *****   **	 **    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C O N V E R T   P A R A M E T R R	F I L R 	       *
      *                                                                *
      *     VERSION 8.06 - July 2002				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PARCON.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JULY 2002.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2002 - 2002
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.(Id No. 4412165050082).

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090                 CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000270 COPY PARAM.SL.

      /
000390 DATA DIVISION.
000400 FILE SECTION.

       COPY APACFIDS.FDE.

000560 COPY PARAM.FDE.

      /
002090 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002140 77  WS-S1	   PIC	9(04)	 COMP-5.
002150 77  WS-S2           PIC  9(04)    COMP-5.
002160 77  WS-S3           PIC  9(04)    COMP-5.
002170 77  WS-S4           PIC  9(04)    COMP-5.
002280 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
002350 77  WS-NETKEY       PIC  9(04)    COMP-5.
002500 77  WS-PAR          PIC  9(01) VALUE 0.
002510 77  WS-INO	   PIC	9(03) VALUE 0.
002520 77  WS-LINES        PIC  9(02) VALUE 0.
002530 77  WS-FIRST	   PIC	9(03) VALUE 0.
002540 77  WS-LAST	   PIC	9(03) VALUE 0.
002550 77  WS-LASTINV      PIC  9(04)    COMP-5.
002560 77  WS-LINE         PIC  9(04)    COMP-5.
002570 77  WS-PAGE         PIC  9(02)    COMP-5.
002580 77  WS-L1           PIC  9(04)    COMP-5.
002590 77  WS-OPTION	   PIC	X(01).
002590 77  WS-IN-OPT       PIC  X(01).
002640 77  WS-NUM          PIC  Z9.
002650 77  WS-ERROR        PIC  9(01).
002670 77  WS-CHAR1        PIC  X(01).
002680 77  WS-CHAR2        PIC  X(01).
002690 77  WS-COMP         PIC  9(01).
002710 77  WS-UPDATE       PIC  9(01) VALUE ZERO.
002720 77  WS-CONREC       PIC  9(01) VALUE ZERO.
002730 77  WS-HEAD         PIC  X(01).
002740 77  WS-TYPE         PIC  X(01).
002750 77  WS-AGE          PIC  X(01).
002760 77  WS-PRC          PIC  X(01).
       77  WS-SPACE	   PIC	X(70) VALUE SPACES.
002880 77  WS-PER          PIC  Z(02)9.99.
002890 77  WS-CR           PIC  X(01).
002900 77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002910 01  WS-DB-LINE.
002920     03  WS-TOP-LINE.
002930         05  WS-TCHAR PIC X(01) OCCURS 80.
002940     03  WS-TOP-LNE2.
002950         05  WS-TCR  PIC X(01) OCCURS 80.
002960     03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
002970         05  FILLER      PIC  X(01).
002980         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003000	   03  WS-MID-LINE.
003010         05  WS-MCHAR PIC X(01) OCCURS 80.
	   03  WS-MINV REDEFINES WS-MID-LINE.
	       05  WS-INV1     PIC  X(01).
	       05  WS-INV78    PIC  X(78).
	       05  WS-INV80    PIC  X(01).
003020	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
003040	   03  WS-BOT-LINE.
003050         05  WS-BCHAR PIC X(01) OCCURS 80.
003060     03  WS-BOT-LNE2.
003070         05  WS-BCR  PIC X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
           03  WS-MM-LINE.
	       05  FILLER  PIC	X(01) VALUE X"B3".
	       05  FILLER  PIC	X(60) VALUE SPACES.
	       05  FILLER  PIC	X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".
003080     03  WS-TOP-LNE.
003090         05  WS-TCHR PIC X(01) OCCURS 80.
003100     03  WS-T-LINE REDEFINES WS-TOP-LNE.
003110         05  FILLER  PIC X(01).
003120         05  WS-H-LINE
003130                     PIC X(78).
003140         05  FILLER  PIC X(01).
003150     03  WS-MID-LNE.
003160         05  WS-MCHR PIC X(01) OCCURS 80.
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
003170	   03  WS-BOT-LNE.
003180         05  WS-BCHR PIC X(01) OCCURS 80.
003190     03  WS-TP-LNE.
003200         05  WS-TCH  PIC X(01) OCCURS 80.
003210     03  WS-MD-LNE.
003220         05  WS-MCH  PIC X(01) OCCURS 80.
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
003230	   03  WS-BT-LNE.
003240         05  WS-BCH  PIC X(01) OCCURS 80.
003190     03  WS-TP-LINE.
003200         05  WS-TCA  PIC X(01) OCCURS 80.
003210     03  WS-MD-LINE.
003220         05  WS-MCA  PIC X(01) OCCURS 80.
	   03  WS-SO78-L REDEFINES WS-MD-LINE.
	       05  WS-MSO1     PIC  X(01).
	       05  WS-MSO78    PIC  X(78).
	       05  WS-MSO80    PIC  X(01).
003230	   03  WS-BT-LINE.
003240         05  WS-BCA  PIC X(01) OCCURS 80.

003340 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "QTP".
	   03  WS-PROG	       PIC  X(03) VALUE "005".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003530 01  W02-FID.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

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
001660	   03  W25-TIME        PIC  9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001680	       05  FILLER      PIC  9(02).
001690	       05  W25-FACT    PIC  9(06).
	       05  W25-DATE REDEFINES W25-FACT.
		   07  W25-YY  PIC  9(02).
		   07  W25-MM  PIC  9(02).
		   07  W25-DD  PIC  9(02).
	   03  W25-CUR-CC      PIC  9(02) COMP-5.
	   03  W25-PRV-CC      PIC  9(02) COMP-5.
	   03  W25-NXT-CC      PIC  9(02) COMP-5.

       COPY W40.WS.

       COPY FUNCTION.WS.

005370 COPY W50.WS.

006590 01  W95-STM.
006600     03  W95-COMP        PIC X(40).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
006840 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S42.
           03  LINE  2 COLUMN 28 FOREGROUND-COLOR 15
				 VALUE "ADD QUOTATION REMARKS".
       COPY S99.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
010270	     PERFORM ZA000-INIT.
010760	     PERFORM EA000.

013250 AA49.
013260	     STOP RUN.

010950 COPY LOCKED.RC2.

       COPY FUNCTION.CRT.

010970 COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
011130 AC000              SECTION.

011290 COPY PARAM.RD.

      /
011370 AD000-WRITE             SECTION.

011410 COPY PARAM.WR.

      /
      *  ****  Q U O T A T I O N   R E M A R K S   A M E N D
      *
021000 EA000		       SECTION 56.
021010 EA00.
021020       DISPLAY CLR-SCREEN.
             DISPLAY S42.
       EA05.
	     MOVE 271		 TO WS-PARKEY.
	     INITIALIZE PAR-RECORD266.

004450 EA80.
004460       WRITE PAR-RECORD266.
004490	     ADD 1		 TO WS-PARKEY
004480	   IF WS-PARKEY < 275
004500	       GO TO EA80.
	     MOVE "C"		 TO PAR-QTE-FMT.
	     MOVE 6		 TO PAR-QTE-MRG.
	     MOVE "Dear Sir/Madam,"
				 TO PAR-QTE-SAL.
	     MOVE "Yours sincerely"
				 TO PAR-QTE-SIG.
	     WRITE PAR-RECORD275.
004490       ADD 1               TO WS-PARKEY
	     INITIALIZE PAR-RECORD276.

       EA81.
004460	     WRITE PAR-RECORD276.
004490	     ADD 1		 TO WS-PARKEY
004480	   IF WS-PARKEY < 285
004500	       GO TO EA81.

       EA999.
             EXIT.

      /
044130 ZA000-INIT	       SECTION.
044140 ZA000-OPEN.
	     ACCEPT W25-FACT FROM DATE.
	     MOVE W25-YY	 TO W12-YEAR.
	     MOVE W25-MM	 TO W12-MONTH.
	     MOVE W25-DD	 TO W12-DAY.
	   IF W25-YY < 50
	       MOVE 20		 TO W12-CENT
	   ELSE
	       MOVE 19		 TO W12-CENT.
	     MOVE W12-TODAY	 TO TODAY-DDMMYY.
003250	     MOVE W12-DAY	 TO W12-DD.
	     MOVE W12-MONTH	 TO W12-MM.
003260	     MOVE W12-CENT	 TO W12-CC.
003270	     MOVE W12-YEAR	 TO W12-YY.
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
	     DISPLAY "ADJUST STOCK AVERAGE COST" AT 0228
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 1126
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "³ Company/Business Code     ³" AT 1226
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1326
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7.
	     ACCEPT WS-SYS-ID AT 1250
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     MOVE WS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     OPEN I-O APACFIDS.
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
	     OPEN I-O PARAM.

       ZA01B.
050580	     MOVE 1		 TO WS-PARKEY.
050590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050600       MOVE PAR-DMY        TO W12-TODAY.
050610       MOVE PAR-YMD        TO W12-T-YMD.
050620       MOVE PAR-COMPANY    TO W95-COMP.
051530	     GO TO ZA999.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
051090       MOVE 1 TO WS-S1.
051100       MOVE SPACES         TO WS-MID-LINE WS-MID-LNE 
				    WS-MD-LINE
                                    WS-MD-LNE.
051110 ZA60.
051120       MOVE WS-G1          TO WS-TCHAR (WS-S1) WS-BCHAR (WS-S1).
051130       MOVE WS-G1          TO WS-TCHR (WS-S1)  WS-BCHR (WS-S1).
051140       MOVE WS-G1          TO WS-TCH (WS-S1)   WS-BCH (WS-S1).
051140       MOVE WS-G1          TO WS-TCA (WS-S1)   WS-BCA (WS-S1).
051150       MOVE WS-G8          TO WS-TCR (WS-S1)   WS-BCR (WS-S1).
051160     IF WS-S1 < 80
051170         ADD 1 TO WS-S1
051180         GO TO ZA60.
051190       MOVE WS-G9          TO WS-TCR   (1).
051200       MOVE WS-G10         TO WS-TCR   (80).
051210       MOVE WS-G11         TO WS-BCR   (1).
051220       MOVE WS-G12         TO WS-BCR   (80).
051230       MOVE WS-G14         TO WS-TCHAR (1)  WS-BCHAR (1)
051240                              WS-TCHR  (1)  WS-BCHR  (1)
051240                              WS-TCA   (1)  WS-BCA   (1)
051250                              WS-TCH   (1)  WS-BCH   (1).
051260       MOVE WS-G13         TO WS-TCHAR (80) WS-BCHAR (80)
051270                              WS-TCHR  (80) WS-BCHR  (80)
051270                              WS-TCA   (80) WS-BCA   (80)
051280                              WS-TCH   (80) WS-BCH   (80).
051290       MOVE WS-G2          TO WS-TCHAR (4)  WS-TCHAR (19)
051300                              WS-TCHAR (50) WS-TCHAR (59) 
051310                              WS-TCHAR (69)
051320                              WS-TCHR  (18) WS-TCHR  (49)
051330                              WS-TCHR  (60) WS-TCHR  (70)
051340                              WS-TCA   (10) WS-TCA   (17)
051340                              WS-TCA   (47) WS-TCA   (56)
051340                              WS-TCA   (59) WS-TCA   (68)
051340                              WS-TCH   (8)  WS-TCH   (49)
051350                              WS-TCH   (61) WS-TCH   (72).
051360       MOVE WS-G3          TO WS-MCHAR (4)  WS-MCHAR (19) 
051370                              WS-MCHAR (50) WS-MCHAR (59) 
051380                              WS-MCHAR (69) WS-MCHAR (80)
                                    WS-MCHAR (1)
051390                              WS-MCH   (8)  WS-MCH   (49)
051400                              WS-MCH   (61) WS-MCH   (72)
                                    WS-MCA   (1)  WS-MCA   (80)
051340                              WS-MCA   (10) WS-MCA   (17)
051340                              WS-MCA   (47) WS-MCA   (56)
051340				    WS-MCA   (59) WS-MCA   (68)
051420                              WS-MCHR  (18) WS-MCHR  (49)
051430                              WS-MCHR  (60) WS-MCHR  (70)
051440                              WS-MCHR  (1)  WS-MCHR  (80) 
051450                              WS-MCH   (1)  WS-MCH   (80).
051460       MOVE WS-G4          TO WS-BCHAR (4)  WS-BCHAR (19) 
051470                              WS-BCHAR (50) WS-BCHAR (59) 
051480                              WS-BCHAR (69)
051490                              WS-BCHR  (18) WS-BCHR  (49)
051500                              WS-BCHR  (60) WS-BCHR  (70)
051340                              WS-BCA   (10) WS-BCA   (17)
051340                              WS-BCA   (47) WS-BCA   (56)
051340                              WS-BCA   (59) WS-BCA   (68)
051510                              WS-BCH   (8)  WS-BCH   (49)
051520                              WS-BCH   (61) WS-BCH   (72).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
051540 ZA200.
051550       DISPLAY "Files locked - Try later" AT 2312
051560                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
051570       DISPLAY "Press any key" AT 2512 
051580		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
051590	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
051600	     STOP RUN.
051610 ZA999.
051620       EXIT.

051640 I-O-ERRORS      SECTION  91.
051650 OPEN-ERROR.
051660       DISPLAY CLR-SCREEN.
051670       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
051680       PERFORM DISPLAY-FILE-NAME.
051690
051700 READ-ERROR.
051710       DISPLAY CLR-SCREEN.
051720       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
051730       PERFORM DISPLAY-FILE-NAME.
051740
051750 WRITE-ERROR.
051760       DISPLAY CLR-SCREEN.
051770       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
051730       PERFORM DISPLAY-FILE-NAME.

051790 DISPLAY-FILE-NAME.
052080	   IF WS-F-ERROR = 15
052090         MOVE WS-PARID     TO WS-FILE
052100	       MOVE WS-PARKEY	 TO WS-KEY.
052310     IF WS-STATUS = "10"
052320         MOVE "End of FILE" TO WS-STAT-MESSAGE
052330     ELSE
052340     IF WS-STATUS = "22"
052350         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
052360     ELSE
052370     IF WS-STATUS = "23"
052380         MOVE "Invalid record number" TO WS-STAT-MESSAGE
052390     ELSE
052400     IF WS-STATUS = "24"
052410         MOVE "DISK full" TO WS-STAT-MESSAGE
052420     ELSE
052430     IF WS-STATUS = "30"
052440         MOVE "DISK error" TO WS-STAT-MESSAGE
052450     ELSE
052460     IF WS-STATUS = "94"
052470         MOVE "FILE locked" TO WS-STAT-MESSAGE.
052480       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
052490       DISPLAY "Status " AT 1212 
052500                WS-STATUS WITH FOREGROUND-COLOR 11
052510               ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
052520     IF WS-STATUS NOT = "94" 
052530         DISPLAY "Key    " AT 1412 
052540                  WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
052550         DISPLAY "Reverse backup or contact program Support"
052560                  AT 1612.
052570         DISPLAY "Please make a note of these details" AT 1812.
052580       STOP RUN.
