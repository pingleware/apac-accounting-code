      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   ********  **	   **  **    **  **    **  *******    ******   *
      *      **     ***    **  **    **  **    **  **	 **  **    **  *
      *      **     ** *   **  **    **  **    **  **	 **  **        *
      *      **     **	*  **  **    **  ********  **	 **  **   ***  *
      *      **     **	 * **	**  **	 **    **  **	 **  **    **  *
      *      **     **	  ***	 ****	 **    **  **	 **  **    **  *
      *   ********  **	   **	  **	 **    **  *******    ******   *
      * 							       *
      *     ENGLISH                                                    *
      *                                                                *
      *     P R I N T	I N V O I C E	H E A D I N G		       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     INVHDG.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2008
				     by James William Lemmon.

		   All rights reserved.

		   e-mail: jwlemmon@gmail.com.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon (Id No. 4412165050082).

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
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
002130 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002180 77  WS-S1	   PIC	9(04)	 COMP-5.
002190 77  WS-S2           PIC  9(04)    COMP-5.
002200 77  WS-S3           PIC  9(04)    COMP-5.
002210 77  WS-S4           PIC  9(04)    COMP-5.
002220 77  WS-S5           PIC  9(04)    COMP-5.
002230 77  WS-S6           PIC  9(04)    COMP-5.
002240 77  WS-S7           PIC  9(04)    COMP-5.
002250 77  WS-S8           PIC  9(04)    COMP-5.
002260 77  WS-S9           PIC  9(04)    COMP-5.
002320 77  WS-PARKEY	   PIC	9(04)	 COMP-5.
002520 77  WS-PORT         PIC  9(05) VALUE 1004.
002530 77  WS-OPEN         PIC  X(01) VALUE X"01".
002540 77  WS-CLOSE        PIC  X(01) VALUE X"00".
002750 77  WS-OPTION       PIC  X(01).
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

       01  PRG-NAME.
	   03  FILLER	   PIC	X(04) VALUE "INVF".
	   03  PRG-NUMBER  PIC	9(02) VALUE ZERO.

003050 01  WS-DB-LINE.
003060     03  WS-TOP-LINE.
003070         05  WS-TCHAR PIC X(01) OCCURS 80.
003080     03  WS-TOP-LNE2.
003090         05  WS-TCR  PIC X(01) OCCURS 80.
003100     03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110         05  FILLER      PIC  X(01).
003120         05  WS-TOP-COMP PIC  X(40).
003130         05  FILLER      PIC  X(39).
003140     03  WS-MID-LINE.
003150         05  WS-MCHAR PIC X(01) OCCURS 80.
	   03  WS-MINV-LINE REDEFINES WS-MID-LINE.
	       05  WS-MIV1     PIC  X(01).
	       05  WS-MIV78    PIC  X(78).
	       05  WS-MIV80    PIC  X(01).
003160	   03  WS-MID-LNE2.
003170	       05  WS-MCR  PIC X(01) OCCURS 80.
	   03  WS-MID78 REDEFINES WS-MID-LNE2.
	       05  FILLER      PIC  X(01).
	       05  WS-BLNK78   PIC  X(78).
	       05  FILLER      PIC  X(01).
003180     03  WS-BOT-LINE.
003190         05  WS-BCHAR PIC X(01) OCCURS 80.
003200     03  WS-BOT-LNE2.
003210         05  WS-BCR  PIC X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
003220     03  WS-TOP-LNE.
003230         05  WS-TCHR PIC X(01) OCCURS 80.
003240     03  WS-T-LINE REDEFINES WS-TOP-LNE.
003250         05  FILLER  PIC X(01).
003260         05  WS-H-LINE
003270                     PIC X(78).
003280         05  FILLER  PIC X(01).
003290     03  WS-MID-LNE.
003300         05  WS-MCHR PIC X(01) OCCURS 80.
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
003310	   03  WS-BOT-LNE.
003320         05  WS-BCHR PIC X(01) OCCURS 80.
003330     03  WS-TP-LNE.
003340         05  WS-TCH  PIC X(01) OCCURS 80.
003350     03  WS-MD-LNE.
003360         05  WS-MCH  PIC X(01) OCCURS 80.
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
003370	   03  WS-BT-LNE.
003380	       05  WS-BCH      PIC X(01) OCCURS 80.
           03  WS-MM-LINE.
	       05  FILLER      PIC  X(01) VALUE X"B3".
	       05  WS-SPACE    PIC  X(60) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

003480 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002040 01  W02-FID.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

       COPY W12.WS.

       COPY W40.DBT.

       COPY W40.WS.

       COPY FUNCTION.WS.

005610 COPY W50.WS.

       COPY W90.DBT.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
007190 SCREEN SECTION.

       COPY BLANK.CRT.

007800 01  S05.
007810     03  LINE 12 COLUMN 12 VALUE "Enter Password >      <".

007820 01  S06.
007830     03  LINE 12 COLUMN 28 FOREGROUND-COLOR 15 PIC  X(06)
007840                           USING W40-CSHPASS AUTO SECURE.

009900 01  S29.
009910     03  LINE 12 COLUMN 24 FOREGROUND-COLOR 15
009920                           PIC Z(06)9.99 USING PAR-FLOAT AUTO.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010230 AA000-MAIN	       SECTION.
010240 AA000-INIT.
010250	     PERFORM ZA000-INIT.
042770	     PERFORM FA000.
013280 AZ100.
013290       EXIT PROGRAM.

       COPY ERROR.SCR.

       COPY FUNCTION.SCR.

010930 COPY LOCKED.RC2.

      /
      *    ****    READ FILES ROUTINES
      *
011110 AC000              SECTION.

011270 COPY PARAM.RD.

      /
      *    ****    INVOICE HEADING - FORMAT SELECTION
      *
050890 FA000		       SECTION 5.
050900 FA00.
	     MOVE "newinv"	 TO W40-ACNO.
	     CALL PRG-NAME USING LS-PARID
				 LS-USER-ID
				 LS0-PROGRAMS
				 LS0-SECURITY
				 W40-COMPANY
				 W90-TRAN
		  ON EXCEPTION
		      GO TO FA05
	     END-CALL.
	     CANCEL PRG-NAME.
	     GO TO FA999.
       FA05.
	     DISPLAY CLEAR-L25.
	     DISPLAY "Program : " AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		      PRG-NAME
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6
			   HIGHLIGHT
		     " not on disk, press "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		     "ANY" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6
				HIGHLIGHT
		     " key to continue"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
051380 FA999.
051390       EXIT.

      /
052700 ZA000-INIT	       SECTION 9.
052710 ZA000-OPEN.
052720       PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
052730	     DISPLAY CLR-SCREEN.
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
      *	     MOVE 44		 TO USER-NUMBER.
      *	     MOVE 1		 TO USER-SETTING.
      *	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
      *    PRE-DISPLAY ACCEPT FIELDS BEFORE DATA-ENTRY
      *
      *      MOVE 76		 TO USER-NUMBER.
      *      MOVE 3		 TO USER-SETTING.
      *      MOVE 1		 TO USER-KEYS.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
      *      CALL CBL_INIT_MOUSE USING MOUSE-HANDLE
      *				       MOUSE-BUTTONS
      *				 RETURNING MOUSE-STATUS.
      *    IF MOUSE-STATUS = ZERO
      *        MOVE "Y"		 TO MOUSE-ATTACHED.
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
052980	     MOVE 3		 TO WS-PARKEY.
052990       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-INV	 TO PRG-NUMBER.
054760	     MOVE 1		 TO WS-PARKEY.
054770       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
054780       MOVE PAR-DMY        TO W12-TODAY.
054790       MOVE PAR-YMD        TO W12-T-YMD.
055260	     GO TO ZA999.
055270 ZA49.
055280       DISPLAY "Too many files OPEN" AT 0812
055290                WITH FOREGROUND-COLOR 14.
055300       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
055310       DISPLAY "Press any key to continue" AT 1212.
055320       ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
055330       GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
055500 ZA55.
055510       MOVE 1 TO WS-S1.
055520       MOVE SPACES TO WS-MID-LINE WS-MID-LNE WS-MID-LNE2.
055530 ZA60.
055540       MOVE WS-G1          TO WS-TCHAR (WS-S1) WS-BCHAR (WS-S1).
055550       MOVE WS-G1          TO WS-TCHR (WS-S1)  WS-BCHR (WS-S1).
055560       MOVE WS-G1          TO WS-TCH (WS-S1)   WS-BCH (WS-S1).
055570       MOVE WS-G8          TO WS-TCR (WS-S1)   WS-BCR (WS-S1).
055580     IF WS-S1 < 80
055590         ADD 1 TO WS-S1
055600         GO TO ZA60.
055610       MOVE WS-G9          TO WS-TCR   (1).
055620       MOVE WS-G10         TO WS-TCR   (80).
055630       MOVE WS-G11         TO WS-BCR   (1).
055640       MOVE WS-G12         TO WS-BCR   (80).
055650       MOVE WS-G14         TO WS-TCHAR (1)  WS-BCHAR (1)
055660                              WS-TCHR  (1)  WS-BCHR  (1)
055670                              WS-TCH   (1)  WS-BCH   (1).
055680       MOVE WS-G13         TO WS-TCHAR (80) WS-BCHAR (80)
055690                              WS-TCHR  (80) WS-BCHR  (80)
055700                              WS-TCH   (80) WS-BCH   (80).
055710       MOVE WS-G2          TO WS-TCHAR (4)  WS-TCHAR (19)
055720                              WS-TCHAR (50) WS-TCHAR (59) 
055730                              WS-TCHAR (69)
055740                              WS-TCHR  (18) WS-TCHR  (49)
055750                              WS-TCHR  (60) WS-TCHR  (70)
055760                              WS-TCH   (8)  WS-TCH   (49)
055770                              WS-TCH   (61) WS-TCH   (72).
055780       MOVE WS-G3          TO WS-MCHAR (4)  WS-MCHAR (19) 
055790                              WS-MCHAR (50) WS-MCHAR (59) 
055800                              WS-MCHAR (69) WS-MCHAR (80)
055810                              WS-MCHAR (1)
055820                              WS-MCH   (8)  WS-MCH   (49)
055830                              WS-MCH   (61) WS-MCH   (72)
055840                              WS-MCR   (1)  WS-MCR   (80)
055850                              WS-MCHR  (18) WS-MCHR  (49)
055860                              WS-MCHR  (60) WS-MCHR  (70)
055870                              WS-MCHR  (1)  WS-MCHR  (80) 
055880                              WS-MCH   (1)  WS-MCH   (80).
055890       MOVE WS-G4          TO WS-BCHAR (4)  WS-BCHAR (19) 
055900                              WS-BCHAR (50) WS-BCHAR (59) 
055910                              WS-BCHAR (69)
055920                              WS-BCHR  (18) WS-BCHR  (49)
055930                              WS-BCHR  (60) WS-BCHR  (70)
055940                              WS-BCH   (8)  WS-BCH   (49)
055950                              WS-BCH   (61) WS-BCH   (72).
055960	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
055970 ZA200.
055980       DISPLAY "Files locked - Try later" AT 2312
055990                WITH FOREGROUND-COLOR 14
056000               " " WS-STATUS WITH FOREGROUND-COLOR 15.
056010       DISPLAY "Press any key" AT 2512 
056020		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
056030	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
056040 ZA205.
056050       EXIT PROGRAM.
056060 ZA999.
056070       EXIT.

056090 I-O-ERRORS      SECTION	9.
056100 OPEN-ERROR.
056110       DISPLAY CLR-SCREEN.
056120       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
056130       PERFORM DISPLAY-FILE-NAME.
056140
056150 READ-ERROR.
056160       DISPLAY CLR-SCREEN.
056170       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
056180       PERFORM DISPLAY-FILE-NAME.
056190
056200 WRITE-ERROR.
056210       DISPLAY CLR-SCREEN.
056220       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
056230       PERFORM DISPLAY-FILE-NAME.
056240

056260 DISPLAY-FILE-NAME.
056510	   IF WS-F-ERROR = 15
056520         MOVE WS-PARID     TO WS-FILE
056530	       MOVE WS-PARKEY	 TO WS-KEY.
056740     IF WS-STATUS = "10"
056750         MOVE "End of FILE" TO WS-STAT-MESSAGE
056760     ELSE
056770     IF WS-STATUS = "22"
056780         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
056790     ELSE
056800     IF WS-STATUS = "23"
056810         MOVE "Invalid record number" TO WS-STAT-MESSAGE
056820     ELSE
056830     IF WS-STATUS = "24"
056840         MOVE "DISK full" TO WS-STAT-MESSAGE
056850     ELSE
056860     IF WS-STATUS = "30"
056870         MOVE "DISK error" TO WS-STAT-MESSAGE
056880     ELSE
056890     IF WS-STATUS = "94"
056900         MOVE "FILE locked" TO WS-STAT-MESSAGE.
056910       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
056920       DISPLAY "Status " AT 1212 
056930                WS-STATUS WITH FOREGROUND-COLOR 11
056940               ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
056950     IF WS-STATUS NOT = "94" 
056960         DISPLAY "Key    " AT 1412 
056970                  WS-KEY WITH FOREGROUND-COLOR 11
056980         DISPLAY "Reverse backup or contact program Support"
056990                  AT 1612.
057000         DISPLAY "Please make a note of these details" AT 1812.
057010       STOP RUN.
