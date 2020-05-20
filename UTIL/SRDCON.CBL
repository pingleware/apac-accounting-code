      $set LINKCOUNT"320"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	 ******	   *****   **	 **    *
      *   **	**  **	  **  **    **	**    **  **   **  ***	 **    *
      *   **	    **	 **   **    **	**	  **   **  ****  **    *
      *    ******   ******    **    **	**	  **   **  ** ** **    *
      * 	**  **	 **   **    **	**	  **   **  **  ****    *
      *   **	**  **	  **  **    **	**    **  **   **  **	***    *
      *    ******   **	  **  *******	 ******    *****   **	 **    *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *  A D J U S T M E N T   C A R D E X   F I L E   C O N V E R T   *
      *                                                                *
      *     VERSION 8.09 - July 2003				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     SRDCON.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   JULY 2003.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2003 - 2004
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.(Id No. 4412165050082).

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080                CONSOLE IS CRT.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

000140 COPY PARAM.SL.

       COPY STKDEX.SL.

000180	   SELECT STXOLD  ASSIGN DISK
                          LOCK EXCLUSIVE
000190                    STATUS WS-STATUS
000200                    ACCESS SEQUENTIAL
000210                    ORGANIZATION SEQUENTIAL.
      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000290 COPY CONTROL.FDE.

000290 COPY PARAM.FDE.

000140 COPY STKDEX.FDE.

      /
000010 FD  STXOLD    LABEL RECORD STANDARD
000020		     VALUE OF FILE-ID W02-STXOLD
000220		     RECORDING MODE IS VARIABLE.
000220 01  SXO-REC1.
000230	   03  SXO-KEY.
	       05  SXO-EXT-CODE.
		 07  SXO-CODE  PIC  X(14).
		 07  SXO-ITM   PIC  X(03).
		 07  FILLER    PIC  X(01).
	       05  SXO-DTE     PIC  9(08)    COMP.
	       05  SXO-SEQ     PIC  9(02)    COMP.
	   03  SXO-REF.
	       05  SXO-USER    PIC  9(03)    COMP-3.
	       05  SXO-PROG    PIC  X(06).
	   03  SXO-WS	       PIC  9(03).
000250	   03  SXO-QNT	       PIC S9(07)V99 COMP-3.
	   03  SXO-COST        PIC S9(07)V99 COMP-3.
	   03  SXO-SELL        PIC S9(07)V99 COMP-3.
	   03  SXO-REMARKS     PIC  X(20).

      /
000430 WORKING-STORAGE SECTION.
000440 77  WS-S1            PIC 9(04)     COMP-5.
001890 77  WS-S2	   PIC	9(04)	 COMP-5.
001890 77  WS-S3           PIC  9(04)    COMP-5.
001890 77  WS-S4	   PIC	9(04)	 COMP-5.
001890 77  WS-S6           PIC  9(04)    COMP-5.
000440 77  WS-SUB	    PIC 9(04)	  COMP-5.
003290 77  WS-NETKEY        PIC 9(04)     COMP-5 VALUE 1.
003340 77  WS-PARKEY	    PIC 9(04)	  COMP-5 VALUE 1.
000520 77  WS-RECOVER	    PIC 9(05)	  COMP-0.
000530 77  WS-TRANS         PIC 9(05)     COMP-0 VALUE 1.
000540 77  WS-EKEY          PIC Z(04)9.
000560 77  WS-ERROR         PIC 9(01)     VALUE ZERO.
000570 77  WS-PROG          PIC X(02).
       77  WS-OPTION        PIC X(01).
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC  X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC  X(01).
               05  WS-H-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC  X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC  X(01) OCCURS 80.
002450     03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC  X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC  X(01).
               05  WS-F-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC  X(01) OCCURS 80.

000590 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

004000 01  W02-FID.

       COPY APACFIDS.ID.

000290 COPY CONTROL.ID.

       COPY PARAM.ID.

000140 COPY STKDEX.ID.

000160 COPY STXOLD.ID.

       COPY W12.WS.

       COPY W40.WS.

       COPY FUNCTION.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
000850 SCREEN SECTION.

       COPY BLANK.CRT.

000860 01  S00.
000910	   03  LINE  2 COLUMN 23 FOREGROUND-COLOR 15
000920		       VALUE "ADJUSTMENTS FILE CONVERTION PROGRAM".
000930     03  LINE  4 COLUMN  2 VALUE "Reading :".
000950     03  LINE  6 COLUMN  2 VALUE "Writing :".
001080 01  S04.
001090     03  LINE 12 COLUMN 12 FOREGROUND-COLOR 11
001100		       VALUE "Convertion of ADJUSTMENT file completed".
001110     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001120                 VALUE "Press any key to continue".
001130 01  S05.
001140     03  LINE 12 COLUMN 12 FOREGROUND-COLOR 11
001150		       VALUE "Convertion unsuccessfull !".
001160     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001170                 VALUE "Reload files from last backup and rekey al
001180-                      "l updates and transactions".
001190     03  LINE 16 COLUMN 12 FOREGROUND-COLOR 11
001200                 VALUE "since creating the last backup disk/s".

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
001220 AA000	       SECTION.
001230 AA00.
041220       MOVE 1 TO WS-S1.
021870	     MOVE SPACES TO WS-MID-LNE.

041240 AA02.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO AA02.
             MOVE WS-G9  TO WS-TCH  (1).
             MOVE WS-G10 TO WS-TCH  (80).
             MOVE WS-G11 TO WS-BCH  (1).
             MOVE WS-G12 TO WS-BCH  (80).
             MOVE WS-G14 TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13 TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2  TO WS-TCHR (16) WS-TCHR (41)
021940                      WS-TCHR (64) WS-TCHR (71).
021950       MOVE WS-G3  TO WS-MCHR (16) WS-MCHR (41)
021960                      WS-MCHR (64) WS-MCHR (71)
			    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4  TO WS-BCHR (16) WS-BCHR (41)
021980                      WS-BCHR (64) WS-BCHR (71).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
001250	     DISPLAY CLR-SCREEN.
001270       PERFORM ZA000.
           IF WS-ERROR = 9
               GO TO AA50.
001260       DISPLAY S00.

001340 AA05.
001350	     READ STXOLD
               AT END
               GO TO AA10.
	     DISPLAY SXO-EXT-CODE AT 0412 WITH FOREGROUND-COLOR 11
		     " " SXO-DTE WITH FOREGROUND-COLOR 11.
001600       PERFORM AW000.
             GO TO AA05.

001450 AA10.
044650	     MOVE 3		 TO WS-NETKEY.
044660       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
	     MOVE "8.09"	 TO STK-VER.
	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
001830	     CLOSE STKDEX
		   STXOLD.
001910       DISPLAY S04.
             ACCEPT WS-OPTION AT 1439 WITH FOREGROUND-COLOR 15 AUTO.
004990	     CALL "CBL_DELETE_FILE" USING W02-STXOLD
005030				    RETURNING WS-STATUS.

       AA15.
001920       EXIT PROGRAM.

001930 AA50.
001940       DISPLAY S05.
001950       STOP RUN.

       COPY LOCKED.RC2.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY CONTROL.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.

       COPY CONTROL.WR.

      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
001960 AW000           SECTION.
011840 AW01.
	     MOVE SXO-EXT-CODE	 TO STX-EXT-CODE.
	     MOVE SXO-DTE	 TO STX-DTE.
	     MOVE SXO-SEQ	 TO STX-SEQ.
	     MOVE SXO-DTE	 TO STX-GRN.
	     MOVE SXO-USER	 TO STX-USER.
	     MOVE SXO-PROG	 TO STX-PROG.
	     MOVE SXO-WS	 TO STX-WS.
000250	     MOVE SXO-QNT	 TO STX-QNT.
	     MOVE SXO-COST	 TO STX-COST.
	     MOVE SXO-SELL	 TO STX-SELL.
	     MOVE SXO-REMARKS	 TO STX-REMARKS.
	     ACCEPT WS-STIME FROM TIME.
	     MOVE WS-STIME	 TO STX-TIME.
	     DISPLAY STX-EXT-CODE AT 0612 WITH FOREGROUND-COLOR 14
		     " " STX-DTE WITH FOREGROUND-COLOR 14.
002310	     WRITE STX-REC1.
           IF WS-STAT1 NOT = "0"
	       MOVE 46		 TO WS-F-ERROR
               PERFORM WRITE-ERROR.

002440 AW999.
002450       EXIT.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

      /
002460 ZA000           SECTION.
002470 ZA00.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
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
	     MOVE "STKDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKDEX.
	     MOVE "STXOLD"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STXOLD.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
004990	     CALL "CBL_RENAME_FILE" USING W02-STKDEX
005000					  W02-STXOLD
005030				    RETURNING WS-STATUS.
	   IF NOT (WS-STATUS = X"0000")
	       MOVE "Unable to Convert ADJUSTMENT file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       MOVE 9		 TO WS-ERROR
	       GO TO ZA999-EXIT.
042240	     OPEN INPUT STXOLD.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 86		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
042240	     OPEN OUTPUT STKDEX.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 46		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
002960       GO TO ZA999-EXIT.
020110 ZA200.
020120       DISPLAY "Files locked - Try later" AT 2412
020130                WITH FOREGROUND-COLOR 14.
020140       DISPLAY "Press any key" AT 2512 
020150		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
020160	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     MOVE 9		 TO WS-ERROR.
003080 ZA999-EXIT.
003090       EXIT.
      /
003100 ZB000-ERROR           SECTION.
      
041480 OPEN-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041480 READ-ERROR.
041490       DISPLAY CLR-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041520 WRITE-ERROR.
041530       DISPLAY CLR-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK TO WS-FILE
041630         MOVE WS-NETKEY TO WS-KEY
041640     ELSE
041650	   IF WS-F-ERROR = 46
041660	       MOVE W02-STKDEX TO WS-FILE
041670         MOVE ZERO       TO WS-KEY
041640     ELSE
041650	   IF WS-F-ERROR = 86
041660	       MOVE W02-STXOLD TO WS-FILE
041670         MOVE ZERO       TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY.
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
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
