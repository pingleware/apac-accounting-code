      ******************************************************************
      *                                                                *
      *    ******   **	      ********	*******  **    **  ********    *
      *   **	**  **		 **	**	  **  **      **       *
      *   **	    **		 **	**	   ****       **       *
      *   **	    **		 **	*****	    **	      **       *
      *   **   ***  **		 **	**	   ****       **       *
      *   **	**  **		 **	**	  **  **      **       *
      *    ******   ********	 **	*******  **    **     **       *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   G / L   E X T R A C T   T R A N S A C T I O N S   T O   A    *
      *   T E M P O R A R Y   F I L E	A N D	D E L E T E	       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     GLTEXT.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   SEPTEMBER 1992.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1992 - 2008
				     by James William Lemmon.
				       (Id No. 4412165050082).

		   All rights reserved.

		   e-mail jwlemmon@gmail.com.

000050 SECURITY.
		   This program is free software; you can redistribute
		   it and/or modify it under the terms of the GNU General
		   Public License as published by the Free Software
		   Foundation; either version 2 of the License, or (at
		   your option) any later version.

		   This program is distributed in the hope that it will
		   be useful, but WITHOUT ANY WARRANTY; without even the
		   implied warranty of MERCHANTABILITY or FITNESS FOR A
		   PARTICULAR PURPOSE.	See the GNU General Public License
		   for more details.

		   You should have received a copy of the GNU General
		   Public License along with this program; if not, write
		   to the Free Software Foundation, Inc., 59 Temple Place
		   - Suite 330, Boston, MA 02111-1307, USA.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080                CONSOLE IS CRT.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

000140 COPY PARAM.SL.

000160 COPY GACCNT.SL.

       COPY GLTRAN.SL.

      $SET KEYCOMPRESS "7"
000210	   SELECT GTREXT ASSIGN DISK
                         LOCK AUTOMATIC
000220                   STATUS WS-STATUS
000230                   ACCESS DYNAMIC
000240                   ORGANIZATION INDEXED
000250			 RECORD TX-KEY
			 ALTERNATE RECORD TX-AKEY DUPLICATES
			 ALTERNATE RECORD TX-VKEY =
			     TX-AC TX-VAL TX-DATE DUPLICATES.
      $SET NOKEYCOMPRESS
      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FD.

000290 COPY CONTROL.IFD.

000290 COPY PARAM.IFD.

000300 COPY GACCNT.FDE.

000140 COPY GLTRAN.FD.

      /
002000 FD  GTREXT    LABEL RECORD IS STANDARD
002010		     VALUE OF FILE-ID IS W02-GTREXT.
002020 01  TX-REC.
	   03  TX-KEY.
002160	       05  TX-AC       PIC  9(06).
002030	       05  TX-DATE     PIC  9(08).
002040	       05  TX-DTE REDEFINES TX-DATE.
002050		   07  TX-CC   PIC  9(02).
002050		   07  TX-YY   PIC  9(02).
002060		   07  TX-MM   PIC  9(02).
002070		   07  TX-DD   PIC  9(02).
	       05  TX-AKEY.
      *
      *     01 OR 02 - Journal Entries
      *     03 OR 04 - Cheques
      *     06 OR 07 - Deposits
      *     08 OR 09 - Electronic Funds
      *
002120		   07  TX-CODE PIC  9(02).
		   07  TX-REFER.
002110		       09  TX-REF
                               PIC  9(07).
		   07  TX-SEQ  PIC  9(02).
	       05  TX-SUB      PIC  9(01).
002080	   03  TX-NAR	       PIC  X(40).
002090	   03  TX-DESC REDEFINES TX-NAR.
002100	       05  TX-NARATIVE PIC  X(36).
               05  FILLER      PIC  9(07)    COMP-3.
002130	   03  TX-VAL	       PIC S9(09)V99 COMP-3.
002170	   03  TX-FLAG	       PIC  X(01).
	   03  TX-BATCH        PIC  9(06)    COMP-3.
	   03  FILLER	       PIC  X(03).

      /
000430 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000440 77  WS-S1	   PIC	9(04)	 COMP-5.
001890 77  WS-S2	   PIC	9(04)	 COMP-5.
001890 77  WS-S3           PIC  9(04)    COMP-5.
001890 77  WS-S4	   PIC	9(04)	 COMP-5.
001890 77  WS-S6           PIC  9(04)    COMP-5.
000440 77  WS-SUB	   PIC	9(04)	 COMP-5.
004540 77  WS-OPEN	   PIC	9(02)	 VALUE ZERO.
003290 77  WS-NETKEY	    PIC 9(04)	  COMP-5 VALUE 1.
003290 77  WS-DEPKEY        PIC 9(04)     COMP-5 VALUE 1.
003340 77  WS-PARKEY        PIC 9(04)     COMP-5 VALUE 1.
003360 77  WS-RECKEY        PIC 9(04)     COMP-5.
000520 77  WS-RECOVER       PIC 9(05)     COMP-0.
000530 77  WS-TRANS         PIC 9(05)     COMP-0 VALUE 1.
000540 77  WS-EKEY          PIC Z(04)9.
000560 77  WS-ERROR         PIC 9(01)     VALUE ZERO.
000570 77  WS-PROG          PIC X(02).
       77  WS-OPTION        PIC X(01).
       77  WS-USUB	   PIC	9(04) COMP-5.
       77  APAC-VERSION     PIC  X(04) VALUE "7.03".
       77  APAC-SYSTEM	    PIC  X(12) VALUE "-G/L Utility".

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

004000 01  W02-FID.

       COPY APACFIDS.ID.

000290 COPY CONTROL.ID.

000300 COPY GACCNT.ID.

000140 COPY GLTRAN.ID.

000970	   03  W02-GTREXT.
000020	       05  W02-PTH11B  PIC  X(03).
000030	       05  FILLER      PIC  X(17) VALUE "_DATA\GTREXT.DAT".

       COPY PARAM.ID.

       COPY W05.GL.

       01  W10-DATE.
	   03  W10-DTE	       PIC 99/99/9999.

       COPY W12.WS.

       COPY W15.GL.

       01  W16-DATES.
004120	   03  W16-START       PIC 9(08).
004130	   03  W16-SDTE-DMY REDEFINES W16-START.
004140	       05  W16-SDAY    PIC 9(02).
004150	       05  W16-SMNTH   PIC 9(02).
004160	       05  W16-SCENT   PIC 9(02).
004160	       05  W16-SYEAR   PIC 9(02).
004170	   03  W16-S-YMD       PIC 9(08).
004180	   03  W16-STRT-YMD REDEFINES W16-S-YMD.
004190	       05  W16-SCC     PIC 9(02).
004190	       05  W16-SYY     PIC 9(02).
004200	       05  W16-SMM     PIC 9(02).
004210	       05  W16-SDD     PIC 9(02).
004120	   03  W16-END	       PIC 9(08).
004130	   03  W16-EDTE-DMY REDEFINES W16-END.
004140	       05  W16-EDAY    PIC 9(02).
004150	       05  W16-EMNTH   PIC 9(02).
004160	       05  W16-ECENT   PIC 9(02).
004160	       05  W16-EYEAR   PIC 9(02).
004170	   03  W16-E-YMD       PIC 9(08).
004180	   03  W16-END-YMD REDEFINES W16-E-YMD.
004190	       05  W16-ECC     PIC 9(02).
004190	       05  W16-EYY     PIC 9(02).
004200	       05  W16-EMM     PIC 9(02).
004210	       05  W16-EDD     PIC 9(02).

       COPY W20.WS.

001570 01  W25-CALCS.
001580	   03  W25-RESULT      PIC  9(09)V99.
001590     03  W25-RESULT1 REDEFINES W25-RESULT.
001600	       05  FILLER      PIC  9(03).
001610	       05  W25-WHOLE   PIC  9(06).
001620	       05  W25-DECIMAL PIC  9(02).
	   03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  FILLER      PIC  9(02).
	       05  W25-KEY     PIC  9(04).
	       05  W25-SUB     PIC  9(01).
	       05  FILLER      PIC  9(02).
001630	   03  W25-PASS.
001640	       05  W25-PASS1   PIC  9(03).
001650	       05  W25-PASS2   PIC  9(03).
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

       COPY CHAIN.WS.

      /
000850 SCREEN SECTION.

       COPY BLANK.CRT.

000860 01  S00.
	   02  S00A.
000910	   03  LINE  2 COLUMN 23 FOREGROUND-COLOR 7 HIGHLIGHT
000920		       VALUE "G/L EXTRACT AND DELETE TRANSACTIONS".
	   02  S00B.
000930     03  LINE  4 COLUMN  2 VALUE "Reading :".
000950     03  LINE  6 COLUMN  2 VALUE "Writing :".

001080 01  S04.
001090	   03  LINE 12 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
001100		       "EXTRACT of G/L TRANSACTIONS completed".
001110	   03  LINE 14 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
001120                 VALUE "Press any key to continue".

001130 01  S05.
001140	   03  LINE 12 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
001150		       VALUE "EXTRACT unsuccessfull !".
001160	   03  LINE 14 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
001170                 VALUE "Reload files from last backup and rekey al
001180-                      "l updates and transactions".
001190	   03  LINE 16 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
001200                 VALUE "since creating the last backup disk/s".

       01  S07.
	   03  LINE 06 COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W16-START AUTO.
	   03  LINE 07 COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W16-END AUTO.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION.
001220 AA000	       SECTION.
001230 AA00.
041220	     MOVE 1		 TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.

041240 AA02.
041250	     MOVE WS-G1 	 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
	     MOVE WS-G8 	 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO AA02.
	     MOVE WS-G9 	 TO WS-TCH  (1).
	     MOVE WS-G10	 TO WS-TCH  (80).
	     MOVE WS-G11	 TO WS-BCH  (1).
	     MOVE WS-G12	 TO WS-BCH  (80).
	     MOVE WS-G14	 TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE WS-G13	 TO WS-TCHR (80) WS-BCHR (80).
021930	     MOVE WS-G2 	 TO WS-TCHR (16) WS-TCHR (41)
021940				    WS-TCHR (64) WS-TCHR (71).
021950	     MOVE WS-G3 	 TO WS-MCHR (16) WS-MCHR (41)
021960				    WS-MCHR (64) WS-MCHR (71)
				    WS-MCHR (1)  WS-MCHR (80).
021970	     MOVE WS-G4 	 TO WS-BCHR (16) WS-BCHR (41)
021980				    WS-BCHR (64) WS-BCHR (71).
	     MOVE "SupervisorÄ"  TO WS-WRKHD.
001250	     DISPLAY CLR-SCREEN.
001270       PERFORM ZA000.
           IF WS-ERROR = 9
               GO TO AA50.
001260	     DISPLAY S00A.

001340 AA05.
	     MOVE W05-TODAY	 TO W16-END W16-START.
	     MOVE 01		 TO W16-SDAY.
	   IF W16-SMNTH > 01
	       SUBTRACT 1	 FROM W16-SMNTH
	   ELSE
	       MOVE 12		 TO W16-SMNTH
	       IF W16-SYEAR = ZERO
		   MOVE 99	 TO W16-SYEAR
		   SUBTRACT 1	 FROM W16-SCENT
	       ELSE
		   SUBTRACT 1	 FROM W16-SYEAR.
	     DISPLAY "For the period." AT 0412.
	     DISPLAY "Start date :" AT 0612.
	     DISPLAY "End date   :" AT 0712.
	     DISPLAY "Dates to be keyed in as DDMMCCYY" AT 0912
                      WITH FOREGROUND-COLOR 11.

       AA07.
             ACCEPT S07.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE 9	 TO WS-ERROR
		     GO TO AA08
		 WHEN OTHER
		     CALL X"E5"
		     GO TO AA07
	       END-EVALUATE.
	     PERFORM CHECK-CORRECT.
018520	   IF WS-OPTION = "N"
018530	       GO TO AA07.
	     MOVE W16-START	 TO W10-DTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Start date" TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
	       GO TO AA07.
	     MOVE W20-DTE	 TO W16-S-YMD W15-YMD.
      *	     MOVE 01		 TO W15-DY.
	     MOVE W16-END	 TO W10-DTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "End date"	 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
	       GO TO AA07.
	     MOVE W20-DTE	 TO W16-E-YMD.
001250	     DISPLAY CLR-SCREEN.
001260	     DISPLAY S00.

       AA08.
	     MOVE 100000	 TO G-AC.
	     PERFORM START-AT-LEDG-AC THRU READ-GACCNT-EXIT.
	   IF WS-F-ERROR = 8
	       GO TO AA20.

019990 AA10.
	     PERFORM READ-GACCNT-NEXT THRU READ-GACCNT-EXIT.
020000	   IF WS-F-ERROR = 8
020010	       GO TO AA20.
	     INITIALIZE T-REC.
	     MOVE G-AC		 TO T-AC.
	     MOVE W15-YMD	 TO T-DATE.
             PERFORM START-AT-GLTRAN-KEY THRU READ-GLTRAN-EXIT.
           IF WS-F-ERROR = 11
	       GO TO AA10.

       AA15.
020210	     PERFORM READ-GLTRAN-NEXT THRU READ-GLTRAN-EXIT.
	     DISPLAY T-KEY AT 0412 WITH FOREGROUND-COLOR 15.
020190     IF (WS-F-ERROR = 11) OR 
	      (G-AC NOT = T-AC) OR
	      (T-DATE > W16-E-YMD)
020200	       GO TO AA10.
	     MOVE T-REC 	 TO TX-REC.
	     WRITE TX-REC.
	     DISPLAY TX-KEY AT 0612 WITH FOREGROUND-COLOR 14.
	     PERFORM DELETE-GLTRAN-REC THRU WRITE-GLTRAN-EXIT.
	     GO TO AA15.

       AA20.
001830	     CLOSE GACCNT
		   GLTRAN
		   GTREXT
                   PARAM
001870             NETWORK.
001910       DISPLAY S04.
             ACCEPT WS-OPTION AT 1439 WITH FOREGROUND-COLOR 15 AUTO.

       AA25.
001920	     STOP RUN.

001930 AA50.
001940       DISPLAY S05.
001950       STOP RUN.

009820 AB000	       SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

       COPY GACCNT.RD1.

       COPY GLTRAN.RD.

008790 AC000	       SECTION.

       COPY GLTRAN.WR.

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.
      *
      *   D A T E   C H E C K   R O U T I N E
      *
      *   PERFORM THIS ROUTINE WITH THE DATE IN W10-DATE (PIC 99/99/9999)
      *   WITH THE FORMAT AS DD/MM/CCYY
      *
      *   ON EXITING - WS-ERROR WILL BE ZERO (0) IF THE DATE IS CORRECT
      *                OR ONE (1) IF THE DATE IS INCORRECT.
      *
008380 CHECK-DATE         SECTION.
      
008390 DATE-CHECK.
008400       UNSTRING W10-DATE DELIMITED BY "/" OR " "
008410	       INTO W20-DD, W20-MM, W20-CCYY
008420         OVERFLOW GO TO DATE-ERROR.
008430       MOVE ZERO           TO WS-ERROR.
008440     IF W20-DATE NOT NUMERIC
008450         GO TO DATE-ERROR.
008470     IF W20-DAY < 01 OR > 31
008480         GO TO DATE-ERROR.
008490     IF W20-MONTH < 01 OR > 12
008500         GO TO DATE-ERROR.
008510     IF W20-MONTH = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
008520         GO TO CHECK-DATE-EXIT.
008530     IF W20-MONTH = 02 AND W20-DAY > 28
008540         GO TO LEAP-YEAR.
008550     IF W20-DAY > 30
008560         GO TO DATE-ERROR.
008570       GO TO CHECK-DATE-EXIT.
      
008580 LEAP-YEAR.
008590     IF W20-DAY > 29
008600         GO TO DATE-ERROR.
008610	     DIVIDE W20-CNYR BY 4 GIVING W25-RESULT.
008620     IF W25-DECIMAL = 0
008630         GO TO CHECK-DATE-EXIT.
      
008640 DATE-ERROR.
008650       MOVE 1              TO WS-ERROR.
      
008660 CHECK-DATE-EXIT.
008670       EXIT.


      /
002460 ZA000           SECTION.
002470 ZA00.
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
044150	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ D o   n o t   d i s p l a y   E R R O R   m e s s a g e s ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   S e t   u p	 t h e	 F U N C T I O N   k e y s     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       ESC, F1 to F10 keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    PAGE-UP and PAGE-DOWN keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   A C T I V A T E   M O U S E		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Set MOUSE key to act as FUNCTION key	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
	     DISPLAY "EXTRACT G/L TRANSACTIONS" AT 0229
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    **** 	  P A R A M E T E R   F I L E		    ****
      *    *************************************************************
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
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
	     MOVE "GLTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLTRAN.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
002660	     MOVE W02-SYSID	 TO W02-PTH11B.
002480	     OPEN I-O PARAM.
042120     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042130             GO TO ZA200.
002490     IF WS-STATUS NOT = "00"
002500	       MOVE 15		 TO WS-F-ERROR
002510         PERFORM OPEN-ERROR.
002520	     MOVE 1		 TO WS-PARKEY.
             READ PARAM.
002540     IF WS-STATUS NOT = "00"
002550	       MOVE 15		 TO WS-F-ERROR
002560         PERFORM READ-ERROR.
034400	     MOVE PAR-DMY	 TO W05-TODAY.
034410	     MOVE PAR-YMD	 TO W05-TYMD.
002520	     MOVE 3		 TO WS-PARKEY.
002530       READ PARAM.
002540     IF WS-STATUS NOT = "00"
002550	       MOVE 15		 TO WS-F-ERROR
002560	       PERFORM READ-ERROR.
042140       OPEN I-O NETWORK.
042150     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042160             GO TO ZA200.
003050	   IF NOT (WS-STATUS = "00" OR "41")
003060	       MOVE 2		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
	     OPEN INPUT GACCNT.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042030		   GO TO ZA200.
035500     IF WS-STATUS NOT = "00"
035510	       MOVE 8		 TO WS-F-ERROR
050480	       PERFORM OPEN-ERROR.
042240	     OPEN OUTPUT GTREXT.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 61		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
042240	     OPEN I-O GLTRAN.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060         MOVE 9            TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
	     MOVE 4		 TO WS-NETKEY.
034450       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
	   IF NOT (G-OPEN = ZERO)
034510	       MOVE G-OPEN	 TO WS-OPEN
	   ELSE
	       MOVE 3		 TO WS-OPEN.
002960	     GO TO ZA999-EXIT.

020110 ZA200.
020120       DISPLAY "Files locked - Try later" AT 2412
020130                WITH FOREGROUND-COLOR 14.
020140       DISPLAY "Press any key" AT 2512 
020150		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
020160	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     MOVE 9 TO WS-ERROR.

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
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041640	   ELSE
041810     IF WS-F-ERROR = 8
041820	       MOVE W02-GACCNT	 TO WS-FILE
041830	       MOVE ZERO	 TO WS-KEY
	       MOVE G-AC	 TO WS-KEYX
041640	   ELSE
041650     IF WS-F-ERROR = 11
041660         MOVE W02-GLTRAN   TO WS-FILE
041670         MOVE ZERO         TO WS-KEY
               MOVE T-KEY        TO WS-KEYX
041640     ELSE
041650     IF WS-F-ERROR = 61
041660	       MOVE W02-GTREXT	 TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE W02-PARAM	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY.
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
042170       STOP RUN.
