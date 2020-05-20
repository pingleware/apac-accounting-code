      $set LINKCOUNT"248"
      ******************************************************************
      *                                                                *
      *    ******   *******   **    **  *******   *******  *******     *
      *   **    **  **    **  **    **  **    **  **       **    **    *
      *   **        **    **  **    **  **   **   **       **    **    *
      *   **        *******   ********  ******    *****    *******     *
      *   **   ***  **    **  **    **  **   **   **       **    **    *
      *   **    **  **    **  **    **  **    **  **       **    **    *
      *    ******   *******   **    **  **    **  *******  *******     *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   G / L   B A T C H   F I L E   R E B U I L D   P R O G R A M  *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      *								       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     GBHREB.
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
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

000140 COPY PARAM.SL.

      $SET KEYCOMPRESS "7"
000210	   SELECT GLBHDR  ASSIGN DISK
			  LOCK EXCLUSIVE
000220			  STATUS WS-STATUS
000230			  ACCESS DYNAMIC
000240			  ORGANIZATION INDEXED
000250			  RECORD GBH-KEY.

000210	   SELECT GLBTCH  ASSIGN DISK
                          LOCK EXCLUSIVE
000220                    STATUS WS-STATUS
000230                    ACCESS DYNAMIC
000240                    ORGANIZATION INDEXED
000250                    RECORD GLB-KEY.
      $SET NOKEYCOMPRESS

000180	   SELECT GBHOLD  ASSIGN DISK
                          LOCK EXCLUSIVE
000190                    STATUS WS-STATUS
000200                    ACCESS SEQUENTIAL
000210                    ORGANIZATION SEQUENTIAL.

000180	   SELECT GLBOLD  ASSIGN DISK
                          LOCK EXCLUSIVE
000190                    STATUS WS-STATUS
000200                    ACCESS SEQUENTIAL
000210                    ORGANIZATION SEQUENTIAL.

      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000290 COPY CONTROL.IFD.

000290 COPY PARAM.IFD.

000140 COPY GLBTCH.FD.

000160 COPY GLBOLD.FD.

       COPY GLBHDR.FD.

       COPY GBHOLD.FD.

      /
000430 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000440 77  WS-S1	    PIC 9(04)	  COMP-5.
001890 77  WS-S2           PIC  9(04)    COMP-5.
001890 77  WS-S3           PIC  9(04)    COMP-5.
001890 77  WS-S4	   PIC	9(04)	 COMP-5.
001890 77  WS-S6           PIC  9(04)    COMP-5.
001890 77  WS-IXS1	   PIC	9(04)	 COMP-5.
000440 77  WS-SUB	   PIC	9(04)	 COMP-5.
003290 77  WS-NETKEY	   PIC	9(04)	 COMP-5 VALUE 1.
003290 77  WS-DEPKEY	   PIC	9(04)	 COMP-5 VALUE 1.
003340 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
003360 77  WS-RECKEY	   PIC	9(04)	 COMP-5.
000520 77  WS-RECOVER	   PIC	9(05)	 COMP-0.
000530 77  WS-TRANS	   PIC	9(05)	 COMP-0 VALUE 1.
000540 77  WS-EKEY	   PIC	Z(04)9.
000560 77  WS-ERROR	   PIC	9(01)	 VALUE ZERO.
001970 77  WS-BATCH-ID	   PIC	X(01).
000570 77  WS-PROG	   PIC	X(02).
       77  WS-OPTION	   PIC	X(01).
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

000140 COPY GLBTCH.ID.

000160 COPY GLBOLD.ID.

       COPY GLBHDR.ID.

       COPY GBHOLD.ID.

       COPY PARAM.ID.

       COPY W12.WS.

       01  W03-BATCH.
	   03  W03-BATCH-DESC PIC  X(40).

       COPY W40.WS.

       COPY FUNCTION.WS.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
	   03  W75-BTCH       PIC  X(01)
			      OCCURS 18.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
000850 SCREEN SECTION.

       COPY BLANK.CRT.

000860 01  S00.
000910	   03  LINE  2 COLUMN 26 FOREGROUND-COLOR 15
000920		       VALUE "G/L BATCH FILE REBUILD PROGRAM".
000930     03  LINE  4 COLUMN  2 VALUE "Reading :".
000950     03  LINE  6 COLUMN  2 VALUE "Writing :".

000860 01  S00H.
000910     03  LINE  2 COLUMN 23 FOREGROUND-COLOR 15
000920		       VALUE "G/L BATCH HEADER FILE REBUILD PROGRAM".
000930     03  LINE  4 COLUMN  2 VALUE "Reading :".
000950     03  LINE  6 COLUMN  2 VALUE "Writing :".

001080 01  S04.
001090     03  LINE 12 COLUMN 12 FOREGROUND-COLOR 11 VALUE 
001100                 "Rebuilt of G/L INTEGRATION file completed".
001110     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001120                 VALUE "Press any key to continue".

001130 01  S05.
001140     03  LINE 12 COLUMN 12 FOREGROUND-COLOR 11
001150                 VALUE "Rebuild unsuccessfull !".
001160     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001170                 VALUE "Reload files from last backup and rekey al
001180-                      "l updates and transactions".
001190     03  LINE 16 COLUMN 12 FOREGROUND-COLOR 11
001200                 VALUE "since creating the last backup disk/s".

       01  S06.
005490	   03  LINE 3 COLUMN 24 FOREGROUND-COLOR 15
005500		      VALUE "G/L (JOURNAL) - REBUILD BATCH FILE".
005510	   03  LINE 5 COLUMN  3 VALUE "Batch Id:".
	   03	      COLUMN 13 FOREGROUND-COLOR 3 HIGHLIGHT
				BACKGROUND-COLOR 5
				PIC  X(21) FROM W02-GLBTCH.
	   03  LINE 6 COLUMN  3 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	       "Key in a one character batch reference code".
	   03  LINE 7 COLUMN  3 VALUE "Descript:".
	   03  LINE 8 COLUMN  3 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	       "Key in up to 40 characters of description".
	   03  LINE 10 COLUMN 3 VALUE "TOTALS".

005570 01  S06A.
	   02  S06AI.
005510	   03  LINE 5 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				BACKGROUND-COLOR 5
				PIC  X(01) USING WS-BATCH-ID AUTO.
	   02  S06AD.
	   03  LINE 7 COLUMN 13 FOREGROUND-COLOR 7 HIGHLIGHT
				BACKGROUND-COLOR 5
				PIC  X(40) USING W03-BATCH-DESC AUTO.
	   02  S06T.
	   03  LINE 10 COLUMN 13 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC Z(08)9.99 FROM GBH-DT.
	   03	       COLUMN 26 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC Z(08)9.99CR FROM GBH-CR.

       01  S-BCH.
	   03  LINE  6 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
	       BACKGROUND-COLOR 7 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
	   03	       COLUMN 75 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "¿".
	   03  LINE  7 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 31 FOREGROUND-COLOR 5
	       BACKGROUND-COLOR 7 VALUE "  B A T C H   I D E N T I T Y
      -	       " L O O K U P ".
	   03	       COLUMN 75 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE  8 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 31 FOREGROUND-COLOR 5
	       BACKGROUND-COLOR 7 VALUE "ÚÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03	       COLUMN 75 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE 21 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "³".
	   03	       COLUMN 31 FOREGROUND-COLOR 5
	       BACKGROUND-COLOR 7 VALUE "ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÙ".
	   03	       COLUMN 75 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
				 VALUE "³".
	   03  LINE 22 COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7 VALUE "À".
	   03	       COLUMN 31 FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
	       VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  SD-BCH.
	   03  COLUMN 32 FOREGROUND-COLOR 5
			 BACKGROUND-COLOR 7 PIC X(01) FROM GBH-ID.
	   03  COLUMN 34 FOREGROUND-COLOR 1 BACKGROUND-COLOR 7
			 PIC X(40) FROM GBH-DESC.

004870 COPY S99.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
001220 AA000	       SECTION.
001230 AA00.
041220	     MOVE 1		 TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.

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
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
001250	     DISPLAY CLR-SCREEN.
001270	     PERFORM ZA000.
	   IF WS-F-ERROR = 101
	       GO TO AA12.
	   IF WS-ERROR = 9
               GO TO AA50.
	     PERFORM BA000.
001250	     DISPLAY CLR-SCREEN.
	   IF WS-F-ERROR = 91
	       GO TO AA12.
	   IF WS-ERROR = 9
               GO TO AA50.
	   IF WS-ERROR = 8
001830	       CLOSE GLBHDR
		     PARAM
001870		     NETWORK
	       GO TO AA15.
001260       DISPLAY S00.

001340 AA05.
001350       READ GLBOLD
               AT END
               GO TO AA10.
             DISPLAY GLO-KEY AT 0412 WITH FOREGROUND-COLOR 11.
001600       PERFORM AW000.
             GO TO AA05.

001450 AA10.
001830	     CLOSE GLBOLD.
004990	     CALL "CBL_DELETE_FILE" USING W02-GLBOLD
005030				    RETURNING WS-STATUS.

001450 AA12.
001830       CLOSE GLBTCH
		   GLBHDR
                   PARAM
001870             NETWORK.
001910       DISPLAY S04.
             ACCEPT WS-OPTION AT 1439 WITH FOREGROUND-COLOR 15 AUTO.

       AA15.
001920       EXIT PROGRAM.

001930 AA50.
001940       DISPLAY S05.
001950       STOP RUN.

007600 AB000	       SECTION.

      *
      *     ***  *     ****  *	 * ****  ****	  ***** *** *	  *****
      *    *   * *     *   * *	 * *   * *   *	  *	 *  *	  *
      *    *	 *     *   * *	 * *   * *   *	  *	 *  *	  *
      *    *	 *     ****  ***** *   * ****	  ***	 *  *	  ***
      *    *  ** *     *   * *	 * *   * *   *	  *	 *  *	  *
      *    *   * *     *   * *	 * *   * *   *	  *	 *  *	  *
      *     ***  ***** ****  *	 * ****  *   *	  *	*** ***** *****
      *
000020 READ-GLBHDR.
000030	     READ GLBHDR
	       KEY IS GBH-KEY.
000040     IF WS-STATUS = "00"
               MOVE ZERO         TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000040     IF WS-STATUS = "23"
	       MOVE 51		 TO WS-F-ERROR
000050	       GO TO READ-GLBHDR-EXIT.
000060     IF WS-STAT1 = "2" OR "3" OR "4"
000070	       MOVE 51		 TO WS-F-ERROR
000080         PERFORM READ-ERROR.

000150 READ-GLBHDR-EXIT.
000160       EXIT.


       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
001960 AW000           SECTION.
011840 AW01.
011870       MOVE GLO-REC        TO GLB-REC.
             DISPLAY GLB-KEY AT 0612 WITH FOREGROUND-COLOR 14.
002310       WRITE GLB-REC.
           IF WS-STAT1 NOT = "0"
               MOVE 41           TO WS-F-ERROR
               PERFORM WRITE-ERROR.

002440 AW999.
002450       EXIT.
      /
      *    THIS ROUTINE REBUILDS THE BATCH HEADER FILE.
      *
       BA000	       SECTION.
       BA00.
001260	     DISPLAY S00H.

001340 BA05.
001350	     READ GBHOLD
               AT END
	       GO TO BA10.
	     DISPLAY GHO-KEY AT 0412 WITH FOREGROUND-COLOR 11.
001600	     PERFORM BW000.
	     GO TO BA05.

001450 BA10.
001830	     CLOSE GBHOLD.
004990	     CALL "CBL_DELETE_FILE" USING W02-GBHOLD
005030				    RETURNING WS-STATUS.
	     CLOSE GLBHDR.
042240	     OPEN I-O GLBHDR.
	     PERFORM BB000.

       BA999.
	     EXIT.
      /
      *    THIS ROUTINE ALLOWS THE USER TO SELECT THE BATCH FILE
      *    THAT WILL BE PASSED TO THE REBUILD ROUTINE.
      *
       BB000	       SECTION.
       BB00.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S06.
	     DISPLAY "F2" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
		     " to view existing batches"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1.
      *
      *    ****   G E T   T H E   B A T C H   I D .
      *
       BB0A.
	     ACCEPT S06AI.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE 8	 TO WS-ERROR
		     GO TO BB999
		 WHEN F2-KEY
018810		     PERFORM BATCH-LOOKUP
		     GO TO BB0A
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BB0A
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-BATCH-ID
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF WS-BATCH-ID = SPACE
	       MOVE "1" 	 TO WS-BATCH-ID
	       GO TO BB0A.
	     MOVE "GLJ" 	 TO GBH-FILE.
	     MOVE WS-BATCH-ID	 TO GBH-ID.
	     PERFORM READ-GLBHDR THRU READ-GLBHDR-EXIT.
	   IF WS-F-ERROR = 51
	       IF RECORD-LOCKED
		   MOVE "Batch requested is locked"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
		   GO TO BB0A
	       END-IF
	       MOVE "Batch requested not on disk"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BB0A
	   ELSE
	       MOVE GBH-DESC	 TO W03-BATCH-DESC
	       DISPLAY S06AD
	       DISPLAY S06T
	       PERFORM CHECK-CORRECT
	       IF WS-OPTION = "N"
		   GO TO BB00.
	     MOVE "GLBOLD"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "1"
			       BY WS-BATCH-ID.
	     MOVE AFID-PATH	 TO W02-GLBOLD.
	     MOVE "GLBTCH"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "1"
			       BY WS-BATCH-ID.
	     MOVE AFID-PATH	 TO W02-GLBTCH.
004990	     CALL "CBL_RENAME_FILE" USING W02-GLBTCH
005000					  W02-GLBOLD
005030				    RETURNING WS-STATUS.
	   IF NOT (WS-STATUS = X"0000")
	       MOVE "Unable to Rebuild BATCH file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       MOVE 9		 TO WS-ERROR
	       GO TO ZA999-EXIT.
042240	     OPEN INPUT GLBOLD.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
	   IF WS-STATUS = "05" OR "35"
003060	       MOVE 91		 TO WS-F-ERROR
	   ELSE
003050	   IF WS-STATUS NOT = "00"
003060	       MOVE 91		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
042240       OPEN OUTPUT GLBTCH.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060         MOVE 41           TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
	     GO TO BB999.

020110 BB200.
020120       DISPLAY "Files locked - Try later" AT 2412
020130                WITH FOREGROUND-COLOR 14.
020140       DISPLAY "Press any key" AT 2512 
020150		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
020160	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     MOVE 9		 TO WS-ERROR.

       BB999.
	     EXIT.
      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
001960 BW000	       SECTION.
011840 BW01.
011870	     MOVE GHO-REC	 TO GBH-REC.
	     DISPLAY GBH-KEY AT 0612 WITH FOREGROUND-COLOR 14.
002310	     WRITE GBH-REC.
           IF WS-STAT1 NOT = "0"
	       MOVE 51		 TO WS-F-ERROR
               PERFORM WRITE-ERROR.

002440 BW999.
002450       EXIT.

       BATCH-LOOKUP  SECTION 89.
012520 BATCH-LUP-SCREEN.
011050	     PERFORM SAVE-SCREEN.
	     PERFORM SAVE-SCREEN-3.
	     DISPLAY S-BCH.

       BATCH-LUP-CLR-SCREEN.
	     MOVE 0930		 TO CPOS.
	     PERFORM BATCH-CRT-EDGE UNTIL CLIN = 21.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 32		 TO SHADE-COL.
	     MOVE 44		 TO SHADE-WIDTH.
	     MOVE 16		 TO SHADE-LINES.
	     PERFORM SCREEN-SHADOW.
000340	     DISPLAY CLEAR-L25.
000350	     MOVE 9		 TO CLIN.
000360	     MOVE 1		 TO W75-S W75-S1.

       BATCH-LUP-TAB-LOOP.
	     MOVE SPACES	 TO W75-BTCH (W75-S1).
           IF W75-S1 < 18
               ADD 1             TO W75-S1
	       GO TO BATCH-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.
	     MOVE "  !" 	 TO GBH-KEY.
006850	     START GLBHDR
	       KEY >= GBH-KEY.
006860	   IF NOT (WS-STATUS = "00")
	       GO TO BATCH-NO-RECORD.

       BATCH-GET-BCH-RECORD.
	   IF WS-OPTION = "J"
	       MOVE SPACE	 TO WS-OPTION
	       GO TO BATCH-SKIP-BCH-READ.
006850	     READ GLBHDR NEXT	 WITH IGNORE LOCK.
009900	   IF NOT (WS-STATUS = "00")
	       SUBTRACT 1	 FROM W75-S
	       GO TO BATCH-FULL-SCREEN.

       BATCH-SKIP-BCH-READ.
	     MOVE GBH-ID	 TO W75-BTCH (W75-S).
	     DISPLAY SD-BCH AT LINE CLIN.
027700	   IF CLIN < 20
027710	       ADD 1		 TO CLIN W75-S
027720	       GO TO BATCH-GET-BCH-RECORD.

       BATCH-FULL-SCREEN.
001020	     DISPLAY " " AT 2503 WITH FOREGROUND-COLOR 6 HIGHLIGHT
					BACKGROUND-COLOR 3
001030		     " highlight - " WITH FOREGROUND-COLOR 1
					  BACKGROUND-COLOR 3
		     "PgUp" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3
		     "/" WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "PgDn" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3
		     " - " WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
001170		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape" WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.

       BATCH-SAVE-FULL-SCREEN.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 0		 TO W75-S1.
	     MOVE 0932		 TO CPOS.

       BATCH-CRT-ATTRIB.
             MOVE 1920           TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
	     MOVE 2000		 TO W41-LENGTH.

       BATCH-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY CLEAR-L25.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
		     GO TO BATCH-ADJ-NO-KEYS
		 WHEN DOWN-KEY
001340		     GO TO BATCH-CHK-NO-KEYS
		 WHEN ENTER-KEY
		 WHEN CR-KEY
		     GO TO BATCH-RESTORE-SCREEN
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BATCH-GET-USER-OPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BATCH-RESTORE-SCREEN
		 WHEN PAGE-DOWN
001340		     GO TO BATCH-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO BATCH-GET-PREV-PAGE
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BATCH-GET-USER-OPT.
	     CALL X"E5".
001280	     GO TO BATCH-GET-USER-OPT.

001240 BATCH-GET-NEXT-PAGE.
	   IF W75-S < 12
	       GO TO BATCH-GET-USER-OPT.

       BATCH-NEW-PAGE.
	     MOVE 0930		 TO CPOS.
	     PERFORM BATCH-CRT-EDGE UNTIL CLIN = 21.
	     MOVE 9		 TO CLIN.
	     MOVE LOW-VALUES	 TO W75-KEYS.
	     MOVE 1		 TO W75-S W75-S1.
	     PERFORM BATCH-LUP-TAB-LOOP.
027830	     GO TO BATCH-GET-BCH-RECORD.

       BATCH-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
	     MOVE "J"		 TO WS-OPTION.

       BATCH-GET-PREV-LOOP.
006850	     READ GLBHDR PREVIOUS
				 WITH IGNORE LOCK.
009900	   IF NOT (WS-STATUS = "00")
	       MOVE SPACE	 TO WS-OPTION
	       GO TO BATCH-NEW-PAGE.
	   IF WS-IXS1 < 24
               ADD 1             TO WS-IXS1
	       GO TO BATCH-GET-PREV-LOOP.
	     GO TO BATCH-NEW-PAGE.

027970 BATCH-NO-RECORD.
027730	     MOVE "No company codes"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
	     GO TO BATCH-RESTORE-SCREEN.

       BATCH-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1 FROM W75-S1 CLIN.
	     GO TO BATCH-HIGHLIGHT.

       BATCH-CHK-NO-KEYS.
	   IF W75-S1 = ZERO
	       MOVE 1		 TO W75-S1
	       GO TO BATCH-HIGHLIGHT.
           IF W75-S1 < W75-S
	       ADD 1		 TO W75-S1 CLIN.

       BATCH-HIGHLIGHT.
	     PERFORM BATCH-CRT-ATTRIB.
	     DISPLAY W43-DET3A (CLIN) AT CPOS
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 7.
	     GO TO BATCH-GET-USER-OPT.

       BATCH-CRT-EDGE.
	     DISPLAY "³" AT CPOS
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
012890		     "³ ³                                        ³"
		      WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 5
		     "³" WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
019260	     ADD 1		 TO CLIN.

       BATCH-RESTORE-SCREEN.
	   IF NOT (W75-S1 = ZERO)
	       MOVE W75-BTCH (W75-S1) TO WS-BATCH-ID.
011320	     PERFORM RESTORE-SCREEN-3.

       BATCH-LOOKUP-EXIT.
             EXIT.

      /
002460 ZA000	       SECTION 90.
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
	     MOVE "GBHOLD"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GBHOLD.
	     MOVE "GLBHDR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLBHDR.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
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
	     MOVE PAR-DMY	 TO W12-TODAY.
049200	     DISPLAY CLR-SCREEN.
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
	       GO TO ZA01.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      * 	   A S	 F U N C T I O N   K E Y
      *
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.

       ZA01.
042140	     OPEN I-O NETWORK.
042150     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042160             GO TO ZA200.
003050	   IF NOT (WS-STATUS = "00" OR "41")
003060         MOVE 2 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
004990	     CALL "CBL_RENAME_FILE" USING W02-GLBHDR
005000					  W02-GBHOLD
005030				    RETURNING WS-STATUS.
042240	     OPEN INPUT GBHOLD.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
	   IF WS-STATUS = "05" OR "35"
003060	       MOVE 101 	 TO WS-F-ERROR
	   ELSE
003050	   IF WS-STATUS NOT = "00"
003060	       MOVE 101 	 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
042240	     OPEN OUTPUT GLBHDR.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 51		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
002960	     GO TO ZA999-EXIT.

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
003100 ZB000-ERROR	     SECTION 91.
      
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
041630	       MOVE WS-NETKEY TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870	       MOVE WS-PARKEY TO WS-KEY
041640	   ELSE
041650     IF WS-F-ERROR = 41
041660         MOVE W02-GLBTCH   TO WS-FILE
041670         MOVE ZERO         TO WS-KEY
               MOVE GLB-KEY      TO WS-KEYX
041640	   ELSE
041650	   IF WS-F-ERROR = 51
041660	       MOVE W02-GLBHDR	 TO WS-FILE
041670         MOVE ZERO         TO WS-KEY
	       MOVE GBH-KEY	 TO WS-KEYX
041640	   ELSE
041650     IF WS-F-ERROR = 91
041660	       MOVE W02-GLBOLD	 TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
041640	   ELSE
041650	   IF WS-F-ERROR = 101
041660	       MOVE W02-GBHOLD	 TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY.
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
