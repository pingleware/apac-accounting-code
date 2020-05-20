      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *   *******   **	  **  *******	 ******	   *****   **	 **    *
      *   **	**  **	  **  **    **	**    **  **   **  ***	 **    *
      *   **	**  **	  **  **   **	**	  **   **  ****	 **    *
      *   *******   **	  **  ******	**	  **   **  ** ** **    *
      *   **	    **	  **  **   **	**	  **   **  **  ****    *
      *   **	    **	  **  **    **	**    **  **   **  **	***    *
      *   **	     ******   **     **	 ******	   *****   **	 **    *
      *                                                                *
      *   ENGLISH                                                      *
      *                                                                *
      *   P U R C H A S E   T R A C K I N G   C O N V E R T  P R O G   *
      *                                                                *
      *     VERSION 8.12.01 - February 2007			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     PURCON.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   FEBRUARY 2007.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2007 - 2007
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

000120 COPY CONTROL.SL.

000120 COPY PARAM.SL.

      $SET KEYCOMPRESS "7"
000010	   SELECT PURDEX  ASSIGN DISK
                          LOCK EXCLUSIVE
000040			  STATUS WS-STATUS
000050                    ACCESS DYNAMIC
000060                    ORGANIZATION INDEXED
000070			  RECORD PRD-KEY
			  ALTERNATE RECORD PRD-SKEY =
				    PRD-EXT-CODE PRD-SUPP DUPLICATES
			  ALTERNATE RECORD PRD-MKEY =
				    PRD-EXT-CODE PRD-DTE DUPLICATES
			  ALTERNATE RECORD PRD-DKEY =
				    PRD-DTE PRD-SUPP DUPLICATES
			  ALTERNATE RECORD PRD-CDKEY =
				    PRD-SUPP PRD-DTE DUPLICATES
			  ALTERNATE RECORD PRD-DSKEY =
				    PRD-DTE PRD-EXT-CODE DUPLICATES
			  ALTERNATE RECORD PRD-RSKEY =
				    PRD-SUPP PRD-REF
				    PRD-DTE  PRD-SEQ DUPLICATES.
      $SET NOKEYCOMPRESS

000180	   SELECT PRDOLD  ASSIGN DISK
                          LOCK EXCLUSIVE
000190                    STATUS WS-STATUS
000200                    ACCESS SEQUENTIAL
000210                    ORGANIZATION SEQUENTIAL.
      /
000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000120 COPY CONTROL.FDE.

000290 COPY PARAM.FDE.

000140 COPY PURDEX.FDE.

000160 COPY PRDOLD.FD.

      /
000430 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000440 77  WS-S1	    PIC 9(04)	  COMP-5.
001580 77  WS-S2	   PIC	9(04)	 COMP-5.
001590 77  WS-S3	   PIC	9(04)	 COMP-5.
001600 77  WS-S4           PIC  9(04)    COMP-5.
000440 77  WS-SUB	    PIC 9(04)	  COMP-5.
003290 77  WS-NETKEY        PIC 9(04)     COMP-5 VALUE 1.
003290 77  WS-DEPKEY        PIC 9(04)     COMP-5 VALUE 1.
003300 77  WS-CTRKEY        PIC 9(04)     COMP-5 VALUE 1.
003340 77  WS-PARKEY        PIC 9(04)     COMP-5 VALUE 1.
003360 77  WS-RECKEY        PIC 9(04)     COMP-5.
000520 77  WS-RECOVER       PIC 9(05)     COMP-0.
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
               05  FILLER      PIC  X(39).
002450     03  WS-MID-LNE.
002460         05  WS-MCHR PIC  X(01) OCCURS 80.
002450     03  WS-MID-LNE2.
002460         05  WS-MCH  PIC  X(01) OCCURS 80.
002470     03  WS-BOT-LNE.
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

000140 COPY PURDEX.ID.

000160 COPY PRDOLD.ID.

000320 01  W12-DATE.
000330     03  W12-TODAY       PIC  9(06).
000340     03  W12-DATE-DMY REDEFINES W12-TODAY.
000350         05  W12-DAY     PIC  9(02).
000360         05  W12-MONTH   PIC  9(02).  
000370         05  W12-YEAR    PIC  9(02).
000380     03  W12-T-YMD       PIC  9(06). 
000390     03  W12-DATE-YMD REDEFINES W12-T-YMD.
000400         05  W12-YY      PIC  9(02).
000410         05  W12-MM      PIC  9(02).
000420         05  W12-DD      PIC  9(02).

       COPY W40.WS.

       COPY FUNCTION.WS.

      /
       LINKAGE SECTION.

       COPY CHAIN.LS.

000850 SCREEN SECTION.
      *
      *    ****  THIS SCREEN - CLEARS THE DISPLAY AND SETS THE DEFAULT
      *          COLOUR TO 3 (LIGHT BLUE).
      *
001950 01  CLR-SCREEN.
003170     03  BLANK SCREEN FOREGROUND-COLOR 3
                            BACKGROUND-COLOR 1.
	   03  CLR-L1-2.
003860	   05  LINE  1 COLUMN  1 BACKGROUND-COLOR 0
				 PIC X(80) USING WS-TOP-LNE2.
	   05  LINE  1 COLUMN 47 BACKGROUND-COLOR 0
				 VALUE "´".
	   05	       COLUMN 48 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC Z9/99/9999 USING TODAY-DDMMYY.
	   05	       COLUMN 58 BACKGROUND-COLOR 0
				 VALUE "Ã".
003870	   05  LINE  2 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
004160	   05	       COLUMN  2 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 4
004170				 VALUE "APAC ".
	   05	       COLUMN  7 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 4
				 VALUE "- Utilities.".
	   05	       COLUMN 19 PIC  X(48) FROM WS-BACKGROUND.
004180	   05	       COLUMN 66 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 4
004190				 VALUE "Version - ".
	   05	       COLUMN 76 BACKGROUND-COLOR 7
				 FOREGROUND-COLOR 4
				 VALUE "8.12".
003920	   05	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
003930	   03  LINE  3 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
003940	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
003950	   03  LINE  4 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
003960	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
003970	   03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
003980	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
003990	   03  LINE  6 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004000	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004010	   03  LINE  7 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004020	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004030	   03  LINE  8 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004040	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004050	   03  LINE  9 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004060	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004070	   03  LINE 10 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004080	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004090	   03  LINE 11 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004100	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004110	   03  LINE 12 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004120	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004130	   03  LINE 13 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004140	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004150	   03  LINE 14 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004160	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004170	   03  LINE 15 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004180	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004190	   03  LINE 16 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004200	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004210	   03  LINE 17 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004220	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004230	   03  LINE 18 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004240	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004250	   03  LINE 19 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004260	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004270	   03  LINE 20 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004280	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004290	   03  LINE 21 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004300	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004310	   03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004320	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
004330	   03  LINE 23 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "³".
	   03	       COLUMN  2 PIC  X(78) FROM WS-BACKGROUND.
004340	   03	       COLUMN 80 BACKGROUND-COLOR 0
				 VALUE "³".
006550	   03  LINE 24 COLUMN  1 BACKGROUND-COLOR 0
				 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      - 	     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
	   03  CLEAR-L25.
	   05  LINE 25 BLANK LINE BACKGROUND-COLOR 3
				  FOREGROUND-COLOR 1.

000860 01  S00.
000910	   03  LINE  2 COLUMN 26 FOREGROUND-COLOR 15
000920		       VALUE "CARDEX FILE CONVERSION PROGRAM".
000930     03  LINE  4 COLUMN  2 VALUE "Reading :".
000950	   03  LINE  6 COLUMN  2 VALUE "Writing :".

001080 01  S04.
001090     03  LINE 12 COLUMN 12 FOREGROUND-COLOR 11
001100		       VALUE "Rebuilt of CARDEX file completed".
001110     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001120		       VALUE "Press any key to continue".

001130 01  S05.
001140     03  LINE 12 COLUMN 12 FOREGROUND-COLOR 11
001150                 VALUE "Rebuild unsuccessfull !".
001160     03  LINE 14 COLUMN 12 FOREGROUND-COLOR 11
001170                 VALUE "Reload files from last backup and rekey al
001180-                      "l updates and transactions".
001190     03  LINE 16 COLUMN 12 FOREGROUND-COLOR 11
001200                 VALUE "since creating the last backup disk/s".

       COPY ERROR.CRT.

004870 COPY S99.CRT.

      /
001210 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
001220 AA000	       SECTION.
001230 AA00.
041220	     MOVE 1		 TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.

041240 AA02.
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 	 TO WS-TCH(WS-S1)  WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270	       ADD 1		 TO WS-S1
041280         GO TO AA02.
	     MOVE WS-G9 	 TO WS-TCH(1).
	     MOVE WS-G10	 TO WS-TCH(80).
	     MOVE WS-G11	 TO WS-BCH(1).
	     MOVE WS-G12	 TO WS-BCH(80).
	     MOVE WS-G14	 TO WS-TCHR(1)	WS-BCHR(1).
	     MOVE WS-G13	 TO WS-TCHR(80) WS-BCHR(80).
021930	     MOVE WS-G2 	 TO WS-TCHR(16) WS-TCHR(41)
021940				    WS-TCHR(64) WS-TCHR(71).
021950	     MOVE WS-G3 	 TO WS-MCHR(16) WS-MCHR(41)
021960				    WS-MCHR(64) WS-MCHR(71)
				    WS-MCHR(1)	WS-MCHR(80).
021970	     MOVE WS-G4 	 TO WS-BCHR(16) WS-BCHR(41)
021980				    WS-BCHR(64) WS-BCHR(71).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
001250	     DISPLAY CLR-SCREEN.
001270       PERFORM ZA000.
           IF WS-ERROR = 9
               GO TO AA50.
001260       DISPLAY S00.

001340 AA05.
001350	     READ PRDOLD
               AT END
               GO TO AA10.
	     DISPLAY POD-SUPP AT 0412 WITH FOREGROUND-COLOR 11
		     " " POD-CODE WITH FOREGROUND-COLOR 11.
001600       PERFORM AW000.
             GO TO AA05.

001450 AA10.
044650	     MOVE 2		 TO WS-NETKEY.
044660       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
	     MOVE "8.12"	 TO P-VER.
	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
001830	     CLOSE PURDEX
		   PRDOLD.
001910       DISPLAY S04.
             ACCEPT WS-OPTION AT 1439 WITH FOREGROUND-COLOR 15 AUTO.
004990	     CALL "CBL_DELETE_FILE" USING W02-PRDOLD
005030				    RETURNING WS-STATUS.

       AA15.
001920       EXIT PROGRAM.

001930 AA50.
001940       DISPLAY S05.
001950       STOP RUN.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  SCREEN-SHADOW 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:						       ³
      *	   ³	       SHADE-ROW   - Top line of the box + 1.	       ³
      *	   ³	       SHADE-COL   - Left line of box + 2.	       ³
      *	   ³	       SHADE-WIDTH - Width of the box - 2.	       ³
      *	   ³	       SHADE-LINES - Hight of box - 1.		       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  CHECK-CORRECT 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³	      "Correct ENTER - N if incorrect"		       ³
      *    ³							       ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³							       ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 13 (default); move another value to SLIN and PERFORM      ³
      *    ³ CHECK-SETUP THRU CHECK-EXIT.			       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³			  ERROR-MESSAGE 		       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display an error message with " - Press ANY key" at    ³
      *    ³ the end; use PERFORM ERROR-MESSAGE.		       ³
      *    ³							       ³
      *    ³ To display just the error message; use PERFORM	       ³
      *    ³ ERROR-LENGTH THRU ERROR-EXIT.			       ³
      *    ³							       ³
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   ³
      *    ³ or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     ³
      *    ³ number to be used to SLIN and then PERFORM ERROR-DISPLAY  ³
      *    ³ THRU ERROR-EXIT.					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY FUNCTION.CRT.


       COPY LOCKED.REC.

      /
      *    ****    READ FILES ROUTINES
      *
008680 AC000              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
010170 AD000             SECTION.

       COPY CONTROL.WR.

      /
      *    THIS ROUTINE WRITES THE DETAILS TO THE NEW FILE.
      *
001960 AW000           SECTION.
011840 AW01.
011870	     MOVE POD-REC1	 TO PRD-REC1.
	     DISPLAY PRD-SUPP AT 0612 WITH FOREGROUND-COLOR 14
		     " " PRD-CODE WITH FOREGROUND-COLOR 14.
002310	     WRITE PRD-REC1.
           IF WS-STAT1 NOT = "0"
	       MOVE 56		 TO WS-F-ERROR
               PERFORM WRITE-ERROR.

002440 AW999.
002450       EXIT.

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
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "PRDOLD"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PRDOLD.
	     MOVE "PURDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PURDEX.
004990	     CALL "CBL_RENAME_FILE" USING W02-PURDEX
005000					  W02-PRDOLD
005030				    RETURNING WS-STATUS.
	   IF NOT (WS-STATUS = X"0000")
	       MOVE "Unable to Rebuild CARDEX file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       MOVE 9		 TO WS-ERROR
	       GO TO ZA999-EXIT.
042240	     OPEN INPUT PRDOLD.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 96		 TO WS-F-ERROR
003070         PERFORM OPEN-ERROR.
042240	     OPEN OUTPUT PURDEX.
042250     IF WS-STAT1 = "9"
               IF FLE-LOCKED
042260             GO TO ZA200.
003050     IF WS-STATUS NOT = "00"
003060	       MOVE 56		 TO WS-F-ERROR
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
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041640     ELSE
041650	   IF WS-F-ERROR = 56
041660	       MOVE W02-PURDEX	 TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
041640     ELSE
041650	   IF WS-F-ERROR = 96
041660	       MOVE W02-PRDOLD	 TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY.
041880     IF WS-STATUS = "10"
041890	       MOVE "End of FILE"
				 TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920	       MOVE "Duplicate record number"
				 TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950	       MOVE "Invalid record number"
				 TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980	       MOVE "DISK full"  TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040	       MOVE "FILE locked"
				 TO WS-STAT-MESSAGE.
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
