      $SET LINKCOUNT"256"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	  ****	   ******    ******    *
      *   **	**     **     **    **	 **  **   **	**  **	  **   *
      *   **	**     **     **    **	**    **  **	**  **	  **   *
      *   **	**     **     *******	**    **   *******   *******   *
      *   **	**     **     **	**    **	**	  **   *
      *   **	**     **     **	 **  **   **	**  **	  **   *
      *   *******      **     **	  ****	   ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *          D E B T O R   B A L A N C E   T A K E - O N           *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  May 2006	- New field included in Accounts Receivable    *
      * 		  for recording number of outstanding Jobs     *
      * 		  linked to the account. Additional filler     *
      * 		  added for future expansion.		       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP099.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 1984. 

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1984 - 2011
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

000120 COPY CONTROL.SL.

000120 COPY PARAM.SL.

       COPY DBTRAN.SL.

000140 COPY DEBTOR.ISL.

       COPY DEBMEM.SL.

000160 DATA DIVISION.
000170 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.IFD.

000190 COPY PARAM.IFD.

       COPY DBTRAN.GFD.

000210 COPY DEBTOR.GFD.

       COPY DEBMEM.FD.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   ** 
      *         **    *    **   **    **   **   **    **  **  
      *         **   ***   **   **    **   ******     *****   
      *          ** ** ** **    **    **   **   **    **  **  
      *           ***   ***     **    **   **    **   **   ** 
      *            *     *       ******    **    **   **    **
      *
000230 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000240 77  WS-S1	   PIC 9(04)	 COMP-5.
000250 77  WS-S2           PIC 9(04)     COMP-5.
000250 77  WS-S3	   PIC 9(04)	 COMP-5.
000250 77  WS-S4	   PIC 9(04)	 COMP-5.
000240 77  WS-IXS1         PIC 9(04)     COMP-5.
000260 77  WS-PARKEY       PIC 9(04)     COMP-5.
000270 77  WS-NETKEY       PIC 9(04)     COMP-5.
000280 77  WS-OPTION	   PIC X(01).
000280 77  WS-TAKEON       PIC X(01) VALUE "A".
000310 77  WS-INDD         PIC 9(01).
000310 77  WS-ERROR        PIC 9(01).
000320 77  WS-PASSW	   PIC X(08).
000370 77  WS-ERR1	   PIC X(22)	  VALUE
000380                    "Invalid account number".
000390 77  WS-ERR2         PIC X(09)      VALUE "No record".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440         05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002430     03  WS-TOP-LNE2.
002440         05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460         05  WS-MCHR PIC X(01) OCCURS 80.
	   03  WS-MDDT-LNE REDEFINES WS-MID-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
002450	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
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

000410 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "001".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

000430 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY DEBTOR.ID.

       COPY DEBMEM.ID.

       COPY PARAM.ID.

000480 01  W10-SPARES.
000490     03  W10-DATE       PIC X(08).

       COPY W12.WS.

       COPY W20.WS.

000700 01  W25-CALCS.
000710     03  W25-TOTAL      PIC S9(09)V99 COMP-3.

       COPY W40.DBT.

       COPY W40.WS.

       COPY FUNCTION.WS.

000720 01  W70-LINKS.
000730     03  W70-OPOUT      PIC S9(09)V99 COMP-3.
000740     03  W70-OPDUE      PIC S9(09)V99 COMP-3.
000750     03  W70-OPCUR      PIC S9(09)V99 COMP-3.
000760     03  W70-OP30       PIC S9(09)V99 COMP-3.
000770     03  W70-OP60       PIC S9(09)V99 COMP-3.
000780     03  W70-OP90       PIC S9(09)V99 COMP-3.
000780     03  W70-OP120      PIC S9(09)V99 COMP-3.
000790     03  W70-OPINT      PIC S9(09)V99 COMP-3.
       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-DNO        PIC X(06) OCCURS 18.
000810 01  W80-ACCOUNT.
000820     03  W80-ACNO       PIC X(06).
000830     03  W80-NUMD REDEFINES W80-ACNO.
000850         05  W80-SERIAL PIC 9(06).
           03  W80-TYPE       PIC 9(01).
000860 01  W90-TRAN.
000870     03  W90-120        PIC S9(09)V99 COMP-3.
000870     03  W90-90         PIC S9(09)V99 COMP-3.
000880     03  W90-60         PIC S9(09)V99 COMP-3.
000890     03  W90-30         PIC S9(09)V99 COMP-3.
000900     03  W90-CUR        PIC S9(09)V99 COMP-3.
000910     03  W90-BAL        PIC S9(09)V99 COMP-3.
000920     03  W90-MTD        PIC S9(09)V99 COMP-3.
000930     03  W90-INT        PIC S9(07)V99 COMP-3. 
004490 01  W100-EDIT.
004500     03  W100-LBAL       PIC Z(06)9.99-.
004500     03  W100-LARR       PIC Z(06)9.99.
004500     03  W100-BAL        PIC Z(08)9.99-.
004510     03  W100-CUR        PIC Z(08)9.99-.
004520     03  W100-30         PIC Z(08)9.99-.
004530     03  W100-60         PIC Z(08)9.99-.
004540     03  W100-90         PIC Z(08)9.99-.
004540     03  W100-120        PIC Z(08)9.99-.
004550     03  W100-INT        PIC Z(08)9.99.
004560     03  W100-CRL        PIC Z(06)9.  
004570     03  W100-KEY        PIC Z(05)9.
       01  W200-MEMO.
           03  W200-MEM-FLDS. 
               05  W200-MEM1   PIC  X(60).
               05  W200-MEM2   PIC  X(60).
               05  W200-MEM3   PIC  X(60).
               05  W200-MEM4   PIC  X(60).
               05  W200-MEM5   PIC  X(60).
               05  W200-MEM6   PIC  X(60).
               05  W200-MEM7   PIC  X(60).
               05  W200-MEM8   PIC  X(60).
               05  W200-MEM9   PIC  X(60).
               05  W200-MEM10  PIC  X(60).
               05  W200-MEM11  PIC  X(60).
               05  W200-MEM12  PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM    PIC  X(60) OCCURS 12.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
000940 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY DEBLUP.CRT.

001010 01  S026-TRANS.
001020	   03  LINE 2  COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
001030				 VALUE "DEBTOR BALANCE TAKE-ON".
001040     03  LINE 4  COLUMN 2  VALUE "Account No:". 
001050     03  LINE 5  COLUMN 2  VALUE "Name:". 
001060	   03  LINE 6  COLUMN 2  VALUE "Balance ".
001070	   03  LINE 7  COLUMN 2  VALUE "M.T.D.  ".
001080	   03  LINE 8  COLUMN 2  VALUE " 30 Days".
001090	   03  LINE 9  COLUMN 2  VALUE " 60 Days".
001100	   03  LINE 10 COLUMN 2  VALUE " 90 Days".
001110	   03  LINE 11 COLUMN 2  VALUE "120 Days".
001110	   03  LINE 12 COLUMN 2  VALUE "Interest".

       01  S28.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W80-ACNO AUTO.

001120 01  S030-TAKE-ON.
001130	   03  LINE  6 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC Z(08)9.99-
001140                           USING W90-BAL AUTO.
001150	   03  LINE  7 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC Z(08)9.99-
001160                           USING W90-CUR AUTO.
001170	   03  LINE  8 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC Z(08)9.99-
001180                           USING W90-30 AUTO.
001190	   03  LINE  9 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC Z(08)9.99-
001200                           USING W90-60 AUTO.
001210	   03  LINE 10 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC Z(08)9.99-
001220                           USING W90-90 AUTO.
001210	   03  LINE 11 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC Z(08)9.99-
001220                           USING W90-120 AUTO.
001230	   03  LINE 12 COLUMN 16 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5 PIC Z(04)9.99-
001240                           USING W90-INT AUTO.   
       01  S-DEB-MEM.
           03  LINE  7 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
      -    "".
       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
      -    "".
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G12.
       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM12 AUTO.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
001260 AA000-MAIN	       SECTION.
001270 AA000-INIT.
003240	   IF LS0-DBLEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
001280	     PERFORM ZA000-INIT.
001310	     GO TO CA000-OPTIONS.

       AA49.
	     EXIT PROGRAM.

       AA50.
	     PERFORM SAVE-SCREEN.
011080	     MOVE 4		 TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM DEBTOR-LOOKUP.
	     PERFORM RESTORE-SCREEN.
       AA99.
             EXIT.

       COPY FUNCTION.CRT.

       COPY LOCKED.REC.

001410 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

       COPY DEBTOR.RD.

000140 COPY DEBMEM.RD.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY DEBTOR.WR.

       COPY DEBTOR.LUP.

       COPY APAC.HLP.

001920 AZ000-END	       SECTION.
001930 AZ000-EOJ.
001950       CLOSE PARAM
001960             DEBTOR
001960             DEBMEM
                   NETWORK.
       AZ05.
             EXIT PROGRAM.
      /
001990 CA000-DEBTOR            SECTION 2.
002000 CA000-OPTIONS.
002020       PERFORM CE000-GET-PASSW.
002030     IF WS-ERROR = 1 
002040          GO TO AZ000-EOJ.
002050       PERFORM CG000-GET-BAL.
002060       GO TO AZ000-EOJ.

002080 CA155-GET-DEBTOR        SECTION 2.
002090 CA155-ACCOUNT.
002130       MOVE W80-ACNO       TO DEB-ACNO.
002170       PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
002180     IF WS-F-ERROR = 6 
002190         MOVE 1            TO WS-ERROR
002200     ELSE
002210     IF DEB-ACNO NOT = W80-ACNO
002220         MOVE 2            TO WS-ERROR
002230     ELSE
002240         MOVE 0            TO WS-ERROR.
002250 CA160-EXIT.
002260       EXIT.

002270 CE000-GET-PASSW          SECTION 2.    
002280 CE00.
004060       MOVE 3              TO WS-PARKEY.
004070       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
002290       MOVE ZERO           TO WS-ERROR.
002300       DISPLAY "Password :" AT 0812.
002310	     ACCEPT WS-PASSW AT 0824
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 NO-ECHO AUTO-SKIP.
002320	     MOVE 8		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING WS-PASSW
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
002380	     MOVE 1		 TO WS-LENGTH.
	     PERFORM CHECK-CORRECT.
002460	   IF WS-OPTION = "N"
002470         GO TO CE00.
002480	   IF NOT (WS-PASSW = PAR-PASSWORD OR "DBALANCE")
002490         MOVE 1            TO WS-ERROR
002500         GO TO CE999.

002520 CE10.
002530       DISPLAY "Change password (" AT 1007
                     "Y" WITH FOREGROUND-COLOR 11
                     "/" "N" WITH FOREGROUND-COLOR 11 ")".
             MOVE "N"            TO WS-OPTION.

002540 CE15.
002550	     ACCEPT WS-OPTION AT 1032
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "Y" OR "N")
               GO TO CE15.
002510	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
002620     IF WS-OPTION = "N"
002640         GO TO CE25.

002670 CE20-CHANGE.
002680       DISPLAY "Password :" AT 1212.
002690	     ACCEPT WS-PASSW AT 1224
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5.
	     MOVE 8		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING WS-PASSW
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE  1		 TO WS-LENGTH.
	     PERFORM CHECK-CORRECT.
002770	   IF WS-OPTION = "N"
002780         GO TO CE20-CHANGE.
002510	     DISPLAY WS-BLNK78 AT 1202 WITH FOREGROUND-COLOR 3.
004060	     MOVE 3		 TO WS-PARKEY.
004070       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
002790	     MOVE WS-PASSW	 TO PAR-PASSWORD.
002800       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.

       CE25.
             DISPLAY "R" AT 1212 WITH FOREGROUND-COLOR 14
                     "eplace, " "A" WITH FOREGROUND-COLOR 14
                     "dd to current balances".

       CE30.
             ACCEPT WS-TAKEON AT 1245 
		    WITH FOREGROUND-COLOR 15
			 BACKGROUND-COLOR 5 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-TAKEON
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-TAKEON = "R" OR "A")
               GO TO CE30.

002810 CE999.
002820       EXIT.

002830 CG000-GET-BAL          SECTION 2.
002840 CG00.
002850       DISPLAY CLR-SCREEN.
002860       DISPLAY S026-TRANS.
002870       MOVE SPACES         TO W80-ACNO.   

002880 CG15.
	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to do Debtor Lookup "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
002890       ACCEPT S28 AT 0414.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO CG999
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
016560		     PERFORM AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CG15
	       END-EVALUATE
	       DISPLAY S28 AT 0414
               IF W80-ACNO = SPACES
015990             GO TO CG15.
	     DISPLAY CLEAR-L25.
002900     IF W80-ACNO = SPACES
002910       GO TO CG65.
002510	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
002920       PERFORM CA155-GET-DEBTOR.
002930     IF WS-ERROR = 1
002940         DISPLAY WS-ERR2 AT 2312 WITH FOREGROUND-COLOR 14
002950         GO TO CG15.
002960     IF WS-ERROR = 2
002970         DISPLAY WS-ERR1 AT 2312 WITH FOREGROUND-COLOR 14
002980         GO TO CG15.
	   IF DEB-OI = "O"
	       MOVE "Open Item - May not use balance take-on"
				 TO WS-ERR-MES
003260	       PERFORM ERROR-LENGTH THRU ERROR-EXIT
	       GO TO CG15.
002990	     DISPLAY DEB-NAME AT 0514
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     PERFORM CHECK-CORRECT.
003080	   IF WS-OPTION = "N"
003090         GO TO CG00.
           IF WS-TAKEON = "R"
003100         IF DEB-BAL NOT = DEB-STM
003110             DISPLAY "May not change balances - 'ENTER'" AT 2312
                            WITH FOREGROUND-COLOR 14
003120             ACCEPT WS-OPTION AT 2346 
			  WITH FOREGROUND-COLOR 15
			       BACKGROUND-COLOR 5 AUTO
003130             GO TO CG00
               ELSE
003140             MOVE DEB-BAL  TO W90-BAL
003150             MOVE DEB-CUR  TO W90-CUR
003160             MOVE DEB-30   TO W90-30 
003170             MOVE DEB-60   TO W90-60 
003180             MOVE DEB-90   TO W90-90 
                   MOVE DEB-120  TO W90-120
003190             MOVE DEB-INT  TO W90-INT
           ELSE
               MOVE ZERO         TO W90-BAL
                                    W90-CUR
                                    W90-30
                                    W90-60
                                    W90-90
                                    W90-120
                                    W90-INT.
	     DISPLAY "Press " AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " to CANCEL"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

003210 CG25.
003220       ACCEPT S030-TAKE-ON.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY	 GO TO CG00
		 WHEN OTHER
		     CALL X"E5"
		     GO TO CG25
	       END-EVALUATE.
	     PERFORM CHECK-CORRECT.
003310	   IF WS-OPTION = "N"
003320         GO TO CG25.
003330	     MOVE ZERO TO WS-ERROR.
003340       COMPUTE W25-TOTAL = W90-CUR + W90-30  + W90-60 +
003350                            W90-90  + W90-120 + W90-INT.
003360     IF W25-TOTAL NOT = W90-BAL
003370         GO TO CG25.

003960       MOVE 1              TO WS-NETKEY.
003970       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
           IF WS-TAKEON = "R"
003380         SUBTRACT DEB-BAL  FROM DEB-OPEN
003390         SUBTRACT DEB-STM  FROM DEB-OUT
003400         SUBTRACT DEB-CUR  FROM DEB-MTD
003410         SUBTRACT DEB-30   FROM DEB-ONE
003420         SUBTRACT DEB-60   FROM DEB-TWO
003430         SUBTRACT DEB-90   FROM DEB-THREE
003430         SUBTRACT DEB-120  FROM DEB-FOUR
003440         SUBTRACT DEB-INT  FROM DEB-INTEREST.
003450       ADD W90-BAL         TO DEB-OPEN.   
003460       ADD W90-BAL         TO DEB-OUT.
003470       ADD W90-CUR         TO DEB-MTD.
003480       ADD W90-30          TO DEB-ONE.
003490       ADD W90-60          TO DEB-TWO.
003500       ADD W90-90          TO DEB-THREE.
003500       ADD W90-120         TO DEB-FOUR.
003510       ADD W90-INT         TO DEB-INTEREST.
003520 CG35.
           IF WS-TAKEON = "R"
003530         MOVE W90-BAL      TO DEB-BAL DEB-STM
003540         MOVE W90-CUR      TO DEB-CUR
003550         MOVE W90-30       TO DEB-30
003560         MOVE W90-60       TO DEB-60
003570         MOVE W90-90       TO DEB-90
003570         MOVE W90-120      TO DEB-120
003580         MOVE W90-INT      TO DEB-INT
           ELSE
003530         ADD W90-BAL       TO DEB-BAL DEB-STM
003540         ADD W90-CUR       TO DEB-CUR
003550         ADD W90-30        TO DEB-30 
003560         ADD W90-60        TO DEB-60 
003570         ADD W90-90        TO DEB-90 
003570         ADD W90-120       TO DEB-120
003580         ADD W90-INT       TO DEB-INT.
003590       PERFORM REWRITE-DEBTOR-UNLOCK THRU WRITE-DEBTOR-EXIT.
003710       PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
003600       GO TO CG00.
003610 CG65.
003720 CG999.
003730       EXIT.

003740 ZA000-INIT              SECTION 2.
003750 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
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
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
003780	     OPEN I-O PARAM.
003790     IF RUNTIME-ERROR
               IF FLE-LOCKED
042030             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
003810     IF WS-STATUS NOT = "00"
003820         MOVE 15           TO WS-ERROR
003830         PERFORM OPEN-ERROR.
	     OPEN I-O NETWORK.
003900     IF RUNTIME-ERROR
               IF FLE-LOCKED
042230             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
003920     IF WS-STATUS NOT = "00"
003930         MOVE 2            TO WS-F-ERROR
003940         PERFORM OPEN-ERROR.
003890       OPEN I-O DEBTOR.
003900     IF RUNTIME-ERROR
               IF FLE-LOCKED
042230             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 6        TO WS-F-ERROR
                   GO TO ZA50.
003920     IF WS-STATUS NOT = "00"
003930         MOVE 6            TO WS-F-ERROR
003940         PERFORM OPEN-ERROR.
               OPEN I-O DEBMEM.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 27       TO WS-F-ERROR
                   GO TO ZA50.
025630       MOVE 1              TO WS-PARKEY.
025640       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
025660       MOVE PAR-DMY        TO W12-TODAY.
025670       MOVE PAR-YMD        TO W12-T-YMD.
004080       GO TO ZA999-EXIT.

       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.

       ZA50.
041850     IF WS-F-ERROR = 6
041860         MOVE "DEBTOR file"
                                 TO WS-FILE
           ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE "DEBTOR MEMO"
                                 TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.
       ZA51.
             EXIT PROGRAM.
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
021940                              WS-TCHR (61) WS-TCHR (72).
021950       MOVE WS-G3          TO WS-MCHR (8)  WS-MCHR (49)
021960                              WS-MCHR (61) WS-MCHR (72)
021960				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
004090 ZA200.
004100       DISPLAY "Files locked - Try later" AT 2312
004110                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
004120       DISPLAY "Press any key" AT 2512 
004130		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
004140	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
004170 ZA999-EXIT.
004180       EXIT.
      /
004190 ZB000-ERROR           SECTION 2.
      
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
041620         MOVE W02-NETWORK  TO WS-FILE
041630         MOVE WS-NETKEY    TO WS-KEY
041760     ELSE
041770     IF WS-F-ERROR = 6
041780	       MOVE W02-DEBTOR TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 27
041860	       MOVE W02-DEBMEM	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX.
041880     IF WS-STATUS = "10"
041890         MOVE "End of FILE" 
                                 TO WS-STAT-MESSAGE
041900     ELSE
041910     IF WS-STATUS = "22"
041920         MOVE "Duplicate record number" 
                                 TO WS-STAT-MESSAGE
041930     ELSE
041940     IF WS-STATUS = "23"
041950         MOVE "Invalid record number" 
                                 TO WS-STAT-MESSAGE
041960     ELSE
041970     IF WS-STATUS = "24"
041980         MOVE "DISK full"  TO WS-STAT-MESSAGE
041990     ELSE
042000     IF WS-STATUS = "30"
042010         MOVE "DISK error" TO WS-STAT-MESSAGE
042020     ELSE
042030     IF WS-STATUS = "94"
042040         MOVE "FILE locked" 
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
