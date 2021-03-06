      $SET LINKCOUNT"320"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	     ****	**     *
      *   **	**  **	  **  **    **	 **  **     **	**     ***     *
      *   **	**  **	  **  **    **	**    **   **	 **	**     *
      *   **	**  *******   **    **	**    **   **	 **	**     *
      *   **	**  **	 **   **    **	**    **   **	 **	**     *
      *   **	**  **	  **  **    **	 **  **     **	**	**     *
      *    ******   **	  **  ******	  ****	     ****     ******   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    O R D E R   N U M B E R   S Y S T E M		       *
      *                                                                *
      *     VERSION 8.14.03 - May 2010				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     ORD001.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 2002.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2002 - 2010
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

000140 COPY CONTROL.SL.

000140 COPY PARAM.SL.

000140 COPY SYSUSE.SL.

000170 DATA DIVISION.
000180 FILE SECTION.

       COPY APACFIDS.FDE.

000140 COPY CONTROL.FDE.

000220 COPY PARAM.FDE.

000310 COPY SYSUSE.FD.

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
000940 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000950 77  WS-S1	       PIC 9(04)     COMP-5.
000960 77  WS-S2               PIC 9(04)     COMP-5.
000970 77  WS-S3               PIC 9(04)     COMP-5.
000980 77  WS-S4               PIC 9(04)     COMP-5.
000990 77  WS-S5               PIC 9(04)     COMP-5.
001000 77  WS-S6               PIC 9(04)     COMP-5.
001000 77  WS-S7	       PIC 9(04)     COMP-5.
001000 77  WS-S8	       PIC 9(04)     COMP-5.
001010 77  WS-PARKEY           PIC 9(04)     COMP-5.
001010 77  WS-PRNKEY           PIC 9(04)     COMP-5.
001010 77  WS-NETKEY	       PIC 9(04)     COMP-5.
001070 77  WS-NUM	       PIC Z9.
001070 77  WS-SNO              PIC 9(02).
       77  WS-ADV-RATE         PIC S9(03)V99 COMP-3.
001090 77  WS-PAGE             PIC 9(04)     COMP-5.
003050 77  WS-NORM         PIC  X(04).
003060 77  WS-COND         PIC  X(04).
003070 77  WS-EXPP         PIC  X(04).
003080 77  WS-ECAN         PIC  X(04).
003090 77  WS-8LPI       PIC  X(04).
003100 77  WS-6LPI       PIC  X(04).
       77  WS-10CPI        PIC  X(04).
       77  WS-12CPI        PIC  X(04).
       77  WS-17CPI        PIC  X(04).
       77  WS-DBLP         PIC  X(04).
       77  WS-DCAN         PIC  X(04).
001100 77  WS-OPTION           PIC X(01).
001100 77  WS-VAT-GST          PIC X(01).
001120 77  WS-ERROR            PIC 9(01).
003150 77  WS-PRN-NO	   PIC X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT	   PIC X(01)	 COMP-X.
001140 77  WS-CHAR1	       PIC X(01).
001150 77  WS-CHAR2            PIC X(01).
001160 77  WS-COMP             PIC 9(01).
001170 77  WS-TYPE             PIC X(01).
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002420 01  WS-DB-LINE.
002430     03  WS-TOP-LNE.
002440	       05  WS-TCHR PIC X(80) VALUE "����������������������������
      -	       "��������������������������������������������������͵".
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002430     03  WS-TOP-LNE2.
002440	       05  WS-TCH  PIC X(80) VALUE "����������������������������
      -        "��������������������������������������������������Ŀ".
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
002450	   03  WS-MID-LNE.
002460	       05  WS-MCHR PIC X(80) VALUE "�              �
      -        "            �                      �      �        �".
002450     03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "�".
	       05  WS-BLNK78   PIC  X(78) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "�".
002470	   03  WS-BOT-LNE.
002480	       05  WS-BCHR PIC X(80) VALUE "����������������������������
      -	       "��������������������������������������������������͵".
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480	       05  WS-BCH  PIC X(80) VALUE "����������������������������
      -        "����������������������������������������������������".

001260 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "ORD".
	   03  WS-PROG	       PIC  X(03) VALUE "001".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY PARAM.ID.

       COPY SYSUSE.ID.

001660 01  W10-INPUT.
001670     03  W10-LEDG       PIC 9(03).
001680	   03  W10-DATE       PIC X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.

       COPY W12.WS.

       01  W15-CONTROL.
           03  W15-DEBINV     PIC  9(08)    COMP-3.
           03  W15-DEBREC     PIC  9(08)    COMP-3.
           03  W15-DEBORD     PIC  9(08)    COMP-3.
           03  W15-DEBJOB     PIC  9(08)    COMP-3.
           03  W15-CRDORD     PIC  9(08)    COMP-3.
001490 01  W15-HEX.
001500     03  W15-ASCII       PIC X(01) OCCURS 256.

       COPY W20.WS.

001890 01  W25-CALCS.
001900     03  W25-RESULT     PIC 9(07)V99.
001910     03  W25-RESULT1 REDEFINES W25-RESULT.
001920         05  FILLER      PIC 9(01).
001930         05  W25-WHOLE  PIC 9(06).
001940         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(06).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
001950     03  W25-PASS.
001960         05  W25-PASS1  PIC 9(03).
001970         05  W25-PASS2  PIC 9(03).
001980     03  W25-TIME       PIC 9(08).
001990     03  W25-TRED REDEFINES W25-TIME.
002000         05  FILLER      PIC 9(02).
002010         05  W25-FACT   PIC 9(06).
002020     03  W25-TOTAL      PIC S9(07)V99  COMP-3.
002030     03  W25-VALUE      PIC S9(07)V99  COMP-3.

       COPY W40.WS.

       COPY FUNCTION.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
      *   ******    ******   *******   ********  ********  **     **
      *  **    **  **    **  **    **  **        **        ***    **
      *  **        **        **    **  **        **        ** *   **
      *   ******   **        ******    *****     *****     **  *  **
      *        **  **        **   **   **        **        **   * **
      *  **    **  **    **  **    **  **        **        **    ***
      *   ******    ******   **    **  ********  ********  **     **
      *
003150 SCREEN SECTION.

       COPY BLANK.CRT.

       01  S37.
	   03  LINE  8 COLUMN 59 BACKGROUND-COLOR 5
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(01)
				 USING DEB-ORD-SYS AUTO.

       COPY S99.CRT.

       COPY ERROR.CRT.

      /
      *        *******    *******     ******     ******
      *        **    **   **    **   **    **   **    **
      *        **    **   **   **    **    **   **
      *        *******    ******     **    **   **
      *        **         **   **    **    **   **
      *        **         **    **   **    **   **    **
      *        **         **    **    ******     ******
      *
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
006220 AA000-MAIN	       SECTION.
006230 AA000-INIT.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
003240	   IF LS0-SALEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
006250	     PERFORM ZA000-INIT.
	     PERFORM YA000.

       AA49.
             EXIT PROGRAM.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    �����������������������������������������������������������Ŀ
      *    �	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      �
      *    �����������������������������������������������������������͵
      *    �			  SCREEN-SHADOW 		       �
      *    �����������������������������������������������������������Ĵ
      *    � To routine is used to display a shadow down the right and �
      *    � along the bottom of a pop-up box. The parameters that are �
      *    � required:						       �
      *	   �	       SHADE-ROW   - Top line of the box + 1.	       �
      *	   �	       SHADE-COL   - Left line of box + 2.	       �
      *	   �	       SHADE-WIDTH - Width of the box - 2.	       �
      *	   �	       SHADE-LINES - Hight of box - 1.		       �
      *    �����������������������������������������������������������͵
      *    �			  CHECK-CORRECT 		       �
      *    �����������������������������������������������������������Ĵ
      *    � This routine displays a pop-up window with the message    �
      *    �	      "Correct ENTER - N if incorrect"		       �
      *    �							       �
      *    � The response is returned in WS-OPTION (in upper case).    �
      *    �							       �
      *    � To start with the pop-up window higher or lower than row  �
      *    � 13 (default); move another value to SLIN and PERFORM      �
      *    � CHECK-SETUP THRU CHECK-EXIT.			       �
      *    �����������������������������������������������������������͵
      *    �			  ERROR-MESSAGE 		       �
      *    �����������������������������������������������������������Ĵ
      *    � To display an error message with " - Press ANY key" at    �
      *    � the end; use PERFORM ERROR-MESSAGE.		       �
      *    �							       �
      *    � To display just the error message; use PERFORM	       �
      *    � ERROR-LENGTH THRU ERROR-EXIT.			       �
      *    �							       �
      *    � To display the error message higher or lower (default is  �
      *    � line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   �
      *    � or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     �
      *    � number to be used to SLIN and then PERFORM ERROR-DISPLAY  �
      *    � THRU ERROR-EXIT.					       �
      *    �������������������������������������������������������������

       COPY FUNCTION.CRT.

       COPY LOCKED.RC2.

       COPY APAC.HLP.

       AA200              SECTION.
       AA201.
002130	     DISPLAY "�" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     WS-BLNK78 WITH FOREGROUND-COLOR 3
		     "�" WITH BACKGROUND-COLOR 0
			      FOREGROUND-COLOR 3.
019260	     ADD 1		 TO CLIN.
       AA205.
             EXIT.
      /    *************************************************************
      *    **** 	     READ FILES ROUTINES		    ****
      *    *************************************************************
006630 AC000-READ	       SECTION.

       COPY CONTROL.RD.

       COPY PARAM.RD.

       COPY SYSUSE.RD.

      *    *************************************************************
      *    **** 	REWRITE AND WRITE FILES ROUTINES	    ****
      *    *************************************************************
006940 AD000-WRITE	       SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY SYSUSE.WR.

      /    *************************************************************
      *    *****     O R D E R	 N U M B E R   S E Q U E N C E	   *****
      *    *************************************************************
008640 YA000		       SECTION 5.
       YA360-FORMATS.
             DISPLAY CLR-SCREEN.
	     DISPLAY "ORDER NUMBERING SYSTEM" AT 0231
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "Use seperate sequence per User (" AT 0822
		     "Y" WITH FOREGROUND-COLOR 3 HIGHLIGHT
		     "/" "N" WITH FOREGROUND-COLOR 3 HIGHLIGHT ")".
	     DISPLAY "F1" AT 2502 WITH BACKGROUND-COLOR 3
				       FOREGROUND-COLOR 6 HIGHLIGHT
		     "=Help" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
014240	     MOVE 1		 TO WS-NETKEY.
014250	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
	   IF NOT (DEB-ORD-SYS = "N" OR "Y")
	       PERFORM YB000
	       MOVE "N" 	 TO DEB-ORD-SYS.

       YA365.
014270	     ACCEPT S37.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO YA365
		 WHEN OTHER
		     CALL X"E5"
		     GO TO YA365
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING DEB-ORD-SYS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (DEB-ORD-SYS = "N" OR "Y")
	       GO TO YA365.

       YA370.
	     PERFORM CLEAR-L50.
012180	     PERFORM CHECK-CORRECT.
014350	   IF WS-OPTION = "N"
014360         GO TO YA365.
014370	     PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.

       YA999.
	     EXIT.
      /
       YB000		       SECTION 50.
       YB00.
	     OPEN I-O SYSUSER.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 23		 TO SHADE-COL.
	     MOVE 38		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "���������������������������������������"
		      AT 0921 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				   BACKGROUND-COLOR 5
		     "�" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
	     DISPLAY "� " AT 1021 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				       BACKGROUND-COLOR 5
		     "Initialising User Order Numbers:     �"
		      WITH BACKGROUND-COLOR 0 FOREGROUND-COLOR 5.
	     DISPLAY "�" AT 1121 WITH FOREGROUND-COLOR 5 HIGHLIGHT
				      BACKGROUND-COLOR 5
		     "����������������������������������������"
		      WITH BACKGROUND-COLOR 0 FOREGROUND-COLOR 5.
	     PERFORM SCREEN-SHADOW.
	     MOVE ZERO		 TO USE-NO.
	     PERFORM START-AT-USER-AC THRU READ-SYSUSER-EXIT.
	   IF WS-F-ERROR = 64
	       GO TO YB10.

       YB05.
	     PERFORM READ-SYSUSER-NEXT-LOCK THRU READ-SYSUSER-EXIT.
	   IF WS-F-ERROR = 64
	       GO TO YB10.
	     DISPLAY USE-NO AT 1056
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE 1		 TO USE-ORD
				    USE-SEQ.
	     MOVE W12-T-YMD	 TO USE-DATE.
	     PERFORM REWRITE-SYSUSER-UNLOCK THRU WRITE-SYSUSER-EXIT.
	     GO TO YB05.

       YB10.
	     PERFORM RESTORE-SCREEN-3.
	     CLOSE SYSUSER.

       YB999-EXIT.
	     EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
015920 ZA000-INIT	       SECTION 80.
015930 ZA000-OPEN.
	     PERFORM ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
	     DISPLAY CLR-SCREEN.
      *    �����������������������������������������������������������Ŀ
      *    �	   S e t   u p	 t h e	 F U N C T I O N   k e y s     �
      *    �������������������������������������������������������������
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    �����������������������������������������������������������Ŀ
      *    �		       ESC, F1 to F10 keys		       �
      *    �������������������������������������������������������������
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    �����������������������������������������������������������Ŀ
      *    �		    PAGE-UP and PAGE-DOWN keys		       �
      *    �������������������������������������������������������������
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    �����������������������������������������������������������Ŀ
      *    �		   A C T I V A T E   M O U S E		       �
      *    �������������������������������������������������������������
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
      *    �����������������������������������������������������������Ŀ
      *    �	       Set MOUSE key to act as FUNCTION key	       �
      *    �������������������������������������������������������������
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
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "SYSUSE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SYSUSER.

       ZA27.
016210       MOVE 1              TO WS-PARKEY.
016220       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
016230       MOVE PAR-DMY        TO W12-TODAY.
016240       MOVE PAR-YMD        TO W12-T-YMD.
044780	     GO TO ZA999-EXIT.

041240 ZA60.
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "Supervisor�"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.

016260 ZA999-EXIT.
016270       EXIT.
      /
016280 ZB000-ERROR	     SECTION 81.

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

041560 DISPLAY-FILE-NAME.
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
