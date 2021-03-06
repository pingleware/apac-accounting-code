      $set LINKCOUNT"248"
      ******************************************************************
      *                                                                *
      *   ********   ******   *******     ****      ****      ****     *
      *      **     **    **  **    **   **  **    **  **    **  **    *
      *      **     **        **    **  **    **  **    **  **    **   *
      *      **     **        *******   **    **  **    **  **    **   *
      *      **     **        **        **    **  **    **  **    **   *
      *   ** **     **    **  **         **  **    **  **    **  **    *
      *    ***       ******   **          ****      ****      ****     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   J O B - C O S T I N G   I N I T I A L I Z E   P R O G R A M  *
      *                                                                *
      *     VERSION 8.13.07 - January 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     JCP000.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 1991.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2008
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
000080                 CONSOLE IS CRT.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000240 COPY CONTROL.SL.

000180 COPY JOBAUD.SL.

000180 COPY JOBCST.SL.

000180 COPY JOBHDR.SL.

000270 COPY JOBREQ.SL.

000270 COPY JOBRMK.SL.

000270 COPY JOBWAR.SL.

       COPY PARAM.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

000240 COPY CONTROL.IFD.

000350 COPY JOBAUD.FD.

000350 COPY JOBCST.FD.

000350 COPY JOBHDR.FD.

000500 COPY JOBREQ.FD.

000500 COPY JOBRMK.FD.

000500 COPY JOBWAR.FD.

       COPY PARAM.IFD.

      /
000430 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000440 77  WS-S1	    PIC  9(04)	   COMP-5.
000450 77  WS-S2	    PIC  9(04)	   COMP-5.
000460 77  WS-S3	    PIC  9(04)	   COMP-5.
000470 77  WS-S4	    PIC  9(04)	   COMP-5.
000520 77  WS-NETKEY	    PIC  9(04)	   COMP-5.
000520 77  WS-PARKEY	    PIC  9(04)	   COMP-5.
000580 77  WS-OPTION	    PIC  X(01).
000590 77  WS-ERROR	    PIC  9(01).
000600 77  WS-DEB	    PIC  9(05).
000610 77  WS-CON	    PIC  9(05).
000620 77  WS-NUM	    PIC  Z(04)9.
000630 77  WS-MES	    PIC  X(11) VALUE "Initialized".
       77  TODAY-DDMMYY     PIC  9(08) COMP-5.
      /
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
002450     03  WS-MID-LNE.
002460         05  WS-MCHR PIC  X(01) OCCURS 80.
002450     03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "�".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "�".
	       05  FILLER      PIC  X(01) VALUE "�".
002470	   03  WS-BOT-LNE.
002480         05  WS-BCHR PIC  X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC  X(01).
               05  WS-F-LINE
                           PIC  X(78).
               05  FILLER  PIC  X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC  X(01) OCCURS 80.

000670 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

000840 01  W02-FID.

       COPY APACFIDS.ID.

000240 COPY CONTROL.ID.

       COPY JOBAUD.ID.

       COPY JOBCST.ID.

       COPY JOBHDR.ID.

002180 COPY JOBREQ.ID.

002180 COPY JOBRMK.ID.

002180 COPY JOBWAR.ID.

       COPY PARAM.ID.

       COPY FUNCTION.WS.

       COPY W40.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
001070 SCREEN SECTION.

       COPY BLANK.CRT.

001140 01  S01.
	   03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
                                 "INITIALIZE DEBTOR FILES".
001150	   03  LINE  4 COLUMN  4 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
001160                        "Please be patient - Initializing files".
001170	   03  LINE  6 COLUMN  4 PIC X(18) USING WS-PARID.
001170	   03  LINE  8 COLUMN  4 PIC X(20) USING W02-NETWORK.
001180	   03  LINE 10 COLUMN  4 PIC X(19) USING W02-JOBCST.
001180	   03  LINE 12 COLUMN  4 PIC X(19) USING W02-JOBREQ.
001180	   03  LINE 14 COLUMN  4 PIC X(19) USING W02-JOBRMK.
	   03  LINE 16 COLUMN  4 PIC X(19) USING W02-JOBAUD.
	   03  LINE 18 COLUMN  4 PIC X(19) USING W02-JOBHDR.
	   03  LINE 20 COLUMN  4 PIC X(19) USING W02-JOBWAR.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
001240 AA000-MAIN	    SECTION.
001250 AA00.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 AA02.
041250       MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO AA02.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (16) WS-TCHR (41)
021940                              WS-TCHR (64) WS-TCHR (71).
021950       MOVE WS-G3          TO WS-MCHR (16) WS-MCHR (41)
021960                              WS-MCHR (64) WS-MCHR (71)
				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (16) WS-BCHR (41)
021980                              WS-BCHR (64) WS-BCHR (71).
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
001260	     DISPLAY CLR-SCREEN.
      *
      *    ****    P A R A M E T E R   F I L E
      *
	     MOVE "PARAM"	 TO AFID-KEY.

       AA03-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO AA03-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       AA03-READ-APACFIDS-EXIT.
	     EXIT.

       AA03A-CONTINUE.
	     MOVE AFID-PATH	 TO W02-PARAM.
	     MOVE "JOBAUD"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBAUD.
      *    *************************************************************
      *    **** J O B	C O S T I N G	-   D E T A I L S   F I L E ****
      *    *************************************************************
	     MOVE "JOBCST"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBCST.
      *    *************************************************************
      *    ****	 J O B	 C O S T I N G	 -   H E A D E R   F I L E  ****
      *    *************************************************************
	     MOVE "JOBHDR"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBHDR.
      *    *************************************************************
      *     J O B   C O S T I N G   -	R E Q U I S I T I O N	F I L E
      *    *************************************************************
	     MOVE "JOBREQ"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBREQ.
      *    *************************************************************
      *    ***	J O B	C O S T I N G	-   R E M A R K S   F I L E  ***
      *    *************************************************************
	     MOVE "JOBRMK"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBRMK.
	     MOVE "JOBWAR"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBWAR.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
001390	     DISPLAY S01.
001420 AA05.
             PERFORM AC000-PARAM.
001500       PERFORM AG000-CONTROL.
             PERFORM AE000-JOBCST.
	     PERFORM AD000-JOBREQ.
	     PERFORM AF000-JOBRMK.
	     PERFORM AH000-JOBAUD.
	     PERFORM AI000-JOBHDR.
	     PERFORM AJ000-JOBWAR.
001510       GO TO AZ000-EOJ.

       COPY ERROR.SCR.

       COPY FUNCTION.SCR.

      /
001520 AC000-PARAM             SECTION.
001530 AC000-INIT.
001570       OPEN I-O PARAM.
             MOVE 201            TO WS-PARKEY.
             READ PARAM.
           IF WS-STATUS = "00"
               GO TO AC20.
001800 AC00.
001810       MOVE WS-PARKEY      TO WS-NUM.
001820	     DISPLAY WS-NUM AT 0626 WITH FOREGROUND-COLOR 15.
       AC05.
             INITIALIZE PAR-RECORD201.
003680 AC15.
003740       WRITE PAR-RECORD201.
           IF WS-STATUS NOT = "00"
               REWRITE PAR-RECORD201.
003750       PERFORM AC00.
003690       ADD 1               TO WS-PARKEY.
003780     IF WS-PARKEY < 251
003790        GO TO AC15.
       AC20.
001580	     DISPLAY "Updated" AT 0833
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
       AC999-EXIT.
             EXIT.
      /
004830 AD000-JOBREQ	       SECTION.
004840 AD000-INIT.
004850	     OPEN OUTPUT JOBREQ.
005610	     DISPLAY WS-MES AT 1233
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
005620 AD999-EXIT.
005630       EXIT.
      /
004830 AE000-JOBCST            SECTION.
004840 AE000-INIT.
004850       OPEN OUTPUT JOBCST.
005610	     DISPLAY WS-MES AT 1033
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
005620 AE999-EXIT.
005630       EXIT.
      /
004830 AF000-JOBRMK	       SECTION.
004840 AF000-INIT.
004850	     OPEN OUTPUT JOBRMK.
005610	     DISPLAY WS-MES AT 1433
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
005620 AF999-EXIT.
005630       EXIT.
      /
004830 AG000-CONTROL           SECTION.
004840 AG000-INIT.
004850       OPEN I-O NETWORK.
004860       MOVE 1              TO WS-NETKEY.
004870       READ NETWORK.
005550	     MOVE 1		 TO DEB-JOBNO DEB-PRTREQ.
	     MOVE "8.13"	 TO DEB-JVER.
005580       REWRITE NET-DEBTOR.
005590       MOVE WS-NETKEY TO WS-NUM. 
005600	     DISPLAY WS-NUM AT 0826 WITH FOREGROUND-COLOR 15.
001580	     DISPLAY "Updated" AT 0833
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
005920 AG999-EXIT.
005930       EXIT.
      /
004830 AH000-JOBAUD	       SECTION.
004840 AH000-INIT.
004850	     OPEN OUTPUT JOBAUD.
005610	     DISPLAY WS-MES AT 1633
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
005620 AH999-EXIT.
005630       EXIT.
      /
004830 AI000-JOBHDR	       SECTION.
004840 AI000-INIT.
004850	     OPEN OUTPUT JOBHDR.
005610	     DISPLAY WS-MES AT 1833
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
005620 AI999-EXIT.
005630       EXIT.
      /
004830 AJ000-JOBWAR	       SECTION.
004840 AJ000-INIT.
004850	     OPEN OUTPUT JOBWAR.
005610	     DISPLAY WS-MES AT 2033
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
005620 AJ999-EXIT.
005630       EXIT.
      /
006170 AZ000-END               SECTION.
006180 AZ000-EOJ.
006260	     CLOSE JOBAUD
		   JOBCST
		   JOBHDR
		   JOBREQ
		   JOBRMK
		   JOBWAR
                   PARAM
006280             NETWORK.
006310 AZ02.
006320	     DISPLAY "***** Initialization Complete ****" AT 2212
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
006350 AZ05.
006390       DISPLAY CLR-SCREEN.
006480	   IF LS0-STP = 1
006490         DISPLAY "Loading Stock initialize program" AT 1212
006500	       MOVE "UTP\STP000" TO LS-NEXT-PRG
	       GO TO ZA999.
006520	   IF LS0-CRP = 1
006530         DISPLAY "Loading Creditors initialize program" AT 1212
006540	       MOVE "UTP\CRP000" TO LS-NEXT-PRG
	       GO TO ZA999.
006560	   IF LS0-GLP = 1
006570         DISPLAY "Loading G/Ledger initialize program" AT 1212
006580	       MOVE "UTP\GLP000" TO LS-NEXT-PRG
	       GO TO ZA999.
	     MOVE SPACES	 TO LS-NEXT-PRG.

       ZA999.
	     EXIT PROGRAM.
