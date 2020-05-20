      $set LINKCOUNT"248"
      ******************************************************************
      *                                                                *
      *    ******   **        *******     ****      ****      ****     *
      *   **    **  **        **    **   **  **    **  **    **  **    *
      *   **        **        **    **  **    **  **    **  **    **   *
      *   **        **        *******   **    **  **    **  **    **   *
      *   **   ***  **        **        **    **  **    **  **    **   *
      *   **    **  **        **         **  **    **  **    **  **    *
      *    ******   ********  **          ****      ****      ****     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     G / L E D G E R   I N I T I A L I Z E   P R O G R A M      *
      *                                                                *
      *     VERSION 8.13.07 - January 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.             GLP000.
000030 AUTHOR.                 J W LEMMON (APAC).
000040 DATE-WRITTEN.           JUNE 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2008
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

000070 ENVIRONMENT DIVISION.
000080 CONFIGURATION SECTION.
000090 SPECIAL-NAMES.
000100                       CONSOLE IS CRT.
000110 INPUT-OUTPUT SECTION.
000120 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

000140 COPY PARAM.SL.

000160 COPY GACCNT.SL.

000180 COPY GLTRAN.SL.

000220 COPY GLINST.SL.

       COPY LEDTRF.SL.

000240 DATA DIVISION.
000250 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.IFD.

000270 COPY PARAM.IFD.

000290 COPY GACCNT.FD.

000310 COPY GLTRAN.FD.

000350 COPY GLINST.FD.

       COPY LEDTRF.FD.

      /
000370 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000380 77  WS-NETKEY	   PIC	9(04)	 COMP-5 VALUE 1.
000380 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
000420 77  WS-GLINSTKEY    PIC	9(04)	 COMP-5 VALUE 1.
000440 77  WS-ISUB	   PIC	9(04)	 COMP-5.
000450 77  WS-S1	   PIC	9(04)	 COMP-5.
000460 77  WS-S2	   PIC	9(04)	 COMP-5.
000460 77  WS-S3	   PIC	9(04)	 COMP-5.
000460 77  WS-S4	   PIC	9(04)	 COMP-5.
000480 77  WS-NUM	   PIC	Z(04)9.
000580 77  WS-OPTION	    PIC  X(01).
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.

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

000500 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

000520 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY GACCNT.ID.

       COPY GLTRAN.ID.

       COPY GLINST.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

000690 01  W05-DATE.
000700	   03  W05-CDATE       PIC 9(08).
000710	   03  W05-DTE REDEFINES W05-CDATE.
000720         05  W05-DD      PIC 9(02).
000730         05  W05-MM      PIC 9(02).
000740	       05  W05-CC      PIC 9(02).
000740         05  W05-YY      PIC 9(02).
	   03  W05-DT2 REDEFINES W05-CDATE.
	       05  W05-DDMM    PIC 9(04).
	       05  W05-CCYY    PIC 9(04).

       COPY FUNCTION.WS.

       COPY W40.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

000750 SCREEN SECTION.

       COPY BLANK.CRT.

000820 01  S01.
	   03  LINE  2 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
                                 "INITIALIZE G/LEDGER FILES".
000830	   03  LINE  4 COLUMN  4 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
000840                        "Please be patient - Initializing files".
000850	   03  LINE  6 COLUMN  4 PIC X(18) USING WS-PARID.
000860	   03  LINE  8 COLUMN  4 PIC X(19) USING W02-GACCNT.
000870	   03  LINE 10 COLUMN  4 PIC X(19) USING W02-GLTRAN.
000890	   03  LINE 12 COLUMN  4 PIC X(19) USING W02-GLINST.
	   03  LINE 14 COLUMN  4 PIC X(19) USING W02-LEDTRF.

000900 01  S02.
000910	   03  LINE  6 COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
000920                           VALUE "Updated".

000930 01  S03.
000940	   03  LINE  8 COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
000950                           VALUE "Initialized".

000960 01  S04.
000970	   03  LINE 10 COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
000980                           VALUE "Initialized".

001020 01  S06.
001030	   03  LINE 12 COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
001040                           VALUE "Initialized".

001020 01  S07.
001030	   03  LINE 14 COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
001040                           VALUE "Initialized".
001050	   03  LINE 16 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
001060                           VALUE "Initialization Complete".
001070	   03  LINE 18 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
001080                           VALUE "**** Backup files ****".

       COPY ERROR.CRT.

      /
001090 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
001100 AA000		SECTION.
001110 AA00.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 AA01.
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 	 TO WS-TCH(WS-S1)  WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO AA01.
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

       AA01-READ-APACFIDS.
000030	     READ APACFIDS WITH IGNORE LOCK
	       KEY IS AFID-KEY.
	   IF WS-STATUS = "00"
	       GO TO AA01-READ-APACFIDS-EXIT.
006260	     STRING "Missing " DELIMITED SIZE
		     AFID-KEY DELIMITED BY " "
		     " file ID - Status " DELIMITED SIZE
		     WS-STATUS DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370	     STOP RUN.

       AA01-READ-APACFIDS-EXIT.
	     EXIT.

       AA01A-CONTINUE.
	     MOVE AFID-PATH	 TO W02-PARAM.
      *    *************************************************************
      *    *** G E N E R A L   L E D G E R   A C C O U N T   F I L E ***
      *    *************************************************************
	     MOVE "GACCNT"	 TO AFID-KEY.
	     PERFORM AA01-READ-APACFIDS THRU AA01-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GACCNT.
      *    *************************************************************
      *    ****	G / L	R E C U R R I N G   T R A N S A C T I O N S ****
      *    *************************************************************
	     MOVE "GLINST"	 TO AFID-KEY.
	     PERFORM AA01-READ-APACFIDS THRU AA01-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLINST.
      *    *************************************************************
      *    G E N E R A L   L E D G E R	 T R A N S A C T I O N	 F I L E
      *    *************************************************************
	     MOVE "GLTRAN"	 TO AFID-KEY.
	     PERFORM AA01-READ-APACFIDS THRU AA01-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GLTRAN.
      *    *************************************************************
      *    **** 	  I N T E G R A T I O N	  F I L E	    ****
      *    *************************************************************
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM AA01-READ-APACFIDS THRU AA01-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM AA01-READ-APACFIDS THRU AA01-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
001130	     DISPLAY S01.
001140       OPEN INPUT PARAM.
001150	     MOVE 1		 TO WS-PARKEY.

001160 AA02.
001170       READ PARAM.
001180     IF WS-STAT1 = "9"
001190         IF WS-STAT2 = 68
001200             GO TO AA02.
001220	     MOVE PAR-DMY	 TO W05-CDATE.

001240 AA03.
001250       READ PARAM.
001260     IF WS-STAT1 = "9"
001270         IF WS-STAT2 = 68
001280             GO TO AA03.
             CLOSE PARAM.
001350       DISPLAY S02.
001360       PERFORM AC000.
001370       DISPLAY S03.
001380       PERFORM AD000.
001390       DISPLAY S04.
001420       PERFORM AF000.
001430       DISPLAY S06.
001420       PERFORM AG000.
001430       DISPLAY S07.
	     MOVE SPACES	 TO LS-NEXT-PRG.

       AA999.
	     EXIT PROGRAM.

       COPY ERROR.SCR.

       COPY FUNCTION.SCR.

001550 AC000	       SECTION.
001560 AC00.
             OPEN I-O NETWORK.
001570       OPEN OUTPUT GACCNT.
             MOVE 4              TO WS-NETKEY.
             READ NETWORK.
             INITIALIZE NET-LEDGER.
001610	     MOVE 02		 TO G-OPEN G-YEAR-END
	     SUBTRACT 1 FROM W05-CCYY
				 GIVING G-LAST-YE.
001630	     MOVE 1		 TO G-GLINST G-BATCH.
	     MOVE "8.13"	 TO G-VER.
             REWRITE NET-RECORD.
001670       CLOSE GACCNT
                   NETWORK.
001680 AC99.
001690       EXIT.

001700 AD000           SECTION.
001710 AD00.
001720       OPEN OUTPUT GLTRAN.
001770       CLOSE GLTRAN.
001780 AD99.
001790       EXIT.

001970 AF000	       SECTION.
001980 AF00.
001990       OPEN OUTPUT GLINST.
002040       CLOSE GLINST.
002050 AF99.
002060       EXIT.

001970 AG000           SECTION.
001980 AG00.
001990       OPEN OUTPUT LEDTRF.
002040       CLOSE LEDTRF.
002050 AG99.
002060       EXIT.
