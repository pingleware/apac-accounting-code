      $set LINKCOUNT"248"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******     ****      ****      ****     *
      *   **    **  **    **  **    **   **  **    **  **    **  **    *
      *   **        **    **  **    **  **    **  **    **  **    **   *
      *   **        ******    *******   **    **  **    **  **    **   *
      *   **        **   **   **        **    **  **    **  **    **   *
      *   **    **  **    **  **         **  **    **  **    **  **    *
      *    ******   **    **  **          ****      ****      ****     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   I N I T I A L I Z E   P R O G R A M      *
      *                                                                *
      *     VERSION 8.13.07 - January 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     CRP000.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1982.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1982 - 2008
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

       COPY CONTROL.SL.

000140 COPY CREDIT.SL.

000180 COPY CRTRAN.SL.

000120 COPY PARAM.SL.

       COPY PDCHEQ.SL.

000200 COPY PORDER.SL.

000240 DATA DIVISION.
000250 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.IFD.

000290 COPY CREDIT.GFD.

000330 COPY CRTRAN.FD.

000270 COPY PARAM.IFD.

       COPY PDCHEQ.FD.

000350 COPY PORDER.GFD.

      /
000390 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000400 77  WS-S1	       PIC  9(04)     COMP-5.
000410 77  WS-S2	       PIC  9(04)     COMP-5.
000420 77  WS-S3	       PIC  9(04)     COMP-5.
000430 77  WS-S4	       PIC  9(04)     COMP-5.
000450 77  WS-PARKEY	       PIC  9(04)     COMP-5.
000460 77  WS-NETKEY	       PIC  9(04)     COMP-5.
000520 77  WS-NUM	       PIC  Z(04)9.
000540 77  WS-DEM	       PIC  9(01).
000580 77  WS-OPTION	    PIC  X(01).
       77  TODAY-DDMMYY	       PIC  9(08) COMP-5.
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

000560 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

000580 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY CREDIT.ID.

       COPY CRTRAN.ID.

       COPY PARAM.ID.

       COPY PDCHEQ.ID.

       COPY PORDER.ID.

       COPY FUNCTION.WS.

       COPY W40.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
000790 SCREEN SECTION.

       COPY BLANK.CRT.

000860 01  S01.
	   03  LINE  2 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
                                 "INITIALIZE CREDITOR FILES".
000870	   03  LINE  4 COLUMN  4 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
000880                        "Please be patient - Initializing files".
000890	   03  LINE  6 COLUMN  4 PIC X(18) USING W02-PARAM.
000900	   03  LINE  8 COLUMN  4 PIC X(19) USING W02-CREDIT.
000920	   03  LINE 10 COLUMN  4 PIC X(19) USING W02-CRTRAN.
000930	   03  LINE 12 COLUMN  4 PIC X(19) USING W02-PORDER.
000940	   03  LINE 14 COLUMN  4 PIC X(20) USING W02-NETWORK.
000940	   03  LINE 16 COLUMN  4 PIC X(19) USING W02-PDCHEQ.

000950 01  S02.
000960	   03  LINE  6 COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
000970                           VALUE "Updated".

000980 01  S03.
000990	   03  LINE  8 COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
001000                           VALUE "Initialized".

001040 01  S05.
001050	   03  LINE 10 COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
001060                           VALUE "Initialized".
001070 01  S06.
001080	   03  LINE 12 COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
001090                           VALUE "Initialized".

001100 01  S07.
001110	   03  LINE 14 COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
001120                           VALUE "Initialized".

001100 01  S08.
001110	   03  LINE 16 COLUMN 33 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5
001120                           VALUE "Initialized".
001130	   03  LINE 18 COLUMN 12 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 5 VALUE
001140				 "**** Initialization Complete ****".

       COPY ERROR.CRT.

      /
001150 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
001160 AA000		  SECTION.
001170 AA00.
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
001180	     DISPLAY CLR-SCREEN.
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
      *    *************************************************************
      *    *** C R E D I T O R	 A C C O U N T	 ( A / P )   F I L E ***
      *    *************************************************************
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
      *    *************************************************************
      *    ( A / P )   C R E D I T O R	 T R A N S A C T I O N	 F I L E
      *    *************************************************************
	     MOVE "CRTRAN"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRTRAN.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    ****        P U R C H A S E	 O R D E R   F I L E	    ****
      *    *************************************************************
	     MOVE "PORDER"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PORDER.
	     MOVE "PDCHEQ"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PDCHEQ.
001390	     DISPLAY S01.
001410 AA05.
001420       PERFORM AC000-PARAM.
001430       PERFORM AD000-CREDIT.
001450       PERFORM AF000-CTRANS.
001460       PERFORM AB000-PORDER.
001470       PERFORM CA000-CONTROL
001460	     PERFORM AE000-PDCHEQ.
001480	     GO TO ZA000-EOJ.

       COPY ERROR.SCR.

       COPY FUNCTION.SCR.

001490 AB000-PORDER	      SECTION.
001500 AB00.
001510       OPEN OUTPUT PORDER.
             CLOSE PORDER.
             OPEN I-O PORDER
001590       DISPLAY S06.
001610 AB999-EXIT.
001620       EXIT.
      /
001630 AC000-PARAM             SECTION.
001640 AC00.
001190       OPEN I-O PARAM.
001200       MOVE 6              TO WS-PARKEY.
       AC02.
             MOVE WS-PARKEY      TO WS-NUM.
	     DISPLAY WS-NUM AT 0626.
001210 AC05.
001220       READ PARAM.
001230     IF WS-STAT1 = "9"
001240         IF WS-STAT2 = 68
001250             GO TO AC05.
	     MOVE ZERO		 TO PAR-RAILD PAR-DISCD
				    PAR-RAILM PAR-DISCM.
             REWRITE PAR-RECORD6.
001700       DISPLAY S02.
001710 AC999-EXIT.
001720       EXIT.
      /
001730 AD000-CREDIT            SECTION.
001740 AD00.
001750       OPEN OUTPUT CREDIT.
             CLOSE CREDIT.
             OPEN I-O CREDIT.
002270       DISPLAY S03.
002280 AD999-EXIT.
002290       EXIT.
      /
001490 AE000-PDCHEQ	      SECTION.
001500 AE00.
001510	     OPEN OUTPUT PDCHEQ.
	     CLOSE PDCHEQ.
	     OPEN I-O PDCHEQ
001590	     DISPLAY S08.
001610 AE999-EXIT.
001620       EXIT.
      /
002500 AF000-CTRANS            SECTION.
002510 AF00.
002520       OPEN OUTPUT CTRANS.
	     CLOSE CTRANS.
	     OPEN I-O CTRANS
002530	     DISPLAY S05.
002570 AF999-EXIT.
002580       EXIT.
      /
002590 CA000-CONTROL           SECTION.
002600 CA00.
004850       OPEN I-O NETWORK.
004860       MOVE 2              TO WS-NETKEY.
004870       INITIALIZE NET-CREDITOR.
005550       MOVE 1              TO P-CRTRAN.
	     MOVE "8.13"	 TO P-VER.
005580       REWRITE NET-CREDITOR.
005590	     MOVE WS-NETKEY	 TO WS-NUM.
005600	     DISPLAY WS-NUM AT 1426 WITH FOREGROUND-COLOR 15.
002720       DISPLAY S07.
002730 CA999-EXIT.
002740       EXIT.
      /
002750 ZA000-END               SECTION.
002760 ZA000-EOJ.
002770	     CLOSE PARAM
002780		   CREDIT
002800		   CTRANS
002810		   PDCHEQ
002810		   PORDER
002820		   NETWORK.
002830 ZA05.
002840       DISPLAY CLR-SCREEN.
002850	   IF LS0-GLP = 1
002860	       DISPLAY "Loading General ledger initialize" AT 1212
002870	       MOVE "UTP\GLP000" TO LS-NEXT-PRG
	       GO TO ZA999.
	     MOVE SPACES	 TO LS-NEXT-PRG.

       ZA999.
	     EXIT PROGRAM.
