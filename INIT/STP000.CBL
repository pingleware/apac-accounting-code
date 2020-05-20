      $set LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******     ****      ****      ****     *
      *   **    **     **     **    **   **  **    **  **    **  **    *
      *   **           **     **    **  **    **  **    **  **    **   *
      *    ******      **     *******   **    **  **    **  **    **   *
      *         **     **     **        **    **  **    **  **    **   *
      *   **    **     **     **         **  **    **  **    **  **    *
      *    ******      **     **          ****      ****      ****     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S T O C K   I N I T I A L I Z E   P R O G R A M            *
      *                                                                *
      *     VERSION 8.13.08 - August 2008			       *
      * 							       *
      ******************************************************************
      *
      * April 1990     - Multi-terminal and minor bugs fixed.
      *                  Various cosmetic changes and enhancements.
      *
      * April 1991     - Allow for VAT 
      *                  Changed STOCK to INDEXED file
      *                  Allow for guarantees - new file
      *                  Introduced network file (STATISTICS)
      *
      * October 1995   - Creditor (Purchase units)
      *
      * June 1997      - Removed disk drive location
      *
      * September 1997 - Separate Sub-Directory per set of books
      *
      * November 1997  - Allow for internal Bar Codes
      *
      * July 1998      - Year 2000 compliance
      * 		 Allow for extended code
      * 		 Allow for packs
      * 		 Allow for cases
      * 		 Allow for weight (Convertion factors)
      *
      * August 2008    - New file (STKALT) - Stock alternate index
      * 		 included for lookups, using any word con-
      * 		 tained in the Stock description.
      *
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STP000.
000030 AUTHOR.         J W LEMMON (CSS).
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

       COPY CARDEX.SL.

       COPY CONTROL.SL.

000140 COPY GARTEE.SL.

000120 COPY PARAM.SL.

000300 COPY PURDEX.SL.

000200 COPY SPARTS.SL.

000140 COPY STKALT.SL.

       COPY STKDEX.SL.

000200 COPY STKMEM.SL.

000140 COPY STOCK.SL.

       COPY WARHSE.SL.

       COPY WSTOCK.SL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CARDEX.FD.

000270 COPY CONTROL.IFD.

000140 COPY GARTEE.FD.

000250 COPY PARAM.IFD.

000300 COPY PURDEX.FD.

000330 COPY SPARTS.FD.

000140 COPY STKALT.GFD.

       COPY STKDEX.FD.

000200 COPY STKMEM.FD.

000270 COPY STOCK.GFD.

000280 COPY WARHSE.FDE.

000280 COPY WSTOCK.FDE.

      /
000350 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000360 77  WS-SUB-1	   PIC	9(04)	  COMP-5.
000380 77  WS-PARKEY       PIC  9(06)     COMP-5.
000380 77  WS-NETKEY       PIC  9(06)     COMP-5.
000420 77  WS-OPTION       PIC  X(01).
000430 77  WS-STK          PIC  9(05).
000460 77  WS-DEM          PIC  9(01).
000280 77  WS-S            PIC  9(02)  COMP-5.
000280 77  WS-S1           PIC  9(02)  COMP-5.
000280 77  WS-S2	   PIC	9(02)  COMP-5.
000280 77  WS-S3	   PIC	9(02)  COMP-5.
000280 77  WS-S4	   PIC	9(02)  COMP-5.
       77  TODAY-DDMMYY	       PIC  9(06) COMP-5.
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

000480 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

000500 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CARDEX.ID.

       COPY CONTROL.ID.

       COPY GARTEE.ID.

       COPY PARAM.ID.

       COPY PURDEX.ID.

       COPY SPARTS.ID.

000140 COPY STKALT.ID.

       COPY STKDEX.ID.

000200 COPY STKMEM.ID.

       COPY STOCK.ID.

000360 COPY WARHSE.ID.

000360 COPY WSTOCK.ID.

       COPY FUNCTION.WS.

       COPY W40.WS.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
000670 SCREEN SECTION.

       COPY BLANK.CRT.

000740 01  S01.
	   03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
                                 "INITIALIZE STOCK FILES".
000750	   03  LINE  4 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
000760                        "Please be patient - Initializing files".
000770	   03  LINE  6 COLUMN 12 PIC  X(18) USING WS-PARID.
000780	   03  LINE  7 COLUMN 12 PIC  X(19) USING W02-STOCKF.
000790	   03  LINE  8 COLUMN 12 PIC  X(19) USING W02-GARTEE.
000800	   03  LINE  9 COLUMN 12 PIC  X(19) USING W02-SPARTS.
000800	   03  LINE 10 COLUMN 12 PIC  X(19) USING W02-STKMEM.
	   03  LINE 11 COLUMN 12 PIC  X(21) USING W02-CARDEX.
	   03  LINE 12 COLUMN 12 PIC  X(21) USING W02-PURDEX.
	   03  LINE 13 COLUMN 12 PIC  X(21) USING W02-STKDEX.
	   03  LINE 14 COLUMN 12 PIC  X(19) USING W02-WARHSE.
	   03  LINE 15 COLUMN 12 PIC  X(19) USING W02-WSTOCK.

000820 01  S02.
000830	   03  FOREGROUND-COLOR 3 HIGHLIGHT
	       BACKGROUND-COLOR 5 VALUE "Updated".

000850 01  S03.
000860	   03  FOREGROUND-COLOR 3 HIGHLIGHT
	       BACKGROUND-COLOR 5 VALUE "Initialized".

       01  S04.
000970	   03  LINE 17 COLUMN 12
		       FOREGROUND-COLOR 3 HIGHLIGHT
		       BACKGROUND-COLOR 5
		       VALUE "**** Initialization Complete ****".

       COPY ERROR.CRT.

      /
000990 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
001000 AA000			SECTION.
001010 AA00.
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
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
012020	     DISPLAY CLR-SCREEN.
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
      *    **** 	    G U A R A N T E E	F I L E		    ****
      *    *************************************************************
	     MOVE "GARTEE"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GARTEE.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    ****    P U R C H A S E S   -   C A R D E X	 F I L E    ****
      *    *************************************************************
	     MOVE "PURDEX"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PURDEX.
      *	   *************************************************************
      *    *** S T O C K   ( I V E N T O R Y )	 P A R T S   F I L E ***
      *    *************************************************************
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
      *    *************************************************************
      *    ****	S T O C K   A L T E R N A T E	I N D E X   F I L E ****
      *    *************************************************************
	     MOVE "STKALT"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKALT.
      *    *************************************************************
      *    ****  A D J U S T M E N T   -   H I S T O R Y   F I L E  ****
      *    *************************************************************
	     MOVE "STKDEX"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKDEX.
      *    *************************************************************
      *    ***	S T O C K   ( I V E N T O R Y )   M E M O   F I L E  ***
      *    *************************************************************
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
      *    *************************************************************
      *    ****      S T O C K	 ( I V E N T O R Y )   F I L E	    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
      *    *************************************************************
      *    **** 	    W A R E H O U S E	F I L E		    ****
      *    *************************************************************
	     MOVE "WARHSE"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WARHSE.
      *    *************************************************************
      *    W A R E H O U S E  -  S T O C K  ( I V E N T O R Y )  F I L E
      *    *************************************************************
	     MOVE "WSTOCK"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-WSTOCK.
001040	     OPEN I-O PARAM.
001050	     MOVE 1		 TO WS-PARKEY.
001060 AA05.
001070       READ PARAM.
001080     IF WS-STAT1 = "9"
001090         IF WS-STAT2 = 68
001100             GO TO AA05.
001110 AA20.
001030	     DISPLAY S01.
001200       PERFORM AC000-PARAM.
001210       PERFORM AD000-STOCK.
001240       PERFORM AE000-GARTEE.
001240       PERFORM AF000-SPARTS.
001240       PERFORM AG000-STKMEM.
	     PERFORM AH000-CARDEX.
	     PERFORM AI000-PURDEX.
	     PERFORM AJ000-STKDEX.
	     PERFORM AK000-WARHSE.
	     PERFORM AL000-WSTOCK.
001250	     GO TO AZ000-EOJ.

       COPY ERROR.SCR.

       COPY FUNCTION.SCR.

      /
001260 AC000-PARAM             SECTION.
001270 AC00.
001280	     MOVE 8		 TO WS-PARKEY.
	     MOVE LOW-VALUES	 TO PAR-RECORD8.
001290	     MOVE "COST OF SALES"
				 TO PAR-CSHD.
001300	     MOVE ZERO		 TO PAR-CSHT PAR-CMTD
				    PAR-CYTD PAR-ADJMTD.
001310       REWRITE PAR-RECORD1.
001320	     DISPLAY S02 AT 0634.
001330 AC999-EXIT.
001340       EXIT.
      /
001350 AD000-STOCK            SECTION.
001360 AD000-INIT.
001370       OPEN OUTPUT STOCK.
             OPEN I-O NETWORK.
	     MOVE 3		 TO WS-NETKEY.

       AD05.
             READ NETWORK.
001080	   IF WS-STAT1 = "9"
001090         IF WS-STAT2 = 68
001100             GO TO AD05.
001410	     MOVE ZERO		 TO STK-ADJMTD STK-TOTAL
				    STK-CMTD   STK-SYTD
001440				    STK-ADJYTD STK-REDGL
				    STK-CYTD   STK-SMTD
				    STK-REDMTD STK-REDYTD.
001470	     MOVE 1		 TO STK-WEEK.
001480	     MOVE "PASSWORD"	 TO STK-PASSWORD.
	     MOVE "8.13"	 TO STK-VER.
001510       REWRITE NET-STOCK.
001520	     DISPLAY S03 AT 0734.
	     OPEN OUTPUT STKALT.
	     CLOSE STKALT.
	     OPEN I-O STKALT.

001530 AD999-EXIT.
001540       EXIT.
      /
001750 AE000-GARTEE           SECTION.
001760 AF000-INIT.
001770       OPEN OUTPUT GARTEE.
001790	     DISPLAY S03 AT 0834.
001830 AF999-EXIT.
001840       EXIT.
      /
001750 AF000-SPARTS           SECTION.
001760 AF000-INIT.
001770       OPEN OUTPUT SPARTS.
001790	     DISPLAY S03 AT 0934.
001830 AF999-EXIT.
001840       EXIT.
      /
001750 AG000-STKMEM	      SECTION.
001760 AG000-INIT.
001770       OPEN OUTPUT STKMEM.
001790	     DISPLAY S03 AT 1034.
001830 AG999-EXIT.
001840       EXIT.
      /
001750 AH000-CARDEX	      SECTION.
001760 AH000-INIT.
001770	     OPEN OUTPUT CARDEX.
001790	     DISPLAY S03 AT 1134.
001830 AH999-EXIT.
001840       EXIT.
      /
001750 AI000-PURDEX	      SECTION.
001760 AI000-INIT.
001770	     OPEN OUTPUT PURDEX.
001790	     DISPLAY S03 AT 1234.
001830 AI999-EXIT.
001840       EXIT.
      /
001750 AJ000-STKDEX	      SECTION.
001760 AJ000-INIT.
001770	     OPEN OUTPUT STKDEX.
001790	     DISPLAY S03 AT 1334.
001830 AJ999-EXIT.
001840       EXIT.
      /
001750 AK000-WARHSE	      SECTION.
001760 AK000-INIT.
001770	     OPEN OUTPUT WARHSE.
001790	     DISPLAY S03 AT 1434.
001830 AK999-EXIT.
001840       EXIT.
      /
001750 AL000-WSTOCK	      SECTION.
001760 AL000-INIT.
001770	     OPEN OUTPUT WSTOCK.
001790	     DISPLAY S03 AT 1534.
             DISPLAY S04.
001830 AL999-EXIT.
001840       EXIT.
      /
002070 AZ000-END               SECTION.
002080 AZ000-EOJ.
002090       CLOSE PARAM.
002100       CLOSE STOCK.
002120       CLOSE NETWORK.
002120	     CLOSE STKALT.
002120       CLOSE STKMEM.
002120       CLOSE SPARTS.
002120       CLOSE GARTEE.
002120	     CLOSE CARDEX.
002120	     CLOSE PURDEX.
	     CLOSE STKDEX.
	     CLOSE WARHSE.
	     CLOSE WSTOCK.

002130 AZ05.
002140       DISPLAY CLR-SCREEN.
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
