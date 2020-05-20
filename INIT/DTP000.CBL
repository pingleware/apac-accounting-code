      $set LINKCOUNT"248"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******     ****      ****      ****     *
      *   **    **     **     **    **   **  **    **  **    **  **    *
      *   **    **     **     **    **  **    **  **    **  **    **   *
      *   **    **     **     *******   **    **  **    **  **    **   *
      *   **    **     **     **        **    **  **    **  **    **   *
      *   **    **     **     **         **  **    **  **    **  **    *
      *   *******      **     **          ****      ****      ****     *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     D E B T O R ' S   I N I T I A L I Z E   P R O G R A M      *
      *                                                                *
      *     VERSION 8.13.09 - November 2008			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  May 2006	- New field included in Accounts Receivable    *
      * 		  for recording number of outstanding Jobs     *
      * 		  linked to the account. Additional filler     *
      * 		  added for future expansion.		       *
      * 							       *
      *  Feb 2007	- New file (DEBDEP/AR_REST.DAT) - Debtor sales *
      * 		  restrictions file has been included which    *
      * 		  will allow for departments to de excluded or *
      * 		  only selected departments allowed. This will *
      * 		  allow the user the abillity to allow or bar  *
      * 		  sales of selected departments.	       *
      * 							       *
      *  Jan 2008	- New file (DEBALT) - Debtor alternate index   *
      * 		  included for lookups, using any word con-    *
      * 		  tained in the Debtor name.		       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTP000.
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

       COPY CARDEX.SL.

000240 COPY CONTROL.SL.

       COPY DBFOTO.SL.

000180 COPY DBTRAN.SL.

000160 COPY DEBALT.SL.

000160 COPY DEBDEP.SL.

000160 COPY DEBTOR.SL.

000140 COPY DEBMEM.SL.

000240 COPY INVOIC.SL.

000120 COPY PARAM.SL.

       COPY SORDER.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CARDEX.FD.

000240 COPY CONTROL.IFD.

       COPY DBFOTO.FD.

000350 COPY DBTRAN.GFD.

000330 COPY DEBALT.GFD.

000330 COPY DEBDEP.GFD.

000330 COPY DEBTOR.GFD.

000140 COPY DEBMEM.FD.

000410 COPY INVOIC.FD.

000290 COPY PARAM.IFD.

       COPY SORDER.FD.

      /
000430 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000440 77  WS-S1	    PIC  9(04)	  COMP-5.
000450 77  WS-S2	    PIC  9(04)	  COMP-5.
000460 77  WS-S3	    PIC  9(04)	  COMP-5.
000470 77  WS-S4	    PIC  9(04)	  COMP-5.
000480 77  WS-PARKEY	    PIC  9(04)	  COMP-5.
000520 77  WS-NETKEY	    PIC  9(04)	  COMP-5.
000620 77  WS-NUM	    PIC  Z(04)9.
000580 77  WS-OPTION	    PIC  X(01).
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

000670 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

000840 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CARDEX.ID.

000240 COPY CONTROL.ID.

       COPY DBFOTO.ID.

       COPY DBTRAN.ID.

000330 COPY DEBALT.ID.

000330 COPY DEBDEP.ID.

       COPY DEBTOR.ID.

000140 COPY DEBMEM.ID.

       COPY INVOIC.ID.

       COPY PARAM.ID.

       COPY SORDER.ID.

       COPY W12.WS.

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
001180	   03  LINE  8 COLUMN  4 PIC X(19) USING W02-DEBTOR.
001200	   03  LINE 10 COLUMN  4 PIC X(19) USING W02-DBTRAN.
001220	   03  LINE 12 COLUMN  4 PIC X(20) USING W02-NETWORK.
001220	   03  LINE 14 COLUMN  4 PIC X(19) USING W02-INVOIC.
	   03  LINE 16 COLUMN  4 PIC X(19) USING W02-SORDER.
	   03  LINE 18 COLUMN  4 PIC X(19) USING W02-DEBMEM.
	   03  LINE 20 COLUMN  4 PIC X(21) USING W02-CARDEX.
	   03  LINE 22 COLUMN  4 PIC X(19) USING W02-DBFOTO.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
001240 AA000-MAIN	     SECTION.
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
				 TO TODAY-DDMMYY W12-TODAY.
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
      *    *************************************************************
      *    ****       S A L E S	  -   C A R D E X   F I L E	    ****
      *    *************************************************************
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
      *    *************************************************************
      *    ****      D E B T O R   C O N T R A C T S   F I L E	    ****
      *    *************************************************************
	     MOVE "DBFOTO"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBFOTO.
      *    *************************************************************
      *    ****    D E B T O R	 T R A N S A C T I O N	 F I L E    ****
      *    *************************************************************
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN.
      *    *************************************************************
      *    *** D E B T O R   A L T E R N A T E	 I N D E X   F I L E ***
      *    *************************************************************
	     MOVE "DEBALT"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBALT.
      *    *************************************************************
      *    **** D E B T O R   S A L E S   R E S T R I C T   F I L E ****
      *    *************************************************************
	     MOVE "DEBDEP"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBDEP.
      *    *************************************************************
      *    **** 	  D E B T O R	M E M O	  F I L E	    ****
      *    *************************************************************
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
      *    *************************************************************
      *    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
      *    *************************************************************
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
      *    *************************************************************
      *    ****        D E B T O R   I N V O I C E   F I L E	    ****
      *    *************************************************************
	     MOVE "INVOIC"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-INVOIC.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    **** 	   S A L E S   O R D E R   F I L E	    ****
      *    *************************************************************
	     MOVE "SORDER"	 TO AFID-KEY.
	     PERFORM AA03-READ-APACFIDS THRU AA03-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SORDER.
001390	     DISPLAY S01.
001420 AA05.
             PERFORM AC000-PARAM.
001460       PERFORM AD000-DEBTOR.
001480       PERFORM AF000-DBTRAN.
001500       PERFORM AG000-CONTROL.
001500       PERFORM AH000-INVOICE.
             PERFORM AE000-SORDER.
             PERFORM AI000-DEBMEM.
	     PERFORM AJ000-CARDEX.
	     PERFORM AK000-DBFOTO.
001510       GO TO AZ000-EOJ.

       COPY ERROR.SCR.

       COPY FUNCTION.SCR.

001520 AC000-PARAM	       SECTION.
001530 AC000-INIT.
001570       OPEN I-O PARAM.
             MOVE 61             TO WS-PARKEY.
001800 AC00.
001810       MOVE WS-PARKEY      TO WS-NUM.
001820	     DISPLAY WS-NUM AT 0626 WITH FOREGROUND-COLOR 15.
       AC05.
             INITIALIZE PAR-RECORD61.
003680 AC15.
003740       REWRITE PAR-RECORD61.
003750       PERFORM AC00.
003690       ADD 1               TO WS-PARKEY.
003780     IF WS-PARKEY < 101
003790        GO TO AC15.
      /
004830 AD000-DEBTOR            SECTION.
004840 AD000-INIT.
004850       OPEN OUTPUT DEBTOR.
	     CLOSE DEBTOR.
	     OPEN I-O DEBTOR.
005610	     DISPLAY WS-MES AT 0833
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     OPEN OUTPUT DEBALT.
	     CLOSE DEBALT.
	     OPEN I-O DEBALT.
	     OPEN OUTPUT DEBDEP.
	     CLOSE DEBDEP.
	     OPEN I-O DEBDEP.

005620 AD999-EXIT.
005630       EXIT.
      /
004830 AE000-SORDER            SECTION.
004840 AE000-INIT.
004850       OPEN OUTPUT SORDER.
	     CLOSE SORDER.
	     OPEN I-O SORDER.
005610	     DISPLAY WS-MES AT 1633
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
005620 AE999-EXIT.
005630       EXIT.
      /
005850 AF000-DBTRAN           SECTION.
005860 AF00.
005870       OPEN OUTPUT DBTRAN.
	     CLOSE DBTRAN.
	     OPEN I-O DBTRAN.
005880	     DISPLAY WS-MES AT 1033
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
005920 AF999-EXIT.
005930        EXIT.
      /
004830 AG000-CONTROL           SECTION.
004840 AG000-INIT.
004850       OPEN I-O NETWORK.
004860       MOVE 1              TO WS-NETKEY.
004870       INITIALIZE NET-DEBTOR.
005550       MOVE 1              TO DEB-BATCH  
                                    DEB-INVOICE
                                    DEB-ORDER
                                    DEB-RECEIPT
				    DEB-JOBNO.
	     MOVE ZERO		 TO DEB-CUR-DTE
				    DEB-30D-DTE
				    DEB-60D-DTE
				    DEB-90D-DTE
				    DEB-120-DTE.
005570       MOVE 2              TO DEB-AUDIT.
	     MOVE "8.13"	 TO DEB-VER.
005580       REWRITE NET-DEBTOR.
005590	     MOVE WS-NETKEY	 TO WS-NUM.
005600	     DISPLAY WS-NUM AT 1226 WITH FOREGROUND-COLOR 15.
001580	     DISPLAY "Updated" AT 1233
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
005920 AG999-EXIT.
005930        EXIT.
      /
006050 AH000-INVOICE           SECTION.
006060 AH00.
006070       OPEN OUTPUT INVOICE.
             CLOSE INVOICE.
             OPEN I-O INVOICE.
006130	     DISPLAY WS-MES AT 1433
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
006140       GO TO AH999-EXIT.
006150 AH999-EXIT.
006160        EXIT.
      /
004830 AI000-DEBMEM            SECTION.
004840 AI000-INIT.
004850       OPEN OUTPUT DEBMEM.
	     CLOSE DEBMEM.
	     OPEN I-O DEBMEM.
005610	     DISPLAY WS-MES AT 1833
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
005620 AI999-EXIT.
005630       EXIT.
      /
004830 AJ000-CARDEX	       SECTION.
004840 AJ000-INIT.
004850	     OPEN OUTPUT CARDEX.
	     CLOSE CARDEX.
	     OPEN I-O CARDEX.
005610	     DISPLAY WS-MES AT 2033
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
005620 AJ999-EXIT.
005630       EXIT.
      /
004830 AK000-DBFOTO	       SECTION.
004840 AK000-INIT.
004850	     OPEN OUTPUT DBFOTO.
	     CLOSE DBFOTO.
	     OPEN I-O DBFOTO.
005610	     DISPLAY WS-MES AT 2233
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
005620 AK999-EXIT.
005630       EXIT.
      /
006170 AZ000-END               SECTION.
006180 AZ000-EOJ.
006260	     CLOSE CARDEX
		   DBFOTO
		   DEBALT
		   DEBDEP
		   DEBTOR
006280             DEBMEM
006280             DBTRAN
006280             PARAM
006280             NETWORK
006300             INVOICE
                   SORDER.
006310 AZ02.
006320       DISPLAY "***** Initialization Complete ****" AT 2212 
006330		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
006350 AZ05.
006390       DISPLAY CLR-SCREEN.
006480	   IF LS0-JCP = 1
006490         DISPLAY "Loading Job Costing initialize program" AT 1212
006500	       MOVE "UTP\JCP000" TO LS-NEXT-PRG
	       GO TO AZ999.
006480	   IF LS0-STP = 1
006490         DISPLAY "Loading Stock initialize program" AT 1212
006500	       MOVE "UTP\STP000" TO LS-NEXT-PRG
	       GO TO AZ999.
006520	   IF LS0-CRP = 1
006530         DISPLAY "Loading Creditors initialize program" AT 1212
006540	       MOVE "UTP\CRP000" TO LS-NEXT-PRG
	       GO TO AZ999.
006560	   IF LS0-GLP = 1
006570         DISPLAY "Loading G/Ledger initialize program" AT 1212
006580	       MOVE "UTP\GLP000" TO LS-NEXT-PRG
	       GO TO AZ999.
	     MOVE SPACES	 TO LS-NEXT-PRG.

       AZ999.
	     EXIT PROGRAM.
