      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	  ****	   ******      **      *
      *   **	**  **	  **  **    **	 **  **   **	**    ***      *
      *   **	    **	  **  **    **	**    **  **	**     **      *
      *   **	    ******    *******	**    **   *******     **      *
      *   **	    **	 **   **	**    **	**     **      *
      *   **	**  **	  **  **	 **  **   **	**     **      *
      *    ******   **	  **  **	  ****	   ******    ******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     C R E D I T O R   C L E A R   B A L A N C E S	       *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.	       CRP091.
000030 AUTHOR.		       J.W.LEMMON (CSS).
000040 DATE-WRITTEN.	       JUNE, 1983.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1983 - 2011
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
000080		       CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000110 INPUT-OUTPUT SECTION.
000120 FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

000160 COPY CREDIT.SL.

000180 COPY CRTRAN.SL.

       COPY PARAM.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

000310 COPY CREDIT.FDE.

000180 COPY CRTRAN.FDE.

000290 COPY PARAM.FDE.

000410 WORKING-STORAGE SECTION.
       77  WS-COPYRIGHT    PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
000420 77  WS-PARKEY	   PIC	9(04)	 COMP-5 VALUE 1.
000440 77  WS-CREKEY       PIC  9(04)    COMP-5 VALUE 1.
003300 77  WS-CTRKEY       PIC  9(06)    COMP-5 VALUE 1.
000450 77  WS-NETKEY       PIC  9(04)    COMP-5 VALUE 1.
000500 77  WS-ERROR        PIC  9(01) VALUE 0.
000530 77  WS-FC           PIC  9(01) VALUE 0.
       77  WS-SUB          PIC  9(04)    COMP-5.
000550 77  WS-S            PIC  9(04)    COMP-5.
000560 77  WS-S1           PIC  9(04)    COMP-5.
000570 77  WS-S2           PIC  9(04)    COMP-5.
000580 77  WS-S3           PIC  9(04)    COMP-5.
000590 77  WS-S4           PIC  9(04)    COMP-5.
000600 77  WS-S5           PIC  9(04)    COMP-5.
000610 77  WS-S6           PIC  9(04)    COMP-5.
000620 77  WS-S7           PIC  9(04)    COMP-5.
002850 77  WS-IXS1         PIC  9(04)    COMP-5.
002860 77  WS-IXS2         PIC  9(04)    COMP-5.
002870 77  WS-IXS3         PIC  9(04)    COMP-5.
002880 77  WS-IXS4         PIC  9(04)    COMP-5.
000630 77  WS-ENT          PIC  X(08).
000640 77  WS-ENT1         PIC  X(08).
000650 77  WS-KEYSTR       PIC  9(04)    COMP-5.
000660 77  WS-OPTION       PIC  X(01).
       77  WS-NUM          PIC  Z9.
000690 77  WS-KEY1         PIC  9(04)    COMP-5.
000700 77  WS-CONREC       PIC  9(01) VALUE ZERO.
000710 77  WS-INDS         PIC  9(01) VALUE 0.
000720 77  WS-INDC         PIC  9(01) VALUE 0.
000730 77  WS-INDEX-STOR   PIC  X(08).
000740 77  WS-INDEX-NAME   PIC  X(08).
000750 77  WS-SKIP         PIC  X(01) VALUE " ".
000750 77  WS-CHAR1        PIC  X(01).
000760 77  WS-LIN          PIC  9(02) VALUE ZERO.
000770 77  WS-LINES        PIC  9(04)    COMP-5 VALUE 66.
000780 77  WS-PAGE         PIC  9(04)    COMP-5.
000790 77  WS-SEASON       PIC  X(01).
000930 77  WS-ER8	   PIC	X(09) VALUE "No record".
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
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
002450     03  WS-MID-LNE2.
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
002430     03  WS-TP-LNE.
002440         05  WS-TCR  PIC X(01) OCCURS 80.
	   03  WS-TPCR-LNE REDEFINES WS-TP-LNE.
	       05  WS-TCR1     PIC  X(01).
	       05  WS-TCR78    PIC  X(78).
	       05  WS-TCR80    PIC  X(01).
002450	   03  WS-MD-LNE.
002460         05  WS-MCR  PIC X(01) OCCURS 80.
	   03  WS-MDCR-LNE REDEFINES WS-MD-LNE.
	       05  WS-MCR1     PIC  X(01).
	       05  WS-MCR78    PIC  X(78).
	       05  WS-MCR80    PIC  X(01).
002470	   03  WS-BT-LNE.
002480         05  WS-BCR  PIC X(01) OCCURS 80.
	   03  WS-BTCR-LNE REDEFINES WS-BT-LNE.
	       05  WS-BCR1     PIC  X(01).
	       05  WS-BCR78    PIC  X(78).
	       05  WS-BCR80    PIC  X(01).

000980 COPY WS.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

001000 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY CREDIT.ID.

000180 COPY CRTRAN.ID.

       COPY PARAM.ID.

       COPY W05.WS.

001310 01  W10-MONTHS.
001320     03  W10-MONTH-NAMES
                             PIC  X(36) VALUE
001330                        "JanFebMarAprMayJunJulAugSepOctNovDec".
001340     03  W10-MONTH     REDEFINES W10-MONTH-NAMES
001350                       PIC  X(03) OCCURS 12.

001360 01  W15-INPUT-AREA.
001370     03  W15-DATE      PIC  X(08).
001380     03  W15-DATE-CHAR REDEFINES W15-DATE
001390                       PIC  X(01)     OCCURS 8.
003410	   03  W15-DTE	       PIC 9(06).
003420	   03  W15-RDATE       REDEFINES W15-DTE.
003430         05  W15-DD      PIC 9(02).
003440         05  W15-MM      PIC 9(02).
003450         05  W15-YY      PIC 9(02).
001400	   03  W15-ACNO      PIC  X(05).
001410     03  W15-SNO       REDEFINES W15-ACNO.
001420         05  W15-APREF PIC  X(01).
001430         05  W15-SERIAL
                             PIC  X(04).
001440     03  W15-ITEM      PIC  9(05).
001450     03  W15-I-CODE    REDEFINES W15-ITEM.
001460         05  W15-PREFIX
                             PIC  9(03).
001470         05  W15-TYPE  PIC  9(02).
001480     03  W15-NAME.
001490         05  W15-NCHAR PIC  X(01) OCCURS 40.
001500     03  W15-N         REDEFINES W15-NAME.
001510         05  W15-DESC.
001520             07  W15-ENTRY
                             PIC  X(08).
001530             07  FILLER
                             PIC  X(16).
001540         05  FILLER    PIC  X(16).
001550     03  W15-UN        PIC  9(05).
001560     03  W15-VALUE     PIC  9(07)V99.
001570     03  W15-VAL2      PIC  9(07)V99.
004630     03  W15-ORD         PIC 9(10).
004640     03  W15-ONO         REDEFINES W15-ORD.
004650         05  W15-OPREF   PIC 9(08).
004660         05  W15-OSUB    PIC 9(02).
004820     03  W15-DEL         PIC 9(06).
           03  W15-DEL-DMY REDEFINES W15-DEL.
               05  W15-DEL-DD  PIC 9(02).
               05  W15-DEL-MM  PIC 9(02).
               05  W15-DEL-YY  PIC 9(02).
001620     03  W15-STYLE     PIC  X(08).
001630     03  W15-COST      PIC S9(07)V99 COMP-3.
001640     03  W15-SELL      PIC S9(07)V99 COMP-3.
001650     03  W15-SETT      PIC  9(06).
001660     03  W15-SET       REDEFINES W15-SETT.
001670         05  W15-30    PIC  9(01).
001680         05  W15-60    PIC  9(01).
001690         05  W15-90    PIC  9(01).
001700         05  W15-120   PIC  9(01).
001710         05  W15-150   PIC  9(01).
001720         05  W15-180   PIC  9(01).
001730     03  W15-PAY       REDEFINES W15-SETT.
001740         05  W15-MONTH PIC  9(01)     OCCURS 6.
001760     03  W15-ADDRESS   PIC  X(120).               
001770     03  W15-ADD  REDEFINES W15-ADDRESS.              
001780         05  W15-ADDR1 PIC  X(30).          
001790         05  W15-ADDR2 PIC  X(30).             
001800         05  W15-ADDR3 PIC  X(30).          
001810         05  W15-ADDR4 PIC  X(30).          
001820     03  W15-ADD2 REDEFINES W15-ADDRESS.                     
001830         05  W15-ACHAR PIC  X(01) OCCURS 120.         
001840     03  W15-PCODE     PIC  9(04).
           03  W15-CONTACT   PIC  X(30).
           03  W15-TEL       PIC  X(14).
           03  W15-FAX       PIC  X(14).
           03  W15-TAX       PIC  X(01).
           03  W15-TERMS     PIC  X(01).
           03  W15-PMNT      PIC  X(12).
001850 01  W20-CALCS.
001860     03  W20-RESULT    PIC  9(06)V99.
001870     03  W20-RESULT-1  REDEFINES W20-RESULT.
001880         05  W20-WHOLE PIC  9(06).
001890         05  W20-DECIMAL
                             PIC  9(02).
001900     03  W20-ORD       PIC  9(07)    COMP-3.
	   03  W21-DATE.
	       05  W21-YY      PIC 99.
	       05  W21-MM      PIC 99.
	       05  W21-DD      PIC 99.
	   03  W21-DTE REDEFINES W21-DATE
                               PIC 9(06).
001910 01  W25-VALUES.
	   03  W25-OUT	     PIC S9(09)V99 COMP-3.
	   03  W25-30D	     PIC S9(09)V99 COMP-3.
	   03  W25-60D	     PIC S9(09)V99 COMP-3.
	   03  W25-90D	     PIC S9(09)V99 COMP-3.
	   03  W25-120D      PIC S9(09)V99 COMP-3.
	   03  W25-150D	     PIC S9(09)V99 COMP-3.
	   03  W25-180D	     PIC S9(09)V99 COMP-3.
	   03  W25-BAL	     PIC S9(09)V99 COMP-3.
001930	   03  W25-V1	     PIC S9(09)V99 COMP-3.
001940	   03  W25-V2	     PIC S9(09)V99 COMP-3.
001950 01  W25-EDIT.
001960     03  W25-5.
001970         05  W25-1     PIC  ZZZZ9.
001980     03  W25-3V2.
001990         05  W25-2     PIC  ZZ9.99.
002000     03  W25-7V2.
002010         05  W25-3     PIC  Z(06)9.99.
002020     03  W25-S7V2.
002030         05  W25-VAL   PIC  Z(07)9.99-.
002040     03  W25-3N.
002050         05  W25-4     PIC  ZZ9.
002060     03  W25-S5V2.
002070         05  W25-CVAL  PIC  Z(06)9.99-.
002080     03  W25-DTE.
002090         05  W25-DATE  PIC  Z9/99/99.
002100     03  W25-DEL.
002110         05  W25-2N    PIC  9(02).
002120     03  W25-6.
002130         05  W25-NO    PIC  Z(07)9.
002140 01  W30-DATE.
002150     03  W30-MM        PIC  X(03).
002160     03  FILLER        PIC  X(01) VALUE ",".
002170     03  W30-YY        PIC  9(02).
002180 01  W40-NADD.
002190     03  W40-ADD.
002200         05  W40-ACHAR PIC  X(01) OCCURS 70.
           03  W40-ADDR REDEFINES W40-ADD.
               05  W40-ADDRS PIC  X(60).
               05  FILLER    PIC  X(10).
002210     03  W40-NAME.
002220         05  W40-NCHAR PIC  X(01) OCCURS 40.

       COPY W40.WS.

       COPY FUNCTION.WS.

002240 COPY W50.WS.

005680 COPY W70CRED.WS.

       01  W75-KEYS.
           03  W75-S         PIC  9(02) COMP-5.
           03  W75-S1        PIC  9(02) COMP-5.
           03  W75-CNO       PIC  X(05) OCCURS 18.

006270 01  W85-PASS.
006280     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY       PIC  9(02)    COMP-3.
006300     03  W85-MARG        PIC S9(02)V99.

002440 01  W95-COMPANY.
002450     03  W95-COMP      PIC  X(40).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
002460 SCREEN SECTION.

       COPY BLANK.CRT.

007080 01  S12.
007090	   03  LINE  2 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
007100				 VALUE "CLEARING ACCOUNT BALANCES".

007860 01  S39.
007870	   03  LINE  5 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007880                           VALUE "This routine does not include a
007890-                          "recovery procedure. Security".
007900	   03  LINE  6 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007910                           VALUE "copies  of the data files should
007920-                          " be made as a power  failure".
007930	   03  LINE  7 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007940                           VALUE "or any other problem during this
007950-                          " procedure would require the".
007960	   03  LINE  8 COLUMN 12 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 4
007970                           VALUE "DATA FILES from the previous bac
007980-                          "kup to be restored !!!      ".
007990     03  LINE 10 COLUMN 12 VALUE "Do you wish to continue (Y/N)?".

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
004910 AA000-START-UP  SECTION.
004920 AA000.
003240	   IF LS0-CRLEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
004930	     PERFORM ZA000-INIT.
004940	     PERFORM HA000.
       AA49.
             EXIT PROGRAM.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY LOCKED.REC.

       COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
005630 AB00-READ      SECTION.

       COPY CONTROL.RD.

       COPY CREDIT.RD.

       COPY PARAM.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
007140 AC00-WRITE    SECTION.

       COPY CONTROL.WR.

       COPY CREDIT.WR.

       COPY PARAM.WR.

      /
011010 CA155-GET-CREDITOR      SECTION.
011020 CA155-ACCOUNT.
011060       MOVE W15-ACNO       TO P-NUMBER.
011130	     PERFORM READ-CREDIT-LOCK THRU READ-CREDIT-EXIT.
011140     IF WS-F-ERROR = 3 
011150         MOVE 1 TO WS-ERROR
011160     ELSE
011170     IF P-NUMBER NOT = W15-ACNO
011180         MOVE 2 TO WS-ERROR
011190     ELSE
011200         MOVE 0 TO WS-ERROR.
011210 CA160-EXIT.
011220       EXIT.
      /
036530 HA000	     SECTION 5.
036540 HA00.
036550       DISPLAY CLR-SCREEN.
	   IF LS-DEM = 6
	       MOVE 9		 TO LS-DEM
	       GO TO HA03.
032000       MOVE 5              TO WS-PARKEY.
032010       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
032020       MOVE PAR-PW         TO W85-PASS. 
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 29		 TO SHADE-COL.
	     MOVE 23		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1127
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
042540	     DISPLAY "³ " AT 1227 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				       BACKGROUND-COLOR 7
		     "Enter Password        "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "À" AT 1327 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.
042550	     ACCEPT W40-CSHPASS AT 1244
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 3 HIGHLIGHT AUTO NO-ECHO.
018300	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
018360	     MOVE 1		 TO WS-LENGTH.
018370     IF NOT (W40-CSHPASS = W85-SUPER (1) OR 
				 W85-SUPER (3) OR
                                 "ZEROCR") 
	       GO TO HA999.
018260       DISPLAY "                       " AT 2551
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
036560	     DISPLAY S12.
036570       DISPLAY S39.
036580 HA02.
036590	     ACCEPT WS-OPTION AT 1043
		    WITH FOREGROUND-COLOR 15 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
036650	   IF NOT (WS-OPTION = "N" OR "Y")
036660         GO TO HA02.
036670     IF WS-OPTION = "N"
036680         GO TO HA999.

       HA03.
012010       MOVE 2              TO WS-NETKEY.
012020       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
	     MOVE ZERO		 TO P-DUE-NOW P-DUE-30
				    P-DUE-150 P-DUE-60
				    P-DUE-180 P-DUE-90
				    P-DUE-120 P-OPEN
				    P-CRED    P-CLOSE
				    P-DEBT.
036700       MOVE "  !"          TO P-NUMBER.
036710       DISPLAY "Account :" AT 1620.
             PERFORM START-AT-CRED-AC THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               GO TO HA10.

036720 HA05.
036730       PERFORM READ-CREDIT-NEXT-LOCK THRU READ-CREDIT-EXIT.
           IF WS-F-ERROR = 3
               GO TO HA10.
036770	     DISPLAY P-NUMBER AT 1630
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE ZERO		 TO P-BAL P-OUT
036800				    P-30D P-120D
036810                              P-60D P-150D 
				    P-90D P-180D
036880				    P-DT  P-CR
036890				    P-VAT P-PBAL.
036930       PERFORM REWRITE-CREDIT-UNLOCK THRU WRITE-CREDIT-EXIT.
             GO TO HA05.

036940 HA10.
             MOVE 1201           TO CPOS.
036990       PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
037000       DISPLAY "Busy clearing transactions" AT 1220.
037040	     MOVE 1		 TO P-CRTRAN.
	     CLOSE CTRANS.
037060       OPEN OUTPUT CTRANS.
037070       CLOSE CTRANS.
037080       OPEN I-O CTRANS.
012990	     PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.

037170 HA999.
037180       EXIT.
      /
019640 ZA000-INIT    SECTION 8.
019650 ZA000-START.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
019660	     DISPLAY CLR-SCREEN.
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
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
	     MOVE "CRTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CRTRAN.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
019960	     MOVE 1		 TO WS-PARKEY.
019970       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
019980       MOVE PAR-COMPANY    TO W95-COMP.
019990       MOVE PAR-DMY        TO W05-DATE.
020000       MOVE PAR-YMD        TO W05-DATE-YMD.
	     MOVE 2		 TO WS-NETKEY.
020030       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
020100       GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MD-LNE WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1          TO WS-TCR  (WS-S1) WS-BCR  (WS-S1).
             MOVE WS-G8          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1)
                                    WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
021930       MOVE WS-G2          TO WS-TCR  (8)  WS-TCR (50)
021940                              WS-TCR  (66).
021950       MOVE WS-G3          TO WS-MCR  (8)  WS-MCR (50)
021960                              WS-MCR  (66) WS-MCR (1)
				    WS-MCR  (80).
021970       MOVE WS-G4          TO WS-BCR  (8)  WS-BCR (50)
021980                              WS-BCR  (66).
051190       MOVE WS-G9          TO WS-TCH  (1).
051200       MOVE WS-G10         TO WS-TCH  (80).
051210       MOVE WS-G11         TO WS-BCH  (1).
051220       MOVE WS-G12         TO WS-BCH  (80).
051230       MOVE WS-G14         TO WS-TCR  (1)  WS-BCR (1).
051260       MOVE WS-G13         TO WS-TCR (80)  WS-BCR (80).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
020110 ZA200.
020120       DISPLAY "Files locked - Try later" AT 2312
020130                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
020140       DISPLAY "Press any key" AT 2512 
020150		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
020160	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
020190 ZA999-EXIT.
020200       EXIT.
      /
020210 ZB000-I-O-ERROR SECTION 9.
      
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
041630         MOVE WS-NETKEY TO WS-KEY
041640     ELSE
041650     IF WS-F-ERROR = 3
041660         MOVE W02-CREDIT TO WS-FILE
041670	       MOVE ZERO	 TO WS-KEY
	       MOVE P-NUMBER	 TO WS-KEYX
           ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870	       MOVE WS-PARKEY TO WS-KEY.
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
