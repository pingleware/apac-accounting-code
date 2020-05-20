      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   *******   *******	*******   *******   **	   **  *
      *   **	**  **	  **  **    **	**    **  **	**  ***    **  *
      *   **	**  **	  **  **    **	**    **  **	**  ** *   **  *
      *   **	**  *******   **    **	*******   *******   **	*  **  *
      *   **	**  **	 **   **    **	**	  **   **   **	 * **  *
      *   **	**  **	  **  **    **	**	  **	**  **	  ***  *
      *    ******   **	  **  *******	**	  **	**  **	   **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     O R D E R ' S   P R I N T   P R O G R A M                  *
      *                                                                *
      *     VERSION 8.15.00 - April 2011			       *
      *								       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     ORDPRN.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   MARCH 1995.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1995 - 2011
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

000200 COPY CORDER.SL.

000160 COPY CREDIT.SL.

000120 COPY PARAM.SL.

000260 COPY PORDER.SL.

000240 COPY RECOVER.SL.

       COPY SHARED.SL.

000140 COPY STOCK.SL.

000250 DATA DIVISION.
000260 FILE SECTION.

       COPY APACFIDS.FDE.

000360 COPY CONTROL.FDE.

000520 COPY CORDER.FDE.

000160 COPY CREDIT.FDE.

       COPY PARAM.FDE.

000260 COPY PORDER.FDE.

000390 COPY RECOVER.GFD.

       COPY SHARED.FDE.

000540 COPY STOCK.FDE.

      /
001690 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001700 77  WS-DISC	   PIC	999V99	 COMP-3.
001710 77  WS-SUB	   PIC	9(04)	 COMP-5.
001710 77  WS-S1	   PIC	9(04)	 COMP-5.
001720 77  WS-S2	   PIC	9(04)	 COMP-5.
001730 77  WS-S3	   PIC	9(04)	 COMP-5.
001740 77  WS-S4	   PIC	9(04)	 COMP-5.
001750 77  WS-S5	   PIC	9(04)	 COMP-5.
001760 77  WS-S6	   PIC	9(04)	 COMP-5.
001770 77  WS-S7	   PIC	9(04)	 COMP-5.
001780 77  WS-S8	   PIC	9(04)	 COMP-5.
001780 77  WS-S9	   PIC	9(04)	 COMP-5.
001790 77  WS-IXS	   PIC	9(04)	 COMP-5.
001800 77  WS-IXS1	   PIC	9(04)	 COMP-5.
001810 77  WS-IXS2	   PIC	9(04)	 COMP-5.
001820 77  WS-IXS3	   PIC	9(04)	 COMP-5.
001830 77  WS-IXS4	   PIC	9(04)	 COMP-5.
001860 77  WS-PARKEY	   PIC	9(06)	 COMP-5.
001870 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001890 77  WS-RECKEY	   PIC	9(06)	 COMP-5.
001900 77  WS-RECOVER	   PIC	9(06)	 COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
001920 77  WS-KEYSTR	   PIC	9(04)	 COMP-5.
001940 77  WS-MONTHS	   PIC S9(01)V99 COMP-3.
001960 77  WS-KEY1	   PIC	9(04)	 COMP-5.
001970 77  WS-KSTORE	   PIC	9(04)	 COMP-5.
001980 77  WS-ITM	   PIC	X(14).
       77  WS-ETYPE	   PIC	X(01).
002010 77  WS-PAGE	   PIC	9(04)	 COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
002020 77  WS-OPTION	   PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
002020 77  WS-SKIP	   PIC	X(01).
002040 77  WS-ADJ	   PIC	X(01).
002070 77  WS-ERROR	   PIC	9(01).
002120 77  WS-AMEND	   PIC	9(01) VALUE 0.
002140 77  WS-CONREC	   PIC	9(01) VALUE ZERO.
002150 77  WS-LEDG	   PIC	X(04).
002160 77  WS-LEDG1	   PIC	X(04).
002200 77  WS-COMP	   PIC	9(01).
002210 77  WS-WEEK	   PIC	9(01).
002220 77  WS-TYPE	   PIC	X(01).
002270 77  WS-PASSWORD	   PIC	X(08).
       77  WS-PRINT	   PIC	9(01).
002280 77  WS-PASS	   PIC	X(08).
002300 77  WS-HDNG	   PIC	X(118) VALUE
002310	   "ORD ITEM CODE           QUANTITY  X-REF/BAR CODE     DESCRIP
      -    "TION                     UNIT COST     TOTAL COST CONTACT".
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
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
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

002330 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "ORD".
	   03  WS-PROG	       PIC  X(03) VALUE "PRN".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002350 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

000360 COPY CORDER.ID.

000160 COPY CREDIT.ID.

       COPY PARAM.ID.

000260 COPY PORDER.ID.

000280 COPY RECOVER.ID.

       COPY SHARED.ID.

000300 COPY STOCK.ID.

       01  W02-PRINTER-DETAILS.
	   03  W02-PRINTER     PIC  X(12).
002570	   03  W02-PGE-LENGTH  PIC  9(02).
001870	   03  W02-PRN-LENGTH  PIC  9(02).
	   03  W02-LINAGE      PIC  9(02).
	   03  W02-PRN-STATUS  PIC  X(01) VALUE "C".
      *
      *    ****    D  =  Detail line
      * 	   C  =  Condensed print
      * 	   E  =  Expanded print
      * 	   H  =  Header line
      * 	   X  =  Cancel expanded print
      * 	   1  =  10 Characters per inch
      * 	   2  =  12 Characters per inch
      * 	   3  =  17 Characters per inch
      * 	   6  =  6 Lines per inch
      * 	   8  =  8 Lines per inch
      *
	   03  W02-PRN-TYPE    PIC  X(01).
	   03  W02-PRN-LINE    PIC  X(136).

	   03  R-SL1 REDEFINES W02-PRN-LINE.
	       05  R-SLA       PIC  X(78).
	       05  FILLER      PIC  X(58).

	   03  R-SL2 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(58).
	       05  R-SLB       PIC  X(78).

000570	   03  R-L1 REDEFINES W02-PRN-LINE.
	       05  R-DET	   PIC X(132).

000590	   03  R-L2 REDEFINES W02-PRN-LINE.
	       05  REP-EXPNAME	   PIC	X(40).
	       05  FILLER	   PIC	X(92).

	   03  R-L3 REDEFINES W02-PRN-LINE.
000930	       05  REP-NADD	   PIC	X(44).
000940	       05  REP-RADD	 REDEFINES REP-NADD.
000950		   07  REP-PC	   PIC	X(04).
000960		   07  FILLER	   PIC	X(40).
	       05  FILLER	   PIC	X(40).
001120	       05  REP-H2	   PIC	X(16).
	       05  REP-SMAN.
001130		   07  REP-TEL	   PIC	X(14).
		   07  REP-GNO REDEFINES REP-TEL
				   PIC	X(14).
		   07  FILLER	   PIC	X(18).

000980	   03  R-L4 REDEFINES W02-PRN-LINE.
001000	       05  REP-NAME	   PIC	X(40).
001010	       05  REP-DADD	 REDEFINES REP-NAME.
001020		   07  REP-PC1	   PIC	X(04).
001030		   07  FILLER	   PIC	X(36).
001040	       05  FILLER	   PIC	X(12).
	       05  REP-H3	   PIC	X(08).
001060	       05  REP-DADD2.
001070		   07  REP-PC2	   PIC	X(30).
001090	       05  FILLER	   PIC	X(02).
	       05  REP-H4	   PIC	X(17).
	       05  REP-STEL.
001550		   07  REP-ORD.
		     09  REP-TERMS.
		       11  FILLER  PIC	X(05).
		       11  REP-AC.
			 13  FILLER  PIC  X(03).
			 13  REP-PGE PIC  ZZ9.
		     09  FILLER    PIC	X(01).
		   07  FILLER	   PIC	X(02).
	       05  FILLER	   PIC	X(01).

000710	   03  R-L5 REDEFINES W02-PRN-LINE.
	       05  OR-HDNG.
		   07  FILLER	   PIC	X(01).
000720		   07  OR-LINE	   PIC	99.
		   07  FILLER	   PIC	X(01).
		   07  OR-CODE.
000740		     09  OR-ITEM   PIC	X(14).
000750		     09  OR-SL	   PIC	X(01).
		     09  OR-ITM	   PIC	X(03).
		   07  FILLER	   PIC	X(03).
000780		   07  OR-QNT	   PIC	Z(05)9.
		   07  FILLER	   PIC	X(03).
		   07  OR-XREF	   PIC	X(18).
		   07  FILLER	   PIC	X(01).
000760		   07  OR-DESC	   PIC	X(30).
		   07  FILLER	   PIC	X(01).
000800		   07  OR-COST	   PIC	Z(07)9.999.
		   07  FILLER	   PIC	X(01).
000820		   07  OR-EXT	   PIC	Z(08)9.99.
		   07  FILLER	   PIC	X(01).
000840		   07  OR-CONTACT  PIC	X(22).

	   03  R-L6 REDEFINES W02-PRN-LINE.
000800	       05  REP-H1	   PIC X(23).
000810	       05  REP-REF	   PIC 9(08).
000860	       05  FILLER	   PIC X(49).

	   03  R-L8 REDEFINES W02-PRN-LINE.
001540	       05  FILLER	   PIC	X(17).
	       05  REP-INVOICE.
		   07  FILLER	   PIC	X(03).
		   07  REP-CRNOTE.
		       09  FILLER  PIC	X(06).
		       09  REP-COPY
				   PIC	X(12).
		       09  FILLER  PIC	X(06).
		   07  FILLER	   PIC	X(03).
	       05  FILLER	   PIC	X(01).
	       05  REP-H14	   PIC	X(08).
	       05  REP-DTE	   PIC	99/99/9999.

       COPY W05.VAT.

       01  W05-EXTRA.
	   03  W05-ROUND       PIC  X(01).
           03  W05-ROUND-VAL   PIC  9V9(02).
           03  W05-RVAL REDEFINES W05-ROUND-VAL.
               05  W05-RAND    PIC  9(01).
               05  W05-CENTS   PIC  9(02).
           03  W05-RVAL2 REDEFINES W05-ROUND-VAL.
               05  FILLER      PIC  9(01).
               05  W05-RVC1    PIC  9(01).
               05  W05-RVC2    PIC  9(01).
           03  W05-HLF-VAL     PIC  9V99.
           03  W05-TQTR-VAL    PIC  9V99.
           03  W05-CALC-VAL    PIC  9(02)V99.
           03  W05-CALC-RRCC REDEFINES W05-CALC-VAL.
               05  W05-RVR1    PIC  9(01).
               05  W05-RVRCC   PIC  9(01)V99.

       01  W09-STOCK.
002780	   03  W09-MARKUP     PIC S9(03)V9999  COMP-3.
002790	   03  W09-CMARKUP    PIC S9(03)V9999  COMP-3.
002790	   03  W09-WMARKUP    PIC S9(03)V9999  COMP-3.

       COPY W10.STK.

	   03  W10-CONTACT     PIC  X(30).
	   03  W10-ODATE       PIC  9(08).
	   03  W10-SUPP        PIC  X(06).
	   03  W10-SNAME       PIC  X(40).
	   03  W10-STERMS.
	       05  W10-30      PIC  9(01).
	       05  W10-60      PIC  9(01).
	       05  W10-90      PIC  9(01).
	       05  W10-120     PIC  9(01).
	       05  W10-150     PIC  9(01).
	       05  W10-180     PIC  9(01).
	   03  W10-TERMS REDEFINES W10-STERMS
			       PIC  9(06).
	   03  W10-NAME.
	       05  W10-N1-2    PIC  X(02).
	       05  W10-N3-4    PIC  X(02).
	       05  W10-N5-6    PIC  X(02).
	       05  W10-N7-8    PIC  X(02).
	       05  W10-N9-10   PIC  X(02).
	       05  W10-N11-12  PIC  X(02).
	       05  W10-N13-14  PIC  X(02).
	       05  W10-N15-16  PIC  X(02).
	       05  W10-N17-18  PIC  X(02).
	       05  W10-N19-20  PIC  X(02).
	       05  W10-N21-22  PIC  X(02).
	       05  W10-N23-24  PIC  X(02).
	       05  W10-N25-26  PIC  X(02).
	       05  W10-N27-28  PIC  X(02).
	       05  W10-N29-30  PIC  X(02).

       COPY W12.WS.

001360 01  W15-INPUT-AREA.
001510	   03  W15-DESC.
001520	       05  W15-ENTRY
                             PIC  X(08).
001530	       05  FILLER    PIC  X(16).
001540         05  FILLER    PIC  X(16).
004630	   03  W15-ORD	       PIC  9(10).
004640     03  W15-ONO         REDEFINES W15-ORD.
004650	       05  W15-OPREF   PIC  9(08).
004660	       05  W15-OSUB    PIC  9(02).
	   03  W15-NAME        PIC  X(30) VALUE SPACES.
	   03  W15-ENCRYPT REDEFINES W15-NAME.
	       05  W15-N1-2    PIC  X(02).
	       05  W15-N3-4    PIC  X(02).
	       05  W15-N5-6    PIC  X(02).
	       05  W15-N7-8    PIC  X(02).
	       05  W15-N9-10   PIC  X(02).
	       05  W15-N11-12  PIC  X(02).
	       05  W15-N13-14  PIC  X(02).
	       05  W15-N15-16  PIC  X(02).
	       05  W15-N17-18  PIC  X(02).
	       05  W15-N19-20  PIC  X(02).
	       05  W15-N21-22  PIC  X(02).
	       05  W15-N23-24  PIC  X(02).
	       05  W15-N25-26  PIC  X(02).
	       05  W15-N27-28  PIC  X(02).
	       05  W15-N29-30  PIC  X(02).

       COPY W20.WS.

004310 01  W25-CALCS.
005040     03  W25-RESULT      PIC 9(05)V99.
004330     03  W25-RESULT1 REDEFINES W25-RESULT.
004340         05  W25-WHOLE   PIC 9(05).
004350         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
003750	   03  W25-SCOST       PIC S9(09)V99 COMP-3.
003760	   03  W25-PCOST       PIC S9(09)V99 COMP-3.
003760	   03  W25-PCST        PIC S9(09)V99 COMP-3.
003760	   03  W25-PTOT        PIC S9(09)V99 COMP-3.
003770	   03  W25-TSELL       PIC S9(09)V99 COMP-3.
003770	   03  W25-TOTAL       PIC S9(09)V99 COMP-3.
003780	   03  W25-VALUE       PIC S9(09)V99 COMP-3.
003780	   03  W25-ADJUST      PIC S9(09)V99 COMP-3.

004930 01  W40-COMPANY.
005090	     05  W40-ADDRESS1.
005100	       07  W40-ADD1    PIC  X(30).
005110	       07  W40-ADD2    PIC  X(30).
005120	       07  W40-ADD3    PIC  X(30).
005130	       07  W40-ADD4    PIC  X(30).
005140       05  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
005150         07  W40-ACHAR OCCURS 120 PIC X(01).
005160	     05  W40-PC1       PIC  X(04).
	     05  W40-TEL       PIC  X(14).
	     05  W40-FAX       PIC  X(14).

       COPY W40.WS.

       01  W42-ATTRIB5.
           03  FILLER          PIC X(2000).

       01  W42-ATTRIB6.
           03  FILLER          PIC X(2000).

       01  W43-SCREEN5.
           03  FILLER          PIC X(2000).

       01  W43-SCREEN6.
           03  FILLER          PIC X(2000).

       COPY FUNCTION.WS.

003800 COPY W50.WS.

005680 COPY W70CRED.WS.

006860 01  W95-STM.
006870     03  W95-COMP        PIC X(40).
006880     03  W95-ADD1        PIC X(30).
006890     03  W95-ADD2        PIC X(30).
006900     03  W95-ADD3        PIC X(30).
006910     03  W95-ADD4        PIC X(30).
006920     03  W95-PC.
006930         05  W95-POST    PIC 9(04).
006940     03  W95-TEL         PIC X(14).

005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V5 REDEFINES W100-V2.
	       05  W100-QNT    PIC  Z(03)9-.
	       05  W100-NO     PIC  Z(03)9.
	       05  FILLER      PIC  X(01).

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004130 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY S99.CRT.

       01  ERROR-LINE.
           03  LINE 25 COLUMN  2 BACKGROUND-COLOR WS-TEMPBG
                                 FOREGROUND-COLOR WS-FGRND-5
                                 HIGHLIGHT
                                 PIC  X(48) FROM WS-ERR-MES.
      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
007740 AA000-MAIN	       SECTION.
007750 AA00.
007760       PERFORM ZA000-INIT.
	     PERFORM BA000.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
005260	     CLOSE CORDER
		   RECOVER.

       AA49.
	     EXIT PROGRAM.

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

       COPY PRINTUTL.AS3.

       COPY LOCKED.RC2.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	      ERASE SCREEN FROM SPECIFIED LOCATION	       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ Uses CPOS (CLIN,CCOL) as starting position and increases  ³
      *    ³ CLIN by 1 until CLIN reaches the line allocated to exit.  ³
      *    ³ The screen is cleared with Column 1 and 80 containing "³" ³
      *    ³ and columns 2 to 79 containing spaces.		       ³
      *    ³  eg.						       ³
      *    ³	  MOVE 0801	 TO CPOS.			       ³
      *    ³	  PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 19.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY CLEAR.CRT.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       R E A D	 F I L E S   R O U T I N E S	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
008180 AC000-READ	       SECTION.

       COPY CONTROL.RD.

       COPY CORDER.RD.

       COPY CREDIT.RD.

       COPY PARAM.RD.

       COPY PORDER.RD.

       COPY SHARED.RD.

       COPY STOCK.RD.

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   &	W R I T E   F I L E S	R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
010170 AD000		 SECTION.

       COPY CONTROL.WR.

       COPY CORDER.WR.

       COPY CREDIT.WR.

       COPY PARAM.WR.

       COPY PORDER.WR.

       COPY SHARED.WR.

       COPY STOCK.WR.

       COPY APAC.HLP.

      /
      *       ****   *****   ***    ***   *   *  *****	****   *   *
      *       *   *  *	    *	*  *   *  *   *  *	*   *  *   *
      *       ****   ***    *	   *   *  *   *  ***	****	* *
      *       *   *  *	    *	*  *   *   * *	 *	*   *	 *
      *       *   *  *****   ***    ***     *	 *****	*   *	 *
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      * 	 THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *        ³ ORIGINAL ACTION (REC-TYPE)  ÄÄ  RECOVERY PROCESS ³
      *        ³ 0 = RECORD CHANGED	     ÄÄ     (REWRITE)	   ³
      *        ³ 1 = RECORD WAS ADDED	     ÄÄ     (DELETE)	   ³
      *        ³ 2 = RECORD WAS DELETED      ÄÄ     (WRITE)	   ³
      *        ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
011730 AY000	       SECTION.
011740 AY01.
011690       MOVE 01             TO REC-FILE.
011700       MOVE WS-PARKEY      TO REC-KEY.
011710	     PERFORM READ-PARAM THRU READ-PARAM-EXIT.
011720       MOVE PAR-RECORD1    TO REC-PARAM.
011780       GO TO AY50.

011790 AY05.
011860       MOVE 05             TO REC-FILE.
011870       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
011890         PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
011900       MOVE STK-RECORD1    TO REC-STOCKF.
011830       GO TO AY50.

011940 AY10.
008160       MOVE 10 TO REC-FILE.
008170       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
           IF WS-ACTION = 0 OR 2
               PERFORM READ-PORDER-LOCK THRU READ-PORDER-EXIT.
008180       MOVE ORD-RECORD1 TO REC-PORDER.
008190       GO TO AY50.

012150 AY37.
012160	     MOVE 37		 TO REC-FILE.
012170	     MOVE WS-SHARED	 TO REC-KEY.
012180	     MOVE SHR-RECORD	 TO REC-NETWORK.
012190       GO TO AY50.

012150 AY39.
012160       MOVE 39             TO REC-FILE.
012170       MOVE WS-NETKEY      TO REC-KEY.
012180       MOVE NET-RECORD     TO REC-NETWORK.
012190       GO TO AY50.

012040 AY49.
012050       MOVE 99             TO REC-FILE.
012060       MOVE ZERO           TO REC-KEY.
012070       MOVE SPACES         TO REC-DETAIL.
012080       PERFORM AY50.
012090       ADD 1               TO WS-TRANS.
012100     IF WS-RECOVER > 95
012110         CLOSE RECOVER
012120         OPEN OUTPUT RECOVER
012130         CLOSE RECOVER
012140         OPEN I-O RECOVER
012150         MOVE ZERO         TO WS-RECOVER.
012160       GO TO AY59.

012170 AY50.
012180       ADD 1               TO WS-RECOVER.
012190       MOVE WS-RECOVER     TO WS-RECKEY.
012200       MOVE WS-TRANS       TO REC-TRAN.
012210       WRITE REC-RECORD.
012220     IF WS-STATUS NOT = "00"
012230         DISPLAY "Write error Recovery file - Status " AT 2312
                        WITH FOREGROUND-COLOR 14 
                        WS-STATUS WITH FOREGROUND-COLOR 15
012250         STOP RUN.
012280 AY59.
012290       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
	     MOVE SHR-STOCK	 TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
012500	     MOVE 4		 TO WS-PARKEY.
012510       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
012520     IF PAR-USERS < 24
012530         MOVE 1            TO WS-SUB
	       MOVE ZERO	 TO WS-WAIT
012540         GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4	S E C O N D S
      *
012550	     DISPLAY "WAITING" AT 2551
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
012560	     COMMIT.
	     ACCEPT WS-STIME FROM TIME.
	     MOVE 400		 TO WS-WAIT.
012580	     PERFORM LOCK-REC-LOOP.
012590	     DISPLAY SPACE AT 2551
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
012600       GO TO AY60.

012610 AY61.
012620	     MOVE "CS"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the CREDITOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012010       MOVE 2              TO WS-NETKEY.
012680	     PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
	     MOVE NET-CREDITOR	 TO W70-CREDCON.
           IF WS-SKIP = "N"
	       IF WS-STOCK = "Y"
012710		   MOVE 3	 TO WS-SHARED
012720		   PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
		   PERFORM AY37 THRU AY59
		   MOVE SHR-RECORD
				 TO NET-RECORD
	       ELSE
012710		   MOVE 3	 TO WS-NETKEY
012720		   PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
012730		   PERFORM AY39 THRU AY59.
012740	     GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N	P R O G R E S S
      *
012750 AY62.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES OR
		   PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R	O R   S T O C K   F I L E S
      * 	  B E I N G   U P D A T E D
      *
	       IF NOT (PAR-PROG(WS-SUB) = SPACES)
012760		   IF PAR-PROG(WS-SUB) = "CR" OR "CS" OR "CG"
					  OR "ST" OR "DS"
      *
      *    ****   Y E S   -   S E T   W A I T	P E R I O D
      *
		       GO TO AY62-WAIT
		   ELSE
		       ADD 1	 TO WS-SUB
		       GO TO AY62
		   END-IF
	       ELSE
      *
      *    ****   I S	T H I S   P R O G R A M   I N	T H E  Q
      *
	       IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S	I T   N E X T	I N   L I N E	T O   P R O C E S S
      *
		   IF WS-WAIT = ZERO
		       GO TO AY63
		   ELSE
		       GO TO AY62-WAIT
		   END-IF
	       ELSE
		   GO TO AY62-WAIT
	       END-IF
	   END-IF.
	     MOVE LS-USER	 TO PAR-USR(WS-SUB).
	     MOVE WS-SUB	 TO PAR-USERS.
	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
	     GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
	     MOVE 300		 TO WS-WAIT.
	   IF NOT (PAR-USR(WS-SUB) = LS-USER)
	       IF WS-SUB < 24
		   ADD 1	 TO WS-SUB
		   GO TO AY62.

       AY62-CHECK.
	   IF WS-WAIT > ZERO
	       COMMIT
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       ACCEPT WS-STIME FROM TIME
	       PERFORM LOCK-REC-LOOP
	       DISPLAY "Waiting" AT 2572
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 14 BLINK
	       GO TO AY60.
012880	     DISPLAY SPACE AT 2572
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

012900 AY63.
012920	     MOVE WS-SUB	 TO WS-USUB.
012940	     GO TO AY61.

012970 AY70.
013030	     MOVE 4		 TO WS-PARKEY.
013040       PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      * 	   NETWORK record 2.
      *
	   IF WS-STOCK = "Y"
	       MOVE NET-RECORD	 TO SHR-RECORD
012980	       PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
	   ELSE
012980	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
012970	     MOVE 2		 TO WS-NETKEY.
012980       MOVE W70-CREDCON    TO NET-CREDITOR.
012990       PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
013020	     PERFORM AY49 THRU AY59.
	     MOVE 1		 TO WS-USUB.

       AY72.
	   IF NOT (PAR-USR(WS-USUB) = LS-USER)
	       ADD 1		 TO WS-USUB
	       GO TO AY72.

       AY75.
013050	     MOVE SPACES	 TO PAR-PROG(WS-USUB)
013060				    PAR-USR(WS-USUB).
	   IF WS-USUB < 24
	       ADD 1 WS-USUB	 GIVING WS-SUB
	   ELSE
	       GO TO AY80.
	   IF NOT (PAR-PROG(WS-SUB) = SPACES)
	       MOVE PAR-PROG(WS-SUB)
				 TO PAR-PROG(WS-USUB)
	       MOVE PAR-USR(WS-SUB)
				 TO PAR-USR(WS-USUB)
	       ADD 1		 TO WS-USUB
	       GO TO AY75.

       AY80.
013070	     SUBTRACT 1 FROM WS-USUB
				 GIVING PAR-USERS.
013080	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
013090	     COMMIT.

011860 AY999.
011870       EXIT.

       COPY CA000X.STK.

       COPY CA500.STK.

032190 BA000	     SECTION 5.
032200 BA00.
	     MOVE SPACES	 TO COR-KEY.
	     MOVE 00000001	 TO COR-DATE.
	     PERFORM START-AT-COR-KEY THRU READ-CORDER-EXIT.
	   IF WS-F-ERROR = 48
	       GO TO BA999.
032240	     DISPLAY CLR-SCREEN.
004860	     DISPLAY "STOCK ORDER PRINT" AT 0232
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "If orders are not printed they will remain in the
      - 	     "work file and may be" AT 0606
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4.
	     DISPLAY "printed later. Orders are only deleted from the wo
      - 	     "rk file, and added  " AT 0706
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4.
	     DISPLAY "to the PURCHASE ORDER file, after they have been p
      - 	     "rinted. The STOCK is" AT 0806
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4.
	     DISPLAY "also updated at this stage. Unless other items are
      - 	     " to be added, it is " AT 0906
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4.
	     DISPLAY "advisable to print orders before exiting from this
      -		     " program.           " AT 1006
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 4.
	     DISPLAY "P" AT 1206
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "rint orders or "
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     "ape to exit".
	     MOVE "P"		 TO WS-OPTION.

       BA05.
	     ACCEPT WS-OPTION AT 1237
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 7 HIGHLIGHT UPDATE AUTO.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     GO TO BA999
                 WHEN OTHER
                     CALL X"E5"
		     GO TO BA05
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
020250	   IF NOT (WS-OPTION = "P")
	       GO TO BA05.
032240	     DISPLAY CLR-SCREEN.
004860	     DISPLAY "STOCK ORDER PRINT" AT 0232
	     DISPLAY "Processing :" AT 0612.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 3		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.

       BA08.
	     MOVE SPACES	TO W10-SUPP.
	     MOVE ZERO		TO W10-ODATE.

       BA10.
	     PERFORM READ-CORDER-NEXT THRU READ-CORDER-EXIT.
	   IF WS-F-ERROR = 48
	       CLOSE CORDER
	       OPEN OUTPUT CORDER
	       CLOSE CORDER
	       OPEN I-O CORDER
	       GO TO BA20.
	   IF (COR-CRED = W10-SUPP) AND
	      (COR-DATE = W10-ODATE)
	       ADD 1		 TO W15-OSUB
	       GO TO BA15.
	   IF NOT (W10-SUPP = SPACES)
	       PERFORM BA20.
	     MOVE COR-CRED	 TO W10-SUPP P-NUMBER.
	     MOVE COR-DATE	 TO W10-ODATE.
	     MOVE ZERO		 TO W25-TOTAL.
      *
      *    ****    Start Recovery Logging.
      *
             MOVE "N"            TO WS-SKIP.
033610       PERFORM AY60 THRU AY999.
	     MOVE 0		 TO WS-ACTION.
033630	     PERFORM READ-CREDIT THRU READ-CREDIT-EXIT.
	     DISPLAY P-NUMBER AT 0624
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
	     DISPLAY P-NAME AT 0630
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
014060	     UNSTRING P-ADDR DELIMITED BY "*"
014070				 INTO W40-ADD1
014080				      W40-ADD2
014090				      W40-ADD3
014100				      W40-ADD4
014110       OVERFLOW
014120           MOVE 1          TO WS-ERROR.
	     MOVE P-TEL 	 TO W40-TEL.
	     MOVE P-FAX 	 TO W40-FAX.

       BA12.
	   IF W70-PORDER = 99999999
               MOVE 1            TO W70-PORDER
           ELSE
	       ADD 1		 TO W70-PORDER.
	     MOVE W70-PORDER	 TO W15-OPREF.
	     MOVE ZERO		 TO W15-OSUB.
      *
      *   ****	 C H E C K   F O R   D U P L I C A T E
      * 	 O R D E R - N U M B E R
      *
	     MOVE W15-OPREF	 TO ORD-ORD.
	     MOVE W15-OSUB	 TO ORD-LINE.
	     PERFORM START-AT-ORDER-NO THRU READ-PORDER-EXIT.
	   IF WS-F-ERROR = 16
	       GO TO BA13.
	     PERFORM READ-PORDER-NEXT THRU READ-PORDER-EXIT.
	   IF NOT (WS-F-ERROR = 0)
	       GO TO BA13.
	   IF ORD-ORD = W15-OPREF
	       GO TO BA12.

       BA13.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
017710	     MOVE SPACES	 TO R-DET.
017750	     MOVE "PURCHASE ORDER NUMBER:"
                                 TO REP-H1.
	     MOVE W15-OPREF	 TO REP-REF.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017750	     MOVE W95-COMP	 TO REP-EXPNAME.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE SPACES	 TO R-DET.
017770	     MOVE W95-ADD1	 TO REP-NADD.
026690	   IF W05-VATNO NOT = SPACES
026700	       MOVE "VAT No/BTW Nr :"
				 TO REP-H2
026710	       MOVE W05-VATNO	 TO REP-GNO.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017790	     MOVE SPACES	 TO R-DET.
017800	     MOVE W95-ADD2	 TO REP-NADD.
017910	   IF W95-TEL NOT = SPACES
017920	       MOVE "TELEPHONE/FOON:"
				 TO REP-H2
017930	       MOVE W95-TEL	 TO REP-TEL.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
017820	     MOVE W95-ADD3	 TO REP-NADD.
	     MOVE "ORD.BY/BEST.DR:"
				 TO REP-H2.
	     MOVE LS0-NAME	 TO W15-NAME.
	     PERFORM BB010.
	     MOVE W10-NAME	 TO REP-SMAN.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
017870	     MOVE SPACES	 TO R-DET.
017840	     MOVE W95-ADD4	 TO REP-NADD.
	     PERFORM CALL-PRINTUTL.
017870	     MOVE SPACES	 TO R-DET.
017880	     MOVE W95-PC	 TO REP-PC.
	     PERFORM CALL-PRINTUTL.
017900	     MOVE SPACES	 TO R-DET.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "E"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE "DATE  :"	 TO REP-H14.
026670	     MOVE COR-DATE	 TO W22-DTE2.
	     MOVE W22-CC2	 TO W22-CC3.
	     MOVE W22-YY2	 TO W22-YY3.
	     MOVE W22-MM2	 TO W22-MM3.
	     MOVE W22-DD2	 TO W22-DD3.
	     MOVE W22-DTE3	 TO REP-DTE.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE "X"		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
026320	     MOVE SPACES	 TO R-DET.
026330	     MOVE P-NAME	 TO REP-NAME.
	     MOVE "SUPPLIER NUMBER : "
				 TO REP-H4.
026550	     MOVE COR-CRED	 TO REP-AC.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
026320	     MOVE SPACES	 TO R-DET.
026350	     MOVE W40-ADD1	 TO REP-NAME.
	     MOVE "SUPPLIER TEL.NO : "
				 TO REP-H4.
	     MOVE W40-TEL	 TO REP-STEL.
	     PERFORM CALL-PRINTUTL.
026380	     MOVE W40-ADD2	 TO REP-NAME.
	     MOVE "SUPPLIER FAX.NO : "
				 TO REP-H4.
	     MOVE W40-FAX	 TO REP-STEL.
	     PERFORM CALL-PRINTUTL.
026320	     MOVE SPACES	 TO R-DET.
026410	     MOVE W40-ADD3	 TO REP-NAME.
	     PERFORM CALL-PRINTUTL.
026320	     MOVE SPACES	 TO R-DET.
026440	     MOVE W40-ADD4	 TO REP-NAME.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
026510	     MOVE P-PC		 TO REP-PC1.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
	     MOVE WS-HDNG	 TO OR-HDNG.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
	     MOVE SPACES	 TO W10-CONTACT.

032420 BA15.
	     MOVE COR-ITEM	 TO STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       SUBTRACT 1	 FROM W15-OSUB
	       GO TO BA10.
	     MOVE W15-OPREF	 TO ORD-ORD.
	     MOVE COR-CRED	 TO ORD-CRED.
	     MOVE W15-OSUB	 TO OR-LINE ORD-LINE.
	   IF WS-USE-ITM = "Y"
	       MOVE COR-CODE	 TO OR-ITEM
	       MOVE "/" 	 TO OR-SL
	       MOVE COR-ITM	 TO OR-ITM
	   ELSE
	       MOVE COR-ITEM	 TO OR-CODE.
	     MOVE COR-ITEM	 TO ORD-EXT-ITEM STK-CODE.
	     MOVE ZERO		 TO WS-ACTION.
	     PERFORM AY05 THRU AY59.
	     MOVE STK-DESC	 TO OR-DESC.
	     MOVE STK-XREF	 TO OR-XREF.
	     MOVE COR-DATE	 TO ORD-DATE STK-DTE.
	     MOVE COR-QUANT	 TO ORD-QUANT OR-QNT.
	     ADD COR-QUANT	 TO STK-ORD.
	     MOVE COR-UCOST	 TO ORD-UCOST OR-COST.
	     COMPUTE W10-MTDC ROUNDED = COR-QUANT * COR-UCOST.
	     MOVE W10-MTDC	 TO ORD-COST  OR-EXT.
	     ADD W10-MTDC	 TO W25-TOTAL.
	     MOVE COR-CONTACT	 TO ORD-CONTACT.
	   IF NOT (COR-CONTACT = W10-CONTACT)
	       MOVE COR-CONTACT  TO OR-CONTACT W10-CONTACT.
	     MOVE COR-TERMS	 TO ORD-TERMS.
	     COMPUTE ORD-SELL ROUNDED = ORD-COST + ((ORD-COST *
					STK-MARKUP) / 100.0000).
	     MOVE COR-DELIV	 TO ORD-DRATE.
	     MOVE ZERO		 TO ORD-OCOST ORD-DISC
				    ORD-DELIV ORD-DDTE.
	     MOVE SPACES	 TO ORD-CURRENCY ORD-BO ORD-PSLIP
				    ORD-INST.
	     MOVE P-PMNT	 TO ORD-PMT.
	     MOVE P-TEL 	 TO ORD-TEL.
	     MOVE P-FAX 	 TO ORD-FAX.
	     PERFORM WRITE-PORDER THRU WRITE-PORDER-EXIT.
	     MOVE 1		 TO WS-ACTION.
	     PERFORM AY10 THRU AY59.
034460	     PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
016020	   IF W02-LINAGE < (W02-PRN-LENGTH - 1)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
016040     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE SPACES	 TO R-DET
	       PERFORM AY70 THRU AY999
	       GO TO BA08.
	     MOVE SPACES	 TO R-DET.
	     GO TO BA10.

       BA20.
	     MOVE SPACES	 TO R-DET.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "O R D E R   T O T A L"
				 TO OR-DESC.
	     MOVE W25-TOTAL	 TO OR-EXT.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     PERFORM CALL-PRINTUTL.
	     MOVE SPACES	 TO R-DET.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     PERFORM AY70 THRU AY999.
	     MOVE ZERO		 TO W25-TOTAL.

032560 BA999.
032570       EXIT.

       BB010	       SECTION.
       BB10.
	     MOVE W15-N29-30	 TO W10-N1-2.
	     MOVE W15-N1-2	 TO W10-N3-4.
	     MOVE W15-N27-28	 TO W10-N5-6.
	     MOVE W15-N3-4	 TO W10-N7-8.
	     MOVE W15-N25-26	 TO W10-N9-10.
	     MOVE W15-N5-6	 TO W10-N11-12.
	     MOVE W15-N23-24	 TO W10-N13-14.
	     MOVE W15-N7-8	 TO W10-N15-16.
	     MOVE W15-N21-22	 TO W10-N17-18.
	     MOVE W15-N9-10	 TO W10-N19-20.
	     MOVE W15-N19-20	 TO W10-N21-22.
	     MOVE W15-N11-12	 TO W10-N23-24.
	     MOVE W15-N17-18	 TO W10-N25-26.
	     MOVE W15-N13-14	 TO W10-N27-28.
	     MOVE W15-N15-16	 TO W10-N29-30.
       BB15.
	     EXIT.
      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
044130 ZA000-INIT	       SECTION 8.
044140 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
044150	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	   S e t   u p	 t h e	 F U N C T I O N   k e y s     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		       ESC, F1 to F10 keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		    PAGE-UP and PAGE-DOWN keys		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		   A C T I V A T E   M O U S E		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	       Set MOUSE key to act as FUNCTION key	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    **** 	  P A R A M E T E R   F I L E		    ****
      *    *************************************************************
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
      *    *************************************************************
      *    ****   P U R C H A S E   O R D E R	W O R K	  F I L E   ****
      *    *************************************************************
	     MOVE "CORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-CORDER.
      *    *************************************************************
      *    *** C R E D I T O R	 A C C O U N T	 ( A / P )   F I L E ***
      *    *************************************************************
	     MOVE "CREDIT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CREDIT.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    ****        P U R C H A S E	 O R D E R   F I L E	    ****
      *    *************************************************************
	     MOVE "PORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-PORDER.
      *    *************************************************************
      *    **** 	    R E C O V E R Y   F I L E		    ****
      *    *************************************************************
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
      *    *************************************************************
      *    ****      S T O C K	 ( I V E N T O R Y )   F I L E	    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
044230	     MOVE 3		 TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
053550	     MOVE PAR-VAT-REG	 TO W05-VATNO.
019930	     OPEN OUTPUT RECOVER.
019940       CLOSE RECOVER.
019950       OPEN I-O RECOVER.
044580	     MOVE 1		 TO WS-PARKEY.
044590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
044600       MOVE PAR-DMY        TO W12-TODAY.
044610       MOVE PAR-YMD        TO W12-T-YMD.
044620       MOVE PAR-COMPANY    TO W95-COMP.
054820       MOVE PAR-TELEPHONE  TO W95-TEL.
054870	     ADD 1		 TO WS-PARKEY.
054880       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
054890       MOVE PAR-ADDRESS1   TO W95-ADD1.
054900       MOVE PAR-ADDRESS2   TO W95-ADD2.
054910       MOVE PAR-ADDRESS3   TO W95-ADD3.
054920       MOVE PAR-ADDRESS4   TO W95-ADD4.
054930       MOVE PAR-POST-CDE   TO W95-POST.
054940     IF W95-ADD4 = SPACES
054950         MOVE W95-PC       TO W95-ADD4
054960         MOVE SPACES       TO W95-PC.
054970     IF W95-ADD3 = SPACES
054980         MOVE W95-ADD4     TO W95-ADD3
054990         MOVE SPACES       TO W95-ADD4.
055000     IF W95-ADD2 = SPACES
055010         MOVE W95-ADD3     TO W95-ADD2
055020         MOVE SPACES       TO W95-ADD3.
055030     IF W95-ADD1 = SPACES
055040         MOVE W95-ADD2     TO W95-ADD1
055050         MOVE SPACES       TO W95-ADD2.
044650	     MOVE 3		 TO WS-NETKEY.
044660       PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
044750       MOVE STK-WEEK       TO WS-WEEK.
044760       MOVE STK-PASSWORD   TO WS-PASSWORD.
044300	     OPEN I-O CORDER.
           IF WS-STATUS = "05"
	       CLOSE CORDER
	       OPEN OUTPUT CORDER
	       CLOSE CORDER
	       OPEN I-O CORDER.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
		   MOVE 48	 TO WS-F-ERROR
                   GO TO ZA50.
	     GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
039850 ZA50.
041850	   IF WS-F-ERROR = 48
041860	       MOVE "ORDER file"
				 TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.
       ZA55.
041220       MOVE 1              TO WS-S1.
021870	     MOVE SPACES	 TO WS-MID-LNE.
041240 ZA60.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2          TO WS-TCHR (18) WS-TCHR (49)
021940                              WS-TCHR (60) WS-TCHR (70).
021950       MOVE WS-G3          TO WS-MCHR (18) WS-MCHR (49)
021960                              WS-MCHR (60) WS-MCHR (70)
				    WS-MCHR (1)  WS-MCHR (80).
021970       MOVE WS-G4          TO WS-BCHR (18) WS-BCHR (49)
021980                              WS-BCHR (60) WS-BCHR (70).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
044790 ZA200.
044800       DISPLAY "Files locked - Try later" AT 2312
044810                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
044820       DISPLAY "Press any key" AT 2512 
044830		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
044840	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
044870 ZA999-EXIT.
044880       EXIT.
      /
044890 ZB000-ERROR	       SECTION 8.
      
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
041510       PERFORM DISPLAY-FILE-NAME.
      
041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK  TO WS-FILE
041630         MOVE WS-NETKEY    TO WS-KEY
041800	   ELSE
041650	   IF WS-F-ERROR = 3
041660         MOVE W02-CREDIT   TO WS-FILE
041670         MOVE ZERO         TO WS-KEY
               MOVE P-NUMBER     TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 16
041860         MOVE W02-PORDER   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
041840	   ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 48
041860	       MOVE W02-CORDER	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE COR-KEY	 TO WS-KEYX.
041880	   IF WS-STATUS = "10"
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
