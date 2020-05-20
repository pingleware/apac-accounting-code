      $SET LINKCOUNT"800"
      ******************************************************************
      *                                                                *
      *   ********   ******   *******	********     **     ******     *
      *      **     **	  **  **    **	   **	    ***    **	 **    *
      *      **     **	  **  **    **	   **	     ** 	**     *
      *      **     **	  **  *******	   **	     **       **       *
      *      **     **	  **  **    **	   **	     **     **	       *
      *   ** **     **	  **  **    **	   **	     **    **	       *
      *    ***	     ******   *******	********   ******  ********    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *      P R I N T   I N V O I C E   P R O G R A M    (J O B)      *
      *                                                                *
      *     VERSION 8.15.01 - August 2011			       *
      * 							       *
      ******************************************************************
      * 							       *
      *  September 2005 - Details of JOBS introduced. New sort key     *
      * 		  JOB-DETAILS = JOB-TYPE JOB-EXT-ITEM JOB-REF  *
      * 							       *
      *  May 2006	- New sort key (Lookup from Stock Item)        *
      * 		  JOB-CODE = JOB-EXT-ITEM JOB-REF	       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     JOBI12.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   SEPTEMBER 1991.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2011
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
000080                 CURSOR IS CSTART
000090		       CONSOLE IS CRT
		       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000130 COPY AUDIT.SL.

       COPY CARDEX.SL.

000150 COPY CONTROL.SL.

000170 COPY DBTRAN.SL.

000150 COPY DEBTOR.ISL.

000210 COPY DEPART.SL.

000250 COPY GARTEE.SL.

000250 COPY INVOIC.SL.

       COPY LEDTRF.SL.

000270 COPY PARAM.SL.

000290 COPY RECOVER.SL.

000310 COPY SALE.SL.

000250 COPY JOBCST.SL.

000250 COPY JOBHDR.SL.

       COPY SHARED.SL.

000330 COPY STOCK.SL.

000350 COPY TXTRAN.SL.

           SELECT CSHDRW  ASSIGN DISK
                          ORGANIZATION SEQUENTIAL
                          ACCESS SEQUENTIAL.

000370     SELECT PRNREP  ASSIGN W02-PRINTER
000380                    ORGANIZATION LINE SEQUENTIAL.

      /
000390 DATA DIVISION.
000400 FILE SECTION.

       COPY APACFIDS.FDE.

000420 COPY AUDIT.FDE.

       COPY CARDEX.FDE.

000440 COPY CONTROL.FDE.

000460 COPY DBTRAN.FDE.

000480 COPY DEBTOR.FDE.

000500 COPY DEPART.FDE.

000250 COPY GARTEE.FDE.

000540 COPY INVOIC.FDE.

000540 COPY JOBCST.FDE.

000540 COPY JOBHDR.FDE.

       COPY LEDTRF.FDE.

000560 COPY PARAM.FDE.

000580 COPY RECOVER.GFD.

000600 COPY SALE.JFD.

       COPY SHARED.FDE.

000620 COPY STOCK.FDE.

000640 COPY TXTRAN.FDE.

      *
      *    O L D   S T Y L E   C A S H   D R A W E R
      *
       FD  CSHDRW    LABEL RECORD STANDARD
                     VALUE OF FILE-ID "COM1".
       01  CSH-REC.
           03  CSH-OPEN     PIC  X(06).

000660 FD  PRNREP    LABEL RECORD OMITTED
000670               LINAGE WS-PGE-LENGTH.
000680 01  REP-LINE1.
000690	   03  REP-DETAIL1     PIC X(132).

000700 01  REP-L1.
000710     03  FILLER          PIC X(10).
000720     03  BAL-REP         PIC X(60).
000730     03  FILLER          PIC X(10).

000740 01  REP-LINE2.
000750	   03  REP-DETAIL2     PIC X(132).

000910 01  INV-L1.
	   03  INV-EXP	       PIC  X(01).
000920	   03  FILLER	       PIC  X(03).
	   03  INV-EXPNAME     PIC  X(40).
	   03  FILLER	       PIC  X(88).

       01  INV-L2.
	   03  FILLER	       PIC  X(06).
000930	   03  INV-NADD        PIC  X(44).
000940     03  INV-RADD      REDEFINES INV-NADD.
000950	       05  INV-PC      PIC  X(04).
000960	       05  FILLER      PIC  X(40).
	   03  FILLER	       PIC  X(40).
001120	   03  INV-H2	       PIC  X(16).
001130	   03  INV-TEL	       PIC  X(14).
	   03  INV-GNO REDEFINES INV-TEL
			       PIC  X(14).
	   03  FILLER	       PIC  X(12).

000980 01  INV-L3.
000990	   03  FILLER	       PIC  X(06).
001000	   03  INV-NAME        PIC  X(40).
001010     03  INV-DADD      REDEFINES INV-NAME.
001020	       05  INV-PC1     PIC  X(04).
001030	       05  FILLER      PIC  X(36).
001040	   03  FILLER	       PIC  X(06).
	   03  INV-H3	       PIC  X(08).
001060	   03  INV-DADD2.
001070	       05  INV-PC2     PIC  X(30).
001090	   03  FILLER	       PIC  X(10).
	   03  INV-H4	       PIC  X(19).
001550	   03  INV-ORD.
	       05  INV-TERMS.
		   07  FILLER  PIC  X(05).
		   07  INV-AC.
		       09  FILLER  PIC	X(03).
		       09  INV-PGE PIC	ZZ9.
	       05  FILLER      PIC  X(01).
	   03  FILLER	       PIC  X(01).

001100 01  INV-L4.
001490	   03  INV-H5	       PIC  X(21).
001140	   03  INV-H6	       PIC  X(32).
001510	   03  INV-H7	       PIC  X(14).
	   03  INV-H8	       PIC  X(13).
	   03  INV-H9	       PIC  X(11).
	   03  INV-H10	       PIC  X(15).
	   03  INV-H11	       PIC  X(14).
	   03  INV-H12	       PIC  X(12).

001150 01  INV-L5.
	   03  INV-ITM-MES.
	       05  INV-EXT-ITM.
001780		   07  INV-ITM PIC  X(14).
		   07  INV-SL  PIC  X(01).
		   07  INV-EXT PIC  X(03).
	       05  FILLER      PIC  X(03).
001790	       05  INV-MES.
001810		   07  INV-DESC	PIC X(30).
001870	   03  INV-REMARKS REDEFINES INV-ITM-MES.
001880	       05  FILLER      PIC  X(11).
001890	       05  INV-RMK     PIC  X(40).
	   03  FILLER	       PIC  X(03).
001760	   03  INV-QNT	       PIC  Z(06)9.99-.
001770	   03  FILLER	       PIC  X(02).
001930	   03  INV-PRICE       PIC  Z(06)9.99-.
	   03  FILLER	       PIC  X(03).
001850	   03  INV-DSC	       PIC  Z9.99.
	   03  FILLER	       PIC  X(03).
	   03  INV-EXTEND      PIC  Z(08)9.99-.
	   03  FILLER	       PIC  X(02).
001910	   03  INV-TAX	       PIC  Z(06)9.99- BLANK WHEN ZERO.
001940	   03  INV-NC.
	       05  FILLER      PIC  X(03).
001950	       05  INV-VAL     PIC  Z(09)9.99-.

       01  INV-L6.
	   03  FILLER	       PIC  X(09).
           03  INV-HD          PIC  X(10).
           03  INV-SERNOS.
               05  FILLER      PIC  X(01).
               05  INV-SERNO   PIC  X(10).
	       05  FILLER      PIC  X(25).
	   03  FILLER	       PIC  X(77).

001970 01  INV-L7.
001980	   03  FILLER	       PIC  X(18).
001990	   03  INV-REMKS.
002010	       05  INV-RMKS.
		 07  FILLER    PIC  X(40).
002020	   03  FILLER	       PIC  X(17).
	   03  INV-H13	       PIC  X(12).
	   03  INV-LINE        PIC  X(45).

       01  INV-L8.
	   03  INV-EXP2        PIC  X(01).
001540	   03  FILLER	       PIC  X(16).
	   03  INV-INVOICE.
	       05  FILLER      PIC  X(03).
	       05  INV-CRNOTE.
		   07  FILLER  PIC  X(06).
		   07  INV-COPY
			       PIC  X(12).
		   07  FILLER  PIC  X(06).
	       05  FILLER      PIC  X(03).
	   03  FILLER	       PIC  X(02).
	   03  INV-H14	       PIC  X(08).
	   03  INV-REF.
	       05  INV-DTE     PIC  99/99/9999.
      /
002090 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002100 77  WS-DISC	   PIC	99V99	 COMP-3.
002110 77  WS-TDISC        PIC  99V99    COMP-3.
002120 77  WS-CASH-AC      PIC  X(06).
002130 77  WS-SUB          PIC  9(04)    COMP-5.
002170 77  WS-SUPER        PIC  9(02)    COMP-5.
002140 77  WS-S1           PIC  9(04)    COMP-5.
002150 77  WS-S2           PIC  9(04)    COMP-5.
002160 77  WS-S3           PIC  9(04)    COMP-5.
002170 77  WS-S4           PIC  9(04)    COMP-5.
002180 77  WS-S5           PIC  9(04)    COMP-5.
002190 77  WS-S6           PIC  9(04)    COMP-5.
002200 77  WS-S7           PIC  9(04)    COMP-5.
002210 77  WS-S8           PIC  9(04)    COMP-5.
002220 77  WS-S9           PIC  9(04)    COMP-5.
002230 77  WS-IXS          PIC  9(04)    COMP-5.
002240 77  WS-IXS1         PIC  9(04)    COMP-5.
002250 77  WS-IXS2         PIC  9(04)    COMP-5.
002260 77  WS-IXS3         PIC  9(04)    COMP-5.
002270 77  WS-IXS4         PIC  9(04)    COMP-5.
002280 77  WS-PARKEY       PIC  9(04)    COMP-5.
002300 77  WS-KEYSTR	   PIC	9(06)	 COMP-5.
002330 77  WS-SALSTR	   PIC	9(04)	 COMP-5.
002340 77  WS-SALKEY       PIC  9(04)    COMP-5.
002350 77  WS-NETKEY       PIC  9(04)    COMP-5.
002360 77  WS-AUDKEY       PIC  9(06)    COMP-5.
002370 77  WS-RECKEY       PIC  9(06)    COMP-5.
002380 77  WS-RECOVER      PIC  9(06)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
000820 77  WS-STOCK	   PIC	X(01).
       77  WS-CARDEX	   PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
       77  WS-PRC-IND	   PIC	X(01).
002450 77  WS-INV          PIC  9(01).  
002450 77  WS-GAR          PIC  9(01) VALUE 0.  
002400 77  WS-PRN1         PIC  9(01) VALUE 0.  
002410 77  WS-PRN2         PIC  9(01) VALUE 0.  
002430 77  WS-START	   PIC	9(08).
002440 77  WS-END          PIC  9(08).
002460 77  WS-TME2	   PIC	9(08).
002520 77  WS-PORT	   PIC	9(05) VALUE 259.
      * 1004.
       77  WS-DRAW1	   PIC	9(05) VALUE 260.
002480 77  WS-OPEN	   PIC	X(01) VALUE X"01".
002490 77  WS-CLOSE        PIC  X(01) VALUE X"00".
002500 77  WS-PAR          PIC  9(01) VALUE 0.
002510 77  WS-INO          PIC  9(02) VALUE 0.
002520 77  WS-LINES        PIC  9(02) VALUE 0.
002530 77  WS-FIRST        PIC  9(02) VALUE 0.
002540 77  WS-LAST         PIC  9(02) VALUE 0.
002550 77  WS-LASTINV      PIC  9(04)    COMP-5.
002560 77  WS-LINE         PIC  9(04)    COMP-5.
002570 77  WS-PAGE         PIC  9(02)    COMP-5.
002580 77  WS-L1           PIC  9(04)    COMP-5.
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
       77  WS-PRN-TYPE	   PIC	X(01) VALUE " ".
003650 77  WS-COND	   PIC	X(06).
003660 77  WS-NORM	   PIC	X(06).
003070 77  WS-EXPP	   PIC	X(06).
003080 77  WS-ECAN	   PIC	X(06).
003090 77  WS-8LPI	   PIC	X(06).
003100 77  WS-6LPI	   PIC	X(06).
       77  WS-10CPI	   PIC	X(06).
       77  WS-12CPI	   PIC	X(06).
       77  WS-17CPI	   PIC	X(06).
002590 77  WS-OPTION	   PIC	X(01).
002600 77  WS-CSHSLE	   PIC	X(01) VALUE "N".
002610 77  WS-SKIP         PIC  X(01) VALUE " ".
002610 77  WS-LOOK         PIC  X(01) VALUE "S".
002620 77  WS-ETYPE        PIC  X(01) VALUE "I".
002650 77  WS-ERROR        PIC  9(01).
002690 77  WS-COMP	   PIC	9(01).
002710 77  WS-UPDATE       PIC  9(01) VALUE ZERO.
002720 77  WS-CONREC       PIC  9(01) VALUE ZERO.
002730 77  WS-HEAD         PIC  X(01).
002740 77  WS-TYPE         PIC  X(01).
002750 77  WS-AGE          PIC  X(01).
002760 77  WS-PRC          PIC  X(01).
002830 77  WS-BATCH	   PIC	9(05).
002880 77  WS-PER	   PIC	Z(02)9.99.
002890 77  WS-CR           PIC  X(01) VALUE "I".
002900 77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.
      /
002910 01  WS-DB-LINE.
002920     03  WS-TOP-LINE.
002930         05  WS-TCHAR PIC X(01) OCCURS 80.
002940     03  WS-TOP-LNE2.
002950         05  WS-TCR  PIC X(01) OCCURS 80.
002960     03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
002970         05  FILLER      PIC  X(01).
002980         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003000	   03  WS-MID-LINE.
001450	       05  WS-MCHAR PIC  X(80) VALUE "³              ³   ³
      -        "                       ³        ³         ³          ³".
	   03  WS-MINV-LINE REDEFINES WS-MID-LINE.
	       05  WS-MIV1     PIC  X(01).
	       05  WS-MIV78    PIC  X(78).
	       05  WS-MIV80    PIC  X(01).
003020	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
003040	   03  WS-BOT-LINE.
003050         05  WS-BCHAR PIC X(01) OCCURS 80.
003060     03  WS-BOT-LNE2.
003070         05  WS-BCR  PIC X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
003260         05  WS-SRT-LINE
003270                     PIC X(50).
003280         05  FILLER  PIC X(29).
003080     03  WS-TOP-LNE.
003090         05  WS-TCHR PIC X(01) OCCURS 80.
003100     03  WS-T-LINE REDEFINES WS-TOP-LNE.
003110         05  FILLER  PIC X(01).
003120         05  WS-H-LINE
003130                     PIC X(78).
003140         05  FILLER  PIC X(01).
003150     03  WS-MID-LNE.
003160         05  WS-MCHR PIC X(01) OCCURS 80.
003170     03  WS-BOT-LNE.
003180         05  WS-BCHR PIC X(01) OCCURS 80.
003190     03  WS-TP-LNE.
003200         05  WS-TCH  PIC X(01) OCCURS 80.
003210     03  WS-MD-LNE.
003220         05  WS-MCH  PIC X(01) OCCURS 80.
003230     03  WS-BT-LNE.
003240         05  WS-BCH  PIC X(01) OCCURS 80.
003190     03  WS-TP-LINE.
003200         05  WS-TCA  PIC X(01) OCCURS 80.
003210     03  WS-MD-LINE.
003220         05  WS-MCA  PIC X(01) OCCURS 80.
003230     03  WS-BT-LINE.
003240         05  WS-BCA  PIC X(01) OCCURS 80.

003340 COPY WS.WS.

       COPY LEDTRF.WS.

003380 01  W00-TRAN-CODES.
           03  W00-T-VAT   PIC S9(09)V99 COMP-3.
003410     03  W00-E-DESC  PIC  X(12).
003420     03  W00-A-DESC  PIC  X(12).
003430     03  W00-ACTION  PIC  9(01).
003440     03  W00-T-CODE  PIC  9(02)    COMP-5.
003450     03  W00-T-GL    PIC  9(06)    COMP-3.
003460     03  W00-T-DAY   PIC  9(06)    COMP-5.
003470     03  W00-T-VAL   PIC S9(09)V99 COMP-3.
003480     03  W00-T-MTD   PIC  9(06)    COMP-5.
003490     03  W00-T-MTDV  PIC S9(09)V99 COMP-3.
003500     03  W00-T-YTD   PIC  9(06)    COMP-5.
003510     03  W00-T-YTDV  PIC S9(09)V99 COMP-3.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003530 01  W02-FID.

       COPY APACFIDS.ID.

003680 COPY AUDIT.ID.

       COPY CARDEX.ID.

003570 COPY CONTROL.ID.

003720 COPY DBTRAN.ID.

003610 COPY DEBTOR.ID.

003760 COPY DEPART.ID.

000250 COPY GARTEE.ID.

003800 COPY INVOIC.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

003820 COPY RECOVER.ID.

003710 COPY SALE.ID.

003670 COPY JOBCST.ID.

003670 COPY JOBHDR.ID.

       COPY SHARED.ID.

003730 COPY STOCK.ID.

003880 COPY TXTRAN.ID.

003770 01  W02-FILE-IDS.
003910     03  W02-PRINTER.
	       05  W02-REPORT PIC X(07) VALUE "JOBI07.".
               05  W02-USER   PIC X(05) VALUE SPACES.

       COPY W05.VAT.

003890 01  W05-INV.
003900     03  W05-ENTRY       PIC  ZZ9.
003910     03  FILLER          PIC  X(01) VALUE SPACES.
003920     03  W05-DATE        PIC  Z9/99/99.
003930     03  FILLER          PIC  X(01) VALUE SPACES.
003940     03  W05-TYPE        PIC  X(03).
003950     03  FILLER          PIC  X(03) VALUE SPACES.
003960     03  W05-REF         PIC  9(07).
003970     03  FILLER          PIC  X(01) VALUE SPACES.
003980     03  W05-VALUE       PIC  Z(06)9.99-.

001270 01  W08-EXPD.
	   03  W08-CHAR        PIC  X(06).
001280	   03  W08-DEC REDEFINES W08-CHAR
			       PIC  X(01) COMP-X OCCURS 6.

       COPY W10.JCP.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

       COPY W11.STK.

       COPY W12.WS.

004410 01  W15-DISCOUNT.
004420     03  W15-DISC        PIC S9(03)V99 COMP-3 OCCURS 10.

       COPY W20.WS.

004520 01  W25-CALCS.
004730     03  W25-RESULT      PIC 9(05)V99.
004740     03  W25-RESULT1 REDEFINES W25-RESULT.
004750         05  W25-WHOLE   PIC 9(03).
004760         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
004570	   03  W25-TOTAL	     PIC 9(07)V99  COMP-3.
004580     03  W25-VALUE       PIC S9(07)V99 COMP-3.
004590     03  W25-CREDIT      PIC S9(07)V99 COMP-3.
       01  W30-MESSAGE.
           03  W30-PERC        PIC Z9.99.
           03  FILLER          PIC  X(40) VALUE 
               "% Settlement discount if paid in 30 DAYS".

       COPY W40.DBT.

       COPY W40.WS.

       COPY FUNCTION.WS.

005370 COPY W50.WS.

005390 01  W60-NME-ADD.
005400     03  W60-NAME.
005410         05  W60-NCHAR OCCURS 40 PIC X(01).
005420     03  W60-ADDRESS.
005430         05  W60-ACHAR OCCURS 60 PIC X(01).
005440	   03  W60-ADDRESS1.
               05  W60-CHR   OCCURS 60 PIC X(01).
005450	   03  W60-ITM.
005460         05  W60-ICHAR OCCURS 14 PIC X(01).

005620 COPY W70DEBT.IWS.

005640 01  W75-KEYS.
005650     03  W75-S          PIC 9(02) COMP-5.
005660     03  W75-S1         PIC 9(02) COMP-5.
005670     03  W75-DNO        PIC X(06) OCCURS 18.
           03  W75-CODE       PIC X(14) OCCURS 18.
           03  W75-JCST       PIC 9(08) OCCURS 18.
005690 01  W75-TITLES.
005700     03  W75-DESCRIP.
005710         05  FILLER      PIC X(28)      VALUE
005720                     "Mr     Mnr    Mrs    Mev    ".
005730         05  FILLER      PIC X(28)      VALUE
005740                     "Miss   Mej    Ms     Ms     ".
005750         05  FILLER      PIC X(28)      VALUE
005760                     "Dr     Dr     Rev    Ds     ".
005770         05  FILLER      PIC X(28)      VALUE
005780                     "Prof   Prof   The HonSy Edel".
005790         05  FILLER      PIC X(28)      VALUE
005800                     "                            ".
005810     03  W75-CODES REDEFINES W75-DESCRIP.
005820         05  W75-TITLE  OCCURS 10.
005830             07  W75-ETITLE PIC X(07).
005840             07  W75-ATITLE PIC X(07).
005860 01  W80-ACCOUNT.
005870     03  W80-ACNO        PIC X(06).
005880     03  W80-CRED REDEFINES W80-ACNO.
005890         05  W80-CNO     PIC 9(05).
005900         05  W80-CAC REDEFINES W80-CNO.
005910             07  W80-CTYPE PIC 9(01).
005920             07  W80-SER   PIC 9(04).
005930         05  FILLER        PIC 9(01).
005940     03  W80-TYPE        PIC 9(01).

       COPY W85.WS.

006090 01  W85-PASS.
006100     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006110     03  W85-ENTRY       PIC  9(02)    COMP-3.
006120     03  W85-MARG        PIC S9(02)V99.
006130 01  W85-BATCHES.
006140         05  W85-BATCH   PIC 9(05).

       COPY W90.JCP.

006590 01  W95-STM.
006600     03  W95-COMP        PIC X(40).
006610     03  W95-ADD1        PIC X(30).
006620     03  W95-ADD2        PIC X(30).
006630     03  W95-ADD3        PIC X(30).
006640     03  W95-ADD4        PIC X(30).
006650     03  W95-PC.
006660         05  W95-POST    PIC 9(04).
006670     03  W95-TEL         PIC X(14).
006680     03  W95-V1.
006690         05  W95-T       PIC Z(04)9.99.
006700     03  W95-V2.
006710         05  W95-B       PIC Z(06)9.99-.
006720     03  W95-D.
006730	       05  W95-DTE     PIC Z9/99/9999.

006740 01  W100-EDIT.
004500     03  W100-LBAL       PIC Z(06)9.99-.
004500     03  W100-LARR       PIC Z(06)9.99.
006750     03  W100-BAL        PIC Z(08)9.99-.
006760     03  W100-CUR        PIC Z(08)9.99-.
006770     03  W100-30         PIC Z(08)9.99-.
006780     03  W100-60         PIC Z(08)9.99-.
006790     03  W100-90         PIC Z(08)9.99-.
006800     03  W100-120        PIC Z(08)9.99-.
006800     03  W100-INT        PIC Z(08)9.99-.
006810     03  W100-CRL        PIC Z(06)9.
           03  W100-SER        PIC ZZ9.
006820     03  W100-V2.
006830         05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
006840 SCREEN SECTION.

       COPY BLANK.CRT.

007910 01  S07.
007920	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
				 PIC X(14) USING W10-ITEM AUTO.

       01  S07-ITM.
005540	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(14) USING W10-ITEM AUTO.
	   03	       COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(03) USING W10-ITM AUTO.

005530 01  S07-EXT.
005540	   03	       COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
005550				 PIC X(18) USING W10-EXT-ITEM AUTO.

009270 01  S26.
009280     03  LINE  2 COLUMN 37 FOREGROUND-COLOR 15 VALUE "INVOICE".
009290     03  LINE  3 COLUMN  2 VALUE "AC No".
009300     03          COLUMN 15 VALUE "Name".
009310     03          COLUMN 61 VALUE "Ord No             ".
009320	   03  LINE  4 COLUMN  2 VALUE "Tech".
009330     03  LINE  5 COLUMN  2 VALUE "Inv. No".
	   03	       COLUMN 20 VALUE "Contact".
009360	   03	       COLUMN 65 VALUE "Date".
009370	   03  LINE  6 COLUMN  2 REVERSE-VIDEO VALUE "Item Code      I".
009380	   03	       COLUMN 18 REVERSE-VIDEO VALUE "TM Description  ".
009390     03          COLUMN 33 REVERSE-VIDEO VALUE "                ".
009400     03          COLUMN 49 REVERSE-VIDEO VALUE "  Quantity ".
009410     03          COLUMN 60 REVERSE-VIDEO VALUE "Unit Price  ".
009420     03          COLUMN 72 REVERSE-VIDEO VALUE "Extended".
009160	   03  LINE  7 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009160	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
009160	   03  LINE 19 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009170	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
010190	   03  LINE 20 COLUMN  3 VALUE "Sub Total ".
010200	   03  LINE 21 COLUMN  3 VALUE "Discount  ".
010210     03  LINE 22 COLUMN  3 VALUE "T A X -VAT".
010230	   03  LINE 23 COLUMN  3 VALUE "AMOUNT DUE".

009490 01  S26B.
009500     03  LINE 20 COLUMN 35 VALUE "Payment Received -".
009510     03          COLUMN 54 VALUE "Cash".
009520     03          COLUMN 66 VALUE ":".
009530     03  LINE 21 COLUMN 54 VALUE "Cheque".
009540     03          COLUMN 66 VALUE ":".
009550     03  LINE 22 COLUMN 54 VALUE "Credit Card :".
009560     03          COLUMN 66 VALUE ":".
009570     03  LINE 23 COLUMN 34 VALUE "-Balance".
009580     03          COLUMN 54 VALUE "CHANGE".
009590     03          COLUMN 66 VALUE ":".
009620 01  S26-QNT.
009630     03          COLUMN 51 FOREGROUND-COLOR 11
009640                           PIC Z(04)9.99 USING W10-QUANT AUTO.
009650 01  S26-PRC.
009660     03          COLUMN 60 FOREGROUND-COLOR 11
009670                           PIC Z(05)9.99 USING W10-SELL AUTO.
009680 01  S26-DSC.
009690     03          COLUMN 30 FOREGROUND-COLOR 11
009700                           PIC Z9.99 USING WS-DISC AUTO.
009710 01  S26-TDSC.
009720     03          COLUMN 15 FOREGROUND-COLOR 11
009730                           PIC Z9.99 USING WS-TDISC AUTO.
009740 01  S26-AVGE.
009750     03          COLUMN 28 FOREGROUND-COLOR 11
009760				 PIC Z(04)9.999 USING STK-AVGE AUTO.
009890 01  S28.
009900     03  LINE 25 COLUMN 25 FOREGROUND-COLOR 15
009910                           PIC ZZ9 USING WS-SALKEY AUTO.
       01  S30.
           03  LINE  8 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G9.
           03          COLUMN 16 FOREGROUND-COLOR 14
                                 PIC X(50) USING WS-SRT-LINE.
           03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G10.
007450     03  LINE  9 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 34 FOREGROUND-COLOR 7
                                 VALUE "CLIENT DETAILS".
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
007450     03  LINE 10 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 16 FOREGROUND-COLOR 7
                                 VALUE "Name    >".
           03          COLUMN 65 FOREGROUND-COLOR 7
                                 VALUE "<".     
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
007450     03  LINE 11 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 16 FOREGROUND-COLOR 7
                                 VALUE "Address >".
           03          COLUMN 55 FOREGROUND-COLOR 7
                                 VALUE "<".
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
007450     03  LINE 12 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 24 FOREGROUND-COLOR 7
                                 VALUE ">".
           03          COLUMN 55 FOREGROUND-COLOR 7
                                 VALUE "<".
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
007450     03  LINE 13 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 24 FOREGROUND-COLOR 7
                                 VALUE ">".
           03          COLUMN 55 FOREGROUND-COLOR 7
                                 VALUE "<".
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
007450     03  LINE 14 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 24 FOREGROUND-COLOR 7
                                 VALUE ">".
           03          COLUMN 55 FOREGROUND-COLOR 7
                                 VALUE "<".
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
007450     03  LINE 15 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 24 FOREGROUND-COLOR 7
                                 VALUE ">".
           03          COLUMN 29 FOREGROUND-COLOR 7
                                 VALUE "<".
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
007450     03  LINE 16 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 16 FOREGROUND-COLOR 7
                                 VALUE "Tel.    >".
           03          COLUMN 39 FOREGROUND-COLOR 7
                                 VALUE "<".
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
007450     03  LINE 17 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03          COLUMN 16 FOREGROUND-COLOR 7
                                 VALUE "Id.No.  >".
           03          COLUMN 39 FOREGROUND-COLOR 7
                                 VALUE "<".
007460     03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G3.
           03  LINE 18 COLUMN 15 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G11.
           03          COLUMN 16 FOREGROUND-COLOR 14
                                 PIC X(50) USING WS-SRT-LINE.
           03          COLUMN 66 FOREGROUND-COLOR 14
                                 PIC X(01) USING WS-G12.
       01  S31.
           03  LINE 10 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(40) USING W40-NM1 AUTO.
           03  LINE 11 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD1 AUTO.
           03  LINE 12 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD2 AUTO.
           03  LINE 13 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD3 AUTO.
           03  LINE 14 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD4 AUTO.
           03  LINE 15 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(04) USING W40-PC AUTO.
           03  LINE 16 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(14) USING W40-HTEL AUTO.
           03  LINE 17 COLUMN 25 FOREGROUND-COLOR 15
                                 PIC X(14) USING W40-ID AUTO.
009930 01  S36.
009940     03  FOREGROUND-COLOR 11 PIC X(06) USING W80-ACNO AUTO.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
010220 PROCEDURE DIVISION USING LS-PARID
				LS-USER-ID
				LS0-PROGRAMS
				W40-COMPANY
				W90-TRAN.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
010270       PERFORM ZA000-INIT.
010430	     GO TO BA00.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

010950 COPY LOCKED.RC2.

010970 COPY CLEAR.CRT.

010970 COPY DEBTOR.TRN.

      /
      *    ****    READ FILES ROUTINES
      *
011130 AC000              SECTION.

011130 COPY AUDIT.RD.

011170 COPY CONTROL.RD.

011170 COPY DBTRAN.RD1.

011210 COPY DEBTOR.RDI.

011210 COPY DEPART.RD.

000250 COPY GARTEE.RD.

011250*COPY INVOIC.RD.

011270 COPY JOBCST.RD.

011270 COPY JOBHDR.RD.

       COPY LEDTRF.RD.

011290 COPY PARAM.RD.

011310 COPY SALE.RD.

       COPY SHARED.RD.

011330 COPY STOCK.RD.

011330 COPY TXTRAN.RD.

      /
011370 AD000-WRITE             SECTION.

011370 COPY AUDIT.WR.

       COPY CARDEX.WR.

011410 COPY CONTROL.WR.

011410 COPY DBTRAN.WR.

011430 COPY DEBTOR.WR.

011450 COPY DEPART.WR.

000250 COPY GARTEE.WR.

011490 COPY INVOIC.WR.

011270 COPY JOBCST.WR.

011270 COPY JOBHDR.WR.

       COPY LEDTRF.WR.

011410 COPY PARAM.WR.

011530 COPY SALE.WR.

       COPY SHARED.WR.

011550 COPY STOCK.WR.

011570 COPY TXTRAN.WR.

011590 COPY CASHDRAW.OP2.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
011670 AY000           SECTION.
011680 AY01.
011690       MOVE 01             TO REC-FILE.
011700       MOVE WS-PARKEY      TO REC-KEY.
011710       PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
011720       MOVE PAR-RECORD1    TO REC-PARAM.
011730       GO TO AY50.
011740 AY02.
011750       MOVE 02             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE ZERO           TO REC-TYPE
011770       PERFORM READ-DEBTOR-LOCK THRU READ-DEBTOR-EXIT.
011780       MOVE DEB-RECORD1    TO REC-DEBTOR.
011790       GO TO AY50.
011800 AY04.
011810       MOVE 04             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011830       MOVE TRA-RECORD1    TO REC-DBTRAN.
011840       GO TO AY50.
011850 AY05.
011860       MOVE 05             TO REC-FILE.
011870       MOVE ZERO           TO REC-KEY.
011880       MOVE ZERO           TO REC-TYPE.
011890       PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
011900       MOVE STK-RECORD1    TO REC-STOCKF.
011910       GO TO AY50.
011850 AY09.
011860       MOVE 09             TO REC-FILE.
011870       MOVE ZERO           TO REC-KEY.
011880       MOVE WS-ACTION      TO REC-TYPE.
011900       MOVE GUA-RECORD1    TO REC-GARTEE.
011910       GO TO AY50.
011920 AY13.
011930       MOVE 13             TO REC-FILE.
011940       MOVE ZERO           TO REC-KEY.
012000       MOVE WS-ACTION      TO REC-TYPE.
011950       MOVE JOB-REC1       TO REC-JOBCST.
011960       GO TO AY50.
011920 AY16.
011930       MOVE 16             TO REC-FILE.
011940       MOVE WS-AUDKEY      TO REC-KEY.
011950       MOVE AUD-REC1       TO REC-AUDITF.
011960       GO TO AY50.
011970 AY19.
011980       MOVE 19             TO REC-FILE.
011990       MOVE ZERO           TO REC-KEY.
012000       MOVE WS-ACTION      TO REC-TYPE.
012010       MOVE TAX-RECORD1    TO REC-TXTRAN.
012020       GO TO AY50.
012020 AY21.
012030	     MOVE 21		 TO REC-FILE.
012040       MOVE ZERO           TO REC-KEY.
012050       MOVE WS-ACTION      TO REC-TYPE.
012060	     MOVE JHD-REC1	 TO REC-JOBHDR.
012070       GO TO AY50.
012030 AY22.
012040       MOVE 22             TO REC-FILE.
012050       MOVE ZERO           TO REC-KEY.
012060       MOVE WS-ACTION      TO REC-TYPE.
012070       MOVE DOC-REC1       TO REC-INVOIC.
012080       GO TO AY50.

012150 AY37.
012160	     MOVE 37		 TO REC-FILE.
012170	     MOVE WS-SHARED	 TO REC-KEY.
012180	     MOVE SHR-RECORD	 TO REC-NETWORK.
012190       GO TO AY50.

012090 AY38.
	     MOVE 38		 TO REC-FILE.
008020	     MOVE ZERO		 TO REC-KEY.
	     MOVE ZERO		 TO REC-TYPE.
012120	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE "XXXX"	 TO DPT-CODE
	       GO TO AY38.
012130	     MOVE DPT-RECORD	 TO REC-DEPART.
012140       GO TO AY50.

012150 AY39.
012160       MOVE 39             TO REC-FILE.
012170       MOVE WS-NETKEY      TO REC-KEY.
012180       MOVE NET-RECORD     TO REC-NETWORK.
012190       GO TO AY50.
012200 AY40.
011080       MOVE 40             TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100       MOVE XFR-REC        TO REC-LEDTRF.
011110       GO TO AY50.
011070 AY43.
011080	     MOVE 43		 TO REC-FILE.
008020       MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
011100	     MOVE CRD-REC1	 TO REC-CARDEX.
011110       GO TO AY50.
012200 AY49.
012210       MOVE 99             TO REC-FILE.
012220       MOVE ZERO           TO REC-KEY.
012230       MOVE SPACES         TO REC-DETAIL.
012240       PERFORM AY50.
012250       ADD 1               TO WS-TRANS.
012260     IF WS-RECOVER > 95
012270         CLOSE RECOVER
012280         OPEN OUTPUT RECOVER
012290         CLOSE RECOVER
012300         OPEN I-O RECOVER
012310         MOVE ZERO         TO WS-RECOVER.
012320       GO TO AY59.
012330 AY50.
012340       ADD 1               TO WS-RECOVER.
012350       MOVE WS-RECOVER     TO WS-RECKEY.
012360       MOVE WS-TRANS       TO REC-TRAN.
012370       WRITE REC-RECORD.
012380     IF WS-STATUS NOT = "00"
012390         DISPLAY "Write error Recovery file - Status " AT 2212
012400                  WITH FOREGROUND-COLOR 14
012410                  WS-STATUS WITH FOREGROUND-COLOR 15
012420         STOP RUN.
012450 AY59.
012460       EXIT.
      *
      *    ****    Start processing transaction
      *
012470 AY60.
012480	   IF WS-SKIP = "Y" OR "A"
012490         GO TO AY999.
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
012620	     MOVE "DS"		 TO PAR-PROG(WS-USUB).
012630	     MOVE LS-USER	 TO PAR-USR(WS-USUB).
012640       PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012670	     MOVE 1		 TO WS-NETKEY.
012680       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
012690       PERFORM AY39 THRU AY59.
012700       MOVE NET-DEBTOR     TO W70-DEBT.
	   IF WS-STOCK = "Y"
012710	       MOVE 3		 TO WS-SHARED
012720	       PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
	       PERFORM AY37 THRU AY59
	       MOVE SHR-RECORD	 TO NET-RECORD
	   ELSE
012710	       MOVE 3		 TO WS-NETKEY
012720	       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
012730	       PERFORM AY39 THRU AY59.
012740       GO TO AY999.
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
012760		   IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
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
      *            NETWORK record 1.
      *
	   IF WS-STOCK = "Y"
	       MOVE NET-RECORD	 TO SHR-RECORD
012980	       PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
	   ELSE
012980	       PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
012990       MOVE 1              TO WS-NETKEY.
013000       MOVE W70-DEBT       TO NET-DEBTOR.
013010	     PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
013020       PERFORM AY49 THRU AY59.
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
	   IF LS0-POS NOT = 2
012430         CLOSE RECOVER
012440         OPEN I-O RECOVER.
013090       COMMIT.

013100 AY999.
013110       EXIT.
      /
013100 AZ000-END               SECTION.
013110 AZ000-EOJ.
013140       MOVE WS-10CPI       TO REP-DETAIL2.
013150       WRITE REP-LINE2 BEFORE 0 LINES.
013160	     CLOSE AUDIT
		   INVOICE
		   PRNREP
		   RECOVER
		   SALE.
013250 AZ100.
013260       EXIT PROGRAM.
      *
      *    ****    Read debtor record - using account number.
      *
013720 CA155-GET-DEBTOR  SECTION 2.
013730 CA155.
012010       MOVE W80-ACNO       TO DEB-ACNO.
012020       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1            TO WS-ERROR
014380     ELSE
013870     IF DEB-ACNO NOT = W80-ACNO
013880         MOVE 2            TO WS-ERROR
013890     ELSE
013900         MOVE 0            TO WS-ERROR
013910         MOVE DEB-TYPE     TO W80-TYPE.
013890 CA160-EXIT.
013900       EXIT.
      *
013910 CA165-NAME        SECTION 2.
013920 CA165.
013930       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
013940                           INTO W40-SURNAME 
013950                                W40-FNAME 
013960                                W40-SNAME
013970       OVERFLOW
013980           MOVE 1          TO WS-ERROR.
013990 CA170-EXIT.
014000       EXIT.
      *
014010 CA175-ADDRESS     SECTION 2.
014020 CA175.
014030       UNSTRING W60-ADDRESS DELIMITED BY "*"
014040                           INTO W40-ADD1 
014050                                W40-ADD2 
014060                                W40-ADD3 
014070                                W40-ADD4
014080       OVERFLOW
014090           MOVE 1          TO WS-ERROR.
014100 CA175-EXIT.
014110       EXIT.
      *
014120 CA185-ADDRESS     SECTION 2.
014130 CA185.
014140       UNSTRING W60-ADDRESS1 DELIMITED BY "*"
014150                           INTO W40-PADD1 
014160                                W40-PADD2 
014170                                W40-PADD3 
014180                                W40-PADD4
014190       OVERFLOW
014200           MOVE 1          TO WS-ERROR.
014210 CA185-EXIT.
014220       EXIT.
      *
      *    ****   G E T   S O R D E R   R E C O R D   U S I N G   JOB-KEY.
      *
      *           WS-ERROR:
      *
      *                    1 = NO RECORD
      *                    2 = NOT JOB RECORD (INVOICE)
      *                    3 = JOB DETAILS INCOMPLETE
      *                    4 = INVALID ACCOUNT NUMBER
      *
014550 GET-JOBCST           SECTION.

014570 GET-JCST-JOBNO.
             MOVE SPACES         TO W40-NM1 W40-AD1 W40-AD2
                                    W40-AD3 W40-AD4 W40-PC.
014580	     MOVE W10-JCST	 TO JHD-REF.
014600       MOVE ZERO           TO WS-ERROR.
014610	     PERFORM READ-JOBHDR THRU READ-JOBHDR-EXIT.
014620	   IF WS-F-ERROR = 14
014630         MOVE 1            TO WS-ERROR
014640         GO TO GET-JOBCST-EXIT.
014650	   IF JHD-DOC NOT = "J"
014660         MOVE 2            TO WS-ERROR
014670         GO TO GET-JOBCST-EXIT.
014680       MOVE 1              TO WS-LAST WS-SALKEY.
014790	     MOVE JHD-AC	 TO W80-ACNO DEB-ACNO.
014800	     PERFORM CA155-GET-DEBTOR.
014810	   IF WS-ERROR NOT = ZERO
014820	       MOVE 4		 TO WS-ERROR
014830	       GO TO GET-JOBCST-EXIT.
	     MOVE JHD-DTE	 TO W40-DTE.
	     MOVE JHD-ITEM	 TO W10-IDESC.
014860	     MOVE JHD-NAME	 TO W40-NAME.
	     MOVE JHD-AD1	 TO W40-AD1.
	     MOVE JHD-AD2	 TO W40-AD2.
	     MOVE JHD-AD3	 TO W40-AD3.
	     MOVE JHD-AD4	 TO W40-AD4.
	     MOVE JHD-PC	 TO W40-PC.
	     MOVE JHD-TEL	 TO W40-TEL.
	     MOVE JHD-HTEL	 TO W40-HTEL.
	     MOVE JHD-CONTACT	 TO W40-CONTACT.
	     MOVE JHD-ID	 TO W40-ID.
014870	     MOVE JHD-ORD	 TO W90-ORD.
014850	     MOVE JHD-OREF	 TO W90-REG.
014890	     MOVE JHD-EXP	 TO W90-EXP.
014910	     MOVE JHD-PRC	 TO WS-PRC.
014880	     MOVE JHD-SMAN	 TO W10-SMAN.
	     MOVE JHD-STATUS	 TO W90-STATUS.
	     MOVE JHD-COMMENT	 TO W10-COMMENT.
	     MOVE JHD-START	 TO W90-START.
015230	     MOVE JHD-SUBTOT	 TO W90-DB.
015240	     MULTIPLY -1 BY JHD-DSCNT
015250				 GIVING W90-TDISC.
015260	     MOVE W90-TDISC	 TO W90-LDISC.
015270	     MOVE JHD-NON-TXBL	 TO W90-NONTAX.
015280	     MOVE JHD-TAXABLE	 TO W90-EXCTOT.
015290	     MOVE JHD-TAX	 TO W90-TTAX.
015300	     MOVE JHD-TOT	 TO W90-TOTAL.
015310	     MOVE JHD-BAL	 TO W90-BAL.
	     MOVE JHD-INSTRUCT	 TO W90-INSTRUCT.
	     MOVE JHD-PMETH	 TO W90-PMETH.
      *
014580	     MOVE W10-JCST	 TO JOB-REF.
022140       MOVE 100            TO JOB-SQN.
014610	     PERFORM START-AT-JOB-NO THRU READ-JOBCST-EXIT.
014680	     MOVE 1		 TO WS-LAST WS-SALKEY.
014620     IF WS-F-ERROR = 13
014640         GO TO GET-JOBCST-EXIT.
014690*      GO TO GET-JCST-TEST-TYPE.

014710 GET-JCST-NEXT.
014720       PERFORM READ-JOBCST-NEXT THRU READ-JOBCST-EXIT.
014730     IF WS-F-ERROR = 13
014750         GO TO GET-JOBCST-EXIT.
	   IF JOB-REF NOT = W10-JCST
	       MOVE W10-JCST   TO JOB-REF
	       GO TO GET-JOBCST-EXIT.

014770 GET-JCST-TEST-TYPE.
      * 		     I T E M   D E T A I L S
014930     IF JOB-TYPE = 1
014940             INITIALIZE SAL-RECORD1
		   MOVE "S"	 TO SAL-TYPE
014950		   MOVE JOB-EXT-ITEM
				 TO SAL-EXT-ITEM
014960		   MOVE JOB-DESC TO SAL-DESC
014970             MOVE JOB-QNT  TO SAL-QNT
014980             MOVE JOB-COST TO SAL-COST
014990             MOVE JOB-SELL TO SAL-SELL
015000             MOVE JOB-VAT  TO SAL-VAT
015000		   MOVE JOB-VAT-SUB
				 TO SAL-VAT-SUB
015010		   MOVE JOB-VAL  TO SAL-VAL
015020             MOVE JOB-INCTAX
                                 TO SAL-INCTAX
015030             MOVE JOB-NONTAX
                                 TO SAL-NONTAX
015040             MOVE JOB-TAXVAL   
                                 TO SAL-TAXVAL
015050             MOVE JOB-DSC  TO SAL-DSC
015060             MOVE JOB-DISC TO SAL-DISC
015070             MOVE JOB-SQN  TO SAL-SQN
                   MOVE JOB-TECH TO SAL-TECH
		   MOVE JOB-SERNO
				 TO SAL-SERNO
		   IF SAL-VAT-SUB > ZERO
		       ADD JOB-TAXVAL
				 TO W90-TAXVAL(SAL-VAT-SUB)
023060		       MOVE W05-VAT(SAL-VAT-SUB)
023060				 TO W05-RATE
023080		       COMPUTE W90-VAT(SAL-VAT-SUB) ROUNDED =
			      (W90-TAXVAL(SAL-VAT-SUB) * W05-RTE)
		   END-IF
015080		   PERFORM GET-JCST-WRITE-SALE
015090     ELSE
      *                      P A R T S   D E T A I L S
015100     IF JOB-TYPE = 2
015110         INITIALIZE SAL-RECORD1
	       MOVE "T" 	 TO SAL-TYPE
014950	       MOVE JOB-EXT-ITEM TO SAL-EXT-ITEM
014960	       MOVE JOB-DESC	 TO SAL-DESC
014970         MOVE JOB-QNT      TO SAL-QNT
015140         MOVE JOB-SQN      TO SAL-SQN
015150	       PERFORM GET-JCST-WRITE-SALE
015160     ELSE
      *                      R E M A R K S
015170     IF JOB-TYPE = 3
015180         INITIALIZE SAL-RECORD1
	       MOVE "R" 	 TO SAL-TYPE
015190         MOVE JOB-SERIAL   TO SAL-DESC
015200         MOVE JOB-SQN      TO SAL-SQN
015210         PERFORM GET-JCST-WRITE-SALE
           ELSE
      *                      I N T E R N A L   N O T E S
           IF JOB-TYPE = 4 
	       GO TO GET-JCST-NEXT.
015340	     GO TO GET-JCST-NEXT.

015360 GET-JCST-WRITE-SALE.
015370       MOVE WS-LAST        TO SAL-CODE.
015380       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
015390       ADD 1               TO WS-LAST WS-SALKEY.
015400
015410 GET-JOBCST-EXIT.
015420       EXIT.

       COPY LEDTRF.UPD.

      /
      *    ****    G E N E R A T E   A   D E B T O R
      *            T R A N S A C T I O N
      *
014810 GENERATE-DEBTOR-TRAN SECTION 2.
014820
014830 DEBTOR-TRAN.
014910
014920 LINK-DEBTOR-TRANS.
015010       INITIALIZE TRA-RECORD1.
015060       MOVE W90-DISC       TO TRA-DISC.
015090       MOVE W20-DTE        TO TRA-DATE.
015100       MOVE W20-DTE        TO DEB-ACTIVE.
015110       MOVE W90-CODE       TO TRA-CODE.
015120       MOVE W90-REF        TO TRA-REF.
             MOVE W90-REG        TO TRA-REG.
             MOVE W90-ORD        TO TRA-ORD.
015130       MOVE DEB-ACNO       TO TRA-AC.
015150	     MOVE W90-DB	 TO TRA-VALUE.
015180	   IF W00-ACTION = 1
015190         MULTIPLY -1 BY W90-NONTAX
015200                           GIVING TRA-TAXFREE
015210     ELSE
015220         MOVE W90-NONTAX   TO TRA-TAXFREE.
015230       MOVE W90-TTAX       TO TRA-TAX.
015240       ADD TRA-TAX         TO W00-T-VAT.
015290       PERFORM WRITE-DBTRAN THRU WRITE-DBTRAN-EXIT.
000900       MOVE 1              TO WS-ACTION.
000220       PERFORM AY04 THRU AY59.
015300       MOVE 1              TO TAX-ACTYPE.
015310       MOVE TRA-CODE       TO TAX-CODE.
015320       MOVE TRA-DATE       TO TAX-DATE.
015330       MOVE TRA-REF        TO TAX-REF.
015340       MOVE TRA-VALUE      TO TAX-VALUE.
015350       MOVE TRA-TAXFREE    TO TAX-TAXFREE.
015360*      MOVE TRA-TAX	 TO TAX-VAT.
015370       MOVE TRA-AC         TO TAX-AC.
015410	     MOVE "2"		 TO TAX-TYPE.
015420       MOVE ZERO           TO TAX-SEQ.
	     MOVE 1		 TO WS-S9.

       DEBTOR-TAX.
	     MOVE W90-TAXVAL(WS-S9)
				 TO TAX-VAL(WS-S9).
	     MOVE W90-VAT(WS-S9) TO TAX-VAT(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO DEBTOR-TAX.
000890	     PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
000900	     MOVE 1		 TO WS-ACTION.
000910	     PERFORM AY19 THRU AY59.
015460	     ADD TRA-VALUE	 TO W00-T-VAL W00-T-MTDV W00-T-YTDV.
015470       ADD 1               TO W00-T-DAY W00-T-MTD  W00-T-YTD.
015480	     PERFORM UPDATE-TRAN-RECORD.
	   IF WX-INT = "Y"
               PERFORM DEBTOR-INTEGRATE. 
015510	     ADD W90-DB 	 TO DEB-DEBT
015520				    DEB-BAL
015530				    DEB-PURYTD
                                    DEB-MTHPUR (W20-MONTH).
015540       GO TO DEBTOR-TRAN-EXIT.

       DEBTOR-INTEGRATE.
             MOVE W20-DTE        TO WX-DATE.
	     MOVE "  DT"	 TO WX-LEDG.
	     MOVE W90-CODE	 TO WX-TRN.
	     MOVE W00-E-DESC	 TO WX-DESC.
             MOVE "- DEBTORS"    TO WX-BOOK.
             MOVE TRA-VALUE      TO WX-VALUE.
             MOVE WX-DEBGL       TO WX-AC.
             MOVE 01             TO WX-TYPE.
             PERFORM GL-TRF-UPDATE.
015550

016030 DEBTOR-TRAN-EXIT.
016040       EXIT.
      /
016060 BA000			SECTION 50.
016070 BA00.
             PERFORM BC000.
             GO TO AZ000-EOJ.
016840 BA80.
016850       MOVE W70-AUDIT      TO WS-AUDKEY.
016860       MOVE LOW-VALUES     TO AUD-REC1.
016870       PERFORM AY16 THRU AY59.
016880	     MOVE STK-CODE	 TO AUD-EXT-CODE.
016890       MOVE 2 TO AUD-TYPE.
016900       MOVE ZERO           TO AUD-SUB.
016910       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
016920       ADD 1               TO W70-AUDIT.
016930       MOVE 1              TO WS-AUDKEY.
016940       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
016950       PERFORM AY16 THRU AY59.
016960       MOVE 1              TO AUD-REP2.
016970       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
016980 BA90.
016990       MOVE W70-AUDIT      TO WS-AUDKEY.
017000       MOVE LOW-VALUES     TO AUD-REC1.
017010       PERFORM AY16 THRU AY59.
017050       MOVE 7              TO AUD-TYPE.
017060	     MOVE LS-DRAW	 TO AUD-SUB.
017070       MOVE W90-REF        TO AUD-CSH.
017080       MOVE W90-EXCTOT     TO AUD-SELL.
017090       MOVE W90-TCOST      TO AUD-COST.
017100       MOVE W10-SMAN       TO AUD-SMAN.
017110       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
017120       ADD 1               TO W70-AUDIT.
017130       MOVE 1              TO WS-AUDKEY.
017140       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
017150       PERFORM AY16 THRU AY59.
017190       MOVE 1              TO AUD-REP7.
017200       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
017210 BA95.
017220       MOVE W70-AUDIT      TO WS-AUDKEY.
017230       MOVE LOW-VALUES     TO AUD-REC1.
017240       PERFORM AY16 THRU AY59.
017290       MOVE 3              TO AUD-TYPE.
017300       MOVE DEB-ACNO       TO AUD-AC.
017310	     MOVE LS-DRAW	 TO AUD-SUB.
017320       MOVE W90-REF        TO AUD-REF.
017330       MOVE W90-TTAX       TO AUD-TAX.
017340       MOVE W90-TOTAL      TO AUD-VAL.
017350       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
017360       ADD 1               TO W70-AUDIT.
017370       MOVE 1              TO WS-AUDKEY.
017380       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
017390       PERFORM AY16 THRU AY59.
017430       MOVE 1              TO AUD-REP3.
017440       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
017450 BA100.
017460       MOVE W70-AUDIT      TO WS-AUDKEY.
017470       MOVE LOW-VALUES     TO AUD-REC1.
017480       PERFORM AY16 THRU AY59.
017520       MOVE 8              TO AUD-TYPE.
017530	     MOVE LS-DRAW	 TO AUD-SUB.
017540       MOVE W10-SMAN       TO AUD-SMAN.
017550       MOVE W90-REF        TO AUD-CSH.
017560       MOVE W90-VALUE      TO AUD-SEL.
017570       MOVE W90-ICST       TO AUD-CST.
017580	     MOVE STK-CODE	 TO AUD-EXT-STOCK.
             MOVE WS-SUPER       TO AUD-SUPER.
017590       PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
017600       ADD 1               TO W70-AUDIT.
017610       MOVE 1              TO WS-AUDKEY.
017620       PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
017630       PERFORM AY16 THRU AY59.
017670       MOVE 1              TO AUD-REP8.
017680       PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
      /
       BB000		   SECTION 50.
       BB00.
           IF DEB-NCASH = "CASH SALE" OR "KONTANT V"
               GO TO BB06.
       BB05.
	     MOVE "Enter client details Y/N  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "N" OR "Y")
               GO TO BB05.
           IF WS-OPTION = "N"
               GO TO BB20.
       BB06.
	     DISPLAY CLEAR-L25.
	     PERFORM SAVE-SCREEN.
	     MOVE 0815		 TO CPOS.
       BB10.
             DISPLAY 
		 "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°"
                 AT CPOS.
           IF CLIN < 18
               ADD 1             TO CLIN
               GO TO BB10.
             DISPLAY S30. 
       BB15.
             ACCEPT S31.
           IF DEB-NCASH = "CASH SALE" OR "KONTANT V"
               IF (W40-NM1 = SPACES) OR
                  (W40-AD1 = SPACES) OR
                  (W40-HTEL = SPACES)
		   MOVE "Require Name, Address & Telephone"
				 TO WS-ERR-MES
		   PERFORM ERROR-LENGTH THRU ERROR-EXIT
		   GO TO BB15.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
               GO TO BB15.
           IF DEB-NCASH = "CASH SALE"
               MOVE W40-NM1      TO W40-NAME
               MOVE W40-AD1      TO W40-ADD1
               MOVE W40-AD2      TO W40-ADD2
               MOVE W40-AD3      TO W40-ADD3
               MOVE W40-AD4      TO W40-ADD4
               MOVE W40-PC       TO W40-PC1.
       BB20.
	     DISPLAY CLEAR-L25.
       BB29.
             EXIT.
      /
017690 BB050		   SECTION 50.
017700 BB50.
017710	     MOVE SPACES	 TO REP-DETAIL2.
	     WRITE INV-L1 BEFORE 2 LINES.
	   IF WS-PRN-TYPE = "H"
	       MOVE WS-EXPP	 TO REP-DETAIL2
	       WRITE INV-L1 BEFORE 0 LINES
	       MOVE SPACES	 TO REP-DETAIL2
	       MOVE X"00"	 TO INV-EXP2
	   ELSE
	       MOVE X"0E"	 TO INV-EXP2.
	     MOVE "TAX INVOICE/BELASTING FAKTUUR"
				 TO INV-INVOICE.
	     WRITE INV-L1 BEFORE 0 LINES.
	     WRITE INV-L1 BEFORE 2 LINES.
017710       MOVE SPACES         TO REP-DETAIL2.
	   IF WS-PRN-TYPE = "H"
	       MOVE X"00"	 TO INV-EXP2
	   ELSE
	       MOVE X"0E"	 TO INV-EXP2.
	     MOVE "DELIVERY NOTE/AFLEWERINGSNOTA"
				 TO INV-INVOICE.
	     WRITE INV-L1 BEFORE 0 LINES.
	     WRITE INV-L1 BEFORE 3 LINES.
017710	     MOVE SPACES	 TO REP-DETAIL2.
	   IF WS-PRN-TYPE = "H"
	       MOVE X"00"	 TO INV-EXP
	   ELSE
	       MOVE X"0E"	 TO INV-EXP.
017750	     MOVE W95-COMP	 TO INV-EXPNAME.
017760	     WRITE INV-L1 BEFORE 1 LINES.
	     MOVE WS-ECAN	 TO REP-DETAIL2.
	     WRITE INV-L1 BEFORE 0 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
017770	     MOVE W95-ADD1	 TO INV-NADD.
026690	   IF W05-VATNO NOT = SPACES
026700	       MOVE "VAT No/BTW Nr :"
				 TO INV-H2
026710         MOVE W05-VATNO    TO INV-GNO.
017780       WRITE INV-L1 BEFORE 1 LINES.
017790       MOVE SPACES         TO REP-DETAIL2.
017800       MOVE W95-ADD2       TO INV-NADD.
017910	   IF W95-TEL NOT = SPACES
017920	       MOVE "TELEPHONE/FOON:"
				 TO INV-H2
017930         MOVE W95-TEL      TO INV-TEL.
017810       WRITE INV-L1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
017820       MOVE W95-ADD3       TO INV-NADD.
017830       WRITE INV-L1 BEFORE 1 LINES.
017840       MOVE W95-ADD4       TO INV-NADD.
017850	     WRITE INV-L1 BEFORE 1 LINES.
017860 BB55.
017870       MOVE SPACES         TO REP-DETAIL2.
017880       MOVE W95-PC         TO INV-PC.
017890	     WRITE INV-L1 BEFORE 1 LINES.
017900       MOVE SPACES         TO REP-DETAIL2.
017950 BB60.
017960       EXIT.
      /
       BB070		   SECTION 50.
       BB70.
             INITIALIZE GUA-RECORD1.
             MOVE ZERO           TO WS-F-ERROR.
	     MOVE STK-CODE	 TO GUA-EXT-CODE.
             MOVE W10-SERNO      TO GUA-SERIAL.
           IF WS-GAR = 1
               GO TO BB80.
             MOVE STK-DESC       TO GUA-DESC.
             MOVE W40-NAME       TO GUA-NAME.
             MOVE W12-T-YMD      TO GUA-DATE1 W20-DTE.
             MOVE STK-GUAR       TO GUA-PERIOD.
             ADD GUA-PERIOD      TO W20-MONTH.
       BB75.
           IF W20-MONTH > 12
               SUBTRACT 12       FROM W20-MONTH
               IF W20-YEAR = 99
                   MOVE ZERO     TO W20-YEAR
		   ADD 1	 TO W20-CENT
                   GO TO BB75
               ELSE
                   ADD 1         TO W20-YEAR
                   GO TO BB75.
             MOVE W20-DTE        TO GUA-DATE2.
             MOVE W90-REF        TO GUA-INV.
             PERFORM WRITE-GARTEE THRU WRITE-GARTEE-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY09 THRU AY59.
             GO TO BB99.
       BB80.
             PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR = 9
               GO TO BB99.
             MOVE 2              TO WS-ACTION.
             PERFORM AY09 THRU AY59.
             PERFORM DELETE-GARTEE-REC THRU WRITE-GARTEE-EXIT.
       BB99.
             EXIT.
      /
      *    ****    P R O D U C E   I N V O I C E
      *
018500 BC000	     SECTION 50.
       BC00.
      *
      *    ****    I N V O I C E   S C R E E N
      *
018580       DISPLAY CLR-SCREEN.
018590       DISPLAY S26.
018620       MOVE 8              TO LIN.
018630       PERFORM BC115 UNTIL LIN > 18.
018640       MOVE W12-TODAY      TO W90-DTE.
018650	     DISPLAY W90-DTE AT 0570 WITH FOREGROUND-COLOR 11.
018660       MOVE ZERO           TO W90-BDT W90-NONTAX W90-LDISC 
018670				    W90-BAL W90-SALES
				    W90-CUR W90-EXCTOT.
	     MOVE LS-SLS-ORDNO	 TO W10-JCST.
             MOVE "N"            TO WS-CSHSLE.
	     MOVE 1		 TO WS-S9.

       BC05A.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BC05A.
	     PERFORM GET-JOBCST.
	   IF W40-AD1 = LOW-VALUES
               MOVE SPACES       TO W40-AD1
           ELSE
           IF W40-AD2 = LOW-VALUES
               MOVE SPACES       TO W40-AD2
           ELSE
           IF W40-AD3 = LOW-VALUES
               MOVE SPACES       TO W40-AD3
           ELSE
           IF W40-AD4 = LOW-VALUES
               MOVE SPACES       TO W40-AD4.
	   IF WS-ERROR NOT = ZERO
	       MOVE "Database corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC599.
             MOVE W10-JCST       TO W90-REG.
           IF W90-EXP = "E" OR "X"
               MOVE W05-VATNO    TO W90-TAX
           ELSE
               MOVE SPACES       TO W90-TAX.
018680       MOVE 1              TO WS-FIRST.
018820       DISPLAY S36 AT 0308
019040       DISPLAY W40-NAME AT 0320 WITH FOREGROUND-COLOR 11.
019900       DISPLAY W90-ORD AT 0368 WITH FOREGROUND-COLOR 11.
019970       DISPLAY W10-SMAN AT 0411 WITH FOREGROUND-COLOR 11.
020010     IF W10-SMAN NOT = ZERO
020030	       ADD 200 W10-SMAN      GIVING WS-PARKEY
020040         PERFORM READ-PARAM THRU READ-PARAM-EXIT
020100	       DISPLAY PAR-TECHNICIAN AT 0420 WITH FOREGROUND-COLOR 11
	       MOVE PAR-TECHNICIAN   TO W40-ASSIST
           ELSE
               MOVE SPACES       TO W40-ASSIST.
	     DISPLAY W40-TEL AT 0466 WITH FOREGROUND-COLOR 11.
             DISPLAY W40-CONTACT AT 0528 WITH FOREGROUND-COLOR 11.
	     MOVE 1		 TO WS-SALKEY.
             SUBTRACT 1          FROM WS-LAST.
       BC15.
022610       MOVE W90-DB         TO W90-TOTAL.
022620       SUBTRACT W90-LDISC  FROM W90-TOTAL.
022630       MOVE W90-DB         TO W100-BAL.
022640       DISPLAY W100-BAL AT 2015 WITH FOREGROUND-COLOR 11.
022650       MULTIPLY -1 BY W90-LDISC 
022660                           GIVING W100-BAL.
022670       DISPLAY W100-BAL AT 2115 WITH FOREGROUND-COLOR 11.
      *
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *
023040	   IF (W90-TAX = SPACES) OR
	      (W90-EXP = "C" OR "X")
022700	       GO TO BC20.
	     MOVE 1		 TO WS-S1.
	     MOVE ZERO		 TO W25-TOTAL.

       BC16.
023070	   IF W90-TAXVAL(WS-S1) NOT = ZERO
023060	       MOVE W05-VAT(WS-S1)
023060				 TO W05-RATE
023080	       COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
		      (W90-TAXVAL(WS-S1) * W05-RTE).
	   IF WS-S1 < 6
	       ADD 1		 TO WS-S1
	       GO TO BC16.
022740	     MOVE W25-TOTAL	 TO W100-BAL
022750	     DISPLAY W100-BAL AT 2215 WITH FOREGROUND-COLOR 11
022760	     ADD W25-TOTAL	 TO W90-TOTAL.

022770 BC20.
022780	     MOVE W90-TOTAL	 TO W100-BAL.
022790       DISPLAY W100-BAL AT 2315 WITH FOREGROUND-COLOR 11.
             GO TO BC185.
023880 BC115.
036820	     DISPLAY (LIN, 1) WS-MIV1 WITH BACKGROUND-COLOR 0
					   FOREGROUND-COLOR 3
			      WS-MIV78 WITH FOREGROUND-COLOR 3
			      WS-MIV80 WITH BACKGROUND-COLOR 0
					    FOREGROUND-COLOR 3.
023900	     ADD 1		 TO LIN.
025130 BC185.
      *
      *    ****    M U S T   T H E   V A L U E   B E   A G E D  
      *
025140	     DISPLAY CLEAR-L25.
025150	     MOVE "M"		 TO WS-AGE.
025160*    IF (W90-ORG = "C") OR (DEB-TERMS = "3")
025170*        GO TO BC188.
025180*     DISPLAY "AGE - " AT 2502
      *	      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
025190*	     "M" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
025200*	     "td, " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
025210*	     "C" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
025220*	     "ur, " WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
025230*	     "3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
025240*	     "0 days, "
      *	      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
025250*	     "6" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
025260*	     "0 days, "
      *	      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
025270*	     "9" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
025280*	     "0 days or "
      *	      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
025290*	     "1" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
025300*	     "20 days > <"
      *	      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
025310*BC186.
025320*      ACCEPT WS-AGE AT 2557
      *	    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
      *		 UPDATE AUTO.
      *     CALL "CBL_TOUPPER" USING WS-AGE
      *			BY VALUE WS-LENGTH
      *			RETURNING WS-STATUS.
025380*   IF NOT (WS-AGE = "C" OR "M" OR "1" OR "3" OR "6" OR "9")
025390*        GO TO BC186.
025400*     DISPLAY CLEAR-L25.
      *
      *    ****    P R O C E S S   T H E   S A L E
      *
025410 BC188.
025420     IF DEB-TERMS NOT = "3"
025430         DISPLAY "Processing Invoice" AT 2520 WITH
025440			BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK
025450     ELSE
025460         DISPLAY "Processing COD sale" AT 2520 WITH
025470			BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
025480       MOVE 1              TO WS-SALKEY.
025490       PERFORM AY60 THRU AY999.
025500       PERFORM AY02 THRU AY59.
025510       MOVE W70-INVOICE    TO W90-REF.
025520       DISPLAY W90-REF AT 0510 WITH FOREGROUND-COLOR 11.
025530     IF W70-INVOICE = 99999999
025540         MOVE 1            TO W70-INVOICE
025550     ELSE
025560         ADD 1             TO W70-INVOICE.
      *
      *    ****    T E S T   F O R   I N D I V I D U A L
      *
025570     IF W80-TYPE NOT = 1
025580         GO TO BC210.
025590       MOVE DEB-NAME       TO W60-NAME.
025600       PERFORM CA165-NAME.
      *
      *    ****    T I T L E
      *
025610     IF DEB-LANG = "A"
025620         MOVE W75-ATITLE (DEB-TITLE) 
025630                           TO W60-NAME
025640     ELSE
025650         MOVE W75-ETITLE (DEB-TITLE) 
025660                           TO W60-NAME.
025670       MOVE 7              TO WS-S6.
025680 BC190.
025690     IF W60-NCHAR (WS-S6) NOT = SPACE
025700         GO TO BC195.
025710       SUBTRACT 1          FROM WS-S6.
025720     IF WS-S6 > 0
025730         GO TO BC190.
025740 BC195.
      *
      *    ****    F I R S T   I N I T I A L
      *
025750       MOVE 1              TO WS-S5.
025760       ADD 2               TO WS-S6.
025770       MOVE W40-NCHAR (25) TO W60-NCHAR (WS-S6).
025780       ADD 1               TO WS-S6.
025790       MOVE "."            TO W60-NCHAR (WS-S6).
025800     IF W40-NCHAR (49) = SPACE
025810         GO TO BC200.
      *
      *    ****    S E C O N D   I N I T I A L
      *
025820       ADD 1               TO WS-S6.
025830       MOVE W40-NCHAR (49) TO W60-NCHAR (WS-S6).
025840       ADD 1               TO WS-S6.
025850       MOVE "."            TO W60-NCHAR (WS-S6).
025860 BC200.
025870       ADD 2               TO WS-S6.
025880 BC205.
      *
      *    ****    S U R N A M E
      *
025890       MOVE W40-NCHAR (WS-S5) 
025900                           TO W60-NCHAR (WS-S6).
025910     IF WS-S5 < 24
025920         ADD 1             TO WS-S5
025930         ADD 1             TO WS-S6
025940         GO TO BC205.
025950       MOVE W60-NAME       TO W40-NAME.
025960 BC210.
025970       MOVE ZERO           TO WS-PAGE.
025980       MOVE SPACES         TO W40-PC1 W40-PC2.
025990	     MOVE DEB-PADDR	 TO W60-ADDRESS.
026000       PERFORM CA175-ADDRESS.
026010     IF NOT (DEB-POST = ZERO OR 9999)
026020         MOVE DEB-POST     TO W40-PC1.
026030     IF W40-ADD4 = SPACES
026040         MOVE W40-PC1      TO W40-ADD4
026050         MOVE SPACES       TO W40-PC1.
026060     IF W40-ADD3 = SPACES
026070         MOVE W40-ADD4     TO W40-ADD3
026080         MOVE SPACES       TO W40-ADD4.
026090     IF W40-ADD2 = SPACES
026100         MOVE W40-ADD3     TO W40-ADD2
026110         MOVE SPACES       TO W40-ADD3.
026120     IF W40-ADD1 = SPACES
026130         MOVE W40-ADD2     TO W40-ADD1
026140         MOVE SPACES       TO W40-ADD2.
026150	     MOVE DEB-ADDRESS	 TO W60-ADDRESS1.
022800       INSPECT W60-ADDRESS1 REPLACING ALL "*" BY " ".
026170	   IF NOT (DEB-PPOST = ZERO OR 9999)
026180	       MOVE DEB-PPOST	 TO W40-PC2.
	     MOVE 60		 TO WS-S1.
       BC211.
           IF W60-CHR (WS-S1) = SPACE
               IF WS-S1 > 1
                   SUBTRACT 1    FROM WS-S1
                   GO TO BC211.
             ADD 2               TO WS-S1.
             MOVE 1              TO WS-S2.
       BC212.
             MOVE W40-PCHAR (WS-S2)
                                 TO W60-CHR (WS-S1).
           IF WS-S2 < 4
               ADD 1             TO WS-S1 WS-S2
               GO TO BC212.
             PERFORM BB000.
038660	     DISPLAY "Processing Invoice" AT 2520 WITH
038670		      BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
026310 BC215.
027940	     PERFORM BB050.
      *
      *    ****    P R I N T   I N V O I C E   H E A D I N G S
      *
026320	     MOVE SPACES	 TO REP-DETAIL2.
	   IF WS-PRN-TYPE = "H"
	       MOVE WS-EXPP	 TO REP-DETAIL2
	       WRITE INV-L1 BEFORE 0 LINES
	       MOVE SPACES	 TO REP-DETAIL2
	       MOVE X"00"	 TO INV-EXP
	   ELSE
	       MOVE X"0E"	 TO INV-EXP.
	     MOVE "REF No:"	 TO INV-H14.
026590       MOVE W90-REF        TO INV-REF.
026370	     WRITE INV-L1 BEFORE 0 LINES.
026370	     WRITE INV-L1 BEFORE 2 LINES.
026320       MOVE SPACES         TO REP-DETAIL2.
	   IF WS-PRN-TYPE = "H"
	       MOVE X"00"	 TO INV-EXP
	   ELSE
	       MOVE X"0E"	 TO INV-EXP.
	     MOVE "DATE  :"	 TO INV-H14.
026670       MOVE W12-TODAY      TO INV-DTE.
026370	     WRITE INV-L1 BEFORE 0 LINES.
026370       WRITE INV-L1 BEFORE 1 LINES.
	     MOVE WS-ECAN	 TO REP-DETAIL2.
	     WRITE INV-L1 BEFORE 0 LINES.
026320	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE "DELIVERY / AFLEWRING"
				 TO INV-DADD2.
026370       WRITE INV-L1 BEFORE 1 LINES.
026320	     MOVE SPACES	 TO REP-DETAIL2.
026330       MOVE W40-NAME       TO INV-NAME.
	     MOVE "PAGE / BLADSY"
				 TO INV-H4.
	     ADD 1		 TO WS-PAGE.
	     MOVE WS-PAGE	 TO INV-PGE.
026370       WRITE INV-L1 BEFORE 1 LINES.
026320	     MOVE SPACES	 TO REP-DETAIL2.
026350       MOVE W40-ADD1       TO INV-NAME.
026360	     MOVE W40-PADD1	 TO INV-DADD2.
026370	     WRITE INV-L1 BEFORE 1 LINES.
026380       MOVE W40-ADD2       TO INV-NAME.
026390	     MOVE W40-PADD2	 TO INV-DADD2.
	     MOVE "ACCOUNT / REKENING"
				 TO INV-H4.
026550	     MOVE W80-ACNO	 TO INV-AC.
026370	     WRITE INV-L1 BEFORE 1 LINES.
026320       MOVE SPACES         TO REP-DETAIL2.
026410       MOVE W40-ADD3       TO INV-NAME.
026420	     MOVE W40-PADD3	 TO INV-DADD2.
026370       WRITE INV-L1 BEFORE 1 LINES.
026320       MOVE SPACES         TO REP-DETAIL2.
026440       MOVE W40-ADD4       TO INV-NAME.
026450	     MOVE W40-PADD4	 TO INV-DADD2.
	     MOVE "ORDER / BESTELLING"
				 TO INV-H4.
026560       MOVE W90-ORD        TO INV-ORD.
026340       WRITE INV-L1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
026510       MOVE W40-PC1        TO INV-PC1.
026510	     MOVE W40-PC2	 TO INV-PC2.
026370       WRITE INV-L1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
      *
      *    ****    P R I N T   T E R M S   O N   T H E   I N V O I C E
      *
	     MOVE "TERMS / TERME"
				 TO INV-H4.
028720	   IF DEB-TERMS = "0"
028730	       MOVE "7 Days/Dae" TO INV-TERMS
028740	   ELSE
028750	   IF DEB-TERMS = "2"
028760	       MOVE "60 Days/Dae"
				 TO INV-TERMS
028770	   ELSE
028780	   IF DEB-TERMS = "1"
028790	       MOVE "30 Days/Dae"
				 TO INV-TERMS
028800	   ELSE
028810	       MOVE "COD/KBA"	 TO INV-TERMS.
026430       WRITE INV-L1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
	   IF W10-SMAN NOT = ZERO
	       MOVE "Assist :"	 TO INV-H3
	       MOVE W40-ASSIST	 TO INV-DADD2.
026460	     WRITE INV-L1 BEFORE 2 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE "  QUANTITY"	 TO INV-H7.
	     MOVE "DISCOUNT"	 TO INV-H9.
	     MOVE "EXCLUSIVE"	 TO INV-H10.
	     MOVE "NETT AMOUNT"  TO INV-H12.
026430	     WRITE INV-L1 BEFORE 0 LINES.
026430	     WRITE INV-L1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE "ITEM CODE/KODE"
				 TO INV-H5.
	     MOVE "DESCRIPTION/BESRYWING"
				 TO INV-H6.
	     MOVE "HOEWEELHEID"  TO INV-H7.
	     MOVE "PRICE/PRYS"	 TO INV-H8.
	     MOVE " AFSLAG"	 TO INV-H9.
	     MOVE "EKSKLUSIEF"	 TO INV-H10.
	     MOVE "VAT/BTW"	 TO INV-H11.
	     MOVE "NETO BEDRAG"  TO INV-H12.
026430	     WRITE INV-L1 BEFORE 0 LINES.
026430	     WRITE INV-L1 BEFORE 2 LINES.
026470 BC216.
      *
      *    ****    S T A R T   I N V O I C E   L O G G I N G   ( RE-PRINT )
      *
026500	     MOVE SPACES	 TO REP-DETAIL2.
026810	     MOVE 31		 TO WS-LINE.
       BC220.
             EXIT.
       BC222.
      *
      *    ****    S T A R T   I N V O I C E   L O G G I N G   ( RE-PRINT )
      *
026490	     MOVE WS-PAGE	 TO DOC-PAGE.
026550	     MOVE W80-ACNO	 TO DOC-AC.
             MOVE 100            TO DOC-SQN.
	     MOVE ZERO		 TO DOC-TYPE.
             MOVE W40-NAME       TO DOC-NAME.
             MOVE ZERO           TO DOC-PICNO.
026640       MOVE WS-CR          TO DOC-DOC.
             MOVE W90-EXP        TO DOC-EXP.
             MOVE WS-PRC         TO DOC-PRC.
026560       MOVE W90-ORD        TO DOC-ORD.
026590       MOVE W90-REF        TO DOC-REF.
026600       MOVE W90-REG        TO DOC-OREF.
026680	     MOVE W12-T-YMD	 TO DOC-DTE.
026800       MOVE W10-SMAN       TO DOC-SMAN.
028820       MOVE W90-TOTAL      TO DOC-VALUE.
      *
      *    ****    L O G   R E C O R D   T O   I N V O I C E   F I L E
      *
       BC225.
             PERFORM WRITE-INVOIC THRU WRITE-INVOIC-EXIT.
015400       MOVE 1              TO WS-ACTION.
             PERFORM AY22 THRU AY59.
026830 BC235.
      *
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            P R I N T   T H E   I N V O I C E
      *
026840     IF WS-SALKEY > WS-LAST
026850         GO TO BC255.
026860       PERFORM READ-SALE THRU READ-SALE-EXIT.
026870	   IF WS-LINE > 0
026880         SUBTRACT 1        FROM WS-LINE
026890     ELSE
026900         PERFORM BC250.
026910	   IF SAL-EXT-ITEM = SPACES
026920         GO TO BC247.
026930	     MOVE SAL-EXT-ITEM	 TO STK-CODE.
026940       PERFORM AY05 THRU AY59.
	   IF STK-IND = 2
	       MOVE SAL-TECH	 TO W10-SMAN
	   ELSE
	       MOVE ZERO	 TO W10-SMAN.
      *
      *    ****    N E X T   E N T R Y   O N   I N V O I C E   L O G
      *            R E C O R D
      *
	     MOVE W80-ACNO	 TO CRD-AC.
	     MOVE ZERO		 TO CRD-SEQ.
017540	     MOVE W10-SMAN	 TO CRD-SMAN.
017550	     MOVE W90-REF	 TO CRD-REF.
	     MOVE STK-SUPP	 TO CRD-SUPP.
	     MOVE W12-T-YMD	 TO CRD-DTE.
	     MOVE W40-NAME	 TO CRD-NAME.
	     MOVE SPACES	 TO CRD-WHS.
026960	     MOVE 1		 TO DOC-TYPE.
             INITIALIZE DOC-DET2.
026970	     MOVE SAL-EXT-ITEM	 TO DOC-EXT-ITEM CRD-EXT-CODE.
026980	     MOVE SAL-DESC	 TO DOC-DESC.
026990	     MOVE SAL-QNT	 TO W10-QUANT DOC-QNT  CRD-QNT.
027010	     MOVE SAL-COST	 TO W10-COST  DOC-COST CRD-COST.
027020	     MOVE SAL-VAT	 TO DOC-VAT.
027030	     MOVE SAL-SELL	 TO W10-SELL  DOC-SELL CRD-SELL.
027050	     MOVE SAL-VAL	 TO W25-VALUE DOC-VAL.
026970	     MOVE SAL-EXT-ITEM	 TO INV-EXT-ITM.
026980	     MOVE SAL-DESC	 TO INV-DESC.
026990	     MOVE SAL-QNT	 TO INV-QNT.
027020	     MOVE SAL-VAT	 TO INV-TAX.
027030	     MOVE SAL-SELL	 TO INV-PRICE.
027070	     MOVE SAL-DISC	 TO W90-CDISC W90-EXCDIS.
027080	     MOVE SAL-DSC	 TO WS-DISC INV-DSC.
027090	     SUBTRACT W90-CDISC FROM W25-VALUE
027100                           GIVING W90-VALUE.
	   IF WS-CARDEX = "Y"
	       PERFORM WRITE-CARDEX THRU WRITE-CARDEX-EXIT
015400	       MOVE 1		 TO WS-ACTION
	       PERFORM AY43 THRU AY59.
	     MOVE SAL-VAT-SUB	 TO WS-VAT-SUB.
027110	   IF W90-TAX = SPACES
027120       IF W90-EXP NOT = "X"
040350	       IF WS-VAT-SUB > ZERO
027140		   MOVE W05-VAT (WS-VAT-SUB)
027150				 TO W05-RATE
027160		   COMPUTE W90-VALUE ROUNDED = (W90-VALUE -
027170                 (((W90-VALUE / (W05-RATE + 100.00000)) * 
027180			  W05-RATE)))
027190		   COMPUTE W90-EXCDIS ROUNDED = (W90-CDISC -
027200                 (((W90-CDISC / (W05-RATE + 100.00000)) *
027210			  W05-RATE))).
	     MOVE W90-VALUE	 TO INV-EXTEND.
	     ADD W90-VALUE SAL-VAT
				 GIVING INV-VAL.
             MOVE SAL-TAXVAL     TO DOC-TAXVAL.
             MOVE SAL-NONTAX     TO DOC-NONTAX.
             MOVE SAL-SUPER      TO WS-SUPER.
	     MOVE SAL-VAT-SUB	 TO WS-VAT-SUB
				    DOC-VAT-SUB.
	   IF WS-VAT-SUB = ZERO
	       MOVE ZERO	 TO DOC-V-PER
	   ELSE
	       MOVE W05-VAT(WS-VAT-SUB)
				 TO DOC-V-PER.
027220*	     ADD W90-VALUE	 TO W90-EXCTOT.
027230	     WRITE INV-L1 BEFORE 1 LINES.
027240	     MOVE SPACES	 TO REP-DETAIL2.
027250       ADD W25-VALUE       TO W90-STOTAL.
      *
      *    ****    H A S   L I N E   D I S C O U N T   B E E N   
      *            A P P L I E D
      *
027260     IF W90-CDISC = ZERO
027270         GO TO BC240.
      *
      *    ****    P R I N T   A N D   L O G   T H E   L I N E
      *            D I S C O U N T
      *
027360	     MOVE WS-DISC	 TO DOC-DSC.
027370       MULTIPLY -1         BY W90-CDISC 
027380				 GIVING DOC-DISC.
027420 BC240.
      *
      *    ****    U P D A T E   S A L E S   A N A L Y S I S
      *
027430	     MOVE STK-LDG	 TO DPT-CODE.
027440	     PERFORM AY38 THRU AY59.
027450       ADD W90-VALUE       TO DPT-SALES-DAY
027460                              DPT-SALES-MTD
027470                              DPT-SALES-YTD.
           IF WX-INT = "Y"
               MOVE W12-T-YMD    TO WX-DATE
	       MOVE STK-LDG	 TO WX-LEDG
               MOVE "JOBS - DEBTORS"
                                 TO WX-NAR
               COMPUTE WX-VALUE = W90-VALUE * -1
               MOVE DPT-SALE-GL  TO WX-AC
               MOVE 04           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
      *
      *    ****    U P D A T E   C O S T - O F - S A L E S
      *
027480	     COMPUTE W90-BCR ROUNDED = (1.000 * W10-QUANT * W10-COST).
027490       ADD W90-BCR         TO DPT-COST-DAY
                                    DPT-COST-MTD
027500                              DPT-COST-YTD.
           IF (WX-INT = "Y") AND (W90-BCR NOT = ZERO)
               MOVE W90-BCR      TO WX-VALUE
               MOVE DPT-COST-GL  TO WX-AC
               MOVE 08           TO WX-TYPE
               PERFORM GL-TRF-UPDATE
               COMPUTE WX-VALUE = W90-BCR * -1
               MOVE DPT-PUR-GL   TO WX-AC
               MOVE 19           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
      *
      *    ****    U P D A T E   D I S C O U N T   G I V E N
      *
027510       ADD W90-EXCDIS      TO DPT-DISC-DAY
027520                              DPT-DISC-MTD
027530                              DPT-DISC-YTD
027540                              DPT-SALES-DAY
027550                              DPT-SALES-MTD
027560                              DPT-SALES-YTD.
027570       PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
027580       MOVE W12-T-YMD      TO STK-ACT.
      *
      *    ****    C H E C K   I F   S T O C K   C O N T R O L L E D    
      *
	   IF WS-PRC-IND = "Y"
027590	       IF STK-IND > 1
027600		   GO TO BC245
	       END-IF
	   ELSE
	   IF STK-IND > ZERO
027600	       GO TO BC245.
027610	     SUBTRACT W10-QUANT  FROM STK-QJOB.
027620     IF STK-DISC > ZERO
027630         GO TO BC245.
      *
      *    ****    C H E C K   R E - O R D E R   L E V E L   A N D
      *            R E P O R T   L O W - L E V E L S
      *
027640     IF STK-QUANT NOT > STK-LEVEL
027650         MOVE 1            TO STK-DISC
027660         PERFORM BA80.
027670 BC245.
027680       ADD W90-BCR         TO W90-BDT STK-MTDC STK-YTDC.
027690	     ADD W10-QUANT	 TO STK-MTD STK-YTD.
027700       ADD W90-VALUE       TO STK-MTDV W90-SALES STK-YTDV.
027710       PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ For MUSIC DEN the Salesman record must also be updated.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF WS-SYS-ID = "MDE" OR "DEZ"
029280	       IF W10-SMAN NOT = ZERO
029290		   ADD 100 W10-SMAN
				 GIVING WS-PARKEY
029300		   PERFORM AY01 THRU AY59
029310		   ADD W90-TCOST TO PAR-CT-MTD PAR-CT-YTD
029320		   ADD W90-EXCTOT
				 TO PAR-SL-MTD PAR-SL-YTD
029330		   PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT
	       END-IF
	   ELSE
      *
      *    ****    U P D A T E	 T H E	 T E C H N I T I A N   R E C O R D
      *
029280     IF W10-SMAN NOT = ZERO
029290	       ADD 200 W10-SMAN  GIVING WS-PARKEY
029300         PERFORM AY01 THRU AY59
	       ADD W90-VALUE	 TO PAR-JB-DAY PAR-JB-MTD PAR-JB-YTD
029330         PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      *
      *    ****    L O G   I N D I V I D U A L   I T E M S   T O   T H E
      *            A U D I T   F I L E
      *
027720       MOVE W90-BCR        TO W90-ICST.
027730       PERFORM BA100.
      ***************************			  \
      *CHECK FOR REVISED WARRANTY *************************>
      ***************************			  /
	     ADD 1		 TO DOC-SEQ.
             MOVE ZERO           TO DOC-SUB.
             PERFORM BC225.
      *
      *    ****    G O   A N D   G E T   T H E   N E X T   I T E M
      *
027740 BC246.
027750       ADD 1               TO WS-SALKEY.
027760       GO TO BC235.
027770 BC247.
027790       MOVE 3              TO DOC-TYPE.
             INITIALIZE DOC-DET2.
      *
      *     C H E C K   F O R   S E R I A L   N U M B E R S
      *
	   IF SAL-SUB NOT = ZERO
               MOVE 1            TO WS-S1
               PERFORM BC248 
	       MOVE "SERIAL NO:" TO INV-HD
	       MOVE SAL-DESC	 TO INV-SERNOS DOC-MSER
               ADD 1             TO DOC-SUB
	   ELSE
	       MOVE ZERO	 TO DOC-SUB
               ADD 1             TO DOC-SEQ
027800	       MOVE SAL-DESC	 TO INV-MES DOC-DESC.
027810	     WRITE INV-L1 BEFORE 1 LINES.
027820       MOVE SPACES         TO REP-DETAIL2.
             PERFORM BC225.
027830       GO TO BC246.
       BC248.
             MOVE SAL-SNO (WS-S1)
                                 TO W10-SERNO.
             MOVE ZERO           TO WS-GAR.
             PERFORM BB070.
             ADD 1               TO WS-S1.
	   IF WS-S1 < 3
               IF SAL-SNO (WS-S1) NOT = SPACES
                   GO TO BC248.
027840 BC250.
      *
      *    ****    C A R R I E D   F O R W A R D   -   N E X T   P A G E
      *
027850       WRITE INV-L1 BEFORE 1 LINES.
027860       MOVE SPACES         TO REP-DETAIL2.
027870	     MOVE "C/FWD - OORGEDRA"
027880				 TO INV-DADD2.
027890	     MOVE W90-STOTAL	 TO INV-VAL.
027900	     WRITE INV-L1 BEFORE 1 LINES.
027860	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE "Please check your invoice."
				 TO INV-REMKS.
027900	     WRITE INV-L1 BEFORE 1 LINES.
	     MOVE "Gaan asseblief u faktuur na."
				 TO INV-REMKS.
027900	     WRITE INV-L1 BEFORE 4 LINES.
027950       PERFORM BC215 THRU BC220.
027970       MOVE "B/FWD - O/B"  TO INV-DESC.
027980       MOVE W90-STOTAL     TO INV-VAL.
027990       SUBTRACT 2          FROM WS-LINE.
028000       WRITE INV-L1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
028010 BC255.
      *
      *    ****    I N V O I C E   T O T A L S
      *
028020       WRITE INV-L1 BEFORE WS-LINE LINES.
028030       MOVE SPACES         TO REP-DETAIL2.
028000	     WRITE INV-L1 BEFORE 1 LINES.
027860	     MOVE SPACES	 TO REP-DETAIL2.
028030	     MOVE ALL "-"	 TO INV-LINE.
      *
      *    ****    P R I N T   S U B - T O T A L
      *
	     MOVE "Please check your invoice."
				 TO INV-REMKS.
028040	     WRITE INV-L1 BEFORE 1 LINES.
027860	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE "TOTAL/TOTAAL" TO INV-H13.
	     MOVE "Gaan aseblief u faktuur na."
				 TO INV-REMKS.
       BC256.
	     MOVE 9		 TO DOC-TYPE.
             MOVE 999            TO DOC-SEQ.
             INITIALIZE DOC-DET3.
028070	     MOVE W90-DB	 TO DOC-SUBTOT.
	     MOVE DEB-TERMS	 TO DOC-TERMS.
      *
      *    ****    P R I N T   D I S C O U N T   A N D   U P D A T E
      *            T H E   S U N D R Y   D I S C O U N T   T O T A L S
      *
028150	   IF W90-TDISC NOT = ZERO
028180	       MULTIPLY -1 BY W90-TDISC
028190				 GIVING DOC-DSCNT
028200	       IF W90-LDISC = ZERO
028210		   MOVE "YYYY"	 TO DPT-CODE
028220		   PERFORM AY38 THRU AY59
028230             ADD W90-TDISC TO DPT-DISC-DAY
028240                              DPT-DISC-MTD
028250                              DPT-DISC-YTD
                   IF WX-INT = "Y"
                       MOVE W90-TDISC    TO WX-VALUE
                       MOVE DPT-DISC-GL  TO WX-AC
		       MOVE DPT-CODE	 TO WX-LEDG
                       MOVE 15           TO WX-TYPE
                       PERFORM GL-TRF-UPDATE
                   END-IF
028260             PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
      *
      *    ****    P R I N T   T H E   T A X   F I G U R E S   A N D 
      *            U P D A T E   T A X   T O T A L S
      *
      *
      * 	   TAX CALCULATED IN JOB001
      * 	       W90-NONTAX   =TOTAL NON TAXABLE VALUE
      * 	       W90-TTAX     =TOTAL TAX (ALL RATES)
      * 	       W90-TAXVAL(?)=TOTAL TAXABLE PER RATE
      * 	       W90-VAT(?)   =TOTAL VAT PER RATE
      * 	       W90-TOTAL    =TOTAL FOR CREDIT NOTE
      *
028660	     MOVE W05-RATE	 TO WS-PER.
028400	     MOVE 9		 TO WS-PARKEY.
028410       PERFORM AY01 THRU AY59.
028660	     MOVE W05-RATE	 TO WS-PER.
	     MOVE W90-EXCTOT	 TO INV-EXTEND.
028680	     MOVE W90-TTAX	 TO DOC-TAX.
028680	     MOVE W90-TTAX	 TO INV-TAX.
      *
      *    ****    I N V O I C E   T O T A L   (P R I N T   &   L O G)
      *
028690 BC260.
028820	     MOVE W90-TOTAL	 TO W90-DB.
028830       MOVE W90-BDT        TO W90-TCOST.
028840	     MOVE W90-DB	 TO DOC-TOT.
	     MOVE W90-EXCTOT	 TO DOC-TAXABLE.
             MOVE W90-NONTAX     TO DOC-NON-TXBL.
             MOVE W90-BAL        TO DOC-BAL.
	     MOVE W40-CONTACT	 TO DOC-CONTACT.
	     MOVE W40-TEL	 TO DOC-TEL.
	     PERFORM BC225.
	     MOVE W90-DB	 TO INV-VAL.
028880	     WRITE INV-L1 BEFORE 0 LINES.
	     MOVE SPACES	 TO INV-REMKS.
028880	     WRITE INV-L1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE ALL "="	 TO INV-LINE.
028100	   IF DEB-SDISC NOT = ZERO
028110	       MOVE W15-DISC (DEB-SDISC)
028120                           TO W30-PERC
028130	       MOVE W30-MESSAGE  TO INV-RMKS.
028880	     WRITE INV-L1 BEFORE 1 LINES.
	     MOVE SPACES	 TO REP-DETAIL2.
	     MOVE "Processed by APAC Accounting - (011) 803-2572"
				 TO REP-DETAIL2.
	     WRITE INV-L1 BEFORE 2 LINES.
028890	     MOVE SPACES	 TO REP-DETAIL2.
028900       MOVE W12-T-YMD      TO W20-DTE.
      *
      *    ****    U P D A T E   T H E   D E B T O R   A C C O U N T
      *
028910       MOVE 1              TO W90-CODE.
028920       PERFORM GET-DEBTOR-TRAN-RECORD.
028930       PERFORM GENERATE-DEBTOR-TRAN.
028400	     MOVE 9		 TO WS-PARKEY.
028410	     PERFORM AY01 THRU AY59.
	     MOVE 1		 TO W05-V-RATE.

       BC262-VAT.
	   IF W90-VAT(W05-V-RATE) = ZERO
	       GO TO BC262-CHECK-END.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
027840	     PERFORM AY38 THRU AY59.
      *
      *    ****    V A T   S T A T I S T I C S
      *
027850	     ADD W90-VAT(W05-V-RATE)
				 TO DPT-O-VAT-DAY
027860				    DPT-O-VAT-MTD
027870				    DPT-O-VAT-YTD.
	     ADD W90-TAXVAL(W05-V-RATE)
				 TO DPT-V-SALES-DAY
				    DPT-V-SALES-MTD
				    DPT-V-SALES-YTD.
	   IF (WX-INT = "Y") AND (W90-VAT(W05-V-RATE) NOT = ZERO)
		COMPUTE WX-VALUE = W90-VAT(W05-V-RATE) * -1
		MOVE DPT-O-VAT-GL
				 TO WX-AC
		MOVE DPT-CODE	 TO WX-LEDG
		MOVE 10 	 TO WX-TYPE
                PERFORM GL-TRF-UPDATE.
027880	     PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
	     ADD W90-TAXVAL(W05-V-RATE)
				 TO PAR-DVAT-DAY
                                    PAR-DVAT-MTD
                                    PAR-DVAT-YTD.

       BC262-CHECK-END.
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO BC262-VAT.
028590	     ADD W90-NONTAX	 TO PAR-DNGS-DAY
028600                              PAR-DNGS-MTD
028610                              PAR-DNGS-YTD
028650	     PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
028940	     MOVE ZERO		 TO W90-CUR W90-30 W90-60
028950                              W90-120 W90-90.
028960     IF WS-AGE = "C"
028970         ADD W90-DB        TO DEB-CUR
028980         ADD W90-DB        TO W90-CUR
028990     ELSE
029000     IF WS-AGE = "3"
029010         ADD W90-DB        TO DEB-30
029020         ADD W90-DB        TO W90-30
029030     ELSE
029040     IF WS-AGE = "6"
029050         ADD W90-DB        TO DEB-60
029060         ADD W90-DB        TO W90-60
029070     ELSE
029080     IF WS-AGE = "9"
029090         ADD W90-DB        TO DEB-90
029100         ADD W90-DB        TO W90-90
029110     ELSE
029120     IF WS-AGE = "1"
029130         ADD W90-DB        TO DEB-120
029140         ADD W90-DB        TO W90-120.
      *
      *    ****    L O G   T H E   S A L E S M A N   &   I N V O I C E -
      *            A U D I T   F I L E   D E T A I L S
      *
029150       PERFORM BA90.
029160       PERFORM BA95.
      *
      *    ****    U P D A T E   T H E   D E B T O R   C A T E G O R Y
      *
029180       ADD 121 DEB-CAT     GIVING W90-RESULT.
029190       DIVIDE W90-RESULT BY 2
029200                           GIVING W90-CATKEY
029210                           REMAINDER W90-CS.
029220       ADD 1               TO W90-CS.
029230       MOVE W90-CATKEY     TO WS-PARKEY.
029240       PERFORM AY01 THRU AY59.
029250	     ADD W90-DB 	 TO PAR-CDRMTD (W90-CS)
029260				    PAR-CDRYTD (W90-CS).
029270       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      *
      *    ****  Reduce the number of jobs on the Accounts Receivable
      * 	 (Debtor) account record.
      *
	     SUBTRACT 1		 FROM DEB-JOBS.
      *
      *    ****    R E W R I T E   T H E   D E B T O R   R E C O R D
      *
029340       PERFORM REWRITE-DEBTOR THRU WRITE-DEBTOR-EXIT.
      *
      *    ****    U P D A T E   T H E   D E B T O R   C O N T R O L
      *            R E C O R D
      *
029350       ADD W90-DB          TO W70-OUT
029360                              W70-DT.
029370       ADD W90-CUR         TO W70-MTD.
029380       ADD W90-30          TO W70-ONE.
029390       ADD W90-60          TO W70-TWO.
029400       ADD W90-90          TO W70-THREE.
029410       ADD W90-120         TO W70-FOUR.
      *
      *    ****    M U S T   T H E   C A S H   D R A W   B E
      *            U P D A T E D
      *
029420     IF W90-ORG = "C"
029430         PERFORM BC268.
      *
      *    ****    U P D A T E   T H E   S T O C K   C O N T R O L
      *            R E C O R D
      *
029440       SUBTRACT W90-BAL    FROM STK-TOTAL.
029450       ADD W90-SALES       TO STK-SMTD STK-SYTD.
029460 BC266.
      *
      *    ****    U P D A T E   T H E   C O S T - O F - S A L E S
      *
029470       MOVE 8              TO WS-PARKEY.
029480       PERFORM AY01 THRU AY59.
029490       ADD W90-BDT         TO PAR-CSHT PAR-CMTD PAR-CYTD.
029500       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
029510 BC267.
      *
      *    ****    U P D A T E   T H E   O R D E R   F I L E
      *
	     MOVE W10-JCST	 TO JHD-REF.
	     PERFORM READ-JOBHDR-LOCK THRU READ-JOBHDR-EXIT.
	     PERFORM AY21 THRU AY59.
	     MOVE "I"		 TO JHD-DOC.
	     MOVE W90-REF	 TO JHD-INVOICE.
	     PERFORM REWRITE-JOBHDR-UNLOCK THRU WRITE-JOBHDR-EXIT.
      *
      *    ****    E N D   O F F   T H E   R E C O V E R Y   L O G
      *
029520       PERFORM AY70 THRU AY999.
      *
      *    ****    C L O S E   F I L E S   
      *            ( FORCE SYSTEM TO WRITE DETAILS TO DISK )
      *
029530	     CLOSE PARAM
029540             DEPART 
029550             DEBTOR 
029560             AUDIT 
029570             STOCK 
029580             INVOICE 
029590		   SALE.
029600       OPEN OUTPUT SALE.
029610       CLOSE SALE.
029620       OPEN I-O PARAM 
029630                DEPART 
029640                DEBTOR 
029650                AUDIT 
029660                STOCK 
029670                INVOICE 
029680                SALE.
029690	     DISPLAY CLEAR-L25.
029820       GO TO BC599.
      *
      *    ****    U P D A T E   T H E   C A S H   D R A W
      *
029840 BC268.
029850       PERFORM OPEN-CASH-DRAWER THRU OPEN-CASH-DRAWER-EXIT.
029860	     ADD 150 LS-DRAW	 GIVING WS-PARKEY.
029870       PERFORM AY01 THRU AY59.
029880       ADD W90-CASH        TO PAR-CASH.
029890       ADD W90-CHQ         TO PAR-CHEQUES.
029900       ADD W90-CARD        TO PAR-CARDS.
029910       SUBTRACT W90-CHANGE FROM PAR-CASH.
029920       ADD W90-TTAX        TO PAR-VATVAL.
029930	     ADD W90-EXCTOT	 TO PAR-TAXVAL.
029940	     COMPUTE PAR-CSH = PAR-CSH + W90-EXCTOT + W90-NONTAX.
029950	     COMPUTE PAR-CSHMTD = PAR-CSHMTD + W90-EXCTOT + W90-NONTAX.
029960	     COMPUTE PAR-CSHYTD = PAR-CSHYTD + W90-EXCTOT + W90-NONTAX.
029970       PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
       BC599.
             EXIT.
      /
049180 ZA000-INIT	       SECTION 90.
049190 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY W12-TODAY.
	     MOVE LS-USUB	 TO WS-USUB.
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
	     MOVE "AUDITF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-AUDITF.
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
	     MOVE "DBTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DBTRAN.
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "GARTEE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GARTEE.
	     MOVE "INVOIC"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-INVOIC.
	     MOVE "JOBCST"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBCST.
	     MOVE "JOBHDR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-JOBHDR.
	     MOVE "LEDTRF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-LEDTRF.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "RECOVER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-RECOVER.
	     MOVE "SALEJ"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-SALE.
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
	     MOVE "TXTRAN"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-TXTRAN.
049320	     MOVE 3		 TO WS-PARKEY.
049330       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-VAT-REG	 TO W05-VATNO.
	     MOVE PAR-CRDX	 TO WS-CARDEX.
	     MOVE PAR-PRICED-IND TO WS-PRC-IND.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.
       ZA01A.
014240       ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
014250       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF TRM-INVP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PRINTER
           ELSE
           IF TRM-INVP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PRINTER
           ELSE
           IF TRM-INVP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PRINTER
           ELSE
           IF TRM-INVP (WS-S6) = 4
               MOVE "COM1"       TO W02-PRINTER
           ELSE
           IF TRM-INVP (WS-S6) = 5
               MOVE "COM2"       TO W02-PRINTER
           ELSE
           IF TRM-INVP (WS-S6) = 9
	       MOVE LS-USER	 TO W02-USER.
             ADD 501 TRM-PRN2 (WS-S6)
                                 GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
                                 TO WS-COND.
             MOVE TRM-NORP (WS-S6)
                                 TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
				 TO WS-EXPP W08-EXPD.
             MOVE TRM-ECAN (WS-S6)
                                 TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
                                 TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
                                 TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
                                 TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
                                 TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
                                 TO WS-17CPI.
	     MOVE TRM-LENGTH (WS-S6)
				 TO WS-PRN-LENGTH.
	     MOVE TRM-PAGE (WS-S6)
				 TO WS-PGE-LENGTH.
	     MOVE TRM-PRN-TYPE (WS-S6)
				 TO WS-PRN-TYPE.
	   IF TRM-PRN-TYPE (WS-S6) = "H"
	       MOVE WS-COND	 TO WS-ECAN
	       MOVE 048		 TO W08-DEC (4)
	       MOVE 056		 TO W08-DEC (5)
	       MOVE W08-EXPD	 TO WS-EXPP.
       ZA01B.
053600       OPEN OUTPUT RECOVER.
053610       CLOSE RECOVER.
053620       OPEN I-O RECOVER.
052760	     DISPLAY "Waiting for printer response" AT 2525
052770		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
053560       OPEN OUTPUT PRNREP.
	     CALL X"91" USING X91-RES X91-FUN PRNREP.
	     DISPLAY SPACES AT 2525
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
053570	     MOVE WS-17CPI	 TO REP-DETAIL2.
053580       WRITE REP-LINE2 BEFORE 0 LINES.
053590       MOVE SPACES         TO REP-DETAIL2.
053630	     OPEN I-O AUDIT.
053640     IF RUNTIME-ERROR
053650         IF FLE-LOCKED
053660             GO TO ZA200
053670         ELSE
053680         IF FLE-LIMIT
053690             GO TO ZA49.
053700     IF WS-STATUS NOT = "00"
053710         MOVE 1            TO WS-F-ERROR
053720         PERFORM OPEN-ERROR.
054130	     OPEN I-O INVOICE.
054140     IF RUNTIME-ERROR
054150         IF FLE-LOCKED
054160             GO TO ZA200
054170         ELSE
054180         IF FLE-LIMIT
054190             GO TO ZA49
054200         ELSE
054210         IF IDX-CORRUPT
054220             MOVE 12       TO WS-F-ERROR
054230             GO TO ZA50.
054240     IF WS-STATUS NOT = "00"
054250         MOVE 12           TO WS-F-ERROR
054260         PERFORM OPEN-ERROR.
050580	     MOVE 1		 TO WS-PARKEY.
050590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050600       MOVE PAR-DMY        TO W12-TODAY.
050610       MOVE PAR-YMD        TO W12-T-YMD.
050620       MOVE PAR-COMPANY    TO W95-COMP.
050640       MOVE PAR-TELEPHONE  TO W95-TEL.
050650     IF PAR-STMINV = "I" OR "X"
050660         MOVE "N"          TO WS-HEAD
050670     ELSE
050680         MOVE "Y"          TO WS-HEAD.
050690       ADD 1               TO WS-PARKEY.
050700       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050710       MOVE PAR-ADDRESS1   TO W95-ADD1.
050720       MOVE PAR-ADDRESS2   TO W95-ADD2.
050730       MOVE PAR-ADDRESS3   TO W95-ADD3.
050740       MOVE PAR-ADDRESS4   TO W95-ADD4.
050750       MOVE PAR-POST-CDE   TO W95-POST.
050760     IF W95-ADD4 = SPACES
050770         MOVE W95-PC       TO W95-ADD4
050780         MOVE SPACES       TO W95-PC.
050790     IF W95-ADD3 = SPACES
050800         MOVE W95-ADD4     TO W95-ADD3
050810         MOVE SPACES       TO W95-ADD4.
050820     IF W95-ADD2 = SPACES
050830         MOVE W95-ADD3     TO W95-ADD2
050840         MOVE SPACES       TO W95-ADD3.
050850     IF W95-ADD1 = SPACES
050860         MOVE W95-ADD2     TO W95-ADD1
050870         MOVE SPACES       TO W95-ADD2.
050520       MOVE 5              TO WS-PARKEY.
050530       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050560	     MOVE PAR-DISCOUNT	 TO W15-DISCOUNT.
	     MOVE 1		 TO W05-V-RATE.

       ZA05-VAT.
	     MOVE W05-VAT-CODE	 TO DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE DPT-R-DATE	 TO WS-VAT-DATE(W05-V-RATE).
	     MOVE DPT-RATE	 TO W05-VAT(W05-V-RATE).
	     ADD 6 W05-V-RATE	 GIVING WS-S1.
	     MOVE DPT-P-RATE	 TO W05-VAT(WS-S1).
	   IF W05-V-RATE < 6
	       ADD 1		 TO W05-V-RATE
	       GO TO ZA05-VAT.
042520	     MOVE 6		 TO WS-PARKEY.
042550       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-INTEGRATE NOT = "Y"
               MOVE "N"          TO WX-INT
           ELSE
               MOVE "Y"          TO WX-INT.
             MOVE PAR-DEBGL      TO WX-DEBGL.
             MOVE PAR-CREGL      TO WX-CREGL.
             MOVE PAR-INTGL      TO WX-INTGL.
             MOVE PAR-BNKGL      TO WX-BNKGL.
             MOVE PAR-UNPROF     TO WX-UNPROF.
             MOVE PAR-REDGL      TO WX-REDGL.
             MOVE PAR-ADJGL      TO WX-ADJGL.
             MOVE PAR-RLGL       TO WX-RLGL.
             MOVE PAR-DSGL       TO WX-DSGL.
050890       MOVE 7              TO WS-PARKEY.
050900       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050910       MOVE PAR-CASH-AC    TO WS-CASH-AC.
055090	   IF LS-DRAW = ZERO
055100	       MOVE 40		 TO LS-DRAW.
055110	     ADD 150 LS-DRAW	 GIVING WS-PARKEY.
055120       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
055130       MOVE PAR-RECORD151  TO W85-RECORD151.
      *
      *     O L D   S T Y L E   C A S H   D R A W E R
      *
           IF W85-DRAWER  = "F"
               OPEN OUTPUT CSHDRW
               MOVE ALL "A"  TO CSH-OPEN.
051530       GO TO ZA999.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA50.
055350     IF WS-F-ERROR = 12
055360         MOVE "INVOICE file"
                                 TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.
       ZA51.
             EXIT PROGRAM.
      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
051090       MOVE 1 TO WS-S1.
051100	     MOVE SPACES	 TO WS-MID-LNE
				    WS-MD-LINE
                                    WS-MD-LNE.
051110 ZA60.
051120       MOVE WS-G1          TO WS-TCHAR (WS-S1) WS-BCHAR (WS-S1).
051130       MOVE WS-G1          TO WS-TCHR (WS-S1)  WS-BCHR (WS-S1).
051140       MOVE WS-G1          TO WS-TCH (WS-S1)   WS-BCH (WS-S1).
051140       MOVE WS-G1          TO WS-TCA (WS-S1)   WS-BCA (WS-S1).
051150       MOVE WS-G8          TO WS-TCR (WS-S1)   WS-BCR (WS-S1).
051160     IF WS-S1 < 80
051170         ADD 1 TO WS-S1
051180         GO TO ZA60.
051190       MOVE WS-G9          TO WS-TCR   (1).
051200       MOVE WS-G10         TO WS-TCR   (80).
051210       MOVE WS-G11         TO WS-BCR   (1).
051220       MOVE WS-G12         TO WS-BCR   (80).
051230       MOVE WS-G14         TO WS-TCHAR (1)  WS-BCHAR (1)
051240                              WS-TCHR  (1)  WS-BCHR  (1)
051240                              WS-TCA   (1)  WS-BCA   (1)
051250                              WS-TCH   (1)  WS-BCH   (1).
051260       MOVE WS-G13         TO WS-TCHAR (80) WS-BCHAR (80)
051270                              WS-TCHR  (80) WS-BCHR  (80)
051270                              WS-TCA   (80) WS-BCA   (80)
051280                              WS-TCH   (80) WS-BCH   (80).
051290       MOVE WS-G2          TO WS-TCHAR (4)  WS-TCHAR (19)
051300                              WS-TCHAR (50) WS-TCHAR (59) 
051310                              WS-TCHAR (69)
051320                              WS-TCHR  (18) WS-TCHR  (49)
051330                              WS-TCHR  (60) WS-TCHR  (70)
051340                              WS-TCA   (10) WS-TCA   (17)
051340                              WS-TCA   (47) WS-TCA   (56)
051340                              WS-TCA   (59) WS-TCA   (68)
051340                              WS-TCH   (8)  WS-TCH   (49)
051350                              WS-TCH   (61) WS-TCH   (72).
051360	     MOVE WS-G3 	 TO WS-MCH   (8)  WS-MCH   (49)
051400                              WS-MCH   (61) WS-MCH   (72)
                                    WS-MCA   (1)  WS-MCA   (80)
051340                              WS-MCA   (10) WS-MCA   (17)
051340                              WS-MCA   (47) WS-MCA   (56)
051340                              WS-MCA   (59) WS-MCA   (68)
051420				    WS-MCHR  (18) WS-MCHR  (49)
051430                              WS-MCHR  (60) WS-MCHR  (70)
051440                              WS-MCHR  (1)  WS-MCHR  (80) 
051450                              WS-MCH   (1)  WS-MCH   (80).
051460       MOVE WS-G4          TO WS-BCHAR (4)  WS-BCHAR (19) 
051470                              WS-BCHAR (50) WS-BCHAR (59) 
051480                              WS-BCHAR (69)
051490                              WS-BCHR  (18) WS-BCHR  (49)
051500                              WS-BCHR  (60) WS-BCHR  (70)
051340                              WS-BCA   (10) WS-BCA   (17)
051340                              WS-BCA   (47) WS-BCA   (56)
051340                              WS-BCA   (59) WS-BCA   (68)
051510                              WS-BCH   (8)  WS-BCH   (49)
051520                              WS-BCH   (61) WS-BCH   (72).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
051540 ZA200.
051550       DISPLAY "Files locked - Try later" AT 2312
051560                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
051570       DISPLAY "Press any key" AT 2512 
051580		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
051590	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
051600       EXIT PROGRAM.
051610 ZA999.
051620       EXIT.

051640 I-O-ERRORS      SECTION	91.
051650 OPEN-ERROR.
051660       DISPLAY CLR-SCREEN.
051670       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
051680       PERFORM DISPLAY-FILE-NAME.
051690
051700 READ-ERROR.
051710       DISPLAY CLR-SCREEN.
051720       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
051730       PERFORM DISPLAY-FILE-NAME.
051740
051750 WRITE-ERROR.
051760       DISPLAY CLR-SCREEN.
051770       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
051730       PERFORM DISPLAY-FILE-NAME.
051780

051790 DISPLAY-FILE-NAME.
056270     IF WS-F-ERROR = 1
056280	       MOVE W02-AUDITF	 TO WS-FILE
056290	       MOVE WS-AUDKEY	 TO WS-KEY
056300     ELSE
051840     IF WS-F-ERROR = 2
051850	       MOVE W02-NETWORK  TO WS-FILE
051860	       MOVE WS-NETKEY	 TO WS-KEY
056340     ELSE
056350     IF WS-F-ERROR = 5
056360         MOVE W02-DBTRAN   TO WS-FILE
041750         MOVE ZERO         TO WS-KEY
               MOVE TRA-KEY      TO WS-KEYX
051910     ELSE
051920     IF WS-F-ERROR = 6
041780         MOVE W02-DEBTOR   TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
056420     ELSE
056430     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
	   ELSE
           IF WS-F-ERROR = 9
               MOVE W02-GARTEE   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE GUA-KEY      TO WS-KEYX
056460     ELSE
056470     IF WS-F-ERROR = 12
041860         MOVE W02-INVOIC   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE DOC-KEY      TO WS-KEYX
052070     ELSE
052080     IF WS-F-ERROR = 15
052090         MOVE WS-PARID     TO WS-FILE
052100         MOVE WS-PARKEY    TO WS-KEY
052190     ELSE
052200     IF WS-F-ERROR = 13
052210         MOVE W02-JOBCST   TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
               MOVE JOB-KEY      TO WS-KEYX
056540     ELSE
056550     IF WS-F-ERROR = 18
056560         MOVE W02-RECOVER  TO WS-FILE
056570         MOVE WS-RECKEY    TO WS-KEY
052150     ELSE
052160     IF WS-F-ERROR = 19
052170	       MOVE W02-SALE	 TO WS-FILE
052180	       MOVE WS-SALKEY	 TO WS-KEY
052190     ELSE
052200     IF WS-F-ERROR = 22
052210         MOVE W02-STOCKF   TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
056700     ELSE
056710     IF WS-F-ERROR = 32
056720         MOVE W02-TXTRAN   TO WS-FILE
056730         MOVE ZERO         TO WS-KEY
	       MOVE TAX-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 40
041860         MOVE W02-LEDTRF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE XFR-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE W02-CARDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CRD-KEY	 TO WS-KEYX.
052310     IF WS-STATUS = "10"
052320         MOVE "End of FILE" TO WS-STAT-MESSAGE
052330     ELSE
052340     IF WS-STATUS = "22"
052350         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
052360     ELSE
052370     IF WS-STATUS = "23"
052380         MOVE "Invalid record number" TO WS-STAT-MESSAGE
052390     ELSE
052400     IF WS-STATUS = "24"
052410         MOVE "DISK full" TO WS-STAT-MESSAGE
052420     ELSE
052430     IF WS-STATUS = "30"
052440         MOVE "DISK error" TO WS-STAT-MESSAGE
052450     ELSE
052460     IF WS-STATUS = "94"
052470         MOVE "FILE locked" TO WS-STAT-MESSAGE.
052480       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
052490       DISPLAY "Status " AT 1212 
052500                WS-STATUS WITH FOREGROUND-COLOR 11
052510               ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
052520     IF WS-STATUS NOT = "94" 
052530         DISPLAY "Key    " AT 1412 
052540                  WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
052550         DISPLAY "Reverse backup or contact program Support"
052560                  AT 1612.
052570         DISPLAY "Please make a note of these details" AT 1812.
052580       STOP RUN.
