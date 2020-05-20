      $SET LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	 ******      **     **	       *
      *   **	**     **     **    **	**    **    ****    **	       *
      *   **	       **     **    **	**	   **  **   **	       *
      *    ******      **     *******	 ******   **	**  **	       *
      * 	**     **     **	      **  ********  **	       *
      *   **	**     **     **	**    **  **	**  **	       *
      *    ******      **     **	 ******   **	**  *******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S A L E S	P E R	S A L E S   L E D G E R   F O R   A    *
      *     S P E C I F I C   P E R I O D			       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPTOP.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1997.

	   This program prints a list of stock items that have been sold
	   during a specified period for the sales ledger that has been
	   requested.

	   The program reads the cardex file to extract the sales and
	   accumulates the sales for each stock item and reduces the
	   sales for each credit note.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1997 - 2008
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

       COPY CARDEX.SL.

000120 COPY CONTROL.SL.

       COPY DEPART.SL.

000120 COPY PARAM.SL.

000140 COPY STOCK.SL.

      $SET KEYCOMPRESS "7"
000010	   SELECT DPTSALES   ASSIGN DISK
000040			     STATUS WS-STATUS
000050			     ACCESS DYNAMIC
000060			     ORGANIZATION INDEXED
000070			     RECORD SAL-KEY
000080			     ALTERNATE RECORD SAL-CODE DUPLICATES
000080			     ALTERNATE RECORD SAL-QUANT DUPLICATES
000090			     ALTERNATE RECORD SAL-TSELL DUPLICATES
000130			     ALTERNATE RECORD SAL-GP DUPLICATES.
      $SET NOKEYCOMPRESS

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CARDEX.FDE.

000120 COPY CONTROL.FDE.

       COPY DEPART.FDE.

000290 COPY PARAM.FDE.

000540 COPY STOCK.FDE.

      /
000010 FD  DPTSALES  IS EXTERNAL
		     LABEL RECORD STANDARD
000020		     VALUE OF FILE-ID W02-DPTSALES.
000220 01  SAL-REC1.
000230	   03  SAL-KEY.
	       05  SAL-LEDG    PIC  X(04).
	       05  SAL-CODE    PIC  X(18).
	   03  SAL-QUANT.
000250	       05  SAL-QNT     PIC S9(07)V99 COMP.
	   03  SAL-COST        PIC S9(09)V99 COMP.
	   03  SAL-TSELL.
	       05  SAL-SELL    PIC S9(09)V99 COMP.
	   03  SAL-GP.
	       05  SAL-PROFIT  PIC S9(09)V99 COMP.
	   03  SAL-DESC        PIC  X(30).

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
001550 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
001560 77  WS-SUB	   PIC	9(04)	 COMP-5.
001560 77  WS-S            PIC  9(04)    COMP-5.
001570 77  WS-S1           PIC  9(04)    COMP-5.
001580 77  WS-S2           PIC  9(04)    COMP-5.
001590 77  WS-S3           PIC  9(04)    COMP-5.
001600 77  WS-S4           PIC  9(04)    COMP-5.
001610 77  WS-S5           PIC  9(04)    COMP-5.
001620 77  WS-S6           PIC  9(04)    COMP-5.
001630 77  WS-S7	   PIC	9(04)	 COMP-5.
001640 77  WS-S8           PIC  9(04)    COMP-5.
001650 77  WS-S9           PIC  9(04)    COMP-5.
002850 77  WS-IXS1         PIC  9(04)    COMP-5.
       77  WS-CRDX	   PIC	9(04)	 COMP-5.
001690 77  WS-ENQPOS	   PIC	9(06)	 COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
001670 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001810 77  WS-KEYSTR       PIC  9(06)    COMP-5.
001820 77  WS-OPTION       PIC  X(01).
003400 77  WS-PAGE	   PIC	9(04)	 COMP-5.
       77  WS-NUM	   PIC	ZZ9.
003550 77  WS-LEDG	   PIC	X(04).
001820 77  WS-ENQ	   PIC	X(01).
       77  WS-MONTH	   PIC	9(02).
001820 77  WS-ETYPE	   PIC	X(01).
001330 77  WS-DESC-H1	    PIC	 X(14) VALUE "Description".
       77  WS-DESC-H2	    PIC	 X(14) VALUE "Description 2".
       77  WS-RT-HD	    PIC	 X(10) VALUE "-Retail   ".
       77  WS-WS-HD	    PIC	 X(10) VALUE "-Wholesale".
       77  WS-CS-HD	    PIC	 X(10) VALUE "-Cash sale".
001850 77  WS-ERROR	   PIC	9(01) VALUE ZERO.
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
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
003290	   03  WS-MID-LNE.
003300	       05  WS-MCHR  PIC  X(80) VALUE "�                �
      -        "                      �          �         �         �".
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
002450	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "�".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "�".
	       05  FILLER      PIC  X(01) VALUE "�".
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

002230 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "SLS".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CARDEX.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000140 COPY STOCK.ID.

       COPY DPTSALES.ID.

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

	   03  R-L1 REDEFINES W02-PRN-LINE.
	       05  R-DETAIL.
		   07  R-HL	   PIC	X(96).
		   07  FILLER	   PIC	X(36).

	   03  R-L2 REDEFINES W02-PRN-LINE.
	       05  R-PGE	   PIC X(06).
	       05  R-PNO	   PIC Z(03)9.
	       05  FILLER	   PIC X(08).
	       05  R-CPY	   PIC X(42).
	       05  FILLER	   PIC X(04).
	       05  R-DATE	   PIC X(06).
	       05  R-DTE	   PIC Z9/99/9999.

	   03  SRP-L1 REDEFINES W02-PRN-LINE.
	       05  SRP-H1	   PIC X(14).
	       05  SRP-NO	   PIC X(04).
	       05  FILLER	   PIC X(01).
	       05  SRP-SMAN	   PIC X(30).
	       05  FILLER	   PIC X(31).

	   03  SRP-L2 REDEFINES W02-PRN-LINE.
	       05  SRP-H2	   PIC	X(51).
	       05  SRP-H3	   PIC	X(29).

	   03  SRP-L3 REDEFINES W02-PRN-LINE.
	       05  SRP-ACT	   PIC	X(06).
	       05  FILLER	   PIC	X(02).
	       05  SRP-NAME	   PIC	X(40).
	       05  FILLER	   PIC	X(01).
	       05  SRP-DATE	   PIC	Z9/99/9999.
	       05  FILLER	   PIC	X(01).
	       05  SRP-REF	   PIC	X(08).
	       05  FILLER	   PIC	X(28).

	   03  SRP-L4 REDEFINES W02-PRN-LINE.
	       05  SRP-H4	   PIC	X(19).
	       05  SRP-H5	   PIC	X(29).
	       05  SRP-H6	   PIC	X(11).
	       05  SRP-H7	   PIC	X(13).
	       05  SRP-H8	   PIC	X(15).
	       05  SRP-H9	   PIC	X(10).

	   03  SRP-L5 REDEFINES W02-PRN-LINE.
	       05  SRP-EXT-ITM.
		   07  SRP-ITEM	   PIC	X(14).
		   07  SRP-SL	   PIC	X(01).
		   07  SRP-ITM	   PIC	X(03).
	       05  FILLER	   PIC	X(01).
	       05  SRP-DESC	   PIC	X(26).
	       05  FILLER	   PIC	X(01).
	       05  SRP-QUANT	   PIC	Z(06)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  SRP-COST	   PIC	Z(07)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  SRP-SELL	   PIC	Z(07)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  SRP-EXTEND	   PIC	Z(07)9.99-.

       COPY W05.VAT.

003740 01  W05.
003750     03  W05-S OCCURS 300   PIC 9(04)     COMP-5.

       COPY W10.STK.

       COPY W12.WS.

       COPY W15.WS.

       01  W15-KEYS.
	   03  W15-LEDG.
	       05  W15-LEGNO   PIC  9(04).

       01  W15-ALLOCATIONS.
           03  W15-DETAIL.
             05  W15-TRAN OCCURS 15.
               07  W15-VALUE   PIC  Z(06)9.99- BLANK WHEN ZERO.
               07  W15-ALLOC   PIC  Z(06)9.99- BLANK WHEN ZERO.
	       07  W15-REC     PIC  X(08).
           03  W15-DISPLAY REDEFINES W15-DETAIL.
               05  W15-BAL1    PIC  X(11).
               05  W15-ALL1    PIC  X(11).
	       05  W15-REC1    PIC  X(08).
               05  W15-BAL2    PIC  X(11).
               05  W15-ALL2    PIC  X(11).
	       05  W15-REC2    PIC  X(08).
               05  W15-BAL3    PIC  X(11).
               05  W15-ALL3    PIC  X(11).
	       05  W15-REC3    PIC  X(08).
               05  W15-BAL4    PIC  X(11).
               05  W15-ALL4    PIC  X(11).
	       05  W15-REC4    PIC  X(08).
               05  W15-BAL5    PIC  X(11).
               05  W15-ALL5    PIC  X(11).
	       05  W15-REC5    PIC  X(08).
               05  W15-BAL6    PIC  X(11).
               05  W15-ALL6    PIC  X(11).
	       05  W15-REC6    PIC  X(08).
               05  W15-BAL7    PIC  X(11).
               05  W15-ALL7    PIC  X(11).
	       05  W15-REC7    PIC  X(08).
               05  W15-BAL8    PIC  X(11).
               05  W15-ALL8    PIC  X(11).
	       05  W15-REC8    PIC  X(08).
               05  W15-BAL9    PIC  X(11).
               05  W15-ALL9    PIC  X(11).
	       05  W15-REC9    PIC  X(08).
               05  W15-BAL10   PIC  X(11).
               05  W15-ALL10   PIC  X(11).
	       05  W15-REC10   PIC  X(08).
               05  W15-BAL11   PIC  X(11).
               05  W15-ALL11   PIC  X(11).
	       05  W15-REC11   PIC  X(08).
               05  W15-BAL12   PIC  X(11).
               05  W15-ALL12   PIC  X(11).
	       05  W15-REC12   PIC  X(08).
               05  W15-BAL13   PIC  X(11).
               05  W15-ALL13   PIC  X(11).
	       05  W15-REC13   PIC  X(08).
               05  W15-BAL14   PIC  X(11).
               05  W15-ALL14   PIC  X(11).
	       05  W15-REC14   PIC  X(08).
               05  W15-BAL15   PIC  X(11).
               05  W15-ALL15   PIC  X(11).
	       05  W15-REC15   PIC  X(08).

       COPY W20.WS.

002850 01  W25-CALCS.
002860     03  W25-RESULT      PIC 9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880         05  W25-WHOLE   PIC 9(05).
002890         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY    PIC 9(04).
               05  W25-SUB    PIC 9(01).
               05  FILLER     PIC 9(02).
002900     03  W25-TOTAL       PIC S9(09)V99 COMP-3.
002910     03  W25-VALUE       PIC S9(09)V99 COMP-3.

       01  W30-DISPLAY.
	   03  W30-CODE        PIC  X(19).
	   03  W30-DESC        PIC  X(31).
	   03  W30-QUANT       PIC  Z(06)9.99-.
	   03  W30-VALUE       PIC  Z(08)9.99-.

002920 01  W40-COMPANY.
002930     03  W40-NAME1.
002940         05  W40-NAME.
002950             07  W40-ENTRY PIC X(08).
002960             07  FILLER  PIC X(32).
002970	       05  FILLER      PIC X(32).
002980     03  W40-NAME2 REDEFINES W40-NAME1.
002990         05  W40-SURNAME PIC X(24).
003000         05  W40-FNAME   PIC X(24).
003010         05  W40-SNAME   PIC X(24).
003020     03  W40-NAME3 REDEFINES W40-NAME1.
003030         05  W40-NCHAR OCCURS 72 PIC X(01).
003040     03  W40-ANAME       PIC X(40).
           03  W40-CONTACT     PIC  X(30).
003050     03  W40-DEBTREC.
               05  W40-ACNO      PIC X(06).
003060         05  FILLER        PIC X(456).
003070     03  W40-ADD REDEFINES W40-DEBTREC.
003080       05  W40-ADDRESS1.
003090         07  W40-ADD1    PIC X(30).
003100         07  W40-ADD2    PIC X(30).
003110         07  W40-ADD3    PIC X(30).
003120         07  W40-ADD4    PIC X(30).
003130       05  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
003140         07  W40-ACHAR OCCURS 120 PIC X(01).
003150       05  W40-PADDRESS.
003160         07  W40-PADD1   PIC X(30).
003170         07  W40-PADD2   PIC X(30).
003180         07  W40-PADD3   PIC X(30).
003190         07  W40-PADD4   PIC X(30).
003200       05  W40-PADDR2   REDEFINES W40-PADDRESS.
003210         07  W40-PCHAR OCCURS 120 PIC X(01).
             05  W40-SPACES    PIC X(30).
             05  W40-TOP.
               07  W40-T1      PIC  X(01).
               07  W40-T2-27   PIC  X(26).
               07  W40-T28     PIC  X(01).
             05  W40-MID.
               07  W40-M1      PIC  X(01).
               07  W40-M2-27   PIC  X(26).
               07  W40-M28     PIC  X(01).
             05  W40-BOT.
               07  W40-B1      PIC  X(01).
               07  W40-B2-27   PIC  X(26).
               07  W40-B28     PIC  X(01).
003220       05  FILLER        PIC X(111).
003230     03  W40-POST        PIC 9(04).
003240     03  W40-PPOST       PIC 9(04).
003250     03  W40-TEL         PIC X(14).
003260     03  W40-HTEL        PIC X(14).
003260     03  W40-FAX         PIC X(14).
003270     03  W40-ID.
003280         05  W40-IDNO    PIC X(14).
003290     03  W40-TAX REDEFINES W40-ID.
003300         05  W40-TAXNO   PIC X(14).
003310     03  W40-TITLE       PIC 9(02).
003320     03  W40-LANG        PIC X(01).
003330     03  W40-CAT         PIC 9(02).
003330     03  W40-OCAT        PIC 9(02).
003340	   03  W40-LEDG        PIC X(04).
003350     03  W40-FIN         PIC X(01).
003360     03  W40-CRL         PIC S9(07)    COMP-3.
003370     03  W40-ONE         PIC S9(09)V99 COMP-3.
003380     03  W40-MTD         PIC S9(09)V99 COMP-3.
003390     03  W40-CUR         PIC S9(09)V99 COMP-3.
003400     03  W40-30          PIC S9(09)V99 COMP-3.
003410     03  W40-60          PIC S9(09)V99 COMP-3.
003420     03  W40-90          PIC S9(09)V99 COMP-3.
003420     03  W40-120         PIC S9(09)V99 COMP-3.
003430     03  W40-INT         PIC S9(09)V99 COMP-3.
003440     03  W40-TERMS       PIC X(01).
003440     03  W40-INV-TYPE    PIC X(01).
003450     03  W40-SDISC       PIC 9(02).
003450     03  W40-DISC        PIC 9(02).
003460     03  W40-SMAN        PIC 9(02).
           03  W40-LAB         PIC  X(01).
           03  W40-OI          PIC  X(01).
           03  W40-OOI         PIC  X(01).

       01  W40-BALANCES.
           03  W40-A-MTD       PIC S9(09)V99 COMP-3.
003390     03  W40-A-CUR       PIC S9(09)V99 COMP-3.
003400     03  W40-A-30        PIC S9(09)V99 COMP-3.
003410     03  W40-A-60        PIC S9(09)V99 COMP-3.
003420     03  W40-A-90        PIC S9(09)V99 COMP-3.
003420     03  W40-A-120       PIC S9(09)V99 COMP-3.
003430     03  W40-A-INT       PIC S9(09)V99 COMP-3.
           03  W40-D-MTD       PIC S9(09)V99 COMP-3.
003390     03  W40-D-CUR       PIC S9(09)V99 COMP-3.
003400     03  W40-D-30        PIC S9(09)V99 COMP-3.
003410     03  W40-D-60        PIC S9(09)V99 COMP-3.
003420     03  W40-D-90        PIC S9(09)V99 COMP-3.
003420     03  W40-D-120       PIC S9(09)V99 COMP-3.
003430     03  W40-D-INT       PIC S9(09)V99 COMP-3.
           03  W40-T-MTD       PIC S9(09)V99 COMP-3.
003390     03  W40-T-CUR       PIC S9(09)V99 COMP-3.
003400     03  W40-T-30        PIC S9(09)V99 COMP-3.
003410     03  W40-T-60        PIC S9(09)V99 COMP-3.
003420     03  W40-T-90        PIC S9(09)V99 COMP-3.
003420     03  W40-T-120       PIC S9(09)V99 COMP-3.
003430     03  W40-T-INT       PIC S9(09)V99 COMP-3.

004440 01  W40-INDIC.
004450*     03	W40-SMAN.
004460* 	05  W40-SREP	PIC 9(01) COMP-5 OCCURS 50.
004470*     03	W40-DRAW.
004480* 	05  W40-DREP	PIC 9(01) COMP-5 OCCURS 40.
004490	   03  W40-EDIT.
004500         05  W40-KEY     PIC ZZZZ9.
004510         05  W40-PERC    PIC ZZ9.99-.
004520     03  W40-EDIT2 REDEFINES W40-EDIT.
004530         05  FILLER      PIC X(01).
004540         05  W40-AMOUNT  PIC Z(06)9.99-.

       COPY W40.WS.

003890 01  W45-TRAN.
003900     03  W45-CODE        PIC 9(02).
003910     03  W45-ENG         PIC X(12).
003920     03  W45-AFR         PIC X(12).
003930     03  W45-ACT         PIC X(01).

       COPY FUNCTION.WS.

003530 COPY W50.WS.

       01  W55-HEADING.
	   03  FILLER	      PIC  X(22) VALUE "Sales for the period:".
	   03  W55-START      PIC  99/99/9999.
	   03  FILLER	      PIC  X(03) VALUE " - ".
	   03  W55-END	      PIC  99/99/9999.

003550 01  W60-NME-ADD.
003560     03  W60-NAME.
003570         05  W60-NCHAR OCCURS 40 PIC X(01).
003580     03  W60-ADDRESS.
003590         05  W60-ACHAR OCCURS 70 PIC X(01).
003580     03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
003600     03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

       01  W75-KEYS.
	   03  W75-S	      PIC  9(02) COMP-5.
	   03  W75-S1	      PIC  9(02) COMP-5.
	   03  W75-S2	      PIC  9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
	   03  W75-SLEDG      PIC  X(04)    OCCURS 10.

003750 01  W75-STORE.
003760	   03  W75-SNAME      PIC  X(08).
	   03  W75-SREF       PIC  X(08).
	   03  W75-SACN       PIC  X(06).
	   03  W75-SDTE       PIC  9(06).
	   03  W75-SSM	      PIC  9(02).

003910 01  W80-ACCOUNT.
003920     03  W80-ACNO        PIC X(06).
003940     03  W80-TYPE        PIC 9(01).

004390 01  W95-STM.
004400     03  W95-COMP        PIC X(40).
004410     03  W95-V1.
004420         05  W95-T       PIC Z(04)9.99.
004430     03  W95-V2.
004440         05  W95-B       PIC Z(06)9.99-.
004450     03  W95-D.
004460	       05  W95-DTE     PIC Z9/99/9999.
004470     03  W95-POST        PIC 9(04).
004480     03  W95-TEL         PIC X(08).

004490 01  W100-EDIT.
004500	   03  W100-COST       PIC Z(06)9.99.
004500	   03  W100-SELL       PIC Z(06)9.99.
004510     03  W100-CUR        PIC Z(08)9.99-.
004520     03  W100-30         PIC Z(08)9.99-.
004530     03  W100-60         PIC Z(08)9.99-.
004540     03  W100-90         PIC Z(08)9.99-.
004540     03  W100-120        PIC Z(08)9.99-.
004550     03  W100-INT        PIC Z(08)9.99.
004560     03  W100-CRL        PIC Z(06)9.
004570     03  W100-KEY        PIC Z(05)9.
	   03  W100-SER        PIC ZZ9.
007020	   03  W100-PRC        PIC Z(06)9.99-.
007100	   03  W100-VAL        PIC Z(08)9.99-.
007120     03  W100-V2.
007130         05  W100-S6V2   PIC Z(05)9.99-.
007140	   03  W100-V3 REDEFINES W100-V2.
007150         05  W100-PRICE  PIC X(09).
007160         05  FILLER      PIC X(01).
007170	   03  W100-V4 REDEFINES W100-V2.
007180         05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
	       05  FILLER      PIC  X(02).
	       05  W100-QNT    PIC Z(04)9.99.

005390 01  W200-TRAN.
005400     03  W200-ACCUM.
	       05  W200-LEDG   PIC  X(04)	    OCCURS 300.
005410         05  W200-DT     PIC S9(07)V99 COMP-3 OCCURS 300.
005420         05  W200-CR     PIC S9(07)V99 COMP-3 OCCURS 300.
005430         05  W200-DTOT   PIC S9(07)V99 COMP-3.
005440         05  W200-CTOT   PIC S9(07)V99 COMP-3.
005450         05  W200-COST   PIC S9(07)V99 COMP-3.
005460         05  W200-SELL   PIC S9(07)V99 COMP-3.
005470         05  W200-NET    PIC S9(07)V99 COMP-3.
005470         05  W200-ADJ    PIC S9(07)V99 COMP-3.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004580 SCREEN SECTION.

       COPY BLANK.CRT.

       01  MENU-INSTRUCT.
	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE "Use ".
	   03	       COLUMN  6 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "".
	   03	       COLUMN  7 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " or ".
	   03	       COLUMN 11 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "".
	   03	       COLUMN 12 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to point to an item, and ".
	   03	       COLUMN 38 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "<Enter>".
	   03	       COLUMN 45 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to Select ".
	   03	       COLUMN 56 HIGHLIGHT
				 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 6
				 VALUE "<Esc>".
	   03	       COLUMN 61 BACKGROUND-COLOR 3
				 FOREGROUND-COLOR 1
				 VALUE " to Exit".

000850 01  S-PROG.
009350	   03  LINE  2 COLUMN 27 FOREGROUND-COLOR 15 VALUE
009360		       "SALES PERFORMANCE REPORTING".

       01  S-MENU.
	   03  LINE  4 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "旼컴컴컴컴컴컴컴컴컴컴컴컴커".
	   03  LINE  5 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "� ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "SALES STATISTICS OPTIONS".
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " �".
	   03  LINE  6 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "� ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Stock Sales - Quantity �".
	   03  LINE  7 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "� ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "             - Value    �".
001470	   03  LINE  8 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "� ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "             - Profit   �".
001470	   03  LINE  9 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "� ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Stock Gross Profit     �".
001470	   03  LINE 10 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "� ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "5".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Sales Performance Rep. �".
001470	   03  LINE 11 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "� ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "6".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Change Selection       �".
001470	   03  LINE 12 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "� ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit                   �".
	   03  LINE 13 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "읕컴컴컴컴컴컴컴컴컴컴컴컴켸".

004470 01  S04.
004480	   03  LINE 10 COLUMN 20 FOREGROUND-COLOR 14
004490                           VALUE "Busy loading stock departments".

009340 01  S05D.
	   03  LINE  4 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커".
	   03  LINE  5 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"�    Transactions for Period.    �".
	   03  LINE  6 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑".
	   03  LINE  7 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"�   Starting Date :              �".
	   03  LINE  8 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"�   Ending Date   :              �".
	   03  LINE  9 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑".
	   03  LINE 10 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"쿏ates to be keyed in as ".
	   03	       COLUMN 49 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "DDMMCCYY".
	   03	       COLUMN 57 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "�".
	   03  LINE 11 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸".

       01  S05A.
	   03  LINE  7 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-START AUTO.
	   03  LINE  8 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-END AUTO.

       01  S06.
	   03  LINE  3 COLUMN  2 VALUE "Sales Ledger:".
	   03	       COLUMN 16 FOREGROUND-COLOR 3 HIGHLIGHT
				 PIC  X(04) FROM DPT-CODE.
	   03	       COLUMN 21 FOREGROUND-COLOR 6 HIGHLIGHT
				 PIC  X(30) FROM DPT-DESC.
	   03  LINE  4 COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
		       VALUE "Sales Performance for the period:- ".
	   03	       COLUMN 37 FOREGROUND-COLOR 6 HIGHLIGHT
				 PIC Z9/99/9999 FROM W15-START.
	   03	       COLUMN 47 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE " to ".
	   03	       COLUMN 51 FOREGROUND-COLOR 6 HIGHLIGHT
				 PIC Z9/99/9999 FROM W15-END.
	   03  LINE  5 COLUMN  6 FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
	       VALUE "Item Code      Stock Description                Qu
      - 	     "antity        Value ".

       01  S08.
	   03  LINE  4 COLUMN 29 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(04) USING W15-LEDG AUTO.

       01  S10D.
009350	   03  LINE  2 COLUMN 27 FOREGROUND-COLOR 15 VALUE
009360		       "SALES PERFORMANCE REPORTING".

007910 01  S36.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(14) USING W10-ITEM AUTO.

       COPY DEPART.CRT.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
003240	   IF LS0-SALEV < 9
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
010270	     PERFORM ZA000-INIT.
	     PERFORM BE000.
	   IF WS-ERROR = 9
	       MOVE "No details in Department file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       CLOSE DPTSALES
	       GO TO AA49.
	     PERFORM BA000.
	     CLOSE DPTSALES.

       AA49.
013150       EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

       COPY DATE.CHK.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP3.

      /
      *    ****    READ FILES ROUTINES
      *
011650 AC000              SECTION.

       COPY CARDEX.RD.

011670 COPY CONTROL.RD.

       COPY DEPART.RD.

011750 COPY PARAM.RD.

       COPY STOCK.RD.

000010 READ-SALES.
000020	     READ DPTSALES	 WITH IGNORE LOCK
000030	       KEY IS SAL-KEY.
000040	   IF WS-STATUS = "00"
000050	       MOVE ZERO	 TO WS-F-ERROR
000060	       GO TO READ-SALES-EXIT.
000070	   IF WS-STATUS = "23"
000080	       MOVE 44		 TO WS-F-ERROR
000090	       GO TO READ-SALES-EXIT.
000100     IF WS-STAT1 = "2" OR "3" OR "4"
000110	       MOVE 44		 TO WS-F-ERROR
000120         PERFORM READ-ERROR.
000160	     GO TO READ-SALES-EXIT.

000380 READ-SALES-NEXT.
000390	     READ DPTSALES NEXT	 WITH IGNORE LOCK.
000400     IF WS-STATUS = "00"
000050	       MOVE ZERO	 TO WS-F-ERROR
000060	       GO TO READ-SALES-EXIT.
000430	   IF (WS-STATUS = "23") OR
000440        (WS-STAT1 = "1")
000450	       MOVE 44		 TO WS-F-ERROR
000460	       GO TO READ-SALES-EXIT.
000470     IF WS-STAT1 = "2" OR "3" OR "4"
000480	       MOVE 44		 TO WS-F-ERROR
000490         PERFORM READ-ERROR.
000530	     GO TO READ-SALES-EXIT.

000380 READ-SALES-PREV.
000390	     READ DPTSALES PREVIOUS WITH IGNORE LOCK.
000400     IF WS-STATUS = "00"
000050	       MOVE ZERO	 TO WS-F-ERROR
000060	       GO TO READ-SALES-EXIT.
000430	   IF (WS-STATUS = "23") OR
000440        (WS-STAT1 = "1")
000450	       MOVE 44		 TO WS-F-ERROR
000460	       GO TO READ-SALES-EXIT.
000470     IF WS-STAT1 = "2" OR "3" OR "4"
000480	       MOVE 44		 TO WS-F-ERROR
000490         PERFORM READ-ERROR.
000530	     GO TO READ-SALES-EXIT.

000750 START-AT-SALES-KEY.
000760	     START DPTSALES
000770	       KEY >= SAL-KEY.
000780     IF WS-STATUS = "00"
000790         MOVE ZERO         TO WS-F-ERROR
000800	       GO TO READ-SALES-EXIT.
000810     IF WS-STATUS = "23"
000820	       MOVE 44		 TO WS-F-ERROR
000830	       GO TO READ-SALES-EXIT.
000840     IF WS-STAT1 = "2" OR "3" OR "4"
000850	       MOVE 44		 TO WS-F-ERROR
000860         PERFORM READ-ERROR.
000870	     GO TO READ-SALES-EXIT.

000750 START-AT-SALES-CODE.
000760	     START DPTSALES
000770	       KEY >= SAL-CODE.
000780     IF WS-STATUS = "00"
000790         MOVE ZERO         TO WS-F-ERROR
000800	       GO TO READ-SALES-EXIT.
000810     IF WS-STATUS = "23"
000820	       MOVE 44		 TO WS-F-ERROR
000830	       GO TO READ-SALES-EXIT.
000840     IF WS-STAT1 = "2" OR "3" OR "4"
000850	       MOVE 44		 TO WS-F-ERROR
000860         PERFORM READ-ERROR.
000870	     GO TO READ-SALES-EXIT.

000890 START-AT-SALES-QNT.
000900	     START DPTSALES
000910	       KEY <= SAL-QUANT.
000920     IF WS-STATUS = "00"
000930         MOVE ZERO         TO WS-F-ERROR
000940	       GO TO READ-SALES-EXIT.
000950     IF WS-STATUS = "23"
000960	       MOVE 44		 TO WS-F-ERROR
000970	       GO TO READ-SALES-EXIT.
000980     IF WS-STAT1 = "2" OR "3" OR "4"
000990	       MOVE 44		 TO WS-F-ERROR
001000         PERFORM READ-ERROR.
001010	     GO TO READ-SALES-EXIT.

001030 START-AT-SALES-TSELL.
001040	     START DPTSALES
001050	       KEY <= SAL-TSELL.
001060     IF WS-STATUS = "00"
001070         MOVE ZERO         TO WS-F-ERROR
001080	       GO TO READ-SALES-EXIT.
001090     IF WS-STATUS = "23"
001100	       MOVE 44		 TO WS-F-ERROR
001110	       GO TO READ-SALES-EXIT.
001120     IF WS-STAT1 = "2" OR "3" OR "4"
001130	       MOVE 44		 TO WS-F-ERROR
001140         PERFORM READ-ERROR.
001150	     GO TO READ-SALES-EXIT.

001170 START-AT-SALES-GP.
001180	     START DPTSALES
001190	       KEY <= SAL-GP.
001200     IF WS-STATUS = "00"
001210         MOVE ZERO         TO WS-F-ERROR
001220	       GO TO READ-SALES-EXIT.
001230     IF WS-STATUS = "23"
001240	       MOVE 44		 TO WS-F-ERROR
001250	       GO TO READ-SALES-EXIT.
001260     IF WS-STAT1 = "2" OR "3" OR "4"
001270	       MOVE 44		 TO WS-F-ERROR
001280         PERFORM READ-ERROR.

001580 READ-SALES-EXIT.
001590       EXIT.

      /
011350 AD000-WRITE             SECTION.

010000 REWRITE-SALES.
010010	     REWRITE SAL-REC1.
000030     IF WS-STAT1 NOT = "0"
010030	       MOVE 44		 TO WS-F-ERROR
010040         PERFORM WRITE-ERROR.
010050	     GO TO WRITE-SALES-EXIT.

010060 WRITE-SALES.
010070	     WRITE SAL-REC1.
000030     IF WS-STAT1 NOT = "0"
010090	       IF WS-STATUS = "22"
010100		   GO TO REWRITE-SALES
010110         ELSE
010120		   MOVE 44	 TO WS-F-ERROR
010130             PERFORM WRITE-ERROR.

010140 WRITE-SALES-EXIT.
010150       EXIT.

       COPY DEPART.LUP.

       COPY APAC.HLP.

      /
      *  ****  E N Q U I R Y   ( S A L E S   P E R F O R M A N C E )
      *
021000 BA000		       SECTION.
021010 BA0.
021020       DISPLAY CLR-SCREEN.
	     DISPLAY S10D.
	     PERFORM SAVE-SCREEN-3.

       BA00.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 26		 TO SHADE-COL.
	     MOVE 32		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
	     DISPLAY S05D.
	     PERFORM SCREEN-SHADOW.
	     MOVE W12-TODAY	 TO W15-START W15-END.

       BA05.
	     DISPLAY "Press " AT 2552
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape to Exit"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
	     ACCEPT S05A.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BA999
		 WHEN OTHER	 CALL X"E5"
		     GO TO BA05
	       END-EVALUATE.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BA05.
	     PERFORM CLEAR-L50.
	     MOVE W15-START	 TO W10-EDTE W55-START.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Start date" TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA05.
             MOVE W20-DTE        TO W15-S-YMD.
	     MOVE W15-END	 TO W10-EDTE W55-END.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "End date"	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA05.
             MOVE W20-DTE        TO W15-E-YMD.
	   IF W15-E-YMD < W15-S-YMD
	       MOVE "End date is before start date"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA05.
	     PERFORM RESTORE-SCREEN-3.
	     MOVE W15-START	 TO W22-DTE1.
             MOVE W15-END        TO W22-DTE2.

       BA06.
	     DISPLAY "Sales Ledger Required:" AT 0406.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F6" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     " Sales Ledger Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     MOVE SPACES	 TO W15-LEDG.

       BA08.
015900	     ACCEPT S08.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
		     GO TO BA999
                 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		     GO TO BA06
		 WHEN F6-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		     GO TO BA08
		 WHEN OTHER
                     CALL X"E5"
		     GO TO BA08
               END-EVALUATE
015990	       GO TO BA08.
009080	   IF WS-LEDG = SPACES
	       GO TO BA999.
009330	     MOVE WS-LEDG	 TO DPT-CODE.
	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
016060	       GO TO BA06.
027330       PERFORM READ-DEPART THRU READ-DEPART-EXIT.
027370	     DISPLAY DPT-DESC AT 0435
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY WS-BLNK78 AT 0902
		     WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1.
	     DISPLAY WS-BLNK78 AT 1102
		     WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1.
	     DISPLAY "Extracting details requested" AT 0912.
	     DISPLAY "Reading:" AT 1112.
	     MOVE W15-S-YMD	 TO CRD-DTE.
	     MOVE SPACES	 TO CRD-EXT-CODE.
	     PERFORM START-AT-CRDX-DSTK THRU READ-CARDEX-EXIT.
000880	   IF WS-F-ERROR = 43
	       GO TO BA20.
	     MOVE SPACES	 TO SAL-LEDG
				    SAL-CODE.

       BA10.
000840	     PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       GO TO BA20.
	   IF CRD-DTE > W15-E-YMD
	       GO TO BA20.
	     DISPLAY CRD-EXT-CODE AT 1121
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
	     MOVE CRD-EXT-CODE	 TO SAL-CODE STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "No Stock Details for this Code"
				 TO STK-DESC
	       MOVE "XXXX"	 TO STK-LDG.
	   IF NOT (STK-LDG = W15-LEDG)
	       GO TO BA10.
	     MOVE W15-LEDG	 TO SAL-LEDG.
	     PERFORM READ-SALES THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       MOVE STK-DESC	 TO SAL-DESC
	       MOVE CRD-QNT	 TO SAL-QNT
	       MOVE ZERO	 TO SAL-COST SAL-SELL SAL-PROFIT
	       IF CRD-QNT < ZERO
		   COMPUTE SAL-COST = SAL-COST - (CRD-QNT * CRD-COST)
		   COMPUTE SAL-SELL = SAL-SELL - (CRD-QNT * CRD-SELL)
	       ELSE
		   COMPUTE SAL-COST = SAL-COST + (CRD-QNT * CRD-COST)
		   COMPUTE SAL-SELL = SAL-SELL + (CRD-QNT * CRD-SELL)
	       END-IF
	       SUBTRACT SAL-COST FROM SAL-SELL
				 GIVING SAL-PROFIT
	       PERFORM WRITE-SALES THRU WRITE-SALES-EXIT
	       GO TO BA10.

       BA15.
	     ADD CRD-QNT	 TO SAL-QNT.
	   IF CRD-QNT < ZERO
	       COMPUTE SAL-COST = SAL-COST - (CRD-QNT * CRD-COST)
	       COMPUTE SAL-SELL = SAL-SELL - (CRD-QNT * CRD-SELL)
	   ELSE
	       COMPUTE SAL-COST = SAL-COST + (CRD-QNT * CRD-COST)
	       COMPUTE SAL-SELL = SAL-SELL + (CRD-QNT * CRD-SELL).
	     SUBTRACT SAL-COST FROM SAL-SELL
				 GIVING SAL-PROFIT.
	     PERFORM REWRITE-SALES THRU WRITE-SALES-EXIT.
	     GO TO BA10.

       BA20.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
006630       DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 31		 TO SHADE-COL.
	     MOVE 26		 TO SHADE-WIDTH.
	     MOVE 9		 TO SHADE-LINES.
	     DISPLAY S-MENU.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.

       BA25.
	     MOVE 5		 TO TOP-ROW.
	     MOVE 11		 TO BOTTOM-ROW.
	     MOVE 4		 TO SCREEN-LIN.
	     MOVE 31		 TO SCREEN-COL.
	     MOVE 24		 TO STRING-LENGTH.
	     PERFORM SAVE-ATTR.

       BA30.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

006660 BA35.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY	 PERFORM MOVE-BLOCK-UP
		 WHEN DOWN-KEY	 PERFORM MOVE-BLOCK-DOWN
		 WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
				 IF MOUSE-ENTER = "Y"
				     MOVE "N"	 TO MOUSE-ENTER
				     GO TO BA40
				 END-IF
				 IF MOUSE-Y = 24
				     IF MOUSE-X = 5
					 MOVE 5  TO KEY-CODE-1
					 GO TO BA35
				     ELSE
				     IF MOUSE-X = 10
					 MOVE 6  TO KEY-CODE-1
					 GO TO BA35
				     ELSE
				     IF MOUSE-X > 36 AND < 44
					 MOVE 0  TO KEY-CODE-1
					 GO TO BA35
				     ELSE
				     IF MOUSE-X > 54 AND < 60
					 MOVE 0  TO KEY-CODE-1
					 MOVE 1  TO KEY-TYPE
					 GO TO BA35
				     END-IF
				 END-IF
		 WHEN ENTER-KEY  GO TO BA40
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA30
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE "0"	 TO WS-OPTION
		     GO TO BA50
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BA30
	   ELSE
	   IF DATA-8BIT
	       MOVE KEY-CODE-1X  TO WS-OPTION
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
	       IF WS-OPTION < "0" OR > "6"
		   CALL X"E5"
		   GO TO BA30
	       END-IF
	       GO TO BA45
	   ELSE
	       CALL X"E5"
	       GO TO BA30.

       BA40.
	   IF SCREEN-LIN < 5
	       CALL X"E5"
	       GO TO BA30
	   ELSE
	   IF SCREEN-LIN = 5
	       MOVE "1"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 6
	       MOVE "2"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 7
	       MOVE "3"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 8
	       MOVE "4"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 9
	       MOVE "5"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 10
	       MOVE "6"		 TO WS-OPTION
	   ELSE
	   IF SCREEN-LIN = 11
	       MOVE "0"		 TO WS-OPTION.
	   GO TO BA50.

       BA45.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 11		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "1"
	       MOVE 5		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "2"
	       MOVE 6		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "3"
	       MOVE 7		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "4"
	       MOVE 8		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "5"
	       MOVE 9		 TO SCREEN-LIN
	   ELSE
	   IF WS-OPTION = "6"
	       MOVE 10		 TO SCREEN-LIN.
	     PERFORM SAVE-ATTR.
	     PERFORM MARK-BLOCK.

       BA50.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
006730	   IF WS-OPTION = "0"
006740	       GO TO BA999.
006750     IF WS-OPTION = "1"
006760	       PERFORM BB000
006770     ELSE
006780     IF WS-OPTION = "2"
006790	       PERFORM BC000
006800     ELSE
006810     IF WS-OPTION = "3"
006820	       PERFORM BD000
006800     ELSE
006810	   IF WS-OPTION = "4"
006820	       PERFORM JA000
006800     ELSE
006810	   IF WS-OPTION = "5"
006820	       PERFORM KA000
006800     ELSE
006810	   IF WS-OPTION = "6"
	       CLOSE DPTSALES
	       OPEN OUTPUT DPTSALES
	       CLOSE DPTSALES
	       OPEN I-O DPTSALES
006820	       GO TO BA0.
007060	     GO TO BA20.

021720 BA999.
021730       EXIT.

       BB000	       SECTION 50.
       BB00.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     DISPLAY S06.
	     DISPLAY " (Quantity Sequence)   " AT 0457
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     MOVE "999999999"	 TO SAL-QUANT.
	     PERFORM START-AT-SALES-QNT THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       MOVE "No data on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BB999.

       BB05.
	     MOVE 0606		 TO CPOS.

       BB10.
	   IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
	       GO TO BB12.
	     PERFORM READ-SALES-PREV THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       DISPLAY "End - Press ANY key" AT 2512
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT
	       CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
	       GO TO BB999.

       BB12.
	     MOVE SAL-CODE	 TO W30-CODE.
	     MOVE SAL-DESC	 TO W30-DESC.
	     MOVE SAL-QNT	 TO W30-QUANT.
	     MOVE SAL-SELL	 TO W30-VALUE.
	     DISPLAY W30-DISPLAY AT CPOS
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF CLIN < 23
	       ADD 1		 TO CLIN
	       GO TO BB10.
001020	     DISPLAY "PgUp" AT 2502 WITH FOREGROUND-COLOR 6 HIGHLIGHT
					 BACKGROUND-COLOR 3
		     "/" WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "PgDn" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3
		     " - " WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
001170		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape." WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.

       BB15.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM CLEAR-L50.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BB999
		 WHEN PAGE-DOWN
001340		     GO TO BB20
		 WHEN PAGE-UP
001340		     GO TO BB30
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BB15.
	     CALL X"E5".
001280	     GO TO BB15.

       BB20.
001480	   IF CLIN < 23
001490	       GO TO BB15.
001500	     MOVE 0602		 TO CPOS.

       BB25.
	     DISPLAY WS-BLNK78 AT CPOS
		     WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1.
	   IF CLIN < 23
	       ADD 1		 TO CLIN
	       GO TO BB25.
001560	     GO TO BB05.

       BB30.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
	      MOVE "J"		  TO WS-OPTION.

       BB35.
	     PERFORM READ-SALES-NEXT THRU READ-SALES-EXIT.
	    IF WS-F-ERROR = 44
	       MOVE SPACE	 TO WS-OPTION
001500	       MOVE 0602	 TO CPOS
	       GO TO BB25.
	    IF WS-IXS1 < 36
		ADD 1		  TO WS-IXS1
	       GO TO BB35.
	     MOVE 0602		  TO CPOS.
	     GO TO BB25.

       BB999.
	     EXIT.

       BC000	       SECTION 51.
       BC00.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     DISPLAY S06.
	     DISPLAY " (Value Sold Sequence) " AT 0457
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     MOVE "99999999999"  TO SAL-TSELL.
	     PERFORM START-AT-SALES-TSELL THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       MOVE "No data on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC999.

       BC05.
	     MOVE 0606		 TO CPOS.

       BC10.
	   IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
	       GO TO BC12.
	     PERFORM READ-SALES-PREV THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       DISPLAY "End - Press ANY key" AT 2512
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT
	       CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
	       GO TO BC999.

       BC12.
	     MOVE SAL-CODE	 TO W30-CODE.
	     MOVE SAL-DESC	 TO W30-DESC.
	     MOVE SAL-QNT	 TO W30-QUANT.
	     MOVE SAL-SELL	 TO W30-VALUE.
	     DISPLAY W30-DISPLAY AT CPOS
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF CLIN < 23
	       ADD 1		 TO CLIN
	       GO TO BC10.
001020	     DISPLAY "PgUp" AT 2502 WITH FOREGROUND-COLOR 6 HIGHLIGHT
					 BACKGROUND-COLOR 3
		     "/" WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "PgDn" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3
		     " - " WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
001170		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape." WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.

       BC15.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM CLEAR-L50.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BC999
		 WHEN PAGE-DOWN
001340		     GO TO BC20
		 WHEN PAGE-UP
001340		     GO TO BC30
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BC15.
	     CALL X"E5".
001280	     GO TO BC15.

       BC20.
001480	   IF CLIN < 23
001490	       GO TO BC15.
001500	     MOVE 0602		 TO CPOS.

       BC25.
	     DISPLAY WS-BLNK78 AT CPOS
		     WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1.
	   IF CLIN < 23
	       ADD 1		 TO CLIN
	       GO TO BC25.
001560	     GO TO BC05.

       BC30.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
	      MOVE "J"		  TO WS-OPTION.

       BC35.
	     PERFORM READ-SALES-NEXT THRU READ-SALES-EXIT.
	    IF WS-F-ERROR = 44
	       MOVE SPACE	 TO WS-OPTION
001500	       MOVE 0602	 TO CPOS
	       GO TO BC25.
	    IF WS-IXS1 < 36
		ADD 1		  TO WS-IXS1
		GO TO BC35.
	     MOVE 0602		  TO CPOS.
	     GO TO BC25.

       BC999.
	     EXIT.

       BD000	       SECTION 52.
       BD00.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     DISPLAY S06.
	     DISPLAY " (G.Profit Sequence)   " AT 0457
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "Profit" AT 0569
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
	     MOVE "99999999999"	 TO SAL-GP.
	     PERFORM START-AT-SALES-GP THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       MOVE "No data on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BD999.

       BD05.
	     MOVE 0606		 TO CPOS.

       BD10.
	   IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
	       GO TO BD12.
	     PERFORM READ-SALES-PREV THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       DISPLAY "End - Press ANY key" AT 2512
			WITH BACKGROUND-COLOR 3
			     FOREGROUND-COLOR 6 HIGHLIGHT
	       CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS
	       GO TO BD999.

       BD12.
	     MOVE SAL-CODE	 TO W30-CODE.
	     MOVE SAL-DESC	 TO W30-DESC.
	     MOVE SAL-QNT	 TO W30-QUANT.
	     MOVE SAL-PROFIT	 TO W30-VALUE.
	     DISPLAY W30-DISPLAY AT CPOS
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	   IF CLIN < 23
	       ADD 1		 TO CLIN
	       GO TO BD10.
001020	     DISPLAY "PgUp" AT 2502 WITH FOREGROUND-COLOR 6 HIGHLIGHT
					 BACKGROUND-COLOR 3
		     "/" WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "PgDn" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 3
		     " - " WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
001170		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape." WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.

       BD15.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     PERFORM CLEAR-L50.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BD999
		 WHEN PAGE-DOWN
001340		     GO TO BD20
		 WHEN PAGE-UP
001340		     GO TO BD30
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO BD15.
	     CALL X"E5".
001280	     GO TO BD15.

       BD20.
001480	   IF CLIN < 23
001490	       GO TO BD15.
001500	     MOVE 0602		 TO CPOS.

       BD25.
	     DISPLAY WS-BLNK78 AT CPOS
		     WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1.
	   IF CLIN < 23
	       ADD 1		 TO CLIN
	       GO TO BD25.
001560	     GO TO BD05.

       BD30.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
	      MOVE "J"		  TO WS-OPTION.

       BD35.
	     PERFORM READ-SALES-NEXT THRU READ-SALES-EXIT.
	    IF WS-F-ERROR = 44
	       MOVE SPACE	 TO WS-OPTION
001500	       MOVE 0602	 TO CPOS
	       GO TO BD25.
	    IF WS-IXS1 < 36
		ADD 1		  TO WS-IXS1
	       GO TO BD35.
	     MOVE 0602		  TO CPOS.
	     GO TO BD25.

       BD999.
	     EXIT.

      /
008500 BE000		SECTION 53.
       BE00.
	     MOVE "!"		 TO DPT-CODE.
	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE 9		 TO WS-ERROR
	   ELSE
	       MOVE ZERO	 TO WS-ERROR.

       BE09.
	     EXIT.
      /
      *    ****    STOCK SALES & GROSS PROFIT ENQUIRY
      *
027010 JA000	       SECTION 54.
027020 JA00.
027030       DISPLAY CLR-SCREEN.
027050       DISPLAY "STOCK SALES & GP ENQUIRY" AT 0227
027060                WITH FOREGROUND-COLOR 15.
027070       DISPLAY "Are you sure Y/N?" AT 0620
027080                WITH FOREGROUND-COLOR 14.

027090 JA02.
027100	     ACCEPT WS-OPTION AT 0638
		    WITH FOREGROUND-COLOR 7 HIGHLIGHT
			 BACKGROUND-COLOR 5 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
027160	   IF NOT (WS-OPTION = "Y" OR "N")
027170         GO TO JA02.
027180     IF WS-OPTION = "N"
027190         GO TO JA999.
027200	     DISPLAY "Accumulating totals" AT 0812.
027210	     DISPLAY "Sales Ledger :" AT 1012.
027220	     MOVE " !"		 TO WS-LEDG.
027240	     MOVE 1		 TO WS-S6.

027250 JA03.
027260	     MOVE ZERO		 TO W200-DT (WS-S6) W200-CR (WS-S6).
	     MOVE SPACES	 TO W200-LEDG(WS-S6).
027270	   IF WS-S6 < 300
027280	       ADD 1		 TO WS-S6
027290         GO TO JA03.
027300	     MOVE ZERO		 TO W200-DTOT W200-CTOT
027310				    W200-COST W200-SELL W200-NET.
020820	     MOVE " !"		 TO DPT-CODE.
	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.

027320 JA05.
027330	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO JA10.
027340	     DISPLAY WS-LEDG AT 1027 WITH FOREGROUND-COLOR 11.
	   IF DPT-SALES-MTD < ZERO
	       MULTIPLY -1	 BY DPT-SALES-MTD
               COMPUTE DPT-SALES-YTD = DPT-SALES-YTD +
		      (DPT-SALES-MTD * 2).
027370	     DISPLAY DPT-DESC WITH FOREGROUND-COLOR 11 AT 1033.
027380       ADD DPT-SALES-MTD   TO W200-DT (1).
027390       ADD DPT-SALES-YTD   TO W200-DT (2).
027400       ADD DPT-RET-MTD     TO W200-DT (3).
027410       ADD DPT-RET-YTD     TO W200-DT (4).
027440       ADD DPT-COST-MTD    TO W200-DT (7).
027450       ADD DPT-COST-YTD    TO W200-DT (8).
027460       ADD DPT-CRET-MTD    TO W200-DT (9).
027470       ADD DPT-CRET-YTD    TO W200-DT (10).
027480       ADD DPT-DISC-MTD    TO W200-DT (5).
027490	     ADD DPT-DISC-YTD	 TO W200-DT (6).
	     GO TO JA05.

027510 JA10.
	     MOVE 0601		 TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
027570	     DISPLAY "M.T.D. Sales   :" AT 0412.
027580	     DISPLAY "       Returns :" AT 0512.
027590	     DISPLAY "-----------" AT 0630
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.
027600	     DISPLAY "       Gross   :" AT 0712.
027610	     DISPLAY " Less Discount :" AT 0812.
027620	     DISPLAY "-----------" AT 0930
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.
027630	     DISPLAY "Nett Sales     :" AT 1012.
027640	     DISPLAY "     Cost      :" AT 1112.
027650	     DISPLAY "Gross Profit   :" AT 1212.
027660	     DISPLAY "===========" AT 1330
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.
027670	     DISPLAY "Y.T.D. Sales   :" AT 1412.
027680	     DISPLAY "       Returns :" AT 1512.
027690	     DISPLAY "-----------" AT 1630
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.
027700	     DISPLAY "       Gross   :" AT 1712.
027710	     DISPLAY " Less Discount :" AT 1812.
027720	     DISPLAY "-----------" AT 1930
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.
027730	     DISPLAY "Nett Sales     :" AT 2012.
027740	     DISPLAY "     Cost      :" AT 2112.
027750	     DISPLAY "Gross Profit   :" AT 2212.
027760	     DISPLAY "===========" AT 2330
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.
027770	     DISPLAY "%" AT 1248
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.
027780	     DISPLAY "%" AT 2248
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 5.
027790       MOVE W200-DT (1) TO W40-AMOUNT W200-CTOT.
027800	     DISPLAY W40-AMOUNT AT 0430
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027810       MOVE W200-DT (3) TO W40-AMOUNT W200-DTOT.
027820	     DISPLAY W40-AMOUNT AT 0530
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027830       SUBTRACT W200-DTOT FROM W200-CTOT.
027840       MOVE W200-CTOT TO W40-AMOUNT.
027850	     DISPLAY W40-AMOUNT AT 0730
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027860       MOVE W200-DT (5) TO W40-AMOUNT W200-DTOT.
027870	     DISPLAY W40-AMOUNT AT 0830
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027880       SUBTRACT W200-DTOT FROM W200-CTOT.
027890       MOVE W200-CTOT TO W40-AMOUNT W200-SELL.
027900	     DISPLAY W40-AMOUNT AT 1030
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027910       SUBTRACT W200-DT (9) FROM W200-DT (7) GIVING W200-COST.
027920       MOVE W200-COST TO W40-AMOUNT.
027930	     DISPLAY W40-AMOUNT AT 1130
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027940       COMPUTE W200-DTOT = W200-SELL - W200-COST.
027950       MOVE W200-DTOT TO W40-AMOUNT.
027960	     DISPLAY W40-AMOUNT AT 1230
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027970       COMPUTE W200-COST = (W200-DTOT / W200-SELL) * 100.
027980       MOVE W200-COST TO W40-PERC.
027990	     DISPLAY W40-PERC AT 1241
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
028000       MOVE W200-DT (2) TO W40-AMOUNT W200-CTOT.
028010	     DISPLAY W40-AMOUNT AT 1430
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
028020       MOVE W200-DT (4) TO W40-AMOUNT W200-DTOT.
028030	     DISPLAY W40-AMOUNT AT 1530
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
028040       SUBTRACT W200-DTOT FROM W200-CTOT.
028050       MOVE W200-CTOT TO W40-AMOUNT.
028060	     DISPLAY W40-AMOUNT AT 1730
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
028070       MOVE W200-DT (6) TO W40-AMOUNT W200-DTOT.
028080	     DISPLAY W40-AMOUNT AT 1830
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
028090       SUBTRACT W200-DTOT FROM W200-CTOT.
028100       MOVE W200-CTOT TO W40-AMOUNT W200-SELL.
028110	     DISPLAY W40-AMOUNT AT 2030
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
028120       SUBTRACT W200-DT (10) FROM W200-DT (8) GIVING W200-COST.
028130       MOVE W200-COST TO W40-AMOUNT.
028140	     DISPLAY W40-AMOUNT AT 2130
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
028150       COMPUTE W200-DTOT = W200-SELL - W200-COST.
028160       MOVE W200-DTOT TO W40-AMOUNT.
028170	     DISPLAY W40-AMOUNT AT 2230
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
028180       COMPUTE W200-COST = (W200-DTOT / W200-SELL) * 100.
028190       MOVE W200-COST TO W40-PERC.
028200	     DISPLAY W40-PERC AT 2241
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
028210	     DISPLAY "Any KEY to continue" AT 2512
028220		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
028230	     ACCEPT WS-OPTION AT 2532
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
028240 JA999.
028250       EXIT.
      /
      *    ****    M O V E M E N T   R E P O R T
      *
       KA000	       SECTION 55.
       KA00.
027030       DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     MOVE ZERO		 TO W25-TOTAL W25-VALUE.
	     MOVE W15-START	 TO W55-START.
	     MOVE W15-END	 TO W55-END.
	     MOVE W15-LEDG	 TO SAL-LEDG
				    DPT-CODE.
027330       PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE "    !"	 TO SAL-CODE.
	     PERFORM START-AT-SALES-KEY THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       GO TO KA35.
	     MOVE ZERO		 TO WS-PAGE.

       KA30.
	     PERFORM READ-SALES-NEXT THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       IF WS-PAGE = ZERO
		   GO TO KA35
	       END-IF
	       GO TO KA40.
	   IF WS-PAGE = ZERO
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	       MOVE 8		 TO W02-PRN-TYPE
	       PERFORM OPEN-PRINTER
	       MOVE "P"		 TO WS-COMMAND
	       PERFORM KB000.
	     MOVE SAL-CODE	 TO SRP-EXT-ITM.
	     MOVE SAL-DESC	 TO SRP-DESC.
	     MOVE SAL-QNT	 TO SRP-QUANT.
	     MOVE SAL-COST	 TO SRP-COST.
	     MOVE SAL-SELL	 TO SRP-SELL.
	     COMPUTE SRP-EXTEND ROUNDED = SAL-SELL / SAL-QNT.
	     ADD SAL-COST	 TO W25-VALUE.
	     ADD SAL-SELL	 TO W25-TOTAL.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM KB000.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO KA45.
012990	     MOVE SPACES	 TO R-DETAIL.
	     GO TO KA30.

       KA35.
	     MOVE "No details for period selected"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
	     GO TO KA999.

       KA40.
007080	     MOVE SPACES	 TO R-DETAIL.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 3)
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM KB000.
	     MOVE "Sales Ledger   T O T A L"
				 TO SRP-DESC.
	     MOVE W25-VALUE	 TO SRP-COST.
	     MOVE W25-TOTAL	 TO SRP-SELL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "******   E N D   O F   R E P O R T   ******"
				 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****   6   L P I
      *
       KA45.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 6		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     PERFORM RESTORE-SCREEN-2.

021720 KA999.
021730       EXIT.


       KB000	       SECTION 55.
       KB00.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
007080	     MOVE SPACES	 TO R-DETAIL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
007090	     MOVE "PAGE:"	 TO R-PGE.
007100       MOVE "DATE:"        TO R-DATE.
007110       ADD 1               TO WS-PAGE.
007120       MOVE WS-PAGE        TO R-PNO.
007130       MOVE W12-TODAY      TO R-DTE.
007140       MOVE W95-COMP       TO R-CPY.
	     PERFORM CALL-PRINTUTL.
007160	     MOVE SPACES	 TO R-DETAIL.
012840	     MOVE W55-HEADING	 TO R-CPY.
	     PERFORM CALL-PRINTUTL.
012860	     MOVE SPACES	 TO R-DETAIL.
012870	     MOVE "SALES LEDGER:"
				 TO SRP-H1.
012880	     MOVE W15-LEDG	 TO SRP-NO.
012890	     MOVE DPT-DESC	 TO SRP-SMAN.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****   1 2	C P I
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
	     MOVE ALL "-"	 TO R-HL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
      *
      *    ****   1 2	C P I
      *
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
012920	     MOVE "Item Code"	 TO SRP-H4.
012920	     MOVE "Description"  TO SRP-H5.
	     MOVE "Quantity"	 TO SRP-H6.
	     MOVE "Total Cost"	 TO SRP-H7.
	     MOVE "Total Sell"	 TO SRP-H8.
	     MOVE "Avg Sell"	 TO SRP-H9.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE ALL "-"	 TO R-HL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012990	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.

       KB999.
	     EXIT.

      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *
041980 ZA000-INIT	       SECTION 90.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY W12-TODAY.
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
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "DPTSALES"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-DPTSALES.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.

       ZA01A.
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
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
053830	     OPEN OUTPUT DPTSALES.
053840	   IF RUNTIME-ERROR
053880         IF FLE-LIMIT
053890		   GO TO ZA49.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 44		 TO WS-F-ERROR
053920         PERFORM OPEN-ERROR.
	     CLOSE DPTSALES.
	     OPEN I-O DPTSALES.
044780	     GO TO ZA999-EXIT.

039780 ZA49.
039790       DISPLAY "Too many files OPEN" AT 0812
039800                WITH FOREGROUND-COLOR 14.
039810       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
039820       DISPLAY "Press any key to continue" AT 1212.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
039840	     GO TO ZA205.
039850 ZA50.
041850	   IF WS-F-ERROR = 43
041860	       MOVE "CARDEX file"
				 TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990                WS-FILE WITH FOREGROUND-COLOR 14.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.

       ZA55.
041220       MOVE 1              TO WS-S1.
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
021970	     MOVE WS-G4 	 TO WS-BCHR (8)  WS-BCHR (49)
021980                              WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "Supervisor�"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
042800 ZA200.
042810       DISPLAY "Files locked - Try later" AT 2312
042820                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
042830       DISPLAY "Press any key" AT 2512
042840		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
042850	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *
042900 ZB000-ERROR	     SECTION 91.

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
041510       GO TO DISPLAY-FILE-NAME.
       I-O-ERROR-EXIT.
             EXIT.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
041800     ELSE
041810     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
056620	   ELSE
056630     IF WS-F-ERROR = 22
056640         MOVE W02-STOCKF   TO WS-FILE
056650         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE W02-CARDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CRD-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 44
041860	       MOVE W02-DPTSALES TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
	       MOVE SAL-KEY	 TO WS-KEYX.
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
