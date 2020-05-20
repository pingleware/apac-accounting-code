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
      *     VERSION 8.06 - August 2002				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPSALES.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1996.

	   This program prints a list of stock items that have been sold
	   during a specified period for the sales ledger that has been
	   requested.

	   The program reads the cardex file to extract the sales and
	   accumulates the sales for each stock item and reduces the
	   sales for each credit note.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1996 - 2002
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.(Id No. 4412165050082).

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
000010	   SELECT MOVEMENT ASSIGN DISK
000040			   STATUS WS-STATUS
000050			   ACCESS DYNAMIC
000060			   ORGANIZATION INDEXED
000070			   RECORD SAL-KEY
000080			   ALTERNATE RECORD SAL-CODE DUPLICATES
000080			   ALTERNATE RECORD SAL-QUANT DUPLICATES
000090			   ALTERNATE RECORD SAL-TSELL DUPLICATES
000130			   ALTERNATE RECORD SAL-GP DUPLICATES.
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
000010 FD  MOVEMENT  IS EXTERNAL
		     LABEL RECORD STANDARD
000020		     VALUE OF FILE-ID W02-MOVEMENT.
000220 01  SAL-REC1.
000230	   03  SAL-KEY.
	       05  SAL-LEDG    PIC  X(04).
	       05  SAL-CODE.
		 07  SAL-ITEM  PIC  X(14).
		 07  SAL-ITM   PIC  X(03).
		 07  FILLER    PIC  X(01).
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
001670 77  WS-DEPKEY	   PIC	9(06)	 COMP-5.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001810 77  WS-KEYSTR       PIC  9(06)    COMP-5.
001820 77  WS-OPTION       PIC  X(01).
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC	9(02)	 VALUE 62.
003400 77  WS-PAGE	   PIC	9(04)	 COMP-5.
       77  WS-NUM	   PIC	ZZ9.
003550 77  WS-LEDG	   PIC	X(04).
001820 77  WS-ENQ	   PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-MONTH	   PIC	9(02).
001820 77  WS-ETYPE	   PIC	X(01).
001850 77  WS-ERROR        PIC  9(01) VALUE ZERO.
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
003300	       05  WS-MCHR  PIC  X(80) VALUE "³                ³
      -        "                      ³          ³         ³         ³".
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

002230 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "STP".
	   03  WS-PROG	       PIC  X(03) VALUE "SAL".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CARDEX.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000140 COPY STOCK.ID.

000010 COPY MOVEMT.ID.

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
	       05  FILLER	   PIC X(09).
	       05  R-CPY	   PIC X(40).
	       05  FILLER	   PIC X(05).
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
	       05  FILLER	   PIC	X(01).
	       05  SRP-NAME	   PIC	X(39).
	       05  FILLER	   PIC	X(01).
	       05  SRP-DATE	   PIC	Z9/99/9999.
	       05  FILLER	   PIC	X(01).
	       05  SRP-REF	   PIC	X(08).
	       05  FILLER	   PIC	X(30).

	   03  SRP-L4 REDEFINES W02-PRN-LINE.
	       05  SRP-H4	   PIC	X(19).
	       05  SRP-H5	   PIC	X(29).
	       05  SRP-H6	   PIC	X(11).
	       05  SRP-H7	   PIC	X(13).
	       05  SRP-H8	   PIC	X(15).
	       05  SRP-H9	   PIC	X(10).

	   03  SRP-L5 REDEFINES W02-PRN-LINE.
	       05  SRP-EXT-ITEM.
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

       COPY W10.STK.

       COPY W12.WS.

       COPY W15.WS.

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
	   03  W30-LEDG        PIC  X(03).
	   03  W30-CODE        PIC  X(15).
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
003340     03  W40-LEDG        PIC 9(03).
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
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "SALES STATISTICS OPTIONS".
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " ³".
	   03  LINE  6 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "1".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Stock Sales - Quantity ³".
	   03  LINE  7 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "2".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "             - Value    ³".
001470	   03  LINE  8 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "3".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "             - Profit   ³".
001470	   03  LINE  9 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "4".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Stock Gross Profit     ³".
001470	   03  LINE 10 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit                   ³".
	   03  LINE 11 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

004470 01  S04.
004480	   03  LINE 10 COLUMN 20 FOREGROUND-COLOR 14
004490                           VALUE "Busy loading stock departments".

009340 01  S05D.
	   03  LINE  4 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"³    Transactions for Period.    ³".
	   03  LINE  6 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  7 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"³   Starting Date :              ³".
	   03  LINE  8 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"³   Ending Date   :              ³".
	   03  LINE  9 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE 10 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"³Dates to be keyed in as ".
	   03	       COLUMN 49 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "DDMMCCYY".
	   03	       COLUMN 57 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "³".
	   03  LINE 11 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05A.
	   03  LINE  7 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-START AUTO.
	   03  LINE  8 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-END AUTO.

       01  S06.
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

       01  S07.
	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(04) USING WS-LEDG AUTO.

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
	       GO TO AA49.
	     PERFORM BA000.
	     CLOSE MOVEMENT.

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

000010 READ-MOVEMENT.
000020	     READ MOVEMENT	 WITH IGNORE LOCK
000030	       KEY IS SAL-KEY.
000040	   IF WS-STATUS = "00"
000050	       MOVE ZERO	 TO WS-F-ERROR
000060	       GO TO READ-MOVEMENT-EXIT.
000070	   IF WS-STATUS = "23"
000080	       MOVE 44		 TO WS-F-ERROR
000090	       GO TO READ-MOVEMENT-EXIT.
000100     IF WS-STAT1 = "2" OR "3" OR "4"
000110	       MOVE 44		 TO WS-F-ERROR
000120         PERFORM READ-ERROR.
000160	     GO TO READ-MOVEMENT-EXIT.

000380 READ-MOVEMENT-NEXT.
000390	     READ MOVEMENT NEXT	 WITH IGNORE LOCK.
000400     IF WS-STATUS = "00"
000050	       MOVE ZERO	 TO WS-F-ERROR
000060	       GO TO READ-MOVEMENT-EXIT.
000430	   IF (WS-STATUS = "23") OR
000440        (WS-STAT1 = "1")
000450	       MOVE 44		 TO WS-F-ERROR
000460	       GO TO READ-MOVEMENT-EXIT.
000470     IF WS-STAT1 = "2" OR "3" OR "4"
000480	       MOVE 44		 TO WS-F-ERROR
000490         PERFORM READ-ERROR.
000530	     GO TO READ-MOVEMENT-EXIT.

000380 READ-MOVEMENT-PREV.
000390	     READ MOVEMENT PREVIOUS WITH IGNORE LOCK.
000400     IF WS-STATUS = "00"
000050	       MOVE ZERO	 TO WS-F-ERROR
000060	       GO TO READ-MOVEMENT-EXIT.
000430	   IF (WS-STATUS = "23") OR
000440        (WS-STAT1 = "1")
000450	       MOVE 44		 TO WS-F-ERROR
000460	       GO TO READ-MOVEMENT-EXIT.
000470     IF WS-STAT1 = "2" OR "3" OR "4"
000480	       MOVE 44		 TO WS-F-ERROR
000490         PERFORM READ-ERROR.
000530	     GO TO READ-MOVEMENT-EXIT.

000750 START-AT-MOVEMENT-CODE.
000760	     START MOVEMENT
000770	       KEY >= SAL-KEY.
000780     IF WS-STATUS = "00"
000790         MOVE ZERO         TO WS-F-ERROR
000800	       GO TO READ-MOVEMENT-EXIT.
000810     IF WS-STATUS = "23"
000820	       MOVE 44		 TO WS-F-ERROR
000830	       GO TO READ-MOVEMENT-EXIT.
000840     IF WS-STAT1 = "2" OR "3" OR "4"
000850	       MOVE 44		 TO WS-F-ERROR
000860         PERFORM READ-ERROR.
000870	     GO TO READ-MOVEMENT-EXIT.

000890 START-AT-MOVEMENT-QNT.
000900	     START MOVEMENT
000910	       KEY <= SAL-QUANT.
000920     IF WS-STATUS = "00"
000930         MOVE ZERO         TO WS-F-ERROR
000940	       GO TO READ-MOVEMENT-EXIT.
000950     IF WS-STATUS = "23"
000960	       MOVE 44		 TO WS-F-ERROR
000970	       GO TO READ-MOVEMENT-EXIT.
000980     IF WS-STAT1 = "2" OR "3" OR "4"
000990	       MOVE 44		 TO WS-F-ERROR
001000         PERFORM READ-ERROR.
001010	     GO TO READ-MOVEMENT-EXIT.

001030 START-AT-MOVEMENT-TSELL.
001040	     START MOVEMENT
001050	       KEY <= SAL-TSELL.
001060     IF WS-STATUS = "00"
001070         MOVE ZERO         TO WS-F-ERROR
001080	       GO TO READ-MOVEMENT-EXIT.
001090     IF WS-STATUS = "23"
001100	       MOVE 44		 TO WS-F-ERROR
001110	       GO TO READ-MOVEMENT-EXIT.
001120     IF WS-STAT1 = "2" OR "3" OR "4"
001130	       MOVE 44		 TO WS-F-ERROR
001140         PERFORM READ-ERROR.
001150	     GO TO READ-MOVEMENT-EXIT.

001170 START-AT-MOVEMENT-GP.
001180	     START MOVEMENT
001190	       KEY <= SAL-GP.
001200     IF WS-STATUS = "00"
001210         MOVE ZERO         TO WS-F-ERROR
001220	       GO TO READ-MOVEMENT-EXIT.
001230     IF WS-STATUS = "23"
001240	       MOVE 44		 TO WS-F-ERROR
001250	       GO TO READ-MOVEMENT-EXIT.
001260     IF WS-STAT1 = "2" OR "3" OR "4"
001270	       MOVE 44		 TO WS-F-ERROR
001280         PERFORM READ-ERROR.

001580 READ-MOVEMENT-EXIT.
001590       EXIT.

      /
011350 AD000-WRITE             SECTION.

010000 REWRITE-MOVEMENT.
010010	     REWRITE SAL-REC1.
000030     IF WS-STAT1 NOT = "0"
010030	       MOVE 44		 TO WS-F-ERROR
010040         PERFORM WRITE-ERROR.
010050	     GO TO WRITE-MOVEMENT-EXIT.

010060 WRITE-MOVEMENT.
010070	     WRITE SAL-REC1.
000030     IF WS-STAT1 NOT = "0"
010090	       IF WS-STATUS = "22"
010100		   GO TO REWRITE-MOVEMENT
010110         ELSE
010120		   MOVE 44	 TO WS-F-ERROR
010130             PERFORM WRITE-ERROR.

010140 WRITE-MOVEMENT-EXIT.
010150       EXIT.


       COPY APAC.HLP.

      /
      *  ****  E N Q U I R Y   ( S A L E S   P E R F O R M A N C E )
      *
021000 BA000		       SECTION 50.
021010 BA0.
021020       DISPLAY CLR-SCREEN.
	     DISPLAY S05D.
	     MOVE W12-TODAY	 TO W15-START W15-END.
	     PERFORM SAVE-SCREEN-3.

       BA00.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 27		 TO SHADE-COL.
	     MOVE 30		 TO SHADE-WIDTH.
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
	     DISPLAY WS-BLNK78 AT 0902
		     WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1.
	     DISPLAY WS-BLNK78 AT 1102
		     WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1.
	     DISPLAY "Extracting details requested" AT 0912.
	     DISPLAY "Reading:" AT 1112.
	     MOVE W15-S-YMD	 TO CRD-DTE.
	     MOVE SPACES	 TO CRD-EXT-CODE.
	     PERFORM START-AT-CRDX-SDTE THRU READ-CARDEX-EXIT.
000880	   IF WS-F-ERROR = 43
	       MOVE "No data for time period selected"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA00.
	     MOVE SPACES	 TO SAL-CODE.
	     PERFORM SAVE-SCREEN-2.

       BA10.
000840	     PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       GO TO BA16.
	   IF CRD-DTE > W15-E-YMD
	       GO TO BA20.
	     DISPLAY CRD-EXT-CODE AT 1121
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
	   IF CRD-EXT-CODE = SAL-CODE
	       GO TO BA15.
	     MOVE CRD-EXT-CODE	 TO SAL-CODE STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "No Stock Details for this Code"
				 TO STK-DESC.
	     MOVE STK-LDG	 TO SAL-LEDG.
	     PERFORM READ-MOVEMENT THRU READ-MOVEMENT-EXIT.
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
	       PERFORM WRITE-MOVEMENT THRU WRITE-MOVEMENT-EXIT
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
	     PERFORM REWRITE-MOVEMENT THRU WRITE-MOVEMENT-EXIT.
	     GO TO BA10.

       BA16.
	     MOVE W15-S-YMD	 TO CRD-DTE.
	     MOVE SPACES	 TO CRD-EXT-CODE.
	     PERFORM START-AT-CRDX-SDTE THRU READ-CARDEX-EXIT.
000880	   IF WS-F-ERROR = 43
	       GO TO BA20.
	     GO TO BA10.

       BA20.
	     DISPLAY "Sales Ledger Required:" AT 1312.
	     ACCEPT S07 AT 1335.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO WS-LEDG
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA20
	       END-EVALUATE.
009080	   IF WS-LEDG = SPACES
	       GO TO BA999.
009330	     MOVE WS-LEDG	 TO DPT-CODE.
	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
009070	       GO TO BA20.
009340	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     DISPLAY DPT-DESC AT 1435
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
       BA25.
	     MOVE "'C'ontinue or 'R'ekey  [ ]"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     MOVE 16		 TO SLIN.
	     PERFORM OPT-SETUP THRU OPT-EXIT.
	   IF NOT (WS-OPTION = "R" OR "C")
	       GO TO BA25.
           IF WS-OPTION = "R"
	       GO TO BA20.
	     MOVE WS-LEDG	 TO SAL-LEDG.
	     MOVE "    !"	 TO SAL-CODE.
	     PERFORM START-AT-MOVEMENT-CODE THRU READ-MOVEMENT-EXIT.
	   IF WS-F-ERROR = 44
	       GO TO BA35.
	     MOVE ZERO		 TO WS-PAGE.

       BA30.
	     PERFORM READ-MOVEMENT-NEXT THRU READ-MOVEMENT-EXIT.
	   IF WS-F-ERROR = 44
	       IF WS-PAGE = ZERO
		   GO TO BA35
	       END-IF
	       GO TO BA40.
	   IF NOT (WS-LEDG = SAL-LEDG)
	       IF WS-PAGE = ZERO
		   GO TO BA35
	       END-IF
	       GO TO BA40.
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
	       PERFORM BB000.
	   IF WS-USE-ITM = "Y"
	       MOVE SAL-ITEM	 TO SRP-ITEM
	       MOVE "/" 	 TO SRP-SL
	       MOVE SAL-ITM	 TO SRP-ITM
	   ELSE
015280	       MOVE SAL-CODE	 TO SRP-EXT-ITEM.
	     MOVE SAL-DESC	 TO SRP-DESC.
	     MOVE SAL-QNT	 TO SRP-QUANT.
	     MOVE SAL-COST	 TO SRP-COST.
	     MOVE SAL-SELL	 TO SRP-SELL.
	     COMPUTE SRP-EXTEND ROUNDED = SAL-SELL / SAL-QNT.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM BB000.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO BA45.
012990	     MOVE SPACES	 TO R-DETAIL.
	     GO TO BA30.

       BA35.
	     MOVE "No details for period selected"
				 TO WS-ERR-STRING.
003260	     PERFORM ERROR-MESSAGE.
	     GO TO BA20.

       BA40.
	     MOVE "******   E N D   O F   R E P O R T   ******"
				 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****   6   L P I
      *
       BA45.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 6		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     PERFORM RESTORE-SCREEN-2.
	     GO TO BA20.

021720 BA999.
021730       EXIT.


       BB000	       SECTION 50.
       BB00.
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
012880	     MOVE WS-LEDG	 TO SRP-NO.
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

       BB999.
	     EXIT.

      /
008500 BE000		SECTION 51.
008510 BE00.
	     MOVE "!"		 TO DPT-CODE.
	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE 9		 TO WS-ERROR
	   ELSE
	       MOVE ZERO	 TO WS-ERROR.

       BE09.
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
	     MOVE "MOVEMT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-MOVEMENT.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
042080	     MOVE 3		 TO WS-PARKEY.
042090       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.

042170 ZA02.
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
      *       DISPLAY "Waiting for printer response" AT 1412
      * 	       WITH FOREGROUND-COLOR 14 BLINK.
042780*       OPEN OUTPUT PRNREP.
053830	     OPEN OUTPUT MOVEMENT.
053840	   IF RUNTIME-ERROR
053880         IF FLE-LIMIT
053890		   GO TO ZA49.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 44		 TO WS-F-ERROR
053920         PERFORM OPEN-ERROR.
	     CLOSE MOVEMENT.
	     OPEN I-O MOVEMENT.
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
	       MOVE "SupervisorÄ"  TO WS-WRKHD
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
041870         MOVE ZERO         TO WS-KEY
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
041860	       MOVE W02-MOVEMENT TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY.
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
