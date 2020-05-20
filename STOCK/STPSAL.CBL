      $SET LINKCOUNT"576"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	 ******     ****    **	       *
      *   **	**     **     **    **	**    **   **  **   **	       *
      *   **	       **     **    **	**	  **	**  **	       *
      *    ******      **     *******	 ******	  ********  **	       *
      * 	**     **     **	      **  **	**  **	       *
      *   **	**     **     **	**    **  **	**  **	       *
      *    ******      **     **	 ******	  **	**  ********   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S A L E S	T R A C K I N G   P R O G R A M 	       *
      *                                                                *
      *     VERSION 8.13.07 - January 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPSAL.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1989.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1989 - 2008
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

000120 COPY DEPART.SL.

000250 COPY INVOIC.SL.

000120 COPY PARAM.SL.

000140 COPY STOCK.SL.

000140 COPY STKMEM.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CARDEX.FDE.

000120 COPY CONTROL.FDE.

000420 COPY DEPART.FDE.

000540 COPY INVOIC.FDE.

000290 COPY PARAM.FDE.

000540 COPY STOCK.FDE.

       COPY STKMEM.FDE.

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
001630 77  WS-S7           PIC  9(04)    COMP-5.
001640 77  WS-S8           PIC  9(04)    COMP-5.
001650 77  WS-S9           PIC  9(04)    COMP-5.
002850 77  WS-IXS1         PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS	   PIC	9(06)	 COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
001670 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001810 77  WS-KEYSTR       PIC  9(06)    COMP-5.
001820 77  WS-OPTION       PIC  X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
001820 77  WS-FORMAT	   PIC	X(01).
003400 77  WS-PAGE	   PIC	9(04)	 COMP-5 VALUE ZERO.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
001820 77  WS-ITMHDG	   PIC	X(01).
       77  WS-MONTH	   PIC	9(02).
001820 77  WS-ETYPE	   PIC	X(01).
001330 77  WS-DESC-H1	    PIC	 X(14) VALUE "Description".
       77  WS-DESC-H2	    PIC	 X(14) VALUE "Description 2".
       77  WS-RT-HD	    PIC	 X(10) VALUE "-Retail   ".
       77  WS-WS-HD	    PIC	 X(10) VALUE "-Wholesale".
       77  WS-CS-HD	    PIC	 X(10) VALUE "-Cash sale".
001850 77  WS-ERROR	   PIC	9(01) VALUE ZERO.
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
       77  PRG-NAME	   PIC	X(12) VALUE "STP\STPLOOK".
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
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "ENQ".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CARDEX.ID.

       COPY CONTROL.ID.

000140 COPY DEPART.ID.

000140 COPY INVOIC.ID.

       COPY PARAM.ID.

000140 COPY STOCK.ID.

000140 COPY STKMEM.ID.

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
	       05  FILLER	   PIC X(06).
	       05  R-HEAD.
		   07  FILLER	   PIC	X(03).
		   07  R-CPY	   PIC	X(42).
	       05  FILLER	   PIC X(03).
	       05  R-DATE	   PIC X(06).
	       05  R-DTE	   PIC Z9/99/9999.

	   03  SRP-L1 REDEFINES W02-PRN-LINE.
	       05  SRP-H1	   PIC	X(06).
	       05  SRP-EXT-ITEM.
		   07  SRP-ITEM	   PIC	X(14).
		   07  SRP-SL	   PIC	X(01).
		   07  SRP-ITM	   PIC	X(03).
	       05  FILLER	   PIC	X(01).
	       05  SRP-DESC	   PIC	X(30).
	       05  FILLER	   PIC	X(25).

	   03  SRP-L2 REDEFINES W02-PRN-LINE.
	       05  SRP-H2	   PIC	X(51).
	       05  SRP-H3	   PIC	X(29).

	   03  SRP-L4 REDEFINES W02-PRN-LINE.
	       05  SRP-H4	   PIC	X(11).
	       05  SRP-H5	   PIC	X(07).
	       05  SRP-H6	   PIC	X(07).
	       05  SRP-H7	   PIC	X(32).
	       05  SRP-H8	   PIC	X(13).
	       05  SRP-H9	   PIC	X(13).
	       05  SRP-H10	   PIC	X(13).

	   03  SRP-L5 REDEFINES W02-PRN-LINE.
	       05  SRP-REF	   PIC	X(08).
	       05  FILLER	   PIC	X(01).
	       05  SRP-DATE-2	   PIC	Z9/99/99.
	       05  FILLER	   PIC	X(01).
	       05  SRP-ACNO	   PIC	X(06).
	       05  FILLER	   PIC	X(01).
	       05  SRP-NAME	   PIC	X(30).
	       05  FILLER	   PIC	X(01).
	       05  SRP-COST	   PIC	Z(07)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  SRP-SELL	   PIC	Z(07)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  SRP-QUANT	   PIC	Z(05)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  SRP-TYPE	   PIC	X(01).

       COPY W05.VAT.

       COPY W10.STK.

       COPY W12.WS.

       01  W15-DATES.
004120	   03  W15-START       PIC 9(08).
004130     03  W15-SDTE-DMY REDEFINES W15-START.
004140         05  W15-SDAY    PIC 9(02).
004150         05  W15-SMNTH   PIC 9(02).
	       05  W15-SCENT   PIC 9(02).
004160         05  W15-SYEAR   PIC 9(02).
004170	   03  W15-S-YMD       PIC 9(08).
004180     03  W15-STRT-YMD REDEFINES W15-S-YMD.
	       05  W15-SCC     PIC 9(02).
004190         05  W15-SYY     PIC 9(02).
004200         05  W15-SMM     PIC 9(02).
004210         05  W15-SDD     PIC 9(02).
004120	   03  W15-END	       PIC 9(08).
004130     03  W15-EDTE-DMY REDEFINES W15-END.
004140         05  W15-EDAY    PIC 9(02).
004150         05  W15-EMNTH   PIC 9(02).
	       05  W15-ECENT   PIC 9(02).
004160         05  W15-EYEAR   PIC 9(02).
004170	   03  W15-E-YMD       PIC 9(08).
004180     03  W15-END-YMD REDEFINES W15-E-YMD.
	       05  W15-ECC     PIC 9(02).
004190         05  W15-EYY     PIC 9(02).
004200         05  W15-EMM     PIC 9(02).
004210         05  W15-EDD     PIC 9(02).

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

       01  W16-DATES-6.
	   03  W16-DATE.
	       05  W16-DD      PIC  9(02).
	       05  W16-MM      PIC  9(02).
	       05  W16-YY      PIC  9(02).
	   03  W16-DTE REDEFINES W16-DATE
			       PIC  9(06).
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

002920 01  W40-COMPANY.
002930     03  W40-NAME1.
002940         05  W40-NAME.
002950             07  W40-ENTRY PIC X(08).
002960             07  FILLER  PIC X(32).
002970         05  FILLER      PIC X(32).
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

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
	   03  W75-S2	      PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
005860	   03  W75-DNO	      PIC X(06) OCCURS 18.
	   03  W75-RNO	      PIC X(08) OCCURS 16.
	   03  W75-DATE       PIC 9(06) OCCURS 16.
	   03  W75-SM	      PIC 9(02) OCCURS 16.

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
004460         05  W95-DTE     PIC Z9/99/99.
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

       01  W200-MEMO.
           03  W200-MEM-FLDS. 
               05  W200-MEM1   PIC  X(60).
               05  W200-MEM2   PIC  X(60).
               05  W200-MEM3   PIC  X(60).
               05  W200-MEM4   PIC  X(60).
               05  W200-MEM5   PIC  X(60).
               05  W200-MEM6   PIC  X(60).
               05  W200-MEM7   PIC  X(60).
               05  W200-MEM8   PIC  X(60).
               05  W200-MEM9   PIC  X(60).
               05  W200-MEM10  PIC  X(60).
               05  W200-MEM11  PIC  X(60).
               05  W200-MEM12  PIC  X(60).
           03  W200-MEM-FOC REDEFINES W200-MEM-FLDS.
               05  W200-MEM    PIC  X(60) OCCURS 12.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
004580 SCREEN SECTION.

       COPY BLANK.CRT.

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

009340 01  S10D.
009350	   03  LINE  2 COLUMN 21 FOREGROUND-COLOR 15 VALUE
009360		       "SALES TRACKING MOVEMENT/REFERENCE LOOKUP".
	   03  LINE  3 COLUMN  3 VALUE "Item Code.".

007910 01  S36.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(14) USING W10-ITEM AUTO.

007910 01  S36-ITM.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(14)/X(03)
				  USING W10-EXT-ITEM AUTO.

007910 01  S36-EXT.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(18) USING W10-EXT-ITEM AUTO.

005530 01  S36-3.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(03) USING W10-I4 AUTO.

005530 01  S36-4.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(04) USING W10-I4 AUTO.

005530 01  S36-5.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(05) USING W10-I5 AUTO.

005530 01  S36-6.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(06) USING W10-I6 AUTO.

005530 01  S36-7.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(07) USING W10-I7 AUTO.

005530 01  S36-8.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(08) USING W10-I8 AUTO.

005530 01  S36-9.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(09) USING W10-I9 AUTO.

005530 01  S36-10.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(10) USING W10-I10 AUTO.

005530 01  S36-11.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(11) USING W10-I11 AUTO.

005530 01  S36-12.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(12) USING W10-I12 AUTO.

005530 01  S36-13.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(13) USING W10-I13 AUTO.

005530 01  S36-14.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
005550				 PIC Z(14) USING W10-I14 AUTO.

       COPY STKLUP.CRT.

009340 01  S-MOV.
009390     03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE 
009400	       "Ref. No.   Date   Ac/No  SP Cost Price Sell.Price Quanti
009410-        "ty Customer Name      ".
009160	   03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009160	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÑÍÍÑÍÍÍÍ
      -        "ÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
009160	   03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
009170	   03	       COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÏÍÍÏÍÍÍÍ
      -        "ÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".

009530 01  SA-ACT.
009540	   03  LINE  3 COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009550				 PIC  X(06) USING W80-ACNO AUTO.

       01  S-DEB-MEM.
           03  LINE  7 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
      -    "".

       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7 VALUE 
           "   M E M O / R E M A R K S / C O R R E S P O N D E N C E    
      -    "".
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(01) USING WS-G12.
       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR 0
                                 BACKGROUND-COLOR 7
                                 PIC X(60) USING W200-MEM12 AUTO.

004870 COPY S99.CRT.

       COPY OPT.CRT.

       COPY ERROR.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
      *	   *************************************************************
      *      To control Sales Tracking security it is necessary to use
      *      two of the levels in the System.
      * 	A Stock level of 3 will allow sales tracking.
      * 	Where the stock level is less than 3, then by giving
      * 	the user a sales level of 3, sales tracking will be
      * 	enabled.
      *    *************************************************************
003240	   IF (LS0-STLEV < 3) AND (LS0-SALEV < 3)
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
010270	     PERFORM ZA000-INIT.
	     MOVE "S"		 TO WS-OPTION.

       AA05.
	     MOVE "'S'creen,'R'eport format or 'E'xit  [S]"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "S" OR "R" OR "E")
	       GO TO AA05.
	   IF WS-OPTION = "E"
	       GO TO AA49.
	     MOVE WS-OPTION	 TO WS-FORMAT.
	     PERFORM BA000.

       AA49.
013150       EXIT PROGRAM.

       COPY AA50.LUP.

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

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³			  OPT-MESSAGE			       ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'. 			       ³
      *    ³							       ³
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³							       ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
      *    ³   PERFORM OPT-MESSAGE.				       ³
      *    ³							       ³
      *    ³ This would be displayed as:			       ³
      *    ³	ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿     ³
      *    ³	³	   Print transactions (Y/N) [ ] 	 ³°°   ³
      *    ³	ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°   ³
      *    ³	  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°   ³
      *    ³							       ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³							       ³
      *    ³ To display the request message higher or lower (default   ³
      *    ³ is line 13) move the line number to be used to SLIN and   ³
      *    ³ then PEFORM OPT-SETUP THRU OPT-EXIT.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY OPTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

      *    *************************************************************
      *    ****        D A T E	 C H E C K   R O U T I N E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Perform this routine with the date in W10-DATE (PIC      ³
      *    ³  99/99/9999) with the format as DD/MM/CCYY 	       ³
      *    ³							       ³
      *    ³  On exiting - WS-ERROR will be zero (0) if the date is    ³
      *    ³  correct or one (1) if the date is incorrect.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY DATE.CHK.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP3.

       COPY AA300.STK.

      /    *************************************************************
      *    ****    READ FILES ROUTINES
      *    *************************************************************
011650 AC000              SECTION.

       COPY CARDEX.RD.

011670 COPY CONTROL.RD.

000140 COPY DEPART.RD.

011750 COPY PARAM.RD.

       COPY INVOIC.RD.

       COPY STOCK.RD.

000140 COPY STKMEM.RD.

      /    *************************************************************
      *    ****	  E N Q U I R Y	  ( S A L E S	T R A C K I N G )
      *    *************************************************************
021000 BA000		       SECTION.
       BA00.
021020	     DISPLAY CLR-SCREEN.
	     PERFORM SAVE-SCREEN-3.
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
		     GO TO BA49
		 WHEN OTHER	 CALL X"E5"
		     GO TO BA05
	       END-EVALUATE.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BA05.
	     DISPLAY CLEAR-L25.
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

       BA10.
	     MOVE SPACES	 TO W10-EXT-ITEM.
	   IF W10-STCK = "N"
	       IF W10-SLNGTH = 3
		   MOVE ZERO	 TO W10-I3
	       ELSE
	       IF W10-SLNGTH = 4
		   MOVE ZERO	 TO W10-I4
	       ELSE
	       IF W10-SLNGTH = 5
		   MOVE ZERO	 TO W10-I5
	       ELSE
	       IF W10-SLNGTH = 6
		   MOVE ZERO	 TO W10-I6
	       ELSE
	       IF W10-SLNGTH = 7
		   MOVE ZERO	 TO W10-I7
	       ELSE
	       IF W10-SLNGTH = 8
		   MOVE ZERO	 TO W10-I8
	       ELSE
	       IF W10-SLNGTH = 9
		   MOVE ZERO	 TO W10-I9
	       ELSE
	       IF W10-SLNGTH = 10
		   MOVE ZERO	 TO W10-I10
	       ELSE
	       IF W10-SLNGTH = 11
		   MOVE ZERO	 TO W10-I11
	       ELSE
	       IF W10-SLNGTH = 12
		   MOVE ZERO	 TO W10-I12
	       ELSE
	       IF W10-SLNGTH = 13
		   MOVE ZERO	 TO W10-I13
	       ELSE
		   MOVE ZERO	 TO W10-I14.
021010 BA15.
021020       DISPLAY CLR-SCREEN.
	     DISPLAY WS-BLNK78 AT 0302
		     WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1.
	     DISPLAY S10D.

       BA20.
	     MOVE W12-MONTH	 TO WS-MONTH.

       BA25.
020960	     PERFORM AA300.
      *    *************************************************************
      *    ****    G E T   S T O C K   C O D E
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-EXT-STK = "Y"
		   ACCEPT S36-EXT AT 0314
	       ELSE
	       IF WS-USE-ITM = "Y"
		   ACCEPT S36-ITM AT 0314
	       ELSE
021070		   ACCEPT S36 AT 0314
	       END-IF
	   ELSE
	   IF W10-SLNGTH = 3
	       ACCEPT S36-3 AT 0314
	   ELSE
	   IF W10-SLNGTH = 4
	       ACCEPT S36-4 AT 0314
	   ELSE
	   IF W10-SLNGTH = 5
	       ACCEPT S36-5 AT 0314
	   ELSE
	   IF W10-SLNGTH = 6
	       ACCEPT S36-6 AT 0314
	   ELSE
	   IF W10-SLNGTH = 7
	       ACCEPT S36-7 AT 0314
	   ELSE
	   IF W10-SLNGTH = 8
	       ACCEPT S36-8 AT 0314
	   ELSE
	   IF W10-SLNGTH = 9
	       ACCEPT S36-9 AT 0314
	   ELSE
	   IF W10-SLNGTH = 10
	       ACCEPT S36-10 AT 0314
	   ELSE
	   IF W10-SLNGTH = 11
	       ACCEPT S36-11 AT 0314
	   ELSE
	   IF W10-SLNGTH = 12
	       ACCEPT S36-12 AT 0314
	   ELSE
	   IF W10-SLNGTH = 13
	       ACCEPT S36-13 AT 0314
	   ELSE
	   IF W10-SLNGTH = 14
	       ACCEPT S36-14 AT 0314.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-EXT-ITEM
018870		     DISPLAY CLEAR-L25
		     GO TO BA49
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "I"	 TO W10-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO W10-ETYPE
		 WHEN F4-KEY
		     MOVE "D"	 TO W10-ETYPE
		 WHEN F5-KEY
		     MOVE "2"	 TO W10-ETYPE
		 WHEN F6-KEY
		     MOVE "X"	 TO W10-ETYPE
		 WHEN F9-KEY
		     MOVE "W"	 TO W10-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BA25
	       END-EVALUATE
016560	       PERFORM AA50-LOOKUP
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	       IF W10-STCK = "A"
		   IF WS-EXT-STK = "Y"
		       DISPLAY S36-EXT AT 0314
		   ELSE
		   IF WS-USE-ITM = "Y"
		       DISPLAY S36-ITM AT 0314
		   ELSE
		       DISPLAY S36 AT 0314
		   END-IF
	       ELSE
		   DISPLAY S36 AT 0314.

       BA30.
	     DISPLAY CLEAR-L25.
021040	   IF W10-EXT-ITEM = SPACES
021060	       GO TO BA25.

       BA35.
021070       PERFORM CA000.
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	   IF W10-STCK = "A"
	       IF WS-EXT-STK = "Y"
		   DISPLAY S36-EXT AT 0314
	       ELSE
	       IF WS-USE-ITM = "Y"
		   DISPLAY S36-ITM AT 0314
	       ELSE
018630		   DISPLAY S36 AT 0314
	       END-IF
	   ELSE
	       DISPLAY S36 AT 0314.
021080	   IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO W10-ETYPE
018620	       PERFORM AA50-LOOKUP
      *    *************************************************************
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *    *************************************************************
	       IF W10-STCK = "A"
		   IF WS-EXT-STK = "Y"
		       DISPLAY S36-EXT AT 0314
		   ELSE
		   IF WS-USE-ITM = "Y"
		       DISPLAY S36-ITM AT 0314
		   ELSE
		       DISPLAY S36 AT 0314
		   END-IF
	       ELSE
		   DISPLAY S36 AT 0314
	       END-IF
018640	       IF W10-EXT-ITEM = SPACES
018720		   GO TO BA25
               ELSE
		   GO TO BA35.
	     DISPLAY "Description" AT 0334.
	     DISPLAY STK-DESC AT 0346
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
	       GO TO BA25.

       BA40.
	   IF WS-FORMAT = "R"
	       PERFORM MOVEMENT-REPORT
	       IF WS-OPTION = "X"
		   GO TO BA55
	       END-IF
	   ELSE
	       PERFORM MOVEMENT-LOOKUP.
	     MOVE SPACES	 TO W80-ACNO.
	     MOVE SPACE 	 TO WS-OPTION.

       BA45.
	     MOVE "Change 'D'ates,'I'tem code or 'E'nd  [ ]"
				 TO WS-ERR-MES.
033910	     PERFORM OPT-MESSAGE.
014920	   IF NOT (WS-OPTION = "D" OR "E" OR "I")
014930	       GO TO BA45.
	   IF WS-OPTION = "I"
	       GO TO BA15
	   ELSE
	   IF WS-OPTION = "D"
	       GO TO BA00.

       BA49.
	   IF NOT (WS-FORMAT = "R")
	       GO TO BA999.

       BA50.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "******   E N D   O F   R E P O R T   ******"
				 TO R-DETAIL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

       BA55.
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 6		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
	     MOVE "C"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.

021720 BA999.
021730       EXIT.

      /    *************************************************************
      *    ****    M O V E M E N T   L O O K U P
      *    *************************************************************
000010 MOVEMENT-LOOKUP		 SECTION 50.

000030 MOVE-LUP-SCREEN.
000050	     DISPLAY S-MOV.
	     INITIALIZE CRD-REC1.
	     MOVE W10-EXT-ITEM	 TO CRD-EXT-CODE.
	     MOVE W15-S-YMD	 TO CRD-DTE.

       MOVE-LUP-CLR-SCREEN.
000060       MOVE 0601           TO CPOS.
000080	     PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.

       MOVE-LUP-CONTINUE.
000180	     MOVE 6		 TO CLIN.
000200       MOVE 1              TO W75-S W75-S1.

       MOVE-LUP-TAB-LOOP.
	     MOVE SPACES	 TO W75-DNO (W75-S1)
				    W75-RNO (W75-S1).
	     MOVE ZERO		 TO W75-DATE (W75-S1)
				    W75-SM (W75-S1).
	   IF W75-S1 < 16
               ADD 1             TO W75-S1
	       GO TO MOVE-LUP-TAB-LOOP.
             MOVE ZERO           TO W75-S1.

       MOVE-LUP-START-CRDX.
	     MOVE SPACES	 TO WS-OPTION.
000750	     PERFORM START-AT-CRDX-SDTE THRU READ-CARDEX-EXIT.
000880	   IF WS-F-ERROR = 43
	       GO TO MOVE-NO-RECORD.

000810 MOVE-GET-CARDEX-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
	       IF NOT (CRD-EXT-CODE = W10-EXT-ITEM)
		   GO TO MOVE-FULL-SCREEN
	       ELSE
		   GO TO MOVE-SKIP-CRDX-READ.
000840	     PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
               SUBTRACT 1        FROM W75-S
	       GO TO MOVE-FULL-SCREEN.

       MOVE-LUP-DISP-ITEM.
	   IF NOT (CRD-EXT-CODE = W10-EXT-ITEM)
	       GO TO MOVE-FULL-SCREEN.
	   IF (CRD-DTE > W15-E-YMD)
	       GO TO MOVE-FULL-SCREEN.

       MOVE-SKIP-CRDX-READ.
000890	     DISPLAY (CLIN, 2) CRD-REF WITH FOREGROUND-COLOR 11.
	     MOVE CRD-DTE	 TO W22-DTE2.
	     MOVE W22-YY2	 TO W16-YY.
	     MOVE W22-MM2	 TO W16-MM.
	     MOVE W22-DD2	 TO W16-DD.
	     MOVE W16-DTE	 TO W95-DTE.
000900	     DISPLAY ( , 11) W95-DTE WITH FOREGROUND-COLOR 11.
000920	     DISPLAY ( , 20) CRD-AC WITH FOREGROUND-COLOR 11.
000940	     DISPLAY ( , 27) CRD-SMAN WITH FOREGROUND-COLOR 11.
	     MOVE CRD-COST	 TO W100-COST.
	     MOVE CRD-SELL	 TO W100-SELL.
	     MOVE CRD-QNT	 TO W100-QNT.
019830	   IF LS-OK = "Y"
000960	       DISPLAY ( , 30) W100-COST WITH FOREGROUND-COLOR 11.
	     DISPLAY ( , 41) W100-SELL WITH FOREGROUND-COLOR 11.
	     DISPLAY ( , 52) W100-QNT WITH FOREGROUND-COLOR 11.
	     DISPLAY ( , 61) CRD-NME WITH FOREGROUND-COLOR 11.
	   IF CRD-SELL < ZERO
	       DISPLAY ( , 78) " C" WITH FOREGROUND-COLOR 14
	   ELSE
	       DISPLAY ( , 78) " I" WITH FOREGROUND-COLOR 14.
000970     IF CLIN < 21
000980         ADD 1             TO CLIN W75-S
000990	       GO TO MOVE-GET-CARDEX-REC.
001000
001010 MOVE-FULL-SCREEN.
	     DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
001020	     DISPLAY " " AT 2303 WITH FOREGROUND-COLOR 14
001030		     " - " WITH FOREGROUND-COLOR 7
		     "PgUp" WITH FOREGROUND-COLOR 14
                     "/" WITH FOREGROUND-COLOR 7
		     "PgDn" WITH FOREGROUND-COLOR 14
		     " - " WITH FOREGROUND-COLOR 7
001170		     "Esc" WITH FOREGROUND-COLOR 14
		     "ape." WITH FOREGROUND-COLOR 7.

       MOVE-SAVE-FULL-SCREEN.
	     PERFORM SAVE-SCREEN-2.
001140	     MOVE 0		 TO W75-S1.
001150       MOVE 0601           TO CPOS.

001180 MOVE-CRT-ATTRIB.
	     MOVE 1920		 TO W41-LENGTH.
	     CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
					      W42-ATTRIB2
					      W41-LENGTH
					RETURNING WS-STATUS.
001220	     MOVE 2000		 TO W41-LENGTH.

001240 MOVE-GET-USER-OPT.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY CLEAR-L25.
	   IF ADIS-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN UP-KEY
001320		     GO TO MOVE-ADJ-NO-KEYS
		 WHEN DOWN-KEY
001340		     GO TO MOVE-CHK-NO-KEYS
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO MOVE-GET-USER-OPT
	   ELSE
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO MOVEMENT-LOOKUP-EXIT
		 WHEN PAGE-DOWN
001340		     GO TO MOVE-GET-NEXT-PAGE
		 WHEN PAGE-UP
001340		     GO TO MOVE-GET-PREV-PAGE
		 WHEN OTHER	 CALL X"E5"
	       END-EVALUATE
	       GO TO MOVE-GET-USER-OPT.
	     CALL X"E5".
001280	     GO TO MOVE-GET-USER-OPT.

001240 MOVE-GET-NEXT-PAGE.
001480     IF W75-S < 16
001490	       GO TO MOVE-GET-USER-OPT.

       MOVE-NEW-PAGE.
001500       MOVE 0601           TO CPOS.
001520	     PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.
001530       MOVE 6              TO CLIN.
001540       MOVE LOW-VALUES     TO W75-KEYS.
001550       MOVE 1              TO W75-S W75-S1.
	     PERFORM MOVE-LUP-TAB-LOOP.
001560	     GO TO MOVE-GET-CARDEX-REC.

       MOVE-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *    *************************************************************
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *    *************************************************************
	      MOVE "J"		  TO WS-OPTION.

       MOVE-GET-PREV-LOOP.
	     PERFORM READ-CARDEX-PREV THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       MOVE SPACE	 TO WS-OPTION
	       GO TO MOVE-NEW-PAGE.
	   IF NOT (CRD-EXT-CODE = W10-EXT-ITEM)
	       PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT
	       GO TO MOVE-NEW-PAGE.
	   IF CRD-DTE < W15-START
	       PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT
	       GO TO MOVE-NEW-PAGE.
	   IF WS-IXS1 < 32
	       ADD 1		 TO WS-IXS1
	       GO TO MOVE-GET-PREV-LOOP.
	     GO TO MOVE-NEW-PAGE.

001580 MOVE-NO-RECORD.
	     DISPLAY CLEAR-L25.
001590	     DISPLAY "No record - Press " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001600		     "ANY" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001610		     " key"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
001770	     GO TO MOVEMENT-LOOKUP-EXIT.

001790 MOVE-ADJ-NO-KEYS.
001800     IF W75-S1 > 1
001810         SUBTRACT 1        FROM W75-S1 CLIN.
001820	     GO TO MOVE-HIGHLIGHT.

001840 MOVE-CHK-NO-KEYS.
001850     IF W75-S1 = ZERO
001860         MOVE 1            TO W75-S1
001870	       GO TO MOVE-HIGHLIGHT.
001880     IF W75-S1 < W75-S
001890         ADD 1             TO W75-S1 CLIN.

001910 MOVE-HIGHLIGHT.
001920	     PERFORM MOVE-CRT-ATTRIB.
001930       DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
001940	     GO TO MOVE-GET-USER-OPT.

002070 MOVE-CRT-EDGE.
002130	     DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
				      FOREGROUND-COLOR 3
		     "        ³        ³      ³  ³          ³          ³
      - 	     "        ³                   "
		      WITH FOREGROUND-COLOR 3
		     "³" WITH BACKGROUND-COLOR 0
			      FOREGROUND-COLOR 3.
002090	     ADD 1		 TO CLIN.


002110 MOVEMENT-LOOKUP-EXIT.
002120       EXIT.
      /    *************************************************************
      *    ****    M O V E M E N T   R E P O R T
      *    *************************************************************
000010 MOVEMENT-REPORT		 SECTION 51.

000030 MOVE-REP-ITEM.
	     MOVE W10-EXT-ITEM	 TO CRD-EXT-CODE.
	     MOVE W15-S-YMD	 TO CRD-DTE.
	     MOVE "Y"		 TO WS-ITMHDG.

       MOVE-REP-START-CRDX.
	     MOVE SPACES	 TO WS-OPTION.
000750	     PERFORM START-AT-CRDX-SDTE THRU READ-CARDEX-EXIT.
000880	   IF WS-F-ERROR = 43
	       GO TO MOVE-REP-NO-RECORD.

000810 MOVE-REP-GET-CARDEX-REC.
000840	     PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       GO TO MOVEMENT-REPORT-PAUSE.

       MOVE-REP-CHECK-ITEM.
	   IF NOT (CRD-EXT-CODE = W10-EXT-ITEM)
	       GO TO MOVEMENT-REPORT-EXIT.
	   IF (CRD-DTE > W15-E-YMD)
	       GO TO MOVEMENT-REPORT-EXIT.
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
	   IF WS-ITMHDG = "Y"
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       PERFORM BB05.
012990	     MOVE SPACES	 TO R-DETAIL.

       MOVE-PRINT-ITEM-READ.
000890	     MOVE CRD-REF	 TO SRP-REF.
	     MOVE CRD-DTE	 TO W22-DTE2.
	     MOVE W22-YY2	 TO W16-YY.
	     MOVE W22-MM2	 TO W16-MM.
	     MOVE W22-DD2	 TO W16-DD.
	     MOVE W16-DTE	 TO SRP-DATE-2.
000920	     MOVE CRD-AC	 TO SRP-ACNO.
	     MOVE CRD-NAME	 TO SRP-NAME.
019830	   IF LS-OK = "Y"
	       MOVE CRD-COST	 TO SRP-COST.
	     MOVE CRD-SELL	 TO SRP-SELL.
	     MOVE CRD-QNT	 TO SRP-QUANT.
	   IF CRD-SELL < ZERO
	       MOVE "C" 	 TO SRP-TYPE
	   ELSE
	       MOVE "I" 	 TO SRP-TYPE.
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
	       MOVE "X" 	 TO WS-OPTION
	       PERFORM CALL-PRINTUTL
014990	       GO TO MOVEMENT-REPORT-EXIT.
012990	     MOVE SPACES	 TO R-DETAIL.
	     GO TO MOVE-REP-GET-CARDEX-REC.
001000
001580 MOVE-REP-NO-RECORD.
	     DISPLAY CLEAR-L25.
001590	     DISPLAY "No record - Press " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001600		     "ANY" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001610		     " key"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
001770	     GO TO MOVEMENT-REPORT-EXIT.

       MOVEMENT-REPORT-PAUSE.
	   IF WS-ITMHDG = "Y"
	       GO TO MOVE-REP-NO-RECORD.
	     DISPLAY CLEAR-L25.
001590	     DISPLAY "Press " AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
001600		     "ANY" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
001610		     " key"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
	     DISPLAY CLEAR-L25.

002110 MOVEMENT-REPORT-EXIT.
002120       EXIT.

       BB000	       SECTION 51.
       BB00.
      *    *************************************************************
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *    *************************************************************
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
012840	     MOVE W55-HEADING	 TO R-HEAD.
	     PERFORM CALL-PRINTUTL.
012860	     MOVE SPACES	 TO R-DETAIL.
      *    *************************************************************
      *    ****   1 2	C P I
      *    *************************************************************
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
012910	     MOVE SPACES	 TO R-DETAIL.
	     MOVE ALL "-"	 TO R-HL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
012920	     MOVE "Ref. No."	 TO SRP-H4.
012920	     MOVE "Date"	 TO SRP-H5.
	     MOVE "Ac.No."	 TO SRP-H6.
	     MOVE "Customer Name"
				 TO SRP-H7.
	     MOVE "Cost Price"	 TO SRP-H8.
	     MOVE "Sell.Price"	 TO SRP-H9.
	     MOVE "Quantity"	 TO SRP-H10.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE ALL "-"	 TO R-HL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012990	     MOVE SPACES	 TO R-DETAIL.
      *    *************************************************************
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *    *************************************************************
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "D"		 TO W02-PRN-TYPE.
      *    *************************************************************
      *    ****    I T E M   H E A D I N G
      *    *************************************************************
       BB05.
012990	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "Item:"	 TO SRP-H1.
	     MOVE STK-CODE	 TO SRP-EXT-ITEM.
	     MOVE STK-DESC	 TO SRP-DESC.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012990	     MOVE SPACES	 TO R-DETAIL.
      *    *************************************************************
      *    ****   1 2	C P I
      *    *************************************************************
	   IF NOT (WS-PRINTER = "S")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 2		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL.
	     MOVE "N"		 TO WS-ITMHDG.

       BB999.
	     EXIT.

       COPY APAC.HLP.

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

      /    *************************************************************
      * 		I N I T I A L I S E   P R O G R A M
      *    *************************************************************
041980 ZA000-INIT	       SECTION 8.
041990 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY W12-TODAY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
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
      *    *************************************************************
      *    ****    P A R A M E T E R   F I L E
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
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "INVOIC"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-INVOIC.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
042080	     MOVE 3		 TO WS-PARKEY.
042090       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT (PAR-STOCK = "A" OR "N")
	       MOVE "A" 	 TO PAR-STOCK.
	     MOVE PAR-STOCK	 TO W10-STCK.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	   IF PAR-STOCK = "N"
	       MOVE PAR-SLNGTH	 TO W10-SLNGTH
	   ELSE
	       MOVE 14		 TO W10-SLNGTH.
	   IF W10-SLNGTH < 3
	       MOVE 3		 TO W10-SLNGTH
	   ELSE
	   IF W10-SLNGTH > 14
	       MOVE 14		 TO W10-SLNGTH.
             MOVE 110            TO WS-USUB.
	   IF LS-USER NUMERIC
	       IF LS-USE > ZERO AND < 110
		   MOVE LS-USE	 TO WS-USUB.

042170 ZA02.
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
      /    *************************************************************
      *    ****    I / O   E R R O R   M E S S A G E S
      *    *************************************************************
042900 ZB000-ERROR	     SECTION 8.
      
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
041620         MOVE W02-NETWORK TO WS-FILE
041630	       MOVE WS-NETKEY TO WS-KEY
056420	   ELSE
056430     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
056460	   ELSE
056470     IF WS-F-ERROR = 12
056480         MOVE W02-INVOIC   TO WS-FILE
056490         MOVE ZERO         TO WS-KEY
               MOVE DOC-KEY      TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID TO WS-FILE
041870         MOVE WS-PARKEY TO WS-KEY
056620	   ELSE
056630     IF WS-F-ERROR = 22
056640         MOVE W02-STOCKF   TO WS-FILE
056650         MOVE ZERO         TO WS-KEY
	       MOVE STK-CODE	 TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE SME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE W02-CARDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CRD-KEY	 TO WS-KEYX.
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
