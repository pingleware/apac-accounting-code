      $SET LINKCOUNT"640"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	 ******   **	    *******    *
      *   **	**     **     **    **	**    **  **	    **	  **   *
      *   **	**     **     **    **	**	  **	    **	  **   *
      *   **	**     **     *******	 ******   **	    **	  **   *
      *   **	**     **     **	      **  **	    **	  **   *
      *   **	**     **     **	**    **  **	    **	  **   *
      *   *******      **     **	 ******   ********  *******    *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *  D E B T O R ' S   S A L E S   P E R   D E P T A R T M E N T   *
      *                                                                *
      *     VERSION 8.15.01 - July 2011 			       *
      *								       *
      ******************************************************************
      * 							       *
      *  May 2010    - This report has been included to get statistics *
      * 	       of sales per department for a selected account. *
      * 	       Used mainly for compititions between dealers    *
      * 	       that are held by distributors.		       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTPSLD.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   MAY 2010.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2010 - 2011
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

       COPY CONTROL.SL.

000140 COPY DEALER.SL.

000140 COPY DEATOT.SL.

000140 COPY DEBMEM.SL.

000140 COPY DEBTOR.ISL.

000210 COPY DEPART.SL.

000120 COPY PARAM.SL.

       COPY SHARED.SL.

000140 COPY STOCK.SL.

000260 DATA DIVISION.
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CARDEX.FDE.

       COPY CONTROL.FDE.

000140 COPY DEALER.FDE.

000140 COPY DEATOT.FDE.

000140 COPY DEBMEM.FDE.

000310 COPY DEBTOR.FDE.

000500 COPY DEPART.FDE.

000350 COPY PARAM.FDE.

       COPY SHARED.FDE.

000540 COPY STOCK.FDE.

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
       77  WS-CRDX	   PIC	9(04)	 COMP-5.
001670 77  WS-NETKEY	   PIC	9(06)	 COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001710 77  WS-RECKEY       PIC  9(06)    COMP-5.
001660 77  WS-SHARED	   PIC	9(06)	 COMP-5.
001730 77  WS-TRANS        PIC  9(04)    COMP-5 VALUE 1.
003400 77  WS-PAGE	   PIC	9(04)	 COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
001820 77  WS-OPTION	   PIC	X(01).
       77  WS-RESTRICT	   PIC	X(01).
       77  WS-RESTCODE	   PIC	X(01).
001820 77  WS-RTYPE	   PIC	X(01).
000750 77  WS-TYP	   PIC	X(01).
001820 77  WS-WRT	   PIC	X(01).
003550 77  WS-LEDG	   PIC	X(04).
001850 77  WS-ERROR	   PIC	9(01) VALUE ZERO.
       77  WS-RT-HD	   PIC	X(10) VALUE "-Retail   ".
       77  WS-WS-HD	   PIC	X(10) VALUE "-Wholesale".
       77  WS-CS-HD	   PIC	X(10) VALUE "-Cash sale".
002010 77  WS-ERR1	   PIC	X(22) VALUE "Invalid Account Number".
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
003350	   03  WS-MD-LNE.
003360	       05  WS-MCH   PIC  X(80) VALUE "³      ³
      -        "                      ³           ³          ³       ³".
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
002450	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
002470	   03  WS-BOT-LNE.
002480	       05  WS-BCHR PIC	X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
	       05  FILLER  PIC	X(01).
               05  WS-F-LINE
			   PIC	X(78).
	       05  FILLER  PIC	X(01).
002470     03  WS-BOT-LNE2.
002480	       05  WS-BCH  PIC	X(01) OCCURS 80.
003240     03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250	       05  FILLER  PIC	X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270			   PIC	X(50).
                  07  FILLER
			   PIC	X(10).
003280	       05  FILLER  PIC	X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

002230 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "DTP".
	   03  WS-PROG	       PIC  X(03) VALUE "SLD".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CARDEX.ID.

000140 COPY CONTROL.ID.

000330 COPY DEALER.ID.

000330 COPY DEATOT.ID.

000140 COPY DEBMEM.ID.

       COPY DEBTOR.ID.

003760 COPY DEPART.ID.

       COPY PARAM.ID.

       COPY SHARED.ID.

000140 COPY STOCK.ID.

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

	   03  DRP-L1 REDEFINES W02-PRN-LINE.
	       05  DRP-H1	   PIC	X(12).
	       05  DRP-DPT-CODE    PIC	X(04).
	       05  FILLER	   PIC	X(01).
	       05  DRP-DPT-NAME    PIC	X(30).
	       05  FILLER	   PIC	X(33).

	   03  DRP-L2 REDEFINES W02-PRN-LINE.
	       05  DRP-H2	   PIC	X(08).
	       05  DRP-H3	   PIC	X(72).

	   03  DRP-L3 REDEFINES W02-PRN-LINE.
	       05  DRP-ACT	   PIC	X(06).
	       05  FILLER	   PIC	X(02).
	       05  DRP-NAME	   PIC	X(72).

	   03  DRP-L4 REDEFINES W02-PRN-LINE.
	       05  DRP-H4	   PIC	X(15).
	       05  DRP-H5	   PIC	X(33).
	       05  DRP-H6	   PIC	X(11).
	       05  DRP-H7	   PIC	X(13).
	       05  DRP-H8	   PIC	X(15).
	       05  DRP-H9	   PIC	X(10).

	   03  DRP-L5 REDEFINES W02-PRN-LINE.
	       05  DRP-ITEM	   PIC	X(14).
	       05  FILLER	   PIC	X(01).
	       05  DRP-DESC	   PIC	X(30).
	       05  FILLER	   PIC	X(01).
	       05  DRP-QUANT	   PIC	Z(06)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  DRP-COST	   PIC	Z(07)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  DRP-SELL	   PIC	Z(07)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  DRP-EXTEND	   PIC	Z(07)9.99-.

	   03  DRP-L6 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC	X(02).
	       05  DRP-H10	   PIC	X(21).
	       05  DRP-H11	   PIC	X(30).
	       05  DRP-H12	   PIC	X(14).
	       05  DRP-H13	   PIC	X(14).
	       05  DRP-H14	   PIC	X(13).

	   03  DRP-L7 REDEFINES W02-PRN-LINE.
	       05  FILLER	   PIC	X(02).
	       05  DRP-ACNO	   PIC	X(06).
	       05  FILLER	   PIC	X(02).
	       05  DRP-AC-NAME	   PIC	X(40).
	       05  FILLER	   PIC	X(01).
	       05  DRP-TOT-QNT	   PIC	Z(08)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  DRP-TOT-COST	   PIC	Z(08)9.99-.
	       05  FILLER	   PIC	X(01).
	       05  DRP-TOT-SELL	   PIC	Z(08)9.99-.
	       05  FILLER	   PIC	X(02).

       01  W10-VARIABLES.
002890	   03  W10-LEDG.
	       05  W10-LEDG-NO	 PIC  9(04).
002910	   03  W10-DATE.
	       05  W10-EDTE	 PIC  99/99/9999.


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

       COPY W20.WS.

002850 01  W25-CALCS.
002860	   03  W25-RESULT      PIC  9(05)V99.
002870     03  W25-RESULT1 REDEFINES W25-RESULT.
002880	       05  W25-WHOLE   PIC  9(05).
002890	       05  W25-DECIMAL PIC  9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  W25-KEY     PIC  9(04).
	       05  W25-SUB     PIC  9(01).
	       05  FILLER      PIC  9(02).
002900     03  W25-TOTAL       PIC S9(09)V99 COMP-3.
002910     03  W25-VALUE       PIC S9(09)V99 COMP-3.
001660	   03  W25-TIME        PIC  9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001680	       05  FILLER      PIC  9(02).
001690	       05  W25-FACT    PIC  9(06).
	   03  W25-DATE REDEFINES W25-TRED.
	       05  W25-YY      PIC  9(04).
	       05  W25-MM      PIC  9(02).
	       05  W25-DD      PIC  9(02).
	   03  W25-CUR-CC      PIC  9(02) COMP-5.
	   03  W25-PRV-CC      PIC  9(02) COMP-5.
	   03  W25-NXT-CC      PIC  9(02) COMP-5.
	   03  W25-QNT	       PIC S9(09)V99  COMP-3.
004380	   03  W25-COST        PIC S9(09)V99  COMP-3.
004390	   03  W25-SELL        PIC S9(09)V99  COMP-3.

       01  W30-DISPLAY.
	   03  W30-SP	       PIC  ZZ9	       BLANK WHEN ZERO.
	   03  FILLER	       PIC  X(02).
	   03  W30-CODE        PIC  X(14).
	   03  FILLER	       PIC  X(01).
	   03  W30-DESC        PIC  X(30).
	   03  FILLER	       PIC  X(01).
	   03  W30-QUANT       PIC  Z(06)9.99- BLANK WHEN ZERO.
	   03  FILLER	       PIC  X(01).
	   03  W30-VALUE       PIC  Z(07)9.99- BLANK WHEN ZERO.

       01  W35-DEALER.
	   03  W35-NO	       PIC  9(06).
	   03  W35-END	       PIC  9(06).
	   03  W35-ACNO        PIC  X(06).

       COPY W40.WS.

       COPY W40.DBT.

       COPY FUNCTION.WS.

003530 COPY W50.WS.

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

       01  W75-RESTRICTIONS.
	   03  W75-DEPARTMENTS.
	       05  W75-DEPT OCCURS 10.
		   07  W75-DEPT-CDE
			       PIC  X(04).
		   07  W75-DEPT-DSC
			       PIC  X(30).
	   03  W75-DEP-CODES REDEFINES W75-DEPARTMENTS.
	       05  W75-DCDE-1  PIC  X(04).
	       05  W75-DDSC-1  PIC  X(30).
	       05  W75-DCDE-2  PIC  X(04).
	       05  W75-DDSC-2  PIC  X(30).
	       05  W75-DCDE-3  PIC  X(04).
	       05  W75-DDSC-3  PIC  X(30).
	       05  W75-DCDE-4  PIC  X(04).
	       05  W75-DDSC-4  PIC  X(30).
	       05  W75-DCDE-5  PIC  X(04).
	       05  W75-DDSC-5  PIC  X(30).
	       05  W75-DCDE-6  PIC  X(04).
	       05  W75-DDSC-6  PIC  X(30).
	       05  W75-DCDE-7  PIC  X(04).
	       05  W75-DDSC-7  PIC  X(30).
	       05  W75-DCDE-8  PIC  X(04).
	       05  W75-DDSC-8  PIC  X(30).
	       05  W75-DCDE-9  PIC  X(04).
	       05  W75-DDSC-9  PIC  X(30).
	       05  W75-DCDE-10 PIC  X(04).
	       05  W75-DDSC-10 PIC  X(30).

       01  W75-KEYS.
	   03  W75-S	      PIC  9(02) COMP-5.
	   03  W75-S1	      PIC  9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
	   03  W75-DNO	      PIC  X(06) OCCURS 18.
	   03  W75-SLEDG      PIC  9(03)    COMP-3 OCCURS 10.

003750 01  W75-TITLES.
003760     03  W75-DESCRIP.
003770         05  FILLER      PIC X(28)      VALUE
003780                     "Mr     Mnr    Mrs    Mev    ".
003790         05  FILLER      PIC X(28)      VALUE
003800                     "Miss   Mej    Ms     Ms     ".
003810         05  FILLER      PIC X(28)      VALUE
003820                     "Dr     Dr     Rev    Ds     ".
003830         05  FILLER      PIC X(28)      VALUE
003840                     "Prof   Prof   The HonSy Edel".
003850         05  FILLER      PIC X(28)      VALUE
003860                     "                            ".
003870     03  W75-CODES REDEFINES W75-DESCRIP.
003880         05  W75-TITLE  OCCURS 10.
003890             07  W75-ETITLE  PIC X(07).
003900             07  W75-ATITLE  PIC X(07).

003910 01  W80-ACCOUNT.
003920     03  W80-ACNO        PIC X(06).
003940     03  W80-TYPE        PIC 9(01).

004390 01  W95-STM.
004400     03  W95-COMP        PIC X(40).
004450	   03  W95-D.
004460	       05  W95-DTE     PIC Z9/99/9999.

004490 01  W100-EDIT.
004500     03  W100-LBAL       PIC Z(06)9.99-.
004500     03  W100-LARR       PIC Z(06)9.99.
004500     03  W100-BAL        PIC Z(08)9.99-.
004510     03  W100-CUR        PIC Z(08)9.99-.
004520     03  W100-30         PIC Z(08)9.99-.
004530     03  W100-60         PIC Z(08)9.99-.
004540     03  W100-90         PIC Z(08)9.99-.
004540     03  W100-120        PIC Z(08)9.99-.
004550     03  W100-INT        PIC Z(08)9.99.
007100	   03  W100-VAL        PIC Z(08)9.99-.
007120	   03  W100-V2.
007130	       05  W100-S6V2   PIC Z(05)9.99-.
007140	   03  W100-V3 REDEFINES W100-V2.
007150         05  W100-PRICE  PIC X(09).
007160         05  FILLER      PIC X(01).
007170	   03  W100-V4 REDEFINES W100-V2.
007180         05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.
004560	   03  W100-CRL        PIC Z(06)9.
004570     03  W100-KEY        PIC Z(05)9.
	   03  W100-START      PIC Z9/99/9999.
	   03  W100-END        PIC Z9/99/9999.

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

004580 SCREEN SECTION.

       COPY BLANK.CRT.

000850 01  S-PROG.
009350	   03  LINE  2 COLUMN 27 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
009360		       "TRANSACTIONS PER SALESPERSON".

       01  S03.
	   03  LINE  4 COLUMN 20 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
		      "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 20 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"³    ".
	   03	       COLUMN 25 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"DEBTOR ACCOUNT RANGE SELECTION".
	   03	       COLUMN 55 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				 "    ³".
	   03  LINE  6 COLUMN 20 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				"³ ".
	   03	       COLUMN 22 FOREGROUND-COLOR 3 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
		       "Printed from highest to lowest value".
	   03	       COLUMN 58 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
				" ³" .
	   03  LINE  7 COLUMN 20 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
		      "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  8 COLUMN 20 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
		      "³ Number of accounts for report:       ³".
	   03  LINE  9 COLUMN 20 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE
		      "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S04.
	   03  LINE  8 COLUMN 53 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC  Z(04)9 USING W35-NO AUTO.

009340 01  S05D.
009350	   03  LINE  2 COLUMN 27 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
009360		       "SALES PERFORMANCE REPORTING".
	   03  LINE  4 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE  5 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³   Transactions for Period.   ³".
	   03  LINE  6 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE  7 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³   Starting Date :            ³".
	   03  LINE  8 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³   Ending Date   :            ³".
	   03  LINE  9 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE 10 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "³Dates to be keyed in as ".
	   03	       COLUMN 50 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "DDMMYY".
	   03	       COLUMN 56 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6 VALUE "³".
	   03  LINE 11 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
				 BACKGROUND-COLOR 6
			      VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
       01  S05A.
	   03  LINE  7 COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-START AUTO.
	   03  LINE  8 COLUMN 45 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC 99/99/9999 USING W15-END AUTO.

       01  S06.
	   03  LINE  4 COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
		      "Debtor/Dept transactions for the period:- ".
	   03	       COLUMN 44 FOREGROUND-COLOR 6 HIGHLIGHT
				 PIC Z9/99/9999 FROM W15-START.
	   03	       COLUMN 54 FOREGROUND-COLOR 7 HIGHLIGHT
				 VALUE " to ".
	   03	       COLUMN 58 FOREGROUND-COLOR 6 HIGHLIGHT
				 PIC Z9/99/9999 FROM W15-END.
	   03  LINE  5 COLUMN  3 FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
	       VALUE "S/P. Item Code      Stock Description
      - 	     "   Quantity        Value ".

003990 01  S08.
	   03  LINE 14 COLUMN 10 FOREGROUND-COLOR 2 HIGHLIGHT
				 BACKGROUND-COLOR 2
				 VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -				       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
	   03  LINE 15 COLUMN 10 FOREGROUND-COLOR 2 HIGHLIGHT
				 BACKGROUND-COLOR 2
				 VALUE "³                      ".
	   03	       COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 2
				 VALUE "SALES LEDGER".
	   03	       COLUMN 45 FOREGROUND-COLOR 2 HIGHLIGHT
				 BACKGROUND-COLOR 2
				 VALUE "                       ³".
	   03  LINE 16 COLUMN 10 FOREGROUND-COLOR 2 HIGHLIGHT
				 BACKGROUND-COLOR 2
				 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -				 "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
	   03  LINE 17 COLUMN 10 FOREGROUND-COLOR 2 HIGHLIGHT
				 BACKGROUND-COLOR 2
				 VALUE "³".
	   03	       COLUMN 11 FOREGROUND-COLOR 0
				 BACKGROUND-COLOR 2
				 VALUE "Ledger No      Description  ".
	   03	       COLUMN 39 FOREGROUND-COLOR 2 HIGHLIGHT
				 BACKGROUND-COLOR 2
				 VALUE "                             ³".
	   03  LINE 18 COLUMN 10 FOREGROUND-COLOR 2 HIGHLIGHT
				 BACKGROUND-COLOR 2
				 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
      -				 "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

005040 01  S10.
005050	   03  S10A.
005060	       05  LINE 17 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 2
003470				     PIC X(04) USING W10-LEDG AUTO.
005080	   03  S10B.
005090	       05  LINE 17 COLUMN 38 FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 2
003470				     PIC X(30) USING DPT-DESC AUTO.

       COPY DEPART1.CRT.

       COPY DEBLUP.CRT.

       COPY DEBTOR.CRT.

007910 01  S36.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
				 PIC X(06) USING W80-ACNO AUTO.

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

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
007730 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
003240	   IF LS0-DBLEV < 5
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
010270	     PERFORM ZA000-INIT.
	     PERFORM BA000.
	     CLOSE CARDEX
013040		   DEALER
		   DEATOT
		   STOCK.

       AA49.
013150       EXIT PROGRAM.

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
      *    ³   PERFORM OPT-MESSAGE (Use this format if function or any ³
      *    ³	       other keys are to be tested on exit)	       ³
      *    ³			     OR 			       ³
      *    ³   PERFORM OPT-MESSAGE UNTIL WS-OPTION = "N" OR "Y".       ³
      *    ³   (Use TEST AFTER if WS-OPTION is preloaded with a        ³
      *    ³   default option)					       ³
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
      *    ³ then PERFORM OPT-SETUP THRU OPT-EXIT.		       ³
      *    ³ (The UNTIL and TEST AFTER options also apply here)        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY OPTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

       COPY DATE.CHK.

      /
      *    ****    READ FILES ROUTINES
      *
011650 AC000              SECTION.

       COPY CARDEX.RD.

       COPY CONTROL.RD.

011190 COPY DEALER.RD.

011190 COPY DEATOT.RD.

000140 COPY DEBMEM.RD.

011710 COPY DEBTOR.RD.

011210 COPY DEPART.RD.

011290 COPY PARAM.RD.

000140 COPY SHARED.RD.

       COPY STOCK.RD.

      /
011350 AD000-WRITE	       SECTION.

       COPY DEALER.WR.

       COPY DEATOT.WR.

       COPY DEPART1.LUP.

       COPY APAC.HLP.

       COPY ABORT.PRN.

       COPY PRINTUTL.AP5.

      *
      *    ****    Read debtor record - using account number.
      *
013610 CA155-GET-DEBTOR  SECTION 2.
013620 CA155.
013630       MOVE W80-ACNO       TO DEB-ACNO.
013640       PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
013650     IF WS-F-ERROR = 6
013660         MOVE 1            TO WS-ERROR
013670     ELSE
013680     IF DEB-ACNO NOT = W80-ACNO
013690         MOVE 2            TO WS-ERROR
013700     ELSE
013710         MOVE 0            TO WS-ERROR
013720         MOVE DEB-TYPE     TO W80-TYPE.
013730 CA160-EXIT.
013740       EXIT.
      *
013750 CA165-NAME        SECTION 2.
013760 CA165.
013770       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
013780                           INTO W40-SURNAME 
013790                                W40-FNAME 
013800                                W40-SNAME
013810       OVERFLOW
013820           MOVE 1          TO WS-ERROR.
013830 CA170-EXIT.
013840       EXIT.
      *
014530 CA175-ADDRESS	 SECTION 2.
014540 CA175.
014550       UNSTRING W60-ADDRESS DELIMITED BY "*"
014560				 INTO W40-ADD1
				      W40-ADD2
				      W40-ADD3
				      W40-ADD4
014570       OVERFLOW
014580           MOVE 1          TO WS-ERROR.
014590 CA175-EXIT.
014600       EXIT.
      *
014610 CA180-ADDRESS     SECTION 2.
014620 CA180.
014630       UNSTRING W60-ADDRESS1 DELIMITED BY "*"
014640                           INTO W40-PADD1 
                                      W40-PADD2 
                                      W40-PADD3 
                                      W40-PADD4
014650       OVERFLOW
014660           MOVE 1          TO WS-ERROR.
014670 CA180-EXIT.
014680       EXIT.
      /
      *  **  S A L E S	 A C C O U N T	 /   D E P A R T M E N T
      *
021000 BA000		       SECTION 50.
021020	     DISPLAY CLR-SCREEN.
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
	     DISPLAY CLEAR-L25.
	     MOVE W15-START	 TO W10-EDTE.
             PERFORM CHECK-DATE.
027400     IF WS-ERROR = 1
027410	       MOVE "Start date" TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BA05.
             MOVE W20-DTE        TO W15-S-YMD.
	     MOVE W15-END	 TO W10-EDTE.
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
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Get the Department Code for the Department details that  ³
      *    ³  are to extracted for the report.			       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     PERFORM BA100 THRU BA199.

       BA08.
	     MOVE ZERO		 TO WS-ERROR.
	     MOVE "Print 'D'etail or 'T'otals (D/T) [ ]"
				 TO WS-ERR-MES.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "D" OR "T".
	     MOVE WS-OPTION	 TO WS-RTYPE.
	     MOVE W15-SMM	 TO WS-CRDX.
	     MOVE W15-START	 TO W22-DTE1.
             MOVE W15-END        TO W22-DTE2.
	     MOVE 13		 TO SHADE-ROW.
	     MOVE 28		 TO SHADE-COL.
	     MOVE 28		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 1226
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "³Extracting details requested³" AT 1326
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "³Reading:                    ³" AT 1426
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1526
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     PERFORM SCREEN-SHADOW.
	     MOVE W15-S-YMD	 TO CRD-DTE.
	     MOVE SPACES	 TO CRD-EXT-CODE.
	     PERFORM START-AT-CRDX-DSTK THRU READ-CARDEX-EXIT.
000880	   IF WS-F-ERROR = 43
	       MOVE "No data for time period selected"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       MOVE 9		 TO WS-ERROR
	       GO TO BA15.
	     MOVE SPACES	 TO DEL-ACCNT.
	   IF WS-RTYPE = "T"
	       GO TO BA40.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Extract the Debtor details from the CARDEX file and write ³
      *    ³ to DEALER file.					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BA10.
000840	     PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       GO TO BA15.
	   IF NOT(CRD-DEPT = W10-LEDG)
	       GO TO BA10.
	   IF CRD-DTE > W15-E-YMD
	       GO TO BA15.
	     DISPLAY CRD-EXT-CODE AT 1436
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
	     MOVE CRD-AC	 TO DEL-ACCNT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Move Item Code to STK-CODE (STOCK FILE) which will be     ³
      *    ³ used as the key to read the stock description.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE CRD-EXT-CODE	 TO DEL-CODE STK-CODE.
	     MOVE CRD-DTE	 TO DEL-DTE.
	     MOVE ZERO		 TO DEL-SEQ.
	     MOVE CRD-REF	 TO DEL-REF.
	     MOVE CRD-SMAN	 TO DEL-SMAN.
	     MOVE CRD-QNT	 TO DEL-QNT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ There are occassions when the Quantity is negative that   ³
      *    ³ either the cost or selling values are positive on the     ³
      *    ³ CARDEX file record and the following has been inserted to ³
      *    ³ correct the error before doing any calculations.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF CRD-QNT < ZERO
	       MOVE "N" 	 TO WS-WRT
	       IF NOT(CRD-SELL < ZERO)
		   MULTIPLY -1	 BY CRD-SELL
		   MOVE "Y"	 TO WS-WRT
	       END-IF
	       IF NOT(CRD-COST < ZERO)
		   MULTIPLY -1	 BY CRD-COST
		   MOVE "Y"	 TO WS-WRT
	       END-IF
	       IF WS-WRT = "Y"
		   REWRITE CRD-REC1.
	   IF CRD-QNT < ZERO
	       COMPUTE DEL-COST = (CRD-QNT * CRD-COST) * -1
	       COMPUTE DEL-SELL = (CRD-QNT * CRD-SELL) * -1
	   ELSE
	       COMPUTE DEL-COST = (CRD-QNT * CRD-COST)
	       COMPUTE DEL-SELL = (CRD-QNT * CRD-SELL).
	     MOVE CRD-NAME	 TO DEL-NAME.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "No Stock Details for this Code"
				 TO STK-DESC
	       MOVE "ZZZZ"	 TO STK-LDG.
	     MOVE STK-DESC	 TO DEL-DESC.
	     MOVE STK-LDG	 TO DEL-LEDG.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Move Dept Code to DPT-CODE (DEPARTMENT FILE) which will   ³
      *    ³ be used as the key to read the department description.    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE CRD-DEPT	 TO DEL-LEDG DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE DPT-DESC	 TO DEL-DESC.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Write details to extract file (DEALER)		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     PERFORM WRITE-DEALER THRU WRITE-DEALER-EXIT.
	     MOVE DEL-ACCNT	 TO DET-ACCNT.
	     MOVE DEL-LEDG	 TO DET-LEDG.
      *    ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸
      *    ³	 ACCUMULATE THE TOTALS FOR THE DEBTOR/SALES LEDGER.    ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ Get the total record for the Debtor/Sales Ledger (DEATOT) ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     PERFORM READ-DEATOT THRU READ-DEATOT-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If no record exists, then create one.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF WS-F-ERROR = 45
	       MOVE ZERO	 TO DET-COST
				    DET-SELL
				    DET-QNT
	       MOVE DEL-NAME	 TO DET-NAME
	       MOVE DEL-DESC	 TO DET-DESC
	       PERFORM WRITE-DEATOT THRU WRITE-DEATOT-EXIT
	       PERFORM READ-DEATOT THRU READ-DEATOT-EXIT.
	     ADD DEL-QNT	 TO DET-QNT.
	     ADD DEL-COST	 TO DET-COST.
	     ADD DEL-SELL	 TO DET-SELL.
	     PERFORM REWRITE-DEATOT THRU WRITE-DEATOT-EXIT.
	     GO TO BA10.
      *    *************************************************************
      *    **** 		Extract complete		    ****
      *    *************************************************************
       BA15.
	   IF WS-ERROR = 9
	       GO TO BA30.
	   IF MOUSE-ATTACHED = "Y"
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 1		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM.
006630       DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     MOVE 5		 TO SHADE-ROW.
	     MOVE 22		 TO SHADE-COL.
	     MOVE 38		 TO SHADE-WIDTH.
	     MOVE 5		 TO SHADE-LINES.
      *
      *        ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *	       ³    DEBTOR ACCOUNT RANGE SELECTION    ³°°
      *	       ³ Printed from highest to lowest value ³°°
      *	       ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´°°
      *	       ³ Number of accounts for report: ..... ³°°
      *	       ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°
      * 	 °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      *
	     DISPLAY S03.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY "Press " AT 2502
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
		     "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
				BACKGROUND-COLOR 3
		     "ape to Exit"
		      WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.

       BA25.
	     ACCEPT S04.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE 9	 TO WS-ERROR
		     GO TO BA30
016550		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN OTHER
		     CALL X"E5" CALL X"E5" CALL X"E5"
		     GO TO BA25
	       END-EVALUATE.
	   IF W35-NO = ZERO
	       GO TO BA25.

       BA30.
	     EXIT.

       BA33.
	   IF WS-ERROR = 9
	       MOVE ZERO	 TO WS-ERROR
	       GO TO BA999.

       BA35.
	     MOVE "P"		 TO WS-OPTION.
050150*	     MOVE "'P'rinter or 'S'creen"
      *				 TO WS-ERR-MES.
      *	     MOVE SPACE 	 TO WS-OPTION.
      *	     MOVE 14		 TO SLIN.
      *	     PERFORM OPT-SETUP THRU OPT-EXIT.
050260*	   IF NOT(WS-OPTION = "P" OR "S")
050270*	       GO TO BA35.
	   IF WS-OPTION = "P"
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
	       PERFORM BB000
      *
      *    ****   6   L P I
      *
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 6		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE "C"		 TO WS-COMMAND
	       PERFORM CALL-PRINTUTL
	   ELSE
	       PERFORM BC000.
007060	     GO TO BA15.

       BA40.
000840	     PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       GO TO BA45.
	   IF CRD-DTE > W15-E-YMD
	       GO TO BA45.
	   IF NOT(CRD-DEPT = W10-LEDG)
	       GO TO BA40.
	     DISPLAY CRD-EXT-CODE AT 1436
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ There are occassions when the Quantity is negative while  ³
      *    ³ either the Cost or Selling values are positive on the     ³
      *    ³ CARDEX file record and the following has been inserted to ³
      *    ³ correct the error before doing any calculations.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF CRD-QNT < ZERO
	       MOVE "N" 	 TO WS-WRT
	       IF NOT(CRD-SELL < ZERO)
		   MULTIPLY -1	 BY CRD-SELL
		   MOVE "Y"	 TO WS-WRT
	       END-IF
	       IF NOT(CRD-COST < ZERO)
		   MULTIPLY -1	 BY CRD-COST
		   MOVE "Y"	 TO WS-WRT
	       END-IF
	       IF WS-WRT = "Y"
		   REWRITE CRD-REC1.
	     MOVE CRD-AC	 TO DET-ACCNT.
	     MOVE CRD-DEPT	 TO DET-LEDG.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Get the year and the month from the Cardex File as these  ³
      *    ³ will be used for the totals as totals are accumulated for ³
      *    ³ each month.					       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     MOVE CRD-DTE	 TO W25-TIME.
      *    ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸
      *    ³	 ACCUMULATE THE TOTALS FOR THE DEBTOR/SALES LEDGER.    ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ Get the total record for the Debtor/Sales Ledger (DEATOT) ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     PERFORM READ-DEATOT THRU READ-DEATOT-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ If no record exists, then create one.		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	   IF WS-F-ERROR = 45
	       MOVE ZERO	 TO DET-COST
				    DET-SELL
				    DET-QNT
	       MOVE CRD-NAME	 TO DET-NAME
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Move Dept Code to DPT-CODE (DEPARTMENT FILE) which will   ³
      *    ³ be used as the key to read the department description.    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	       MOVE CRD-DEPT	 TO DPT-CODE
	       PERFORM READ-DEPART THRU READ-DEPART-EXIT
	       MOVE DPT-DESC	 TO DET-DESC
	       PERFORM WRITE-DEATOT THRU WRITE-DEATOT-EXIT
	       PERFORM READ-DEATOT THRU READ-DEATOT-EXIT.
	       COMPUTE DET-QNT = DET-QNT + CRD-QNT.
	   IF CRD-QNT < ZERO
	       COMPUTE DET-COST = DET-COST + ((CRD-QNT * CRD-COST) * -1)
	       COMPUTE DET-SELL = DET-SELL + ((CRD-QNT * CRD-SELL) * -1)
	   ELSE
	       COMPUTE DET-COST = DET-COST + (CRD-QNT * CRD-COST)
	       COMPUTE DET-SELL = DET-SELL + (CRD-QNT * CRD-SELL).
	     PERFORM REWRITE-DEATOT THRU WRITE-DEATOT-EXIT.
	     GO TO BA40.

       BA45.
	     PERFORM BA15 THRU BA30.
	   IF WS-ERROR = 9
	       MOVE ZERO	 TO WS-ERROR
	       GO TO BA999.

       BA50.
	     MOVE "P"		 TO WS-OPTION.
050150*	     MOVE "'P'rinter or 'S'creen"
      *				 TO WS-ERR-MES.
      *	     MOVE SPACE 	 TO WS-OPTION.
      *	     MOVE 14		 TO SLIN.
      *	     PERFORM OPT-SETUP THRU OPT-EXIT.
050260*	   IF NOT(WS-OPTION = "P" OR "S")
050270*	       GO TO BA50.
	   IF WS-OPTION = "P"
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
	       PERFORM BD000
      *
      *    ****   6   L P I
      *
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 6		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE "C"		 TO WS-COMMAND
	       PERFORM CALL-PRINTUTL
	   ELSE
	       PERFORM BE000.
007060	     GO TO BA15.

       BA100.
	     MOVE SPACES	 TO W10-LEDG
				    WS-LEDG.
	   IF WS-OPTION = "N"
	       GO TO BA199.
	     PERFORM SAVE-SCREEN-3.
	     MOVE 15		 TO SHADE-ROW.
	     MOVE 12		 TO SHADE-COL.
	     MOVE 57		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     DISPLAY S08.
	     PERFORM SCREEN-SHADOW.
011010	     DISPLAY "F2" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT
011020		     "=Sales Ledger Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BA105.
	     ACCEPT S10A.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-LEDG
		     GO TO BA199
		 WHEN F2-KEY
011050		     PERFORM SAVE-SCREEN
		     PERFORM DEPART-VIEW
011320		     PERFORM RESTORE-SCREEN
		 WHEN OTHER
                     CALL X"E5"
               END-EVALUATE
015990	       GO TO BA105.
019250	     DISPLAY CLEAR-L25.
009170	   IF W10-LEDG = SPACES
009180	       GO TO BA199.
009240	     MOVE W10-LEDG	 TO DPT-CODE.
009250	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE "Invalid ledger code"
				 TO WS-ERR-STRING
	       PERFORM ERROR-MESSAGE
	       GO TO BA105.
	     DISPLAY S10.
	     PERFORM CHECK-CORRECT.
	   IF WS-OPTION = "N"
	       GO TO BA105.
	     MOVE W10-LEDG	 TO WS-LEDG.
	     PERFORM RESTORE-SCREEN-3.

       BA199.
	     EXIT.

021720 BA999.
021730	     EXIT.

       BB000	       SECTION 50.
       BB00.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     MOVE W15-START	 TO W100-START.
	     MOVE W15-END	 TO W100-END.
	     DISPLAY "Debtors/Dept transactions for the period:- "
		      AT 0404 WITH FOREGROUND-COLOR 7 HIGHLIGHT
		      W100-START WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     " to " WITH FOREGROUND-COLOR 7 HIGHLIGHT
		      W100-END WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 27		 TO SHADE-COL.
	     MOVE 29		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 1025
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "³ Reading Transaction Details ³" AT 1125
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "³                             ³" AT 1225
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1325
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     PERFORM SCREEN-SHADOW.
012530	     MOVE ZERO		 TO WS-PAGE W25-COST W25-QNT
				    W35-END W25-SELL.
	     MOVE SPACES	 TO W35-ACNO.
	     MOVE 999999999.99	 TO DET-SELL.
	     MOVE W10-LEDG	 TO DET-LEDG.
	     PERFORM START-AT-DPT-VAL THRU READ-DEATOT-EXIT.
	   IF WS-F-ERROR = 45
	       MOVE "No data on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BB999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Get the first account for the Department/Debtor Sales     ³
      *    ³ statistics report. 				       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     PERFORM READ-DEATOT-PREV THRU READ-DEATOT-EXIT.
	     ADD 1		 TO W35-END.
	     INITIALIZE DEL-AKEY.
	     MOVE DET-ACCNT	 TO DEL-ACCNT.
	     PERFORM START-AT-DEALER-ACT THRU READ-DEALER-EXIT.
	   IF WS-F-ERROR = 44
	       MOVE "Extract file DEALER corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BB999.
	     PERFORM READ-DEALER-NEXT THRU READ-DEALER-EXIT.

       BB10.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
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
	     MOVE "SALES PER DEPARTMENT/DEBTOR REPORT"
				 TO R-CPY.
	     PERFORM CALL-PRINTUTL.
012860	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "Department:"  TO DRP-H1.
	     MOVE DEL-LEDG	 TO DRP-DPT-CODE DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE DPT-DESC	 TO DRP-DPT-NAME.
	     PERFORM CALL-PRINTUTL.
012860	     MOVE SPACES	 TO R-DETAIL.
      *
      *    ****   1 2	C P I	( 9 6	Characters per line )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
	     MOVE ALL "-"	 TO R-HL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "Account"	 TO DRP-H2.
	     MOVE "Debtor Name"  TO DRP-H3.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****   1 2	C P I	( 9 6	Characters per line )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
012920	     MOVE "Item Code"	 TO DRP-H4.
012920	     MOVE "Description"  TO DRP-H5.
	     MOVE "Quantity"	 TO DRP-H6.
	     MOVE "Cost Price"	 TO DRP-H7.
	     MOVE "Sell Price"	 TO DRP-H8.
	     MOVE "Extended"	 TO DRP-H9.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE ALL "-"	 TO R-HL.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
012990	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "D"		 TO W02-PRN-TYPE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		 Print the heading for the account.	       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BB15.
	     PERFORM BB30.
	     MOVE DEL-ACCNT	 TO W35-ACNO.

012580 BB20.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO BB999.
	     MOVE DEL-DTE	 TO W95-DTE.
	   IF NOT(WS-PRINTER = "S")
	       DISPLAY DEL-SMAN AT 1227
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       DISPLAY W95-DTE AT 1230
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       DISPLAY DEL-REF AT 1239
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       DISPLAY DEL-ACCNT AT 1248
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
002830	     MOVE DEL-ITEM	 TO DRP-ITEM.
002850	     MOVE DEL-DESC	 TO DRP-DESC.
002870	     MOVE DEL-QNT	 TO DRP-QUANT.
	   IF DEL-QNT < ZERO
	       COMPUTE DRP-COST = ((DEL-COST / DEL-QNT) * -1)
	       COMPUTE DRP-SELL = ((DEL-SELL / DEL-QNT) * -1)
	   ELSE
	       COMPUTE DRP-COST = DEL-COST / DEL-QNT
	       COMPUTE DRP-SELL = DEL-SELL / DEL-QNT.
002890	     MOVE DEL-SELL	 TO DRP-EXTEND.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM BB10.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO BB999.
012990	     MOVE SPACES	 TO R-DETAIL.
	     PERFORM READ-DEALER-NEXT THRU READ-DEALER-EXIT.
	   IF NOT(WS-F-ERROR = 44)
	       IF DEL-ACCNT = W35-ACNO
		   GO TO BB20.
	     PERFORM BB25.
	     GO TO BB35.

       BB25.
	     MOVE SPACES	 TO R-DETAIL.
	     PERFORM CALL-PRINTUTL.
	     MOVE "Debtor Total" TO DRP-DESC.
	     MOVE DET-QNT	 TO DRP-QUANT.
	     MOVE DET-COST	 TO DRP-COST.
	     MOVE DET-SELL	 TO DRP-EXTEND.
	     ADD DET-SELL	 TO W25-SELL.
	     ADD DET-COST	 TO W25-COST.
	     ADD DET-QNT	 TO W25-QNT.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM BB10.
012990	     MOVE SPACES	 TO R-DETAIL.

       BB30.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
	     MOVE DET-ACCNT	 TO DRP-ACT.
	     MOVE DET-NAME	 TO DRP-NAME.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM BB10.
      *
      *    ****   1 2	C P I	( 9 6	Characters per line )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.

       BB35.
	   IF W35-END = W35-NO
	       MOVE 9		 TO WS-ERROR
	       GO TO BB40.
	     PERFORM READ-DEATOT-PREV THRU READ-DEATOT-EXIT.
	   IF (WS-F-ERROR = 45)
	       MOVE 9		 TO WS-ERROR
	       GO TO BB40.
	   IF W02-LINAGE < (W02-PRN-LENGTH - 2)
	       MOVE 2		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM BB10.
	     ADD 1		 TO W35-END.
	     INITIALIZE DEL-AKEY.
	     MOVE DET-ACCNT	 TO DEL-ACCNT.
	     PERFORM START-AT-DEALER-ACT THRU READ-DEALER-EXIT.
	   IF WS-F-ERROR = 44
	       MOVE "Extract file DEALER corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BB999.
	     PERFORM READ-DEALER-NEXT THRU READ-DEALER-EXIT.

       BB40.
012990	     MOVE SPACES	 TO R-DETAIL.
	   IF WS-ERROR = 9
	       GO TO BB45.
	     GO TO BB15.
      *
      *    ****    P R I N T   G R A N D   T O T A L
      *
       BB45.
	     PERFORM CALL-PRINTUTL.
012640	     MOVE "TOTAL - ALL DEBTORS"
				 TO DRP-DESC.
	     MOVE W25-QNT	 TO DRP-QUANT.
	     MOVE W25-COST	 TO DRP-COST.
012660	     MOVE W25-SELL	 TO DRP-EXTEND.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
012730	     MOVE SPACES	 TO R-DETAIL.

       BB999.
	     EXIT.
      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³		     S C R E E N   L A Y O U T		       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BC000	       SECTION 51.
       BC00.

       BC999.
	     EXIT.

       BD000	       SECTION 52.
       BD00.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     MOVE W15-START	 TO W100-START.
	     MOVE W15-END	 TO W100-END.
	     DISPLAY "Debtors/Dept transactions for the period:- "
		      AT 0404 WITH FOREGROUND-COLOR 7 HIGHLIGHT
		      W100-START WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     " to " WITH FOREGROUND-COLOR 7 HIGHLIGHT
		      W100-END WITH FOREGROUND-COLOR 6 HIGHLIGHT.
	     MOVE 11		 TO SHADE-ROW.
	     MOVE 27		 TO SHADE-COL.
	     MOVE 29		 TO SHADE-WIDTH.
	     MOVE 3		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 1025
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "³ Reading Debtor Sales Totals ³" AT 1125
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "³                             ³" AT 1225
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1325
		      WITH BACKGROUND-COLOR 7
			   FOREGROUND-COLOR 7 HIGHLIGHT.
	     PERFORM SCREEN-SHADOW.
012530	     MOVE ZERO		 TO WS-PAGE W25-COST W25-QNT
				    W35-END W25-SELL.
	     MOVE SPACES	 TO W35-ACNO.
	     MOVE 999999999.99	 TO DET-SELL.
	     MOVE W10-LEDG	 TO DET-LEDG.
	     PERFORM START-AT-DPT-VAL THRU READ-DEATOT-EXIT.
	   IF WS-F-ERROR = 45
	       MOVE "No data on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BD999.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Get the first account for the Department/Debtor Sales     ³
      *    ³ statistics report. 				       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     PERFORM READ-DEATOT-PREV THRU READ-DEATOT-EXIT.
	     ADD 1		 TO W35-END.

       BD10.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
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
	     MOVE "SALES PER DEPARTMENT/DEBTOR REPORT"
				 TO R-CPY.
	     PERFORM CALL-PRINTUTL.
012860	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "Department:"  TO DRP-H1.
	     MOVE DEL-LEDG	 TO DRP-DPT-CODE DPT-CODE.
	     PERFORM READ-DEPART THRU READ-DEPART-EXIT.
	     MOVE DPT-DESC	 TO DRP-DPT-NAME.
	     PERFORM CALL-PRINTUTL.
012860	     MOVE SPACES	 TO R-DETAIL.
      *
      *    ****   1 2	C P I	( 9 6	Characters per line )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
	     MOVE ALL "-"	 TO R-HL.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
012910	     MOVE SPACES	 TO R-DETAIL.
	     MOVE "Account"	 TO DRP-H10.
	     MOVE "Debtor Name"  TO DRP-H11.
	     MOVE "Tot. Quant"	 TO DRP-H12.
	     MOVE "Total Cost"	 TO DRP-H13.
	     MOVE "Total Sell"	 TO DRP-H14.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.

012580 BD15.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO BB999.
	   IF NOT(WS-PRINTER = "S")
	       DISPLAY DET-ACCNT AT 1227
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5
	       DISPLAY DET-DISP	AT 1234
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
002830	     MOVE DET-ACCNT	 TO DRP-ACNO.
002850	     MOVE DET-NAME	 TO DRP-AC-NAME.
	     MOVE DET-QNT	 TO DRP-TOT-QNT.
002870	     MOVE DET-COST	 TO DRP-TOT-COST.
002870	     MOVE DET-SELL	 TO DRP-TOT-SELL.
	     ADD DET-SELL	 TO W25-SELL.
	     ADD DET-COST	 TO W25-COST.
	     ADD DET-QNT	 TO W25-QNT.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009800	       PERFORM BB10.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
				 TO R-DETAIL
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014990	       GO TO BB999.
012990	     MOVE SPACES	 TO R-DETAIL.
	   IF W35-END = W35-NO
	       GO TO BD20.
	     PERFORM READ-DEATOT-PREV THRU READ-DEATOT-EXIT.
	   IF NOT(WS-F-ERROR = 45)
	       ADD 1		 TO W35-END
	       GO TO BD15.
      *
      *    ****    P R I N T   G R A N D   T O T A L
      *
       BD20.
	     PERFORM CALL-PRINTUTL.
012640	     MOVE "TOTAL - ALL DEBTORS"
				 TO DRP-DESC.
	     MOVE W25-QNT	 TO DRP-TOT-QNT.
012660	     MOVE W25-COST	 TO DRP-TOT-COST.
012660	     MOVE W25-SELL	 TO DRP-TOT-SELL.
	     MOVE 0		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
	     MOVE 99		 TO WS-ADVANCE.
	     MOVE "E"		 TO WS-COMMAND.
	     PERFORM CALL-PRINTUTL.
	     MOVE "P"		 TO WS-COMMAND.
012730	     MOVE SPACES	 TO R-DETAIL.

       BD999.
	     EXIT.

       BE000	       SECTION 53.
       BE00.

       BE999.
	     EXIT.




      /
      *    ****    I N I T I A L I Z E   P R O G R A M
      *   
041980 ZA000-INIT	       SECTION 80.
041990 ZA000-OPEN.
	     PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
042000	     DISPLAY CLR-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     COPY FUNCTION.PRO.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³	    Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
      *    *************************************************************
      *    ****       S A L E S	  -   C A R D E X   F I L E	    ****
      *    *************************************************************
	     MOVE "CARDEX"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-CARDEX.
      *    *************************************************************
      *    **** D E A L E R   S A L E S   S T A T I S T I C S	F I L E 	  ****
      *    *************************************************************
	     MOVE "DEALER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-DEALER.
      *    *************************************************************
      *    ****    D E B T O R	 S A L E S   T O T A L	 F I L E    ****
      *    *************************************************************
	     MOVE "DEATOT"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-DEATOT.
      *    *************************************************************
      *    **** 	  D E B T O R	M E M O	  F I L E	    ****
      *    *************************************************************
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
      *    *************************************************************
      *    **** 	       D E B T O R   F I L E		    ****
      *    *************************************************************
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
      *    *************************************************************
      *    **** 	   D E P A R T M E N T	 F I L E	    ****
      *    *************************************************************
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.

       ZA01.
042720	     MOVE 1		 TO WS-PARKEY.
042730       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
042740       MOVE PAR-COMPANY    TO W95-COMP.
042750       MOVE PAR-DMY        TO W12-TODAY.
042760       MOVE PAR-YMD        TO W12-T-YMD.
      *    *************************************************************
      *    ****     O P E N   D E A L E R   S T A T S	F I L E	    ****
      *    *************************************************************
042170 ZA02.
053830	     OPEN OUTPUT DEALER.
053840	   IF RUNTIME-ERROR
053880         IF FLE-LIMIT
053890		   GO TO ZA49.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 44		 TO WS-F-ERROR
053920         PERFORM OPEN-ERROR.
	     CLOSE DEALER.
	     OPEN I-O DEALER.
	     OPEN OUTPUT DEATOT.
042220	   IF RUNTIME-ERROR
	       IF FLE-LIMIT
		   GO TO ZA49.
042240	   IF WS-STATUS NOT = "00"
042250	       MOVE 45		 TO WS-F-ERROR
050210	       PERFORM OPEN-ERROR.
	     CLOSE DEATOT.
	     OPEN I-O DEATOT.
      *    *************************************************************
      *    ****       S A L E S	  -   C A R D E X   F I L E	    ****
      *    *************************************************************
053830	     OPEN I-O CARDEX.
053840	   IF RUNTIME-ERROR
053850         IF FLE-LOCKED
053860             GO TO ZA200
053870         ELSE
053880         IF FLE-LIMIT
053890		   GO TO ZA49
               ELSE
	       IF IDX-CORRUPT
		   MOVE 43	 TO WS-F-ERROR
		   GO TO ZA50.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 43		 TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT(WS-STATUS = "41")
053920		   PERFORM OPEN-ERROR.
054300	     OPEN I-O STOCK.
054310     IF RUNTIME-ERROR
054320         IF FLE-LOCKED
054330             GO TO ZA200
054340         ELSE
054350         IF FLE-LIMIT
054360             GO TO ZA49
054370         ELSE
054380	       IF IDX-CORRUPT
054390             MOVE 22       TO WS-F-ERROR
054400             GO TO ZA50.
054410     IF WS-STATUS NOT = "00"
054420         MOVE 22           TO WS-F-ERROR
	       IF WS-STATUS = "39"
038770		   GO TO ZA50
	       ELSE
	       IF NOT(WS-STATUS = "41")
054430		   PERFORM OPEN-ERROR.
044780	     GO TO ZA999-EXIT.

       COPY ZA49.PRO.

039850 ZA50.
041850	   IF WS-F-ERROR = 43
041860	       MOVE "SALES CARDEX"
				 TO WS-FILE.
039980       DISPLAY "Rebuild " AT 0812
039990		      WS-FILE WITH FOREGROUND-COLOR 6 HIGHLIGHT.
040000       DISPLAY "Press any key to continue" AT 1012.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
040020 ZA51.
040030       EXIT PROGRAM.

       ZA55.
041220       MOVE 1              TO WS-S1.

041240 ZA60.
041250	     MOVE WS-G1 	 TO WS-TCHR(WS-S1) WS-BCHR(WS-S1).
	     MOVE WS-G8 	 TO WS-TCH(WS-S1)  WS-BCH(WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1             TO WS-S1
041280         GO TO ZA60.
	     MOVE WS-G9 	 TO WS-TCH(1).
	     MOVE WS-G10	 TO WS-TCH(80).
	     MOVE WS-G11	 TO WS-BCH(1).
	     MOVE WS-G12	 TO WS-BCH(80).
	     MOVE WS-G14	 TO WS-TCHR(1)	WS-BCHR(1).
	     MOVE WS-G13	 TO WS-TCHR(80) WS-BCHR(80).
021930	     MOVE WS-G2 	 TO WS-TCHR(8)	WS-TCHR(49)
021940				    WS-TCHR(61) WS-TCHR(72).
021970	     MOVE WS-G4 	 TO WS-BCHR(8)	WS-BCHR(49)
021980				    WS-BCHR(61) WS-BCHR(72).
	     MOVE WS-G13	 TO WS-TCHR(80) WS-BCHR(80).
	     MOVE WS-G14	 TO WS-TCHR(1)	WS-BCHR(1).
	     MOVE "SupervisorÄ"  TO WS-WRKHD.

055970 ZA200.

       COPY LOCKED.PRO.

056040 ZA205.
056050       EXIT PROGRAM.

042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 81.
      
       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041610	   IF WS-F-ERROR = 2
041620	       MOVE W02-NETWORK  TO WS-FILE
041630	       MOVE WS-NETKEY	 TO WS-KEY
056380	   ELSE
056390     IF WS-F-ERROR = 6
056400	       MOVE W02-DEBTOR	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
056420	   ELSE
056430     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
041640	   ELSE
041850	   IF WS-F-ERROR = 15
041860	       MOVE WS-PARID	 TO WS-FILE
041870	       MOVE WS-PARKEY	 TO WS-KEY
056620	   ELSE
056630     IF WS-F-ERROR = 22
056640         MOVE W02-STOCKF   TO WS-FILE
056650         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840	   ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 43
041860	       MOVE W02-CARDEX	 TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE CRD-KEY	 TO WS-KEYX
041840	   ELSE
041850	   IF WS-F-ERROR = 44
041860	       MOVE W02-DEALER	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 45
041860	       MOVE W02-DEATOT	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY.

       COPY DISPERR.PRO.
