      $SET LINKCOUNT"384"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******	********   ******   *******    *
      *   **	**     **     **    **	   **	  **	**  **	  **   *
      *   **	       **     **    **	   **	  **	**  **	  **   *
      *    ******      **     *******	   **	  **	**  *******    *
      * 	**     **     **	   **	  **	**  **	       *
      *   **	**     **     **	   **	  **	**  **	       *
      *    ******      **     **	   **	   ******   **	       *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     S A L E S	P E R F O R M A N C E	P R O G R A M	       *
      *                                                                *
      *     VERSION 8.13.07 - March 2008			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     STPTOP.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   OCTOBER 1992.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1992 - 2008
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

       COPY DEPART.SL.

000120 COPY PARAM.SL.

000140 COPY STOCK.SL.

      $SET KEYCOMPRESS "7"
000010	   SELECT SALES   ASSIGN DISK
000040                    STATUS WS-STATUS
000050                    ACCESS DYNAMIC
000060                    ORGANIZATION INDEXED
000070			  RECORD SAL-EXT-CODE
000080			  ALTERNATE RECORD SAL-QUANT DUPLICATES
000090			  ALTERNATE RECORD SAL-TSELL DUPLICATES
000130			  ALTERNATE RECORD SAL-GP DUPLICATES.
      $SET NOKEYCOMPRESS

000260 DATA DIVISION.
      /
      * 	********  ******   **	     ********	******
      * 	**	    **	   **	     **        **    **
      * 	**	    **	   **	     **        **
      * 	*****	    **	   **	     *****	******
      * 	**	    **	   **	     ** 	     **
      * 	**	    **	   **	     **        **    **
      * 	**	  ******   ********  ********	******
      *
000270 FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CARDEX.FDE.

       COPY DEPART.FDE.

000290 COPY PARAM.FDE.

000540 COPY STOCK.FDE.

      /
000010 FD  SALES     IS EXTERNAL
		     LABEL RECORD STANDARD
000020		     VALUE OF FILE-ID W02-SALES.
000220 01  SAL-REC1.
000230	   03  SAL-KEY.
	       05  SAL-EXT-CODE.
		   07  SAL-CODE	PIC  X(14).
		   07  SAL-ITM	PIC  X(03).
		   07  FILLER	PIC  X(01).
	   03  SAL-QUANT.
000250	       05  SAL-QNT     PIC S9(07)V99 COMP.
	   03  SAL-COST        PIC S9(09)V99 COMP.
	   03  SAL-TSELL.
	       05  SAL-SELL    PIC S9(09)V99 COMP.
	   03  SAL-GP.
	       05  SAL-PROFIT  PIC S9(09)V99 COMP.
	   03  SAL-DESC        PIC  X(30).
	   03  SAL-XREF        PIC  X(18).
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
003270 77  WS-PAGE	   PIC	9(04)	 COMP-5.
       77  PRG-PRINTUTL	   PIC	X(12) VALUE "UTP\PRINTUTL".
001690 77  WS-ENQPOS	   PIC	9(06)	 COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001810 77  WS-KEYSTR       PIC  9(06)    COMP-5.
001820 77  WS-OPTION       PIC  X(01).
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
	   03  WS-PROG	       PIC  X(03) VALUE "TOP".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

002310 01  W02-FID.

       COPY APACFIDS.ID.

       COPY CARDEX.ID.

       COPY DEPART.ID.

       COPY PARAM.ID.

000140 COPY STOCK.ID.

       COPY PERFRM.ID.

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
000390	   03  REP-LINE1 REDEFINES W02-PRN-LINE.
000400	       05  REP-DETAIL1 PIC  X(140).
000410	   03  REP-LINE2 REDEFINES W02-PRN-LINE.
000420	       05  REP-DETAIL2 PIC  X(80).
000430	   03  REP-LINE3 REDEFINES W02-PRN-LINE.
000490	       05  REP-DATE    PIC  X(06).
000500	       05  REP-DTE     PIC  Z9/99/9999.
000460	       05  FILLER      PIC  X(03).
	       05  REP-PRC-HD.
		   07  FILLER  PIC  X(01).
000470		   07  REP-COMPANY  PIC	X(40).
		   07  FILLER  PIC  X(02).
000480	       05  FILLER      PIC  X(08).
000440	       05  REP-PAGE    PIC  X(06).
000450	       05  REP-P-NO    PIC  Z(03)9.
000430	   03  REP-LINE4 REDEFINES W02-PRN-LINE.
	       05  FILLER      PIC  X(01).
	       05  REP-HEAD    PIC  X(94).
	       05  FILLER      PIC  X(01).

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

       01  W30-LINE.
	   03  W30-DISPLAY.
	       05  W30-CODE    PIC  X(19).
	       05  W30-DESC    PIC  X(31).
	       05  W30-QUANT   PIC  Z(06)9.99-.
	       05  W30-VALUE   PIC  Z(08)9.99-.
	   03  W30-EXTRA.
	       05  W30-SPACE   PIC  X(01).
	       05  W30-XREF    PIC  X(18).

       01  W35-HEADINGS.
	   02  W35-PERIOD.
	   03  FILLER	 PIC  X(35)
			 VALUE "Sales Performance for the period:- ".
	   03  W35-START PIC  Z9/99/9999.
	   03  FILLER	 PIC  X(04) VALUE " to ".
	   03  W35-END	 PIC  Z9/99/9999.
	   03  W35-REP	 PIC  X(18).
	   02  W35-HEADING.
	   03  FILLER	 PIC  X(86)
	       VALUE "Item Code          Stock Description
      - 	     "  Quantity        Value  X-Ref Code".

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
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
	   03  W75-S2	      PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
005860     03  W75-DNO        PIC X(06) OCCURS 18.
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
009350	   03  LINE  2 COLUMN 27 FOREGROUND-COLOR 7
				 HIGHLIGHT VALUE
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
				 VALUE "5".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Stock Gross Profit-MTD ³".
001470	   03  LINE 11 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "³ ".
	   03	       COLUMN 31 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7
				 HIGHLIGHT
				 VALUE "0".
	   03	       COLUMN 32 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE " Exit                   ³".
	   03  LINE 12 COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 0
				 VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

009340 01  S05D.
009350	   03  LINE  2 COLUMN 27 FOREGROUND-COLOR 7
				 HIGHLIGHT VALUE
009360		       "SALES PERFORMANCE REPORTING".
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
	   03  LINE  5 COLUMN  4 FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
	       VALUE "Item Code          Stock Description
      - 	     "  Quantity        Value ".

007910 01  S36.
007920	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(14) USING W10-ITEM AUTO.

004870 COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
003240	   IF LS0-SALEV < 3
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
010270	     PERFORM ZA000-INIT.
	     PERFORM BA000.
	     CLOSE SALES.
	   IF WS-ENQ = "P"
	       MOVE "C"		 TO WS-COMMAND
	       PERFORM CALL-PRINTUTL.

       AA49.
013150       EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

011390 COPY LOCKED.RC2.

011410 COPY CLEAR.CRT.

       COPY DATE.CHK.

       COPY ABORT.PRN.

       COPY PRINTUTL.AS3.

      /
005810 AB000              SECTION.
005110 AB05.
      *
      *    ****   1 O	C P I	( N O R M A L	P R I N T )
      *
005120	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 1		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
005120	     MOVE SPACES	 TO REP-DETAIL1.
005130	     MOVE "Page:"	 TO REP-PAGE.
005140	     MOVE "Date:"	 TO REP-DATE.
005150	     MOVE W95-COMP	 TO REP-COMPANY.
005160       ADD 1               TO WS-PAGE.
005170	     MOVE WS-PAGE	 TO REP-P-NO.
005180	     MOVE W12-TODAY	 TO REP-DTE.
	     MOVE 2		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005200	     MOVE SPACES	 TO REP-DETAIL1.

005210 AB10.
005220	     MOVE W35-PERIOD	 TO REP-HEAD.
	     MOVE 2		 TO WS-ADVANCE.
	     MOVE 1		 TO WS-ADVANCE.
	     PERFORM CALL-PRINTUTL.
005240	     MOVE SPACES	 TO REP-DETAIL1.
      *
      *    ****   1 2	C P I	( 9 6	Characters per line )
      *
	     MOVE 0		 TO WS-ADVANCE.
	     MOVE 2		 TO W02-PRN-TYPE.
	     PERFORM CALL-PRINTUTL.
	     MOVE "H"		 TO W02-PRN-TYPE.
	     MOVE 2		 TO WS-ADVANCE.
	     MOVE W35-HEADING	 TO REP-HEAD.
	     PERFORM CALL-PRINTUTL.
005240	     MOVE SPACES	 TO REP-DETAIL1.
	     MOVE 1		 TO WS-ADVANCE.
	     MOVE "D"		 TO W02-PRN-TYPE.

       AB20.
	     EXIT.

      /
      *    ****    READ FILES ROUTINES
      *
011650 AC000              SECTION.

       COPY CARDEX.RD.

       COPY DEPART.RD.

011750 COPY PARAM.RD.

       COPY STOCK.RD.

000010 READ-SALES.
000020	     READ SALES 	 WITH IGNORE LOCK
000030	       KEY IS SAL-EXT-CODE.
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
000390	     READ SALES NEXT	 WITH IGNORE LOCK.
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
000390	     READ SALES PREVIOUS WITH IGNORE LOCK.
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

000750 START-AT-SALES-CODE.
000760	     START SALES
000770	       KEY >= SAL-EXT-CODE.
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
000900	     START SALES
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
001040	     START SALES
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
001180	     START SALES
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

       COPY APAC.HLP.

      /
      *  ****  E N Q U I R Y   ( S A L E S   P E R F O R M A N C E )
      *
021000 BA000		       SECTION.
021010 BA00.
021020       DISPLAY CLR-SCREEN.
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
	     MOVE W15-START	 TO W22-DTE1 W35-START.
	     MOVE W15-END	 TO W22-DTE2 W35-END.
	     DISPLAY WS-BLNK78 AT 0902
		     WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1.
	     DISPLAY WS-BLNK78 AT 1102
		     WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1.
011320	     PERFORM RESTORE-SCREEN-3.
	     DISPLAY "Extracting details requested" AT 0912.
	     DISPLAY "Reading:" AT 1112.
	     MOVE W15-S-YMD	 TO CRD-DTE.
	     MOVE SPACES	 TO CRD-EXT-CODE.
	     PERFORM START-AT-CRDX-DSTK THRU READ-CARDEX-EXIT.
000880	   IF WS-F-ERROR = 43
	       GO TO BA16.
	     MOVE SPACES	 TO SAL-EXT-CODE.

       BA10.
000840	     PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
	   IF WS-F-ERROR = 43
	       GO TO BA16.
	   IF CRD-DTE > W15-E-YMD
	       GO TO BA16.
	     DISPLAY CRD-EXT-CODE AT 1121
		     WITH BACKGROUND-COLOR 5
			  FOREGROUND-COLOR 3 HIGHLIGHT.
	   IF CRD-EXT-CODE = SAL-EXT-CODE
	       GO TO BA15.
	     MOVE CRD-EXT-CODE	 TO SAL-EXT-CODE STK-CODE.
	     PERFORM READ-STOCK THRU READ-STOCK-EXIT.
	   IF WS-F-ERROR = 22
	       MOVE "No Stock Details for this Code"
				 TO STK-DESC.
	     PERFORM READ-SALES THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       MOVE STK-DESC	 TO SAL-DESC
	       MOVE STK-XREF	 TO SAL-XREF
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

       BA16.
	     MOVE "'P'rint or 'D'isplay format  [D]"
				 TO WS-ERR-MES.
	     MOVE "D"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT (WS-OPTION = "D" OR "P")
	       GO TO BA16.
	     MOVE WS-OPTION	 TO WS-ENQ.
	   IF WS-ENQ = "D"
	       GO TO BA20.
	     MOVE ZERO		 TO WS-PAGE.
      *
      *  ***  ****  ***** *   *   ****	****  *** *   * ***** ***** ****
      * *   * *   * *	  **  *   *   * *   *  *  **  *   *   *     *	*
      * *   * ****  ***   * * *   ****	****   *  * * *   *   ***   ****
      * *   * *     *	  *  **   *	*   *  *  *  **   *   *     *	*
      *  ***  *     ***** *   *   *	*   * *** *   *   *   ***** *	*
      *
	     PERFORM OPEN-PRINTER.
	     MOVE "P"		 TO WS-COMMAND.

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
	     MOVE 8		 TO SHADE-LINES.
	     DISPLAY S-MENU.
	     DISPLAY MENU-INSTRUCT.
	     PERFORM SCREEN-SHADOW.
       BA25.
	     MOVE 5		 TO TOP-ROW.
	     MOVE 10		 TO BOTTOM-ROW.
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
	       IF WS-OPTION < "0" OR > "5"
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
	       MOVE "0"		 TO WS-OPTION.
	   GO TO BA50.

       BA45.
	     PERFORM CLEAR-BLOCK.
	   IF WS-OPTION = "0"
	       MOVE 10		 TO SCREEN-LIN
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
	       MOVE 9		 TO SCREEN-LIN.
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
006820	       PERFORM BE000
006800     ELSE
006810	   IF WS-OPTION = "5"
006820	       PERFORM JA000.
	     MOVE "P"		 TO WS-COMMAND.
007060	     GO TO BA20.

021720 BA999.
021730       EXIT.

       BB000	       SECTION 50.
       BB00.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	   IF WS-ENQ = "P"
	       MOVE " (Quantity Seq.)"
				 TO W35-REP
	       MOVE ZERO	 TO WS-PAGE
	       PERFORM AB000
	   ELSE
	       DISPLAY S06
	       DISPLAY " (Quantity Seq.)  " AT 0462
			WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     MOVE "999999999"	 TO SAL-QUANT.
	     PERFORM START-AT-SALES-QNT THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       MOVE "No data on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BB999.

       BB05.
	     MOVE 0604		 TO CPOS.

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
	     MOVE SAL-EXT-CODE	 TO W30-CODE.
	     MOVE SAL-DESC	 TO W30-DESC.
	     MOVE SAL-QNT	 TO W30-QUANT.
	     MOVE SAL-SELL	 TO W30-VALUE.
	     MOVE SPACE 	 TO W30-SPACE.
	     MOVE SAL-XREF	 TO W30-XREF.
	   IF WS-ENQ = "P"
	       GO TO BB40.
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
	     DISPLAY CLEAR-L25.
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

       BB40.
	     MOVE W30-LINE	  TO REP-HEAD.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009740	       PERFORM AB000.
009750	     MOVE SPACES	 TO REP-DETAIL1.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE 1		 TO WS-ADVANCE
010010	       MOVE SPACES	 TO REP-DETAIL1
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P" 	 TO WS-COMMAND
	       GO TO BB999.
	     PERFORM READ-SALES-PREV THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014970	       MOVE "**** END OF REPORT ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       GO TO BB999.
	     GO TO BB12.

       BB999.
	     EXIT.

       BC000	       SECTION 51.
       BC00.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	   IF WS-ENQ = "P"
	       MOVE " (Value Sold Seq.)"
				 TO W35-REP
	       MOVE ZERO	 TO WS-PAGE
	       PERFORM AB000
	   ELSE
	       DISPLAY S06
	       DISPLAY " (Value Sold Seq.)" AT 0462
			WITH FOREGROUND-COLOR 7 HIGHLIGHT.
	     MOVE "99999999999"  TO SAL-TSELL.
	     PERFORM START-AT-SALES-TSELL THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       MOVE "No data on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BC999.

       BC05.
	     MOVE 0604		 TO CPOS.

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
	     MOVE SAL-EXT-CODE	 TO W30-CODE.
	     MOVE SAL-DESC	 TO W30-DESC.
	     MOVE SAL-QNT	 TO W30-QUANT.
	     MOVE SAL-SELL	 TO W30-VALUE.
	     MOVE SPACE 	 TO W30-SPACE.
	     MOVE SAL-XREF	 TO W30-XREF.
	   IF WS-ENQ = "P"
	       GO TO BC40.
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
	     DISPLAY CLEAR-L25.
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

       BC40.
	     MOVE W30-LINE	  TO REP-HEAD.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009740	       PERFORM AB000.
009750	     MOVE SPACES	 TO REP-DETAIL1.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE 1		 TO WS-ADVANCE
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P" 	 TO WS-COMMAND
	       GO TO BC999.
	     PERFORM READ-SALES-PREV THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       PERFORM CALL-PRINTUTL
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE 1		 TO WS-ADVANCE
014970	       MOVE "**** END OF REPORT ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       GO TO BC999.
	     GO TO BC12.

       BC999.
	     EXIT.

       BD000	       SECTION 52.
       BD00.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	   IF WS-ENQ = "P"
	       MOVE " (G.Profit Seq.)"
				 TO W35-REP
	       MOVE ZERO	 TO WS-PAGE
	       PERFORM AB000
	   ELSE
	       DISPLAY S06
	       DISPLAY " (G.Profit Seq.)  " AT 0462
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
	     MOVE 0604		 TO CPOS.

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
	     MOVE SAL-EXT-CODE	 TO W30-CODE.
	     MOVE SAL-DESC	 TO W30-DESC.
	     MOVE SAL-QNT	 TO W30-QUANT.
	     MOVE SAL-PROFIT	 TO W30-VALUE.
	     MOVE SPACE 	 TO W30-SPACE.
	     MOVE SAL-XREF	 TO W30-XREF.
	   IF WS-ENQ = "P"
	       GO TO BD40.
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
	     DISPLAY CLEAR-L25.
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

       BD40.
	     MOVE W30-LINE	  TO REP-HEAD.
	   IF W02-LINAGE < W02-PRN-LENGTH
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009780     ELSE
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
009740	       PERFORM AB000.
009750	     MOVE SPACES	 TO REP-DETAIL1.
	     PERFORM PRINT-ABORT.
	   IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE 1		 TO WS-ADVANCE
014970	       MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       MOVE "P" 	 TO WS-COMMAND
	       GO TO BD999.
	     PERFORM READ-SALES-PREV THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       MOVE 0		 TO WS-ADVANCE
	       MOVE 1		 TO W02-PRN-TYPE
	       PERFORM CALL-PRINTUTL
	       MOVE "D"		 TO W02-PRN-TYPE
	       MOVE 1		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
014970	       MOVE "**** END OF REPORT ****"
                                 TO REP-DETAIL1
	       MOVE 99		 TO WS-ADVANCE
	       PERFORM CALL-PRINTUTL
	       GO TO BD999.
	     GO TO BD12.

       BD999.
	     EXIT.

       BE000	       SECTION 53.
       BE00.
	     DISPLAY CLR-SCREEN.
	     DISPLAY S-PROG.
	     MOVE " !"		 TO SAL-EXT-CODE.
	     PERFORM START-AT-SALES-CODE THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       MOVE "No data on file"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO BE999.
027300	     MOVE ZERO		 TO W200-COST W200-SELL W200-NET.
      *
      * 	       ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * 	       ³  Extracting Gross Profit Data	 ³°°
      * 	       ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´°°
      * 	       ³ Sales Record xxxxxxxxxxxxxxxxxx ³°°
      * 	       ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°
      * 		°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
      *
	     PERFORM SAVE-SCREEN-3.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 24		 TO SHADE-COL.
	     MOVE 33		 TO SHADE-WIDTH.
	     MOVE 4		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0622
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³	 Extracting Gross Profit Data   " AT 0722
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "Ã" AT 0822 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³ Sales Record" AT 0922
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "                    ³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "À" AT 1022 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.

       BE05.
	     PERFORM READ-SALES-NEXT THRU READ-SALES-EXIT.
	   IF WS-F-ERROR = 44
	       GO TO BE10.
	     DISPLAY SAL-EXT-CODE AT 0937
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     ADD SAL-COST	 TO W200-COST.
	     ADD SAL-SELL	 TO W200-SELL.
	     ADD SAL-PROFIT	 TO W200-NET.
	     GO TO BE05.

       BE10.
011320	     PERFORM RESTORE-SCREEN-3.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 21		 TO SHADE-COL.
	     MOVE 39		 TO SHADE-WIDTH.
	     MOVE 7		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0619
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³For the Period " AT 0719
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		      W35-START WITH FOREGROUND-COLOR 7 HIGHLIGHT
				     BACKGROUND-COLOR 7
		     " to " WITH FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 7
		      W35-END WITH FOREGROUND-COLOR 7 HIGHLIGHT
				   BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "Ã" AT 0819 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³   Total Sales	" AT 0919
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "                      ³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³         Cost    " AT 1019
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "                      ³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³                 ÄÄÄÄÄÄÄÄÄÄÄ" AT 1119
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "           ³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "³   Gross Profit  " AT 1219
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "                      ³"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "À" AT 1319 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.

	     DISPLAY "%" AT 1257
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 5.
	     MOVE W200-SELL	 TO W40-AMOUNT.
027800	     DISPLAY W40-AMOUNT AT 0937
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE W200-COST	 TO W40-AMOUNT.
027800	     DISPLAY W40-AMOUNT AT 1037
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE W200-NET	 TO W40-AMOUNT.
027800	     DISPLAY W40-AMOUNT AT 1237
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
027970	     COMPUTE W200-ADJ = (W200-NET / W200-SELL) * 100.
027980	     MOVE W200-ADJ	 TO W40-PERC.
027990	     DISPLAY W40-PERC AT 1250
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY "Press ANY key to continue" AT 1527.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BE999.
	     EXIT.

      /
      *    ****    STOCK SALES & GROSS PROFIT ENQUIRY
      *
027010 JA000	       SECTION 54.
027020 JA00.
027030       DISPLAY CLR-SCREEN.
027050       DISPLAY "STOCK SALES & GP ENQUIRY" AT 0227
027060		      WITH FOREGROUND-COLOR 7 HIGHLIGHT.
027070       DISPLAY "Are you sure Y/N?" AT 0620
027080		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.

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
027230	     MOVE " !"		 TO DPT-CODE.
027240	     MOVE 1		 TO WS-S6.
	     PERFORM START-AT-DEPT-CODE THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       MOVE "No departments loaded"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO JA999.

027250 JA03.
027260	     MOVE ZERO		 TO W200-DT (WS-S6) W200-CR (WS-S6).
027270	   IF WS-S6 < 297
027280	       ADD 1		 TO WS-S6
027290         GO TO JA03.
027300	     MOVE ZERO		 TO W200-DTOT W200-CTOT
027310				    W200-COST W200-SELL W200-NET.

027320 JA05.
027330	     PERFORM READ-DEPART-NEXT THRU READ-DEPART-EXIT.
	   IF WS-F-ERROR = 7
	       GO TO JA10.
	   IF (DPT-CODE = "XXXX" OR "YYYY") OR
	      (DPT-SCDE = "VAT")
	       GO TO JA05.
027340	     DISPLAY DPT-CODE AT 1027
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
           IF DPT-SALES-MTD < ZERO
	       MULTIPLY -1	 BY DPT-SALES-MTD
               COMPUTE DPT-SALES-YTD = DPT-SALES-YTD + 
		      (DPT-SALES-MTD * 2).
027370	     DISPLAY DPT-DESC WITH FOREGROUND-COLOR 3 HIGHLIGHT AT 1033.
027380       ADD DPT-SALES-MTD   TO W200-DT (1).
027390       ADD DPT-SALES-YTD   TO W200-DT (2).
027400       ADD DPT-RET-MTD     TO W200-DT (3).
027410       ADD DPT-RET-YTD     TO W200-DT (4).
027440       ADD DPT-COST-MTD    TO W200-DT (7).
027450       ADD DPT-COST-YTD    TO W200-DT (8).
027460       ADD DPT-CRET-MTD    TO W200-DT (9).
027470       ADD DPT-CRET-YTD    TO W200-DT (10).
027480       ADD DPT-DISC-MTD    TO W200-DT (5).
027490       ADD DPT-DISC-YTD    TO W200-DT (6).
027550	     GO TO JA05.

027510 JA10.
             MOVE 0601           TO CPOS.
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
028220		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT.
028230	     ACCEPT WS-OPTION AT 2532
		    WITH BACKGROUND-COLOR 3
			 FOREGROUND-COLOR 7 HIGHLIGHT AUTO.

028240 JA999.
028250       EXIT.
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
	     MOVE "PERFRM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-SALES.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
042080	     MOVE 3		 TO WS-PARKEY.
042090       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
053830	     OPEN OUTPUT SALES.
053840	   IF RUNTIME-ERROR
053880         IF FLE-LIMIT
053890		   GO TO ZA49.
053900     IF WS-STATUS NOT = "00"
053910	       MOVE 44		 TO WS-F-ERROR
053920         PERFORM OPEN-ERROR.
	     CLOSE SALES.
	     OPEN I-O SALES.
044780	     GO TO ZA999-EXIT.

039780 ZA49.
039790       DISPLAY "Too many files OPEN" AT 0812
039800		      WITH FOREGROUND-COLOR 6 HIGHLIGHT.
039810       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
039820       DISPLAY "Press any key to continue" AT 1212.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
039840	     GO TO ZA205.

039850 ZA50.
041850	   IF WS-F-ERROR = 43
041860	       MOVE "CARDEX file"
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
042820		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
		     " " WS-STATUS WITH
			   FOREGROUND-COLOR 7 HIGHLIGHT.
042830       DISPLAY "Press any key" AT 2512 
042840		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT.
042850	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3
			 FOREGROUND-COLOR 7 HIGHLIGHT AUTO.

       ZA205.
             EXIT PROGRAM.

042880 ZA999-EXIT.
042890       EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
042900 ZB000-ERROR	     SECTION 91.
      
       COPY ERRORS.PRO.

041560 DISPLAY-FILE-NAME.
041810	   IF WS-F-ERROR = 7
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
041860	       MOVE W02-SALES	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY.

       COPY DISPERR.PRO.
