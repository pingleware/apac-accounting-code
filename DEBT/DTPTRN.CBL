      $SET LINKCOUNT"468"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******	********  *******   **	  **   *
      *   **	**     **     **    **	   **	  **	**  ***   **   *
      *   **	**     **     **    **	   **	  **	**  ****  **   *
      *   **	**     **     *******	   **	  ******    ** ** **   *
      *   **	**     **     **	   **	  **   **   **	****   *
      *   **	**     **     **	   **	  **	**  **	 ***   *
      *   *******      **     **	   **	  **	**  **	  **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *      R E C U R I N G   T R A N S A C T I O N S	(CREATE)       *
      *                                                                *
      *     VERSION 8.15.00 - December 2010			       *
      * 							       *
      * This program is still under development and is scheduled to be *
      * completed during 2011. (This is a nice to have, but not a must *
      * have or critical requirement). This program will allow for the *
      * User to setup recurring transactions for an account. Provision *
      * will be made for weekly, monthly, by-monthly, quarterly, six-  *
      * monthly and anual payments.				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     DTPTRN.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 2002.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2002 - 2011
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
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000150 COPY CONTROL.SL.

000190 COPY DEBTOR.ISL.

000140 COPY DEBMEM.SL.

000210 COPY DEPART.SL.

000250 COPY GARTEE.SL.

000270 COPY PARAM.SL.

       COPY RECTRN.SL.

000310 COPY SALE.SL.

000330 COPY STOCK.SL.

000140 COPY STKMEM.SL.

      /
000390 DATA DIVISION.
000400 FILE SECTION.

       COPY APACFIDS.FDE.

000440 COPY CONTROL.FDE.

000480 COPY DEBTOR.FDE.

000140 COPY DEBMEM.FDE.

000500 COPY DEPART.FDE.

000560 COPY PARAM.FDE.

       COPY RECTRN.FD.

000620 COPY STOCK.FDE.

       COPY STKMEM.FDE.

      /
002130 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002140 77  WS-DISC	   PIC	99V99	 COMP-3.
002150 77  WS-TDISC        PIC  99V99    COMP-3.
002160 77  WS-CASH-AC      PIC  X(06).
002170 77  WS-SUB          PIC  9(04)    COMP-5.
002170 77  WS-SUPER        PIC  9(02)    COMP-5.
002180 77  WS-S1           PIC  9(04)    COMP-5.
002190 77  WS-S2           PIC  9(04)    COMP-5.
002200 77  WS-S3           PIC  9(04)    COMP-5.
002210 77  WS-S4           PIC  9(04)    COMP-5.
002220 77  WS-S5           PIC  9(04)    COMP-5.
002230 77  WS-S6           PIC  9(04)    COMP-5.
002240 77  WS-S7           PIC  9(04)    COMP-5.
002250 77  WS-S8           PIC  9(04)    COMP-5.
002260 77  WS-S9           PIC  9(04)    COMP-5.
002270 77  WS-IXS          PIC  9(04)    COMP-5.
002280 77  WS-IXS1         PIC  9(04)    COMP-5.
002290 77  WS-IXS2         PIC  9(04)    COMP-5.
002300 77  WS-IXS3         PIC  9(04)    COMP-5.
002310 77  WS-IXS4         PIC  9(04)    COMP-5.
002320 77  WS-PARKEY       PIC  9(04)    COMP-5.
002340 77  WS-DEPKEY       PIC  9(04)    COMP-5.
001690 77  WS-ENQPOS       PIC  9(06)    COMP-5.
001700 77  WS-ENQEND       PIC  9(06)    COMP-5.
002350 77  WS-KEYSTR       PIC  9(06)    COMP-5.
002380 77  WS-SALSTR       PIC  9(04)    COMP-5.
002390 77  WS-SALKEY       PIC  9(04)    COMP-5.
002400 77  WS-NETKEY       PIC  9(04)    COMP-5.
002450 77  WS-GAR          PIC  9(01) VALUE 0.  
       77  WS-NUM          PIC  Z9.
002470 77  WS-ADV-RATE     PIC  9(03)V99 COMP-3.
       77  WS-ADVALOREM    PIC	X(01).
002480 77  WS-START        PIC  9(08).
002490 77  WS-END          PIC  9(08).
002550 77  WS-PAR          PIC  9(01) VALUE 0.
002560 77  WS-INO          PIC  9(02) VALUE 0.
002570 77  WS-LINES        PIC  9(02) VALUE 0.
002580 77  WS-FIRST	   PIC	9(03) VALUE 0.
002590 77  WS-LAST	   PIC	9(03) VALUE 0.
002600 77  WS-LASTINV      PIC  9(04)    COMP-5.
002630 77  WS-L1           PIC  9(04)    COMP-5.
002750 77  WS-OPTION	   PIC	X(01).
002770 77  WS-SKIP         PIC  X(01) VALUE " ".
002780 77  WS-LOOK         PIC  X(01) VALUE "S".
002790 77  WS-ETYPE        PIC  X(01) VALUE "I".
002820 77  WS-ERROR        PIC  9(01).
002840 77  WS-CHAR1        PIC  X(01).
002850 77  WS-CHAR2        PIC  X(01).
002860 77  WS-COMP         PIC  9(01).
002890 77  WS-HEAD         PIC  X(01).
002900 77  WS-TYPE         PIC  X(01).
002910 77  WS-AGE          PIC  X(01).
002920 77  WS-PRC          PIC  X(01).
002920 77  WS-INPUT        PIC  X(01) VALUE "N".
002920 77  WS-COPY         PIC  X(01) VALUE "N".
002920 77  WS-NOSELL       PIC  X(01) VALUE "N".
       77  WS-PRNINV       PIC  X(01) VALUE "Y".
       77  WS-BYPASS       PIC  X(01) VALUE "Y".
002950 77  WS-BATCH        PIC  9(05).
002960 77  WS-CHK          PIC  X(33) VALUE
002970                          "ENTER if correct - N if incorrect".
002980 77  WS-A            PIC  X(19) VALUE "Any key to continue".
002990 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
003000 77  WS-ERR1         PIC  X(22) VALUE "Invalid Account Number".
003010 77  WS-ERR2         PIC  X(09) VALUE "No Record".
003020 77  WS-PER          PIC  Z(02)9.99.
003030 77  WS-CR           PIC  X(01).
003040 77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
003040 77  WS-BAL          PIC  S9(09)V9(02) COMP-3.
      /
       01  PRG-NAME.
	   03  FILLER	   PIC	X(08) VALUE "INV\INVF".
	   03  PRG-NUMBER  PIC	9(02) VALUE ZERO.

003050 01  WS-DB-LINE.
003060     03  WS-TOP-LINE.
003070	       05  WS-TCHAR PIC  X(80) VALUE "ÆÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍµ".
003080     03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
003100	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110         05  FILLER      PIC  X(01).
003120         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003140	   03  WS-MID-LINE.
001450	       05  WS-MCHAR PIC  X(80) VALUE "³   ³              ³
      -        "                       ³        ³         ³          ³".
	   03  WS-MINV-LINE REDEFINES WS-MID-LINE.
	       05  WS-MIV1     PIC  X(01).
	       05  WS-MIV78    PIC  X(78).
	       05  WS-MIV80    PIC  X(01).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
	       05  FILLER      PIC  X(01) VALUE "³".
003180	   03  WS-BOT-LINE.
003190	       05  WS-BCHAR PIC  X(80) VALUE "ÆÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍµ".
003200     03  WS-BOT-LNE2.
001510	       05  WS-BCR   PIC  X(80) VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
003220     03  WS-TOP-LNE.
003230	       05  WS-TCHR  PIC	 X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍµ".
003240     03  WS-T-LINE REDEFINES WS-TOP-LNE.
003250         05  FILLER  PIC X(01).
003260         05  WS-H-LINE
003270                     PIC X(78).
003280         05  FILLER  PIC X(01).
003290     03  WS-MID-LNE.
003300	       05  WS-MCHR  PIC  X(80) VALUE "³                ³
      -        "                      ³          ³         ³         ³".
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
003310	   03  WS-BOT-LNE.
003320	       05  WS-BCHR  PIC	 X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍµ".
003330	   03  WS-TP-LNE.
003340	       05  WS-TCH   PIC  X(80) VALUE "ÆÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍµ".
003350     03  WS-MD-LNE.
003360	       05  WS-MCH   PIC  X(80) VALUE "³      ³
      -        "                      ³           ³          ³       ³".
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
003370	   03  WS-BT-LNE.
003380	       05  WS-BCH   PIC  X(80) VALUE "ÆÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍµ".
	   03  WS-MM-LINE.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-SPACE    PIC  X(60) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "³".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "Í".

003480 COPY WS.WS.

003500 COPY CHAIN.WS.

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

003660 01  W02-FID.

       COPY APACFIDS.ID.

003700 COPY CONTROL.ID.

003740 COPY DEBTOR.ID.

000140 COPY DEBMEM.ID.

003760 COPY DEPART.ID.

000250 COPY GARTEE.ID.

       COPY PARAM.ID.

003840 COPY SALE.ID.

003860 COPY STOCK.ID.

000140 COPY STKMEM.ID.

003920 01  W05-TAX-RATES.
003930     03  W05-VAT-RATES.
003940         05  W05-VAT     PIC S9(03)V99 COMP-3 OCCURS 6.
003950     03  W05-TAX-RATES REDEFINES W05-VAT-RATES.
003960         05  W05-VAT-R1  PIC S9(03)V99 COMP-3.
003970         05  W05-VAT-R2  PIC S9(03)V99 COMP-3.
003980         05  W05-VAT-R3  PIC S9(03)V99 COMP-3.
003990         05  W05-VAT-R4  PIC S9(03)V99 COMP-3.
004000         05  W05-VAT-R5  PIC S9(03)V99 COMP-3.
004010         05  W05-VAT-R6  PIC S9(03)V99 COMP-3.
004020     03  W05-RATE        PIC S9(03)V99 COMP-3.
004030     03  W05-RTE REDEFINES W05-RATE
004040                         PIC S9V9(04)  COMP-3.
004050     03  W05-VATNO       PIC  X(14).

004060 01  W05-INV.
004070     03  W05-ENTRY       PIC  ZZZ9.
004080     03  FILLER          PIC  X(01) VALUE SPACES.
004090	   03  W05-DATE        PIC  Z9/99/9999.
004100     03  FILLER          PIC  X(01) VALUE SPACES.
004110     03  W05-TYPE        PIC  X(03).
004120     03  FILLER          PIC  X(01) VALUE SPACES.
004130     03  W05-REF         PIC  X(08).
004140     03  FILLER          PIC  X(01) VALUE SPACES.
004150     03  W05-VALUE       PIC  Z(06)9.99-.

004160 01  W10-STOCK.
004170     03  W10-ITEM        PIC X(14).
           03  W10-SERNO       PIC X(10).
004180     03  W10-DESC.
	       05  W10-SDSC    PIC  X(29).
	       05  FILLER      PIC  X(01).
004190	   03  W10-REMKS       PIC X(29).
004200     03  W10-IND         PIC X(01).
004210     03  W10-QUANT       PIC 9(07)V99  COMP-3.
004220     03  W10-REORD       PIC 9(07)V99  COMP-3.
004230     03  W10-COST        PIC 9(07)V99  COMP-3.
004240     03  W10-SELL        PIC 9(07)V99  COMP-3.
004250     03  W10-SPAPRC      PIC 9(07)V99  COMP-3.
004260     03  W10-LEDG        PIC 9(04)     COMP-5.
004270     03  W10-SMAN        PIC 9(02).
004280     03  W10-DISC        PIC 9(02).
004290     03  W10-ORDER       PIC 9(07)V99  COMP-3.
004300	   03  W10-DATE        PIC  X(10).
002520	   03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.
004310	   03  W10-AVRG        PIC 9(07)V99  COMP-3.
004320     03  W10-MARG        PIC 9(05)V99  COMP-3.
004330     03  W10-ACTIVE      PIC X(08).
004340     03  W10-CODE1      PIC X(14).
004350     03  W10-COD1 REDEFINES W10-CODE1.
004360         05  W10-CD1    PIC X(02).
004370         05  W10-CDE1   PIC X(12).
004380     03  W10-CODE2      PIC X(14).
004390     03  W10-COD2 REDEFINES W10-CODE2.
004400         05  W10-CD2    PIC X(02).
004410         05  W10-CDE2   PIC X(12).
004420     03  W10-ACODE      PIC X(10).
004430     03  W10-ACDE1 REDEFINES W10-ACODE.
004440         05  W10-ACRSE  PIC X(01).
004450         05  W10-ASFINE PIC X(02).
004460         05  FILLER     PIC X(07).
004470     03  W10-VSELL      PIC S9(07)V99  COMP-3.
004480     03  W10-VCASH      PIC S9(07)V99  COMP-3.
004490     03  W10-VWSALE     PIC S9(07)V99  COMP-3.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

       COPY W12.WS.

004610 01  W15-DISCOUNT.
004620     03  W15-DISC        PIC S9(03)V99 COMP-3 OCCURS 10.

004720 01  W25-CALCS.
004730     03  W25-RESULT      PIC 9(05)V99.
004740     03  W25-RESULT1 REDEFINES W25-RESULT.
004750         05  W25-WHOLE   PIC 9(03).
004760         05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
004770     03  W25-TOTAL       PIC 9(07)V99  COMP-3.
004780     03  W25-VALUE       PIC S9(07)V99 COMP-3.
004790     03  W25-CREDIT      PIC S9(07)V99 COMP-3.

       COPY W40.DBT.

       COPY W40.WS.

       COPY FUNCTION.WS.

005610 COPY W50.WS.

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
005690     03  W60-ITM.
005700         05  W60-ICHAR OCCURS 14 PIC X(01).

005830 01  W75-KEYS.
005840     03  W75-S          PIC 9(02) COMP-5.
005850     03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
005860     03  W75-DNO        PIC X(06) OCCURS 18.
           03  W75-ITEMS OCCURS 18.
               05  W75-CODE   PIC X(14).
               05  W75-SELL   PIC 9(07)V99 COMP-3.
               05  W75-VSELL  PIC 9(07)V99 COMP-3.
               05  W75-CASH   PIC 9(07)V99 COMP-3.
               05  W75-VCASH  PIC 9(07)V99 COMP-3.
               05  W75-WSALE  PIC 9(07)V99 COMP-3.
               05  W75-VWSALE PIC 9(07)V99 COMP-3.

005880 01  W75-TITLES.
005890     03  W75-DESCRIP.
005900         05  FILLER      PIC X(28)      VALUE
005910                     "Mr     Mnr    Mrs    Mev    ".
005920         05  FILLER      PIC X(28)      VALUE
005930                     "Miss   Mej    Ms     Ms     ".
005940         05  FILLER      PIC X(28)      VALUE
005950                     "Dr     Dr     Rev    Ds     ".
005960         05  FILLER      PIC X(28)      VALUE
005970                     "Prof   Prof   The HonSy Edel".
005980         05  FILLER      PIC X(28)      VALUE
005990                     "                            ".
006000     03  W75-CODES REDEFINES W75-DESCRIP.
006010         05  W75-TITLE  OCCURS 10.
006020             07  W75-ETITLE PIC X(07).
006030             07  W75-ATITLE PIC X(07).

006040 01  W80-ACCOUNT.
006050     03  W80-ACNO        PIC X(06).
006060     03  W80-CRED REDEFINES W80-ACNO.
006070         05  W80-CNO     PIC 9(05).
006080         05  W80-CAC REDEFINES W80-CNO.
006090             07  W80-CTYPE PIC 9(01).
006100             07  W80-SER   PIC 9(04).
006110         05  FILLER        PIC 9(01).
006120     03  W80-TYPE        PIC 9(01).

006270 01  W85-PASS.
006280     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006290     03  W85-ENTRY       PIC  9(02)    COMP-3.
006300     03  W85-MARG        PIC S9(02)V99.

006310 01  W85-BATCHES.
006320         05  W85-BATCH   PIC 9(05).

       COPY W90.DBT.

007010 01  W100-EDIT.
007020     03  W100-LBAL       PIC Z(06)9.99-.
007030	   03  W100-LARR       PIC Z(06)9.99.
007040     03  W100-BAL        PIC Z(08)9.99-.
007050     03  W100-CUR        PIC Z(08)9.99-.
007060     03  W100-30         PIC Z(08)9.99-.
007070     03  W100-60         PIC Z(08)9.99-.
007080     03  W100-90         PIC Z(08)9.99-.
007090     03  W100-120        PIC Z(08)9.99-.
007100     03  W100-INT        PIC Z(08)9.99-.
007110     03  W100-CRL        PIC Z(06)9.
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
               05  W100-QNT    PIC Z(05)9.99-.
	   03  W100-COST       PIC Z(06)9.99.
           03  W100-AVRG       PIC Z(06)9.99.

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
      /
007190 SCREEN SECTION.

       COPY BLANK.CRT.

007800 01  S05.
007810     03  LINE 12 COLUMN 12 VALUE "Enter Password >      <".

007820 01  S06.
007830	   03  LINE 12 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
		       PIC X(06) USING W40-CSHPASS AUTO SECURE.

       COPY STKLUP.CRT.

       COPY DEBLUP.CRT.

008990 01  S25.
	   03  LINE  7 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	      "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
009000	   03  LINE  8 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
				 "³Addresses:                 ".
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Balance  ".
009010	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-BAL.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009030	   03  LINE  9 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ 1 ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-ADD1.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  MTD      ".
009040	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING W100-CUR.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009060	   03  LINE 10 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-ADD2.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Current  ".
009070	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-CUR.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009090	   03  LINE 11 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-ADD3.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "   30 Days ".
009100	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-30.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009120	   03  LINE 12 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-ADD4.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "   60 Days ".
009130	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-60.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009150	   03  LINE 13 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PC1.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "   90 Days ".
009160	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-90.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009180	   03  LINE 14 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³ 2 ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PADD1.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  120 Days ".
009190	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-120.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009210	   03  LINE 15 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PADD2.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Interest ".
009220	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-INT.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
009240	   03  LINE 16 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PADD3.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Cr Limit ".
009250	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9 USING DEB-CRL.
009090	   03	       COLUMN 69 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "    ³".
	   03  LINE 17 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PADD4.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "  Last Pmt ".
	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z9/99/99 USING DEB-PMT.
009090	   03	       COLUMN 68 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "     ³".
	   03  LINE 18 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³   ".
	   03	       COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC  X(24) USING W40-PC2.
	   03	       COLUMN 49 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "       Amt ".
	   03	       COLUMN 60 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC Z(08)9.99- USING DEB-LAST-PMT.
009090	   03	       COLUMN 73 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "³".
	   03  LINE 19 COLUMN 21 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT VALUE
	      "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       COPY INVOICE.CRT.

009600 01  S26-QNT.
009610	   03	       COLUMN 51 FOREGROUND-COLOR 3 HIGHLIGHT
009620                           PIC Z(04)9.99 USING W10-QUANT AUTO.

009630 01  S26-PRC.
009640	   03	       COLUMN 60 FOREGROUND-COLOR 3 HIGHLIGHT
009650                           PIC Z(05)9.99 USING W10-SELL AUTO.

009660 01  S26-DSC.
009670	   03	       COLUMN 30 FOREGROUND-COLOR 3 HIGHLIGHT
009680                           PIC Z9.99 USING WS-DISC AUTO.

009690 01  S26-TDSC.
009700	   03	       COLUMN 15 FOREGROUND-COLOR 3 HIGHLIGHT
009710                           PIC Z9.99 USING WS-TDISC AUTO.

009720 01  S26-AVGE.
009730	   03	       COLUMN 28 FOREGROUND-COLOR 3 HIGHLIGHT
009740                           PIC Z(04)9.99 USING STK-AVGE AUTO.

009870 01  S28.
009880	   03  LINE 25 COLUMN 65 FOREGROUND-COLOR 7 HIGHLIGHT
009890                           PIC ZZZ9 USING WS-SALKEY AUTO.

       01  S30.
	   03  LINE  9 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ú".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 PIC X(50) USING WS-SRT-LINE.
	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "¿".
007450	   03  LINE 10 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(18) FROM WS-SPACE.
	   03	       COLUMN 34 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "CLIENT DETAILS".
	   03	       COLUMN 48 BACKGROUND-COLOR 2
				 PIC  X(18) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
007450	   03  LINE 11 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "Name    >".
	   03	       COLUMN 65 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".     
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
007450	   03  LINE 12 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "Address >".
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
	   03	       COLUMN 56 BACKGROUND-COLOR 2
				 PIC  X(10) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
007450	   03  LINE 13 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(08) FROM WS-SPACE.
	   03	       COLUMN 24 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE ">".
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
	   03	       COLUMN 56 BACKGROUND-COLOR 2
				 PIC  X(10) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
007450	   03  LINE 14 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(08) FROM WS-SPACE.
	   03	       COLUMN 24 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE ">".
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
	   03	       COLUMN 56 BACKGROUND-COLOR 2
				 PIC  X(10) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
007450	   03  LINE 15 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(08) FROM WS-SPACE.
	   03	       COLUMN 24 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE ">".
	   03	       COLUMN 55 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
	   03	       COLUMN 56 BACKGROUND-COLOR 2
				 PIC  X(10) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
007450	   03  LINE 16 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 PIC  X(08) FROM WS-SPACE.
	   03	       COLUMN 24 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE ">".
	   03	       COLUMN 29 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
	   03	       COLUMN 30 BACKGROUND-COLOR 2
				 PIC  X(36) FROM WS-SPACE.
007460	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "³".
	   03  LINE 17 COLUMN 15 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "À".
	   03	       COLUMN 16 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
                                 PIC X(50) USING WS-SRT-LINE.
	   03	       COLUMN 66 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 6 HIGHLIGHT
				 VALUE "Ù".

       01  S31.
	   03  LINE 11 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(40) USING W40-NAME AUTO.
	   03  LINE 12 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(30) USING W40-ADD1 AUTO.
	   03  LINE 13 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(30) USING W40-ADD2 AUTO.
	   03  LINE 14 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(30) USING W40-ADD3 AUTO.
	   03  LINE 15 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(30) USING W40-ADD4 AUTO.
	   03  LINE 16 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(04) USING W40-PC1 AUTO.

009930 01  S36.
009940	   03  FOREGROUND-COLOR 3 HIGHLIGHT
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
           "   M E M O / R E M A R K S / S P E C I F I C A T I O N S    
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

       01  ERROR-LINE.
	   03  LINE 25 COLUMN  2 BACKGROUND-COLOR WS-TEMPBG
				 FOREGROUND-COLOR WS-FGRND-5
				 HIGHLIGHT
				 PIC  X(48) FROM WS-ERR-MES.
      /
010220 PROCEDURE DIVISION USING WS-PARID WS-USER-ID WS0-PROGRAMS.
010230 AA000-MAIN              SECTION.
010240 AA000-INIT.
010250       PERFORM ZA000-INIT.
016330	     PERFORM BC000.
	     CLOSE SALE.
013280 AZ100.
013290       EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
010460 AA50D.
010470       MOVE "D" TO WS-LOOK.
010480 AA50.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
010740	   IF WS-LOOK = "D"
010750         PERFORM DEBTOR-LOOKUP
010760     ELSE
010770         PERFORM STOCK-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
010890	     MOVE "S" TO WS-LOOK.
010900 AA99.
010910       EXIT.

       COPY FUNCTION.CRT.

010930 COPY LOCKED.RC2.

010950 COPY CLEAR.CRT.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
010990 AA300              SECTION.
011000 AA301.
011010	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011020		     " Item code/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011030		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011040		     " Alt code/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011060		     " Description/ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011070		     "F4" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011080		     " Cross Reference Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
011090 AA305.
011100       EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
011110 AC000              SECTION.

011150 COPY CONTROL.RD.

011190 COPY DEBTOR.RD.

000140 COPY DEBMEM.RD.

011210 COPY DEPART.RD.

000250 COPY GARTEE.RD.

011270 COPY PARAM.RD.

011290 COPY SALE.RD.

000140 COPY STKMEM.RD.

011310 COPY STOCK.RD.

      /
011350 AD000-WRITE             SECTION.

011390 COPY CONTROL.WR.

011430 COPY DEBTOR.WR.

011450 COPY DEPART.WR.

000250 COPY GARTEE.WR.

011510 COPY PARAM.WR.

011530 COPY SALE.WR.

011550 COPY STOCK.WR.

011610 COPY STOCK.LUP.

011630 COPY DEBTOR.ILU.

      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
013300 CA000         SECTION.
013310 CA00.
013320       MOVE 14             TO W50-LENGTH.
013330 CA05.
013340       MOVE W10-ITEM       TO W50-DATA.
013350     IF W50-1 = SPACE
013360         MOVE W50-13       TO W10-ITEM
013370         GO TO CA05.      
	     CALL "CBL_TOUPPER" USING W50-DATA
				BY VALUE W50-LENGTH
				RETURNING W50-STATUS.
013430	     MOVE W50-ITEM	 TO W10-ITEM.
013440 CA06.
013450       MOVE W50-ITEM       TO STK-CODE.
013460 CA10.
013470       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
013480     IF WS-F-ERROR = 0
013490         PERFORM CA500.
013500 CA99.
013510       EXIT.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   
      *           A L T E R N A T E   C O D E
      *
013520 CA100         SECTION.
013530 CA101.
013540       MOVE 10             TO W50-LENGTH.
013550 CA105.
013560       MOVE W10-ACODE      TO W50-DATA.
013570     IF W50-1 = SPACE
013580         MOVE W50-9        TO W10-ACODE
013590         GO TO CA105.      
	     CALL "CBL_TOUPPER" USING W50-DATA
				BY VALUE W50-LENGTH
				RETURNING W50-STATUS.
013650	     MOVE W50-ACODE	 TO STK-ACODE W10-ACODE.
013660 CA110.
013670       PERFORM START-AT-ALT-CODE THRU READ-STOCK-EXIT.
013680     IF WS-F-ERROR = 22
013690         GO TO CA119.
013700       PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
013710     IF WS-F-ERROR = 0
013720         PERFORM CA500.
013730 CA119.
013740       EXIT.
      *
      *    ****    Read debtor record - using account number.
      *
013750 CA155-GET-DEBTOR  SECTION 2.
013760 CA155.
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
013920 CA160-EXIT.
013930       EXIT.
      *
013940 CA165-NAME        SECTION 2.
013950 CA165.
013960       UNSTRING W60-NAME DELIMITED BY "*" OR "!"
013970                           INTO W40-SURNAME 
013980                                W40-FNAME 
013990                                W40-SNAME
014000       OVERFLOW
014010           MOVE 1          TO WS-ERROR.
014020 CA170-EXIT.
014030       EXIT.
      *
014040 CA175-ADDRESS     SECTION 2.
014050 CA175.
014060       UNSTRING W60-ADDRESS DELIMITED BY "*"
014070                           INTO W40-ADD1 
014080                                W40-ADD2 
014090                                W40-ADD3 
014100                                W40-ADD4
014110       OVERFLOW
014120           MOVE 1          TO WS-ERROR.
014130 CA175-EXIT.
014140       EXIT.
      *
014150 CA185-ADDRESS     SECTION 2.
014160 CA185.
014170       UNSTRING W60-ADDRESS1 DELIMITED BY "*"
014180                           INTO W40-PADD1 
014190                                W40-PADD2 
014200                                W40-PADD3 
014210                                W40-PADD4
014220       OVERFLOW
014230           MOVE 1          TO WS-ERROR.
014240 CA185-EXIT.
014250       EXIT.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   X R E F
      *
014260 CA200         SECTION.
014270 CA201.
014280       MOVE 14             TO W50-LENGTH.
014290 CA205.
014300       MOVE W10-CODE2      TO W50-DATA.
014310     IF W50-1 = SPACE
014320         MOVE W50-13       TO W10-CODE2
014330         GO TO CA205.      
	     CALL "CBL_TOUPPER" USING W50-DATA
				BY VALUE W50-LENGTH
				RETURNING W50-STATUS.
014390	     MOVE W50-ITEM	 TO W10-CODE2.
014400 CA206.
014410       MOVE W50-ITEM       TO STK-XREF.
014420 CA210.
014430       PERFORM START-AT-STOCK-XREF THRU READ-STOCK-EXIT.
014440     IF WS-F-ERROR = 22
014450         GO TO CA299.
014460       PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
014470       PERFORM CA500.
014480 CA299.
014490       EXIT.
      *
      *    ****    Calculate the inclusive prices
      *
014500 CA500         SECTION.
014510 CA505.
014520     IF STK-WSALE = ZERO
014530         IF STK-CASH NOT = ZERO
014540             MOVE STK-CASH TO STK-WSALE
014550         ELSE
014560             MOVE STK-SELL TO STK-WSALE.
014570     IF STK-CASH = ZERO
014580         MOVE STK-SELL     TO STK-CASH.
014590       MOVE STK-CASH       TO W10-VCASH
014600       MOVE STK-SELL       TO W10-VSELL
014610       MOVE STK-WSALE      TO W10-VWSALE.
014620       MOVE ZERO           TO W05-RATE.
014700     IF STK-TAX > 0
014710	       MOVE W05-VAT (WS-VAT-SUB)
014720                           TO W05-RATE.
014730       COMPUTE W10-VCASH ROUNDED 
014740             = W10-VCASH + (W10-VCASH * W05-RTE).
014750       COMPUTE W10-VWSALE ROUNDED 
014760             = W10-VWSALE + (W10-VWSALE * W05-RTE).
014770       COMPUTE W10-VSELL ROUNDED 
014780             = W10-VSELL + (W10-VSELL * W05-RTE).
014790 CA599.
014800       EXIT.

      /
       BB000               SECTION.
       BB00.
             DISPLAY "Enter client details " AT 2545
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Y" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "/" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "? > <" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
             MOVE "Y"            TO WS-OPTION.
       BB05.
020190	     ACCEPT WS-OPTION AT 2572
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
020250	   IF NOT (WS-OPTION = "N" OR "Y")
               GO TO BB05.
           IF WS-OPTION = "N"
               GO TO BB20.
	     DISPLAY CLEAR-L25.
	     PERFORM SAVE-SCREEN.
	     MOVE SPACES	 TO W40-NAME W40-ADD1 W40-ADD2
                                    W40-ADD3 W40-ADD4 W40-PC1.
             MOVE 0915           TO CPOS.
       BB10.
             DISPLAY 
                 "                                                    "
                 AT CPOS.
           IF CLIN < 17
               ADD 1             TO CLIN
               GO TO BB10.
	     MOVE 10		 TO SHADE-ROW.
	     MOVE 17		 TO SHADE-COL.
	     MOVE 50		 TO SHADE-WIDTH.
	     MOVE 8		 TO SHADE-LINES.
             DISPLAY S30. 
	     PERFORM SCREEN-SHADOW.
       BB15.
             ACCEPT S31.
020180	     DISPLAY WS-CHK AT 2518
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
020190	     ACCEPT WS-OPTION AT 2552
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
020250	   IF WS-OPTION = "N"
               GO TO BB15.
       BB20.
	     DISPLAY CLEAR-L25.
       BB29.
             EXIT.
      /
       BB030           SECTION.
       BB30.
             MOVE ZERO           TO WS-ERROR.
             MOVE 1              TO WS-S1.
             MOVE W10-QUANT      TO WS-S2.
             MOVE SPACES         TO W43-SERIALS.
	     PERFORM SAVE-SCREEN.
       BB40.
	     DISPLAY "Serial No:" AT LINE LIN COLUMN 51
		      WITH FOREGROUND-COLOR 14
			   BACKGROUND-COLOR 7
		     "            (   )" WITH FOREGROUND-COLOR 15
					      BACKGROUND-COLOR 7.
       BB41.
             MOVE WS-S1          TO W100-SER.
	     DISPLAY W100-SER AT LINE LIN COLUMN 74
		     WITH FOREGROUND-COLOR 15
			  BACKGROUND-COLOR 7.
       BB42.
	     ACCEPT W43-SERNO(WS-S1) AT LINE LIN COLUMN 62
		    WITH FOREGROUND-COLOR 0
			 BACKGROUND-COLOR 7 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W43-SERIALS
		     GO TO BB45
	       END-EVALUATE.
	     DISPLAY CLEAR-L25.
020180	     DISPLAY WS-CHK AT 2518
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
020190	     ACCEPT WS-OPTION AT 2552
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     DISPLAY CLEAR-L25.
020250     IF WS-OPTION = "N"
               GO TO BB42.
           IF W43-SERNO (WS-S1) = SPACES
               GO TO BB42.
             MOVE STK-CODE       TO GUA-CODE.
             MOVE W43-SERNO (WS-S1)
                                 TO GUA-SERIAL.
             PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR = ZERO
               DISPLAY "Serial number duplicated" AT 2502
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
               GO TO BB42.
       BB44.
           IF WS-S1 < WS-S2
               ADD 1             TO WS-S1
               GO TO BB41.
       BB45.
	     PERFORM RESTORE-SCREEN.
       BB49.
             EXIT.
      /
017970 BB100               SECTION.
017980 BB100-MARG.
017990       MOVE ZERO TO WS-ERROR.
018010     IF W90-TAX = SPACES
018020         IF STK-TAX NOT = ZERO
018030		   COMPUTE W10-SPAPRC ROUNDED = (W10-SPAPRC /
018040                (100.0000 + W05-RATE)) * 100. 
           IF (WS-PRC = "R") AND (STK-SELL NOT > W10-SPAPRC)
015720         GO TO BB110
           ELSE
           IF (WS-PRC = "C") AND (STK-CASH NOT > W10-SPAPRC)
015720         GO TO BB110
           ELSE
018050     IF STK-WSALE NOT > W10-SPAPRC
018060         GO TO BB110.
018070       COMPUTE W10-MARG = ((W10-SPAPRC - W10-COST) / W10-SPAPRC)
018080                          * 100.00.
018090     IF W10-MARG NOT < W85-MARG
018100         GO TO BB110.
018110	     DISPLAY "Margin less than allowed - Override " AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		      "Y" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		      "/" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		      "N" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT.
018130 BB105.
018140	     ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
018200	   IF NOT (WS-OPTION = "N" OR "Y")
018210         GO TO BB105.
018220     IF WS-OPTION = "N"
018230         MOVE 1 TO WS-ERROR
018240         GO TO BB110.
018250 BB106.
018260       DISPLAY "Enter password >      <" AT 2551
018270		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
018280	     ACCEPT W40-CSHPASS AT 2567
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
018290			 AUTO NO-ECHO.
018300       MOVE 6 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W40-CSHPASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
018360	     MOVE 1 TO WS-LENGTH.
       BB107.
           IF W40-CSHPASS = "!@$^*)"
               MOVE 10           TO WS-SUPER
           ELSE
           IF W40-CSHPASS = W85-SUPER (1)
               MOVE 1            TO WS-SUPER
           ELSE
           IF W40-CSHPASS = W85-SUPER (9)
               MOVE 9            TO WS-SUPER
           ELSE
018470         MOVE 1            TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
           GO TO BB110.
       BB108.
           IF W40-CSHPASS = "!#%&()"
               MOVE 10           TO WS-SUPER
           ELSE
           IF W40-CSHPASS = W85-SUPER (1)
               MOVE 1            TO WS-SUPER
           ELSE
           IF W40-CSHPASS = W85-SUPER (6)
               MOVE 6            TO WS-SUPER
           ELSE
018470         MOVE 1            TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
018480 BB110.
018490       EXIT.
      /
      *    ****    I N V O I C E   A N D   C A S H   S A L E
      *
018500 BC000	     SECTION 5.
018510 BC00.
      *
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *
018520       MOVE 5              TO WS-PARKEY.
018530       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
018540       MOVE PAR-PW         TO W85-PASS. 
018570 BC05.
      *
      *    ****    I N V O I C E   S C R E E N
      *
018580       DISPLAY CLR-SCREEN.
018590       DISPLAY S26.
018620       MOVE 8              TO LIN.
018630       PERFORM BC115 UNTIL LIN > 18.
018640       MOVE W12-TODAY      TO W90-DTE.
018650       DISPLAY W90-DTE AT 0572 WITH FOREGROUND-COLOR 11.
018660       MOVE ZERO           TO W90-BDT W90-NONTAX W90-LDISC 
018670                              W90-BAL W90-TAXVAL W90-SALES
018680                              W90-CUR W90-EXCTOT W90-ADVTAX.
018690       MOVE 1              TO WS-FIRST WS-LAST.
	     MOVE 1		 TO WS-VAT-SUB.
018700       MOVE SPACES         TO W90-TAX.
018710       MOVE "I"            TO WS-CR.
             MOVE "N"            TO WS-INPUT.
018720 BC10.
      *
      *    ****    D E B T O R   A C C O U N T   N U M B E R
      *
018770       MOVE SPACES         TO W80-ACNO.
018780 BC11.
018790	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
018800		     " to do Debtor Lookup"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
018810       ACCEPT S36 AT 0308.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W80-ACNO
		     GO TO BC11A
016550		 WHEN F1-KEY
016560		     PERFORM AA50D THRU AA50
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC11
	       END-EVALUATE
018840	       DISPLAY S36 AT 0308
018850         IF W80-ACNO = SPACES
018860             GO TO BC11.
018870	     DISPLAY CLEAR-L25.
       BC11A.
018880	   IF W80-ACNO = SPACES
018900         GO TO BC599.
	     MOVE 6		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W80-ACNO
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
018910 BC12.
018920	     DISPLAY CLEAR-L25.
018930       PERFORM CA155-GET-DEBTOR.
018940     IF WS-ERROR = 1
018950	       DISPLAY "No record" AT 2540
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
018960         GO TO BC11.
018970     IF WS-ERROR = 2
018980         DISPLAY "Invalid account" AT 2540
018990			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
019000         GO TO BC11.
019010 BC15.
      *
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *
	     MOVE DEB-ACNO	 TO W40-ACNO.
019020	     MOVE DEB-NAME	 TO W40-NAME.
019030     IF W80-TYPE = 1
019040         INSPECT W40-NAME REPLACING ALL "*" BY " "
019050         INSPECT W40-NAME REPLACING FIRST "!" BY " ".
019060       DISPLAY W40-NAME AT 0320 WITH FOREGROUND-COLOR 11.
019070       COMPUTE W100-CUR = DEB-BAL - (DEB-CUR + DEB-30 +
019080                          DEB-60 + DEB-90 + DEB-120 + DEB-INT).
	     PERFORM BC189 THRU BC215.
      *       DISPLAY "Press any key to continue" AT 2502
      *		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
      *	     ACCEPT WS-OPTION AT 2528
      *		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     DISPLAY CLEAR-L25.
      *
      *    ****    I S   T H I S   T H E   C O R R E C T   A C C O U N T
      *
             PERFORM BC18-MEMO THRU BC18E.
	     PERFORM SAVE-SCREEN-2.
	     MOVE 8		 TO SHADE-ROW.
	     MOVE 23		 TO SHADE-COL.
	     MOVE 51		 TO SHADE-WIDTH.
	     MOVE 12		 TO SHADE-LINES.
019090	     DISPLAY S25.
	     PERFORM SCREEN-SHADOW.
      *
      *    ****   C A T E G O R Y   8 0   -  S U S P E N D E D   A / C
      *
           IF DEB-CAT = 80
               DISPLAY "This account has been suspended - Press any key"
			AT 2502
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
	       ACCEPT WS-OPTION AT 2550
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO
               GO TO BC05.
       BC16.
019100	     DISPLAY "C" AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
019110		     "ontinue "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
019120		     "R" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
019130		     "ekey or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
019140		     "E" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
019150		     "xit > < - Press "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F9" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " for Memo/Remarks"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
             MOVE "C"            TO WS-OPTION.
019110 BC18.
019120       ACCEPT WS-OPTION AT 2536 
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN NOT F9-KEY GO TO BC19
	       END-EVALUATE
	   ELSE
	       GO TO BC19.
       BC18-MEMO.
019250	     DISPLAY CLEAR-L25.
             MOVE W80-ACNO       TO DME-AC.
             MOVE 1              TO DME-SEQ.
             PERFORM READ-DEBMEM THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR NOT = ZERO
               GO TO BC18E.
             INITIALIZE W200-MEM-FLDS.
       BC18A.
             MOVE DME-SEQ        TO WS-S1.
             MOVE DME-DATA       TO W200-MEM (WS-S1).
             PERFORM READ-DEBMEM-NEXT THRU READ-DEBMEM-EXIT.
           IF WS-F-ERROR = ZERO
               IF DME-AC = W80-ACNO
                   GO TO BC18A.
	     PERFORM SAVE-SCREEN.
	     MOVE 7		 TO SHADE-ROW.
	     MOVE 19		 TO SHADE-COL.
	     MOVE 60		 TO SHADE-WIDTH.
	     MOVE 15		 TO SHADE-LINES.
	     DISPLAY S-MEM.
	     DISPLAY S-DEB-MEM.
	     PERFORM SCREEN-SHADOW.
	     DISPLAY SA-MEM.
021280       DISPLAY "ENTER to continue" AT 2524
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
021290	     ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     PERFORM RESTORE-SCREEN.
       BC18E.
             EXIT.
       BC18-E.
             GO TO BC16.
       BC19.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
019230	   IF NOT (WS-OPTION = "C" OR "E" OR "R")
019240         GO TO BC18.
	     PERFORM RESTORE-SCREEN-2.
019270 BC20.
019280	     DISPLAY CLEAR-L25.
019290     IF WS-OPTION = "R"
019300         GO TO BC05.
019310     IF WS-OPTION = "E"
019330         GO TO BC599.
019340       MOVE "Y"            TO WS-OPTION.
      *
      *    ****    C H E C K   A R R E A R S   &   C R E D I T   L I M I T
      *
019350     IF (DEB-60 > ZERO) OR (DEB-90 > ZERO) OR (DEB-120 > ZERO)
019360         DISPLAY "Arrears > 30 days - Override Y/N" AT 2501
019370			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
019380	       ACCEPT WS-OPTION AT 2534
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
019390			   UPDATE AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
019450	       IF WS-OPTION = "Y"
019460             MOVE ZERO     TO WS-ERROR
019470             PERFORM BB106
019470             PERFORM BB108
019480             IF WS-ERROR = 1
019490                 MOVE "N"  TO WS-OPTION.                       
019500	     DISPLAY CLEAR-L25.
019510     IF WS-OPTION NOT = "Y"
019520         GO TO BC05.
019530     IF DEB-CRL > ZERO
019540         IF DEB-BAL > DEB-CRL
019550             DISPLAY "Credit limit exceded - Override Y/N" AT 2501
019560			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
019570             ACCEPT WS-OPTION AT 2538
019580			  WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
			       UPDATE AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
019640	       IF WS-OPTION = "Y"
019650             MOVE ZERO     TO WS-ERROR
019660             PERFORM BB106
019470             PERFORM BB108
019670             IF WS-ERROR = 1
019680                 MOVE "N"  TO WS-OPTION.                       
019690	     DISPLAY CLEAR-L25.
019700     IF WS-OPTION NOT = "Y"
019710         GO TO BC05.   
             MOVE DEB-BAL        TO WS-BAL.
           IF NOT VALID-PRICE
               MOVE "R"          TO DEB-PRICE.
             MOVE DEB-PRICE      TO WS-PRC.
019920 BC25.
019930       MOVE SPACES         TO W90-ORD W90-REG.
019940 BC30.
A19950	     DISPLAY CLEAR-L25.
019960       ACCEPT W90-ORD AT 0368 WITH FOREGROUND-COLOR 11 
019970                                   UPDATE AUTO.
      *
      *    ****    S A L E S M A N   N U M B E R
      *
019990	     MOVE DEB-SMAN	 TO W10-SMAN.
020020 BC35.
020030       ACCEPT W10-SMAN AT 0411 WITH FOREGROUND-COLOR 11
020040                                    UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BC05
	       END-EVALUATE.
020050     IF W10-SMAN > 50
020060         GO TO BC35.
             MOVE SPACES         TO W40-ASSIST.
020070     IF W10-SMAN = ZERO
020080	       GO TO BC36.
020090       ADD 100 W10-SMAN    GIVING WS-PARKEY.
020100       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
020110     IF PAR-SALESMAN = SPACES
020120         DISPLAY "Invalid Salesman Number" AT 2518
020130			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
020140         GO TO BC35.
020150	     DISPLAY CLEAR-L25.
020160	     DISPLAY PAR-SALESMAN AT 0420
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT.
             MOVE PAR-SALESMAN   TO W40-ASSIST.
       BC36.
	   IF DEB-B-TEL = SPACES
	       MOVE DEB-H-TEL	 TO W40-TEL
	   ELSE
	       MOVE DEB-B-TEL	 TO W40-TEL.
	    DISPLAY W40-TEL AT 0466
		    WITH FOREGROUND-COLOR 3 HIGHLIGHT.
020170 BC40.
020300     IF DEB-INV-TYPE = "E" 
020310         MOVE "Y"      TO WS-OPTION
020320     ELSE
020330         MOVE "N"      TO WS-OPTION.
020430	     DISPLAY "TAX Exclusive (" AT 2520
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
020440		     "Y" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
020450		     "/" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
020460		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
020470		     ") or e"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
020480		     "X" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
020490		     "port?"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       BC42.
020500	     ACCEPT WS-OPTION AT 2551
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
020510			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BC35
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "Y" OR "N" OR "X")
               GO TO BC42.
020580       MOVE WS-OPTION      TO W90-EXP.
           IF WS-OPTION = "Y"
               MOVE "E"          TO W90-EXP.
020600     IF WS-OPTION = "N"
020610         MOVE SPACES       TO W90-TAX
               MOVE "I"          TO W90-EXP
020630     ELSE
020640	       MOVE W05-VATNO	 TO W90-TAX.
	     MOVE DEB-CONTACT	 TO W90-CONTACT.
020660	     ACCEPT W90-CONTACT AT 0529
		    WITH FOREGROUND-COLOR 3 HIGHLIGHT
			 UPDATE AUTO.
       BC43.
	     DISPLAY CLEAR-L25.
	   IF WS-ADVALOREM = "N"
	       MOVE "N" 	 TO W90-ADV
	       GO TO BC45.
	     DISPLAY "Ad Valorm tax (" AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Y" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "/" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     ")? > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
	     MOVE "N"		 TO WS-OPTION.
       BC44. 
020370	     ACCEPT WS-OPTION AT 2534
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
020380			 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BC40
	       END-EVALUATE.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "Y" OR "N")
               GO TO BC44.
             MOVE WS-OPTION      TO W90-ADV.
020550	     DISPLAY CLEAR-L25.
020750 BC45.
020760       ACCEPT W90-REG AT 0554 WITH FOREGROUND-COLOR 11
020770                                   UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BC43
	       END-EVALUATE.
020780	     DISPLAY CLEAR-L25.
020890	     MOVE ZERO		 TO W90-DB W90-CR.
020900 BC50.
020910       MOVE 8              TO LIN.
020920 BC55.
020930       MOVE SPACES         TO W10-DESC  W10-ITEM W10-SERNO.
020940       MOVE ZERO           TO W10-SELL WS-SUPER.
020940       MOVE 1              TO W10-QUANT.
020950 BC60.
020960       PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
020970	     ACCEPT W10-ITEM AT LINE LIN COLUMN 6
		    WITH FOREGROUND-COLOR 11 UPDATE AUTO.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     MOVE SPACES TO W10-ITEM
		     GO TO BC60A
		 WHEN F1-KEY
		     MOVE "I"	 TO WS-ETYPE
		 WHEN F2-KEY
		     MOVE "A"	 TO WS-ETYPE
		 WHEN F3-KEY
		     MOVE "D"	 TO WS-ETYPE
		 WHEN F4-KEY
		     MOVE "X"	 TO WS-ETYPE
		 WHEN OTHER
		     CALL X"E5"
		     GO TO BC60
	       END-EVALUATE
	       MOVE "S" 	 TO WS-LOOK
016560	       PERFORM AA50
021010	       DISPLAY W10-ITEM AT LINE LIN COLUMN 6
		       WITH FOREGROUND-COLOR 11
021020         IF W10-ITEM = SPACES
021030             GO TO BC60.
       BC60A.
021040	   IF W10-ITEM = SPACES
021050         SUBTRACT 1        FROM WS-LAST
021060         GO TO BC120.
       BC61.
021070       PERFORM CA000.
021080     IF WS-F-ERROR = 22
	       MOVE "Z" 	 TO WS-ETYPE
018620	       PERFORM AA50
018630	       DISPLAY W10-ITEM AT LINE LIN COLUMN 6
		       WITH FOREGROUND-COLOR 11
018640         IF W10-ITEM = SPACES
018720             GO TO BC60
               ELSE
                   GO TO BC61.
021110       MOVE WS-LAST        TO WS-SALKEY.
021120       INITIALIZE SAL-RECORD1.
021130       MOVE WS-LAST        TO SAL-CODE.
      *
      *    ****    I T E M   N U M B E R   O N   I N V O I C E
      *
021140	     DISPLAY WS-LAST AT LINE LIN COLUMN 2
		     WITH FOREGROUND-COLOR 11.
021150       MOVE STK-CODE       TO SAL-ITEM.
021160	     DISPLAY CLEAR-L25.
      *
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *
021170 BC65.
021180     IF STK-IND = 3
021190         MOVE ZERO         TO STK-AVGE
021200	       ACCEPT W10-SDSC AT LINE LIN COLUMN 21
		      WITH FOREGROUND-COLOR 11 UPDATE AUTO
021220         DISPLAY "Index Unit cost" AT 2512
021230			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
021240         ACCEPT S26-AVGE AT LINE 25
021250     ELSE
021260         MOVE STK-DESC     TO W10-DESC
021270	       DISPLAY W10-SDSC AT LINE LIN COLUMN 21
		       WITH FOREGROUND-COLOR 11.
021280     IF STK-IND = 3
021290         IF STK-AVGE = ZERO
021300             DISPLAY "Unit cost is zero" AT 2512
021310			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
021320             GO TO BC65.
021330	     DISPLAY CLEAR-L25.
      *
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *
021340       MOVE W10-ITEM       TO SAL-ITEM.
021350       MOVE W10-DESC       TO SAL-MSER.
021360       MOVE STK-AVGE       TO SAL-COST W10-COST.
      *
      *    ****    G E T   Q U A N T I T Y
      *
       BC66.
021370       ACCEPT S26-QNT AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
021390		     DISPLAY WS-MIV78 AT LINE LIN COLUMN 2
			     WITH FOREGROUND-COLOR 3
021400		     GO TO BC55
		 END-EVALUATE.
	   IF (W10-QUANT = ZERO)
021390	       DISPLAY WS-MIV78 AT LINE LIN COLUMN 2
		       WITH FOREGROUND-COLOR 3
021400         GO TO BC55.
021410	     DISPLAY CLEAR-L25.
             MOVE ZERO           TO WS-ERROR.
           IF STK-GUAR NOT = ZERO
               PERFORM BB030
           ELSE
               MOVE SPACES       TO W43-SERNO (1).
           IF WS-ERROR = 1
               GO TO BC66.
      *
      *    ****    G E T   S E L L I N G   P R I C E   (ACCOUNT OR CASH)
      *
021420     IF STK-IND < 3
021550         IF W90-TAX NOT = SPACES
021560             IF WS-PRC = "R"
021570                 MOVE STK-SELL 
021580                           TO W10-SELL
021590             ELSE
021600             IF WS-PRC = "C"
021610                 MOVE STK-CASH 
021620                           TO W10-SELL
021630             ELSE
021640                 MOVE STK-WSALE 
021650                           TO W10-SELL
021660         ELSE
021670             IF WS-PRC = "R"
021680                 MOVE W10-VSELL 
021690                           TO W10-SELL
021700             ELSE
021710             IF WS-PRC = "C"
021720                 MOVE W10-VCASH 
021730                           TO W10-SELL
021740             ELSE
021750                 MOVE W10-VWSALE 
021760                           TO W10-SELL.
	   IF NOT (PRG-NUMBER = 03)
	       GO TO BC69.
	     MOVE STK-COST TO W100-COST.
             MOVE STK-AVGE TO W100-AVRG.
       BC68.
021550     IF (W90-TAX NOT = SPACES) AND (STK-TAX > 0)
014710         MOVE W05-VAT (STK-TAX)
014720                           TO W05-RATE
               COMPUTE WS-PER = ((W10-SELL - ((W10-SELL / 
                      (100.0000 + W05-RATE)) * W05-RATE)
                       - STK-COST) / STK-COST) * 100.000 + .005
           ELSE
               COMPUTE WS-PER = ((W10-SELL - STK-COST) / STK-COST)
                                  * 100.000 + .005.
             DISPLAY "Cost: " AT 2501 
                     W100-COST WITH FOREGROUND-COLOR 11
                     " Average: " 
                     W100-AVRG WITH FOREGROUND-COLOR 11
                     " Markup: " WS-PER WITH FOREGROUND-COLOR 11.
       BC69.
021790	     MOVE W10-SELL	 TO W10-SPAPRC.
021770 BC70.
021780       ACCEPT S26-PRC AT LINE LIN.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
		     GO TO BC66
	       END-EVALUATE.
	   IF NOT (PRG-NUMBER = 03)
	       GO TO BC74.
	     PERFORM BC68.
             DISPLAY "C" AT 2554 WITH FOREGROUND-COLOR 14
                     "ontinue or " WITH FOREGROUND-COLOR 7
                     "R" WITH FOREGROUND-COLOR 14
                     "ekey > <" WITH FOREGROUND-COLOR 7.
             MOVE "C"            TO WS-OPTION.
       BC71.
             ACCEPT WS-OPTION AT 2573 
                    WITH FOREGROUND-COLOR 15 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
017330	   IF NOT (WS-OPTION = "C" OR "R")
	       GO TO BC71.
           IF WS-OPTION = "R"
017340	       GO TO BC70.
021850	     DISPLAY CLEAR-L25.
021790	     MOVE W10-SELL	 TO W10-SPAPRC.
       BC74.
	   IF W10-SELL < W10-SPAPRC
021790	       MOVE W10-SELL	 TO W10-SPAPRC
021810	       PERFORM BB100
021820	       IF WS-ERROR = 1
021830		   MOVE ZERO	 TO WS-ERROR
021840		   GO TO BC70.
021850	     DISPLAY CLEAR-L25.
021860 BC75.
021870       MOVE W10-SELL       TO SAL-SELL.
021880 BC76.
021890     IF STK-IND > 0
021900         GO TO BC80.
      *
      *    ****    C H E C K   S T O C K   Q U A N T I T I E S
      *
021910     IF STK-QUANT = ZERO
021920         DISPLAY "No stock - Continue (Y/N) > <" AT 2512
021930			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
021940         DISPLAY "'Y' will make stock holding negative" AT 2542
021950			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
021960	       ACCEPT WS-OPTION AT 2539
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO
	       CALL "CBL_TOUPPER" USING WS-OPTION
				  BY VALUE WS-LENGTH
				  RETURNING WS-STATUS
022020	       DISPLAY CLEAR-L25
022030         IF WS-OPTION = "N"
022040             MOVE W10-QUANT 
022050                           TO W10-REORD
022060             GO TO BC55.
022070     IF (W10-QUANT NOT > STK-QUANT) OR
022080        (STK-QUANT NOT > ZERO)
022090         GO TO BC80.
022100       SUBTRACT STK-QUANT FROM W10-QUANT 
022110                           GIVING W10-REORD.
022120       MOVE STK-QUANT      TO W100-BAL.
022130       DISPLAY "Insufficient Stock " AT 2502
022140		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
022150		      W100-BAL
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
022160		      " Accept (Enter/N) ?"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
022170	     ACCEPT WS-OPTION AT 2552
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
022230	     DISPLAY CLEAR-L25.
022240     IF WS-OPTION = "N"
022250         MOVE W10-QUANT    TO W10-REORD
022260         GO TO BC55.
022270       MOVE STK-QUANT      TO W10-QUANT.
022280       ACCEPT S26-QNT AT LINE LIN.
022290 BC80.
      *
      *    ****    C A L C U L A T E   E X T E N D E D   V A L U E
      *
022300       MOVE W10-QUANT      TO SAL-QNT.
022310       COMPUTE W25-VALUE ROUNDED = (1.000 * W10-SELL * W10-QUANT).
022320       MOVE W25-VALUE      TO W100-EXT SAL-VAL.
022330	     DISPLAY W100-EXT AT LINE LIN COLUMN 70
		     WITH FOREGROUND-COLOR 11.
      *
      *    ****    L I N E   D I S C O U N T   ( PERCENTAGE )
      *
022340	     DISPLAY "Discount %" AT 2518
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
022350       MOVE ZERO           TO WS-DISC W90-CDISC.
022360     IF DEB-DISC > ZERO
022370         MOVE STK-LEDG     TO WS-DEPKEY
022380         PERFORM READ-DEPART THRU READ-DEPART-EXIT
022390         MOVE DPT-DSETT (DEB-DISC) 
022400                           TO WS-DISC.
022410 BC85.
022420       ACCEPT S26-DSC AT LINE 25.
	   IF USER-FUNC
	       EVALUATE KEY-CODE-1
		 WHEN ESC-KEY
022540		     DISPLAY CLEAR-L25
		     GO TO BC70
	       END-EVALUATE.
022430     IF WS-DISC = ZERO
022540	       DISPLAY CLEAR-L25
022440         GO TO BC90.
           IF DEB-DISC > ZERO
               IF WS-DISC > DPT-DSETT (DEB-DISC)
018260             DISPLAY "Enter password >      <" AT 2551
018270			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
018280             ACCEPT W40-CSHPASS AT 2567 
			  WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
			       AUTO NO-ECHO
018300             MOVE 6 TO WS-LENGTH
		   CALL "CBL_TOUPPER" USING W40-CSHPASS
				      BY VALUE WS-LENGTH
				      RETURNING WS-STATUS
018360             MOVE 1 TO WS-LENGTH
                   IF W40-CSHPASS NOT = W85-SUPER (1)
                       MOVE DPT-DSETT (DEB-DISC)
                                 TO WS-DISC.
022450       COMPUTE W10-SPAPRC ROUNDED = W10-SELL -
022460              (W10-SELL * WS-DISC / 100.00000).
      *
      *    ****    C H E C K   M A R G I N
      *
022470     IF DEB-DISC > ZERO
022480         IF DPT-DSETT (DEB-DISC) = WS-DISC
022490             GO TO BC86.
022500       PERFORM BB100.
022510     IF WS-ERROR = 1
022520         GO TO BC85.
022530 BC86.
022540	     DISPLAY CLEAR-L25.
022550       MOVE WS-DISC        TO SAL-DSC.
022560	     COMPUTE W90-CDISC =
                   ((W25-VALUE * WS-DISC) / 100.00000).
022570	     MOVE W90-CDISC	 TO W100-PRC SAL-DISC.
022580	     DISPLAY W100-PRC AT 2540
		     WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15.
022590 BC90.
      *
      *    ****    A R E   T H E   D E T A I L S   C O R R E C T
      *
022700 BC95.
      *
      *    ****    A C C U M U L A T E   TOTAL COST / TOTAL DISCOUNT /
      *            VALUES SUBJECT TO TAX AND NON-TAX.
      *
019830     IF (STK-IND < 2) OR (STK-IND = 3)
022720         COMPUTE W90-BAL = W90-BAL + (W10-QUANT * W10-COST).
022730       ADD W90-CDISC       TO W90-LDISC.
022740       SUBTRACT W90-CDISC FROM W25-VALUE 
022750                           GIVING W90-VALUE.
022840     IF (STK-TAX = 0) OR (W90-EXP = "X")
022850         ADD W90-VALUE     TO W90-NONTAX
022860         MOVE W90-VALUE    TO SAL-NONTAX
022870     ELSE
022880         MOVE W90-VALUE    TO SAL-TAXVAL
022890         ADD W90-VALUE     TO W90-TAXVAL.
022910     IF W90-TAX NOT = SPACES
022920         COMPUTE SAL-VAT ROUNDED = SAL-TAXVAL * W05-RTE.
      *
      *    ****    A C C U M U L A T E   T O T A L   S A L E
      *
022940       ADD W25-VALUE       TO W90-DB.
022950 BC100.
022960       MOVE W90-DB         TO W90-TOTAL.
022970       SUBTRACT W90-LDISC  FROM W90-TOTAL.
022980	     MOVE W90-DB	 TO W100-VAL.
022990	     DISPLAY W100-VAL AT 2015 WITH FOREGROUND-COLOR 11.
023000       MULTIPLY -1 BY W90-LDISC 
023010				 GIVING W100-VAL.
023020	     DISPLAY W100-VAL AT 2115 WITH FOREGROUND-COLOR 11.
      *
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *
           IF W90-ADV = "Y"
               COMPUTE W90-ADVTAX ROUNDED = 
                      (W90-TOTAL * WS-ADV-RATE / 100.00000).
023040     IF W90-TAX = SPACES
023050         GO TO BC102.
023060	     MOVE W05-VAT (WS-VAT-SUB)
				 TO W05-RATE.
023070     IF W90-TAXVAL NOT = ZERO
023080         COMPUTE W25-TOTAL ROUNDED = 
                     ((W90-TAXVAL + W90-ADVTAX) * W05-RTE)
023100	       MOVE W25-TOTAL	 TO W100-VAL
023120	       ADD W25-TOTAL	 TO W90-TOTAL
	   ELSE
	       MOVE ZERO	 TO W100-VAL.
023110	     DISPLAY W100-VAL AT 2215 WITH FOREGROUND-COLOR 11.
023130 BC102.
           IF W90-ADV = "Y"
               MOVE W90-ADVTAX   TO W100-BAL
               DISPLAY W100-BAL AT 2241 WITH FOREGROUND-COLOR 11
               ADD W90-ADVTAX    TO W90-TOTAL. 
023140	     MOVE W90-TOTAL	 TO W100-VAL.
023150	     DISPLAY W100-VAL AT 2315 WITH FOREGROUND-COLOR 11.
023160 BC105.
023170	     DISPLAY "A" AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
023180		     "ccept, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
023190		     "C" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
023200		     "ancel > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
023210       MOVE "A"            TO WS-OPTION.
023220 BC110.
      *
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E  
      *            A C C E P T E D.
      *
023230	     ACCEPT WS-OPTION AT 2528
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
023240			 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
023300	   IF NOT (WS-OPTION = "A" OR "C")
023310         GO TO BC110.
023320	     DISPLAY CLEAR-L25.
023330     IF WS-OPTION = "C"
023340	       DISPLAY WS-MIV78 AT LINE LIN COLUMN 2
		       WITH FOREGROUND-COLOR 3
023350         PERFORM BC114
023360         PERFORM BC100 THRU BC102
023370         GO TO BC55.               
023380 BC111.
             MOVE WS-SUPER       TO SAL-SUPER.
023390       PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
023400       ADD 1               TO WS-LAST.
           IF W43-SERNO (1) = SPACES
               GO TO BC111E.
             MOVE W10-QUANT      TO WS-S2.
             MOVE 1              TO WS-S1.
             MOVE SPACES         TO SAL-ITEM.
       BC111A.
             MOVE 1              TO WS-S3.
             MOVE SPACES         TO SAL-MSER.
       BC111B.
             MOVE W43-SERNO (WS-S1)
                                 TO SAL-SNO (WS-S3).
             ADD 1               TO WS-S1.
           IF WS-S1 > WS-S2
               PERFORM BC111C
               GO TO BC111E.
           IF WS-S3 < 3
               ADD 1             TO WS-S3
               GO TO BC111B.
       BC111C.
             ADD 1               TO SAL-SUB.         
021110       MOVE WS-LAST        TO WS-SALKEY.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
023400       ADD 1               TO WS-LAST.
       BC111D.
             ADD 1               TO WS-S1.
           IF WS-S1 NOT > WS-S2
               GO TO BC111A.
       BC111E.
      *
      *    ****    T E S T   I F   S C R E E N   F U L L
      *
023410     IF LIN < 18
023420         ADD 1             TO LIN
023430         GO TO BC112.
023440       MOVE 8              TO LIN.
023450       PERFORM BC115 UNTIL LIN > 18.
023460       GO TO BC50.
023470 BC112.
023480	     DISPLAY "R" AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
023490		     "emarks, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
023500		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
023510		     "ext or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
023520		     "E" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
023530		     "nd > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
023540       MOVE WS-INPUT       TO WS-OPTION.
023550	     ACCEPT WS-OPTION AT 2534
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
023560			 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
023620	   IF NOT (WS-OPTION = "R" OR "N" OR "E")
023630         GO TO BC112.
023640	     DISPLAY CLEAR-L25.
             MOVE WS-OPTION      TO WS-INPUT.
023650     IF WS-OPTION = "N"
023660         GO TO BC55.
023670     IF WS-OPTION = "E"
023680         SUBTRACT 1        FROM WS-LAST
023690         GO TO BC120.
023700       MOVE SPACES         TO W10-REMKS W43-SERNO (1).
023710	     ACCEPT (LIN, 21) W10-REMKS WITH FOREGROUND-COLOR 11
023720                                       UPDATE AUTO.
023730     IF W10-REMKS = SPACES
023740         GO TO BC112.
023750	     DISPLAY WS-LAST AT LINE LIN COLUMN 2
		     WITH FOREGROUND-COLOR 11.
023760       MOVE WS-LAST        TO WS-SALKEY.
023770       INITIALIZE SAL-RECORD1.
023780       MOVE WS-LAST        TO SAL-CODE.
023790       MOVE W10-REMKS      TO SAL-MSER.
023800       GO TO BC111.
023810 BC114.
023820       SUBTRACT W25-VALUE  FROM W90-DB.
023830       SUBTRACT W90-CDISC  FROM W90-LDISC.
023840       SUBTRACT SAL-NONTAX FROM W90-NONTAX.
023850       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL.
019830     IF (STK-IND < 2) OR (STK-IND = 3)
023870         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
023880 BC115.
023890	     DISPLAY WS-MIV1 AT LINE LIN COLUMN 1
		     WITH BACKGROUND-COLOR 0 FOREGROUND-COLOR 3
		     WS-MIV78 WITH FOREGROUND-COLOR 3
		     WS-MIV80 WITH BACKGROUND-COLOR 0
				   FOREGROUND-COLOR 3.
023900       ADD 1               TO LIN.
023910 BC120.
      *
      *    ****    W A S   T H E R E   A N   E N T R Y
      *
023920     IF WS-LAST = ZERO
023930         GO TO BC05.
      *
      *    ****    P R O C E S S   T H E   S A L E
      *
023940 BC125.
023950	     DISPLAY CLEAR-L25.
023960	     DISPLAY "P" AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
023970		     "rocess Invoice, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
023980		     "A" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
023990		     "mend sales, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
024000		     "C" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
024010		     "ancel or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
024020		     "M" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
024030		     "ore items"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
024040 BC130.
024050	     ACCEPT WS-OPTION AT 2563
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
024110	   IF NOT (WS-OPTION = "P" OR "A" OR "C" OR "M")
024120         GO TO BC130.
024130	     DISPLAY CLEAR-L25.
024140     IF WS-OPTION = "M"
024150         ADD 1             TO WS-LAST
024160         GO TO BC112.
024170     IF WS-OPTION = "A"
024180         PERFORM BC270 THRU BC335
024190         GO TO BC125.
024200     IF WS-OPTION = "C"
024210         GO TO BC599.
024220       MOVE ZERO           TO W90-STOTAL WS-TDISC W90-TDISC.
024230     IF W90-LDISC NOT = ZERO
024240         MOVE W90-LDISC    TO W90-TDISC
024250         GO TO BC140.
024260       MOVE W90-DB         TO W90-DVAL.
024270       GO TO BC140.
      *
      *     VERSION 6.0  - The following paragraph has been bypassed as
      *                    discount is only allowed per item at this 
      *                    stage.
      *
024280 BC135.
      *
      *    ****    I F   N O   L I N E   D I S C O U N T   -   T H E N
      *            A L L O W   F O R   D I S C O U N T   O N   T H E
      *            I N V O I C E   T O T A L   
      *
      *      ACCEPT S26-TDSC AT LINE 21.
      *      DISPLAY WS-CHK AT 2518 WITH FOREGROUND-COLOR 14.
      *      ACCEPT WS-OPTION AT 2552 WITH FOREGROUND-COLOR 15 AUTO.
      *      CALL "_MSUPCASE" USING WS-OPTION
      *                       LENGTH WS-OPTION
      *                              WS-LENGTH
      *                       LENGTH WS-LENGTH
      *                              WS-STATUS.
      *      DISPLAY CLEAR-L25.
      *    IF WS-OPTION = "N"
      *        GO TO BC135.
      *    IF WS-TDISC = ZERO
      *        GO TO BC140.
      *      COMPUTE W90-TDISC = W90-DVAL * WS-TDISC / 100.
      *      ADD W90-NONTAX W90-TAXVAL GIVING W90-TOTAL.
      *      COMPUTE W90-VALUE = (W90-TDISC / W90-TOTAL) * W90-TAXVAL.
      *      SUBTRACT W90-VALUE  FROM W90-TAXVAL.
      *      SUBTRACT W90-VALUE  FROM W90-TDISC GIVING W90-TOTAL.
      *      SUBTRACT W90-TOTAL  FROM W90-NONTAX.
024290 BC140.
024300       SUBTRACT W90-TDISC FROM W90-DB 
024310                           GIVING W90-TSELL W90-TOTAL.
024320       MULTIPLY -1 BY W90-TDISC 
024330				 GIVING W100-VAL.
024340	     DISPLAY W100-VAL AT 2115 WITH FOREGROUND-COLOR 11.
           IF W90-ADV = "Y"
               COMPUTE W90-ADVTAX ROUNDED = 
                      (W90-TOTAL * WS-ADV-RATE / 100.000000).
024350       MOVE ZERO           TO W90-TTAX.
024400     IF (W90-TAX = SPACES) OR (W90-TAXVAL = ZERO)
024430         GO TO BC142.
024440	     MOVE W05-VAT (WS-VAT-SUB)
				 TO W05-RATE.
021490       COMPUTE W90-TTAX ROUNDED = ((W90-TAXVAL + W90-ADVTAX)
                                          * W05-RTE).
024470	     MOVE W90-TTAX	 TO W100-VAL.
024480	     DISPLAY W100-VAL AT 2215 WITH FOREGROUND-COLOR 11.
024490       ADD W90-TTAX        TO W90-TOTAL.
024500 BC142.
           IF W90-ADV = "Y"
               MOVE W90-ADVTAX   TO W100-BAL
               DISPLAY W100-BAL AT 2241 WITH FOREGROUND-COLOR 11
               ADD W90-ADVTAX    TO W90-TOTAL. 
025130 BC185.
      *
      *    ****    M U S T   T H E   V A L U E   B E   A G E D  
      *
025140	     DISPLAY CLEAR-L25.
025150       MOVE "M" TO WS-AGE.
	     DISPLAY "Print Invoice " AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "Y" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "/" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "N" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     " > <" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
             MOVE "Y"            TO WS-PRNINV.
025310 BC186.
025320       ACCEPT WS-PRNINV AT 2521
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
			 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-PRNINV
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-PRNINV = "Y" OR "N")
	       GO TO BC186.
	     MOVE WS-PRNINV	 TO W40-PRNINV.
	     MOVE WS-LAST	 TO W40-LAST.
025400	     DISPLAY CLEAR-L25.
      *
      *    ****    P R O C E S S   T H E   S A L E
      *
	   IF DEB-TERMS = "3"
	       PERFORM BB000.
025430	     DISPLAY "Processing Invoice" AT 2502 WITH
025440		      BACKGROUND-COLOR 3
		      FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
029610	     CLOSE SALE.
             OPEN I-O SALE.
	     MOVE W10-SMAN	 TO W40-SMAN.
	     MOVE WS-VAT-SUB	 TO W40-VAT-SUB.
	     MOVE WS-PRC	 TO W40-PRC.
	     MOVE "N"		 TO W40-REPRINT.
	     MOVE WS-CR 	 TO W40-CR.
	     CALL PRG-NAME USING WS-PARID
				 WS-USER-ID
				 WS0-PROGRAMS
				 W40-COMPANY
				 W90-TRAN
		  ON EXCEPTION
		      GO TO BC187
	     END-CALL.
	     CANCEL PRG-NAME.
	     GO TO BC188.
       BC187.
	     DISPLAY CLEAR-L25.
	     DISPLAY "Program : " AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		      PRG-NAME
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6
			   HIGHLIGHT
		     " not on disk, press "
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1
		     "ANY" WITH BACKGROUND-COLOR 3
				FOREGROUND-COLOR 6
				HIGHLIGHT
		     " key to continue"
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 1.
	     CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
025410 BC188.
029690	     DISPLAY CLEAR-L25.
029610       CLOSE SALE.
029600       OPEN OUTPUT SALE.
029610       CLOSE SALE.
             OPEN I-O SALE.
029700	     DISPLAY "ENTER" AT 2512
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
029710		     " to continue - "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
029720		     "E" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
029730		     " to exit"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
029740	     ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
029800	   IF WS-OPTION = "E"
029820         GO TO BC599.
029830       GO TO BC05.
       BC189.
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
025980       MOVE SPACES         TO W40-PC1 W40-PC2.
025990       MOVE DEB-ADDRESS    TO W60-ADDRESS.
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
026150       MOVE DEB-PADDR      TO W60-ADDRESS1.
026160       PERFORM CA185-ADDRESS.
026170     IF NOT (DEB-PPOST = ZERO OR 9999)
026180         MOVE DEB-PPOST    TO W40-PC2.
026190     IF W40-PADD4 = SPACES
026200         MOVE W40-PC2      TO W40-PADD4
026210         MOVE SPACES       TO W40-PC2.
026220     IF W40-PADD3 = SPACES
026230         MOVE W40-PADD4    TO W40-PADD3
026240         MOVE SPACES       TO W40-PADD4.
026250     IF W40-PADD2 = SPACES
026260         MOVE W40-PADD3    TO W40-PADD2
026270         MOVE SPACES       TO W40-PADD3.
026280     IF W40-PADD1 = SPACES
026290         MOVE W40-PADD2    TO W40-PADD1
026300         MOVE SPACES       TO W40-PADD2.
       BC215.
	       EXIT.
      *
      *    ****    A M E N D   S A L E S   D E T A I L S
      *
029980 BC270.
	     DISPLAY "Amend " AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "H" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "eader, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "S" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ales details or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "E" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "xit > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
             MOVE "S"            TO WS-OPTION.
       BC271.
             ACCEPT WS-OPTION AT 2539
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
			 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "E" OR "H" OR "S")
               GO TO BC271.
	     DISPLAY CLEAR-L25.
           IF WS-OPTION = "S"
               GO TO BC272
           ELSE
           IF WS-OPTION = "E"
               GO TO BC335.
       BC271A.
	     DISPLAY "Amend " AT 2502
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "O" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "rd No, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "R" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "ef No, "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "S" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "alesman or "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "E" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		     "xit > <"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
             MOVE SPACE          TO WS-OPTION.
       BC271B.
             ACCEPT WS-OPTION AT 2542
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
			 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	   IF NOT (WS-OPTION = "E" OR "O" OR "R" OR "S")
               GO TO BC271B.
	     DISPLAY CLEAR-L25.
           IF WS-OPTION = "E"
               GO TO BC335.
           IF WS-OPTION = "O"
017660         ACCEPT W90-ORD AT 0368 
                      WITH FOREGROUND-COLOR 11 UPDATE AUTO
               GO TO BC271A
           ELSE
           IF WS-OPTION = "R"
               ACCEPT W90-REG AT 0554 
                      WITH FOREGROUND-COLOR 11 UPDATE AUTO
               GO TO BC271A.
       BC271C.
017700       ACCEPT W10-SMAN AT 0411 WITH FOREGROUND-COLOR 11
017710                                    UPDATE AUTO.
017720     IF W10-SMAN > 50
017730         GO TO BC271C.
             MOVE SPACES         TO W40-ASSIST.
	     MOVE W10-SMAN	 TO W40-SMAN.
017740     IF W10-SMAN = ZERO
               DISPLAY "                              "  AT 0420
                       WITH FOREGROUND-COLOR 3
017750         GO TO BC271A.
017820	     DISPLAY CLEAR-L25.
017760       ADD 100 W10-SMAN    GIVING WS-PARKEY.
017770       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
017780     IF PAR-SALESMAN = SPACES
017790         DISPLAY "Invalid Salesman Number" AT 2518
017800			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
017810         GO TO BC271C.
017820	     DISPLAY CLEAR-L25.
017830       DISPLAY PAR-SALESMAN AT 0420 WITH FOREGROUND-COLOR 11.
             MOVE PAR-SALESMAN   TO W40-ASSIST.
             GO TO BC271A.
       BC272.
029990       MOVE 8              TO LIN.
030000       PERFORM BC115 UNTIL LIN > 18.
030010       MOVE 8              TO LIN.
030020       MOVE 1              TO WS-LINES WS-FIRST.
030030 BC275.
030040       MOVE WS-FIRST TO WS-SALKEY.
030050 BC280.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
030060     IF WS-SALKEY > WS-LAST
030070         GO TO BC285.
030080       PERFORM READ-SALE THRU READ-SALE-EXIT.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
030090	     DISPLAY SAL-CODE AT LINE LIN COLUMN 2
		     WITH FOREGROUND-COLOR 11.
030100     IF SAL-ITEM = SPACES
030110	       DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		       WITH FOREGROUND-COLOR 11
030120         GO TO BC282.
030130	     DISPLAY SAL-ITEM AT LINE LIN COLUMN 6
		     WITH FOREGROUND-COLOR 11.
030140	     DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		     WITH FOREGROUND-COLOR 11.
030150       MOVE SAL-QNT        TO W10-QUANT.
030160       DISPLAY S26-QNT AT LINE LIN.
030170       MOVE SAL-SELL       TO W10-SELL.
030180       DISPLAY S26-PRC AT LINE LIN.
030190       MOVE SAL-VAL        TO W100-EXT.
030200	     DISPLAY W100-EXT AT LINE LIN COLUMN 70
		     WITH FOREGROUND-COLOR 11.
      *
      *    ****    I S   T H E   S C R E E N   F U L L
      *
030210 BC282.
030220     IF LIN < 18
030230         ADD 1             TO LIN WS-SALKEY WS-LINES
030240         GO TO BC280.
030250 BC285.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
029400     IF WS-LINES < 11
029410         IF WS-FIRST = 1
029420		   DISPLAY "A" AT 2520
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
			   "mend, "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "D" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "elete an Item - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "E" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "xit > <"
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
029440         ELSE
029450		   DISPLAY "A" AT 2502
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
			   "mend, "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "D" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "elete an Item - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "P" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "revious screen - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "E" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "xit > <"
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
029480             MOVE 2        TO WS-LINES
029490     ELSE
029500     IF WS-SALKEY < WS-LAST
029510         IF WS-FIRST = 1
029520		   DISPLAY "A" AT 2506
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
			   "mend, "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "D" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "elete an Item - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "N" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "ext screen - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "E" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "xit > <"
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
029550             MOVE 1        TO WS-LINES
029560         ELSE
029570		   DISPLAY "A" AT 2504
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
			   "mend, "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "D" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "elete - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "N" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "ext, "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "P" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "revious screen - "
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
			   "E" WITH BACKGROUND-COLOR 3
				    FOREGROUND-COLOR 14
			   "xit > <"
			    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
029600             MOVE 3        TO WS-LINES
029610     ELSE
029620         MOVE 2            TO WS-LINES
029630	       DISPLAY "A" at 2502
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       "mend, "
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		       "D" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       "elete an Item - "
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		       "P" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		       "revious screen - "
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		       "E" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
		       "xit > <"
			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
030520 BC290.
030530	     ACCEPT WS-OPTION AT 2550
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
030590	   IF NOT (WS-OPTION = "A" OR "D" OR "E" OR "N" OR "P")
030600         GO TO BC290.
030610     IF WS-OPTION = "E"
030620         GO TO BC335.
030630     IF WS-OPTION = "A" OR "D"
030640         GO TO BC300.
      *
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *
030650     IF WS-LINES = 2 OR 3
030660         IF WS-OPTION = "P"
030670             PERFORM BC295
030680		   DISPLAY CLEAR-L25
030690             SUBTRACT 11   FROM WS-FIRST
030700             GO TO BC275.
      *
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *
030710     IF WS-LINES = 1 OR 3
030720         IF WS-OPTION = "N"
030730             PERFORM BC295
030740		   DISPLAY CLEAR-L25
030750             ADD 11        TO WS-FIRST
030760             GO TO BC275.
030770	     GO TO BC285.
030780 BC295.
      *
      *    ****    C L E A R   S C R E E N
      *
030790       MOVE 8              TO LIN.
030800       PERFORM BC115 UNTIL LIN > 18.
030810       MOVE 8              TO LIN.
030820       MOVE 1              TO WS-LINES.
030830 BC300.
      *
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *
030840     IF WS-OPTION = "A"
030850         GO TO BC310.
      *
      *    ****    M A K E   S U R E   T H A T   A N   I T E M 
      *            M U S T   B E   D E L E T E D
      *
030990	     DISPLAY CLEAR-L25.
030860       DISPLAY "Are you sure Y/N?" AT 2501
030870		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
030880       MOVE "N"            TO WS-OPTION.
030890 BC305.
030900	     ACCEPT WS-OPTION AT 2519
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
030910			 UPDATE AUTO.
	     CALL "CBL_TOUPPER" USING WS-OPTION
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
030970	   IF NOT (WS-OPTION = "N" OR "Y")
030980         GO TO BC305.              
030990	     DISPLAY CLEAR-L25.
031000     IF WS-OPTION = "N"
031010	       GO TO BC285.
031020       MOVE "D"            TO WS-OPTION.
031030 BC310.
031040	     DISPLAY CLEAR-L25.
031050       DISPLAY "Enter item No to Amend/Delete >  <" AT 2501
031060		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
031070       MOVE ZERO           TO WS-INO.
031080 BC315.
      *
      *    ****    G E T   T H E   N U M B E R   O F   T H E   I T E M
      *            T H A T   M U S T   B E   A M E N D E D   O R
      *            D E L E T E D
      *
031090	     ACCEPT WS-INO AT 2532
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15
031100                                  UPDATE AUTO.
031110     IF WS-INO = ZERO
031120	       DISPLAY CLEAR-L25
031130	       GO TO BC285.
031140     IF WS-INO > WS-SALKEY
031150         GO TO BC315.
031160     IF WS-INO < WS-FIRST
031170         GO TO BC315.
      *
      *    ****    C A L C U L A T E   T H E   L I N E   N U M B E R
      *            O N   W H I C H   T H E   I T E M   I S
      *            D I S P L A Y E D
      *
031180     IF WS-INO < 12
031190         ADD 7 WS-INO      GIVING LIN
031200     ELSE
031210         COMPUTE LIN = WS-INO - WS-FIRST + 8.
      *
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *
031220       MOVE WS-INO         TO WS-SALKEY.
031230       PERFORM READ-SALE THRU READ-SALE-EXIT.
031240     IF SAL-ITEM = SPACES
031250         GO TO BC316.
031260       MOVE SAL-ITEM       TO W10-ITEM STK-CODE.
031270       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
031280       MOVE SAL-QNT        TO W10-QUANT.
031290       MOVE SAL-COST       TO W10-COST.
031300       MOVE SAL-SELL       TO W10-SELL.
031310       MOVE SAL-DISC       TO W90-CDISC.
031320       MOVE SAL-DSC        TO WS-DISC.
031330       MOVE SAL-VAL        TO W25-VALUE.
      *
      *    ****    R E D U C E   T O T A L S 
      *
031340       SUBTRACT W25-VALUE  FROM W90-DB.
031350       SUBTRACT W90-CDISC  FROM W90-LDISC.
031360       SUBTRACT SAL-NONTAX FROM W90-NONTAX.
031370       SUBTRACT SAL-TAXVAL FROM W90-TAXVAL.
019830     IF (STK-IND < 2) OR (STK-IND = 3)
031390         COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *
      *    ****    I S   I T   A N   A M E N D M E N T
      *
031400 BC316.
031410     IF WS-OPTION = "A"
031420         GO TO BC325.
      *
      *    ****    R E - D I S P L A Y   T O T A L S
      *
031430     IF SAL-ITEM NOT = SPACES
031440         PERFORM BC100 THRU BC102.
031450 BC320.
      *
      *    ****    D E L E T E   I T E M   F R O M   S A L E   F I L E
      *
031460       MOVE WS-SALKEY      TO WS-SALSTR.
031470       ADD 1               TO WS-SALKEY.
031480     IF WS-SALKEY > WS-LAST
031490         SUBTRACT 1        FROM WS-LAST
031500         GO TO BC272.
031510       PERFORM READ-SALE THRU READ-SALE-EXIT.
031520       SUBTRACT 1          FROM SAL-CODE.
031530       MOVE WS-SALSTR      TO WS-SALKEY.
031540       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
031550       ADD 1               TO WS-SALKEY.
031560       GO TO BC320.
031570 BC325.
031580     IF SAL-ITEM = SPACES
031590	       ACCEPT (LIN, 21) SAL-SDSC WITH FOREGROUND-COLOR 11
031600                                        UPDATE AUTO
031610         GO TO BC332.
      *
      *    ****    A M E N D   Q U A N T I T Y
      *
031620       ACCEPT S26-QNT AT LINE LIN.
031630	     DISPLAY CLEAR-L25.
031640     IF W10-QUANT = ZERO
031650         DISPLAY "Quantity MAY NOT be zero" AT 2501
031660			WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
031670         MOVE SAL-QNT      TO W10-QUANT
031680         GO TO BC325.
      *
      *    ****    R E C A L C U L A T E   V A L U E S
      *
031690       MULTIPLY W10-SELL BY W10-QUANT 
031700                           GIVING W25-VALUE.
031710       MOVE W25-VALUE      TO W100-EXT.
031720	     DISPLAY W100-EXT AT LINE LIN COLUMN 70
		     WITH FOREGROUND-COLOR 11.
      *
      *    ****    U P D A T E   S A L E   F I L E   R E C O R D
      *
031830       MOVE W10-QUANT      TO SAL-QNT.
031840       MOVE W25-VALUE      TO SAL-VAL.
031850     IF WS-DISC = ZERO
031860         GO TO BC330.
031870       COMPUTE W90-CDISC = (W25-VALUE * WS-DISC) / 100.
031880       MOVE W90-CDISC      TO SAL-DISC.
031890 BC330.
      *
      *    ****    R E - C A L C U L A T E   &   D I S P L A Y
      *            T O T A L S
      *
031900       PERFORM BC95 THRU BC102.
031910 BC332.
031920       PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
031930       GO TO BC270.
031940 BC335.
031950       EXIT.
031960 BC599.
031970       EXIT.
      /
052700 ZA000-INIT	       SECTION 9.
052710 ZA000-OPEN.
055960	     MOVE WS-COMPANY	 TO WS-TOP-COMP.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.

	     MOVE WS-SYS-ID	 TO W02-SYSID.

	   IF LS-USER = WS-SYS-ID
	       MOVE "SupervisorÄ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
052730	     DISPLAY CLR-SCREEN.
      *
      *    ****    D O	 N O T	 D I S P L A Y	 E R R O R
      * 	   M E S S A G E S
      *
	     MOVE 44		 TO USER-NUMBER.
	     MOVE 1		 TO USER-SETTING.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
      *    PRE-DISPLAY ACCEPT FIELDS BEFORE DATA-ENTRY
      *
      *      MOVE 76		 TO USER-NUMBER.
      *      MOVE 3		 TO USER-SETTING.
      *      MOVE 1		 TO USER-KEYS.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
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
      *      CALL CBL_INIT_MOUSE USING MOUSE-HANDLE
      *				       MOUSE-BUTTONS
      *				 RETURNING MOUSE-STATUS.
      *    IF MOUSE-STATUS = ZERO
      *        MOVE "Y"		 TO MOUSE-ATTACHED.
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
	     MOVE "DEBMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBMEM.
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
	     MOVE "DEPART"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEPART.
	     MOVE "GARTEE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GARTEE.
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
	     MOVE "SALE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-SALE.
	     MOVE "STKMEM"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STKMEM.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
052980	     MOVE 3		 TO WS-PARKEY.
052990       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	     MOVE PAR-INV	 TO PRG-NUMBER.
053550	     MOVE PAR-VAT-REG	 TO W05-VATNO.
           IF PAR-POS = "8"
               MOVE "N"          TO WS-BYPASS.
054270       OPEN OUTPUT SALE.
054280       CLOSE SALE.
054290       OPEN I-O SALE.
054760       MOVE 1              TO WS-PARKEY.
054770       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
054780       MOVE PAR-DMY        TO W12-TODAY.
054790       MOVE PAR-YMD        TO W12-T-YMD.
054710       MOVE 5              TO WS-PARKEY.
054720       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF PAR-ADVALOREM NOT = "Y"
	       MOVE "N" 	 TO WS-ADVALOREM
	   ELSE
	       MOVE "Y" 	 TO WS-ADVALOREM.
054730       MOVE PAR-ADV-RATE   TO WS-ADV-RATE.
054740       MOVE PAR-VAT-RATES  TO W05-VAT-RATES.
	     MOVE PAR-RATE-DATE	 TO WS-VAT-DATE.
054750	     MOVE PAR-DISCOUNT	 TO W15-DISCOUNT.
055260       GO TO ZA999.
055270 ZA49.
055280       DISPLAY "Too many files OPEN" AT 0812
055290                WITH FOREGROUND-COLOR 14.
055300       DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
055310       DISPLAY "Press any key to continue" AT 1212.
055320       ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
055330       GO TO ZA205.
055970 ZA200.
055980       DISPLAY "Files locked - Try later" AT 2312
055990                WITH FOREGROUND-COLOR 14
056000               " " WS-STATUS WITH FOREGROUND-COLOR 15.
056010       DISPLAY "Press any key" AT 2512 
056020		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
056030	     ACCEPT WS-OPTION AT 2526
		    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
056040 ZA205.
056050       EXIT PROGRAM.
056060 ZA999.
056070       EXIT.

056090 I-O-ERRORS      SECTION	9.
056100 OPEN-ERROR.
056110       DISPLAY CLR-SCREEN.
056120       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
056130       PERFORM DISPLAY-FILE-NAME.
056140
056150 READ-ERROR.
056160       DISPLAY CLR-SCREEN.
056170       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
056180       PERFORM DISPLAY-FILE-NAME.
056190
056200 WRITE-ERROR.
056210       DISPLAY CLR-SCREEN.
056220       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
056230       PERFORM DISPLAY-FILE-NAME.
056240

056260 DISPLAY-FILE-NAME.
056310	   IF WS-F-ERROR = 2
056320         MOVE W02-NETWORK TO WS-FILE
056330         MOVE WS-NETKEY TO WS-KEY
056380     ELSE
056390     IF WS-F-ERROR = 6
056400         MOVE W02-DEBTOR TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
056420     ELSE
056430     IF WS-F-ERROR = 7
056440         MOVE W02-DEPART TO WS-FILE
056450         MOVE WS-DEPKEY TO WS-KEY
           ELSE
           IF WS-F-ERROR = 9
               MOVE W02-GARTEE   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE GUA-KEY      TO WS-KEYX
056500	   ELSE
056510     IF WS-F-ERROR = 15
056520         MOVE WS-PARID     TO WS-FILE
056530         MOVE WS-PARKEY    TO WS-KEY
056580	   ELSE
056590     IF WS-F-ERROR = 19
056600         MOVE W02-SALE TO WS-FILE
056610         MOVE WS-SALKEY TO WS-KEY
056620     ELSE
056630     IF WS-F-ERROR = 22
056640         MOVE W02-STOCKF   TO WS-FILE
056650         MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 25
041860         MOVE W02-STKMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
               MOVE SME-KEY      TO WS-KEYX
041840     ELSE
041850     IF WS-F-ERROR = 27
041860         MOVE W02-DEBMEM   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
	       MOVE DME-KEY	 TO WS-KEYX.
056740     IF WS-STATUS = "10"
056750         MOVE "End of FILE" TO WS-STAT-MESSAGE
056760     ELSE
056770     IF WS-STATUS = "22"
056780         MOVE "Duplicate record number" TO WS-STAT-MESSAGE
056790     ELSE
056800     IF WS-STATUS = "23"
056810         MOVE "Invalid record number" TO WS-STAT-MESSAGE
056820     ELSE
056830     IF WS-STATUS = "24"
056840         MOVE "DISK full" TO WS-STAT-MESSAGE
056850     ELSE
056860     IF WS-STATUS = "30"
056870         MOVE "DISK error" TO WS-STAT-MESSAGE
056880     ELSE
056890     IF WS-STATUS = "94"
056900         MOVE "FILE locked" TO WS-STAT-MESSAGE.
056910       DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
056920       DISPLAY "Status " AT 1212 
056930                WS-STATUS WITH FOREGROUND-COLOR 11
056940               ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
056950     IF WS-STATUS NOT = "94" 
056960         DISPLAY "Key    " AT 1412 
056970                  WS-KEY WITH FOREGROUND-COLOR 11
056980         DISPLAY "Reverse backup or contact program Support"
056990                  AT 1612.
057000         DISPLAY "Please make a note of these details" AT 1812.
057010       STOP RUN.
