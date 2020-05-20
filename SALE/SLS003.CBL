      $set LINKCOUNT"712"
      ******************************************************************
      *                                                                *
      *    ******   **         ******      ****      ****     ******   *
      *   **    **  **        **    **    **  **    **  **   **    **  *
      *   **        **        **         **    **  **    **        **  *
      *    ******   **         ******    **    **  **    **     ****   *
      *         **  **              **   **    **  **    **        **  *
      *   **    **  **        **    **    **  **    **  **   **    **  *
      *    ******   ********   ******      ****      ****     ******   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *      P R O C E S S   S A L E S   O R D E R   P R O G R A M     *
      *                                                                *
      *     VERSION 8.15.04 - January 2012			       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     SLS003.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 1991.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2012
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
000090                 CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
000100 INPUT-OUTPUT SECTION.
000110 FILE-CONTROL.

       COPY APACFIDS.SL.

000150 COPY CONTROL.SL.

000190 COPY DEBTOR.ISL.

000190 COPY DEPART.SL.

000250 COPY GARTEE.SL.

000270 COPY PARAM.SL.

000310 COPY SALE.SL.

       COPY SHARED.SL.

000250 COPY SORDER.SL.

000200 COPY SPARTS.SL.

000330 COPY STOCK.SL.

      /
000390 DATA DIVISION.
000400 FILE SECTION.

       COPY APACFIDS.FDE.

000440 COPY CONTROL.FDE.

000480 COPY DEBTOR.FDE.

000420 COPY DEPART.FDE.

000250 COPY GARTEE.FDE.

000560 COPY PARAM.FDE.

000600 COPY SALE.IFD.

       COPY SHARED.FDE.

000540 COPY SORDER.FDE.

000360 COPY SPARTS.FDE.

000620 COPY STOCK.FDE.

      /
002090 WORKING-STORAGE SECTION.
       77  WS-CHECK	   PIC	X(18)	 VALUE
			   "aeWlimemnomLalismJ".
002100 77  WS-DISC	   PIC	99V99	 COMP-3.
002110 77  WS-TDISC        PIC  99V99    COMP-3.
002120 77  WS-CASH-AC      PIC  X(06).
002130 77  WS-SUB          PIC  9(04)    COMP-5.
002140 77  WS-P1	   PIC	9(04)	 COMP-5.
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
002330 77  WS-SALSTR       PIC  9(04)    COMP-5.
002340 77  WS-SALKEY       PIC  9(04)    COMP-5.
002350 77  WS-NETKEY       PIC  9(04)    COMP-5.
002360 77  WS-AUDKEY       PIC  9(06)    COMP-5.
002370 77  WS-RECKEY       PIC  9(06)    COMP-5.
002380 77  WS-RECOVER      PIC  9(06)    COMP-5.
000820 77  WS-SHARED	   PIC	9(04)	 COMP-5 VALUE 1.
002440 77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
002400 77  WS-PRN1	   PIC	9(01) VALUE 0.
002410 77  WS-PRN2         PIC  9(01) VALUE 0.  
002190 77  WS-USE-PACKS    PIC	X(01).
       77  WS-USE-ITM	   PIC	X(01).
       77  WS-EXT-STK	   PIC	X(01).
002430 77  WS-START	   PIC	9(08).
002440 77  WS-END          PIC  9(08).
002460 77  WS-TME2         PIC  9(08).
002470 77  WS-PORT         PIC  9(05) VALUE 1004.
002480 77  WS-OPEN         PIC  X(01) VALUE X"01".
002490 77  WS-CLOSE        PIC  X(01) VALUE X"00".
002500 77  WS-PAR          PIC  9(01) VALUE 0.
002510 77  WS-INO	   PIC	9(03) VALUE 0.
002520 77  WS-LINES        PIC  9(02) VALUE 0.
002530 77  WS-FIRST	   PIC	9(03) VALUE 0.
002540 77  WS-LAST	   PIC	9(03) VALUE 0.
002550 77  WS-LASTINV      PIC  9(04)    COMP-5.
002560 77  WS-LINE         PIC  9(04)    COMP-5.
002570 77  WS-PAGE         PIC  9(02)    COMP-5.
002580 77  WS-L1           PIC  9(04)    COMP-5.
002590 77  WS-OPTION       PIC  X(01).
002610 77  WS-SKIP	   PIC	X(01) VALUE " ".
002610 77  WS-LOOK         PIC  X(01) VALUE "S".
002620 77  WS-MULTI        PIC  X(01) VALUE "N".
002620 77  WS-ETYPE        PIC  X(01) VALUE "I".
002650 77  WS-ERROR        PIC  9(01).
002690 77  WS-COMP         PIC  9(01).
002710 77  WS-UPDATE       PIC  9(01) VALUE ZERO.
002720 77  WS-CONREC       PIC  9(01) VALUE ZERO.
       77  WS-NOSELL	   PIC	X(01) VALUE "N".
002730 77  WS-HEAD         PIC  X(01).
002740 77  WS-TYPE         PIC  X(01).
002760 77  WS-PRC          PIC  X(01).
002280 77  WS-PASS	   PIC	X(08).
002860 77  WS-A            PIC  X(19) VALUE "Any key to continue".
002870 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
002010 77  WS-ERR1         PIC  X(22) VALUE "Invalid Account Number".
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
002880 77  WS-PER          PIC  Z(02)9.99.
002890 77  WS-CR           PIC  X(01).
002900 77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY	   PIC	9(08) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

       01  PRG-INVOICE.
	   03  FILLER	   PIC	X(08) VALUE "SLP\OINV".
	   03  PRG-NUMBER  PIC	9(02) VALUE ZERO.
      /
002910 01  WS-DB-LINE.
003060	   03  WS-TOP-LINE.
003070	       05  WS-TCHAR PIC  X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÑÍÍÍÍÍÍ
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
001450	       05  WS-MCHAR PIC  X(80) VALUE "³              ³   ³
      -        "                       ³        ³         ³          ³".
	   03  WS-MINV REDEFINES WS-MID-LINE.
	       05  WS-INV1     PIC  X(01).
	       05  WS-INV78    PIC  X(78).
	       05  WS-INV80    PIC  X(01).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-BLNK78   PIC  X(78) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "³".
003180	   03  WS-BOT-LINE.
003190	       05  WS-BCHAR PIC  X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÏÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍµ".
003200	   03  WS-BOT-LNE2.
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
	   03  WS-MM-LINE.
	       05  FILLER      PIC  X(01) VALUE "³".
	       05  WS-SPACE    PIC  X(60) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "³".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "Í".
003220	   03  WS-TOP-LNE.
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
001690	   03  WS-TP-LINE.
001700         05  WS-TCA  PIC X(01) OCCURS 80.
001710     03  WS-MD-LINE.
001720         05  WS-MCA  PIC X(01) OCCURS 80.
	   03  WS-SO78-L REDEFINES WS-MD-LINE.
	       05  WS-MSO1     PIC  X(01).
	       05  WS-MSO78    PIC  X(78).
	       05  WS-MSO80    PIC  X(01).
001730	   03  WS-BT-LINE.
001740         05  WS-BCA  PIC X(01) OCCURS 80.

003340 COPY WS.WS.

       01  WS-HELP.
	   03  WS-MODULE       PIC  X(03) VALUE "SLS".
	   03  WS-PROG	       PIC  X(03) VALUE "003".

000290 01  WS-PARID.
000020	   03  WS-SYS-ID       PIC  X(03).

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

003530 01  W02-FID.

       COPY APACFIDS.ID.

003570 COPY CONTROL.ID.

003610 COPY DEBTOR.ID.

002120 COPY DEPART.ID.

000250 COPY GARTEE.ID.

       COPY PARAM.ID.

003710 COPY SALE.ID.

       COPY SHARED.ID.

003670 COPY SORDER.ID.

000360 COPY SPARTS.ID.

003730 COPY STOCK.ID.

       COPY W05.VAT.

       COPY W10.SAL.

003120 01  W11-PARTS.
003130     03  W11-PART-DETAILS OCCURS 30.
003140         05  W11-CODE   PIC X(14).
003150         05  W11-DESC   PIC X(30).
003160         05  W11-QUANT  PIC S9(05)V9999 COMP-3.

       COPY W12.WS.

       01  W14-ENTRIES.
           03  W14-SUB         PIC 9(03)     COMP-3.
           03  W14-SQN         PIC 9(05)     COMP-3 OCCURS 200.

       01  W14-ORDER.
           03  W14-REF         PIC Z(08).
           03  W14-AC          PIC X(06).
           03  W14-NAME.
	       05  W14-NME     PIC X(27).
	       05  FILLER      PIC X(13).
	   03  W14-DATE        PIC Z9/99/9999.
           03  W14-PS          PIC 9(02).
           03  W14-DOC         PIC X(01).
           03  W14-DDOC.
               05  W14-INV     PIC Z(08).
           03  W14-VALUE       PIC Z(07)9.99.

004410 01  W15-DISCOUNT.
004420     03  W15-DISC        PIC S9(03)V99 COMP-3 OCCURS 10.

       COPY W20.WS.

004520 01  W25-CALCS.
004530     03  W25-RESULT      PIC 9(02)V99.
004540     03  W25-RESULT1 REDEFINES W25-RESULT.
004550         05  W25-WHOLE   PIC 9(02).
004560         05  W25-DECIMAL PIC 9(02).
004570     03  W25-TOTAL       PIC 9(07)V99  COMP-3.
004580     03  W25-VALUE       PIC S9(07)V99 COMP-3.
004590     03  W25-CREDIT      PIC S9(07)V99 COMP-3.

       01  W32-MESSAGE.
	   03  W32-REMARKS OCCURS 10
			       PIC  X(63).

       01  W34-HEADER.
	   03  W34-FORMAT      PIC  X(01).
	   03  W34-MARGIN      PIC  9(02) COMP-5.
	   03  W34-SALUTATION  PIC  X(63).

       COPY W40.DBT.

       COPY W40.WS.

       COPY FUNCTION.WS.

005370 COPY W50.WS.

005390 01  W60-NME-ADD.
005400     03  W60-NAME.
005410         05  W60-NCHAR OCCURS 40 PIC X(01).
005420     03  W60-ADDRESS.
005430         05  W60-ACHAR OCCURS 60 PIC X(01).
005440     03  W60-ADDRESS1    PIC X(60).

005620 COPY W70DEBT.IWS.

005640 01  W75-KEYS.
005650     03  W75-S          PIC 9(02) COMP-5.
005660     03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
005670     03  W75-DNO        PIC X(06) OCCURS 18.
	   03  W75-SORD       PIC 9(08) OCCURS 18.

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

       COPY W90.SAL.

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
006730         05  W95-DTE     PIC Z9/99/99.

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

007420 01  S02.
007430	   03  LINE 2  COLUMN 27 FOREGROUND-COLOR 7 HIGHLIGHT
007440                           VALUE "SALES/ORDER PROGRAM CONTROL".

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

       COPY SORLUP.CRT.

009600 01  S26-QNT.
009610	   03	       COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009620                           PIC Z(04)9.99 USING W10-QUANT AUTO.

009630 01  S26-PRC.
009640	   03	       COLUMN 60 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009650                           PIC Z(05)9.99 USING W10-SELL AUTO.

009660 01  S26-DSC.
009670	   03	       COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009680                           PIC Z9.99 USING WS-DISC AUTO.

009690 01  S26-TDSC.
009700	   03	       COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 0
009710                           PIC Z9.99 USING WS-TDISC AUTO.

009720 01  S26-AVGE.
009730	   03	       COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
				 BACKGROUND-COLOR 5
009740				 PIC Z(04)9.999 USING STK-AVGE AUTO.

009290 01  S30.
009300	   03  LINE  2 COLUMN 35 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "SALES ORDER".
009310     03  LINE  3 COLUMN  2 VALUE "Ac No".
009320     03          COLUMN 15 VALUE "Name".
	   03	       COLUMN 20 BACKGROUND-COLOR 5
				 PIC  X(40) FROM WS-SPACE.
010070	   03	       COLUMN 61 VALUE "Ord No°".
	   03	       COLUMN 68 BACKGROUND-COLOR 5
				 PIC  X(12) FROM WS-SPACE.
010080	   03  LINE  4 COLUMN  2 VALUE "Assisted".
	   03	       COLUMN 11 BACKGROUND-COLOR 5
				 PIC  X(02) FROM WS-SPACE.
	   03	       COLUMN 61 VALUE "Tel.".
	   03	       COLUMN 66 BACKGROUND-COLOR 5
				 PIC  X(14) FROM WS-SPACE.
010090	   03  LINE  5 COLUMN  2 VALUE "Slip No".
	   03	       COLUMN 10 BACKGROUND-COLOR 0
				 PIC  X(08) FROM WS-SPACE.
	   03	       COLUMN 20 VALUE "Contact".
	   03	       COLUMN 29 BACKGROUND-COLOR 5
				 PIC  X(14) FROM WS-SPACE.
010100	   03	       COLUMN 65 VALUE "Date".
	   03	       COLUMN 70 BACKGROUND-COLOR 5
				 PIC  X(08) FROM WS-SPACE.
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
009470	   03  LINE 20 COLUMN  3 VALUE "Sub Total".
009480     03  LINE 21 COLUMN  3 VALUE "Discount".
009490     03  LINE 22 COLUMN  3 VALUE "T A X -VAT".
009500     03  LINE 23 COLUMN  3 VALUE "AMOUNT DUE".

       01  S30A.
	   03  FOREGROUND-COLOR 3 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC Z(08) USING W10-SORD AUTO.

009950 01  S36.
009960	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W80-ACNO AUTO.

       01  S40.
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

       01  S41.
	   03  LINE 11 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(40) USING W40-NM1 AUTO.
	   03  LINE 12 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(30) USING W40-AD1 AUTO.
	   03  LINE 13 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(30) USING W40-AD2 AUTO.
	   03  LINE 14 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(30) USING W40-AD3 AUTO.
	   03  LINE 15 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(30) USING W40-AD4 AUTO.
	   03  LINE 16 COLUMN 25 BACKGROUND-COLOR 2
				 FOREGROUND-COLOR 7 HIGHLIGHT
				 PIC X(04) USING W40-PC AUTO.

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
	   IF LS-DSKDRV = "L"
	       MOVE "Not available on Local drive"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
003240	   IF LS0-SALEV < 1
	       MOVE "Not Authorised"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
	       GO TO AA49.
010270	     PERFORM ZA000-INIT.
010430	     GO TO BA00.

       AA49.
	     EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
010500 AA50.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM SORDER-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
010920 AA99.
010930       EXIT.

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

010950 COPY LOCKED.RC2.

010970 COPY CLEAR.CRT.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
011010 AA400              SECTION.
011020 AA401.
011590	     DISPLAY "F1" AT 2502
		      WITH BACKGROUND-COLOR 3
			   FOREGROUND-COLOR 6 HIGHLIGHT "=Help,"
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F2" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
		     " Order no./ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011610		     "F3" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
011620		     " Account no./ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F4" WITH BACKGROUND-COLOR 3
			       FOREGROUND-COLOR 6 HIGHLIGHT
                     " From specified date Lookup" 
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
011110 AA405.
011120       EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
011130 AC000              SECTION.

011170 COPY CONTROL.RD.

011210 COPY DEBTOR.RD.

011730 COPY DEPART.RD.

000250 COPY GARTEE.RD.

011290 COPY PARAM.RD.

011310 COPY SALE.RD.

       COPY SHARED.RD.

011270 COPY SORDER.RD.

       COPY SPARTS.RD.

011330 COPY STOCK.RD.

      /
011370 AD000-WRITE             SECTION.

011410 COPY CONTROL.WR.

011410 COPY PARAM.WR.

011530 COPY SALE.WR.

011270 COPY SORDER.WR.

       COPY SORDER.LUP.

       COPY APAC.HLP.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
011670 AY000           SECTION.
012470 AY60.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
012720	     GO TO AY999.
012950 AY70.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 1.
      *
013070       COMMIT.
013080 AY999.
013090       EXIT.
      /
013100 AZ000-END               SECTION.
013110 AZ000-EOJ.
013120	     CLOSE SALE.
013250 AZ100.
013260       EXIT PROGRAM.

       COPY CA000.STK.

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

       COPY CA200.STK.

       COPY CA500.STK.

      *
      *    ****   G E T   S O R D E R   R E C O R D   U S I N G   SOR-KEY.
      *
      *           WS-ERROR:
      *
      *                    1 = NO RECORD
      *                    2 = NOT ORDER RECORD (QUOTE OR INVOICE)
      *                    3 = ORDER DETAILS INCOMPLETE
      *                    4 = INVALID ACCOUNT NUMBER
      *
013270 GET-SORDER	    SECTION 50.

013280 GET-SORD-ORDNO.
             INITIALIZE W14-ENTRIES.
	     MOVE 1		 TO W14-SUB WS-S7.
             MOVE SPACES         TO W40-NM1 W40-AD1 W40-AD2
                                    W40-AD3 W40-AD4 W40-PC.
013310       MOVE W10-SORD       TO SOR-REF.
013420       MOVE 100            TO SOR-SQN.
             MOVE ZERO           TO WS-ERROR.
	     PERFORM READ-SORDER THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               MOVE 1            TO WS-ERROR
               GO TO GET-SORDER-EXIT.
           IF SOR-DOC NOT = "O"
               MOVE 2            TO WS-ERROR
               GO TO GET-SORDER-EXIT.
             MOVE 1              TO WS-LAST WS-SALKEY.
           IF WS-MULTI NOT = "D"
               GO TO GET-SORD-TEST-TYPE.

       GET-SORD-NEXT.
	     PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               MOVE 3            TO WS-ERROR
               GO TO GET-SORDER-EXIT.
           IF WS-MULTI NOT = "D"
               GO TO GET-SORD-TEST-TYPE.
           IF SOR-TYPE = 9
               GO TO GET-SORDER-EXIT.
           IF SOR-TYPE = 3
               IF SOR-SUB > ZERO
                   PERFORM DELETE-SORDER-REC THRU WRITE-SORDER-EXIT.
             GO TO GET-SORD-NEXT.

       GET-SORD-TEST-TYPE.
      *                      H E A D E R   R E C O R D
           IF SOR-TYPE = 0
               MOVE SOR-AC       TO W80-ACNO DEB-ACNO
               PERFORM CA155-GET-DEBTOR
               IF WS-ERROR NOT = ZERO
                   MOVE 4        TO WS-ERROR
                   GO TO GET-SORDER-EXIT
               END-IF
      *    *************************************************************
      *    *		 C l i e n t   V A T   n u m b e r	       *
      *    *  N e e d	t o   p r i n t   o n	t h e	I n v o i c e  *
      *    *************************************************************
	       MOVE DEB-TAX	 TO W40-TAX
	       MOVE SOR-OREF	 TO W90-REG
               MOVE SOR-NAME     TO W40-NAME
               MOVE SOR-ORD      TO W90-ORD
               MOVE SOR-SMAN     TO W10-SMAN
               MOVE SOR-EXP      TO W90-EXP
               MOVE SOR-PRC      TO WS-PRC
	       MOVE SOR-CONTACT  TO W40-CONTACT
               MOVE SOR-TEL      TO W40-TEL
           ELSE
      *                      I T E M   D E T A I L S
           IF SOR-TYPE = 1
               PERFORM GET-CHECK-STOCK-FILE
               IF WS-F-ERROR = 0
                   INITIALIZE SAL-RECORD1
		   MOVE SOR-EXT-ITEM TO SAL-EXT-ITEM
                   MOVE SOR-DESC     TO SAL-MSER
                   MOVE SOR-QNT      TO SAL-QNT
      *		   MOVE SOR-QORD     TO SAL-ORD
                   MOVE SOR-COST     TO SAL-COST
                   MOVE SOR-SELL     TO SAL-SELL
                   MOVE SOR-VAT      TO SAL-VAT
015000		   MOVE SOR-VAT-SUB  TO SAL-VAT-SUB
                   MOVE SOR-VAL      TO SAL-VAL
                   MOVE SOR-INCTAX   TO SAL-INCTAX
                   MOVE SOR-NONTAX   TO SAL-NONTAX
                   MOVE SOR-TAXVAL   TO SAL-TAXVAL
                   MOVE SOR-DSC      TO SAL-DSC
                   MOVE SOR-DISC     TO SAL-DISC
		   MOVE SOR-PACK     TO SAL-PACK
		   MOVE SOR-SQN      TO SAL-SQN
		   IF SAL-VAT-SUB > ZERO
		       ADD SOR-TAXVAL
				 TO W90-TAXVAL(SAL-VAT-SUB)
		   END-IF
		   IF (PRG-NUMBER = 3) AND (STK-LDG = "98  ")
		       MOVE "Y"	 TO  W40-SPAR
		   END-IF
		   PERFORM GET-SORD-WRITE-SALE
               ELSE
                   PERFORM GET-CHANGE-TO-REMKS
           ELSE
      *                      P A R T S   D E T A I L S
           IF SOR-TYPE = 2
               INITIALIZE SAL-RECORD1
	       MOVE SOR-EXT-ITEM TO SAL-EXT-ITEM
               MOVE SOR-DESC     TO SAL-DESC
               MOVE SOR-QNT      TO SAL-QNT
               MOVE SOR-SERNO    TO SAL-SERNO
               MOVE SOR-SQN      TO SAL-SQN
               PERFORM GET-SORD-WRITE-SALE
           ELSE
      *                      R E M A R K S
           IF SOR-TYPE = 3
               IF SOR-SUB > ZERO
                   GO TO GET-SORD-NEXT
               ELSE
                   INITIALIZE SAL-RECORD1
                   MOVE SOR-DESC     TO SAL-MSER
                   MOVE SOR-SQN      TO SAL-SQN
                   PERFORM GET-SORD-WRITE-SALE
           ELSE
      *                      I N T E R N A L   N O T E S
           IF SOR-TYPE = 4
               GO TO GET-SORD-NEXT
015160     ELSE
      *                      A D D R E S S 
015170     IF SOR-TYPE = 5
               MOVE SOR-AD1      TO W40-AD1
               MOVE SOR-AD2      TO W40-AD2
               MOVE SOR-AD3      TO W40-AD3
               MOVE SOR-AD4      TO W40-AD4
               MOVE SOR-PC       TO W40-PC
	   ELSE
      * 		     S A L U T A T I O N
015170	   IF SOR-TYPE = 7
	       MOVE SOR-TEXT	 TO W34-SALUTATION
	   ELSE
      * 		     T E X T
015170	   IF SOR-TYPE = 8
	       MOVE SOR-TEXT	 TO W32-REMARKS(WS-S7)
	       ADD 1		 TO WS-S7
	   ELSE
      *                      T R A I L E R
               MOVE SOR-SUBTOT   TO W90-DB
               MULTIPLY -1 BY SOR-DSCNT    
                                 GIVING W90-TDISC
               MOVE W90-TDISC    TO W90-LDISC
               MOVE SOR-NON-TXBL TO W90-NONTAX
	       MOVE SOR-TAXABLE  TO W90-EXCTOT
               MOVE SOR-TAX      TO W90-TTAX
               MOVE SOR-TOT      TO W90-TOTAL
               MOVE SOR-BAL      TO W90-BAL
	       MOVE SOR-INSTRUCT TO W90-INSTRUCT
               MOVE SOR-PMETH    TO W90-PMETH
               MOVE SOR-STATUS   TO W40-STATUS
               MOVE SOR-HTEL     TO W40-HTEL
               MOVE SOR-ID       TO W40-ID
               IF W14-SUB > 1
                   GO TO GET-ADJUST-TOTALS
               ELSE
                   GO TO GET-SORDER-EXIT.
             GO TO GET-SORD-NEXT.

       GET-SORD-WRITE-SALE.
             MOVE WS-LAST        TO SAL-CODE.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
             ADD 1               TO WS-LAST WS-SALKEY.

       GET-CHECK-STOCK-FILE.
	     MOVE SOR-EXT-ITEM	 TO STK-CODE.
013560       PERFORM READ-STOCK THRU READ-STOCK-EXIT.

       GET-CHANGE-TO-REMKS.
             INITIALIZE SAL-RECORD1.
	     STRING SOR-EXT-ITEM DELIMITED BY SIZE
                    SOR-DESC DELIMITED BY SIZE
                                 INTO SAL-DESC.
	     MOVE SOR-SQN	 TO SAL-SQN W14-SQN(W14-SUB).
             ADD 1               TO W14-SUB.
             PERFORM GET-SORD-WRITE-SALE.
             GO TO GET-SORD-NEXT.

       GET-ADJUST-TOTALS.
             SUBTRACT 1 FROM W14-SUB
                                 GIVING WS-SUB.
             MOVE 1              TO W14-SUB.

       GET-ADJUST-TOTALS-LOOP.
	     MOVE W14-SQN(W14-SUB)
                                 TO SOR-SQN.
             PERFORM READ-SORDER-LOCK THRU READ-SORDER-EXIT.
025000       SUBTRACT SOR-VAL    FROM W90-DB.
025010       SUBTRACT SOR-DISC   FROM W90-LDISC.
025020       SUBTRACT SOR-NONTAX FROM W90-NONTAX.
	   IF NOT(SOR-VAT-SUB = ZERO)
025030	       SUBTRACT SOR-TAXVAL FROM W90-TAXVAL(WS-VAT-SUB).
025050       COMPUTE W90-BAL = W90-BAL - (SOR-COST * SOR-QNT).
             MOVE 3              TO SOR-TYPE.
	     STRING SOR-EXT-ITEM DELIMITED BY SIZE
                    SOR-DESC DELIMITED BY SIZE
                                 INTO SOR-DESC.
	     MOVE SPACES	 TO SOR-EXT-ITEM SOR-SERNO.
             MOVE ZERO           TO SOR-QNT SOR-COST SOR-INCTAX
                                    SOR-VAT SOR-SELL SOR-NONTAX
                                    SOR-VAL SOR-DISC SOR-TAXVAL
                                    SOR-DSC.
      *				     SOR-QORD.
             PERFORM REWRITE-SORDER-UNLOCK THRU WRITE-SORDER-EXIT.
           IF W14-SUB < WS-SUB
               ADD 1             TO W14-SUB
               GO TO GET-ADJUST-TOTALS-LOOP.
       
013470 GET-SORDER-EXIT.
013480       EXIT.
      /
016060 BA000                    SECTION.
016070 BA00.
      *                            P R O C E S S   O R D E R
016320       MOVE "O"            TO W90-ORG.
016340       PERFORM BC000.
      *                            E X I T   P R O G R A M
016250       GO TO AZ000-EOJ.

      /
       BB000		   SECTION 51.
       BB00.
	   IF DEB-NCASH = "CASH SALE" OR "KONTANT V"
               GO TO BB06.

       BB05.
	     MOVE "Enter client details Y/N  [Y]"
				 TO WS-ERR-MES.
	     MOVE "Y"		 TO WS-OPTION.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "N" OR "Y".
	   IF WS-OPTION = "N"
	       GO TO BB29.

       BB06.
	     PERFORM SAVE-SCREEN-2.
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
	     DISPLAY S40.
	     PERFORM SCREEN-SHADOW.
       BB15.
	     ACCEPT S41.
	     PERFORM CHECK-CORRECT.
020250	   IF WS-OPTION = "N"
               GO TO BB15.
	   IF DEB-NCASH = "CASH SALE" OR "KONTANT V"
               IF (W40-NM1 = SPACES) OR
		  (W40-AD1 = SPACES)
		   MOVE "Require Name & Address"
				 TO WS-ERR-STRING
003260		   PERFORM ERROR-MESSAGE
		   GO TO BB15.
	   IF DEB-NCASH = "CASH SALE"
               MOVE W40-NM1      TO W40-NAME
               MOVE W40-AD1      TO W40-ADD1
               MOVE W40-AD2      TO W40-ADD2
               MOVE W40-AD3      TO W40-ADD3
               MOVE W40-AD4      TO W40-ADD4
               MOVE W40-PC       TO W40-PC1.
       BB20.
011320	     PERFORM RESTORE-SCREEN-2.
	     DISPLAY CLEAR-L25.
       BB29.
             EXIT.
      /
       BB030	       SECTION 52.
       BB30.
             MOVE ZERO           TO WS-ERROR.
             MOVE 1              TO WS-S1.
             MOVE W10-QUANT      TO WS-S2.
011050       PERFORM SAVE-SCREEN.
	     ADD 1 LIN		 GIVING CLIN.
       BB40.
	     MOVE 43		 TO CCOL.
	     DISPLAY "Serial No:" AT CPOS
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
			   BACKGROUND-COLOR 7
			"                    (   )"
			 WITH FOREGROUND-COLOR 7 HIGHLIGHT
			      BACKGROUND-COLOR 7.
       BB41.
             MOVE WS-S1          TO W100-SER.
             MOVE 74             TO CCOL.
	     DISPLAY W100-SER AT CPOS
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 7.
       BB42.
	     MOVE 54		 TO CCOL.
	     ACCEPT W43-SERNO(WS-S1) AT CPOS
                    WITH FOREGROUND-COLOR 0 
                         BACKGROUND-COLOR 7 UPDATE AUTO.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     MOVE SPACES TO W43-SERIALS
                     GO TO BB45
                 WHEN OTHER
                     CALL X"E5"
                     GO TO BB42
               END-EVALUATE.
	     DISPLAY CLEAR-L25.
	   IF W43-SERNO(WS-S1) = SPACES
               GO TO BB42.
             MOVE 10             TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W43-SERNO(WS-S1)
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	   IF W43-SERNO(WS-S1) = "NONE"
	       MOVE SPACES	 TO W43-SERNO(1)
               MOVE 1            TO WS-S2
               GO TO BB45.
             MOVE STK-CODE       TO GUA-CODE.
	     MOVE W43-SERNO(WS-S1)
                                 TO GUA-SERIAL.
             PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR = ZERO
	       MOVE "Serial number duplicated"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO BB47.

       BB44.
           IF WS-S1 < WS-S2
               ADD 1             TO WS-S1
               GO TO BB41.

       BB45.
011320       PERFORM RESTORE-SCREEN.
	     GO TO BB49.

       BB47.
032920	     MOVE "'C'ontinue or 'R'ekey  [R]"
				 TO WS-ERR-MES.
	     MOVE "R"		 TO WS-OPTION.

       BB48.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "C" OR "R".
	     DISPLAY CLEAR-L25.
           IF WS-OPTION = "C"
               GO TO BB44.
             GO TO BB42.
       BB49.
             EXIT.
      /
      *    ****    P R O C E S S   O R D E R S 
      *
018500 BC000	     SECTION 5.
      *
      *    ****    S A L E S   O R D E R   S C R E E N
      *
018570 BC05.
018580	     DISPLAY CLR-SCREEN.
018590       DISPLAY S30.
018620       MOVE 8              TO LIN.
018630       PERFORM BC115 UNTIL LIN > 18.
018640       MOVE W12-TODAY      TO W90-DTE.
018650	     DISPLAY W90-DTE AT 0570
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
018660	     MOVE ZERO		 TO W90-BDT W90-LDISC W90-NONTAX
018670				    W90-BAL W90-SALES W90-EXCTOT
				    W90-CUR W90-PMETH W40-STATUS.
018680       MOVE 1              TO WS-FIRST WS-LAST.
018690	     MOVE SPACES	 TO W90-TAX W90-INSTRUCT W40-SPAR.
	     MOVE "N"		 TO WS-NOSELL.
	     MOVE 1		 TO WS-S9.

       BC08.
018660	     MOVE ZERO		 TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
	   IF WS-S9 < 12
	       ADD 1		 TO WS-S9
	       GO TO BC08.
	     MOVE ZERO		 TO W10-SORD.
             MOVE "N"            TO WS-MULTI.
             INITIALIZE W14-ENTRIES.

018710 BC10.
             PERFORM AA400.
025260       ACCEPT S30A AT 0510.
	   IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     MOVE ZERO   TO W10-SORD
                     GO TO BC599
		 WHEN F1-KEY
		     PERFORM HELP-ROUTINE
		 WHEN F2-KEY
		     MOVE "O"	 TO WS-ETYPE
		 WHEN F3-KEY
		     MOVE "A"	 TO WS-ETYPE
		 WHEN F4-KEY
		     MOVE "T"	 TO WS-ETYPE
		 WHEN OTHER
                     CALL X"E5"
                     GO TO BC10
               END-EVALUATE
020770	       PERFORM AA50-LOOKUP
020780         DISPLAY S30A AT 0510
020790         IF W10-SORD = ZERO
020800             GO TO BC10.
018850	     DISPLAY CLEAR-L25.
020810     IF W10-SORD = ZERO
020830         GO TO BC599.
             MOVE W10-SORD       TO W90-REF.

018890 BC12.
             PERFORM GET-SORDER.
           IF WS-ERROR = 1
	       MOVE WS-ERR2	 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO BC10.
           IF WS-ERROR = 2
	       MOVE "Not an order"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO BC10.
           IF WS-ERROR = 3
	       MOVE "Order corrupted"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO BC10.
           IF WS-ERROR = 4
	       MOVE "Invalid a/c no"
				 TO WS-ERR-STRING
003260	       PERFORM ERROR-MESSAGE
               GO TO BC10.
           IF W90-EXP = "E" OR "X"
               MOVE W05-VATNO    TO W90-TAX
           ELSE
               MOVE SPACES       TO W90-TAX.
018820       DISPLAY S36 AT 0308
016780	     DISPLAY W40-NAME AT 0320
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
019900	     DISPLAY W90-ORD AT 0368
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
017670			  BACKGROUND-COLOR 5.
019970	     DISPLAY W10-SMAN AT 0411
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
017710			  BACKGROUND-COLOR 5.
020010	   IF W10-SMAN NOT = ZERO
020030         ADD 100 W10-SMAN    GIVING WS-PARKEY
020040         PERFORM READ-PARAM THRU READ-PARAM-EXIT
020100	       DISPLAY PAR-SALESMAN AT 0420
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
	     DISPLAY W40-TEL AT 0466
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY W40-CONTACT AT 0528
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     MOVE 1		 TO WS-FIRST WS-SALKEY.
             SUBTRACT 1          FROM WS-LAST.

018990 BC15.
022610       MOVE W90-DB         TO W90-TOTAL.
022620       SUBTRACT W90-LDISC  FROM W90-TOTAL.
022630       MOVE W90-DB         TO W100-BAL.
022640	     DISPLAY W100-BAL AT 2015
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
022650	     MULTIPLY -1 BY W90-LDISC
022660                           GIVING W100-BAL.
022670	     DISPLAY W100-BAL AT 2115
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
      *
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *
022690	   IF W90-TAX = SPACES
022700         GO TO BC20.
	     MOVE 1		 TO WS-S1.
	     MOVE ZERO		 TO W25-TOTAL.

       BC18.
023070	   IF W90-TAXVAL(WS-S1) NOT = ZERO
023060	       MOVE W05-VAT(WS-S1)
023060				 TO W05-RATE
023080	       COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
		      (W90-TAXVAL(WS-S1) * W05-RTE).
	   IF WS-S1 < 6
	       ADD 1		 TO WS-S1
	       GO TO BC18.
022740	     MOVE W25-TOTAL	 TO W100-BAL.
022760	     ADD W25-TOTAL	 TO W90-TOTAL.
022750	     DISPLAY W100-BAL AT 2215
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.

022770 BC20.
022780	     MOVE W90-TOTAL	 TO W100-BAL.
022790	     DISPLAY W100-BAL AT 2315
		     WITH FOREGROUND-COLOR 3 HIGHLIGHT
			  BACKGROUND-COLOR 5.
      *
      *    ****    D I S P L A Y   S A L E S   D E T A I L S
      *
029120 BC25.
029130       MOVE 8              TO LIN.
029140       PERFORM BC115 UNTIL LIN > 18.
029150       MOVE 8              TO LIN.
029160       MOVE 1              TO WS-LINES WS-FIRST.

029170 BC30.
029180	     MOVE WS-FIRST	 TO WS-SALKEY.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
029190 BC35.
029200	   IF WS-SALKEY > WS-LAST
029210         GO TO BC45.
029220       PERFORM READ-SALE THRU READ-SALE-EXIT.
	     MOVE SAL-EXT-ITEM	 TO W10-EXT-ITEM.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
029240	   IF SAL-EXT-ITEM = SPACES
023480	       DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
029260	       GO TO BC40.
      *
      *        *****   E X T E N D E D	 C O D E   *****
      *
	   IF WS-EXT-STK = "Y"
	       DISPLAY S07-EXT AT LINE LIN
	   ELSE
      *
      *    *****   A L P H A - N U M E R I C   C O D E	 *****
      *
	   IF W10-STCK = "A"
	       IF WS-USE-ITM = "Y"
		   DISPLAY S07-ITM AT LINE LIN
	       ELSE
021070		   DISPLAY S07 AT LINE LIN.
023510	     DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
029290	     MOVE SAL-QNT	 TO W10-QUANT.
029300       DISPLAY S26-QNT AT LINE LIN.
029310       MOVE SAL-SELL       TO W10-SELL.
029320       DISPLAY S26-PRC AT LINE LIN.
029330       MOVE SAL-VAL        TO W100-EXT.
023570	     DISPLAY W100-EXT AT COLUMN 70
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
	     MOVE SPACES	 TO W43-SERIALS.
           IF STK-GUAR = ZERO
               GO TO BC42.
           IF SAL-SERNO NOT = SPACE
               MOVE 1            TO WS-S1
               MOVE W10-QUANT    TO WS-S2
               GO TO BC42.
             PERFORM BB030.
           IF WS-ERROR = 1
               GO TO BC599.
             PERFORM AY60 THRU AY999.
             MOVE SAL-SQN        TO SOR-SQN.
             MOVE W10-SORD       TO SOR-REF.
	     PERFORM READ-SORDER-LOCK THRU READ-SORDER-EXIT.
             MOVE "Y"            TO SOR-SERNO
                                    SAL-SERNO.
             PERFORM REWRITE-SORDER-UNLOCK THRU WRITE-SORDER-EXIT.
             PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
             MOVE "Y"            TO WS-MULTI.
             MOVE W10-QUANT      TO WS-S2.
             MOVE 1              TO WS-S1.
             MOVE 3              TO SOR-TYPE.

       BC36.
             MOVE 1              TO WS-S3.
             MOVE SPACES         TO SOR-DETAIL.

       BC37.
	     MOVE W43-SERNO(WS-S1)
				 TO SOR-SNO(WS-S3).
             ADD 1               TO WS-S1.
           IF WS-S1 > WS-S2
               PERFORM BC38
               GO TO BC40.
	   IF WS-S3 < 2
               ADD 1             TO WS-S3
               GO TO BC37.

       BC38.
             ADD 1               TO SOR-SUB.         
             PERFORM WRITE-SORDER THRU WRITE-SORDER-EXIT.

       BC39.
             ADD 1               TO WS-S1.
           IF WS-S1 NOT > WS-S2
               GO TO BC36.
      *
      *    ****    I S   T H E   S C R E E N   F U L L
      *
029350 BC40.
             PERFORM AY70 THRU AY999.
029350 BC42.
029360     IF LIN < 18
029370         ADD 1             TO LIN WS-SALKEY WS-LINES
029380         GO TO BC35.
	     MOVE "Press any key to continue"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
	     PERFORM BC25.
             GO TO BC35.

       BC45.
             EXIT.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
029390 BC50.
029420	     MOVE "Print 'I'nvoice - 'E'xit"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.

029660 BC55.
	     PERFORM OPT-MESSAGE TEST AFTER
		     UNTIL WS-OPTION = "E" OR "P" OR "I" OR "*".
029750     IF WS-OPTION = "E"
               IF WS-MULTI = "Y"
                   MOVE "D"      TO WS-MULTI
                   PERFORM GET-SORDER
               END-IF
029760         GO TO BC05.
      *
      *        R U M A R C   -	 O N L Y
      *
	   IF WS-OPTION = "*"
	       PERFORM MA000.
	   IF DEB-TERMS = "3"
	       PERFORM BB000.
      *
      *    ****    C L E A R   S C R E E N
      *
029920 BC60.
029930	     MOVE 8		 TO LIN.
029940       PERFORM BC115 UNTIL LIN > 18.
029950       MOVE 8              TO LIN.
029960       MOVE 1              TO WS-LINES.
024350	     MOVE ZERO		 TO W90-TTAX.
	     MOVE 1		 TO WS-P1.
      *
      *    ****    C A L C U L A T E   T O T A L   V A T
      *
       BC61.
037540	   IF (W90-TAXVAL(WS-P1) = ZERO)
024430	       GO TO BC62.
024440	     MOVE W05-VAT(WS-P1) TO W05-RATE.
021490*	     COMPUTE W90-TTAX ROUNDED = (W90-TTAX +
      *		    (W90-TAXVAL(WS-P1) * W05-RTE)).
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Split the calculation into two COMPUTE statements as the ³
      *    ³  VAT report was occasionally out of balance (VAT dropping ³
      *    ³  the decimals of cents).				       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	     COMPUTE W90-VAT(WS-P1) ROUNDED =
		     W90-TAXVAL(WS-P1) * W05-RTE.
021490	     COMPUTE W90-TTAX ROUNDED = W90-TTAX + W90-VAT(WS-P1).
      *
      *    ****    C H E C K   I F   A L L   V A T   R A T E S
      * 	   H A V E   B E E N   C A L C U L A T E D
      *
       BC62.
	   IF WS-P1 < 6
	       ADD 1		 TO WS-P1
	       GO TO BC61.
      *
      *    ****    P R I N T   I N V O I C E
      *
       BC65.
             DISPLAY CLR-SCREEN.
	     DISPLAY "PROCESS INVOICE" AT 0236
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
             DISPLAY "Printing invoice" AT 1212
		      WITH FOREGROUND-COLOR 6 HIGHLIGHT
	     MOVE W90-REF	 TO LS-SLS-ORDNO.
	     MOVE WS-NOSELL	 TO W40-NOSELL.
	     CLOSE SALE.
004200	     CALL PRG-INVOICE USING LS-PARID
				    LS-USER-ID
				    LS0-PROGRAMS
				    LS0-SECURITY
				    W40-COMPANY
				    W90-TRAN
		  ON EXCEPTION
		     GO TO BC100
	     END-CALL.
	     CANCEL PRG-INVOICE.
028820       OPEN OUTPUT SALE.
028830       CLOSE SALE.
028840	     OPEN I-O SALE.
029700	     MOVE "ENTER to continue - E to exit"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
	     PERFORM OPT-MESSAGE.
028940	   IF NOT(WS-OPTION = "E")
028960         GO TO BC05.
             GO TO BC599.

       BC100.
	     MOVE SPACES     TO WS-ERR-MES.
	     STRING "Program- " DELIMITED SIZE
		     PRG-INVOICE DELIMITED " "
		     " not on disk, press ANY key"
		     DELIMITED SIZE
		     INTO WS-ERR-MES.
	     PERFORM ERROR-LENGTH THRU ERROR-EXIT.
	     GO TO BC599.
023520 BC115.
028770	     DISPLAY "³" AT LINE LIN COLUMN 1
			 WITH BACKGROUND-COLOR 0 FOREGROUND-COLOR 3
		      WS-INV78 WITH FOREGROUND-COLOR 3
		     "³" WITH BACKGROUND-COLOR 0 FOREGROUND-COLOR 3.
023540	     ADD 1		 TO LIN.
031080 BC599.
031090       EXIT.
      /
       MA000	       SECTION 89.
       MA00.
	     PERFORM SAVE-SCREEN.
	     MOVE SPACES	 TO WS-PASS.
	     MOVE 12		 TO SHADE-ROW.
	     MOVE 29		 TO SHADE-COL.
	     MOVE 25		 TO SHADE-WIDTH.
	     MOVE 2		 TO SHADE-LINES.
	     DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1127
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
042540	     DISPLAY "³ " AT 1227 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				       BACKGROUND-COLOR 7
		     "Enter Password          "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 7
		     "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     DISPLAY "À" AT 1327 WITH FOREGROUND-COLOR 7 HIGHLIGHT
				      BACKGROUND-COLOR 7
		     "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		      WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
	     PERFORM SCREEN-SHADOW.
042550	     ACCEPT WS-PASS AT 1244
		    WITH BACKGROUND-COLOR 5
			 FOREGROUND-COLOR 3 HIGHLIGHT AUTO NO-ECHO.
	     MOVE 8		 TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING WS-PASS
				BY VALUE WS-LENGTH
				RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
	     PERFORM RESTORE-SCREEN.
	   IF WS-PASS = "AVOIDING"
	       MOVE "Y" 	 TO WS-NOSELL
	   ELSE
	   IF WS-PASS = "PRINTING"
	       MOVE "P" 	 TO WS-NOSELL
	   ELSE
	       MOVE "N" 	 TO WS-NOSELL.

       MA99.
	     EXIT.
      /
049180 ZA000-INIT	       SECTION 90.
049190 ZA000-OPEN.
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
      *    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
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
	     MOVE "GARTEE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-GARTEE.
      *    *************************************************************
      *    **** 	    C O N T R O L   F I L E		    ****
      *    *************************************************************
	     MOVE "NETWORK"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-NETWORK.
      *    *************************************************************
      *    **** 		S A L E S   F I L E		    ****
      *    *************************************************************
	     MOVE "SALE"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     INSPECT AFID-PATH REPLACING FIRST "XXX"
			       BY LS-USER.
	     MOVE AFID-PATH	 TO W02-SALE.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E	    ****
      *    *************************************************************
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
      *    *************************************************************
      *    **** 	  S A L E S   O R D E R   F I L E	    ****
      *    *************************************************************
	     MOVE "SORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SORDER.
      *	   *************************************************************
      *    **** 	  S T O C K   P A R T S   F I L E	    ****
      *    *************************************************************
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
      *    *************************************************************
      *    **** 	       S T O C K   F I L E		    ****
      *    *************************************************************
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
049320	     MOVE 3		 TO WS-PARKEY.
049330       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
	   IF NOT(PAR-STOCK = "A" OR "N")
	       MOVE "A" 	 TO PAR-STOCK.
	     MOVE PAR-STOCK	 TO W10-STCK.
	   IF PAR-STOCK = "N"
	       MOVE PAR-SLNGTH	 TO W10-SLNGTH
	   ELSE
	       MOVE 14		 TO W10-SLNGTH.
	   IF W10-SLNGTH < 3
	       MOVE 3		 TO W10-SLNGTH
	   ELSE
	   IF W10-SLNGTH > 14
	       MOVE 14		 TO W10-SLNGTH.
	     MOVE PAR-PACKS	 TO WS-USE-PACKS.
	     MOVE PAR-USE-ITM	 TO WS-USE-ITM.
	     MOVE PAR-EXT-STK	 TO WS-EXT-STK.
	     MOVE PAR-VAT-REG	 TO W05-VATNO.
025110	     MOVE 3		 TO WS-PARKEY.
	     READ PARAM.
	   IF PAR-INV = ZERO
	       MOVE 1		 TO PRG-NUMBER
	   ELSE
	       MOVE PAR-INV	 TO PRG-NUMBER.
050280	     OPEN OUTPUT SALE.
050290       CLOSE SALE.
050300       OPEN I-O SALE.
050520	     MOVE 5		 TO WS-PARKEY.
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
050890       MOVE 7              TO WS-PARKEY.
050900       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050910       MOVE PAR-CASH-AC    TO WS-CASH-AC.
051530       GO TO ZA999.

       COPY ZA49.PRO.

      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
040050	     MOVE 1		 TO WS-S1.
040060	     MOVE SPACES	 TO WS-MD-LINE.

040090 ZA60.
040130	     MOVE WS-G1 	 TO WS-TCA(WS-S1) WS-BCA(WS-S1).
040150	   IF WS-S1 < 80
040160	       ADD 1		 TO WS-S1
040170         GO TO ZA60.
040220	     MOVE WS-G14	 TO WS-TCA(1)  WS-BCA(1).
040260	     MOVE WS-G13	 TO WS-TCA(80) WS-BCA(80).
040300	     MOVE WS-G2 	 TO WS-TCA(10) WS-TCA(17)
040360				    WS-TCA(47) WS-TCA(56)
040370				    WS-TCA(59) WS-TCA(68).
040400	     MOVE WS-G3 	 TO WS-MCA(1)  WS-MCA(80)
040470				    WS-MCA(10) WS-MCA(17)
040480				    WS-MCA(45) WS-MCA(56)
040490				    WS-MCA(59) WS-MCA(68).
040550	     MOVE WS-G4 	 TO WS-BCA(10) WS-BCA(17)
040610				    WS-BCA(47) WS-BCA(56)
040620				    WS-BCA(59) WS-BCA(68).
040650	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "SupervisorÄ"
				 TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"
				 TO WS-WRKHD
	       MOVE LS-USER	 TO WS-WRKST.
051540 ZA200.

       COPY LOCKED.PRO.

       ZA205.
051600       EXIT PROGRAM.

051610 ZA999.
051620       EXIT.

051640 I-O-ERRORS      SECTION	91.

       COPY ERRORS.PRO.

051790 DISPLAY-FILE-NAME.
051840     IF WS-F-ERROR = 2
051850	       MOVE W02-NETWORK  TO WS-FILE
051860	       MOVE WS-NETKEY	 TO WS-KEY
051910     ELSE
051920     IF WS-F-ERROR = 6
041780	       MOVE W02-DEBTOR	 TO WS-FILE
041790         MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
041070	   ELSE
041080     IF WS-F-ERROR = 7
041820	       MOVE W02-DEPART	 TO WS-FILE
041870	       MOVE ZERO	 TO WS-KEY
041830	       MOVE DPT-CODE	 TO WS-KEYX
	   ELSE
           IF WS-F-ERROR = 9
	       MOVE W02-GARTEE	 TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE GUA-KEY      TO WS-KEYX
052070	   ELSE
052080     IF WS-F-ERROR = 15
052090         MOVE WS-PARID     TO WS-FILE
052100         MOVE WS-PARKEY    TO WS-KEY
052150     ELSE
052160     IF WS-F-ERROR = 19
052170	       MOVE W02-SALE	 TO WS-FILE
052180	       MOVE WS-SALKEY	 TO WS-KEY
052190     ELSE
052200     IF WS-F-ERROR = 20
052210         MOVE W02-SORDER   TO WS-FILE
052220         MOVE ZERO         TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 21
041860         MOVE W02-SPARTS   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY
052190     ELSE
052200     IF WS-F-ERROR = 22
052210         MOVE W02-STOCKF   TO WS-FILE
052220	       MOVE ZERO	 TO WS-KEY
041840	   ELSE
041850	   IF WS-F-ERROR = 37
041860	       MOVE W02-SHARED	 TO WS-FILE
041870	       MOVE WS-SHARED	 TO WS-KEY.

       COPY DISPERR.PRO.
