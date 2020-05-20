      $set LINKCOUNT"596"
      ******************************************************************
      *                                                                *
      *    ******   **	       ******	 ********   ******   **    **  *
      *   **	**  **	      **    **	    **	   **	 **  **   **   *
      *   **	    **	      **	    **	   **	     **  **    *
      *    ******   **	       ******	    **	   **	     *****     *
      * 	**  **		    **	    **	   **	     **  **    *
      *   **	**  **	      **    **	    **	   **	 **  **   **   *
      *    ******   ********   ******	    **	    ******   **    **  *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *      P R O C E S S   S A L E S   O R D E R   P R O G R A M     *
      *                                                                *
      *     VERSION 8.14.03 - May 2010				       *
      * 							       *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     SLSTCK.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   AUGUST 1991.

		   COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2010
				     by James William Lemmon.

		   All rights reserved.

000050 SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
000060		   James William Lemmon.(Id No. 4412165050082).

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
002310 77  WS-DBTKEY       PIC  9(06)    COMP-5.
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
002420 77  WS-ADV-RATE     PIC  9(03)V99 COMP-3.
002430 77  WS-START        PIC  9(08).
002440 77  WS-END          PIC  9(08).
002450 77  WS-TIME         PIC  9(08).
002460 77  WS-TME2         PIC  9(08).
002500 77  WS-PAR	   PIC	9(01) VALUE 0.
002510 77  WS-INO	   PIC	9(03) VALUE 0.
002520 77  WS-LINES        PIC  9(02) VALUE 0.
002530 77  WS-FIRST	   PIC	9(03) VALUE 0.
002540 77  WS-LAST	   PIC	9(03) VALUE 0.
002550 77  WS-LASTINV      PIC  9(04)    COMP-5.
002560 77  WS-LINE         PIC  9(04)    COMP-5.
002570 77  WS-PAGE         PIC  9(02)    COMP-5.
002580 77  WS-L1           PIC  9(04)    COMP-5.
002590 77  WS-OPTION       PIC  X(01).
002610 77  WS-SKIP         PIC  X(01) VALUE " ".
002610 77  WS-LOOK         PIC  X(01) VALUE "S".
002620 77  WS-MULTI        PIC  X(01) VALUE "N".
002620 77  WS-ETYPE        PIC  X(01) VALUE "I".
002650 77  WS-ERROR        PIC  9(01).
002660 77  WS-SELECT       PIC  X(13) VALUE "Select Option".
002670 77  WS-CHAR1        PIC  X(01).
002680 77  WS-CHAR2        PIC  X(01).
002690 77  WS-COMP         PIC  9(01).
002710 77  WS-UPDATE       PIC  9(01) VALUE ZERO.
002720 77  WS-CONREC       PIC  9(01) VALUE ZERO.
002730 77  WS-HEAD         PIC  X(01).
002740 77  WS-TYPE         PIC  X(01).
002750 77  WS-AGE          PIC  X(01).
002760 77  WS-PRC          PIC  X(01).
002770 77  WS-INDEX-STORE  PIC  X(04).
002780 77  WS-INDEX-NAME   PIC  X(04).
002830 77  WS-BATCH        PIC  9(05).
002860 77  WS-A 	   PIC	X(19) VALUE "Any key to continue".
002870 77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
002010 77  WS-ERR1         PIC  X(22) VALUE "Invalid Account Number".
002020 77  WS-ERR2         PIC  X(09) VALUE "No Record".
002880 77  WS-PER          PIC  Z(02)9.99.
002890 77  WS-CR           PIC  X(01).
002900 77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY	   PIC	9(06) COMP-5.
       77  WS-USUB	   PIC	9(04) COMP-5.

       01  PRG-INVOICE.
	   03  FILLER	   PIC	X(08) VALUE "SLP\OINV".
	   03  PRG-NUMBER  PIC	9(02) VALUE ZERO.
      /
002910 01  WS-DB-LINE.
003060	   03  WS-TOP-LINE.
003070	       05  WS-TCHAR PIC  X(80) VALUE "ฦออออออออออออออัอออัออออออ
      -        "อออออออออออออออออออออออัออออออออัอออออออออัออออออออออต".
003080     03  WS-TOP-LNE2.
001390	       05  WS-TCR   PIC  X(80) VALUE "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
      -        "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ".
003100	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110         05  FILLER      PIC  X(01).
003120         05  WS-TOP-COMP PIC  X(40).
001430	       05  FILLER      PIC  X(23).
	       05  WS-WRKHD    PIC  X(11).
001430	       05  FILLER      PIC  X(01).
	       05  WS-WRKST    PIC  X(03).
001430	       05  FILLER      PIC  X(01).
003140	   03  WS-MID-LINE.
001450	       05  WS-MCHAR PIC  X(80) VALUE "ณ              ณ   ณ
      -        "                       ณ        ณ         ณ          ณ".
	   03  WS-MINV REDEFINES WS-MID-LINE.
	       05  WS-INV1     PIC  X(01).
	       05  WS-INV78    PIC  X(78).
	       05  WS-INV80    PIC  X(01).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "ณ".
	       05  WS-BLNK78   PIC  X(78) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "ณ".
003180	   03  WS-BOT-LINE.
003190	       05  WS-BCHAR PIC  X(80) VALUE "ฦออออออออออออออฯอออฯออออออ
      -        "อออออออออออออออออออออออฯออออออออฯอออออออออฯออออออออออต".
003200	   03  WS-BOT-LNE2.
001510	       05  WS-BCR   PIC  X(80) VALUE "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
      -        "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู".
003240	   03  WS-B-LINE REDEFINES WS-BOT-LNE2.
003250         05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
003260            07  WS-SRT-LINE
003270                     PIC X(50).
                  07  FILLER
                           PIC X(10).
003280         05  FILLER  PIC X(19).
	   03  WS-MM-LINE.
	       05  FILLER      PIC  X(01) VALUE "ณ".
	       05  WS-SPACE    PIC  X(60) VALUE SPACES.
	       05  FILLER      PIC  X(01) VALUE "ณ".
	   03  WS-HME-LINE PIC	X(60) VALUE ALL "อ".
003220	   03  WS-TOP-LNE.
003230	       05  WS-TCHR  PIC	 X(80) VALUE "ฦออออออออออออออออัออออออออ
      -        "ออออออออออออออออออออออัออออออออออัอออออออออัอออออออออต".
003240     03  WS-T-LINE REDEFINES WS-TOP-LNE.
003250         05  FILLER  PIC X(01).
003260         05  WS-H-LINE
003270                     PIC X(78).
003280         05  FILLER  PIC X(01).
003290     03  WS-MID-LNE.
003300	       05  WS-MCHR  PIC  X(80) VALUE "ณ                ณ
      -        "                      ณ          ณ         ณ         ณ".
	   03  WS-MST-LNE REDEFINES WS-MID-LNE.
	       05  WS-MST1     PIC  X(01).
	       05  WS-MST78    PIC  X(78).
	       05  WS-MST80    PIC  X(01).
003310	   03  WS-BOT-LNE.
003320	       05  WS-BCHR  PIC	 X(80) VALUE "ฦออออออออออออออออฯออออออออ
      -        "ออออออออออออออออออออออฯออออออออออฯอออออออออฯอออออออออต".
003330	   03  WS-TP-LNE.
003340	       05  WS-TCH   PIC  X(80) VALUE "ฦออออออัออออออออออออออออออ
      -        "ออออออออออออออออออออออัอออออออออออัออออออออออัอออออออต".
003350     03  WS-MD-LNE.
003360	       05  WS-MCH   PIC  X(80) VALUE "ณ      ณ
      -        "                      ณ           ณ          ณ       ณ".
	   03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
	       05  WS-MDT1     PIC  X(01).
	       05  WS-MDT78    PIC  X(78).
	       05  WS-MDT80    PIC  X(01).
003370	   03  WS-BT-LNE.
003380	       05  WS-BCH   PIC  X(80) VALUE "ฦออออออฯออออออออออออออออออ
      -        "ออออออออออออออออออออออฯอออออออออออฯออออออออออฯอออออออต".
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
003250 01  WS-ENTRY.
003260     03  WS-ENT0     PIC  X(08).
003270     03  WS-TYPE0    PIC  X(01).
003280     03  WS-KEY0     PIC  9(06)    COMP-5.
003290 01  WS-ENTRY1.
003300     03  WS-ENT1     PIC  X(08).
003310     03  WS-TYPE1    PIC  X(01).
003320     03  WS-KEY1     PIC  9(06)    COMP-5.

003340 COPY WS.WS.

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

000250 COPY GARTEE.ID.

       COPY PARAM.ID.

003710 COPY SALE.ID.

       COPY SHARED.ID.

003670 COPY SORDER.ID.

000360 COPY SPARTS.ID.

003730 COPY STOCK.ID.

003790 01  W05-TAX-RATES.
003800     03  W05-VAT-RATES.
003810         05  W05-VAT     PIC S9(03)V99 COMP-3 OCCURS 6.
003820     03  W05-TAX-RATES REDEFINES W05-VAT-RATES.
003830         05  W05-VAT-R1  PIC S9(03)V99 COMP-3.
003840         05  W05-VAT-R2  PIC S9(03)V99 COMP-3.
003850         05  W05-VAT-R3  PIC S9(03)V99 COMP-3.
003860         05  W05-VAT-R4  PIC S9(03)V99 COMP-3.
003870         05  W05-VAT-R5  PIC S9(03)V99 COMP-3.
003880         05  W05-VAT-R6  PIC S9(03)V99 COMP-3.
           03  W05-RATE        PIC S9(03)V99 COMP-3.
           03  W05-RTE REDEFINES W05-RATE
                               PIC S9V9(04)  COMP-3.
           03  W05-VATNO       PIC  X(14).
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
002550 01  W10-STOCK.
002560     03  W10-ENTRY      PIC X(14).
002560     03  W10-ITEM       PIC X(14).
002570     03  W10-CODE1      PIC X(14).
002580     03  W10-COD1 REDEFINES W10-CODE1.
002590         05  W10-CD1    PIC X(02).
002600         05  W10-CDE1   PIC X(12).
002610     03  W10-CODE2      PIC X(14).
002620     03  W10-COD2 REDEFINES W10-CODE2.
002630         05  W10-CD2    PIC X(02).
002640         05  W10-CDE2   PIC X(12).
002650     03  W10-ACODE      PIC X(10).
002660     03  W10-ACDE1 REDEFINES W10-ACODE.
002670         05  W10-ACRSE  PIC X(01).
002680         05  W10-ASFINE PIC X(02).
002690         05  FILLER     PIC X(07).
002750     03  W10-CAT        PIC 9(02) VALUE ZERO.
002760     03  W10-BIN        PIC X(03).
	   03  W10-SERNO      PIC X(10).
002770     03  W10-DESC.
	     04  W10-SDSC.
	       05  W10-DENT   PIC  X(14).
	       05  FILLER     PIC  X(15).
	     04  FILLER       PIC  X(01).
002770     03  W10-DEPT       PIC X(30).
002780     03  W10-MARKUP     PIC S9(03)V9999 COMP-3.
002790     03  W10-CMARKUP    PIC S9(03)V9999 COMP-3.
002790     03  W10-WMARKUP    PIC S9(03)V9999 COMP-3.
002800     03  W10-IND        PIC X(01).
002810     03  W10-TAX        PIC 9(01).
002820     03  W10-QUANT      PIC S9(07)V9(04) COMP-3.
002820     03  W10-QNT        PIC S9(07)V9(04) COMP-3.
002830     03  W10-REORD      PIC S9(07)V99  COMP-3.
002840     03  W10-COST       PIC S9(06)V999  COMP-3.
003580     03  W10-GUAR       PIC  9(02)     COMP-5.
002850     03  W10-SELL       PIC S9(07)V99  COMP-3.
002860     03  W10-CASH       PIC S9(07)V99  COMP-3.
002860     03  W10-WSALE      PIC S9(07)V99  COMP-3.
002850     03  W10-VSELL      PIC S9(07)V99  COMP-3.
002860     03  W10-VCASH      PIC S9(07)V99  COMP-3.
002860     03  W10-VWSALE     PIC S9(07)V99  COMP-3.
002870     03  W10-CCOST      PIC S9(07)V9(6) COMP-3.
002880     03  W10-CSELL      PIC S9(07)V9(6) COMP-3.
004080     03  W10-SPAPRC     PIC 9(05)V99  COMP-3.
002890     03  W10-LEDG       PIC 9(04)      COMP-5.
004100     03  W10-SMAN       PIC 9(02).
004110     03  W10-DISC       PIC 9(02).
002900     03  W10-ORDER      PIC S9(07)V99  COMP-3.
002910     03  W10-DATE.
               05  W10-EDTE   PIC 99/99/99.
002910     03  W10-ODTE       PIC 9(06)      COMP-3.
002910     03  W10-PURCH      PIC 9(06)      COMP-3.
002920	   03  W10-AVRG       PIC S9(06)V999 COMP-3.
004150     03  W10-MARG       PIC 9(05)V99  COMP-3.
002930     03  W10-ACTIVE     PIC X(08).
002940     03  W10-ADATE REDEFINES W10-ACTIVE PIC Z9/99/99.
002950     03  W10-DISC1      PIC 99V99.
002960     03  W10-DISC2      PIC 99V99.
002970     03  W10-ADJUST     PIC S9(03)V99  COMP-3.
002980     03  W10-ADJ REDEFINES W10-ADJUST
002990                        PIC S9V9999    COMP-3.
003000     03  W10-MTH1       PIC S9(09)V99  COMP-3.
003010     03  W10-MTH2       PIC S9(09)V99  COMP-3.
003020     03  W10-MTH3       PIC S9(09)V99  COMP-3.
003030     03  W10-MAVG       PIC S9(09)V99  COMP-3.
003040     03  W10-MTDC       PIC S9(09)V99  COMP-3.
003050     03  W10-YTDC       PIC S9(09)V99  COMP-3.
003060     03  W10-LTIME      PIC S9(01)V99  COMP-3.
003070     03  W10-KEY        PIC 9(04)      COMP-5.
003080     03  W10-MSELL      PIC S9(07)V9999 COMP-3.
003080     03  W10-MCOST      PIC S9(07)V9999 COMP-3.
002820     03  W10-MQUANT     PIC S9(07)V9999 COMP-3.
003090     03  W10-SDESC      PIC X(17).
003100     03  W10-MQNT       PIC 9(05)V9999.
003110     03  W10-COMMENT.
004020         05  W10-REMKS  PIC X(30).
               05  FILLER     PIC X(10).
           03  W10-SORD       PIC 9(08).
003120 01  W11-PARTS.
003130     03  W11-PART-DETAILS OCCURS 30.
003140         05  W11-CODE   PIC X(14).
003150         05  W11-DESC   PIC X(30).
003160         05  W11-QUANT  PIC S9(05)V9999 COMP-3.
004300 01  W12-DATE.
004310     03  W12-TODAY       PIC 9(06).
004320     03  W12-DATE-DMY REDEFINES W12-TODAY.
004330         05  W12-DAY     PIC 9(02).
004340         05  W12-MONTH   PIC 9(02).
004350         05  W12-YEAR    PIC 9(02).
004360     03  W12-T-YMD       PIC 9(06).
004370     03  W12-DATE-YMD REDEFINES W12-T-YMD.
004380         05  W12-YY      PIC 9(02).
004390         05  W12-MM      PIC 9(02).
004400         05  W12-DD      PIC 9(02).
       01  W14-ORDER.
           03  W14-REF         PIC Z(08).
           03  W14-AC          PIC X(06).
           03  W14-NAME.
               05  W14-NME     PIC X(29).
               05  FILLER      PIC X(11).
           03  W14-DATE        PIC Z9/99/99.
           03  W14-PS          PIC 9(02).
           03  W14-DOC         PIC X(01).
           03  W14-DDOC.
               05  W14-INV     PIC Z(08).
           03  W14-VALUE       PIC Z(07)9.99.
       01  W14-ENTRIES.
           03  W14-SUB         PIC 9(03)     COMP-3.
           03  W14-SQN         PIC 9(05)     COMP-3 OCCURS 200.
004410 01  W15-DISCOUNT.
004420     03  W15-DISC        PIC S9(03)V99 COMP-3 OCCURS 10.
004430 01  W20-DATE.
004440     03  W20-YY.
004450         05  W20-YEAR    PIC 9(02).
004460     03  W20-MM.
004470         05  W20-MONTH   PIC 9(02).
004480     03  W20-DD.
004490         05  W20-DAY     PIC 9(02).
004500 01  W20-DTE1 REDEFINES W20-DATE.
004510     03  W20-DTE         PIC 9(06).
003560 01  W21-DATE.
003610     03  W21-DD.
003620         05  W21-DAY     PIC 9(02).
003590     03  W21-MM.
003600         05  W21-MONTH   PIC 9(02).
003570     03  W21-YY.
003580         05  W21-YEAR    PIC 9(02).
003630 01  W21-DTE1 REDEFINES W21-DATE.
003640     03  W21-DTE         PIC 9(06).
003560 01  W22-DATE.
003610     03  W22-DD.
003620         05  W22-DAY     PIC 9(02).
003590     03  W22-MM.
003600         05  W22-MONTH   PIC 9(02).
003570     03  W22-YY.
003580         05  W22-YEAR    PIC 9(02).
003630 01  W22-DTE1 REDEFINES W22-DATE.
003640     03  W22-DTE         PIC 9(06).
004520 01  W25-CALCS.
004530     03  W25-RESULT      PIC 9(02)V99.
004540     03  W25-RESULT1 REDEFINES W25-RESULT.
004550         05  W25-WHOLE   PIC 9(02).
004560         05  W25-DECIMAL PIC 9(02).
004570     03  W25-TOTAL       PIC 9(07)V99  COMP-3.
004580     03  W25-VALUE       PIC S9(07)V99 COMP-3.
004590     03  W25-CREDIT      PIC S9(07)V99 COMP-3.

       01  W30-MESSAGE.
           03  W30-PERC        PIC Z9.99.
           03  FILLER          PIC  X(40) VALUE 
               "% Settlement discount if paid in 30 DAYS".

       01  W32-MESSAGE.
	   03  W32-REMARKS OCCURS 10
			       PIC  X(63).
       01  W34-HEADER.
	   03  W34-FORMAT      PIC  X(01).
	   03  W34-MARGIN      PIC  9(02) COMP-5.
	   03  W34-SALUTATION  PIC  X(63).

004690 01  W40-COMPANY.
004700     03  W40-NAME1.
004710         05  W40-NAME.
004720             07  W40-ENTRY PIC X(07).
004730             07  FILLER  PIC X(33).
004740         05  FILLER      PIC X(32).
004750     03  W40-NAME2 REDEFINES W40-NAME1.
004760         05  W40-SURNAME PIC X(24).
004770         05  W40-FNAME   PIC X(24).
004780         05  W40-SNAME   PIC X(24).
004790     03  W40-NAME3 REDEFINES W40-NAME1.
004800         05  W40-NCHAR OCCURS 72 PIC X(01).
004810     03  W40-ANAME       PIC X(40).
004660	   03  W40-NME.
004670         05  W40-NCH     PIC  X(01) OCCURS 32.
	   03  W40-CONTACT     PIC  X(30).
004820     03  W40-DEBTREC.
004830         05  FILLER        PIC X(330).
004840     03  W40-ADD REDEFINES W40-DEBTREC.
004850       05  W40-ADDRESS1.
004860         07  W40-ADD1    PIC X(30).
004870         07  W40-ADD2    PIC X(30).
004880         07  W40-ADD3    PIC X(30).
004890         07  W40-ADD4    PIC X(30).
004900       05  W40-ADDRESS2 REDEFINES W40-ADDRESS1.
004910         07  W40-ACHAR OCCURS 120 PIC X(01).
004920       05  W40-PC1       PIC X(04).
004930       05  W40-PADDRESS.
004940         07  W40-PADD1   PIC X(30).
004950         07  W40-PADD2   PIC X(30).
004960         07  W40-PADD3   PIC X(30).
004970         07  W40-PADD4   PIC X(30).
004980       05  W40-PADDR2   REDEFINES W40-PADDRESS.
004990         07  W40-PCHAR OCCURS 120 PIC X(01).
005000       05  W40-PC2       PIC X(04).
005010       05  FILLER        PIC X(82).
005020     03  W40-POST        PIC 9(04).
005030     03  W40-PPOST       PIC 9(04).
005040     03  W40-TEL         PIC X(14).
005050     03  W40-HTEL        PIC X(14).
005060     03  W40-ID.
005070         05  W40-IDNO    PIC X(14).
005080     03  W40-TAX REDEFINES W40-ID.
005090         05  W40-TAXNO   PIC X(14).
005100     03  W40-TITLE       PIC 9(02).
005110     03  W40-LANG        PIC X(01).
005120     03  W40-SMAN        PIC 9(02). 
           03  W40-LOC         PIC  X(03).
           03  W40-STATUS      PIC  9(03) COMP-3.
           03  W40-CUS-DET.
               05  W40-NM1     PIC  X(40).
               05  W40-AD1     PIC  X(30).
               05  W40-AD2     PIC  X(30).
               05  W40-AD3     PIC  X(30).
               05  W40-AD4     PIC  X(30).
               05  W40-PC      PIC  X(04).

       COPY W40.WS.

       COPY FUNCTION.WS.

005370 COPY W50.WS.

005390 01  W60-NME-ADD.
005400     03  W60-NAME.
005410         05  W60-NCHAR OCCURS 40 PIC X(01).
005420     03  W60-ADDRESS.
005430         05  W60-ACHAR OCCURS 60 PIC X(01).
005440     03  W60-ADDRESS1    PIC X(60).
005450     03  W60-ITM.
005460         05  W60-ICHAR OCCURS 14 PIC X(01).
003610 01  W65-ENTRY.
003620     03  W65-FOUR.
003630         05  W65-CHAR OCCURS 8 PIC X(01).
003640     03  W65-PREFIX REDEFINES W65-FOUR.
003650         05  W65-PREF    PIC X(02).
003660         05  FILLER      PIC X(06).
003670 01  W65-INDEX.
003680     03  W65-INPUT.
003690         05  W65-ICHAR OCCURS 8 PIC X(01).

005620 COPY W70DEBT.IWS.

005640 01  W75-KEYS.
005650     03  W75-S          PIC 9(02) COMP-5.
005660     03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
005670     03  W75-DNO        PIC X(06) OCCURS 18.
           03  W75-ITEMS OCCURS 18.
               05  W75-CODE   PIC X(14).
               05  W75-SELL   PIC 9(07)V99 COMP-3.
               05  W75-VSELL  PIC 9(07)V99 COMP-3.
               05  W75-CASH   PIC 9(07)V99 COMP-3.
               05  W75-VCASH  PIC 9(07)V99 COMP-3.
               05  W75-WSALE  PIC 9(07)V99 COMP-3.
               05  W75-VWSALE PIC 9(07)V99 COMP-3.
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
005950 01  W85-RECORD151.
005960     03  W85-CSHDTE      PIC  9(06).
005970     03  W85-ESC         PIC  X(01).
005980     03  W85-DBL         PIC  X(01).
005990     03  W85-CDBL        PIC  X(01).
006000     03  W85-SI          PIC  X(01).
006010     03  W85-CSI         PIC  X(01).
006020     03  W85-SO          PIC  X(01).
006030     03  W85-CSO         PIC  X(01).
006040     03  W85-6LPI        PIC  X(01).
006050     03  W85-8LPI        PIC  X(01).
006060     03  W85-NUL         PIC  X(01).
006070     03  W85-ADVANCE     PIC  9(02) COMP-5.
006080     03  W85-CPRN        PIC  9(01).
006090 01  W85-PASS.
006100     03  W85-SUPER       PIC  X(06)    OCCURS 9.
006110     03  W85-ENTRY       PIC  9(02)    COMP-3.
006120     03  W85-MARG        PIC S9(02)V99.
006130 01  W85-BATCHES.
006140         05  W85-BATCH   PIC 9(05).
006150 01  W90-TRAN.
006160     03  W90-DATE        PIC X(08).
006170     03  W90-DTE REDEFINES W90-DATE
006180                         PIC 99/99/99.
006190     03  W90-CODE        PIC 9(02).
004020     03  W90-RESULT      PIC  9(04)    COMP-5.
004020     03  W90-TRNKEY      PIC  9(04)    COMP-5.
004020     03  W90-TS          PIC  9(02)    COMP-5.
004020     03  W90-CATKEY      PIC  9(04)    COMP-5.
004020     03  W90-CS          PIC  9(02)    COMP-5.
006200     03  W90-ORG         PIC X(01).
006200     03  W90-EXP         PIC X(01).
006200     03  W90-ADV         PIC X(01).
006200     03  W90-PMETH       PIC X(01).
006210     03  W90-ORD         PIC X(12).
006220     03  W90-REF         PIC X(08).
006230     03  W90-REG         PIC X(09).
006230     03  W90-INSTRUCT    PIC X(09).
006240     03  W90-DB          PIC 9(07)V99  COMP-3.
006250     03  W90-CR          PIC 9(07)V99  COMP-3.
006260     03  W90-BDT         PIC S9(07)V99 COMP-3.
006270     03  W90-BCR         PIC S9(07)V99 COMP-3.
006280     03  W90-ICST        PIC S9(07)V99 COMP-3.
006290     03  W90-DESC        PIC X(12).
006300     03  W90-120         PIC S9(07)V99 COMP-3.
006310     03  W90-90          PIC S9(07)V99 COMP-3.
006320     03  W90-60          PIC S9(07)V99 COMP-3.
006330     03  W90-30          PIC S9(07)V99 COMP-3.
006340     03  W90-DUE         PIC S9(07)V99 COMP-3.
006350     03  W90-CASH        PIC S9(07)V99 COMP-3.
006360     03  W90-CHQ         PIC S9(07)V99 COMP-3.
006370     03  W90-CRD         PIC S9(07)V99 COMP-3.
006380     03  W90-CARD        PIC S9(07)V99 COMP-3.
006390     03  W90-CHANGE      PIC S9(07)V99 COMP-3.
006400     03  W90-PMT         PIC S9(07)V99 COMP-3.
006410     03  W90-CUR         PIC S9(07)V99 COMP-3.
006420     03  W90-BAL         PIC S9(07)V99 COMP-3.
006420     03  W90-VAT         PIC S9(07)V99 COMP-3.
006420     03  W90-ADVTAX      PIC S9(07)V99 COMP-3.
006430     03  W90-TAX         PIC X(14).
006440     03  W90-DVAL        PIC 9(07)V99.
006450     03  W90-SALES       PIC S9(07)V99 COMP-3.
006460     03  W90-TDISC       PIC S9(07)V99 COMP-3.
006470     03  W90-TTAX        PIC S9(07)V99 COMP-3.
006480     03  W90-TCOST       PIC S9(07)V99 COMP-3.
006490     03  W90-TSELL       PIC S9(07)V99 COMP-3.
006500     03  W90-DISC        PIC S9(07)V99 COMP-3.
006510     03  W90-CRED        PIC S9(07)V99 COMP-3.
006520     03  W90-CDISC       PIC S9(07)V99 COMP-3.
006530     03  W90-LDISC       PIC S9(07)V99 COMP-3.
006540     03  W90-TAXVAL      PIC S9(07)V99 COMP-3.
006550     03  W90-EXCDIS      PIC S9(07)V99 COMP-3.
006550     03  W90-EXCTOT      PIC S9(07)V99 COMP-3.
006550     03  W90-NONTAX      PIC S9(07)V99 COMP-3.
006560     03  W90-VALUE       PIC S9(07)V99 COMP-3.
006570     03  W90-STOTAL      PIC S9(07)V99 COMP-3.
006580     03  W90-TOTAL       PIC S9(07)V9(06) COMP-3.
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
007430     03  LINE 2  COLUMN 27 FOREGROUND-COLOR 15
007440                           VALUE "SALES/ORDER PROGRAM CONTROL".

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
009300     03  LINE  2 COLUMN 35 FOREGROUND-COLOR 15 
                                 VALUE "SALES ORDER".
009310     03  LINE  3 COLUMN  2 VALUE "Ac No".
009320     03          COLUMN 15 VALUE "Name".
009330     03          COLUMN 61 VALUE "Ord No             ".
009340	   03  LINE  4 COLUMN  2 VALUE "Assisted".
009350	   03  LINE  5 COLUMN  2 VALUE "Slip No".
009380     03          COLUMN 67 VALUE "Date".
009370	   03  LINE  6 COLUMN  2 REVERSE-VIDEO VALUE "Item Code      I".
009380	   03	       COLUMN 18 REVERSE-VIDEO VALUE "TM Description  ".
009390     03          COLUMN 33 REVERSE-VIDEO VALUE "                ".
009400     03          COLUMN 49 REVERSE-VIDEO VALUE "  Quantity ".
009410     03          COLUMN 60 REVERSE-VIDEO VALUE "Unit Price  ".
009420     03          COLUMN 72 REVERSE-VIDEO VALUE "Extended".
009160	   03  LINE  7 COLUMN  1 BACKGROUND-COLOR 0 VALUE "ฦ".
009160	   03	       COLUMN  2 VALUE "ออออออออออออออัอออัอออออออออออออ
      -        "ออออออออออออออออัออออออออัอออออออออัออออออออออ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "ต".
009160	   03  LINE 19 COLUMN  1 BACKGROUND-COLOR 0 VALUE "ฦ".
009170	   03	       COLUMN  2 VALUE "ออออออออออออออฯอออฯอออออออออออออ
      -        "ออออออออออออออออฯออออออออฯอออออออออฯออออออออออ".
	   03	       COLUMN 80 BACKGROUND-COLOR 0 VALUE "ต".
009470	   03  LINE 20 COLUMN  3 VALUE "Sub Total".
009480     03  LINE 21 COLUMN  3 VALUE "Discount".
009490     03  LINE 22 COLUMN  3 VALUE "T A X -VAT".
009500     03  LINE 23 COLUMN  3 VALUE "AMOUNT DUE".

       01  S30A.
           03  FOREGROUND-COLOR 11 PIC Z(08) USING W10-SORD AUTO.

009950 01  S36.
009960	   03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
				  PIC X(06) USING W80-ACNO AUTO.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
001230 PROCEDURE DIVISION
		 USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
010250 AA000-MAIN	       SECTION.
010260 AA000-INIT.
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
       AA50O.
             MOVE "O"            TO WS-LOOK.

010500 AA50.
011050	     PERFORM SAVE-SCREEN.
011080       MOVE 4              TO W44-FUNCTION.
011090	     PERFORM SCREEN-CONTENTS.
	     PERFORM SORDER-LOOKUP.
011320	     PERFORM RESTORE-SCREEN.
010910	     MOVE "S" TO WS-LOOK.
010920 AA99.
010930       EXIT.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY OPTION.CRT.

010950 COPY LOCKED.RC2.

010970 COPY CLEAR.CRT.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S 
      *
011010 AA400              SECTION.
011020 AA401.
011030	     DISPLAY "F1" AT 2501
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011040		     " Order no./ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
011050		     "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
011060		     " Account no./ "
		      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
		     "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
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

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
011670 AY000           SECTION.
012470 AY60.
      *
      *    ****    Read the fifth record in the SHARED file and
      * 	   lock it. Log details to the RECOVERY file.
      *
012690	     MOVE 1		 TO WS-COUNT.
012690	     MOVE 5		 TO WS-SHARED.
012700	     PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
012720	     GO TO AY999.
012950 AY70.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      * 	   SHARED record 5.
      *
012990       UNLOCK NETWORK.
013070       COMMIT.
013080 AY999.
013090       EXIT.
      /
013100 AZ000-END               SECTION.
013110 AZ000-EOJ.
013120	     CLOSE SALE.
013250 AZ100.
013260       EXIT PROGRAM.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
013270 CA000         SECTION.
013280 CA00.
013290       MOVE 14             TO W50-LENGTH.
013300 CA05.
013310       MOVE W10-ITEM       TO W50-DATA.
013320     IF W50-1 = SPACE
013330         MOVE W50-13       TO W10-ITEM
013340         GO TO CA05.      
	     CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
013400	     MOVE W50-ITEM	 TO W10-ITEM.
013410 CA06.
013420       MOVE W50-ITEM       TO STK-CODE.
013430 CA10.
013440       PERFORM READ-STOCK THRU READ-STOCK-EXIT.
013450     IF WS-F-ERROR = 0
013460         PERFORM CA500.
013470 CA99.
013480       EXIT.
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
      *    ****    Calculate the inclusive prices
      *
014470 CA500         SECTION.
014480 CA505.
      *    ****    A V E R A G E   C O S T
	   IF STK-QUANT < 0.001
	       MOVE STK-COST	 TO STK-AVGE.
014490	   IF STK-WSALE = ZERO
014500         IF STK-CASH NOT = ZERO
014510             MOVE STK-CASH TO STK-WSALE
014520         ELSE
014530             MOVE STK-SELL TO STK-WSALE.
014540     IF STK-CASH = ZERO
014550         MOVE STK-SELL     TO STK-CASH.
014560       MOVE STK-CASH       TO W10-VCASH
014570       MOVE STK-SELL       TO W10-VSELL
014580       MOVE STK-WSALE      TO W10-VWSALE.
014590       MOVE ZERO           TO W05-RATE.
014670     IF STK-TAX > 0
014680         MOVE W05-VAT (STK-TAX)
014690                           TO W05-RATE.
014700       COMPUTE W10-VCASH ROUNDED 
014710             = W10-VCASH + (W10-VCASH * W05-RTE).
014720       COMPUTE W10-VWSALE ROUNDED 
014730             = W10-VWSALE + (W10-VWSALE * W05-RTE).
014740       COMPUTE W10-VSELL ROUNDED 
014750             = W10-VSELL + (W10-VSELL * W05-RTE).
014760 CA599.
014770       EXIT.
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
013270 GET-SORDER           SECTION.

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
               MOVE SOR-OREF     TO W90-REG
               MOVE SOR-NAME     TO W40-NAME
               MOVE SOR-ORD      TO W90-ORD
               MOVE SOR-SMAN     TO W10-SMAN
               MOVE SOR-EXP      TO W90-EXP
               MOVE SOR-PRC      TO WS-PRC
               MOVE SOR-ADV      TO W90-ADV
               MOVE SOR-CONTACT  TO W40-CONTACT
               MOVE SOR-TEL      TO W40-TEL
               MOVE SOR-BIN      TO W40-LOC
           ELSE
      *                      I T E M   D E T A I L S
           IF SOR-TYPE = 1
               PERFORM GET-CHECK-STOCK-FILE
               IF WS-F-ERROR = 0
                   INITIALIZE SAL-RECORD1
                   MOVE SOR-ITM  TO SAL-ITEM
                   MOVE SOR-DESC TO SAL-DESC
                   MOVE SOR-QNT  TO SAL-QNT
                   MOVE SOR-COST TO SAL-COST
                   MOVE SOR-SELL TO SAL-SELL
                   MOVE SOR-VAT  TO SAL-VAT
                   MOVE SOR-VAL  TO SAL-VAL
                   MOVE SOR-INCTAX
                                 TO SAL-INCTAX
                   MOVE SOR-NONTAX
                                 TO SAL-NONTAX
                   MOVE SOR-TAXVAL
                                 TO SAL-TAXVAL
                   MOVE SOR-DSC  TO SAL-DSC
                   MOVE SOR-DISC TO SAL-DISC
                   MOVE SOR-SERNO
                                 TO SAL-SERNO
                   MOVE SOR-SQN  TO SAL-SQN
                   PERFORM GET-SORD-WRITE-SALE
               ELSE
                   PERFORM GET-CHANGE-TO-REMKS
           ELSE
      *                      P A R T S   D E T A I L S
           IF SOR-TYPE = 2
               INITIALIZE SAL-RECORD1
               MOVE SOR-ITM      TO SAL-ITEM
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
	       MOVE SOR-TEXT	 TO W32-REMARKS (WS-S7)
	       ADD 1		 TO WS-S7
	   ELSE
      *                      T R A I L E R
               MOVE SOR-SUBTOT   TO W90-DB
               MULTIPLY -1 BY SOR-DSCNT    
                                 GIVING W90-TDISC
               MOVE W90-TDISC    TO W90-LDISC
               MOVE SOR-NON-TXBL TO W90-NONTAX
               MOVE SOR-TAXABLE  TO W90-TAXVAL
               MOVE SOR-TAX      TO W90-TTAX
               MOVE SOR-TOT      TO W90-TOTAL
               MOVE SOR-BAL      TO W90-BAL
               MOVE SOR-ADVTAX   TO W90-ADVTAX
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
             MOVE SOR-ITM        TO STK-CODE.
013560       PERFORM READ-STOCK THRU READ-STOCK-EXIT.

       GET-CHANGE-TO-REMKS.
             INITIALIZE SAL-RECORD1.
             STRING SOR-ITM DELIMITED BY SIZE
                    SOR-DESC DELIMITED BY SIZE
                                 INTO SAL-DESC.
             MOVE SOR-SQN        TO SAL-SQN W14-SQN (W14-SUB).
             ADD 1               TO W14-SUB.
             PERFORM GET-SORD-WRITE-SALE.
             GO TO GET-SORD-NEXT.

       GET-ADJUST-TOTALS.
             SUBTRACT 1 FROM W14-SUB
                                 GIVING WS-SUB.
             MOVE 1              TO W14-SUB.

       GET-ADJUST-TOTALS-LOOP.
             MOVE W14-SQN (W14-SUB)
                                 TO SOR-SQN.
             PERFORM READ-SORDER-LOCK THRU READ-SORDER-EXIT.
025000       SUBTRACT SOR-VAL    FROM W90-DB.
025010       SUBTRACT SOR-DISC   FROM W90-LDISC.
025020       SUBTRACT SOR-NONTAX FROM W90-NONTAX.
025030       SUBTRACT SOR-TAXVAL FROM W90-TAXVAL.
025050       COMPUTE W90-BAL = W90-BAL - (SOR-COST * SOR-QNT).
             MOVE 3              TO SOR-TYPE.
             STRING SOR-ITM DELIMITED BY SIZE
                    SOR-DESC DELIMITED BY SIZE
                                 INTO SOR-DESC.
             MOVE SPACES         TO SOR-ITM SOR-SERNO.
             MOVE ZERO           TO SOR-QNT SOR-COST SOR-INCTAX
                                    SOR-VAT SOR-SELL SOR-NONTAX
                                    SOR-VAL SOR-DISC SOR-TAXVAL
                                    SOR-DSC.
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
       BB030           SECTION.
       BB30.
             MOVE ZERO           TO WS-ERROR.
             MOVE 1              TO WS-S1.
             MOVE W10-QUANT      TO WS-S2.
011050       PERFORM SAVE-SCREEN.
	     ADD 1 LIN		 GIVING CLIN.
       BB40.
             MOVE 51             TO CCOL.
             DISPLAY "Serial No:" AT CPOS WITH FOREGROUND-COLOR 14
                                               BACKGROUND-COLOR 7
                     "            (   )" WITH FOREGROUND-COLOR 15
                                              BACKGROUND-COLOR 7.
       BB41.
             MOVE WS-S1          TO W100-SER.
             MOVE 74             TO CCOL.
             DISPLAY W100-SER AT CPOS WITH FOREGROUND-COLOR 15
                                           BACKGROUND-COLOR 7.
       BB42.
             MOVE 62             TO CCOL.
             ACCEPT W43-SERNO (WS-S1) AT CPOS 
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
           IF W43-SERNO (WS-S1) = SPACES
               GO TO BB42.
             MOVE 10             TO WS-LENGTH.
	     CALL "CBL_TOUPPER" USING W43-SERNO (WS-S1)
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
	     MOVE 1		 TO WS-LENGTH.
           IF W43-SERNO (WS-S1) = "NONE"
               MOVE SPACES       TO W43-SERNO (1)
               MOVE 1            TO WS-S2
               GO TO BB45.
             MOVE STK-CODE       TO GUA-CODE.
             MOVE W43-SERNO (WS-S1)
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
	     PERFORM OPT-MESSAGE.
	   IF NOT (WS-OPTION = "C" OR "R")
               GO TO BB48.
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
018510 BC00.
      *
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *
018520       MOVE 5              TO WS-PARKEY.
018530       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
018540       MOVE PAR-PW         TO W85-PASS. 
018570 BC05.
      *
      *    ****    S A L E S   O R D E R   S C R E E N
      *
018580       DISPLAY CLR-SCREEN.
018590       DISPLAY S30.
018620       MOVE 8              TO LIN.
018630       PERFORM BC115 UNTIL LIN > 18.
018640       MOVE W12-TODAY      TO W90-DTE.
018650       DISPLAY W90-DTE AT 0572 WITH FOREGROUND-COLOR 11.
018660       MOVE ZERO           TO W90-BDT W90-NONTAX W90-LDISC 
018670                              W90-BAL W90-TAXVAL W90-SALES
                                    W90-CUR W90-EXCTOT W90-ADVTAX.
018680       MOVE 1              TO WS-FIRST WS-LAST.
             MOVE ZERO           TO W10-SORD.
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
                     MOVE "O"    TO WS-ETYPE
                 WHEN F2-KEY
                     MOVE "A"    TO WS-ETYPE
                 WHEN F3-KEY
                     MOVE "T"    TO WS-ETYPE
                 WHEN OTHER
                     CALL X"E5"
                     GO TO BC10
               END-EVALUATE
020770	       PERFORM AA50O THRU AA50
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
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
017670			  BACKGROUND-COLOR 5.
019970	     DISPLAY W10-SMAN AT 0411
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
017710			  BACKGROUND-COLOR 5.
020010	   IF W10-SMAN NOT = ZERO
020030         ADD 100 W10-SMAN    GIVING WS-PARKEY
020040         PERFORM READ-PARAM THRU READ-PARAM-EXIT
020100	       DISPLAY PAR-SALESMAN AT 0420
		       WITH FOREGROUND-COLOR 3 HIGHLIGHT
			    BACKGROUND-COLOR 5.
	     DISPLAY W40-TEL AT 0466
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
			  BACKGROUND-COLOR 5.
	     DISPLAY W40-CONTACT AT 0528
		     WITH FOREGROUND-COLOR 7 HIGHLIGHT
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
           IF W90-ADV = "Y"
               COMPUTE W90-ADVTAX ROUNDED = 
                      (W90-TOTAL * WS-ADV-RATE / 100.00000).
022690     IF W90-TAX = SPACES
022700         GO TO BC20.
             MOVE W05-VAT-R1     TO W05-RATE.
022710     IF W90-TAXVAL NOT = ZERO
022720         COMPUTE W25-TOTAL ROUNDED = 
                     ((W90-TAXVAL + W90-ADVTAX) * W05-RTE)
022740	       MOVE W25-TOTAL	 TO W100-BAL
022750         DISPLAY W100-BAL AT 2215 WITH FOREGROUND-COLOR 11
022760         ADD W25-TOTAL     TO W90-TOTAL.
022770 BC20.
      *     IF W90-ADV = "Y"
      * 	MOVE W90-ADVTAX   TO W100-BAL
      * 	DISPLAY W100-BAL AT 2241 WITH FOREGROUND-COLOR 11
      * 	ADD W90-ADVTAX	  TO W90-TOTAL.
022780       MOVE W90-TOTAL      TO W100-BAL.
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
029180       MOVE WS-FIRST TO WS-SALKEY.
029190 BC35.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
029200     IF WS-SALKEY > WS-LAST
029210         GO TO BC45.
029220       PERFORM READ-SALE THRU READ-SALE-EXIT.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
029240	   IF SAL-ITEM = SPACES
023480	       DISPLAY (LIN, 21) SAL-SDSC
			WITH FOREGROUND-COLOR 7 HIGHLIGHT
			     BACKGROUND-COLOR 0
029260	       GO TO BC40.
023500	     DISPLAY (LIN, 2) SAL-ITEM
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 0.
023460	     DISPLAY ( , 17) "   "
		      WITH FOREGROUND-COLOR 7 HIGHLIGHT
			   BACKGROUND-COLOR 0.
023510	     DISPLAY ( , 21) SAL-SDSC
		      WITH FOREGROUND-COLOR 3 HIGHLIGHT
			   BACKGROUND-COLOR 0.
029290	     MOVE SAL-QNT	 TO W10-QUANT.
029300       DISPLAY S26-QNT AT LINE LIN.
029310       MOVE SAL-SELL       TO W10-SELL.
029320       DISPLAY S26-PRC AT LINE LIN.
029330       MOVE SAL-VAL        TO W100-EXT.
023570	     DISPLAY ( , 70) W100-EXT
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
             MOVE W43-SERNO (WS-S1)
                                 TO SOR-SNO (WS-S3).
             ADD 1               TO WS-S1.
           IF WS-S1 > WS-S2
               PERFORM BC38
               GO TO BC40.
           IF WS-S3 < 3
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
029390 BC50.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
029420	     MOVE "Print 'I'nvoice - 'E'xit"
				 TO WS-ERR-MES.
	     MOVE SPACE 	 TO WS-OPTION.
029660 BC55.
	     PERFORM OPT-MESSAGE.
029730	   IF NOT (WS-OPTION = "E" OR "P" OR "I")
029740	       GO TO BC50.
029750     IF WS-OPTION = "E"
               IF WS-MULTI = "Y"
                   MOVE "D"      TO WS-MULTI
                   PERFORM GET-SORDER
               END-IF
029760         GO TO BC05.
029920 BC60.
      *
      *    ****    C L E A R   S C R E E N
      *
029930       MOVE 8              TO LIN.
029940       PERFORM BC115 UNTIL LIN > 18.
029950       MOVE 8              TO LIN.
029960       MOVE 1              TO WS-LINES.
      *
      *    ****    P R I N T   I N V O I C E
      *
       BC65.
             DISPLAY CLR-SCREEN.
             DISPLAY "PROCESS INVOICE" AT 0236 WITH FOREGROUND-COLOR 15
             DISPLAY "Printing invoice" AT 1212
                      WITH FOREGROUND-COLOR 14
	     MOVE W90-REF	 TO LS-SLS-ORDNO.
	     CLOSE SALE.
004200	     CALL PRG-INVOICE USING LS-PARID LS-USER-ID LS0-PROGRAMS
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
028940	   IF NOT (WS-OPTION = "E")
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
028770	     DISPLAY (LIN, 1) "ณ" WITH BACKGROUND-COLOR 0
				       FOREGROUND-COLOR 3
			       WS-INV78 WITH FOREGROUND-COLOR 3
			      "ณ" WITH BACKGROUND-COLOR 0
				       FOREGROUND-COLOR 3.
023540	     ADD 1		 TO LIN.
031080 BC599.
031090       EXIT.
      /
049180 ZA000-INIT	       SECTION 9.
049190 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
	     MOVE LS-PARID	 TO WS-PARID.
	     MOVE LS-L-OR-N	 TO W02-L-OR-N.
	     MOVE WS-SYS-ID	 TO W02-SYSID.
	     MOVE LS-TODAY-DDMMYY
				 TO TODAY-DDMMYY.
	     MOVE LS-USUB	 TO WS-USUB.
049200	     DISPLAY CLR-SCREEN.
      *
      *    ****    S E T   U P   T H E   F U N C T I O N   K E Y S
      *
             MOVE 1              TO USER-ACTION
                                    USER-SETTING.
      *
      *    ESC, F1 to F10 keys
      *
             MOVE ZERO           TO USER-NUMBER.
             MOVE 11             TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PAGE-UP AND PAGE-DOWN KEYS
      *
             MOVE 53             TO USER-NUMBER.
             MOVE 2              TO USER-KEYS.
             CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
             MOVE 64             TO MOUSE-FUNC.
             MOVE 1              TO MOUSE-PARAM.
             CALL X"AF" USING MOUSE-FUNC
                              MOUSE-PARAM.
           IF MOUSE-FUNC NOT = 255
               MOVE "Y"          TO MOUSE-ATTACHED
               MOVE 66           TO MOUSE-FUNC
               MOVE 0            TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC
                                MOUSE-PARAM
           ELSE
               GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      *            A S   F U N C T I O N   K E Y
      *
             MOVE 3              TO USER-ACTION.
             MOVE 27             TO USER-NUMBER.
             MOVE 2              TO USER-KEYS.
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
	     MOVE "DEBTOR"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-DEBTOR.
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
	     MOVE "SHARED"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SHARED.
	     MOVE "SORDER"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SORDER.
	     MOVE "SPARTS"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-SPARTS.
	     MOVE "STOCKF"	 TO AFID-KEY.
	     PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
	     MOVE AFID-PATH	 TO W02-STOCKF.
049320	     MOVE 3		 TO WS-PARKEY.
049330       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
050510	     MOVE "V"		 TO WS-VAT-GST.
050520       MOVE 5              TO WS-PARKEY.
050530       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
050540*      MOVE PAR-ADV-RATE	 TO WS-ADV-RATE.
050550       MOVE PAR-VAT-RATES  TO W05-VAT-RATES.
050560       MOVE PAR-DISCOUNT   TO W15-DISCOUNT.
050580       MOVE 1              TO WS-PARKEY.
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
040050	     MOVE 1 TO WS-S1.
040060	     MOVE SPACES	 TO WS-MD-LINE.
040090 ZA60.
040130	     MOVE WS-G1 	 TO WS-TCA (WS-S1)   WS-BCA (WS-S1).
040150	   IF WS-S1 < 80
040160         ADD 1 TO WS-S1
040170         GO TO ZA60.
040220	     MOVE WS-G14	 TO WS-TCA   (1)  WS-BCA   (1).
040260	     MOVE WS-G13	 TO WS-TCA   (80) WS-BCA   (80).
040300	     MOVE WS-G2 	 TO WS-TCA   (10) WS-TCA   (17)
040360                              WS-TCA   (47) WS-TCA   (56)
040370				    WS-TCA   (59) WS-TCA   (68).
040400	     MOVE WS-G3 	 TO WS-MCA   (1)  WS-MCA   (80)
040470                              WS-MCA   (10) WS-MCA   (17)
040480                              WS-MCA   (47) WS-MCA   (56)
040490				    WS-MCA   (59) WS-MCA   (68).
040550	     MOVE WS-G4 	 TO WS-BCA   (10) WS-BCA   (17)
040610                              WS-BCA   (47) WS-BCA   (56)
040620				    WS-BCA   (59) WS-BCA   (68).
040650	     MOVE LS-COMPANY	 TO WS-TOP-COMP.
	   IF LS-USER = LS-SYS-ID
	       MOVE "Supervisorฤ"  TO WS-WRKHD
	   ELSE
	       MOVE "Workstation"  TO WS-WRKHD
	       MOVE LS-USER	   TO WS-WRKST.
051540 ZA200.
051550       DISPLAY "Files locked - Try later" AT 2312
051560                WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
051570	     DISPLAY "Press any key" AT 2512
051580                WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
051590       ACCEPT WS-OPTION AT 2526
                    WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
051600       EXIT PROGRAM.
051610 ZA999.
051620       EXIT.

051640 I-O-ERRORS      SECTION	9.
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
051840     IF WS-F-ERROR = 2
051850         MOVE W02-NETWORK TO WS-FILE
051860         MOVE WS-NETKEY TO WS-KEY
051910     ELSE
051920     IF WS-F-ERROR = 6
051930         MOVE W02-DEBTOR TO WS-FILE
051940         MOVE WS-DEBKEY TO WS-KEY
           ELSE
           IF WS-F-ERROR = 9
               MOVE W02-GARTEE   TO WS-FILE
               MOVE ZERO         TO WS-KEY
052070     ELSE
052080     IF WS-F-ERROR = 15
052090         MOVE WS-PARID     TO WS-FILE
052100         MOVE WS-PARKEY    TO WS-KEY
052150     ELSE
052160     IF WS-F-ERROR = 19
052170         MOVE W02-SALE TO WS-FILE
052180         MOVE WS-SALKEY TO WS-KEY
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
052550         DISPLAY "Reverse backup or contact program Support"
052560                  AT 1612.
052570         DISPLAY "Please make a note of these details" AT 1812.
052580       STOP RUN.
