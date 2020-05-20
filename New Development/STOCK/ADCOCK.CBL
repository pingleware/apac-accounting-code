      ******************************************************************
      *                                                                *
      *       **     ******* ******   ******    ******   **   **   *
      *      ****    **    **  **    **  **    **  **  **  **  **    *
      *     ** **   **    **  **  **    **  **      ** **     *
      *    **  **  **    **  **  **    **  **      ****      *
      *    ********  **    **  **  **    **  **      ** **     *
      *    **  **  **    **  **    **  **    **  **  **  **  **    *
      *    **  **  ******* ******   ******    ******   **   **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *      S T O C K   U P D A T E   P R I C E S   P R O G R A M     *
      *                                                                *
      *    VERSION 6.4 - August 1993.           *
      *                                                                *
      ******************************************************************
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     ADCOCK.
000030 AUTHOR.         J.W.LEMMON (APAC).
000040 DATE-WRITTEN.   JANUARY 1983.
000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
                       CURSOR IS CSTART
000080         CONSOLE IS CRT
         CRT STATUS IS KEY-STATUS.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

000120 COPY CONTROL.SL.

000120 COPY DEPART.SL.

000120 COPY PARAM.SL.

000140 COPY STOCK.SL.

           SELECT PRICES  ASSIGN DISK
000190                    ACCESS SEQUENTIAL
000200                    ORGANIZATION LINE SEQUENTIAL.

000250 DATA DIVISION.
000260 FILE SECTION.

000120 COPY CONTROL.FD.

000120 COPY DEPART.FD.

000280 COPY PARAM.FD.

000300 COPY STOCK.FD.

000290 FD  PRICES    LABEL RECORD STANDARD
000300               VALUE OF FILE-ID W02-PRICES.
000310 01  PRC-RECORD1.
000320    03  PRC-CODE        PIC  X(07).
000330    03  PRC-DESC        PIC  X(32).
000360    03  PRC-COST        PIC  9(04)V999.
    03  PRC-SELL        PIC  9(04)V999.
           03  PRC-VENDOR      PIC  9(06).
    03  PRC-LEDG        PIC  9(04).
    03  PRC-MARKUP      PIC  9(02).99.
    03  PRC-TAX        PIC  X(01).
    03  PRC-DISCOUNT    PIC  9(02).99.
      /
001690 WORKING-STORAGE SECTION.
001710 77  WS-SUB           PIC 9(04)     COMP-5.
001710 77  WS-S1            PIC 9(04)     COMP-5.
001720 77  WS-S2            PIC 9(04)     COMP-5.
001730 77  WS-S3            PIC 9(04)     COMP-5.
001740 77  WS-S4            PIC 9(04)     COMP-5.
001750 77  WS-S5            PIC 9(04)     COMP-5.
001760 77  WS-S6            PIC 9(04)     COMP-5.
001770 77  WS-S7            PIC 9(04)     COMP-5.
001780 77  WS-S8            PIC 9(04)     COMP-5.
001790 77  WS-IXS           PIC 9(04)     COMP-5.
001800 77  WS-IXS1          PIC 9(04)     COMP-5.
001810 77  WS-IXS2          PIC 9(04)     COMP-5.
001820 77  WS-IXS3          PIC 9(04)     COMP-5.
001830 77  WS-IXS4          PIC 9(04)     COMP-5.
001840 77  WS-DEPKEY        PIC 9(04)     COMP-5.
001860 77  WS-PARKEY        PIC 9(04)     COMP-5.
001880 77  WS-NETKEY        PIC 9(04)     COMP-5.
       77  WS-PRCKEY        PIC 9(04)     COMP-5.
       77  WS-RECKEY        PIC 9(04)     COMP-5.
001920 77  WS-KEYSTR        PIC 9(04)     COMP-5.
001960 77  WS-KEY1          PIC 9(04)     COMP-5.
001970 77  WS-KSTORE        PIC 9(04)     COMP-5.
001980 77  WS-ITM           PIC X(12).
001990 77  WS-ITM1          PIC X(12).
002020 77  WS-OPTION        PIC X(01).
002020 77  WS-SKIP          PIC X(01).
002070 77  WS-ERROR         PIC 9(01).
002080 77  WS-IND           PIC 9(01)     VALUE 0.
002100 77  WS-INDP          PIC 9(01)     VALUE 0.
002110 77  WS-INDS          PIC 9(01)     VALUE 0.
002120 77  WS-AMEND         PIC 9(01)     VALUE 0.
002150 77  WS-LEDG          PIC 9(03).
002160 77  WS-LEDG1         PIC 9(03).
002170 77  WS-SELECT        PIC X(13)     VALUE "Select Option".
002180 77  WS-CHAR1         PIC X(01).
002190 77  WS-CHAR2         PIC X(01).
002200 77  WS-COMP          PIC 9(01).
002220 77  WS-TYPE          PIC X(01).
002240 77  WS-INDEX-STORE   PIC X(12).
002250 77  WS-INDEX-NAME    PIC X(12).
       77  WS-PRINT         PIC 9(01).
002280 77  WS-PASS          PIC X(08).
002290 77  WS-OK            PIC X(01) VALUE "N".
002300 77  WS-CHK           PIC X(33)      VALUE
002310                    "ENTER if correct - N if incorrect".
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
002450     03  WS-MID-LNE.
002460         05  WS-MCHR PIC X(01) OCCURS 80.
002450     03  WS-MID-LNE2.
002460         05  WS-MCH  PIC X(01) OCCURS 80.
002470     03  WS-BOT-LNE.
002480         05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
002470     03  WS-BOT-LNE2.
002480         05  WS-BCH  PIC X(01) OCCURS 80.

002330 COPY WS.WS.

       COPY CHAIN.WS.

002350 01  W02-FID.

       COPY CONTROL.ID.

000120 COPY DEPART.ID.

       COPY STOCK.ID.

002440     03  W02-PRICES.
        05  W02-DSKP    PIC  X(01) VALUE "A".
002460        05  FILLER      PIC  X(08) VALUE ":STK.".
        05  W02-EXTP    PIC  X(02) VALUE "01".
        05  FILLER      PIC  X(04) VALUE ".POS".

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
002770     03  W10-DESC.
               05  W10-SDSC    PIC X(30).
               05  FILLER      PIC X(10).
           03  W10-DEBT REDEFINES W10-DESC.
               05  W10-DENT   PIC X(14).
               05  FILLER     PIC X(26).
002770     03  W10-DEPT       PIC X(30).
002780     03  W10-MARKUP     PIC S9(03)V9999 COMP-3.
002790     03  W10-CMARKUP    PIC S9(03)V9999 COMP-3.
002790     03  W10-WMARKUP    PIC S9(03)V9999 COMP-3.
002800     03  W10-IND        PIC X(01).
002810     03  W10-TAX        PIC 9(01).
002820     03  W10-QUANT      PIC S9(07)     COMP-3.
002820     03  W10-QNT        PIC S9(07)     COMP-3.
002830     03  W10-REORD      PIC S9(07)V99  COMP-3.
002840     03  W10-COST       PIC S9(06)V999 COMP-3.
003580     03  W10-GUAR       PIC  9(02)     COMP-5.
002850     03  W10-SELL       PIC S9(07)V99  COMP-3.
002860     03  W10-CASH       PIC S9(07)V99  COMP-3.
002860     03  W10-WSALE      PIC S9(07)V99  COMP-3.
002850     03  W10-VSELL      PIC S9(07)V99  COMP-3.
002860     03  W10-VCASH      PIC S9(07)V99  COMP-3.
002860     03  W10-VWSALE     PIC S9(07)V99  COMP-3.
002870     03  W10-CCOST      PIC S9(07)V9(6) COMP-3.
002880     03  W10-CSELL      PIC S9(07)V9(6) COMP-3.
002890     03  W10-LEDG       PIC 9(04)      COMP-5.
002900     03  W10-ORDER      PIC S9(07)V99  COMP-3.
002910     03  W10-DATE.
               05  W10-EDTE   PIC 99/99/99.
002910     03  W10-ODTE       PIC 9(06)      COMP-3.
002910     03  W10-PURCH      PIC 9(06)      COMP-3.
002920     03  W10-AVRG       PIC S9(07)V99  COMP-3.
002930     03  W10-ACTIVE     PIC X(08).
002940     03  W10-ADATE REDEFINES W10-ACTIVE PIC Z9/99/99.
002950     03  W10-DISC1      PIC 99V99.
002960     03  W10-DISC2      PIC 99V99.
002970     03  W10-ADJUST     PIC S9(03)V99  COMP-3.
002980     03  W10-ADJ REDEFINES W10-ADJUST
002990                        PIC S9V9999    COMP-3.
003000     03  W10-MTH1       PIC S9(09)     COMP-3.
003010     03  W10-MTH2       PIC S9(09)     COMP-3.
003020     03  W10-MTH3       PIC S9(09)     COMP-3.
003030     03  W10-MAVG       PIC S9(09)     COMP-3.
003040     03  W10-MTDC       PIC S9(09)V99  COMP-3.
003050     03  W10-YTDC       PIC S9(09)V99  COMP-3.
003060     03  W10-LTIME      PIC S9(01)V99  COMP-3.
003070     03  W10-KEY        PIC 9(04)      COMP-5.
003080     03  W10-MSELL      PIC S9(07)V9999 COMP-3.
003080     03  W10-MCOST      PIC S9(07)V9999 COMP-3.
002820     03  W10-MQUANT     PIC S9(07)     COMP-3.
003090     03  W10-SDESC      PIC X(30).
003100     03  W10-MQNT       PIC 9(05).
003110     03  W10-COMMENT    PIC X(40).
003170 01  W12-DATE.
003180     03  W12-TODAY      PIC 9(06).
003190     03  W12-DATE-DMY REDEFINES W12-TODAY.
003200         05  W12-DAY    PIC 9(02).
003210         05  W12-MONTH  PIC 9(02).
003220         05  W12-YEAR   PIC 9(02).
003230     03  W12-T-YMD      PIC 9(06).
003240     03  W12-DATE-YMD REDEFINES W12-T-YMD.
003250         05  W12-YY     PIC 9(02).
003260         05  W12-MM     PIC 9(02).
003270         05  W12-DD     PIC 9(02).
003280 01  W15-RECORD1.
003290     03  W15-CODE       PIC X(12).
003300     03  W15-BIN        PIC X(03).
003310     03  W15-DESC       PIC X(30).
003320     03  W15-QUANT      PIC S9(05)V99 COMP-3.
003330     03  W15-LEVEL      PIC S9(05)V99 COMP-3.
003340     03  W15-COST       PIC S9(05)V99 COMP-3.
003350     03  W15-SELL       PIC S9(05)V99 COMP-3.
003360     03  W15-CASH       PIC S9(05)V99 COMP-3.
003370     03  W15-AVGE       PIC S9(05)V99 COMP-3.
003380     03  W15-ORD        PIC S9(05)V99 COMP-3.
003390     03  W15-LTIME      PIC S9(01)V99 COMP-3.
003400     03  W15-MTD        PIC S9(05)V99 COMP-3.
003410     03  W15-MTDC       PIC S9(07)V99 COMP-3.
003420     03  W15-MTDV       PIC S9(07)V99 COMP-3.
003430     03  W15-YTD        PIC S9(05)V99 COMP-3.
003440     03  W15-YTDC       PIC S9(07)V99 COMP-3.
003450     03  W15-YTDV       PIC S9(07)V99 COMP-3.
003460     03  W15-MTH1       PIC S9(05)V99 COMP-3.
003470     03  W15-MTH2       PIC S9(05)V99 COMP-3.
003480     03  W15-MTH3       PIC S9(05)V99 COMP-3.
003490     03  W15-MAVG       PIC S9(05)V99 COMP-3.
003500     03  W15-MARKUP     PIC S9(03)V99 COMP-3.
003510     03  W15-CMARKUP    PIC S9(03)V99 COMP-3.
003520     03  W15-ACT        PIC 9(06)     COMP-3.
003530     03  W15-PUR        PIC 9(06)     COMP-3.
003530     03  W15-DTE        PIC 9(06)     COMP-3.
003540     03  W15-LEDG       PIC 9(04)     COMP-5.
003550     03  W15-DISC       PIC X(01).
003560     03  W15-IND        PIC 9(01).
003570     03  W15-LNK        PIC 9(04)     COMP-5.
003580     03  W15-OLNK       PIC 9(04)     COMP-5.
003590     03  W15-GST        PIC X(01).
003600     03  W15-XREF       PIC X(12).
003600     03  W15-BCODE      PIC X(16).
003610 01  W20-DATE.
003620     03  W20-YY.
003630         05  W20-YEAR   PIC 9(02).
003640     03  W20-MM.
003650         05  W20-MONTH  PIC 9(02).
003660     03  W20-DD.
003670         05  W20-DAY    PIC 9(02).
003680 01  W20-DTE1 REDEFINES W20-DATE.
003690     03  W20-DTE        PIC 9(06).
003700 01  W25-CALCS.
003710     03  W25-RESULT     PIC 9(02)V99.
003720     03  W25-RESULT1 REDEFINES W25-RESULT.
003730         05  W25-WHOLE  PIC 9(02).
003740         05  W25-DECIMAL PIC 9(02).
003750     03  W25-SCOST      PIC S9(07)V99 COMP-3.
003760     03  W25-PCOST      PIC S9(07)V99 COMP-3.
003770     03  W25-TSELL      PIC S9(07)V99 COMP-3.
003770     03  W25-TOTAL      PIC S9(07)V99 COMP-3.
003780     03  W25-VALUE      PIC S9(07)V99 COMP-3.
003780     03  W25-ADJUST     PIC S9(07)V99 COMP-3.

       COPY W40.WS

003890 01  W45-TRAN.
003900     03  W45-CODE        PIC 9(02).
003910     03  W45-ENG         PIC X(12).
003920     03  W45-AFR         PIC X(12).
003930     03  W45-ACT         PIC X(01).

       COPY FUNCTION.WS.

003800 COPY W50.WS.


003890 COPY W70.WS.

003910 COPY W70STOCK.WS.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-SKEY       PIC 9(04) COMP-5
                              OCCURS 18.
003930 01  W80-EDIT.
003940     03  W80-CNO        PIC 9(04).
003950     03  W80-V1.
003960         05  W80-S7V2   PIC Z(06)9.99-.
003970     03  W80-V2.
003980         05  W80-S5V2   PIC Z(04)9.99-.
003990     03  W80-DTE.
004000         05  W80-DATE   PIC Z9/99/99.
004010     03  W80-QNT.
004020         05  W80-5      PIC Z(04)9.99.
004030     03  W80-QNT1 REDEFINES W80-QNT.
004040         05  W80-3V2    PIC Z(02)9.9999.
004050     03  W80-NO.
004060         05  W80-5N     PIC 9(05).
004070         05  W80-REC REDEFINES W80-5N
004080                        PIC ZZZZ9.
004090     03  W80-DEL.
004100         05  W80-2N      PIC 9(02).
004110 01  W90-BAL             PIC S9(07)V99 COMP-3.
004120 01  W95-COMP            PIC X(40).
005120 01  W100-EDIT.
003970     03  W100-V2.
003980         05  W100-S6V2   PIC Z(05)9.99-.
      /
004130 SCREEN SECTION.

       COPY BLANK.CRT.

       01  ERROR-LINE.
    03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     HIGHLIGHT
     PIC  X(48) FROM WS-ERR-MES.
      /
007730 PROCEDURE DIVISION.
007740 AA000-MAIN              SECTION.
007750 AA000-INIT.
007760       PERFORM ZA000-INIT.
      PERFORM BA000.
007790       GO TO AZ000-EOJ.

       COPY LOCKED.REC.

      /
      *    ****    READ FILES ROUTINES
      *
008180 AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY STOCK.RD.

      /
      *    ****    REWRITE AND WRITE FILES ROUTINES
      *
009850 AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY DEPART.WR.

       COPY PARAM.WR.

       COPY STOCK.WR.

      /
012350 AZ000-END               SECTION.
012360 AZ000-EOJ.
012370       CLOSE STOCK
012380             PARAM
012390             DEPART
                   PRICES
                   NETWORK.
012440 AZ010.
             STOP RUN.

012510 BA000           SECTION 1.
012520 BA00.
012530       PERFORM ERASE-SCREEN.
             DISPLAY "Updating Stock Prices" AT 0230
                      WITH FOREGROUND-COLOR 9.
             DISPLAY "PRICES Record :" AT 0603.
             DISPLAY "Stock Details :" AT 0803.
             MOVE ZERO TO WS-PRCKEY WS-RECKEY.
       BA05.
             READ PRICES AT END
                         GO TO BA10.
    IF PRC-CODE = SPACES
               GO TO BA05.
             ADD 1 TO WS-PRCKEY WS-RECKEY.
             MOVE WS-PRCKEY      TO W80-REC.
             DISPLAY W80-NO AT 0619 WITH FOREGROUND-COLOR 11.
             DISPLAY PRC-CODE AT 0819 WITH FOREGROUND-COLOR 15
                     " " PRC-DESC WITH FOREGROUND-COLOR 14.
             MOVE PRC-CODE       TO W10-ITEM.
             PERFORM CA500.
           IF WS-F-ERROR = ZERO
               PERFORM BC000
               GO TO BA05.
             PERFORM BB000.
             GO TO BA05.
       BA10.
             DISPLAY "Update complete - Press " AT 2312
                      WITH FOREGROUND-COLOR 14 "ENTER"
                      WITH FOREGROUND-COLOR 15.
             ACCEPT WS-OPTION AT 2342 WITH FOREGROUND-COLOR 15 AUTO.
       BA999.
             EXIT.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
015170 CA500         SECTION.
015180 CA501.
013310       MOVE 14             TO W50-LENGTH.
       CA505.
013320       MOVE W10-ITEM       TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-13       TO W10-ITEM
               GO TO CA505.
      CALL "CBL_TOUPPER" USING W50-DATA
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
013380      MOVE W50-ITEM  TO W10-ITEM.
013380       MOVE W50-ITEM       TO STK-CODE.
             PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
       CA599.
             EXIT.
      /
      *    ****   C R E A T E   N E W   S T O C K
      *
015670 BB000           SECTION.
015680 BB00.
      *       MOVE PRC-TS   TO W10-BIN.
      MOVE "001"   TO W10-BIN.
    IF PRC-TAX = "E"
        MOVE ZERO  TO W10-TAX
    ELSE
        MOVE 1   TO W10-TAX.
             MOVE PRC-DESC       TO W10-DESC.
015760       MOVE SPACES         TO W10-CODE2 W10-ACODE.
             MOVE PRC-COST       TO W10-COST W10-AVRG.
           IF PRC-LEDG < 1 OR > 296
               MOVE 297          TO W10-LEDG
           ELSE
               MOVE PRC-LEDG     TO W10-LEDG.
             MOVE PRC-SELL       TO W10-SELL W10-CASH W10-WSALE.
015780       MOVE ZERO           TO W10-QUANT W10-ODTE
015790                              W10-ORDER W10-GUAR
                                    W10-PURCH W10-LTIME
                                    W10-REORD W10-IND.
             MOVE W10-LEDG       TO WS-DEPKEY.
             PERFORM READ-DEPART THRU READ-DEPART-EXIT.
012010       MOVE 3              TO WS-NETKEY.
012020       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
016260 BB20.
             INITIALIZE STK-RECORD1.
016670       MOVE W10-ITEM       TO STK-CODE WS-ITM.
016680       MOVE W10-BIN        TO STK-BIN.
016690       MOVE W10-DESC       TO STK-DESC.
016700       MOVE W10-TAX        TO STK-TAX.
016730       PERFORM BB50.
016740       MOVE W10-MARKUP     TO STK-MARKUP.
016750       MOVE W10-QUANT      TO STK-QUANT.
016760       MOVE W10-REORD      TO STK-LEVEL.
016770       MOVE W10-LTIME      TO STK-LTIME.
016780       MOVE W10-COST       TO W10-CCOST.
016790       MOVE W10-SELL       TO W10-CSELL.
016800     IF W10-COST = ZERO
016810         IF W10-SELL NOT = ZERO
016820             COMPUTE W10-CCOST ROUNDED = (W10-CSELL /
016830                     (100 + W10-MARKUP) * 100).
016840     IF W10-SELL = ZERO
016850         IF W10-COST NOT = ZERO
016860             COMPUTE W10-CSELL ROUNDED = (W10-CCOST +
016870                     ((W10-CCOST * W10-MARKUP) / 100)).
016880       MOVE W10-CCOST      TO W10-COST.
016890       MOVE W10-CSELL      TO W10-SELL.
016900       MOVE W10-COST       TO STK-COST.
016910       MOVE W10-SELL       TO STK-SELL.
016920       MOVE W10-LEDG       TO STK-LEDG.
             MOVE W10-GUAR       TO STK-GUAR.
016930       MOVE W10-ORDER      TO STK-ORD.
016940       MOVE W10-ODTE       TO STK-DTE.
             MOVE W10-PURCH      TO STK-PUR.
016980       MOVE W10-AVRG       TO STK-AVGE.
016990       MOVE W10-IND        TO STK-IND.
016730       PERFORM BB52.
017000     IF (W10-CMARKUP = ZERO) AND
017010        (W10-CASH = ZERO)
017020         MOVE W10-MARKUP TO W10-CMARKUP
017030         MOVE W10-SELL TO W10-CASH
017040         GO TO BB21.
017050     IF W10-CMARKUP = ZERO
017060         MOVE W10-CASH TO W10-CSELL
017070         COMPUTE W10-CMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017080                                       / W10-CCOST) * 100).
017090     IF W10-CASH = ZERO
017100         COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST *
017110                                      W10-CMARKUP) / 100))
017120         MOVE W10-CSELL TO W10-CASH.
017130 BB21.
017140       MOVE W10-CMARKUP TO STK-CMARKUP.
017150       MOVE W10-CASH TO STK-CASH.
             PERFORM BB54.
017000     IF (W10-WMARKUP = ZERO) AND
017010        (W10-WSALE = ZERO)
017020         MOVE W10-CMARKUP TO W10-WMARKUP
017030         MOVE W10-CASH TO W10-WSALE
017040         GO TO BB21D.
017050     IF W10-WMARKUP = ZERO
017060         MOVE W10-WSALE TO W10-CSELL
017070         COMPUTE W10-WMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017080                                       / W10-CCOST) * 100).
017090     IF W10-WSALE = ZERO
017100         COMPUTE W10-CSELL ROUNDED = (W10-CCOST + ((W10-CCOST *
017110                                      W10-WMARKUP) / 100))
017120         MOVE W10-CSELL TO W10-WSALE.
       BB21D.
017140       MOVE W10-WMARKUP    TO STK-WMARKUP.
017150       MOVE W10-WSALE      TO STK-WSALE.
017160       MOVE W10-CODE2      TO STK-XREF.
017170       MOVE W10-ACODE      TO STK-ACODE.
012010       MOVE 3              TO WS-NETKEY.
012020       PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             ADD 1               TO STK-RECORDS.
017190       PERFORM WRITE-STOCK THRU WRITE-STOCK-EXIT.
017230       MOVE STK-RECORD1 TO W15-RECORD1.
012270       PERFORM REWRITE-CONTROL-UNLOCK THRU WRITE-CONTROL-EXIT.
017500       GO TO BB999.
      *
      *    ****  D E F A U L T   M A R K U P
      *
017540 BB50.
017600     IF W10-SELL > ZERO AND W10-COST > ZERO
017610         MOVE W10-SELL     TO W10-CSELL
017620         MOVE W10-COST     TO W10-CCOST
017630         COMPUTE W10-MARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017640                                       / W10-COST) * 100)
017590     ELSE
017580         MOVE DPT-MARKUP   TO W10-MARKUP.
       BB51.
             EXIT.
      *
      *    ****  D E F A U L T   C A S H   M A R K U P
      *
       BB52.
017600     IF W10-CASH > ZERO AND W10-COST > ZERO
017610         MOVE W10-CASH     TO W10-CSELL
017620         MOVE W10-COST     TO W10-CCOST
017630         COMPUTE W10-CMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017640                                        / W10-COST) * 100)
017590     ELSE
017580         MOVE DPT-CMRKUP   TO W10-CMARKUP.
       BB53.
017660       EXIT.
      *
      *    ****  D E F A U L T   W H O L E S A L E   M A R K U P
      *
       BB54.
017600     IF W10-WSALE > ZERO AND W10-COST > ZERO
017610         MOVE W10-WSALE    TO W10-CSELL
017620         MOVE W10-COST     TO W10-CCOST
017630         COMPUTE W10-WMARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017640                                        / W10-COST) * 100)
017590     ELSE
017580         MOVE DPT-WMRKUP   TO W10-WMARKUP.
017650 BB55.
017660       EXIT.
019380 BB999.
019390       EXIT.
       BC000           SECTION.
       BC00.
           IF (PRC-COST < STK-COST) AND (PRC-LEDG = STK-LEDG)
               IF STK-QUANT NOT = ZERO
                   UNLOCK STOCK
                   GO TO BC999.
           IF PRC-COST = ZERO
               GO TO BC05.
           IF PRC-COST < STK-COST
               IF STK-QUANT NOT = ZERO
                   GO TO BC05.
             MOVE PRC-COST       TO STK-COST W10-CCOST.
             MOVE PRC-SELL       TO STK-SELL STK-WSALE
                                    STK-CASH W10-CSELL.
017070       COMPUTE W10-MARKUP ROUNDED = (((W10-CSELL - W10-CCOST)
017080                                     / W10-CCOST) * 100).
016870       MOVE W10-MARKUP     TO STK-MARKUP
016870                              STK-CMARKUP
016870                              STK-WMARKUP.
       BC05.
      *      MOVE PRC-TS  TO STK-BIN.
             MOVE PRC-LEDG       TO STK-LEDG.
             PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
       BC999.
             EXIT.
      /
044130 ZA000-INIT              SECTION 80.
044140 ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
042000      PERFORM ERASE-SCREEN.
      *
      *    ****    D O  N O T  D I S P L A Y  E R R O R
      *     M E S S A G E S
      *
      MOVE 44   TO USER-NUMBER.
      MOVE 1   TO USER-SETTING.
      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    S E T   U P   T H E   F U N C T I O N   K E Y S
      *
      MOVE 1   TO USER-ACTION
        USER-SETTING.
      *
      *    ESC, F1 to F10 keys
      *
      MOVE ZERO   TO USER-NUMBER.
      MOVE 11   TO USER-KEYS.
      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PAGE-UP AND PAGE-DOWN KEYS
      *
      MOVE 53   TO USER-NUMBER.
      MOVE 2   TO USER-KEYS.
      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    PRE-DISPLAY ACCEPT FIELDS BEFORE DATA-ENTRY
      *
      *      MOVE 76   TO USER-NUMBER.
      *      MOVE 3   TO USER-SETTING.
      *      MOVE 1   TO USER-KEYS.
      *      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *
      *    ****    A C T I V A T E   M O U S E
      *
      MOVE 64   TO MOUSE-FUNC.
      MOVE 1   TO MOUSE-PARAM.
      CALL X"AF" USING MOUSE-FUNC
         MOUSE-PARAM.
    IF MOUSE-FUNC NOT = 255
        MOVE "Y"   TO MOUSE-ATTACHED
        MOVE 66   TO MOUSE-FUNC
        MOVE 0   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM
    ELSE
        GO TO ZA00A.
      *
      *    ****    S E T   M O U S E   K E Y   T O   A C T
      *     A S  F U N C T I O N   K E Y
      *
      MOVE 3   TO USER-ACTION.
      MOVE 27   TO USER-NUMBER.
      MOVE 2   TO USER-KEYS.
      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *      CALL CBL_INIT_MOUSE USING MOUSE-HANDLE
      *           MOUSE-BUTTONS
      *     RETURNING MOUSE-STATUS.
      *    IF MOUSE-STATUS = ZERO
      *        MOVE "Y"   TO MOUSE-ATTACHED.
      *
      *    ****    P A R A M E T E R   F I L E
      *
       ZA00A.
      MOVE "PARAM"  TO AFID-KEY.

       ZA00-READ-APACFIDS.
000030      READ APACFIDS WITH IGNORE LOCK
        KEY IS AFID-KEY.
    IF WS-STATUS = "00"
        GO TO ZA00-READ-APACFIDS-EXIT.
006260      STRING "Missing " DELIMITED SIZE
       AFID-KEY DELIMITED BY " "
       " file ID - Status " DELIMITED SIZE
       WS-STATUS DELIMITED SIZE
       INTO WS-ERR-MES.
      PERFORM ERROR-LENGTH THRU ERROR-EXIT.
006370      STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
      EXIT.

       ZA00A-CONTINUE.
      MOVE AFID-PATH  TO W02-PARAM.



044170     OPEN I-O PARAM.
044180     IF RUNTIME-ERROR
               IF FLE-LOCKED
042030             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
044200     IF WS-STATUS NOT = "00"
044210         MOVE 15           TO WS-F-ERROR
044220         GO TO ZA999-EXIT.
044230       MOVE 3              TO WS-PARKEY.
044240       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
044250       MOVE PAR-DISC       TO W02-DSK22 W02-DSK7 W02-DSK2.
044260       MOVE PAR-EXT        TO W02-EXT22 W02-EXT7 W02-EXT2.
      DISPLAY "Disk drive for STK??.POS ?   " AT 0812
                     "*" WITH FOREGROUND-COLOR 14
                     " to EXIT".
      DISPLAY "Stock File No. ?" AT 1012.
             MOVE "A" TO W02-DSKP.
       ZA05.
             ACCEPT W02-DSKP AT 0836
                    WITH FOREGROUND-COLOR 15 UPDATE AUTO.
           IF W02-DSKP = "*"
               CLOSE PARAM
               GO TO ZA205.
      CALL "CBL_TOUPPER" USING W02-DSKP
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
       ZA10.
             ACCEPT W02-EXTP AT 1029
                    WITH FOREGROUND-COLOR 15 UPDATE AUTO.
018450       DISPLAY WS-CHK AT 2318 WITH FOREGROUND-COLOR 14.
018460       ACCEPT WS-OPTION AT 2352 WITH FOREGROUND-COLOR 15 AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
018520    IF WS-OPTION = "N"
018530         GO TO ZA05.
044300       OPEN I-O NETWORK.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
042030             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
044330     IF WS-STATUS NOT = "00"
044340         MOVE 2            TO WS-F-ERROR
044350         GO TO ZA999-EXIT.
044300       OPEN I-O DEPART.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
042030             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
044330     IF WS-STATUS NOT = "00"
044340         MOVE 7            TO WS-F-ERROR
044350         GO TO ZA999-EXIT.
044300       OPEN I-O STOCK.
044310     IF RUNTIME-ERROR
               IF FLE-LOCKED
040420             GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 22       TO WS-F-ERROR
                   GO TO ZA50.
044330     IF WS-STATUS NOT = "00"
044340         MOVE 22           TO WS-F-ERROR
044350         GO TO ZA999-EXIT.
044360       OPEN INPUT PRICES.
044580       MOVE 1 TO WS-PARKEY.
044590       PERFORM READ-PARAM THRU READ-PARAM-EXIT.
044600       MOVE PAR-DMY TO W12-TODAY.
044610       MOVE PAR-YMD TO W12-T-YMD.
044620       MOVE PAR-COMPANY TO W95-COMP.
             GO TO ZA999-EXIT.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA50.
           IF WS-F-ERROR = 22
               MOVE "STOCK file" TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.
       ZA51.
             EXIT PROGRAM.
       ZA55.
041220       MOVE 1 TO WS-S1.
021870       MOVE SPACES TO WS-MID-LNE WS-MID-LNE2.
041240 ZA60.
041250       MOVE WS-G1 TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8 TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
041260     IF WS-S1 < 80
041270         ADD 1 TO WS-S1
041280         GO TO ZA60.
             MOVE WS-G9  TO WS-TCH  (1).
             MOVE WS-G10 TO WS-TCH  (80).
             MOVE WS-G11 TO WS-BCH  (1).
             MOVE WS-G12 TO WS-BCH  (80).
             MOVE WS-G14 TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13 TO WS-TCHR (80) WS-BCHR (80).
021930       MOVE WS-G2  TO WS-TCHR (16) WS-TCHR (41)
021940                      WS-TCHR (64) WS-TCHR (71).
021950       MOVE WS-G3  TO WS-MCHR (16) WS-MCHR (41)
021960                      WS-MCHR (64) WS-MCHR (71)
                            WS-MCHR (1)  WS-MCHR (80)
                            WS-MCH  (1)  WS-MCH  (80).
021970       MOVE WS-G4  TO WS-BCHR (16) WS-BCHR (41)
021980                      WS-BCHR (64) WS-BCHR (71).
044790 ZA200.
044800       DISPLAY "Files locked - Try later" AT 2412
044810                WITH FOREGROUND-COLOR 14.
044820       DISPLAY "Press any key" AT 2512
044830                WITH FOREGROUND-COLOR 14.
044840       ACCEPT WS-OPTION AT 2526 WITH FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
044870 ZA999-EXIT.
044880       EXIT.
      /
044890 ZB000-ERROR             SECTION 81.

041480 OPEN-ERROR.
041490       PERFORM ERASE-SCREEN.
041500       DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.

041480 READ-ERROR.
041490       PERFORM ERASE-SCREEN.
041500       DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       GO TO DISPLAY-FILE-NAME.

041520 WRITE-ERROR.
041530       PERFORM ERASE-SCREEN.
041540       DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
041510       PERFORM DISPLAY-FILE-NAME.

041560 DISPLAY-FILE-NAME.
041610     IF WS-F-ERROR = 2
041620         MOVE W02-NETWORK  TO WS-FILE
041630         MOVE WS-NETKEY    TO WS-KEY
041800     ELSE
041810     IF WS-F-ERROR = 7
041820         MOVE W02-DEPART   TO WS-FILE
041830         MOVE WS-DEPKEY    TO WS-KEY
041840     ELSE
041850     IF WS-F-ERROR = 15
041860         MOVE WS-PARID     TO WS-FILE
041870         MOVE WS-PARKEY    TO WS-KEY
           ELSE
041850     IF WS-F-ERROR = 22
041860         MOVE W02-STOCKF   TO WS-FILE
041870         MOVE ZERO         TO WS-KEY.
041880     IF WS-STATUS = "10"
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
045420         DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
045440         DISPLAY "Please make a note of these details" AT 1812.
042170       STOP RUN.
