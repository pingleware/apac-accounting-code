      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *    ******   ********  ******** *******   ********  ********   *
      *   ** **     **     ** **    **  **     **        *
      *   **        **     ** **    **  **     **        *
      *    ******      **     ***** *******   *****     *****      *
      *  **     **     ** **    **  **     **        *
      *   ** **     **     ** **    **  **     **        *
      *    ******      **     ** **    **  ********  **        *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     ( A P A C )   S T A F F   R E F R E S H M E N T S        *
      *                                                                *
      *     VERSION 8.14.03 - May 2010           *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     STFREF.
       AUTHOR.         J W LEMMON  (APAC).
       DATE-WRITTEN.   JUNE 1990.

     COPYRIGHT NOTICE: COPYRIGHT (C) 1990 - 2010
         by James William Lemmon.

     All rights reserved.

       SECURITY.   PROGRAM MATERIAL IS THE SOLE PROPERTY OF
           James William Lemmon.(Id No. 4412165050082).

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               CURSOR IS CSTART
               CONSOLE IS CRT
         CRT STATUS IS KEY-STATUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY APACFIDS.SL.

       COPY CONTROL.SL.

       COPY LEDTRF.SL.

       COPY PARAM.SL.

       COPY RECOVER.SL.

          SELECT CSHSLE  ASSIGN DISK
                          ORGANIZATION LINE SEQUENTIAL
                          ACCESS SEQUENTIAL.
      /
       DATA DIVISION.
       FILE SECTION.

       COPY APACFIDS.FDE.

       COPY CONTROL.FDE.

       COPY LEDTRF.FDE.

       COPY PARAM.FDE.

       COPY RECOVER.FD.

       FD  CSHSLE    LABEL RECORD STANDARD
                     VALUE OF FILE-ID W02-PRINTER.
       01  CSH-L.
           03  CSH-DATA.
        05  CSH-LINE PIC  X(40).
        05  FILLER   PIC  X(40).
      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-DISC     PIC  9(02)V99 COMP-3.
       77  WS-TDISC         PIC  9(02)V99 COMP-3.
       77  WS-SUB           PIC  9(04)    COMP-5.
       77  WS-SUPER         PIC  9(02)    COMP-5.
       77  WS-S1            PIC  9(04)    COMP-5.
       77  WS-S2            PIC  9(04)    COMP-5.
       77  WS-S3            PIC  9(04)    COMP-5.
       77  WS-S4            PIC  9(04)    COMP-5.
       77  WS-S5            PIC  9(04)    COMP-5.
       77  WS-S6            PIC  9(04)    COMP-5.
       77  WS-S7            PIC  9(04)    COMP-5.
       77  WS-S8            PIC  9(04)    COMP-5.
       77  WS-S9            PIC  9(04)    COMP-5.
       77  WS-IXS           PIC  9(04)    COMP-5.
       77  WS-IXS1          PIC  9(04)    COMP-5.
       77  WS-IXS2          PIC  9(04)    COMP-5.
       77  WS-IXS3          PIC  9(04)    COMP-5.
       77  WS-IXS4          PIC  9(04)    COMP-5.
       77  WS-STKKEY        PIC  9(04)    COMP-5.
       77  WS-PARKEY        PIC  9(04)    COMP-5.
       77  WS-DEBKEY        PIC  9(04)    COMP-5.
       77  WS-NETKEY        PIC  9(04)    COMP-5.
       77  WS-AUDKEY        PIC  9(04)    COMP-5.
       77  WS-SALSTR        PIC  9(04)    COMP-5.
       77  WS-SALKEY        PIC  9(04)    COMP-5.
       77  WS-RECKEY        PIC  9(04)    COMP-5.
       77  WS-RECOVER       PIC  9(04)    COMP-5.
       77  WS-TRANS         PIC  9(04)    COMP-5 VALUE 1.
       77  WS-KEY1          PIC  9(04)    COMP-5.
       77  WS-ITM           PIC  X(14).
       77  WS-ITM1          PIC  X(14).
       77  WS-START         PIC  9(08).
       77  WS-END           PIC  9(08).
       77  WS-TIME          PIC  9(08).
       77  WS-TME2          PIC  9(08).
       77  WS-PORT    PIC 9(05) VALUE 259.
      * 1004.
       77  WS-DRAW1    PIC 9(05) VALUE 260.
       77  WS-OPEN     PIC  X(01) VALUE X"01".
       77  WS-CLOSE         PIC  X(01) VALUE X"00".
       77  WS-PAR           PIC  9(01) VALUE 0.
       77  WS-INO           PIC  9(02) VALUE 0.
       77  WS-LINES         PIC  9(02) VALUE 0.
       77  WS-FIRST         PIC  9(02) VALUE 0.
       77  WS-LAST          PIC  9(02) VALUE 0.
       77  WS-PAGE          PIC  9(04)    COMP-5.
       77  WS-OPTION        PIC  X(01).
       77  WS-PRNINV        PIC  X(01) VALUE "Y".
       77  WS-VAT-GST      PIC  X(01) VALUE "V".
       77  WS-SKIP         PIC  X(01) VALUE " ".
       77  WS-LOOK         PIC  X(01) VALUE "S".
       77  WS-ETYPE        PIC  X(01) VALUE "I".
       77  WS-WS            PIC X(01).
       77  WS-NUM           PIC  Z9.
       77  WS-ERROR         PIC  9(01).
       77  WS-SELECT        PIC  X(13)     VALUE "Select Option".
       77  WS-CHAR1         PIC  X(01).
       77  WS-CHAR2         PIC  X(01).
       77  WS-COMP          PIC  9(01).
       77  WS-INDI          PIC  9(01) VALUE ZERO.
       77  WS-UPDATE        PIC  9(01) VALUE ZERO.
       77  WS-CONREC        PIC  9(01) VALUE ZERO.
       77  WS-HEAD          PIC  X(01).
       77  WS-TYPE          PIC  X(01).
       77  WS-AGE           PIC  X(01).
       77  WS-PRC           PIC  X(01).
       77  WS-BATCH         PIC  9(05).
       77  WS-A             PIC  X(19) VALUE "Any key to continue".
       77  WS-PER           PIC  Z(02)9.99.
       77  WS-CR            PIC  X(01).
       77  WS-GSTRES        PIC  S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
       77  WS-DRAW    PIC 9(02).
      /
       01  WS-DB-LINE.
           03  WS-TOP-LINE.
               05  WS-TCHAR PIC X(01) OCCURS 80.
           03  WS-TOP-LNE2.
               05  WS-TCR  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
              05  FILLER      PIC  X(23).
        05  WS-WRKHD    PIC  X(11).
              05  FILLER      PIC  X(01).
        05  WS-WRKST    PIC  X(03).
              05  FILLER      PIC  X(01).
          03  WS-MID-LINE.
               05  WS-MCHAR PIC X(01) OCCURS 80.
    03  WS-MINV-LINE REDEFINES WS-MID-LINE.
        05  WS-MIV1     PIC  X(01).
        05  WS-MIV78    PIC  X(78).
        05  WS-MIV80    PIC  X(01).
          03  WS-MID-LNE2.
        05  FILLER      PIC  X(01) VALUE "ณ".
        05  WS-BLNK78   PIC  X(78) VALUE ALL "ฐ".
        05  FILLER      PIC  X(01) VALUE "ณ".
          03  WS-BOT-LINE.
               05  WS-BCHAR PIC X(01) OCCURS 80.
           03  WS-BOT-LNE2.
               05  WS-BCR  PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE2.
               05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
                  07  WS-SRT-LINE
                           PIC X(50).
                  07  FILLER
                           PIC X(10).
               05  FILLER  PIC X(19).
           03  WS-TOP-LNE.
               05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
           03  WS-MID-LNE.
               05  WS-MCHR PIC X(01) OCCURS 80.
    03  WS-MST-LNE REDEFINES WS-MID-LNE.
        05  WS-MST1     PIC  X(01).
        05  WS-MST78    PIC  X(78).
        05  WS-MST80    PIC  X(01).
          03  WS-BOT-LNE.
               05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-TP-LNE.
               05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-MD-LNE.
               05  WS-MCH  PIC X(01) OCCURS 80.
           03  WS-BT-LNE.
               05  WS-BCH  PIC X(01) OCCURS 80.
           03  WS-MM-LINE.
        05  FILLER      PIC  X(01) VALUE "ณ".
        05  WS-SPACE    PIC  X(60) VALUE SPACES.
        05  FILLER      PIC  X(01) VALUE "ณ".
    03  WS-HME-LINE PIC X(60) VALUE ALL "อ".

       COPY WS.WS.

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "CSH".
    03  WS-PROG        PIC  X(03) VALUE "003".

       COPY LEDTRF.WS.

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FID.

       COPY APACFIDS.ID.

       COPY CONTROL.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

       COPY RECOVER.ID.

       01  W02-FILE-IDS.
           03  W02-PRINTER     PIC X(12) VALUE "LPT1".

       COPY W05.VAT.

       COPY W10.SAL.

       COPY W12.WS.

       01  W15-CASH-SLIP.
           03  W15-L1.
      *  05  W15-ESC1 PIC  X(01).
      *  05  W15-DBLE PIC  X(01).
               05  W15-EXPP1   PIC  X(01).
        05  FILLER      PIC  X(04) VALUE SPACES.
               05  W15-COMP    PIC  X(32) VALUE SPACES.
               05  W15-ECAN1   PIC  X(01).
           03  W15-L1A.
      *  05  W15-ESC5 PIC  X(01).
      *  05  W15-DBLE3 PIC  X(01).
               05  W15-EXPP4   PIC  X(01).
              05  W15-DOCTYP  PIC  X(19) VALUE SPACES.
               05  W15-ECAN4   PIC  X(01).
          03  W15-L4.
              05  FILLER      PIC  X(19) VALUE "Date/Datum       :".
               05  W15-DATE    PIC  Z9/99/99.
          03  W15-L4A.
              05  FILLER      PIC  X(19) VALUE "Time/Tyd         :".
              05  W15-TIME    PIC  X(08).
           03  W15-L5.
              05  FILLER      PIC  X(19) VALUE "Salesperson      :".
              05  W15-SMAN    PIC  X(21).
           03  W15-L7A.
              05  FILLER      PIC  X(19) VALUE "Petty Cash Vouch :".
               05  W15-CRNO    PIC  X(08).
          03  W15-L13.
              05  W15-REMK    PIC X(31).
              05  W15-VAL     PIC Z(04)9.99-.

       COPY W20.WS.

       01  W25-CALCS.
           03  W25-RESULT      PIC  9(02)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC  9(02).
               05  W25-DECIMAL PIC  9(02).
           03  W25-TOTAL       PIC  9(07)V99 COMP-3.
           03  W25-VALUE       PIC S9(07)V99 COMP-3.
           03  W25-INSTAL      PIC S9(07)V99 COMP-3.
           03  W25-CREDIT      PIC S9(07)V99 COMP-3.
           03  W25-CUR         PIC S9(07)V99 COMP-3.
           03  W25-INSTL.
               05  W25-MTHS OCCURS 6
                               PIC S9(07)V99 COMP-3.
           03  W25-INSTALS REDEFINES W25-INSTL.
               05  W25-MTH1    PIC S9(07)V99 COMP-3.
               05  W25-MTH2    PIC S9(07)V99 COMP-3.
               05  W25-MTH3    PIC S9(07)V99 COMP-3.
               05  W25-MTH4    PIC S9(07)V99 COMP-3.
               05  W25-MTH5    PIC S9(07)V99 COMP-3.
               05  W25-MTH6    PIC S9(07)V99 COMP-3.
          03  W25-TIME        PIC 9(08).
           03  W25-TRED REDEFINES  W25-TIME.
               05  W25-HH      PIC 9(02).
               05  W25-MM      PIC 9(02).
               05  W25-SS      PIC 9(02).
               05  FILLER      PIC 9(02).

       01  W40-COMPANY.
          03  W40-NAME.
              05  W40-NCH     PIC  X(01) OCCURS 32.

       COPY W40.WS.

       COPY FUNCTION.WS.

       COPY W50.WS.

       01  W60-ITEM.
           03  W60-ITM.
               05  W60-ICHAR OCCURS 14
                               PIC  X(01).
       01  W65-ENTRY.
           03  W65-FOUR.
               05  W65-CHAR OCCURS 2
                               PIC  X(01).
       COPY W70DEBT.IWS.

       01  W85-RECORD151.
           03  W85-CSHDTE      PIC  9(06).
           03  W85-FLOAT       PIC S9(09)V99 COMP-3.
           03  W85-CASH        PIC S9(09)V99 COMP-3.
           03  W85-CHEQUES     PIC S9(09)V99 COMP-3.
           03  W85-CARDS       PIC S9(09)V99 COMP-3.
           03  W85-VATVAL      PIC S9(09)V99 COMP-3.
           03  W85-VOUCHERS    PIC S9(09)V99 COMP-3.
           03  W85-RECEIPTS    PIC S9(09)V99 COMP-3.
           03  W85-CSH         PIC S9(09)V99 COMP-3.
           03  W85-CSHMTD      PIC S9(09)V99 COMP-3.
           03  W85-CSHYTD      PIC S9(09)V99 COMP-3.
           03  W85-ESC         PIC  X(01).
           03  W85-DBL         PIC  X(01).
           03  W85-CDBL        PIC  X(01).
           03  W85-SI          PIC  X(01).
           03  W85-CSI         PIC  X(01).
           03  W85-SO          PIC  X(01).
           03  W85-CSO         PIC  X(01).
           03  W85-6LPI        PIC  X(01).
           03  W85-8LPI        PIC  X(01).
           03  W85-NUL         PIC  X(01).
           03  W85-ADVANCE     PIC  9(02) COMP-5.
           03  W85-CPRN        PIC  9(01).
           03  W85-ITMS        PIC S9(07)    COMP-3.
           03  W85-CUST        PIC S9(07)    COMP-3.
           03  W85-TAXVAL      PIC S9(09)V99 COMP-3.
           03  W85-ADVTAX      PIC S9(09)V99 COMP-3.
          03  W85-PETTY       PIC S9(09)V99 COMP-3.
    03  W85-SPEED       PIC S9(09)V99 COMP-3.
          03  FILLER        PIC  X(14).
    03  W85-PORT        PIC  9(01).
           03  W85-DRAWER      PIC  X(01).
       01  W85-PASS.
           03  W85-SUPER OCCURS 9
                               PIC  X(06).
           03  W85-ENTRY       PIC  9(02)    COMP-3.
           03  W85-MARG        PIC S9(02)V99.
       01  W90-TRAN.
           03  W90-DATE        PIC  X(08).
           03  W90-DTE REDEFINES W90-DATE
                               PIC  99/99/99.
           03  W90-CODE        PIC  9(02).
           03  W90-ORD         PIC  X(12).
           03  W90-REF         PIC  X(08).
           03  W90-REG         PIC  X(09).
           03  W90-DB          PIC  9(07)V99 COMP-3.
           03  W90-CR          PIC  9(07)V99 COMP-3.
           03  W90-BDT         PIC S9(07)V99 COMP-3.
           03  W90-BCR         PIC S9(07)V99 COMP-3.
           03  W90-DESC        PIC  X(12).
           03  W90-120         PIC S9(07)V99 COMP-3.
           03  W90-90          PIC S9(07)V99 COMP-3.
           03  W90-60          PIC S9(07)V99 COMP-3.
           03  W90-30          PIC S9(07)V99 COMP-3.
           03  W90-INT         PIC S9(07)V99 COMP-3.
           03  W90-DUE         PIC S9(07)V99 COMP-3.
           03  W90-CASH        PIC S9(07)V99 COMP-3.
           03  W90-CHQ         PIC S9(07)V99 COMP-3.
           03  W90-CARD        PIC S9(07)V99 COMP-3.
           03  W90-CHANGE      PIC S9(07)V99 COMP-3.
           03  W90-PMT         PIC S9(07)V99 COMP-3.
           03  W90-CUR         PIC S9(07)V99 COMP-3.
           03  W90-BAL         PIC S9(07)V99 COMP-3.
           03  W90-TAX         PIC  X(14).
           03  W90-DVAL        PIC  9(07)V99.
           03  W90-SALES       PIC S9(07)V99 COMP-3.
           03  W90-TDISC       PIC S9(07)V99 COMP-3.
          03  W90-ATAX        PIC S9(07)V99 COMP-3.
           03  W90-TTAX        PIC S9(07)V99 COMP-3.
           03  W90-TCOST       PIC S9(07)V99 COMP-3.
           03  W90-TSELL       PIC S9(07)V99 COMP-3.
           03  W90-DISC        PIC S9(07)V99 COMP-3.
           03  W90-CRD         PIC S9(07)V99 COMP-3.
           03  W90-CDISC       PIC S9(07)V99 COMP-3.
           03  W90-LDISC       PIC S9(07)V99 COMP-3.
           03  W90-TAXVAL      PIC S9(07)V99 COMP-3.
           03  W90-EXCDIS      PIC S9(07)V99 COMP-3.
           03  W90-EXCTOT      PIC S9(07)V99 COMP-3.
           03  W90-NONTAX      PIC S9(07)V99 COMP-3.
           03  W90-VALUE       PIC S9(07)V99 COMP-3.
           03  W90-STOTAL      PIC S9(07)V99 COMP-3.
           03  W90-TOTAL       PIC S9(07)V9(06) COMP-3.
       01  W95-STM.
           03  W95-COMP        PIC  X(40).
           03  W95-ADD1        PIC  X(30).
           03  W95-ADD2        PIC  X(30).
           03  W95-ADD3        PIC  X(30).
           03  W95-ADD4        PIC  X(30).
           03  W95-POST        PIC  9(04).
           03  W95-TEL         PIC  X(08).
    03  W95-TIME.
               05  W95-HH      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-MM      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-SS      PIC 99.

       01  W100-EDIT.
           03  W100-BAL        PIC  Z(06)9.99-.
           03  W100-CUR        PIC  Z(06)9.99-.
           03  W100-30         PIC  Z(06)9.99-.
           03  W100-60         PIC  Z(06)9.99-.
           03  W100-90         PIC  Z(06)9.99-.
           03  W100-INT        PIC  Z(06)9.99-.
           03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.

       LINKAGE SECTION.

       COPY CHAIN.LS.

      /
       SCREEN SECTION.

 COPY BLANK.CRT.

       01  S05.
          03  LINE 12 COLUMN 12 VALUE "Enter Password".

       01  S06.
          03  LINE 12 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5 PIC  X(06)
     USING W40-CSHPASS AUTO SECURE.

       01  S07.
          03  LINE  2 COLUMN 27 FOREGROUND-COLOR 15
     VALUE "STAFF REFRESHMENTS/WAISTAGE".
          03  LINE  4 COLUMN 24 VALUE "Salesperson".
    03        COLUMN 36 BACKGROUND-COLOR 5
     PIC  X(02) FROM WS-SPACE.
          03  LINE  5 COLUMN 24 VALUE "Ref. Number".
    03        COLUMN 36 BACKGROUND-COLOR 5
     PIC  X(08) FROM WS-SPACE.
          03  LINE  6 COLUMN 24 VALUE "Date       ".
    03        COLUMN 36 BACKGROUND-COLOR 5
     PIC  X(08) FROM WS-SPACE.
    03  LINE  7 COLUMN 24 VALUE "Narrative  ".
    03        COLUMN 36 BACKGROUND-COLOR 5
     PIC  X(24) FROM WS-SPACE.
          03  LINE  8 COLUMN  2 REVERSE-VIDEO VALUE "No Item Code    ".
           03          COLUMN 17 REVERSE-VIDEO VALUE "   Description  ".
           03          COLUMN 33 REVERSE-VIDEO VALUE "                ".
           03          COLUMN 49 REVERSE-VIDEO VALUE "  Quantity ".
          03        COLUMN 60 REVERSE-VIDEO VALUE " Unit Cost  ".
           03          COLUMN 72 REVERSE-VIDEO VALUE "Extended".
          03  LINE  9 COLUMN  1 BACKGROUND-COLOR 0 VALUE "ฦ".
          03        COLUMN  2 VALUE "อออัออออออออออออออัอออออออออออออ
      -        "ออออออออออออออออัออออออออัอออออออออัออออออออออ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "ต".
          03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0 VALUE "ฦ".
          03        COLUMN  2 VALUE "อออฯออออออออออออออฯอออออออออออออ
      -        "ออออออออออออออออฯออออออออฯอออออออออฯออออออออออ".

       01  S26-CASH.
          03  LINE  9 COLUMN 36 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC Z(06)9.99- USING W90-CASH AUTO.

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000    SECTION.
       AA000-INIT.
          IF LS0-SALEV < 3
        MOVE "Not Authorised"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
      PERFORM JA000.
      CLOSE CSHSLE
     RECOVER.
       AA49.
             EXIT PROGRAM.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY CONTROL.RD.

       COPY LEDTRF.RD.

       COPY PARAM.RD.

       COPY SHARED.RD.

      /
       AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY LEDTRF.WR.

       COPY PARAM.WR.

       COPY APAC.HLP.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
       AY000           SECTION.
       AY01.
             MOVE 01             TO REC-FILE.
             MOVE WS-PARKEY      TO REC-KEY.
            PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-RECORD1    TO REC-PARAM.
             GO TO AY50.
       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
             GO TO AY50.
       AY40.
             MOVE 40             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
             MOVE XFR-REC        TO REC-LEDTRF.
             GO TO AY50.
       AY49.
             MOVE 99             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE SPACES         TO REC-DETAIL.
             PERFORM AY50.
             ADD 1               TO WS-TRANS.
           IF WS-RECOVER > 95
               CLOSE RECOVER
               OPEN OUTPUT RECOVER
               CLOSE RECOVER
               OPEN I-O RECOVER
               MOVE ZERO         TO WS-RECOVER.
             GO TO AY59.
       AY50.
             ADD 1               TO WS-RECOVER.
             MOVE WS-RECOVER     TO WS-RECKEY.
             MOVE WS-TRANS       TO REC-TRAN.
             WRITE REC-RECORD.
           IF WS-STATUS NOT = "00"
               DISPLAY "Write error Recovery file - Status " AT 2212
                        WITH FOREGROUND-COLOR 14
                        WS-STATUS WITH FOREGROUND-COLOR 15
               STOP RUN.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
            MOVE 1   TO WS-COUNT.
            MOVE 5   TO WS-SHARED.
            PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
      MOVE SHR-STOCK  TO WS-STOCK.
      *
      *    ****    Read PARAM record 4 - Lock record and start recovery
      *            file procedure.
      *
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK THRU READ-PARAM-EXIT.
           IF PAR-USERS < 24
               MOVE 1            TO WS-SUB
        MOVE ZERO  TO WS-WAIT
               GO TO AY62.
      *
      *    ****   Q   F U L L  -  W A I T   F O R   4 S E C O N D S
      *
            DISPLAY "WAITING" AT 2551
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 12.
            COMMIT.
      ACCEPT WS-STIME FROM TIME.
      MOVE 400   TO WS-WAIT.
            PERFORM LOCK-REC-LOOP.
            DISPLAY SPACE AT 2551
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.
             GO TO AY60.

       AY61.
            MOVE "DS"   TO PAR-PROG(WS-USUB).
            MOVE LS-USER  TO PAR-USR(WS-USUB).
             PERFORM REWRITE-PARAM-UNLOCK THRU WRITE-PARAM-EXIT.
      *
      *    ****    Read the DEBTOR control record in the NETWORK file
      *            and lock it. Log details to the RECOVERY file.
      *
            MOVE 1   TO WS-NETKEY.
             PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT.
             PERFORM AY39 THRU AY59.
             MOVE NET-DEBTOR     TO W70-DEBT.
    IF WS-STOCK = "Y"
              MOVE 3   TO WS-SHARED
              PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT
        PERFORM AY37 THRU AY59
        MOVE SHR-RECORD  TO NET-RECORD
    ELSE
              MOVE 3   TO WS-NETKEY
              PERFORM READ-CONTROL-LOCK THRU READ-CONTROL-EXIT
              PERFORM AY39 THRU AY59.
             GO TO AY999.
      *
      *    ****   A R E   A N Y   U P D A T E S   I N P R O G R E S S
      *
       AY62.
    IF NOT (PAR-PROG(WS-SUB) = SPACES OR
     PAR-USR(WS-SUB) = SPACES)
      *
      *    ****   A R E   D E B T O R O R   S T O C K   F I L E S
      *    B E I N G   U P D A T E D
      *
        IF NOT (PAR-PROG(WS-SUB) = SPACES)
           IF PAR-PROG(WS-SUB) = "DB" OR "DS" OR "CS" OR "ST"
      *
      *    ****   Y E S   -   S E T   W A I T P E R I O D
      *
         GO TO AY62-WAIT
     ELSE
         ADD 1  TO WS-SUB
         GO TO AY62
     END-IF
        ELSE
      *
      *    ****   I S T H I S   P R O G R A M   I N T H E  Q
      *
        IF PAR-USR(WS-SUB) = LS-USER
      *
      *    ****   I S I T   N E X T I N   L I N E T O   P R O C E S S
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
      MOVE LS-USER  TO PAR-USR(WS-SUB).
      MOVE WS-SUB  TO PAR-USERS.
      PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
      GO TO AY62-CHECK.
      *
      *    ****   S E T   W A I T   P E R I O D
      *
       AY62-WAIT.
      MOVE 300   TO WS-WAIT.
    IF NOT (PAR-USR(WS-SUB) = LS-USER)
        IF WS-SUB < 24
     ADD 1  TO WS-SUB
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
            DISPLAY SPACE AT 2572
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 3.

       AY63.
            MOVE WS-SUB  TO WS-USUB.
            GO TO AY61.

       AY70.
            MOVE 4   TO WS-PARKEY.
             PERFORM READ-PARAM-LOCK-END THRU READ-PARAM-EXIT.
      *
      *    ****    Write links back and unlock PARAM record 4 and
      *            NETWORK record 1.
      *
    IF WS-STOCK = "Y"
        MOVE NET-RECORD  TO SHR-RECORD
              PERFORM REWRITE-SHARED THRU WRITE-SHARED-EXIT
    ELSE
              PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             MOVE 1              TO WS-NETKEY.
             MOVE W70-DEBT       TO NET-DEBTOR.
            PERFORM REWRITE-CONTROL THRU WRITE-CONTROL-EXIT.
             PERFORM AY49 THRU AY59.
      MOVE 1   TO WS-USUB.

       AY72.
    IF NOT (PAR-USR(WS-USUB) = LS-USER)
        ADD 1   TO WS-USUB
        GO TO AY72.

       AY75.
            MOVE SPACES  TO PAR-PROG(WS-USUB)
              PAR-USR(WS-USUB).
    IF WS-USUB < 24
        ADD 1 WS-USUB  GIVING WS-SUB
    ELSE
        GO TO AY80.
    IF NOT (PAR-PROG(WS-SUB) = SPACES)
        MOVE PAR-PROG(WS-SUB)
     TO PAR-PROG(WS-USUB)
        MOVE PAR-USR(WS-SUB)
     TO PAR-USR(WS-USUB)
        ADD 1   TO WS-USUB
        GO TO AY75.

       AY80.
            SUBTRACT 1 FROM WS-USUB
     GIVING PAR-USERS.
            PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
    IF LS0-POS NOT = 2
               CLOSE RECOVER
               OPEN I-O RECOVER.
             COMMIT.

       AY999.
             EXIT.

       COPY LEDTRF.UPD.

      /
       JA000         SECTION 50.
       JA00.
             PERFORM ERASE-SCREEN.
            DISPLAY "PROCESS PETTY CASH VOUCHER" AT 0229
                      WITH FOREGROUND-COLOR 15.
             MOVE SPACES TO W40-CSHPASS.
             DISPLAY S05.
             ACCEPT S06.
             MOVE 6              TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-CSHPASS
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            MOVE 1   TO WS-LENGTH.
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW         TO W85-PASS.
           IF NOT (W40-CSHPASS = W85-SUPER (1) OR
                                 W85-SUPER (5) OR
                                 "!@#$%^")
              MOVE "Invalid password"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO JA999.
             ADD 150 WS-DRAW     GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-RECORD151  TO W85-RECORD151.
       JA05.
             PERFORM ERASE-SCREEN.
             DISPLAY S07.
            MOVE 10   TO LIN.
            PERFORM JA48 UNTIL LIN > 18.
            MOVE W85-CSHDTE TO  W90-DTE W15-DATE.
            DISPLAY W90-DTE AT 0636
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            MOVE ZERO   TO W10-SMAN.
             MOVE SPACES         TO CSH-DATA.
       JA10.
      DISPLAY "F1" AT 2502
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "ape to EXIT"
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
            ACCEPT W10-SMAN AT 0436
            WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO JA999
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO JA10
   WHEN OTHER
       CALL X"E5"
       GO TO JA10
        END-EVALUATE.
    IF W10-SMAN = 99
        GO TO JA999.
           IF W10-SMAN > 50
              GO TO JA10.
      PERFORM CLEAR-L50.
           IF W10-SMAN = ZERO
        MOVE SPACES  TO W15-SMAN
              GO TO JA15.
             ADD 100 W10-SMAN    GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-SALESMAN = SPACES
              GO TO JA10.
            DISPLAY PAR-SALESMAN AT 0439
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            MOVE PAR-SALESMAN TO W15-SMAN.
       JA15.
            MOVE SPACES  TO W10-DESC.
       JA20.
            ACCEPT W10-DESC AT 0736
               WITH FOREGROUND-COLOR 7 HIGHLIGHT
       BACKGROUND-COLOR 5 UPDATE AUTO.
    IF W10-DESC = SPACES
        GO TO JA20.
       JA25.
      *
      *    ****    C A S H   V A L U E
      *
            PERFORM CLEAR-L50.
             ACCEPT S26-CASH.
            MOVE W90-CASH  TO W90-PMT.
      *
      *    ****    A R E   P A Y M E N T   D E T A I L S   C O R R E C T
      *
       JA40.
            MOVE "'C'orrect or 'R'ekey refund"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "C" OR "R")
              GO TO JA40.
           IF WS-OPTION = "R"
              GO TO JA25.
    IF W90-CASH = ZERO
        GO TO JA999.
      *
      *    ****    P R O C E S S   P E T T Y   C A S H  V O U C H E R
      *
            DISPLAY "Processing Petty Cash" AT 2520 WITH
              BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
             PERFORM AY60 THRU AY999.
             MOVE W70-INVOICE TO W90-REF W15-CRNO.
           IF W70-INVOICE = 99999999
               MOVE 1 TO W70-INVOICE
           ELSE
               ADD 1 TO W70-INVOICE.
            DISPLAY  W90-REF AT 0536
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 5.
      MOVE W85-SI  TO CSH-DATA.
      WRITE CSH-L.
    IF W90-CASH < ZERO
        MOVE "PETTY CASH REFUND"
     TO W15-DOCTYP
    ELSE
              MOVE "PETTY CASH VOUCHER"
     TO W15-DOCTYP.
            MOVE W15-L1A  TO CSH-DATA.
             WRITE CSH-L.
            MOVE W15-L1  TO CSH-DATA.
             WRITE CSH-L.
            MOVE W15-L4  TO CSH-DATA.
             WRITE CSH-L.
            ACCEPT W25-TIME FROM TIME.
             MOVE W25-HH         TO W95-HH.
             MOVE W25-MM         TO W95-MM.
             MOVE W25-SS         TO W95-SS.
      MOVE W95-TIME  TO W15-TIME.
            MOVE W15-L4A  TO CSH-DATA.
             WRITE CSH-L.
          IF W10-SMAN NOT = ZERO
              MOVE W15-L5  TO CSH-DATA
               WRITE CSH-L.
            MOVE W15-L7A  TO CSH-DATA.
             WRITE CSH-L.
            MOVE ALL "-"  TO CSH-LINE.
             WRITE CSH-L.
            MOVE SPACES  TO CSH-DATA.
             WRITE CSH-L.
            MOVE W10-DESC  TO W15-REMK.
            MOVE W90-CASH  TO W15-VAL W25-VALUE.
            MOVE W15-L13  TO CSH-DATA.
             WRITE CSH-L.
            MOVE SPACES  TO CSH-DATA.
             WRITE CSH-L.
            MOVE ALL "-"  TO CSH-LINE.
             WRITE CSH-L.
            MOVE SPACES  TO CSH-DATA.
            MOVE 1   TO WS-S9.
       JA45.
             WRITE CSH-L.
          IF WS-S9 < 4
              ADD 1   TO WS-S9
              GO TO JA45.
      MOVE "Authorised________________________"
     TO CSH-LINE.
      WRITE CSH-L.
      MOVE SPACES  TO CSH-DATA.
            WRITE CSH-L.
             MOVE SPACES TO CSH-DATA.
             MOVE 1 TO WS-S9.
       JA50.
             WRITE CSH-L.
           IF WS-S9 < W85-ADVANCE
               ADD 1 TO WS-S9
              GO TO JA50.
      MOVE W85-CSI  TO CSH-DATA.
             WRITE CSH-L.
             MOVE SPACES         TO CSH-DATA.
      CLOSE CSHSLE.
      OPEN OUTPUT CSHSLE.
      CALL X"91" USING X91-RES X91-FUN CSHSLE.
            ADD 150 WS-DRAW  GIVING WS-PARKEY.
             PERFORM AY01 THRU AY59.
            SUBTRACT W90-CASH  FROM PAR-PETTY.
      *          PAR-CASH.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
            MOVE PAR-RECORD151  TO W85-RECORD151.
            MOVE 7   TO WS-PARKEY.
             PERFORM AY01 THRU AY59.
      *
      *    ****    U P D A T E  P E T T Y   C A S H
      *
    IF WX-INT = "Y"
               MOVE W12-T-YMD    TO WX-DATE
        MOVE WS-DRAW  TO WX-LEDG
        MOVE "Petty Cash Vouchers"
                                 TO WX-NAR
        COMPUTE WX-VALUE = W90-CASH * -1
               MOVE PAR-CASH-GL  TO WX-AC
        MOVE 37   TO WX-TYPE
        PERFORM GL-TRF-UPDATE
        MOVE W90-CASH  TO WX-VALUE
        MOVE WX-PETGL  TO WX-AC
        MOVE 40   TO WX-TYPE
        PERFORM GL-TRF-UPDATE.
             PERFORM AY70 THRU AY999.
            CLOSE PARAM NETWORK.
            OPEN I-O PARAM NETWORK.
            PERFORM CLEAR-L50.
            MOVE "ENTER to continue"
     TO WS-ERR-MES.
      PERFORM OPT-MESSAGE.
       JA999.
             EXIT.
      /
       ZA000-INIT        SECTION 91.
       ZA000-OPEN.
             PERFORM ZA58 THRU ZA60.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY.
      MOVE LS-USUB  TO WS-USUB.
      MOVE LS-DRAW  TO WS-DRAW.
            PERFORM ERASE-SCREEN.
            DISPLAY "CASH DRAWER PROGRAM" AT 0231
                      WITH FOREGROUND-COLOR 15.
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
      *
      *    ****    P A R A M E T E R   F I L E
      *
       ZA00A.
      MOVE "PARAM"  TO AFID-KEY.

       ZA00-READ-APACFIDS.
            READ APACFIDS WITH IGNORE LOCK
        KEY IS AFID-KEY.
    IF WS-STATUS = "00"
        GO TO ZA00-READ-APACFIDS-EXIT.
            STRING "Missing " DELIMITED SIZE
       AFID-KEY DELIMITED BY " "
       " file ID - Status " DELIMITED SIZE
       WS-STATUS DELIMITED SIZE
       INTO WS-ERR-MES.
      PERFORM ERROR-LENGTH THRU ERROR-EXIT.
            STOP RUN.

       ZA00-READ-APACFIDS-EXIT.
      EXIT.

       ZA00A-CONTINUE.
      MOVE AFID-PATH  TO W02-PARAM.
      MOVE "LEDTRF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-LEDTRF.
      MOVE "NETWORK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-NETWORK.
      MOVE "RECOVER"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      INSPECT AFID-PATH REPLACING FIRST "XXX"
          BY LS-USER.
      MOVE AFID-PATH  TO W02-RECOVER.
            MOVE ZERO TO WS-LINES.
           IF WS-DRAW = ZERO
               MOVE 40 TO WS-DRAW.
             MOVE 3 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *      MOVE PAR-CRDX  TO WS-CARDEX.
      MOVE PAR-VAT-REG  TO W05-VATNO.
             OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
            MOVE 5 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-VAT-RATES  TO W05-VAT-RATES.
             MOVE 1 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-CSHEAD = SPACES
              MOVE "Setup cash slip headings"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO ZA205.
             MOVE PAR-DMY TO W12-TODAY.
             MOVE PAR-YMD TO W12-T-YMD.
             MOVE PAR-CSHEAD     TO W15-COMP W40-NAME.
             MOVE 6 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-WHOLESALE TO WS-WS.
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
      MOVE PAR-PETGL  TO WX-PETGL.
             ADD 150 WS-DRAW GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE W12-TODAY TO PAR-CSHDTE
             MOVE PAR-RECORD151 TO W85-RECORD151.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             MOVE 5 TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW       TO W85-PASS.
             MOVE 32 TO WS-S1.
             DISPLAY "Waiting for printer response" AT 1431
                      WITH FOREGROUND-COLOR 14 BLINK.
       ZA05.
           IF W40-NCH (WS-S1) = SPACE
               SUBTRACT 1 FROM WS-S1
               GO TO ZA05.
           IF WS-S1 > 16
               MOVE W85-NUL TO W15-EXPP1 W15-ECAN1
           ELSE
               MOVE W85-SO TO W15-EXPP1
               MOVE W85-CSO TO W15-ECAN1.
            MOVE W85-SO TO W15-EXPP4.
            MOVE W85-CSO TO W15-ECAN4.
           IF W85-CPRN = 1
               MOVE "LPT1" TO W02-PRINTER
           ELSE
           IF W85-CPRN = 2
               MOVE "LPT2" TO W02-PRINTER
           ELSE
           IF W85-CPRN = 3
               MOVE "LPT3" TO W02-PRINTER
           ELSE
           IF W85-CPRN = 4
               MOVE "COM1" TO W02-PRINTER
           ELSE
           IF W85-CPRN = 5
               MOVE "COM2" TO W02-PRINTER
           ELSE
           IF W85-CPRN = 9
        MOVE "CSH003.PRN" TO W02-PRINTER.
             OPEN OUTPUT CSHSLE.
      CALL X"91" USING X91-RES X91-FUN CSHSLE.
      GO TO ZA999.
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
       ZA58.
             MOVE 1 TO WS-S1.
            MOVE SPACES TO WS-MID-LINE WS-MID-LNE.
       ZA60.
             MOVE WS-G1          TO WS-TCHAR (WS-S1) WS-BCHAR (WS-S1).
             MOVE WS-G1          TO WS-TCHR (WS-S1)  WS-BCHR (WS-S1).
             MOVE WS-G1          TO WS-TCH (WS-S1)   WS-BCH (WS-S1).
             MOVE WS-G8          TO WS-TCR (WS-S1)   WS-BCR (WS-S1).
           IF WS-S1 < 80
               ADD 1 TO WS-S1
               GO TO ZA60.
             MOVE WS-G9          TO WS-TCR   (1).
             MOVE WS-G10         TO WS-TCR   (80).
             MOVE WS-G11         TO WS-BCR   (1).
             MOVE WS-G12         TO WS-BCR   (80).
             MOVE WS-G14         TO WS-TCHAR (1)  WS-BCHAR (1)
                                    WS-TCHR  (1)  WS-BCHR  (1)
                                    WS-TCH   (1)  WS-BCH   (1).
             MOVE WS-G13         TO WS-TCHAR (80) WS-BCHAR (80)
                                    WS-TCHR  (80) WS-BCHR  (80)
                                    WS-TCH   (80) WS-BCH   (80).
             MOVE WS-G2          TO WS-TCHAR (4)  WS-TCHAR (19)
                                    WS-TCHAR (50) WS-TCHAR (59)
                                    WS-TCHAR (69)
                                    WS-TCHR  (18) WS-TCHR  (49)
                                    WS-TCHR  (60) WS-TCHR  (70)
                                    WS-TCH   (7)  WS-TCH   (48)
                                    WS-TCH   (60) WS-TCH   (72).
             MOVE WS-G3          TO WS-MCHAR (4)  WS-MCHAR (19)
                                    WS-MCHAR (50) WS-MCHAR (59)
                                    WS-MCHAR (69) WS-MCHAR (80)
                                    WS-MCHAR (1)
                                    WS-MCH   (7)  WS-MCH   (48)
              WS-MCH   (60) WS-MCH   (72)
                                    WS-MCHR  (18) WS-MCHR  (49)
                                    WS-MCHR  (60) WS-MCHR  (70)
              WS-MCHR  (1)  WS-MCHR  (80).
             MOVE WS-G4          TO WS-BCHAR (4)  WS-BCHAR (19)
                                    WS-BCHAR (50) WS-BCHAR (59)
                                    WS-BCHAR (69)
                                    WS-BCHR  (18) WS-BCHR  (49)
                                    WS-BCHR  (60) WS-BCHR  (70)
                                    WS-BCH   (7)  WS-BCH   (48)
                                    WS-BCH   (60) WS-BCH   (72).
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "Supervisorฤ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
       ZA200.
             DISPLAY "Files locked - Try later" AT 2312
                      WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
             DISPLAY "Press any key" AT 2512
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
            ACCEPT WS-OPTION AT 2526
      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999.
             EXIT.
      /
       ZB000-ERROR      SECTION 92.

       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.
      
       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.
      
       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
             PERFORM DISPLAY-FILE-NAME.
      

       DISPLAY-FILE-NAME.
          IF WS-F-ERROR = 2
               MOVE W02-NETWORK TO WS-FILE
               MOVE WS-NETKEY TO WS-KEY
          ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
               MOVE WS-PARKEY    TO WS-KEY
           ELSE
           IF WS-F-ERROR = 18
               MOVE W02-RECOVER  TO WS-FILE
              MOVE WS-RECKEY  TO WS-KEY
          ELSE
          IF WS-F-ERROR = 37
              MOVE W02-SHARED  TO WS-FILE
              MOVE WS-SHARED  TO WS-KEY
          ELSE
           IF WS-F-ERROR = 40
               MOVE W02-LEDTRF   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE XFR-KEY  TO WS-KEYX.
           IF WS-STATUS = "10"
               MOVE "End of FILE" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "22"
               MOVE "Duplicate record number" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "23"
               MOVE "Invalid record number" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "24"
               MOVE "DISK full" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "30"
               MOVE "DISK error" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "94"
               MOVE "FILE locked" TO WS-STAT-MESSAGE.
             DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR 11.
             DISPLAY "Status " AT 1212
                      WS-STATUS WITH FOREGROUND-COLOR 11
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR 14.
           IF WS-STATUS NOT = "94"
               DISPLAY "Key    " AT 1412
                        WS-KEY WITH FOREGROUND-COLOR 11
                        " " WS-KEYX WITH FOREGROUND-COLOR 11
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
             STOP RUN.
