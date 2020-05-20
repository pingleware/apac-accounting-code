      $set LINKCOUNT"640" GNT"QTP002.GNT"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******      ****      ****     ******   *
      *   **    **     **     **    **    **  **    **  **   **    **  *
      *   **    **     **     **    **   **    **  **    **       **   *
      *   **    **     **     *******    **    **  **    **     **     *
      *   **   ***     **     **         **    **  **    **    **      *
      *    ******      **     **          **  **    **  **   **        *
      *         **     **     **           ****      ****    *******   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *      A M E N D   Q U O T A T I O N                             *
      *                                                                *
      *       Version 9.00.82 - April 2016                             *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     QTP002.
       AUTHOR.         J.W.LEMMON (APAC).
       DATE-WRITTEN.   AUGUST 1991.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1991 - 2016
                    by James William Lemmon.
                      (Id No. 4412165050082).

                All rights reserved.

                e-mail jwlemmon@gmail.com.

       SECURITY.
                This program is free software; you can redistribute
                it and/or modify it under the terms of the GNU General
                Public License as published by the Free Software
                Foundation; either version 3 of the License, or (at
                your option) any later version.

                This program is distributed in the hope that it will
                be useful, but WITHOUT ANY WARRANTY; without even the
                implied warranty of MERCHANTABILITY or FITNESS FOR A
                PARTICULAR PURPOSE. See the GNU General Public License
                for more details.

                You should have received a copy of the GNU General
                Public License along with this program. If not, see
                <http://www.gnu.org/licenses/>.

       INSTALLATION.          APAC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               CURSOR IS CSTART
                       CONSOLE IS CRT
                       CRT STATUS IS KEY-STATUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CONTROL.SL.

       COPY DEBTOR.SL.

       COPY DEPART.SL.

       COPY PARAM.SL.

       COPY RECOVER.SL.

       COPY SALE.SL.

       COPY SHARED.SL.

       COPY SORDER.SL.

       COPY SPARTS.SL.

       COPY STKMEM.SL.

       COPY STOCK.SL.

       COPY SYSUSE.SL.

      /
       DATA DIVISION.
       FILE SECTION.

       COPY CONTROL.FDE.

       COPY DEBTOR.FDE.

       COPY DEPART.FDE.

       COPY PARAM.FDE.

       COPY RECOVER.FD.

       COPY SALE.FD.

       COPY SHARED.FDE.

       COPY SORDER.FDE.

       COPY SPARTS.FDE.

       COPY STKMEM.FDE.

       COPY STOCK.FDE.

       COPY SYSUSE.FDE.

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-DISC    PIC 99V99  COMP-3.
       77  WS-TDISC        PIC  99V99    COMP-3.
       77  WS-CASH-AC      PIC  X(06).
       77  WS-SUB          PIC  9(04)    COMP-5.
       77  WS-SUB-6        PIC  9(04)    COMP-5.
       77  WS-SUPER        PIC  9(02)    COMP-5.
       77  WS-P1    PIC 9(04)  COMP-5.
       77  WS-S1           PIC  9(04)    COMP-5.
       77  WS-S2           PIC  9(04)    COMP-5.
       77  WS-S3           PIC  9(04)    COMP-5.
       77  WS-S4           PIC  9(04)    COMP-5.
       77  WS-S5           PIC  9(04)    COMP-5.
       77  WS-S6           PIC  9(04)    COMP-5.
       77  WS-S7           PIC  9(04)    COMP-5.
       77  WS-S8           PIC  9(04)    COMP-5.
       77  WS-S9           PIC  9(04)    COMP-5.
       77  WS-IXS          PIC  9(04)    COMP-5.
       77  WS-IXS1         PIC  9(04)    COMP-5.
       77  WS-IXS2         PIC  9(04)    COMP-5.
       77  WS-IXS3         PIC  9(04)    COMP-5.
       77  WS-IXS4         PIC  9(04)    COMP-5.
       77  WS-PARKEY       PIC  9(04)    COMP-5.
       77  WS-ENQPOS    PIC 9(06)  COMP-5.
       77  WS-ENQEND       PIC  9(06)    COMP-5.
       77  WS-KEYSTR       PIC  9(06)    COMP-5.
       77  WS-DBTKEY       PIC  9(06)    COMP-5.
       77  WS-SALSTR       PIC  9(04)    COMP-5.
       77  WS-SALKEY       PIC  9(04)    COMP-5.
       77  WS-NETKEY       PIC  9(04)    COMP-5.
       77  WS-AUDKEY       PIC  9(06)    COMP-5.
       77  WS-RECKEY       PIC  9(06)    COMP-5.
       77  WS-RECOVER      PIC  9(06)    COMP-5.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
       77  WS-PRN1    PIC 9(01) VALUE 0.
       77  WS-PRN2         PIC  9(01) VALUE 0.
       77  WS-USE-PACKS    PIC X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-START    PIC 9(08).
       77  WS-END          PIC  9(08).
       77  WS-TME2         PIC  9(08).
       77  WS-PORT         PIC  9(05) VALUE 1004.
       77  WS-OPEN         PIC  X(01) VALUE X"01".
       77  WS-CLOSE        PIC  X(01) VALUE X"00".
       77  WS-PAR          PIC  9(01) VALUE 0.
       77  WS-INO    PIC 9(03) VALUE 0.
       77  WS-LINES        PIC  9(02) VALUE 0.
       77  WS-FIRST    PIC 9(03) VALUE 0.
       77  WS-SALPOS    PIC 9(03) VALUE 0.
       77  WS-LAST    PIC 9(03) VALUE 0.
       77  WS-LASTINV      PIC  9(04)    COMP-5.
       77  WS-LINE         PIC  9(04)    COMP-5.
       77  WS-PAGE         PIC  9(02)    COMP-5.
       77  WS-L1           PIC  9(04)    COMP-5.
       77  WS-PGE-LENGTH   PIC 9(02)  VALUE 66.
       77  WS-PRN-LENGTH   PIC 9(02)  VALUE 62.
       77  WS-COND    PIC X(06).
       77  WS-NORM    PIC X(06).
       77  WS-EXPP    PIC X(06).
       77  WS-ECAN    PIC X(06).
       77  WS-8LPI    PIC X(06).
       77  WS-6LPI    PIC X(06).
       77  WS-10CPI    PIC X(06).
       77  WS-12CPI    PIC X(06).
       77  WS-17CPI    PIC X(06).
       77  WS-OPTION    PIC X(01).
       77  WS-IN-OPT       PIC  X(01).
       77  WS-PGADV        PIC  X(01) VALUE "Y".
       77  WS-SKIP         PIC  X(01) VALUE " ".
       77  WS-LOOK         PIC  X(01) VALUE "S".
       77  WS-DOC          PIC  X(01) VALUE "Q".
       77  WS-QTE          PIC  X(01) VALUE "N".
       77  WS-ETYPE        PIC  X(01) VALUE "I".
       77  WS-NUM          PIC  Z9.
       77  WS-ERROR        PIC  9(01).
       77  WS-COMP    PIC 9(01).
       77  WS-UPDATE       PIC  9(01) VALUE ZERO.
       77  WS-CONREC       PIC  9(01) VALUE ZERO.
       77  WS-HEAD         PIC  X(01).
       77  WS-TYPE         PIC  X(01).
       77  WS-AGE          PIC  X(01).
       77  WS-PRC          PIC  X(01).
       77  WS-MEASURE    PIC X(01) VALUE "N".
       77  PRG-NAME    PIC X(12) VALUE "STP\STPLOOK".
       77  WS-BATCH    PIC 9(05).
       77  WS-A     PIC X(19) VALUE "Any key to continue".
       77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
       77  WS-ERR1         PIC  X(22) VALUE "Invalid Account Number".
       77  WS-ERR2         PIC  X(09) VALUE "No Record".
       77  WS-PER          PIC  Z(02)9.99.
       77  WS-CR           PIC  X(01).
       77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
      /
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *      C = Clear comment line (50)
      *      E = Clear lines (any line or lines between 5 and 46)
      *       H = Change heading
      *       S = Clear the screen and display headings
      *
           03  CRT-TYPE            PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START           PIC  9(02) VALUE 5.
           03  CRT-END             PIC  9(02) VALUE 46.
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "QTP002".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "SALES - AMEND QUOTATIONS".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-WRKHD           PIC  X(11).
           03  CRT-WRKST           PIC  X(03).

       01  WS-DB-LINE.
          03  WS-TOP-LINE.
              05  WS-TCHAR PIC  X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÑÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍµ".
           03  WS-TOP-LNE2.
              05  WS-TCR   PIC  X(80) VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
          03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
              05  FILLER      PIC  X(23).
        05  WS-WRKHD    PIC  X(11).
              05  FILLER      PIC  X(01).
        05  WS-WRKST    PIC  X(03).
              05  FILLER      PIC  X(01).
          03  WS-MID-LINE.
              05  WS-MCHAR PIC  X(80) VALUE "³              ³   ³
      -        "                       ³        ³         ³          ³".
    03  WS-MINV REDEFINES WS-MID-LINE.
        05  WS-INV1     PIC  X(01).
        05  WS-INV78    PIC  X(78).
        05  WS-INV80    PIC  X(01).
          03  WS-MID-LNE2.
        05  FILLER      PIC  X(01) VALUE "³".
        05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
        05  FILLER      PIC  X(01) VALUE "³".
          03  WS-BOT-LINE.
              05  WS-BCHAR PIC  X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÏÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍµ".
          03  WS-BOT-LNE2.
              05  WS-BCR   PIC  X(80) VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
          03  WS-B-LINE REDEFINES WS-BOT-LNE2.
               05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
                  07  WS-SRT-LINE
                           PIC X(50).
                  07  FILLER
                           PIC X(10).
               05  FILLER  PIC X(19).
    03  WS-MM-LINE.
        05  FILLER      PIC  X(01) VALUE "³".
        05  WS-SPACE    PIC  X(60) VALUE SPACES.
        05  FILLER      PIC  X(01) VALUE "³".
    03  WS-HME-LINE PIC X(60) VALUE ALL "Í".
          03  WS-TOP-LNE.
              05  WS-TCHR  PIC  X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍµ".
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
           03  WS-MID-LNE.
              05  WS-MCHR  PIC  X(80) VALUE "³                ³
      -        "                      ³          ³         ³         ³".
    03  WS-MST-LNE REDEFINES WS-MID-LNE.
        05  WS-MST1     PIC  X(01).
        05  WS-MST78    PIC  X(78).
        05  WS-MST80    PIC  X(01).
          03  WS-BOT-LNE.
              05  WS-BCHR  PIC  X(80) VALUE "ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍµ".
          03  WS-TP-LNE.
              05  WS-TCH   PIC  X(80) VALUE "ÆÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍµ".
           03  WS-MD-LNE.
              05  WS-MCH   PIC  X(80) VALUE "³      ³
      -        "                      ³           ³          ³       ³".
    03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
        05  WS-MDT1     PIC  X(01).
        05  WS-MDT78    PIC  X(78).
        05  WS-MDT80    PIC  X(01).
          03  WS-BT-LNE.
              05  WS-BCH   PIC  X(80) VALUE "ÆÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍµ".
          03  WS-TP-LINE.
               05  WS-TCA  PIC X(01) OCCURS 80.
           03  WS-MD-LINE.
               05  WS-MCA  PIC X(01) OCCURS 80.
    03  WS-SO78-L REDEFINES WS-MD-LINE.
        05  WS-MSO1     PIC  X(01).
        05  WS-MSO78    PIC  X(78).
        05  WS-MSO80    PIC  X(01).
          03  WS-BT-LINE.
               05  WS-BCA  PIC X(01) OCCURS 80.

       01  WS-ENTRY.
           03  WS-ENT0     PIC  X(08).
           03  WS-TYPE0    PIC  X(01).
           03  WS-KEY0     PIC  9(06)    COMP-5.
       01  WS-ENTRY1.
           03  WS-ENT1     PIC  X(08).
           03  WS-TYPE1    PIC  X(01).
           03  WS-KEY1     PIC  9(06)    COMP-5.

       COPY WS.WS.

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "QTP".
    03  WS-PROG        PIC  X(03) VALUE "002".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  WS-LOCAL-ID.
    03  WS-LOCAL-DRIVE  PIC  X(01).
    03  FILLER        PIC  X(14) VALUE ":\APACSHR\SORD".
    03  WS-USER-ID      PIC  X(03).
    03  FILLER        PIC  X(04) VALUE ".ORD".

       01  WS-LOCAL-ID2.
    03  WS-LOCAL-DRIVE2 PIC  X(01).
    03  FILLER        PIC  X(19) VALUE ":\APACSHR\USERS.DAT".

       01  WS-SORDER-STR.
    03  FILLER        PIC  X(64).

       01  W02-FID.

       COPY PRNDISK.ID.

       01  W02-PRINTER-DETAILS.
    03  W02-PRINTER     PIC  X(12).
          03  W02-PGE-LENGTH  PIC  9(02).
          03  W02-PRN-LENGTH  PIC  9(02).
    03  W02-LINAGE      PIC  9(02).
    03  W02-PRN-STATUS  PIC  X(01) VALUE "C".
      *
      *    ****    D  =  Detail line
      *     C  =  Condensed print
      *     E  =  Expanded print
      *     H  =  Header line
      *     X  =  Cancel expanded print
      *     1  =  10 Characters per inch
      *     2  =  12 Characters per inch
      *     3  =  17 Characters per inch
      *     6  =  6 Lines per inch
      *     8  =  8 Lines per inch
      *
    03  W02-PRN-TYPE    PIC  X(01).
    03  W02-PRN-LINE    PIC  X(136).

          03  REP-L1 REDEFINES W02-PRN-LINE.
              05  REP-DETAIL1    PIC X(80).

          03  REP-L2 REDEFINES W02-PRN-LINE.
              05  REP-DETAIL2    PIC X(96).

          03  REP-L2A REDEFINES W02-PRN-LINE.
        05  FILLER    PIC X(04).
        05  REP-COMPANY    PIC X(92).

          03  REP-L3 REDEFINES W02-PRN-LINE.
        05  FILLER    PIC X(08).
              05  REP-NAME    PIC X(40).
        05  FILLER    PIC X(15).
        05  REP-INFO.
           07  REP-AC    PIC X(13).
           07  REP-ACNO    PIC X(06).
     07  FILLER    PIC X(06).
        05  REP-INFO2 REDEFINES REP-INFO.
           07  REP-DATE    PIC X(11).
           07  REP-DTE    PIC Z9/99/9999.
     07  FILLER    PIC X(04).
        05  REP-INFO3 REDEFINES REP-INFO.
           07  REP-ORD    PIC X(11).
     07  REP-ORDNO   PIC X(14).
        05  FILLER    PIC X(08).

          03  REP-L4 REDEFINES W02-PRN-LINE.
        05  FILLER    PIC X(07).
              05  REP-H1    PIC X(26).
              05  REP-REF    PIC 9(08).
              05  FILLER    PIC X(55).

          03  REP-L5 REDEFINES W02-PRN-LINE.
        05  FILLER    PIC X(03).
              05  REP-H2    PIC X(19).
              05  REP-H3    PIC X(32).
              05  REP-H4    PIC X(09).
              05  REP-H5    PIC X(14).
        05  REP-H6    PIC X(08).
        05  REP-H7    PIC X(05).
        05  FILLER    PIC X(02).

          03  REP-L6 REDEFINES W02-PRN-LINE.
        05  FILLER    PIC X(03).
        05  REP-EXT-ITEM.
           07  REP-ITM1    PIC X(14).
     07  REP-SL    PIC X(01).
     07  REP-ITM    PIC X(03).
        05  FILLER    PIC X(01).
              05  REP-DESC    PIC X(30).
        05  FILLER    PIC X(01).
              05  REP-QNT    PIC Z(05)9.99.
              05  FILLER    PIC X(01).
        05  REP-NC.
     07  REP-PRICE   PIC Z(05)9.99.
        05  FILLER    PIC X(01).
        05  REP-VAT    PIC Z(06)9.99.
        05  FILLER    PIC X(01).
        05  REP-VAL    PIC Z(06)9.99.
        05  FILLER    PIC X(02).

    03  REP-L7 REDEFINES W02-PRN-LINE.
        05  FILLER    PIC X(11).
        05  REP-SMAN    PIC X(30).
        05  FILLER    PIC X(01).
        05  REP-H8    PIC X(10).
        05  REP-TERMS    PIC X(28).

    03  REP-L8 REDEFINES W02-PRN-LINE.
        05  FILLER    PIC X(40).
        05  REP-TEL    PIC X(05).
        05  REP-TELNO    PIC X(16).
        05  FILLER    PIC X(35).

    03  REP-L9 REDEFINES W02-PRN-LINE.
        05  FILLER    PIC X(08).
        05  REP-TEXT    PIC X(63).
        05  FILLER    PIC X(09).

       COPY W05.VAT.

       01  W05-INV.
           03  W05-ENTRY       PIC  ZZ9.
           03  FILLER          PIC  X(01) VALUE SPACES.
          03  W05-DATE        PIC  Z9/99/9999.
           03  FILLER          PIC  X(01) VALUE SPACES.
           03  W05-TYPE        PIC  X(03).
           03  FILLER          PIC  X(03) VALUE SPACES.
           03  W05-REF         PIC  9(07).
           03  FILLER          PIC  X(01) VALUE SPACES.
           03  W05-VALUE       PIC  Z(06)9.99-.

       COPY W10.SAL.

       COPY W12.WS.

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

       01  W14-DELETE.
           03  W14-SUB         PIC 9(03)     COMP-3.
           03  W14-SQN         PIC 9(05)     COMP-3 OCCURS 200.

       01  W15-DISCOUNT.
           03  W15-DISC        PIC S9(03)V99 COMP-3 OCCURS 10.

       COPY W20.WS.

       01  W25-CALCS.
           03  W25-RESULT      PIC 9(05)V99.
           03  W25-RESULT1 REDEFINES W25-RESULT.
               05  W25-WHOLE   PIC 9(03).
               05  W25-DECIMAL PIC 9(02).
           03  W25-RESULT2 REDEFINES W25-RESULT.
               05  W25-KEY     PIC 9(04).
               05  W25-SUB     PIC 9(01).
               05  FILLER      PIC 9(02).
           03  W25-TOTAL       PIC 9(07)V99  COMP-3.
           03  W25-VALUE       PIC S9(07)V99 COMP-3.
           03  W25-CREDIT      PIC S9(07)V99 COMP-3.
           03  W25-TIME        PIC 9(08).
           03  W25-TRED REDEFINES  W25-TIME.
               05  W25-HH      PIC 9(02).
               05  W25-MM      PIC 9(02).
               05  W25-SS      PIC 9(02).
               05  FILLER      PIC 9(02).

       01  W30-MESSAGE.
           03  W30-REMARKS OCCURS 4
                               PIC  X(63).

       01  W32-MESSAGE.
    03  W32-REMARKS OCCURS 10
          PIC  X(63).

       01  W34-HEADER.
    03  W34-FORMAT      PIC  X(01).
    03  W34-MARGIN      PIC  9(02) COMP-5.
    03  W34-SALUTATION  PIC  X(63).
    03  W34-SIGNATURE   PIC  X(20).

       01  W35-DETKEY.
           03  W35-KEY.
             05  W35-REF       PIC  X(08).
             05  W35-SEQN.
                 07  W35-SEQ   PIC  9(03).
                 07  W35-SUB   PIC  9(02).
             05  W35-SQN REDEFINES W35-SEQN
                               PIC  9(05).

       COPY W40.DBT.

       COPY W40.WS.

       COPY FUNCTION.WS.

       COPY W50.WS.

       01  W55-QUOTE.
           03  W55-DSCNT       PIC S9(07)V99 COMP-3 VALUE ZERO.
           03  W55-TTAX        PIC S9(07)V99 COMP-3 VALUE ZERO.
           03  W55-SUBTOT      PIC S9(07)V99 COMP-3 VALUE ZERO.
           03  W55-VALUE       PIC S9(07)V99 COMP-3 VALUE ZERO.

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR OCCURS 40 PIC X(01).
           03  W60-ADDRESS.
               05  W60-ACHAR OCCURS 70 PIC X(01).
           03  W60-ADDR REDEFINES W60-ADDRESS.
               05  W60-ADD     PIC X(60).
               05  FILLER      PIC X(10).
           03  W60-ADDRESS1.
               05  W60-PAD     PIC X(60).
               05  FILLER      PIC X(10).

       COPY W70DEBT.IWS.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
           03  W75-DNO        PIC X(06) OCCURS 18.
           03  W75-ITEMS OCCURS 18.
               05  W75-CODE   PIC X(14).
               05  W75-SELL   PIC 9(07)V99 COMP-3.
               05  W75-VSELL  PIC 9(07)V99 COMP-3.
               05  W75-CASH   PIC 9(07)V99 COMP-3.
               05  W75-VCASH  PIC 9(07)V99 COMP-3.
               05  W75-WSALE  PIC 9(07)V99 COMP-3.
               05  W75-VWSALE PIC 9(07)V99 COMP-3.
           03  W75-SORD       PIC 9(08) OCCURS 18.
       01  W75-TITLES.
           03  W75-DESCRIP.
               05  FILLER      PIC X(28)      VALUE
                           "Mr     Mnr    Mrs    Mev    ".
               05  FILLER      PIC X(28)      VALUE
                           "Miss   Mej    Ms     Ms     ".
               05  FILLER      PIC X(28)      VALUE
                           "Dr     Dr     Rev    Ds     ".
               05  FILLER      PIC X(28)      VALUE
                           "Prof   Prof   The HonSy Edel".
               05  FILLER      PIC X(28)      VALUE
                           "                            ".
           03  W75-CODES REDEFINES W75-DESCRIP.
               05  W75-TITLE  OCCURS 10.
                   07  W75-ETITLE PIC X(07).
                   07  W75-ATITLE PIC X(07).
       01  W80-ACCOUNT.
           03  W80-ACNO        PIC X(06).
           03  W80-CRED REDEFINES W80-ACNO.
               05  W80-CNO     PIC 9(05).
               05  W80-CAC REDEFINES W80-CNO.
                   07  W80-CTYPE PIC 9(01).
                   07  W80-SER   PIC 9(04).
               05  FILLER        PIC 9(01).
           03  W80-TYPE        PIC 9(01).
       01  W85-PASS.
           03  W85-SUPER       PIC  X(06)    OCCURS 9.
           03  W85-ENTRY       PIC  9(02)    COMP-3.
           03  W85-MARG        PIC S9(02)V99.
       01  W85-BATCHES.
               05  W85-BATCH   PIC 9(05).

       COPY W90.SAL.

       01  W95-STM.
           03  W95-COMP        PIC X(40).
           03  W95-ADD1        PIC X(30).
           03  W95-ADD2        PIC X(30).
           03  W95-ADD3        PIC X(30).
           03  W95-ADD4        PIC X(30).
           03  W95-PC.
               05  W95-POST    PIC 9(04).
           03  W95-TEL         PIC X(14).
           03  W95-V1.
               05  W95-T       PIC Z(04)9.99.
           03  W95-V2.
               05  W95-B       PIC Z(06)9.99-.
           03  W95-D.
              05  W95-DTE     PIC Z9/99/9999.
           03  W95-TIME.
               05  W95-HH      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-MM      PIC 99.
               05  FILLER      PIC X(01) VALUE ":".
               05  W95-SS      PIC 99.
       01  W100-EDIT.
           03  W100-LBAL       PIC Z(06)9.99-.
    03  W100-LNGTH REDEFINES W100-LBAL.
        05  FILLER      PIC  X(02).
        05  W100-LENGTH PIC  ZZZ9.9999.
          03  W100-LARR       PIC  Z(06)9.99.
    03  W100-BRDTH REDEFINES W100-LARR.
        05  FILLER      PIC  X(01).
        05  W100-BREADTH PIC ZZZ9.9999.
          03  W100-BAL        PIC Z(08)9.99-.
           03  W100-CUR        PIC Z(08)9.99-.
           03  W100-30         PIC Z(08)9.99-.
           03  W100-60         PIC Z(08)9.99-.
           03  W100-90         PIC Z(08)9.99-.
           03  W100-120        PIC Z(08)9.99-.
           03  W100-INT        PIC Z(08)9.99-.
           03  W100-CRL        PIC Z(06)9.
    03  W100-NMB REDEFINES W100-CRL.
        05  FILLER      PIC  X(02).
        05  W100-NUMBER PIC  ZZZ9.
          03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
           03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
           03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.
           03  W100-V5 REDEFINES W100-V2.
               05  W100-QNT    PIC Z(05)9.99-.

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
       SCREEN SECTION.

       COPY BLANK.CRT.

       COPY QTELUP.CRT.

       01  S07.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
     PIC X(14) USING W10-ITEM AUTO.

       01  S07-ITM.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC X(14) USING W10-ITEM AUTO.
    03        COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC X(03) USING W10-ITM AUTO.

       01  S07-EXT.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC X(18) USING W10-EXT-ITEM AUTO.

       01  S07-3.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(03) USING W10-I4 AUTO.

       01  S07-4.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(04) USING W10-I4 AUTO.

       01  S07-5.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(05) USING W10-I5 AUTO.

       01  S07-6.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(06) USING W10-I6 AUTO.

       01  S07-7.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(07) USING W10-I7 AUTO.

       01  S07-8.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(08) USING W10-I8 AUTO.

       01  S07-9.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(09) USING W10-I9 AUTO.

       01  S07-10.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(10) USING W10-I10 AUTO.

       01  S07-11.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(11) USING W10-I11 AUTO.

       01  S07-12.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(12) USING W10-I12 AUTO.

       01  S07-13.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(13) USING W10-I13 AUTO.

       01  S07-14.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(14) USING W10-I14 AUTO.

       01  S26-QNT.
          03        COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
                                 PIC Z(04)9.99 USING W10-QUANT AUTO.

       01  S26-PRC.
          03        COLUMN 60 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(05)9.99 USING W10-SELL AUTO.

       01  S26-DSC.
          03        COLUMN 30 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z9.99 USING WS-DISC AUTO.

       01  S26-TDSC.
          03        COLUMN 15 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
                                 PIC Z9.99 USING WS-TDISC AUTO.

       01  S26-AVGE.
          03        COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC Z(04)9.999 USING STK-AVGE AUTO.

       01  S28.
          03  LINE 25 COLUMN 65 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZZ9 USING WS-SALKEY AUTO.

       01  S30.
          03  LINE  2 COLUMN 36 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "QUOTATION".
          03  LINE  3 COLUMN  2 VALUE "AC No".
          03        COLUMN 15 VALUE "Name".
    03        COLUMN 20 BACKGROUND-COLOR 5
     PIC  X(40) FROM WS-SPACE.
          03        COLUMN 61 VALUE "Ord No°".
    03        COLUMN 68 BACKGROUND-COLOR 5
     PIC  X(12) FROM WS-SPACE.
          03  LINE  4 COLUMN  2 VALUE "Assisted".
    03        COLUMN 11 BACKGROUND-COLOR 5
     PIC  X(02) FROM WS-SPACE.
    03        COLUMN 61 VALUE "Tel.".
    03        COLUMN 66 BACKGROUND-COLOR 5
     PIC  X(14) FROM WS-SPACE.
          03  LINE  5 COLUMN  2 VALUE "Ref. No".
    03        COLUMN 10 BACKGROUND-COLOR 0
     PIC  X(08) FROM WS-SPACE.
          03        COLUMN 20 VALUE "Contact".
    03        COLUMN 29 BACKGROUND-COLOR 5
     PIC  X(14) FROM WS-SPACE.
          03        COLUMN 47 VALUE "Ref No".
    03        COLUMN 54 BACKGROUND-COLOR 5
     PIC  X(09) FROM WS-SPACE.
          03        COLUMN 65 VALUE "Date".
    03        COLUMN 70 BACKGROUND-COLOR 5
     PIC  X(10) FROM WS-SPACE.
          03  LINE  6 COLUMN  2 REVERSE-VIDEO VALUE "Item Code      I".
          03        COLUMN 18 REVERSE-VIDEO VALUE "TM Description  ".
           03          COLUMN 33 REVERSE-VIDEO VALUE "                ".
           03          COLUMN 49 REVERSE-VIDEO VALUE "  Quantity ".
           03          COLUMN 60 REVERSE-VIDEO VALUE "Unit Price  ".
           03          COLUMN 72 REVERSE-VIDEO VALUE "Extended".
          03  LINE  7 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
          03  LINE 19 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
          03  LINE 20 COLUMN  3 VALUE "Sub Total ".
          03  LINE 21 COLUMN  3 VALUE "Discount  ".
           03  LINE 22 COLUMN  3 VALUE "T A X -VAT".
           03  LINE 23 COLUMN  3 VALUE "AMOUNT DUE".

       01  S30A.
    03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC Z(08) USING W10-SORD AUTO.

       01  S31.
          03  LINE  2 COLUMN 33 FOREGROUND-COLOR 7 HIGHLIGHT
                                 VALUE "AMEND QUOTATION".

       01  S36.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(06) USING W80-ACNO AUTO.

       01  S40.
    03  LINE  8 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Ú".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 PIC X(50) USING WS-SRT-LINE.
    03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "¿".
          03  LINE  9 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     PIC  X(18) FROM WS-SPACE.
    03        COLUMN 34 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "CLIENT DETAILS".
    03        COLUMN 48 BACKGROUND-COLOR 2
     PIC  X(18) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE 10 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "Name    >".
    03        COLUMN 65 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "<".
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE 11 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "Address >".
    03        COLUMN 55 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "<".
    03        COLUMN 56 BACKGROUND-COLOR 2
     PIC  X(10) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE 12 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     PIC  X(08) FROM WS-SPACE.
    03        COLUMN 24 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE ">".
    03        COLUMN 55 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "<".
    03        COLUMN 56 BACKGROUND-COLOR 2
     PIC  X(10) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE 13 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     PIC  X(08) FROM WS-SPACE.
    03        COLUMN 24 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE ">".
    03        COLUMN 55 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "<".
    03        COLUMN 56 BACKGROUND-COLOR 2
     PIC  X(10) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE 14 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     PIC  X(08) FROM WS-SPACE.
    03        COLUMN 24 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE ">".
    03        COLUMN 55 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "<".
    03        COLUMN 56 BACKGROUND-COLOR 2
     PIC  X(10) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE 15 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     PIC  X(08) FROM WS-SPACE.
    03        COLUMN 24 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE ">".
    03        COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "<".
    03        COLUMN 30 BACKGROUND-COLOR 2
     PIC  X(36) FROM WS-SPACE.
          03        COLUMN 66 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE 16 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "Tel.    >".
    03        COLUMN 39 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "<".
    03        COLUMN 40 BACKGROUND-COLOR 2
     PIC  X(26) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE 17 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "Id.No.  >".
    03        COLUMN 39 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "<".
    03        COLUMN 40 BACKGROUND-COLOR 2
     PIC  X(26) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03  LINE 18 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "À".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 PIC X(50) USING WS-SRT-LINE.
    03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Ù".

       01  S41.
    03  LINE 10 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(40) USING W40-NM1 AUTO.
    03  LINE 11 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(30) USING W40-AD1 AUTO.
    03  LINE 12 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(30) USING W40-AD2 AUTO.
    03  LINE 13 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(30) USING W40-AD3 AUTO.
    03  LINE 14 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(30) USING W40-AD4 AUTO.
    03  LINE 15 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(04) USING W40-PC AUTO.
    03  LINE 16 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(14) USING W40-HTEL AUTO.
    03  LINE 17 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
                                 PIC X(14) USING W40-ID AUTO.

       01  S42.
    03  LINE  9 COLUMN  9 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7
     PIC X(63) USING W34-SALUTATION AUTO.
    03  LINE 10 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
     PIC X(63) USING W32-REMARKS (1) AUTO.
    03  LINE 11 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
     PIC X(63) USING W32-REMARKS (2) AUTO.
    03  LINE 12 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
     PIC X(63) USING W32-REMARKS (3) AUTO.
    03  LINE 13 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
     PIC X(63) USING W32-REMARKS (4) AUTO.
    03  LINE 14 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
     PIC X(63) USING W32-REMARKS (5) AUTO.
    03  LINE 15 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
     PIC X(63) USING W32-REMARKS (6) AUTO.
    03  LINE 16 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
     PIC X(63) USING W32-REMARKS (7) AUTO.
    03  LINE 17 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
     PIC X(63) USING W32-REMARKS (8) AUTO.
    03  LINE 18 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
     PIC X(63) USING W32-REMARKS (9) AUTO.
    03  LINE 19 COLUMN  9 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
     PIC X(63) USING W32-REMARKS (10) AUTO.
    03  LINE 20 COLUMN 21 FOREGROUND-COLOR 0 HIGHLIGHT
     BACKGROUND-COLOR 7
     PIC X(20) USING W34-SIGNATURE AUTO.

       01  S-MEASURE.
    03  LINE  4 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Ú".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     PIC X(19) USING WS-SRT-LINE.
    03        COLUMN 35 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "¿".
          03  LINE  5 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     PIC  X(04) FROM WS-SPACE.
    03        COLUMN 20 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "MEASUREMENTS".
    03        COLUMN 32 BACKGROUND-COLOR 2
     PIC  X(03) FROM WS-SPACE.
          03        COLUMN 35 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE  6 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Length  >".
    03        COLUMN 34 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
          03        COLUMN 35 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE  7 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Breadth >".
    03        COLUMN 34 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
          03        COLUMN 35 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
          03  LINE  8 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Pieces  >    <".
          03        COLUMN 30 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "     ³".
    03  LINE  9 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "À".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     PIC X(19) USING WS-SRT-LINE.
    03        COLUMN 35 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Ù".

       01  SA-MEASURE.
    03  LINE  6 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC ZZZ9.9999 USING W10-LENGTH AUTO.
    03  LINE  7 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC ZZZ9.9999 USING W10-BREADTH AUTO.
    03  LINE  8 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7 HIGHLIGHT
     PIC ZZZ9 USING W10-NUMBER AUTO.

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

       COPY S99.CRT.

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000-MAIN        SECTION.
       AA000-INIT.
          IF LS0-SALEV < 1
        MOVE "Not Authorised"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
       AA05.
            PERFORM BD000.
    IF WS-OPTION = "Q"
        GO TO AA10.
          IF WS-OPTION = "Y" OR "P"
        PERFORM BE000
               DISPLAY "Printing quotation" AT 2502
         WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 6 HIGHLIGHT
               PERFORM BB200.
             OPEN OUTPUT SALE.
             CLOSE SALE.
             OPEN I-O SALE.
            MOVE "ENTER to continue - E to exit"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "E")
              GO TO AA05.
       AA10.
            CLOSE RECOVER
     SALE.
    IF W70-ORD-SYS = "Y"
        IF WS-LOCAL-DRIVE = "C"
     CLOSE SORDER
    SYSUSER
     MOVE WS-SORDER-STR
     TO W02-SORDER
     OPEN I-O SORDER.

       AA49.
             EXIT PROGRAM.

       COPY AA50SO.LUP.

       COPY FUNCTION.CRT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     OPT-MESSAGE          ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'.           ³
      *    ³              ³
      *    ³ The option request must be placed in WS-ERR-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³              ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      ³
      *    ³   PERFORM OPT-MESSAGE (Use this format if function or any ³
      *    ³        other keys are to be tested on exit)        ³
      *    ³        OR           ³
      *    ³   PERFORM OPT-MESSAGE UNTIL WS-OPTION = "N" OR "Y".       ³
      *    ³   (Use TEST AFTER if WS-OPTION is preloaded with a        ³
      *    ³   default option)            ³
      *    ³              ³
      *    ³ This would be displayed as:          ³
      *    ³ ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿     ³
      *    ³ ³    Print transactions (Y/N) [ ]   ³°°   ³
      *    ³ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°°   ³
      *    ³   °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°   ³
      *    ³              ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³              ³
      *    ³ To display the request message higher or lower (default   ³
      *    ³ is line 13) move the line number to be used to SLIN and   ³
      *    ³ then PERFORM OPT-SETUP THRU OPT-EXIT.         ³
      *    ³ (The UNTIL and TEST AFTER options also apply here)        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       AA300              SECTION.
       AA301.
            DISPLAY "F1" AT 2502
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 6 HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3
          FOREGROUND-COLOR 6 HIGHLIGHT
             "=Item,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F3" WITH BACKGROUND-COLOR 3
          FOREGROUND-COLOR 6 HIGHLIGHT
             "=Alt,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F4" WITH BACKGROUND-COLOR 3
          FOREGROUND-COLOR 6 HIGHLIGHT
             "=Desc,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F5" WITH BACKGROUND-COLOR 3
          FOREGROUND-COLOR 6 HIGHLIGHT
             "=Desc2,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F6" WITH BACKGROUND-COLOR 3
          FOREGROUND-COLOR 6 HIGHLIGHT
             "=X/Ref,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F9" WITH BACKGROUND-COLOR 3
          FOREGROUND-COLOR 6 HIGHLIGHT
             "=Word,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "Esc" WITH BACKGROUND-COLOR 3
    FOREGROUND-COLOR 6 HIGHLIGHT
       "ape to end"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       AA305.
             EXIT.
      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       AA400              SECTION.
       AA401.
            DISPLAY "F1" AT 2501
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 6 HIGHLIGHT
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3
          FOREGROUND-COLOR 6 HIGHLIGHT
             " Quote no./ "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F3" WITH BACKGROUND-COLOR 3
          FOREGROUND-COLOR 6 HIGHLIGHT
             " Account no./ "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F4" WITH BACKGROUND-COLOR 3
          FOREGROUND-COLOR 6 HIGHLIGHT
       " From specified date Lookup"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       AA405.
             EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY CONTROL.RD.

       COPY DEBTOR.RD.

       COPY DEPART.RD.

       COPY PARAM.RD.

       COPY SALE.RD.

       COPY SHARED.RD.

       COPY SORDER.RD.

       COPY SPARTS.RD.

       COPY STKMEM.RD.

       COPY STOCK.RD.

       COPY SYSUSE.RD.

      /
       AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY SALE.WR.

       COPY SORDER.WR.

       COPY SORDER.LUP.

       COPY APAC.HLP.

       COPY PRINTUTL.AS9.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
       AY000           SECTION.
       AY17.
             MOVE 17             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE SOR-REC1       TO REC-SORDER.
             GO TO AY50.

       AY39.
             MOVE 39             TO REC-FILE.
             MOVE WS-NETKEY      TO REC-KEY.
             MOVE NET-RECORD     TO REC-NETWORK.
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
         WITH FOREGROUND-COLOR 6 HIGHLIGHT
         WS-STATUS WITH FOREGROUND-COLOR 7 HIGHLIGHT
               STOP RUN.

       AY59.
             EXIT.
      *
      *    ****    Start processing transaction
      *
       AY60.
          IF WS-SKIP = "Y" OR "A"
               GO TO AY999.
            MOVE 1   TO WS-COUNT.
            MOVE 5   TO WS-SHARED.
            PERFORM READ-SHARED-LOCK THRU READ-SHARED-EXIT.
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
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 4 HIGHLIGHT.
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
        FOREGROUND-COLOR 6 HIGHLIGHT BLINK
        ACCEPT WS-STIME FROM TIME
        PERFORM LOCK-REC-LOOP
        DISPLAY "Waiting" AT 2572
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 6 HIGHLIGHT BLINK
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
            MOVE 1   TO WS-NETKEY.
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

       COPY CA000.STK.

       COPY CA100.STK.

      *
      *    ****    Read debtor record - using account number.
      *
       CA155-GET-DEBTOR  SECTION 2.
       CA155.
             MOVE W80-ACNO       TO DEB-ACNO.
             PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
               MOVE 1            TO WS-ERROR
           ELSE
           IF DEB-ACNO NOT = W80-ACNO
               MOVE 2            TO WS-ERROR
           ELSE
               MOVE 0            TO WS-ERROR
               MOVE DEB-TYPE     TO W80-TYPE.
       CA160-EXIT.
             EXIT.
      *
       CA165-NAME        SECTION 2.
       CA165.
             UNSTRING W60-NAME DELIMITED BY "*" OR "!"
                                 INTO W40-SURNAME
                                      W40-FNAME
                                      W40-SNAME
             OVERFLOW
                 MOVE 1          TO WS-ERROR.
       CA170-EXIT.
             EXIT.
      *
       CA175-ADDRESS     SECTION 2.
       CA175.
             UNSTRING W60-ADDRESS DELIMITED BY "*"
                                 INTO W40-ADD1
                                      W40-ADD2
                                      W40-ADD3
                                      W40-ADD4
             OVERFLOW
                 MOVE 1          TO WS-ERROR.
       CA175-EXIT.
             EXIT.
      *
       CA185-ADDRESS     SECTION 2.
       CA185.
             UNSTRING W60-ADDRESS1 DELIMITED BY "*"
                                 INTO W40-PADD1
                                      W40-PADD2
                                      W40-PADD3
                                      W40-PADD4
             OVERFLOW
                 MOVE 1          TO WS-ERROR.
       CA185-EXIT.
             EXIT.

       COPY CA200.STK.

       COPY CA500.STK.

      *
      *    ****   G E T   S O R D E R   R E C O R D   U S I N G   SOR-KEY.
      *
      *           WS-ERROR:
      *
      *                    1 = NO RECORD
      *                    2 = NOT QUOTE RECORD (ORDER OR INVOICE)
      *                    3 = QUOTE DETAILS INCOMPLETE
      *                    4 = INVALID ACCOUNT NUMBER
      *
       GET-SORDER           SECTION.

       GET-SORD-ORDNO.
             INITIALIZE W14-DELETE.
      MOVE 1   TO W14-SUB WS-S7.
             MOVE SPACES         TO W40-NM1 W40-AD1 W40-AD2
                                    W40-AD3 W40-AD4 W40-PC
                                    W35-KEY.
             MOVE W10-SORD       TO SOR-REF W90-RF.
             MOVE 100            TO SOR-SQN.
             MOVE ZERO           TO WS-ERROR.
      PERFORM READ-SORDER THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               MOVE 1            TO WS-ERROR
               GO TO GET-SORDER-EXIT.
           IF SOR-DOC NOT = WS-DOC
               MOVE 2            TO WS-ERROR
               GO TO GET-SORDER-EXIT.
             MOVE 1              TO WS-LAST WS-SALKEY.
             GO TO GET-SORD-TEST-TYPE.

       GET-SORD-NEXT.
      PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               MOVE 3            TO WS-ERROR
               GO TO GET-SORDER-EXIT.

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
               MOVE SOR-NAME     TO W40-NAME W40-NM1
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
                   MOVE SOR-COST     TO SAL-COST
                   MOVE SOR-SELL     TO SAL-SELL
                   MOVE SOR-VAT      TO SAL-VAT
           MOVE SOR-VAT-SUB  TO SAL-VAT-SUB
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
     PERFORM GET-SORD-WRITE-SALE
               ELSE
                   PERFORM GET-CHANGE-TO-REMKS
           ELSE
      *                      D I S C O U N T   D E T A I L S
           IF SOR-TYPE = 2
               INITIALIZE SAL-RECORD1
        MOVE SOR-EXT-ITEM TO SAL-EXT-ITEM
               MOVE SOR-DESC     TO SAL-MSER
               MOVE SOR-QNT      TO SAL-QNT
               MOVE SOR-SQN      TO SAL-SQN
           ELSE
      *                      R E M A R K S
           IF SOR-TYPE = 3
               INITIALIZE SAL-RECORD1
               MOVE SOR-SERIAL   TO SAL-MSER
               MOVE SOR-SQN      TO SAL-SQN
               PERFORM GET-SORD-WRITE-SALE
           ELSE
      *                      I N T E R N A L   N O T E S
           IF SOR-TYPE = 4
               INITIALIZE SAL-RECORD1
               MOVE SOR-DESC     TO SAL-MSER
               MOVE SOR-SQN      TO SAL-SQN
               MOVE 1            TO SAL-SUPER
               PERFORM GET-SORD-WRITE-SALE
           ELSE
      *                      A D D R E S S
           IF SOR-TYPE = 5
               MOVE SOR-KEY      TO W35-KEY
               MOVE SOR-AD1      TO W40-AD1
               MOVE SOR-AD2      TO W40-AD2
               MOVE SOR-AD3      TO W40-AD3
               MOVE SOR-AD4      TO W40-AD4
               MOVE SOR-PC       TO W40-PC
           ELSE
      *        S A L U T A T I O N
          IF SOR-TYPE = 7
        MOVE SOR-TEXT  TO W34-SALUTATION
        MOVE SOR-SIGN  TO W34-SIGNATURE
    ELSE
      *        T E X T
          IF SOR-TYPE = 8
        MOVE SOR-TEXT  TO W32-REMARKS (WS-S7)
        ADD 1   TO WS-S7
    ELSE
      *        T R A I L E R
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
      MOVE SOR-EXT-ITEM  TO STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.

       GET-CHANGE-TO-REMKS.
             INITIALIZE SAL-RECORD1.
      STRING SOR-EXT-ITEM DELIMITED BY SIZE
                    SOR-DESC DELIMITED BY SIZE
                                 INTO SAL-DESC.
             MOVE SOR-SQN        TO SAL-SQN W14-SQN (W14-SUB).
             ADD 1               TO W14-SUB.
             PERFORM GET-SORD-WRITE-SALE.

       GET-ADJUST-TOTALS.
             SUBTRACT 1 FROM W14-SUB
                                 GIVING WS-SUB.
             MOVE 1              TO W14-SUB.

       GET-ADJUST-TOTALS-LOOP.
             MOVE W14-SQN (W14-SUB)
                                 TO SOR-SQN.
             PERFORM READ-SORDER-LOCK THRU READ-SORDER-EXIT.
             SUBTRACT SOR-VAL    FROM W90-DB.
             SUBTRACT SOR-DISC   FROM W90-LDISC.
             SUBTRACT SOR-NONTAX FROM W90-NONTAX.
            SUBTRACT SOR-TAXVAL FROM W90-EXCTOT.
             COMPUTE W90-BAL = W90-BAL - (SOR-COST * SOR-QNT).
             MOVE 3              TO SOR-TYPE.
      STRING SOR-EXT-ITEM DELIMITED BY SIZE
                    SOR-DESC DELIMITED BY SIZE
                                 INTO SOR-DESC.
      MOVE SPACES  TO SOR-EXT-ITEM SOR-SERNO.
             MOVE ZERO           TO SOR-QNT SOR-COST SOR-INCTAX
                                    SOR-VAT SOR-SELL SOR-NONTAX
                                    SOR-VAL SOR-DISC SOR-TAXVAL
                                    SOR-DSC.
             PERFORM REWRITE-SORDER-UNLOCK THRU WRITE-SORDER-EXIT.
           IF W14-SUB < WS-SUB
               ADD 1             TO W14-SUB
               GO TO GET-ADJUST-TOTALS-LOOP.

       GET-SORDER-EXIT.
             EXIT.
      /
       BB000     SECTION.
       BB00.
    IF DEB-NCASH = "CASH SALE" OR
     "KONTANT V"
        GO TO BB06.
       BB05.
      MOVE "Enter client details Y/N  [Y]"
     TO WS-ERR-MES.
      MOVE "Y"   TO WS-OPTION.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "N" OR "Y".
    IF WS-OPTION = "N"
        GO TO BB20.
       BB06.
      PERFORM CLEAR-L50.
      PERFORM SAVE-SCREEN.
      MOVE SPACES  TO W40-NM1 W40-AD1 W40-AD2
         W40-AD3 W40-AD4 W40-HTEL
         W40-ID  W40-PC.
      MOVE 0815   TO CPOS.
       BB10.
     DISPLAY
  "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°"
   AT CPOS WITH BACKGROUND-COLOR 1 FOREGROUND-COLOR 3.
    IF CLIN < 18
        ADD 1   TO CLIN
        GO TO BB10.
      DISPLAY S40.

       BB15.
      ACCEPT S41.
    IF DEB-NCASH = "CASH SALE" OR
     "KONTANT V"
        IF (W40-NM1 = SPACES) OR
    (W40-AD1 = SPACES) OR
    (W40-HTEL = SPACES)
     MOVE "Require Name, Address & Telephone"
     TO WS-ERR-STRING
           PERFORM ERROR-MESSAGE
     GO TO BB15.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BB15.
    IF DEB-NCASH = "CASH SALE" OR
     "KONTANT V"
        MOVE W40-NM1  TO W40-NAME
        MOVE W40-AD1  TO W40-ADD1
        MOVE W40-AD2  TO W40-ADD2
        MOVE W40-AD3  TO W40-ADD3
        MOVE W40-AD4  TO W40-ADD4
        MOVE W40-PC  TO W40-PC1.
       BB20.
      PERFORM CLEAR-L50.
       BB29.
      EXIT.
      /
      *    ****    G E T   M E A S U R E M E N T S   (LENGTH/BREADTH)
      *     A N D   N U M B E R  O F   P I E C E S
      *
       BB050        SECTION.
       BB50.
      PERFORM SAVE-SCREEN-2.
      MOVE 5   TO SHADE-ROW.
      MOVE 17   TO SHADE-COL.
      MOVE 19   TO SHADE-WIDTH.
      MOVE 5   TO SHADE-LINES.
            DISPLAY S-MEASURE.
      PERFORM SCREEN-SHADOW.

       BB55.
      ACCEPT SA-MEASURE.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BB55.
      COMPUTE W10-QUANT ROUNDED = W10-LENGTH * W10-BREADTH
           * W10-NUMBER.
      MOVE 1   TO WS-P1.
      MOVE "Y"   TO WS-MEASURE.

       BB60.
      PERFORM RESTORE-SCREEN-2.
       BB69.
             EXIT.

      /
       BB100               SECTION.
       BB100-MARG.
            MOVE ZERO   TO WS-ERROR.
          IF (W90-TAX = SPACES) AND
       (W90-EXP NOT = "C" AND "X")
              IF STK-TAX NOT = ZERO
           COMPUTE W10-SPAPRC ROUNDED = (W10-SPAPRC /
                      (100.0000 + W05-RATE)) * 100.
    IF (WS-PRC = "1") AND (STK-SELL NOT > W10-SPAPRC)
               GO TO BB110
           ELSE
    IF (WS-PRC = "2") AND (STK-CASH NOT > W10-SPAPRC)
               GO TO BB110
           ELSE
           IF STK-WSALE NOT > W10-SPAPRC
               GO TO BB110.
             COMPUTE W10-MARG = ((W10-SPAPRC - W10-COST) / W10-SPAPRC)
                                * 100.00.
           IF W10-MARG NOT < W85-MARG
               GO TO BB110.

       BB105.
            MOVE "Margin less than allowed - Override Y/N?"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "N" OR "Y".
           IF WS-OPTION = "N"
               MOVE 1 TO WS-ERROR
               GO TO BB110.

       BB106.
      DISPLAY SPACE AT 2551
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
            DISPLAY "Enter password" AT 2551
              WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 6 HIGHLIGHT.
            ACCEPT W40-CSHPASS AT 2567
      WITH BACKGROUND-COLOR 5
    FOREGROUND-COLOR 7 HIGHLIGHT
          AUTO NO-ECHO.
            MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-CSHPASS
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
            MOVE 1   TO WS-LENGTH.
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
               MOVE 1            TO WS-ERROR
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
               MOVE 1            TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
       BB110.
             EXIT.
      /
      *    ****    P R I N T   Q U O T A T I O N
      *
       BB200      SECTION 50.
       BB201.
      *
      *  ***  ****  ***** *   *   **** ****  *** *   * ***** ***** ****
      * *   * *   * *   **  *   *   * *   *  *  **  *   *   *     * *
      * *   * ****  ***   * * *   **** ****   *  * * *   *   ***   ****
      * *   * *     *   *  **   * *   *  *  *  **   *   *     * *
      *  ***  *     ***** *   *   * *   * *** *   *   *   ***** * *
      *
      PERFORM OPEN-PRINTER.
      MOVE "P"   TO WS-COMMAND.
      MOVE ZERO   TO W55-TTAX
                                    W55-SUBTOT W55-VALUE
                                    W55-DSCNT.
      *
      *    ****   1 2 C P I ( C O U R I E R   P R I N T )
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE 2   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
    IF W34-MARGIN > ZERO
        MOVE SPACES  TO REP-DETAIL2
        MOVE W34-MARGIN  TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
    IF W34-FORMAT = "L"
        GO TO BB210.
      *
      *    ****   E X P A N D E D   P R I N T
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE "E"   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE W95-COMP  TO REP-COMPANY.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      *
      *    ****   C A N C E L E X P A N D E D   P R I N T
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE "X"   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
      MOVE SPACES  TO REP-DETAIL2.
            MOVE W95-ADD1  TO REP-NAME.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
            MOVE W95-ADD2  TO REP-NAME.
    IF W95-ADD3 = SPACES
        GO TO BB205
    ELSE
        PERFORM CALL-PRINTUTL.
            MOVE W95-ADD3  TO REP-NAME.
    IF W95-ADD4 = SPACES
        GO TO BB205
    ELSE
        PERFORM CALL-PRINTUTL.
            MOVE W95-ADD4  TO REP-NAME.
    IF W95-PC = SPACES
        GO TO BB205
    ELSE
        PERFORM CALL-PRINTUTL.
            MOVE W95-PC  TO REP-NAME.

       BB205.
          IF W95-TEL NOT = SPACES
              MOVE "TEL:"  TO REP-TEL
              MOVE W95-TEL  TO REP-TELNO.
      PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL2.
      PERFORM CALL-PRINTUTL.

       BB210.
      *
      *    ****   E X P A N D E D   P R I N T
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE "E"   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
      MOVE SPACES  TO REP-DETAIL2.
             MOVE "QUOTATION - REFERENCE NO:"
                                 TO REP-H1.
             MOVE W90-REF        TO REP-REF.
      MOVE 0   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      *
      *    ****   C A N C E L E X P A N D E D   P R I N T
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE "X"   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
      MOVE SPACES  TO REP-DETAIL2.
      MOVE 1   TO WS-ADVANCE.

       BB225.
      MOVE W40-NAME  TO REP-NAME.
      MOVE "Account  :"  TO REP-AC.
             MOVE W80-ACNO       TO REP-ACNO.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
           IF W40-AD1 NOT = SPACES
        MOVE W40-NM1  TO W40-NAME
               MOVE W40-AD1      TO W40-ADD1
               MOVE W40-AD2      TO W40-ADD2
               MOVE W40-AD3      TO W40-ADD3
               MOVE W40-AD4      TO W40-ADD4
               MOVE W40-PC       TO W40-PC1.
          IF W40-ADD4 = SPACES
               MOVE W40-PC1      TO W40-ADD4
               MOVE SPACES       TO W40-PC1.
           IF W40-ADD3 = SPACES
               MOVE W40-ADD4     TO W40-ADD3
               MOVE SPACES       TO W40-ADD4.
           IF W40-ADD2 = SPACES
               MOVE W40-ADD3     TO W40-ADD2
               MOVE SPACES       TO W40-ADD3.
           IF W40-ADD1 = SPACES
               MOVE W40-ADD2     TO W40-ADD1
               MOVE SPACES       TO W40-ADD2.
      MOVE W40-ADD1  TO REP-NAME.
      MOVE "Date     :"  TO REP-DATE.
             MOVE W12-TODAY      TO REP-DTE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
      MOVE W40-ADD2  TO REP-NAME.
    IF W90-ORD NOT = SPACES
        MOVE "Your Ref :" TO REP-ORD
        MOVE W90-ORD  TO REP-ORDNO.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
      MOVE W40-ADD3  TO REP-NAME.
    IF W40-CONTACT NOT = SPACES
        MOVE "Contact  :" TO REP-ORD
        MOVE W40-CONTACT  TO REP-ORDNO.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
      MOVE W40-ADD4  TO REP-NAME.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
      MOVE W40-PC1  TO REP-NAME.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.

       BB230.
    IF NOT (W40-PC = SPACES)
        PERFORM CALL-PRINTUTL.
      *
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE 1   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
    IF NOT (W34-SALUTATION = SPACES)
        MOVE W34-SALUTATION
     TO REP-TEXT
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      MOVE SPACES  TO REP-DETAIL2.
      MOVE 1   TO WS-S6.
      MOVE 10   TO WS-S7.

       BB231.
    IF W32-REMARKS (WS-S7) = SPACES
        IF WS-S7 > 1
     SUBTRACT 1  FROM WS-S7
     GO TO BB231.

       BB232.
      MOVE W32-REMARKS (WS-S6)
     TO REP-TEXT.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
    IF WS-S6 < WS-S7
        ADD 1   TO WS-S6
        GO TO BB232.
      PERFORM CALL-PRINTUTL.

       BB235.
      *
      *    ****   1 2 C P I ( C O U R I E R   P R I N T )
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE 2   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
             MOVE "ITEM CODE"    TO REP-H2.
             MOVE "DESCRIPTION"  TO REP-H3.
             MOVE "QUANTITY"     TO REP-H4.
             MOVE "UNT-PRICE"    TO REP-H5.
             MOVE "V.A.T."       TO REP-H6.
             MOVE "VALUE"        TO REP-H7.
      MOVE 2   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
      MOVE "D"   TO W02-PRN-TYPE.
      *
      *    ****    R E A D   O R D E R   F I L E   A N D   P R I N T
      *
       BB240.
             MOVE W90-REF        TO SOR-REF.
             MOVE ZERO           TO SOR-SQN.
             PERFORM START-AT-ORDER-NO THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
        MOVE "Database corrupted"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BB299.
       BB245.
             PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
        GO TO BB270.
           IF SOR-REF NOT = W90-REF
        GO TO BB270.
    IF SOR-TYPE = 0 OR 7 OR 8
        GO TO BB245.
           IF SOR-TYPE = 9
        GO TO BB270.
           IF SOR-TYPE NOT = 1
        IF SOR-TYPE = 2 OR 4 OR 5
     GO TO BB245
               ELSE
                   MOVE SOR-DESC TO REP-DESC
     PERFORM BB250
     GO TO BB245.
      MOVE SOR-EXT-ITEM  TO PRT-EXT-ITEM.
    IF WS-USE-ITM = "Y"
        MOVE SOR-ITEM  TO REP-ITM1
        MOVE "/"   TO REP-SL
        MOVE SOR-ITM  TO REP-ITM
    ELSE
        MOVE SOR-EXT-ITEM TO REP-EXT-ITEM.
      MOVE SOR-DESC  TO REP-DESC.
             MOVE SOR-QNT        TO REP-QNT.
           IF SOR-SELL = ZERO
               MOVE "  N/C"      TO REP-NC
           ELSE
               MOVE SOR-SELL     TO REP-PRICE.
      MOVE SOR-VAT  TO REP-VAT.
    IF W90-EXP = "I"
        MOVE SOR-VAL  TO REP-VAL
    ELSE
        ADD SOR-VAL SOR-VAT
     GIVING REP-VAL.
            PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
        PERFORM BB250
              GO TO BB245.
       BB250.
          IF W02-LINAGE < W02-PRN-LENGTH
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
    ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
      *
      *    ****  P R O D U C T I O N   D E T A I L S
      *
       BB255.
             MOVE 1              TO WS-S6.

       BB260.
           IF PRT-CODE (WS-S6) = SPACES
               ADD 1             TO WS-S6
              GO TO BB265.
            MOVE PRT-EXT-CODE(WS-S6)
     TO REP-ITM1 W10-EXT-ITEM.
            MULTIPLY PRT-QUANT (WS-S6) BY SOR-QNT
                                 GIVING REP-QNT.
             PERFORM CA000.
           IF WS-F-ERROR = 22
               ADD 1             TO WS-S6
              GO TO BB265.
             MOVE STK-DESC       TO REP-DESC.
      PERFORM BB250.
            ADD 1   TO WS-S6.

       BB265.
           IF WS-S6 < 31
              GO TO BB260.
      GO TO BB245.
      *
      *    ****    P R I N T   Q U O T A T I O N   T O T A L
      *
       BB270.
          IF W02-LINAGE < (W02-PRN-LENGTH - 8)
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
    ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
             MOVE "Sub Total:"   TO REP-H5.
             MOVE SOR-SUBTOT     TO REP-VAL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
    IF SOR-DSCNT NOT = ZERO
               MOVE "Discount :" TO REP-H5
               MOVE SOR-DSCNT    TO REP-VAL
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
    IF SOR-TAX NOT = ZERO
               MOVE "V.A.T.   :" TO REP-H5
               MOVE SOR-TAX      TO REP-VAL
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE "TOTAL    :"  TO REP-H5.
             MOVE SOR-TOT        TO REP-VAL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL2.
             ADD SOR-SUBTOT      TO W55-SUBTOT.
             ADD SOR-DSCNT       TO W55-DSCNT.
             ADD SOR-TAX         TO W55-TTAX.
             ADD SOR-TOT         TO W55-VALUE.
       BB275.
          IF W02-LINAGE < (W02-PRN-LENGTH - 10)
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
    ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      *
      *    ****   1 O C P I ( N O R M A L P R I N T )
      *
      MOVE 0   TO WS-ADVANCE.
      MOVE 1   TO W02-PRN-TYPE.
      PERFORM CALL-PRINTUTL.
      MOVE "D"   TO W02-PRN-TYPE.
      MOVE W30-REMARKS (1)
     TO REP-TEXT.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE W30-REMARKS (2)
     TO REP-TEXT.
      PERFORM CALL-PRINTUTL.
      MOVE W30-REMARKS (3)
     TO REP-TEXT.
      PERFORM CALL-PRINTUTL.
      MOVE W30-REMARKS (4)
     TO REP-TEXT.
      PERFORM CALL-PRINTUTL.
             MOVE SPACES         TO REP-DETAIL2.
      PERFORM CALL-PRINTUTL.
      MOVE W34-SIGNATURE  TO REP-TEXT.
      MOVE 4   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
             MOVE SPACES         TO REP-DETAIL2.
    IF W10-SMAN > ZERO
        ADD 100 W10-SMAN  GIVING WS-PARKEY
        PERFORM READ-PARAM THRU READ-PARAM-EXIT
        MOVE PAR-SALESMAN TO REP-TEXT.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
             MOVE SPACES         TO REP-DETAIL2.
      MOVE "E"   TO WS-COMMAND.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.

       BB285.
      MOVE "C"   TO WS-COMMAND.
      PERFORM CALL-PRINTUTL.

       BB299.
             EXIT.
      /
      *    ****    A M E N D   Q U O T A T I O N
      *
       BD000         SECTION 51.
       BD00.
      *
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW         TO W85-PASS.
       BD05.
      *
      *    ****    Q U O T A T I O N   S C R E E N
      *
             PERFORM ERASE-SCREEN.
             DISPLAY S30.
      *
      *            C H A N G E   S C R E E N   H E A D I N G
      *
             DISPLAY S31.
             MOVE 8              TO LIN.
             PERFORM BD115 UNTIL LIN > 18.
             MOVE W12-TODAY      TO W90-DTE.
            DISPLAY W90-DTE AT 0570
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
             MOVE ZERO           TO W90-BDT W90-NONTAX W90-LDISC
              W90-BAL W90-SALES
        W90-CUR W90-EXCTOT.
            MOVE 1   TO WS-FIRST WS-SALPOS WS-LAST.
             MOVE ZERO           TO W10-SORD.
             MOVE "N"            TO WS-IN-OPT.
      MOVE 1   TO WS-S9.

       BD08.
            MOVE ZERO   TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
    IF WS-S9 < 12
        ADD 1   TO WS-S9
        GO TO BD08.

       BD10.
             PERFORM AA400.
             ACCEPT S30A AT 0510.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     MOVE ZERO   TO W10-SORD
       GO TO BD268
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "O"  TO WS-ETYPE
   WHEN F3-KEY
                     MOVE "A"    TO WS-ETYPE
   WHEN F4-KEY
                     MOVE "T"    TO WS-ETYPE
                 WHEN OTHER
                     CALL X"E5"
                     GO TO BD10
               END-EVALUATE
              PERFORM AA50O THRU AA59
               DISPLAY S30A AT 0510
               IF W10-SORD = ZERO
                   GO TO BD10.
            PERFORM CLEAR-L50.
           IF W10-SORD = ZERO
              GO TO BD268.
      MOVE 275   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-QTE-SAL  TO W34-SALUTATION.
      MOVE PAR-QTE-SIG  TO W34-SIGNATURE.
      MOVE 0   TO WS-S7.

       BD11.
      ADD 1   TO WS-PARKEY WS-S7.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-QHDR1  TO W32-REMARKS (WS-S7).
      ADD 1   TO WS-S7.
      MOVE PAR-QHDR2  TO W32-REMARKS (WS-S7).
    IF WS-S7 < 10
        GO TO BD11.
             MOVE W10-SORD       TO W90-REF.

       BD12.
             PERFORM GET-SORDER.
           IF WS-ERROR = 1
        MOVE WS-ERR2  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BD10.
           IF WS-ERROR = 2
        MOVE "Not a quotation"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BD10.
           IF WS-ERROR = 3
        MOVE "Quote corrupted"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BD10.
           IF WS-ERROR = 4
        MOVE "Invalid a/c no"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BD10.
      INITIALIZE W14-DELETE.
      MOVE 0   TO W14-SUB.

       BD13.
    IF WS-S7 > 1 AND < 11
        MOVE SPACES  TO W32-REMARKS (WS-S7)
        ADD 1   TO WS-S7
        GO TO BD13.
             INITIALIZE W14-DELETE.
           IF W90-EXP = "E" OR "X"
               MOVE W05-VATNO    TO W90-TAX
           ELSE
               MOVE SPACES       TO W90-TAX.
             DISPLAY S36 AT 0308
            DISPLAY W40-NAME AT 0320
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            DISPLAY W90-ORD AT 0368
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            DISPLAY W10-SMAN AT 0411
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
           IF W10-SMAN NOT = ZERO
               ADD 100 W10-SMAN    GIVING WS-PARKEY
               PERFORM READ-PARAM THRU READ-PARAM-EXIT
              DISPLAY PAR-SALESMAN AT 0420
         WITH FOREGROUND-COLOR 3 HIGHLIGHT
       BACKGROUND-COLOR 5.
    IF DEB-NCASH = "CASH SALE" OR
                          "KONTANT V"
        DISPLAY W40-HTEL AT 0466
         WITH FOREGROUND-COLOR 7 HIGHLIGHT
       BACKGROUND-COLOR 5
    ELSE
        DISPLAY W40-TEL AT 0466
         WITH FOREGROUND-COLOR 7 HIGHLIGHT
       BACKGROUND-COLOR 5.
      DISPLAY W40-CONTACT AT 0528
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5.
             MOVE 1              TO WS-FIRST WS-SALKEY.
             SUBTRACT 1          FROM WS-LAST.
      PERFORM BD100 THRU BD102.

       BD15.
      PERFORM BD270A.
      PERFORM CLEAR-L50.

       BD20.
            ADD 1   TO WS-LAST.
             GO TO BD112.

       BD50.
             MOVE 8              TO LIN.

       BD55.
            MOVE SPACES  TO W10-ITEM.

       BD56.
            MOVE SPACES  TO W10-DESC.
             MOVE ZERO           TO W10-SELL.
             MOVE 1              TO W10-QUANT.

       BD60.
             PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
    IF WS-EXT-STK = "Y"
        ACCEPT S07-EXT AT LINE LIN
    ELSE
      *
      *        ***** ALPHA-NUMERIC CODE *****
      *
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     ACCEPT S07-ITM AT LINE LIN
        ELSE
           ACCEPT S07 AT LINE LIN
        END-IF
    ELSE
      *
      *    ***** NUMERIC CODES APPLY *****
      *
    IF W10-SLNGTH = 3
        ACCEPT S07-3 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S07-4 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S07-5 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S07-6 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S07-7 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S07-8 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S07-9 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S07-10 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S07-11 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S07-12 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S07-13 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S07-14 AT LINE LIN.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
                     GO TO BD60A
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "I"  TO W10-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO W10-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO W10-ETYPE
   WHEN F5-KEY
       MOVE "2"  TO W10-ETYPE
   WHEN F6-KEY
       MOVE "X"  TO W10-ETYPE
   WHEN F9-KEY
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Lookup using any word or part in the Stock Description.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       MOVE "W"  TO W10-ETYPE
   WHEN OTHER
                     CALL X"E5"
                     GO TO BD60
               END-EVALUATE
              PERFORM AA50 THRU AA59
        PERFORM BD56
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT AT LINE LIN
        ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM AT LINE LIN
     ELSE
               DISPLAY S07 AT LINE LIN.
              IF W10-EXT-ITEM = SPACES
                   GO TO BD60.

       BD60A.
          IF W10-EXT-ITEM = SPACES
               SUBTRACT 1        FROM WS-LAST
               GO TO BD120.

       BD61.
             PERFORM CA000.
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT AT LINE LIN
    ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT LINE LIN
        ELSE
           DISPLAY S07 AT LINE LIN.
          IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50 THRU AA59
        PERFORM BD56
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT AT LINE LIN
        ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM AT LINE LIN
     ELSE
               DISPLAY S07 AT LINE LIN
     END-IF
        END-IF
        END-IF
              IF W10-EXT-ITEM = SPACES
                   GO TO BD60
               ELSE
     GO TO BD61.

       BD62.
    IF STK-IND = 4
        MOVE "Header Record"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO BD60.
            MOVE WS-LAST  TO WS-SALKEY.
             INITIALIZE SAL-RECORD1.
             MOVE WS-LAST        TO SAL-CODE.
            MOVE STK-CODE  TO SAL-EXT-ITEM.
            PERFORM CLEAR-L50.
      *
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *
       BD65.
           IF STK-IND = 3
               MOVE ZERO         TO STK-AVGE
               MOVE SPACES       TO W10-DESC
              ACCEPT (LIN, 21) W10-SDSC
         WITH FOREGROUND-COLOR 7 HIGHLIGHT
       BACKGROUND-COLOR 0 UPDATE AUTO
               DISPLAY "Index Unit cost" AT 2512
         WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 6 HIGHLIGHT
               ACCEPT S26-AVGE AT LINE 25
           ELSE
               MOVE STK-DESC     TO W10-DESC
              DISPLAY (LIN, 21) W10-SDSC
   WITH FOREGROUND-COLOR 3 HIGHLIGHT
        BACKGROUND-COLOR 0.
           IF STK-IND = 3
               IF STK-AVGE = ZERO
           MOVE "Unit cost is zero"
     TO WS-ERR-STRING
           PERFORM ERROR-MESSAGE
                   GO TO BD65.
            PERFORM CLEAR-L50.
      *
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *
       BD65-RESTART.
            MOVE W10-EXT-ITEM  TO SAL-EXT-ITEM.
             MOVE W10-DESC       TO SAL-MSER.
             MOVE STK-AVGE       TO SAL-COST W10-COST.
      *
      *    ****    G E T   Q U A N T I T Y
      *
      MOVE ZERO   TO W10-LENGTH W10-BREADTH W10-NUMBER.
      MOVE "N"   TO WS-MEASURE.
      MOVE 1   TO WS-P1.
    IF (LS0-LEV < 3) OR
       (STK-USE-PACKS = "N")
        GO TO BD66.
      DISPLAY "µ" AT 0750
        STK-PER WITH BACKGROUND-COLOR 5
       FOREGROUND-COLOR 6 HIGHLIGHT
       "Æ".
      GO TO BD66.

       BD65-PACK.
    IF NOT (STK-UNT(WS-P1) = ZERO)
        DISPLAY STK-PR(WS-P1) AT 0751
         WITH BACKGROUND-COLOR 5
       FOREGROUND-COLOR 6 HIGHLIGHT
    ELSE
    IF WS-P1 = 1
        DISPLAY "  Each  "  AT 0751
   WITH BACKGROUND-COLOR 5
        FOREGROUND-COLOR 6 HIGHLIGHT
    ELSE
        SUBTRACT 1  FROM WS-P1
        GO TO BD65-PACK.

       BD66.
      DISPLAY "F6" AT 2502
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 6 HIGHLIGHT
       " to key in "
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 1
       "- Cutting size (Wood, Glass etc) "
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 7 HIGHLIGHT
       "or "
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 1
       "Esc" WITH BACKGROUND-COLOR 3
    FOREGROUND-COLOR 6 HIGHLIGHT
       "ape" WITH BACKGROUND-COLOR 3
    FOREGROUND-COLOR 1.
    IF LS0-LEV > 2
        IF STK-USE-PACKS = "Y"
     DISPLAY "" AT 2560
       WITH BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6 HIGHLIGHT
      " for Packs"
       WITH BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1.
            ACCEPT S26-QNT AT LINE LIN.
    IF ADIS-FUNC
        IF LS0-LEV < 3
     GO TO BD66
        END-IF
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
       IF WS-P1 > 1
    SUBTRACT 1  FROM WS-P1
    GO TO BD65-PACK
   WHEN DOWN-KEY
       IF WS-P1 < 4
    ADD 1      TO WS-P1
    GO TO BD65-PACK
        END-EVALUATE
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             DISPLAY (LIN, 2) WS-INV78 WITH FOREGROUND-COLOR 3
             GO TO BD55
   WHEN F6-KEY
       PERFORM BB050
             DISPLAY S26-QNT AT LINE LIN
   END-EVALUATE.
          IF W10-QUANT = ZERO
              DISPLAY (LIN, 2) WS-INV78
   WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
              GO TO BD55.
      MULTIPLY STK-UNT(WS-P1) BY W10-QUANT
     GIVING W10-QNT.
      *
      *    ****    G E T   S E L L I N G   P R I C E   (ACCOUNT OR CASH)
      *
       BD66A.
    IF WS-P1 > 1
        MULTIPLY STK-UNT(WS-P1) BY STK-AVGE
     GIVING SAL-COST W10-COST
    ELSE
              MOVE STK-AVGE  TO SAL-COST W10-COST.
      MOVE W90-TAX  TO W10-ENTRY.
      *
      *    ****    C H A N G E  F O R  M U L T I P L E   R A T E S
      *
      MOVE STK-TAX  TO WS-VAT-SUB.
          IF STK-IND < 3
               IF W90-TAX NOT = SPACES
      *
      *    ****    E X C L U S I V E   P R I C E S
      *
           IF WS-PRC = "1"
               MOVE STK-SL(WS-P1)
                                 TO W10-SELL
           ELSE
           IF WS-PRC = "2"
               MOVE STK-CSH(WS-P1)
                                 TO W10-SELL
           ELSE
               MOVE STK-WSL(WS-P1)
                                 TO W10-SELL
              ELSE
      *
      *    ****    I N C L U S I V E   P R I C E S
      *
           IF WS-PRC = "1"
               MOVE W10-VSL(WS-P1)
                                 TO W10-SELL
                   ELSE
           IF WS-PRC = "2"
               MOVE W10-VCSH(WS-P1)
                                 TO W10-SELL
                   ELSE
               MOVE W10-VWSL(WS-P1)
                                 TO W10-SELL.
      MOVE W10-ENTRY  TO W90-TAX.

       BD68.
             MOVE W10-SELL       TO W10-SPAPRC.

       BD70.
             ACCEPT S26-PRC AT LINE LIN.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BD66
        END-EVALUATE.
    IF (W10-SELL < W10-SPAPRC) AND
       ((W10-SPAPRC - W10-SELL) < 0.01)
        MOVE W10-SELL  TO W10-SPAPRC.
    IF W10-SELL < W10-SPAPRC
              PERFORM CLEAR-L50
               COMPUTE WS-DISC ROUNDED = ((W10-SELL - W10-SPAPRC) /
                                           W10-SELL) * 100.00000
              DISPLAY "Discount %" AT 2518
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 6 HIGHLIGHT
               DISPLAY S26-DSC AT LINE 25
        DISPLAY "R" AT 2541
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 6 HIGHLIGHT
         "ekey or "
   WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
         "ENTER"
   WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 6 HIGHLIGHT
         " to continue"
   WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
        ACCEPT WS-OPTION AT 2568
        WITH BACKGROUND-COLOR 5
      FOREGROUND-COLOR 7 HIGHLIGHT AUTO
        CALL "CBL_TOUPPER" USING WS-OPTION
                                  BY VALUE WS-LENGTH
                                  RETURNING WS-STATUS
              PERFORM CLEAR-L50
               IF WS-OPTION = "R"
                   GO TO BD70.
    IF W10-SELL < W10-SPAPRC
              MOVE W10-SELL  TO W10-SPAPRC
              PERFORM BB100
              IF WS-ERROR = 1
           MOVE ZERO  TO WS-ERROR
           GO TO BD66A.

       BD75.
             MOVE W10-SELL       TO SAL-SELL.
      *
      *    ****    C A L C U L A T E   E X T E N D E D   V A L U E
      *
       BD80.
            MOVE W10-QUANT  TO SAL-QNT.
      MOVE WS-P1   TO SAL-PACK.
             COMPUTE W25-VALUE ROUNDED = (1.000 * W10-SELL * W10-QUANT).
             MOVE W25-VALUE      TO W100-EXT SAL-VAL.
            DISPLAY (LIN, 70) W100-EXT
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
      *
      *    ****    L I N E   D I S C O U N T   ( PERCENTAGE )
      *
            PERFORM CLEAR-L50.
            DISPLAY "Discount %" AT 2518
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 6 HIGHLIGHT.
             MOVE ZERO           TO WS-DISC W90-CDISC.
      *
      *    ****   I F D I S C O U N T    A P P L I C A B L E
      *    G E T   P E R C E N T A G E F R O M
      *    D E P A R T M E N T F I L E
      *
          IF (DEB-DISC > 0 AND < 11)
              MOVE STK-LDG  TO DPT-CODE
               PERFORM READ-DEPART THRU READ-DEPART-EXIT
              MOVE DPT-DSETT(DEB-DISC)
                                 TO WS-DISC.

       BD85.
             ACCEPT S26-DSC AT LINE 25.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             PERFORM CLEAR-L50
       GO TO BD70
        END-EVALUATE.
          IF WS-DISC = ZERO
              PERFORM CLEAR-L50
               GO TO BD90.
            COMPUTE W10-SPAPRC ROUNDED = W10-SELL -
                    (W10-SELL * WS-DISC / 100.0000).
      *
      *    ****    C H E C K   M A R G I N
      *
          IF (DEB-DISC > 0 AND < 11)
              IF DPT-DSETT(DEB-DISC) = WS-DISC
                   GO TO BD86.
             PERFORM BB100.
           IF WS-ERROR = 1
               GO TO BD85.

       BD86.
            PERFORM CLEAR-L50.
            MOVE WS-DISC  TO SAL-DSC.
            COMPUTE W90-CDISC ROUNDED =
     ((W25-VALUE * WS-DISC) / 100.00000).
            MOVE W90-CDISC  TO W100-LBAL SAL-DISC.
            DISPLAY W100-LBAL AT 2540
       WITH BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3 HIGHLIGHT.
       BD90.
      *
      *    ****    A R E   T H E   D E T A I L S   C O R R E C T
      *
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO BD60.
      *
      *    ****    A C C U M U L A T E   TOTAL COST / TOTAL DISCOUNT /
      *            VALUES SUBJECT TO TAX AND NON-TAX.
      *
       BD95.
          IF (STK-IND < 2) OR (STK-IND = 3)
               COMPUTE W90-BAL = W90-BAL + (W10-QUANT * W10-COST).
             ADD W90-CDISC       TO W90-LDISC.
             SUBTRACT W90-CDISC FROM W25-VALUE
                                 GIVING W90-VALUE.
    IF (STK-TAX = 0) OR (W90-EXP = "X" OR "C")
               ADD W90-VALUE     TO W90-NONTAX
               MOVE W90-VALUE    TO SAL-NONTAX
        MOVE ZERO  TO SAL-VAT-SUB
           ELSE
               MOVE W90-VALUE    TO SAL-TAXVAL
        IF W90-EXP = "I"
     COMPUTE W90-TAXVAL(WS-VAT-SUB) =
      W90-TAXVAL(WS-VAT-SUB) + (W90-VALUE -
    ((W90-VALUE / (100.0000 + W05-RATE)) *
      W05-RATE))
        ELSE
           ADD W90-VALUE TO W90-TAXVAL(WS-VAT-SUB)
        END-IF
        MOVE WS-VAT-SUB  TO SAL-VAT-SUB.
          IF (W90-TAX NOT = SPACES) AND
       (W90-EXP NOT = "C" AND "X")
        COMPUTE SAL-VAT ROUNDED = SAL-TAXVAL * W05-RTE.
    IF (W90-EXP = "I") AND (STK-TAX NOT = 0)
        COMPUTE SAL-VAT ROUNDED = (SAL-TAXVAL /
        (100.0000 + W05-RATE)) * W05-RATE.
    IF SAL-VAT-SUB > 0
        ADD SAL-VAT  TO W90-VAT(SAL-VAT-SUB).
      *
      *    ****    A C C U M U L A T E   T O T A L   Q U O T E
      *
             ADD W25-VALUE       TO W90-DB.

       BD100.
             MOVE W90-DB         TO W90-TOTAL.
             SUBTRACT W90-LDISC  FROM W90-TOTAL.
             MOVE W90-DB         TO W100-BAL.
            DISPLAY W100-BAL AT 2015
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
             MULTIPLY -1 BY W90-LDISC
                                 GIVING W100-BAL.
            DISPLAY W100-BAL AT 2115
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      *
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *
          IF (W90-TAX = SPACES) OR
       (W90-EXP = "C" OR "X")
               GO TO BD102.
      MOVE 1   TO WS-S1.
      MOVE ZERO   TO W25-TOTAL.

       BD101.
          IF W90-TAXVAL(WS-S1) NOT = ZERO
              MOVE W05-VAT(WS-S1)
           TO W05-RATE
              COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
        (W90-TAXVAL(WS-S1) * W05-RTE).
    IF WS-S1 < 6
        ADD 1   TO WS-S1
        GO TO BD101.
            MOVE W25-TOTAL  TO W100-BAL.
            ADD W25-TOTAL  TO W90-TOTAL.
            DISPLAY W100-BAL AT 2215
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.

       BD102.
            MOVE W90-TOTAL  TO W100-BAL.
            DISPLAY W100-BAL AT 2315
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.

       BD105.
            MOVE "'A'ccept or 'C'ancel   [A]"
     TO WS-ERR-MES.
            MOVE "A"   TO WS-OPTION.
      *
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E
      *            A C C E P T E D.
      *
       BD110.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "A" OR "C".
           IF WS-OPTION = "C"
              DISPLAY (LIN, 2) WS-INV78
   WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 1
              PERFORM BD114
               PERFORM BD100 THRU BD102
               GO TO BD55.

       BD111.
             MOVE WS-SUPER       TO SAL-SUPER.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
             ADD 1               TO WS-LAST.
      *
      *    ****    T E S T   F O R   C U T T I N G   S P E C S
      *
    IF W10-LENGTH > ZERO AND
       W10-BREADTH > ZERO AND
       W10-NUMBER > ZERO
              IF LIN < 18
           ADD 1  TO LIN
        ELSE
           MOVE 8  TO WS-S7
           PERFORM BD111-SHIFT
        END-IF
              MOVE WS-LAST  TO WS-SALKEY
              INITIALIZE SAL-RECORD1
              MOVE WS-LAST  TO SAL-CODE
        MOVE SPACES  TO W10-REMKS
        MOVE W10-LENGTH  TO W100-LENGTH
        MOVE W10-BREADTH  TO W100-BREADTH
        MOVE W10-NUMBER  TO W100-NUMBER
        STRING W100-LENGTH DELIMITED SIZE
        " by " DELIMITED SIZE
        W100-BREADTH DELIMITED SIZE
        " X " DELIMITED SIZE
        W100-NUMBER DELIMITED SIZE
        INTO W10-REMKS
              MOVE W10-REMKS  TO SAL-MSER
        MOVE WS-SUPER  TO SAL-SUPER
              PERFORM WRITE-SALE THRU WRITE-SALE-EXIT
              DISPLAY (LIN, 21) W10-REMKS
   WITH FOREGROUND-COLOR 7 HIGHLIGHT
        BACKGROUND-COLOR 0 UPDATE AUTO
              ADD 1   TO WS-LAST.
      *
      *    ****    T E S T   I F   S C R E E N   F U L L
      *
           IF LIN < 18
               ADD 1             TO LIN
               GO TO BD112.
      PERFORM SAVE-SCREEN-2.
      MOVE 8   TO WS-S7.
      *
      *    ****    S C R O L L  D E T A I L   L I N E S   U P
      *
       BD111-SHIFT.
      ADD 1 WS-S7  GIVING WS-S8.
      MOVE W42-LINES (WS-S8)
     TO W42-LINES (WS-S7).
      MOVE W43-LINE (WS-S8)
     TO W43-LINE (WS-S7).
    IF WS-S8 < 18
        ADD 1   TO WS-S7
        GO TO BD111-SHIFT.
      PERFORM RESTORE-SCREEN-2.
      *
      *    ****    C L E A R   L A S T  D E T A I L   L I N E
      *
            DISPLAY (LIN, 1) WS-INV1 WITH BACKGROUND-COLOR 0
        FOREGROUND-COLOR 3
         WS-INV78 WITH FOREGROUND-COLOR 3
         WS-INV80 WITH BACKGROUND-COLOR 0
         FOREGROUND-COLOR 3.
      ADD 1   TO WS-SALPOS.

       BD112.
      MOVE SPACES     TO WS-ERR-MES.
            STRING "'I'nternal notes,'R'emarks,'N'ext or 'E'nd  ["
      DELIMITED SIZE
      WS-IN-OPT DELIMITED SIZE
      "]" DELIMITED SIZE
     INTO WS-ERR-MES.
            MOVE WS-IN-OPT  TO WS-OPTION.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "I" OR "R" OR "N" OR "E" OR "C".
      *
      *    ****  "C"   M A Y   B E   U S E D   T O   C H A N G E
      *    C L I E N T D E T A I L S
      *
    IF WS-OPTION = "C"
        PERFORM BD591A
        GO TO BD112.
             MOVE WS-OPTION      TO WS-IN-OPT.
           IF WS-OPTION = "N"
               GO TO BD55.
           IF WS-OPTION = "E"
               SUBTRACT 1        FROM WS-LAST
               GO TO BD120.
             MOVE SPACES         TO W10-REMKS.
            ACCEPT (LIN, 21) W10-REMKS
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0 UPDATE AUTO.
           IF W10-REMKS = SPACES
               GO TO BD112.
            MOVE WS-LAST  TO WS-SALKEY.
             INITIALIZE SAL-RECORD1.
             MOVE WS-LAST        TO SAL-CODE.
             MOVE W10-REMKS      TO SAL-MSER.
           IF WS-OPTION = "I"
        DISPLAY "(I)" AT LINE LIN COLUMN 5
   WITH FOREGROUND-COLOR 6 HIGHLIGHT
               MOVE 1            TO SAL-SUPER
           ELSE
        DISPLAY "(R)" AT LINE LIN COLUMN 5
   WITH FOREGROUND-COLOR 7 HIGHLIGHT.
             GO TO BD111.

       BD114.
             SUBTRACT W25-VALUE  FROM W90-DB.
             SUBTRACT W90-CDISC  FROM W90-LDISC.
             SUBTRACT SAL-NONTAX FROM W90-NONTAX.
    IF SAL-VAT-SUB > ZERO
              SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(SAL-VAT-SUB)
        SUBTRACT SAL-VAT  FROM W90-VAT(SAL-VAT-SUB).
          IF (STK-IND < 2) OR (STK-IND = 3)
               COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *
      *    ****   C L E A R   S C R E E N   L I N E / S
      *
       BD115.
            DISPLAY (LIN, 1) WS-INV1 WITH BACKGROUND-COLOR 0
        FOREGROUND-COLOR 3
         WS-INV78 WITH FOREGROUND-COLOR 3
         WS-INV80 WITH BACKGROUND-COLOR 0
         FOREGROUND-COLOR 3.
            ADD 1   TO LIN.
      *
      *    ****    W A S   T H E R E   A N   E N T R Y
      *
       BD120.
          IF WS-LAST = ZERO
               GO TO BD05.
      *
      *    ****    U P D A T E   T H E   Q U O T E
      *
       BD125.
            MOVE "'U'pdate,'P'rint,'A'mend,'C'ancel,'M'ore items"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.

       BD130.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "U" OR "A" OR "C" OR "M" OR "P".
           IF WS-OPTION = "M"
               ADD 1             TO WS-LAST
               GO TO BD112.
           IF WS-OPTION = "A"
              PERFORM BD270A
               GO TO BD125.
           IF WS-OPTION = "C"
              GO TO BD268.
             MOVE ZERO           TO W90-STOTAL WS-TDISC W90-TDISC.
           IF W90-LDISC NOT = ZERO
               MOVE W90-LDISC    TO W90-TDISC
               GO TO BD140.
            MOVE W90-DB  TO W90-DVAL.

       BD140.
             SUBTRACT W90-TDISC FROM W90-DB
                                 GIVING W90-TSELL W90-TOTAL.
             MULTIPLY -1 BY W90-TDISC
                                 GIVING W100-BAL.
            DISPLAY W100-BAL AT 2115
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            MOVE ZERO   TO W90-TTAX.
      MOVE 1   TO WS-P1.
          IF (W90-TAX = SPACES) OR (W90-EXP = "C" OR "X")
              GO TO BD142.
      *
      *    ****    C A L C U L A T E   T O T A L   V A T
      *
       BD141.
          IF (W90-TAXVAL(WS-P1) = ZERO)
              GO TO BD142.
            MOVE W05-VAT(WS-P1) TO W05-RATE.
            COMPUTE W90-TTAX ROUNDED =
      (W90-TAXVAL(WS-P1) * W05-RTE).
      ADD W90-TAXVAL(WS-P1)
     TO W90-EXCTOT.
      *
      *    ****    C H E C K   I F   A L L   V A T   R A T E S
      *     H A V E   B E E N   C A L C U L A T E D
      *
       BD142.
    IF WS-P1 < 6
        ADD 1   TO WS-P1
        GO TO BD141.
            MOVE W90-TTAX  TO W100-BAL.
            DISPLAY W100-BAL AT 2215
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
             ADD W90-TTAX        TO W90-TOTAL.
            MOVE W90-TOTAL  TO W100-BAL.
            DISPLAY W100-BAL AT 2315
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CLEAR-L50.
      *
      *    ****    U P D A T E   Q U O T A T I O N   D E T A I L S
      *
       BD145.
           DISPLAY "Updating Quote" AT 2520
       WITH BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6 HIGHLIGHT BLINK.
             MOVE ZERO           TO WS-SALKEY.
             PERFORM AY60 THRU AY999.
      *
      *    ****    U P D A T E   O R D E R   F I L E
      *
       BD190.
            MOVE W90-REF  TO SOR-REF.
           IF W14-SUB = ZERO
               GO TO BD205.
      *
      *    ****    D E L E T E   R E D U N D A N T   R E C O R D S
      *
             MOVE 1              TO WS-S1.

       BD195.
           IF W14-SQN (WS-S1) NOT = ZERO
               MOVE W14-SQN (WS-S1)
                                 TO SOR-SQN
               PERFORM READ-SORDER-LOCK THRU READ-SORDER-EXIT
              MOVE 2   TO WS-ACTION
               PERFORM AY17 THRU AY59
               PERFORM DELETE-SORDER-REC THRU WRITE-SORDER-EXIT.
           IF WS-S1 < W14-SUB
               ADD 1             TO WS-S1
               GO TO BD195.
             GO TO BD205.

       BD200.
             PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
      *
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            L O G   T O   O R D E R   F I L E
      *
       BD205.
      ADD 1   TO WS-SALKEY.
           IF WS-SALKEY > WS-LAST
               GO TO BD255.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
           IF SAL-SQN = ZERO
               SUBTRACT 1        FROM WS-SALKEY
               GO TO BD235.
             MOVE SAL-SQN        TO SOR-SQN.
             PERFORM READ-SORDER THRU READ-SORDER-EXIT.
             MOVE 0              TO WS-ACTION.
             PERFORM AY17 THRU AY59.
           IF SOR-TYPE NOT = 1
               GO TO BD210.
            MOVE SAL-EXT-ITEM  TO SOR-EXT-ITEM.
      MOVE SAL-MSER  TO SOR-DESC.
             MOVE SAL-QNT        TO SOR-QNT.
             MOVE SAL-COST       TO SOR-COST.
             MOVE SAL-VAT        TO SOR-VAT.
            MOVE SAL-VAT-SUB  TO SOR-VAT-SUB.
             MOVE SAL-SELL       TO SOR-SELL.
             MOVE SAL-VAL        TO SOR-VAL.
             MOVE SAL-DISC       TO SOR-DISC.
             MOVE SAL-DSC        TO SOR-DSC.
             MOVE SAL-INCTAX     TO SOR-INCTAX.
             MOVE SAL-NONTAX     TO SOR-NONTAX.
             MOVE SAL-TAXVAL     TO SOR-TAXVAL.
      MOVE SAL-PACK  TO SOR-PACK.
      GO TO BD200.

       BD210.
             MOVE SAL-DESC       TO SOR-DESC.
             GO TO BD200.
      *
      *    ****    L O G   R E C O R D   T O   O R D E R   F I L E
      *
       BD225.
             PERFORM WRITE-SORDER THRU WRITE-SORDER-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY17 THRU AY59.
      *
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            L O G   T O   O R D E R   F I L E
      *
       BD235.
      ADD 1   TO WS-SALKEY.
           IF WS-SALKEY > WS-LAST
               GO TO BD255.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
          IF SAL-EXT-ITEM = SPACES
               GO TO BD247.
             MOVE 1              TO SOR-TYPE.
            MOVE SAL-EXT-ITEM  TO SOR-EXT-ITEM.
             MOVE SAL-DESC       TO SOR-DESC.
             MOVE SAL-QNT        TO SOR-QNT.
             MOVE SAL-COST       TO SOR-COST.
            MOVE SAL-VAT-SUB  TO SOR-VAT-SUB.
             MOVE SAL-VAT        TO SOR-VAT.
             MOVE SAL-SELL       TO SOR-SELL.
             MOVE SAL-VAL        TO SOR-VAL.
             MOVE SAL-DISC       TO SOR-DISC.
             MOVE SAL-DSC        TO SOR-DSC.
             MOVE SAL-INCTAX     TO SOR-INCTAX.
             MOVE SAL-NONTAX     TO SOR-NONTAX.
             MOVE SAL-TAXVAL     TO SOR-TAXVAL.
      MOVE SAL-PACK  TO SOR-PACK.
             MOVE SPACES         TO SOR-SERNO.
             ADD 1               TO SOR-SEQ.
             PERFORM BD225.
             PERFORM BD248 THRU BD254.
             GO TO BD235.

       BD247.
           IF SAL-SUPER = 1
               MOVE 4            TO SOR-TYPE
           ELSE
        MOVE 3   TO SOR-TYPE.
             INITIALIZE SOR-DET2.
             MOVE SAL-MSER       TO SOR-SERIAL.
             ADD 1               TO SOR-SEQ.
             GO TO BD225.

       BD248.
      MOVE SOR-EXT-ITEM  TO PRT-EXT-ITEM.
            PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
               GO TO BD254.
      *
      *    ****  P R O D U C T I O N   D E T A I L S
      *
             MOVE 1              TO WS-S6.

       BD249.
          IF PRT-EXT-CODE(WS-S6) = SPACES
               ADD 1             TO WS-S6
               GO TO BD250.
            MOVE PRT-EXT-CODE(WS-S6)
     TO W10-EXT-ITEM.
             PERFORM CA000.
           IF WS-F-ERROR = 22
               ADD 1             TO WS-S6
               GO TO BD250.
             MOVE 2              TO SOR-TYPE.
             INITIALIZE SOR-DET2.
      MOVE STK-CODE  TO SOR-EXT-ITEM.
             MOVE STK-DESC       TO SOR-DESC.
      MULTIPLY SAL-QNT BY PRT-QUANT(WS-S6)
                                 GIVING SOR-QNT.
             ADD 1               TO SOR-SEQ.
             PERFORM BD225.
            ADD 1   TO WS-S6.
       BD250.
           IF WS-S6 < 31
               GO TO BD249.
       BD254.
             EXIT.
      *
      *    ****    C H E C K / U P D A T E   C L I E N T   D E T A I L S
      *
       BD255.
    IF W40-AD1 = SPACES
               GO TO BD256.
           IF W35-KEY = SPACES
               MOVE 5            TO SOR-TYPE
               ADD 1             TO SOR-SEQ
           ELSE
               MOVE W35-KEY      TO SOR-KEY
               PERFORM READ-SORDER-LOCK THRU READ-SORDER-EXIT
               MOVE 0            TO WS-ACTION
               PERFORM AY17 THRU AY59.
             MOVE W40-AD1        TO SOR-AD1.
             MOVE W40-AD2        TO SOR-AD2.
             MOVE W40-AD3        TO SOR-AD3.
             MOVE W40-AD4        TO SOR-AD4.
             MOVE W40-PC         TO SOR-PC.
           IF W35-KEY = SPACES
               PERFORM WRITE-SORDER THRU WRITE-SORDER-EXIT
               MOVE 1            TO WS-ACTION
               PERFORM AY17 THRU AY59
           ELSE
               PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
      *
      *    ****    Q U O T E   S A L U T A T I O N   &  T E X T
      *
       BD256.
      MOVE 99800   TO SOR-SQN.
      MOVE 7   TO SOR-TYPE.
      *
      *    ****    C H E C K   I F   R E C O R D   E X I S T S
      *
      PERFORM READ-SORDER THRU READ-SORDER-EXIT.
      *
      *    ****    I F  N O   E X I S T I N G  R E C O R D
      *     C R E A T E   O N E
      *
    IF WS-F-ERROR = 20
        PERFORM WRITE-SORDER THRU WRITE-SORDER-EXIT.
      INITIALIZE SOR-DET5.
      MOVE W34-SALUTATION TO SOR-TEXT.
      MOVE W34-SIGNATURE  TO SOR-SIGN,
      PERFORM BD200.
      MOVE 1   TO WS-S6.
      MOVE 10   TO WS-S7.

       BD257.
    IF W32-REMARKS (WS-S7) = SPACES
        IF WS-S7 > 1
     SUBTRACT 1  FROM WS-S7
     GO TO BD257.
      MOVE 8   TO SOR-TYPE.
      *
      *    ****    C H E C K   I F   R E C O R D   E X I S T S
      *
       BD258.
      MOVE WS-S6   TO SOR-SUB.
      PERFORM READ-SORDER THRU READ-SORDER-EXIT.
      *
      *    ****    I F  N O   E X I S T I N G  R E C O R D
      *     C R E A T E   O N E
      *
    IF WS-F-ERROR = 20
        PERFORM WRITE-SORDER THRU WRITE-SORDER-EXIT.
      MOVE W32-REMARKS (WS-S6)
     TO SOR-TEXT.
      MOVE WS-S6   TO SOR-SUB.
      PERFORM BD200.
    IF WS-S6 < WS-S7
        ADD 1   TO WS-S6
        GO TO BD258.
      *
      *    ****    Q U O T E   T O T A L S
      *
             MOVE 99900          TO SOR-SQN.
             PERFORM READ-SORDER THRU READ-SORDER-EXIT.
             MOVE 0              TO WS-ACTION.
             PERFORM AY17 THRU AY59.
             MOVE W90-DB         TO SOR-SUBTOT.
             MULTIPLY -1 BY W90-TDISC
                                 GIVING SOR-DSCNT
      MOVE W90-EXCTOT  TO SOR-TAXABLE.
             MOVE W90-NONTAX     TO SOR-NON-TXBL.
             MOVE W90-TTAX       TO SOR-TAX.
             MOVE W90-TOTAL      TO SOR-TOT.
             MOVE W90-BAL        TO SOR-BAL.
             MOVE W90-PMETH      TO SOR-PMETH.
             MOVE W40-STATUS     TO SOR-STATUS.
             MOVE W40-HTEL       TO SOR-HTEL.
             MOVE W40-ID         TO SOR-ID.
             PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
      *
      *    ****    E N D   O F F   T H E   R E C O V E R Y   L O G
      *
             PERFORM AY70 THRU AY999.
      *
      *    ****    C L O S E   F I L E S
      *            ( FORCE SYSTEM TO WRITE DETAILS TO DISK )
      *
            CLOSE SALE.
            PERFORM CLEAR-L50.
           IF WS-OPTION = "P"
               GO TO BD261.

       BD260.
            MOVE "Print quotation (Y/N)?  [ ]"
     TO WS-ERR-MES.
            MOVE "Y"   TO WS-OPTION.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "Y" OR "N".
      *
      *    ****    T E S T   F O R   I N D I V I D U A L
      *
       BD261.
          IF W80-TYPE NOT = 1
               GO TO BD266.
             MOVE DEB-NAME       TO W60-NAME.
             PERFORM CA165-NAME.
      *
      *    ****    T I T L E
      *
           IF DEB-LANG = "A"
               MOVE W75-ATITLE (DEB-TITLE)
                                 TO W60-NAME
           ELSE
               MOVE W75-ETITLE (DEB-TITLE)
                                 TO W60-NAME.
             MOVE 7              TO WS-S6.

       BD262.
           IF W60-NCHAR (WS-S6) NOT = SPACE
               GO TO BD263.
             SUBTRACT 1          FROM WS-S6.
           IF WS-S6 > 0
               GO TO BD262.
      *
      *    ****    F I R S T   I N I T I A L
      *
       BD263.
            MOVE 1   TO WS-S5.
             ADD 2               TO WS-S6.
             MOVE W40-NCHAR (25) TO W60-NCHAR (WS-S6).
             ADD 1               TO WS-S6.
             MOVE "."            TO W60-NCHAR (WS-S6).
           IF W40-NCHAR (49) = SPACE
               GO TO BD264.
      *
      *    ****    S E C O N D   I N I T I A L
      *
             ADD 1               TO WS-S6.
             MOVE W40-NCHAR (49) TO W60-NCHAR (WS-S6).
             ADD 1               TO WS-S6.
             MOVE "."            TO W60-NCHAR (WS-S6).

       BD264.
             ADD 2               TO WS-S6.
      *
      *    ****    S U R N A M E
      *
       BD265.
            MOVE W40-NCHAR (WS-S5)
                                 TO W60-NCHAR (WS-S6).
           IF WS-S5 < 24
               ADD 1             TO WS-S5
               ADD 1             TO WS-S6
               GO TO BD265.
             MOVE W60-NAME       TO W40-NAME.

       BD266.
             MOVE ZERO           TO WS-PAGE.
             MOVE SPACES         TO W40-PC1 W40-PC2.
             MOVE DEB-PADDR      TO W60-ADDRESS.
             PERFORM CA175-ADDRESS.
           IF NOT (DEB-PPOST = ZERO OR 9999)
               MOVE DEB-PPOST    TO W40-PC1.
           IF W40-ADDRESS1 = SPACES
               MOVE DEB-ADDRESS  TO W60-ADDRESS
               PERFORM CA175-ADDRESS
               MOVE SPACES       TO W40-PC1
               IF NOT (DEB-POST = ZERO OR 9999)
                   MOVE DEB-POST TO W40-PC1.
           IF W40-ADD4 = SPACES
               MOVE W40-PC1      TO W40-ADD4
               MOVE SPACES       TO W40-PC1.
           IF W40-ADD3 = SPACES
               MOVE W40-ADD4     TO W40-ADD3
               MOVE SPACES       TO W40-ADD4.
           IF W40-ADD2 = SPACES
               MOVE W40-ADD3     TO W40-ADD2
               MOVE SPACES       TO W40-ADD3.
           IF W40-ADD1 = SPACES
               MOVE W40-ADD2     TO W40-ADD1
               MOVE SPACES       TO W40-ADD2.
            GO TO BD269.

       BD268.
      MOVE "Q"   TO WS-OPTION.

       BD269.
      EXIT.
      *
      *    ****    A M E N D   Q U O T E   D E T A I L S
      *
       BD270A  SECTION 56.
       BD270.
      MOVE "Amend - 'H'eader, 'S'ales details or 'E'xit"
     TO WS-ERR-MES.
      MOVE "S"   TO WS-OPTION.

       BD271.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "E" OR "H" OR "S".
           IF WS-OPTION = "S"
               GO TO BD272
           ELSE
           IF WS-OPTION = "E"
        GO TO BD590.
             MOVE W10-SORD       TO SOR-REF.
             MOVE 100            TO SOR-SQN.
             MOVE ZERO           TO WS-ERROR.
             PERFORM READ-SORDER THRU READ-SORDER-EXIT.

       BD271A.
      MOVE "Amend-'O'rd No,'S'alesman,'T'el.,'C'ont.,'E'xit"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.

       BD271B.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "C" OR "E" OR "O" OR "S" OR "T".
           IF WS-OPTION = "E"
        GO TO BD590.
           IF WS-OPTION = "C"
               ACCEPT W40-CONTACT AT 0528
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 UPDATE AUTO
               MOVE W40-CONTACT  TO SOR-CONTACT
               PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT
        GO TO BD271A
           ELSE
           IF WS-OPTION = "O"
               ACCEPT W90-ORD AT 0368
        WITH FOREGROUND-COLOR 3 HIGHLIGHT UPDATE AUTO
               MOVE W90-ORD      TO SOR-ORD
               PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT
        GO TO BD271A
           ELSE
           IF WS-OPTION = "T"
               ACCEPT W40-TEL AT 0466
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 5 UPDATE AUTO
               MOVE W40-TEL      TO SOR-TEL
               PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT
        GO TO BD271A.
       BD271C.
            ACCEPT W10-SMAN AT 0411
      WITH FOREGROUND-COLOR 3 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
           IF W10-SMAN > 50
               GO TO BD271C.
           IF W10-SMAN = ZERO
        DISPLAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" AT 0420
                       WITH FOREGROUND-COLOR 3
               MOVE ZERO         TO SOR-SMAN
               PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT
              GO TO BD271A.
             ADD 100 W10-SMAN    GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-SALESMAN = SPACES
              MOVE "Invalid Salesman Number"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              GO TO BD271C.
            DISPLAY PAR-SALESMAN AT 0420
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
             MOVE W10-SMAN       TO SOR-SMAN.
             PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
             GO TO BD271A.

       BD272.
             MOVE 8              TO LIN.
             PERFORM BD115 UNTIL LIN > 18.
             MOVE 8              TO LIN.
             MOVE 1              TO WS-LINES WS-FIRST.

       BD275.
            MOVE WS-FIRST  TO WS-SALKEY WS-SALPOS.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
       BD280.
          IF WS-SALKEY > WS-LAST
        SUBTRACT 1  FROM WS-SALKEY
              GO TO BD284.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
      MOVE SAL-EXT-ITEM  TO W10-EXT-ITEM.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
          IF SAL-EXT-ITEM = SPACES
              DISPLAY (LIN, 21) SAL-SDSC
   WITH FOREGROUND-COLOR 7 HIGHLIGHT
        BACKGROUND-COLOR 0
               IF SAL-SUPER = 1
     DISPLAY "(I)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR 6 HIGHLIGHT
               ELSE
     DISPLAY "(R)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
               END-IF
               GO TO BD282.
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT AT LINE LIN
    ELSE
      *
      *    *****   A L P H A - N U M E R I C   C O D E  *****
      *
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT LINE LIN
        ELSE
           DISPLAY S07 AT LINE LIN.
            DISPLAY (LIN, 21) SAL-SDSC
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
             MOVE SAL-QNT        TO W10-QUANT.
             DISPLAY S26-QNT AT LINE LIN.
             MOVE SAL-SELL       TO W10-SELL.
             DISPLAY S26-PRC AT LINE LIN.
             MOVE SAL-VAL        TO W100-EXT.
            DISPLAY ( , 70) W100-EXT
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
      *
      *    ****    I S   T H E   S C R E E N   F U L L
      *
       BD282.
           IF LIN < 18
               ADD 1             TO LIN WS-SALKEY WS-LINES
               GO TO BD280.

       BD284.
      MOVE 7   TO LIN.
      PERFORM CLEAR-L50.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
       BD285.
001020      DISPLAY " " AT 2503 WITH FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 3
             " highlight," WITH FOREGROUND-COLOR 1
       BACKGROUND-COLOR 3
       "PgUp" WITH FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 3
       "/" WITH FOREGROUND-COLOR 1
         BACKGROUND-COLOR 3
       "PgDn" WITH FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 3
       "," WITH FOREGROUND-COLOR 1
    BACKGROUND-COLOR 3
             "A" WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 3
             "mend," WITH FOREGROUND-COLOR 1
           BACKGROUND-COLOR 3
             "D" WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 3
             "elete or " WITH FOREGROUND-COLOR 1
           BACKGROUND-COLOR 3
             "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
    BACKGROUND-COLOR 3
       "ape" WITH FOREGROUND-COLOR 1
    BACKGROUND-COLOR 3.
      *
      *    ****    S A V E   C U R R E N T   P A G E
      *
      PERFORM SAVE-SCREEN-2.
      *
      *    ****    S C R E E N  A T T R I B U T E S
      *
       BD289.
             MOVE 3920           TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
           W42-ATTRIB2
           W41-LENGTH
     RETURNING WS-STATUS.
      MOVE 4000   TO W41-LENGTH.
      *
      *    ****    G E T   U S E R   O P T I O N
      *
       BD290.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
       GO TO BD292
   WHEN DOWN-KEY
             GO TO BD293
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO BD290
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       PERFORM CLEAR-L50
       GO TO BD582
      *
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *
   WHEN PAGE-DOWN
             GO TO BD305
      *
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *
   WHEN PAGE-UP
             GO TO BD300
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO BD290
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        IF (WS-OPTION = "A" OR "D") AND
    (LIN > 7)
     PERFORM CLEAR-L50
     GO TO BD310.
      CALL X"E5".
            GO TO BD290.
      *
      *    ****    U P  K E Y  ( M O V E   H I G H L I G H T  U P )
      *
       BD292.
    IF (WS-SALPOS = 1) OR (LIN < 8)
        GO TO BD294.
      SUBTRACT 1   FROM WS-SALPOS.
    IF LIN > 8
        SUBTRACT 1  FROM LIN
        GO TO BD294.
      MOVE 18   TO WS-S7.
      *
      *    ****     M O V E   T H E   D E T A I L S   D O W N
      *      O N E   L I N E
      *
       BD292-LOOP.
      SUBTRACT 1 FROM WS-S7
     GIVING WS-S8.
      MOVE W42-LINES (WS-S8)
     TO W42-LINES (WS-S7).
      MOVE W43-LINE (WS-S8)
     TO W43-LINE (WS-S7).
    IF WS-S7 > 9
        SUBTRACT 1  FROM WS-S7
        GO TO BD292-LOOP.
      MOVE WS-SALPOS  TO WS-SALKEY WS-FIRST.
      *
      *    ****    C L E A R   F I R S T   O R  L A S T
      *     D E T A I L  L I N E.
      *
       BD292-DISPLAY.
      PERFORM RESTORE-SCREEN-2.
            DISPLAY (LIN, 1) WS-INV1 WITH BACKGROUND-COLOR 0
        FOREGROUND-COLOR 3
         WS-INV78 WITH FOREGROUND-COLOR 3
         WS-INV80 WITH BACKGROUND-COLOR 0
         FOREGROUND-COLOR 3.
      *
      *    ****    G E T   N E X T   O R   P R E V I O U S
      *     D E T A I L  L I N E.
      *
            PERFORM READ-SALE THRU READ-SALE-EXIT.
      MOVE SAL-EXT-ITEM  TO W10-EXT-ITEM.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
          IF SAL-EXT-ITEM = SPACES
              DISPLAY (LIN, 21) SAL-SDSC
   WITH FOREGROUND-COLOR 7 HIGHLIGHT
        BACKGROUND-COLOR 0
        IF SAL-SUPER = 1
     DISPLAY "(I)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR 6 HIGHLIGHT
               ELSE
     DISPLAY "(R)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
        END-IF
              GO TO BD292-END-DISP.
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT AT LINE LIN
    ELSE
      *
      *    *****   A L P H A - N U M E R I C   C O D E  *****
      *
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT LINE LIN
        ELSE
           DISPLAY S07 AT LINE LIN.
            DISPLAY (LIN, 21) SAL-SDSC
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
             MOVE SAL-QNT        TO W10-QUANT.
             DISPLAY S26-QNT AT LINE LIN.
             MOVE SAL-SELL       TO W10-SELL.
             DISPLAY S26-PRC AT LINE LIN.
             MOVE SAL-VAL        TO W100-EXT.
            DISPLAY ( , 70) W100-EXT
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.

       BD292-END-DISP.
      PERFORM SAVE-SCREEN-2.
      GO TO BD294.
      *
      *    ****    D O W N   K E Y   ( M O V E  H I G H L I G H T
      *           D O W N )
      *
       BD293.
    IF (WS-SALPOS = WS-LAST) AND (LIN > 7)
        GO TO BD294.
      *
      *    ****    WS-SALPOS CONTAINS KEY OF HIGHLIGHTED ITEM
      *
    IF LIN > 7
        ADD 1   TO WS-SALPOS.
    IF LIN < 18
        ADD 1   TO LIN
        GO TO BD294.
      MOVE 8   TO WS-S7.
      *
      *    ****     M O V E   T H E   D E T A I L S   U P   O N E
      *      L I N E
      *
       BD293-LOOP.
      ADD 1 WS-S7  GIVING WS-S8.
      MOVE W42-LINES (WS-S8)
     TO W42-LINES (WS-S7).
      MOVE W43-LINE (WS-S8)
     TO W43-LINE (WS-S7).
    IF WS-S8 < 18
        ADD 1   TO WS-S7
        GO TO BD293-LOOP.
      ADD 1   TO WS-FIRST.
      MOVE WS-SALPOS  TO WS-SALKEY.
      GO TO BD292-DISPLAY.
      *
      *    ****    H I G H L I G H T   C U R R E N T   L I N E
      *
       BD294.
            PERFORM BD289.
    IF LIN > 7
              DISPLAY (LIN, 1) W43-DET (LIN)
   WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
            GO TO BD290.
      *
      *    ****    C L E A R   S C R E E N
      *
       BD295.
            MOVE 8   TO LIN.
            PERFORM BD115 UNTIL LIN > 18.
            MOVE 8   TO LIN.
             MOVE 1              TO WS-LINES.
      *
      *    ****   P R E V I O U S   S C R E E N   R E Q U I R E D
      *
       BD300.
    IF WS-FIRST = 1
        GO TO BD290.
    IF WS-FIRST > 11
        SUBTRACT 11  FROM WS-FIRST
    ELSE
        MOVE 1   TO WS-FIRST.
      PERFORM BD295.
      GO TO BD275.
      *
      *    ****    N E X T   S C R E E N   R E Q U I R E D
      *
       BD305.
    IF (WS-FIRST + 11) > WS-LAST
        GO TO BD290.
      ADD 11   TO WS-FIRST.
      PERFORM BD295.
      GO TO BD275.
      *
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *
       BD310.
          IF WS-OPTION = "A"
              GO TO BD320.
      *
      *    ****    M A K E   S U R E   T H A T   A N   I T E M
      *     M U S T   B E   D E L E T E D
      *
       BD315.
            MOVE "Are you sure Y/N?  [ ]"
     TO WS-ERR-MES.
            MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE
       UNTIL WS-OPTION = "N" OR "Y".
           IF WS-OPTION = "N"
              GO TO BD285.
      *
      *    ****    C H A N G E  B A C K   T O  Delete
      *
             MOVE "D"            TO WS-OPTION.
      *
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *
       BD320.
            MOVE WS-SALPOS  TO WS-SALKEY.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
          IF SAL-EXT-ITEM = SPACES
              GO TO BD322.
            MOVE SAL-EXT-ITEM  TO W10-EXT-ITEM STK-CODE.
            PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             MOVE SAL-QNT        TO W10-QUANT.
             MOVE SAL-COST       TO W10-COST.
             MOVE SAL-SELL       TO W10-SELL.
             MOVE SAL-DISC       TO W90-CDISC.
             MOVE SAL-DSC        TO WS-DISC.
             MOVE SAL-VAL        TO W25-VALUE.
      MOVE SAL-VAT-SUB  TO WS-VAT-SUB.
      MOVE SAL-PACK  TO WS-P1.
      *
      *    ****    R E D U C E   T O T A L S
      *
             SUBTRACT W25-VALUE  FROM W90-DB.
             SUBTRACT W90-CDISC  FROM W90-LDISC.
             SUBTRACT SAL-NONTAX FROM W90-NONTAX.
    IF WS-VAT-SUB > ZERO
              SUBTRACT SAL-TAXVAL FROM W90-TAXVAL(WS-VAT-SUB)
        SUBTRACT SAL-VAT  FROM W90-VAT(SAL-VAT-SUB).
          IF (STK-IND < 2) OR (STK-IND = 3)
               COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *
      *    ****    I S   I T   A N   A M E N D M E N T
      *
       BD322.
      PERFORM BD289.
          IF WS-OPTION = "A"
              GO TO BD325.
      *
      *    ****    R E - D I S P L A Y   T O T A L S
      *
          IF SAL-EXT-ITEM NOT = SPACES
              PERFORM BD100 THRU BD102.
      *
      *    ****    S T O R E   R E F E R E N C E   T O  B E
      *     D E L E T E D   -   S E E   BD190
      *
      ADD 1   TO W14-SUB.
      MOVE SAL-SQN  TO W14-SQN(W14-SUB).
      *
      *    ****    D E L E T E   I T E M   F R O M   S A L E   F I L E
      *
       BD324.
            MOVE WS-SALKEY  TO WS-SALSTR.
             ADD 1               TO WS-SALKEY.
           IF WS-SALKEY > WS-LAST
              SUBTRACT 1  FROM WS-LAST
        MOVE WS-LAST  TO WS-SALKEY
              GO TO BD272.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
             SUBTRACT 1          FROM SAL-CODE.
             MOVE WS-SALSTR      TO WS-SALKEY.
             PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
             ADD 1               TO WS-SALKEY.
            GO TO BD324.

       BD325.
      PERFORM BD289.
          IF SAL-EXT-ITEM = SPACES
              ACCEPT (LIN, 21) SAL-SDSC
         WITH FOREGROUND-COLOR 7 HIGHLIGHT
             BACKGROUND-COLOR 0 UPDATE AUTO
              GO TO BD581.

       BD330.
            PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
    IF WS-EXT-STK = "Y"
        ACCEPT S07-EXT AT LINE LIN
    ELSE
      *
      *        ***** ALPHA-NUMERIC CODE *****
      *
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     ACCEPT S07-ITM AT LINE LIN
        ELSE
           ACCEPT S07 AT LINE LIN
        END-IF
    ELSE
      *
      *    ***** NUMERIC CODES APPLY *****
      *
    IF W10-SLNGTH = 3
        ACCEPT S07-3 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S07-4 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S07-5 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S07-6 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S07-7 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S07-8 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S07-9 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S07-10 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S07-11 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S07-12 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S07-13 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S07-14 AT LINE LIN.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "I"  TO W10-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO W10-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO W10-ETYPE
   WHEN F5-KEY
       MOVE "2"  TO W10-ETYPE
   WHEN F6-KEY
       MOVE "X"  TO W10-ETYPE
   WHEN F9-KEY
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Lookup using any word or part in the Stock Description.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       MOVE "W"  TO W10-ETYPE
   WHEN OTHER
                     CALL X"E5"
       GO TO BD330
               END-EVALUATE
              PERFORM AA50 THRU AA59
        PERFORM BD56
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT AT LINE LIN
        ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM AT LINE LIN
     ELSE
               DISPLAY S07 AT LINE LIN.
              IF W10-EXT-ITEM = SPACES
     MOVE SAL-EXT-ITEM
     TO W10-EXT-ITEM
     GO TO BD330.

       BD335.
             PERFORM CA000.
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT AT LINE LIN
    ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT LINE LIN
        ELSE
           DISPLAY S07 AT LINE LIN.
          IF WS-F-ERROR = 22
        MOVE "Z"   TO W10-ETYPE
              PERFORM AA50 THRU AA59
        PERFORM BD56
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
        IF WS-EXT-STK = "Y"
     DISPLAY S07-EXT AT LINE LIN
        ELSE
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-USE-ITM = "Y"
         DISPLAY S07-ITM AT LINE LIN
     ELSE
               DISPLAY S07 AT LINE LIN
     END-IF
        END-IF
              IF W10-EXT-ITEM = SPACES
     MOVE SAL-EXT-ITEM
     TO W10-EXT-ITEM
           GO TO BD330
               ELSE
     GO TO BD335.
            MOVE STK-CODE  TO SAL-EXT-ITEM.
            PERFORM CLEAR-L50.
      *
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *
       BD340.
           IF STK-IND = 3
               MOVE ZERO         TO STK-AVGE
              ACCEPT (LIN, 21) W10-SDSC
         WITH FOREGROUND-COLOR 7 HIGHLIGHT
             BACKGROUND-COLOR 0 UPDATE AUTO
               DISPLAY "Index Unit cost" AT 2512
         WITH BACKGROUND-COLOR 3
        FOREGROUND-COLOR 6 HIGHLIGHT
               ACCEPT S26-AVGE AT LINE 25
           ELSE
               MOVE STK-DESC     TO W10-DESC
              DISPLAY W10-SDSC AT LINE LIN COLUMN 21
         WITH FOREGROUND-COLOR 3 HIGHLIGHT
       BACKGROUND-COLOR 0.
           IF STK-IND = 3
               IF STK-AVGE = ZERO
           MOVE "Unit cost is zero"
     TO WS-ERR-STRING
           PERFORM ERROR-MESSAGE
           GO TO BD340.
            PERFORM CLEAR-L50.
      *
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *
            MOVE W10-EXT-ITEM  TO SAL-EXT-ITEM.
             MOVE W10-DESC       TO SAL-MSER.
             MOVE STK-AVGE       TO SAL-COST W10-COST.
      *
      *    ****    A M E N D   Q U A N T I T Y
      *
             ACCEPT S26-QNT AT LINE LIN.
           IF W10-QUANT = ZERO
              MOVE "Quantity MAY NOT be zero"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               MOVE SAL-QNT      TO W10-QUANT
              GO TO BD325.
      *
      *    ****    A M E N D   S E L L I N G   P R I C E
      *
      PERFORM BD66A THRU BD75.
      *
      *    ****    R E C A L C U L A T E   V A L U E S
      *
             MULTIPLY W10-SELL BY W10-QUANT
                                 GIVING W25-VALUE.
             MOVE W25-VALUE      TO W100-EXT.
            DISPLAY (LIN, 70) W100-EXT
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
              GO TO BD325.
      *
      *    ****    U P D A T E   S A L E   F I L E   R E C O R D
      *
             MOVE W10-QUANT      TO SAL-QNT.
             MOVE W10-SELL       TO SAL-SELL.
             MOVE W25-VALUE      TO SAL-VAL.
           IF WS-DISC = ZERO
              GO TO BD580.
             COMPUTE W90-CDISC = (W25-VALUE * WS-DISC) / 100.
             MOVE W90-CDISC      TO SAL-DISC.
      *
      *    ****    R E - C A L C U L A T E   &   D I S P L A Y
      *            T O T A L S
      *
       BD580.
            PERFORM BD95 THRU BD102.

       BD581.
             PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
            GO TO BD272.
      *
      *    ****    C L E A R   D E T A I L   L I N E S
      *
       BD582.
      PERFORM BD295.
      *
      *    ****    D I S P L A Y   L A S T   P A G E
      *
    IF WS-LAST > 10
        SUBTRACT 9 FROM WS-LAST
     GIVING WS-SALKEY
    ELSE
        MOVE 1   TO WS-SALKEY.

       BD583.
            PERFORM READ-SALE THRU READ-SALE-EXIT.
      MOVE SAL-EXT-ITEM  TO W10-EXT-ITEM.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
          IF SAL-EXT-ITEM = SPACES
              DISPLAY (LIN, 21) SAL-SDSC
   WITH FOREGROUND-COLOR 7 HIGHLIGHT
        BACKGROUND-COLOR 0
        IF SAL-SUPER = 1
     DISPLAY "(I)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR 6 HIGHLIGHT
               ELSE
     DISPLAY "(R)" AT LINE LIN COLUMN 5
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
        END-IF
              GO TO BD584.
      *
      *        *****   E X T E N D E D  C O D E   *****
      *
    IF WS-EXT-STK = "Y"
        DISPLAY S07-EXT AT LINE LIN
    ELSE
      *
      *    *****   A L P H A - N U M E R I C   C O D E  *****
      *
    IF W10-STCK = "A"
        IF WS-USE-ITM = "Y"
     DISPLAY S07-ITM AT LINE LIN
        ELSE
           DISPLAY S07 AT LINE LIN.
            DISPLAY (LIN, 21) SAL-SDSC
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
            MOVE SAL-QNT  TO W10-QUANT.
             DISPLAY S26-QNT AT LINE LIN.
             MOVE SAL-SELL       TO W10-SELL.
             DISPLAY S26-PRC AT LINE LIN.
             MOVE SAL-VAL        TO W100-EXT.
            DISPLAY ( , 70) W100-EXT
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
       BD584.
      ADD 1   TO LIN.
          IF WS-SALKEY < WS-LAST
        ADD 1   TO WS-SALKEY
              GO TO BD583.

       BD590.
             EXIT.

       BD591A        SECTION 57.
       BD591.
      PERFORM SAVE-SCREEN.
     MOVE 0815   TO CPOS.

       BD592.
      DISPLAY
   "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°"
    AT CPOS.
    IF CLIN < 18
        ADD 1   TO CLIN
       GO TO BD592.
      DISPLAY S40.
      PERFORM BB15 THRU BB29.
      PERFORM RESTORE-SCREEN.

       BD593.
      EXIT.
      *
      *    ****    A M E N D   Q U O T A T I O N   T E X T
      *
       BE000        SECTION 53.
       BE00.
      PERFORM SAVE-SCREEN-3.
      MOVE 7   TO SHADE-ROW.
      MOVE 10   TO SHADE-COL.
      MOVE 63   TO SHADE-WIDTH.
      MOVE 14   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 0608
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "³   Q U O T A T I O N   S A L U T A T I O N   A N
      -       "D   T E X T   ³" AT 0708
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" AT 0808
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "³" AT 0908
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
        W34-SALUTATION
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1008
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
        W32-REMARKS (1)
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1108
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
        W32-REMARKS (2)
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1208
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
        W32-REMARKS (3)
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1308
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
        W32-REMARKS (4)
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1408
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
        W32-REMARKS (5)
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1508
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
        W32-REMARKS (6)
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1608
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
        W32-REMARKS (7)
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1708
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
        W32-REMARKS (8)
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1808
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
        W32-REMARKS (9)
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1908
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
        W32-REMARKS (10)
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
      -       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 2008
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      PERFORM SCREEN-SHADOW.

       BE05.
            MOVE "Amend salutation/text (Y/N)?  [ ]"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      MOVE 21   TO SLIN.
      PERFORM OPT-SETUP THRU OPT-EXIT
       UNTIL WS-OPTION = "Y" OR "N".
    IF WS-OPTION = "N"
        GO TO BE99.

       BE10.
      ACCEPT S42.
      MOVE 21   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
          IF WS-OPTION = "N"
              GO TO BE10.

       BE99.
      PERFORM RESTORE-SCREEN-3.

       BE999.
      EXIT.

      /
       ZA000-INIT              SECTION 90.
       ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY.
      MOVE LS-USUB  TO WS-USUB.
            PERFORM ERASE-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      COPY FUNCTION.PRO.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Setup the file ID's including path names           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    *************************************************************
      *    ****    P A R A M E T E R   F I L E      ****
      *    *************************************************************
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
      *    *************************************************************
      *    ****  D E B T O R   A C C O U N T   ( A / R )   F I L E  ****
      *    *************************************************************
      MOVE "DEBTOR"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-DEBTOR.
      *    *************************************************************
      *    ****     D E P A R T M E N T  F I L E     ****
      *    *************************************************************
      MOVE "DEPART"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-DEPART.
      *    *************************************************************
      *    ****      C O N T R O L   F I L E      ****
      *    *************************************************************
      MOVE "NETWORK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-NETWORK.
      MOVE "PRNDISK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      INSPECT AFID-PATH REPLACING FIRST "XXX"
          BY LS-USER.
      MOVE AFID-PATH  TO W02-PRNDISK.
      *    *************************************************************
      *    ****      R E C O V E R Y   F I L E      ****
      *    *************************************************************
      MOVE "RECOVER"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      INSPECT AFID-PATH REPLACING FIRST "XXX"
          BY LS-USER.
      MOVE AFID-PATH  TO W02-RECOVER.
      MOVE "SALE"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      INSPECT AFID-PATH REPLACING FIRST "XXX"
          BY LS-USER.
      MOVE AFID-PATH  TO W02-SALE.
      MOVE "SORDER"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SORDER.
      *    *************************************************************
      *    ****      N E T W O R K   (S H A R E D)   F I L E     ****
      *    *************************************************************
      MOVE "SHARED"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SHARED.
      *    *************************************************************
      *    ****    S T O C K   P A R T S   F I L E     ****
      *    *************************************************************
      MOVE "SPARTS"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SPARTS.
      *    *************************************************************
      *    ****     S T O C K   M E M O  F I L E     ****
      *    *************************************************************
      MOVE "STKMEM"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STKMEM.
      *    *************************************************************
      *    ****         S T O C K   F I L E      ****
      *    *************************************************************
      MOVE "STOCKF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STOCKF.
      MOVE "SYSUSE"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SYSUSER.

       ZA01.
             MOVE 3              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF NOT (PAR-STOCK = "A" OR "N")
        MOVE "A"   TO PAR-STOCK.
      MOVE PAR-STOCK  TO W10-STCK.
    IF PAR-STOCK = "N"
        MOVE PAR-SLNGTH  TO W10-SLNGTH
    ELSE
        MOVE 14   TO W10-SLNGTH.
    IF W10-SLNGTH < 3
        MOVE 3   TO W10-SLNGTH
    ELSE
    IF W10-SLNGTH > 14
        MOVE 14   TO W10-SLNGTH.
      MOVE PAR-PACKS  TO WS-USE-PACKS.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.

       ZA01B.
             MOVE PAR-VAT-REG    TO W05-VATNO.
            OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
            OPEN OUTPUT SALE.
             CLOSE SALE.
             OPEN I-O SALE.
            MOVE 5   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-DISCOUNT  TO W15-DISCOUNT.
      MOVE 1   TO W05-V-RATE.

       ZA05-VAT.
      MOVE W05-VAT-CODE  TO DPT-CODE.
      PERFORM READ-DEPART THRU READ-DEPART-EXIT.
      MOVE DPT-R-DATE  TO WS-VAT-DATE(W05-V-RATE).
      MOVE DPT-RATE  TO W05-VAT(W05-V-RATE).
      ADD 6 W05-V-RATE  GIVING WS-S1.
      MOVE DPT-P-RATE  TO W05-VAT(WS-S1).
    IF W05-V-RATE < 6
        ADD 1   TO W05-V-RATE
        GO TO ZA05-VAT.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-TELEPHONE  TO W95-TEL.
           IF PAR-STMINV = "I" OR "X"
               MOVE "N"          TO WS-HEAD
           ELSE
               MOVE "Y"          TO WS-HEAD.
             ADD 1               TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-ADDRESS1   TO W95-ADD1.
             MOVE PAR-ADDRESS2   TO W95-ADD2.
             MOVE PAR-ADDRESS3   TO W95-ADD3.
             MOVE PAR-ADDRESS4   TO W95-ADD4.
             MOVE PAR-POST-CDE   TO W95-POST.
           IF W95-ADD4 = SPACES
               MOVE W95-PC       TO W95-ADD4
               MOVE SPACES       TO W95-PC.
           IF W95-ADD3 = SPACES
               MOVE W95-ADD4     TO W95-ADD3
               MOVE SPACES       TO W95-ADD4.
           IF W95-ADD2 = SPACES
               MOVE W95-ADD3     TO W95-ADD2
               MOVE SPACES       TO W95-ADD3.
           IF W95-ADD1 = SPACES
               MOVE W95-ADD2     TO W95-ADD1
               MOVE SPACES       TO W95-ADD2.
      MOVE 266   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-QTE1  TO W30-REMARKS(1).
      MOVE PAR-QTE2  TO W30-REMARKS(2).
             ADD 1               TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-QTE1  TO W30-REMARKS(3).
      MOVE PAR-QTE2  TO W30-REMARKS(4).
            MOVE 1   TO WS-NETKEY.
            PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE NET-DEBTOR     TO W70-DEBT.
    IF W70-ORD-SYS = "Y"
      *        GET HARD DRIVE ID AND IF LOCAL DRIVE (C)
      *        CHANGE ORDER FILE-ID TO SORD??? (??? = USER NO)
        CALL "PC_READ_DRIVE" USING WS-LOCAL-DRIVE
        RETURNING WS-STATUS
        CALL "CBL_TOUPPER" USING WS-LOCAL-DRIVE
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        IF WS-LOCAL-DRIVE = "C"
     CLOSE SORDER
     MOVE W02-SORDER
     TO WS-SORDER-STR
     MOVE LS0-NO  TO WS-USER-ID
     MOVE WS-LOCAL-ID
     TO W02-SORDER
     MOVE WS-LOCAL-DRIVE
     TO WS-LOCAL-DRIVE2
     MOVE WS-LOCAL-ID2
     TO W02-SYSUSER
     OPEN I-O SORDER
     OPEN I-O SYSUSER.
      MOVE 275   TO WS-PARKEY.
            READ PARAM IGNORE LOCK.
          IF NOT (WS-STATUS = "00")
              GO TO ZA10.
      MOVE PAR-QTE-FMT  TO W34-FORMAT.
      MOVE PAR-QTE-MRG  TO W34-MARGIN.
      MOVE PAR-QTE-SAL  TO W34-SALUTATION.
      MOVE PAR-QTE-SIG  TO W34-SIGNATURE.
      ADD 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-QHDR1  TO W32-REMARKS(1).
      MOVE PAR-QHDR2  TO W32-REMARKS(2).
      ADD 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-QHDR1  TO W32-REMARKS(3).
      MOVE PAR-QHDR2  TO W32-REMARKS(4).
      ADD 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-QHDR1  TO W32-REMARKS(5).
      MOVE PAR-QHDR2  TO W32-REMARKS(6).
      ADD 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-QHDR1  TO W32-REMARKS(7).
      MOVE PAR-QHDR2  TO W32-REMARKS(8).
      ADD 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-QHDR1  TO W32-REMARKS(9).
      MOVE PAR-QHDR2  TO W32-REMARKS(10).
            GO TO ZA999.

       ZA10.
      MOVE "C"   TO PAR-QTE-FMT W34-FORMAT.
      MOVE 6   TO PAR-QTE-MRG W34-MARGIN.
      MOVE "Dear Sir/Madam,"
     TO PAR-QTE-SAL W34-SALUTATION.
      MOVE "Yours sincerely"
     TO PAR-QTE-SIG W34-SIGNATURE.
            WRITE PAR-RECORD1.
      ADD 1   TO WS-PARKEY.
      MOVE SPACES  TO PAR-QHDR1 W32-REMARKS(1).
      MOVE SPACES  TO PAR-QHDR2 W32-REMARKS(2).
            WRITE PAR-RECORD1.
      ADD 1   TO WS-PARKEY.
      MOVE SPACES  TO PAR-QHDR1 W32-REMARKS(3).
      MOVE SPACES  TO PAR-QHDR2 W32-REMARKS(4).
            WRITE PAR-RECORD1.
      ADD 1   TO WS-PARKEY.
      MOVE SPACES  TO PAR-QHDR1 W32-REMARKS(5).
      MOVE SPACES  TO PAR-QHDR2 W32-REMARKS(6).
            WRITE PAR-RECORD1.
      ADD 1   TO WS-PARKEY.
      MOVE SPACES  TO PAR-QHDR1 W32-REMARKS(7).
      MOVE SPACES  TO PAR-QHDR2 W32-REMARKS(8).
            WRITE PAR-RECORD1.
      ADD 1   TO WS-PARKEY.
      MOVE SPACES  TO PAR-QHDR1 W32-REMARKS(9).
      MOVE SPACES  TO PAR-QHDR2 W32-REMARKS(10).
            WRITE PAR-RECORD1.
            GO TO ZA999.

       COPY ZA49.PRO.

      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
            MOVE 1   TO WS-S1.
            MOVE SPACES  TO WS-MD-LINE.
       ZA60.
            MOVE WS-G1   TO WS-TCA(WS-S1) WS-BCA(WS-S1).
          IF WS-S1 < 80
               ADD 1 TO WS-S1
               GO TO ZA60.
            MOVE WS-G14  TO WS-TCA(1)  WS-BCA(1).
            MOVE WS-G13  TO WS-TCA(80) WS-BCA(80).
            MOVE WS-G2   TO WS-TCA(10) WS-TCA(17)
              WS-TCA(47) WS-TCA(56)
              WS-TCA(59) WS-TCA(68).
            MOVE WS-G3   TO WS-MCA(1)  WS-MCA(80)
              WS-MCA(10) WS-MCA(17)
              WS-MCA(45) WS-MCA(56)
              WS-MCA(59) WS-MCA(68).
            MOVE WS-G4   TO WS-BCA(10) WS-BCA(17)
              WS-BCA(47) WS-BCA(56)
              WS-BCA(59) WS-BCA(68).
            MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
       ZA200.

       COPY LOCKED.PRO.

       ZA205.
             EXIT PROGRAM.

       ZA999.
             EXIT.

       I-O-ERRORS      SECTION  91.

       COPY ERRORS.PRO.

       DISPLAY-FILE-NAME.
           IF WS-F-ERROR = 2
              MOVE W02-NETWORK  TO WS-FILE
              MOVE WS-NETKEY  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 6
               MOVE W02-DEBTOR   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 7
              MOVE W02-DEPART  TO WS-FILE
              MOVE ZERO  TO WS-KEY
              MOVE DPT-CODE  TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
               MOVE WS-PARKEY    TO WS-KEY
           ELSE
           IF WS-F-ERROR = 18
               MOVE W02-RECOVER  TO WS-FILE
               MOVE WS-RECKEY    TO WS-KEY
           ELSE
           IF WS-F-ERROR = 19
              MOVE W02-SALE  TO WS-FILE
              MOVE WS-SALKEY  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 20
               MOVE W02-SORDER   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE SOR-KEY      TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 21
               MOVE W02-SPARTS   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE PRT-ITEM     TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 22
               MOVE W02-STOCKF   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 25
               MOVE W02-STKMEM   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE SME-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 37
              MOVE W02-SHARED  TO WS-FILE
              MOVE WS-SHARED  TO WS-KEY
    ELSE
          IF WS-F-ERROR = 64
              MOVE W02-SYSUSER  TO WS-FILE
              MOVE USE-NO  TO WS-KEY.

       COPY DISPERR.PRO.
