      $set LINKCOUNT"596" GNT"QTP004.GNT"
      ******************************************************************
      *                                                                *
      *    ******   ********  *******     ****      ****        ***    *
      *   **    **     **     **    **   **  **    **  **      * **    *
      *   **    **     **     **    **  **    **  **    **    *  **    *
      *   **    **     **     *******   **    **  **    **   *   **    *
      *   **   ***     **     **        **    **  **    **  ********   *
      *    ******      **     **         **  **    **  **        **    *
      *         **     **     **          ****      ****        ****   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *      C O N V E R T   Q U O T A T I O N   T O   O R D E R       *
      *                                                                *
      *       Version 9.00.82 - April 2016                             *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     QTP004.
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

       COPY STOCK.SL.

       COPY SYSUSE.SL.

          SELECT PRNDSK  ASSIGN W02-PRNDISK
                          ORGANIZATION LINE SEQUENTIAL.

          SELECT PRNPIC  ASSIGN W02-PICSLIP
                          ORGANIZATION LINE SEQUENTIAL.

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

       COPY STOCK.FDE.

       COPY SYSUSE.FDE.

       FD  PRNDSK    LABEL RECORD OMITTED
             LINAGE IS WS-PGE-LENGTH.
       01  REP-L1.
           03  REP-DETAIL1     PIC X(80).

       01  REP-L2.
    03  REP-PG.
               05 REP-LN       PIC 9(04).
          03  REP-DETAIL2     PIC X(96).

       01  REP-L3.
    03  FILLER        PIC  X(04).
           03  REP-AC          PIC X(11).
           03  REP-ACNO        PIC X(06).
           03  FILLER          PIC X(01).
           03  REP-NAME        PIC X(40).
           03  FILLER          PIC X(08).
           03  REP-DATE        PIC X(06).
           03  REP-DTE         PIC Z9/99/99.

       01  REP-L4.
    03  FILLER        PIC  X(04).
           03  REP-H1          PIC X(26).
           03  REP-REF         PIC 9(08).
           03  FILLER          PIC X(46).

       01  REP-L5.
    03  FILLER        PIC  X(04).
           03  FILLER          PIC X(02).
           03  REP-H2          PIC X(16).
           03  REP-H3          PIC X(33).
           03  REP-H4          PIC X(10).
           03  REP-H5          PIC X(15).
           03  REP-H6          PIC X(13).
           03  REP-H7          PIC X(05).
           03  FILLER          PIC X(02).

       01  REP-L6.
    03  FILLER        PIC  X(04).
           03  FILLER          PIC X(02).
           03  REP-ITM1        PIC X(14).
           03  FILLER          PIC X(02).
           03  REP-DESC        PIC X(30).
           03  FILLER          PIC X(02).
           03  REP-QNT         PIC Z(05)9.99.
           03  FILLER          PIC X(02).
           03  REP-NC.
               05  REP-PRICE   PIC Z(05)9.99.
           03  FILLER          PIC X(02).
           03  REP-VAT         PIC Z(06)9.99.
           03  FILLER          PIC X(02).
           03  REP-VAL         PIC Z(06)9.99.
           03  FILLER          PIC X(02).

       01  REP-L7.
    03  FILLER        PIC  X(04).
           03  FILLER          PIC X(11).
           03  REP-SMAN        PIC X(30).
           03  FILLER          PIC X(01).
           03  REP-H8          PIC X(10).
           03  REP-TERMS       PIC X(28).

       01  REP-L8.
    03  FILLER        PIC  X(04).
           03  FILLER          PIC X(11).
           03  REP-ORD         PIC X(14).
           03  FILLER          PIC X(03).
           03  REP-H9          PIC X(10).
           03  REP-CONTACT     PIC X(42).

       01  PIC-L1.
    03  PIC-PG.
               05  PIC-LN      PIC 9(04).
          03  PIC-DETAIL2     PIC X(96).

       01  PIC-L2.
    03  FILLER        PIC  X(04).
           03  PIC-DETAIL1     PIC X(80).

       01  PIC-L3.
    03  FILLER        PIC  X(04).
           03  PIC-AC          PIC X(10).
           03  PIC-ACNO        PIC X(06).
           03  FILLER          PIC X(01).
           03  PIC-NAME        PIC X(40).
           03  FILLER          PIC X(08).
           03  PIC-DATE        PIC X(06).
           03  PIC-TIME.
               05  PIC-DTE     PIC Z9/99/99.

       01  PIC-L3A.
    03  FILLER        PIC  X(04).
           03  FILLER          PIC X(10).
           03  PIC-TEL         PIC X(14).
           03  PIC-CNTCT       PIC X(10).
           03  PIC-CONTACT     PIC X(30).
           03  FILLER          PIC X(32).

       01  PIC-L4.
    03  FILLER        PIC  X(04).
           03  PIC-H1          PIC X(29).
           03  PIC-REF         PIC 9(08)/99.
           03  FILLER          PIC X(40).

       01  PIC-L5.
    03  FILLER        PIC  X(04).
           03  PIC-H2          PIC X(15).
           03  PIC-H3          PIC X(47).
           03  PIC-H4          PIC X(10).
           03  PIC-H5          PIC X(08).

       01  PIC-L6.
    03  FILLER        PIC  X(04).
           03  PIC-ITM1        PIC X(14).
           03  FILLER          PIC X(01).
           03  PIC-DESC.
               05  FILLER      PIC X(31).
               05  PIC-QTY     PIC Z(05)9.999 BLANK WHEN ZERO.
               05  FILLER      PIC X(04).
           03  PIC-DSC REDEFINES PIC-DESC.
               05  PIC-ITM2    PIC X(14).
               05  FILLER      PIC X(01).
               05  PIC-DSCR    PIC X(30).
           03  PIC-QNT         PIC Z(05)9.999.
           03  FILLER          PIC X(10).

       FD  PRNPIC    LABEL RECORD OMITTED
             LINAGE IS WS-PGE-LENGTH.
       01  PSL-LINE.
          03  PSL-DETAIL      PIC X(132).

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
       77  WS-TME2    PIC 9(08).
       77  WS-PORT         PIC  9(05) VALUE 1004.
       77  WS-OPEN         PIC  X(01) VALUE X"01".
       77  WS-CLOSE        PIC  X(01) VALUE X"00".
       77  WS-PAR          PIC  9(01) VALUE 0.
       77  WS-INO    PIC 9(03) VALUE 0.
       77  WS-LINES        PIC  9(02) VALUE 0.
       77  WS-FIRST    PIC 9(03) VALUE 0.
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
       77  WS-BATCH    PIC 9(05).
       77  WS-NODEP     PIC  X(30) VALUE
                            "NO SUCH   D E P A R T M E N T".
       77  WS-A            PIC  X(19) VALUE "Any key to continue".
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
           03  CRT-PROGRAM         PIC  X(15) VALUE "QTP004".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "SALES - CONVERT QUOTATION TO ORDER".
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
              05  WS-TCHAR PIC  X(80) VALUE "ÆÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍ
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
              05  WS-BCHAR PIC  X(80) VALUE "ÆÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍ
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
    03  WS-PROG        PIC  X(03) VALUE "004".

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

       01  W02-FILE-IDS.
           03  W02-PRINTER.
        05  W02-REPORT PIC X(07) VALUE "QTP004.".
               05  W02-USER   PIC X(05) VALUE SPACES.
           03  W02-PICSLIP.
               05  W02-PSLIP  PIC X(07) VALUE "PICSLP.".
               05  W02-USERP  PIC X(05) VALUE SPACES.

       COPY W05.VAT.

       01  W05-INV.
           03  W05-ENTRY       PIC  ZZ9.
           03  FILLER          PIC  X(01) VALUE SPACES.
           03  W05-DATE        PIC  Z9/99/99.
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

       01  W15-ALLOCATIONS.
           03  W15-DETAIL.
             05  W15-TRAN OCCURS 15.
               07  W15-VALUE   PIC  Z(06)9.99- BLANK WHEN ZERO.
               07  W15-ALLOC   PIC  Z(06)9.99- BLANK WHEN ZERO.
           03  W15-DISPLAY REDEFINES W15-DETAIL.
               05  W15-BAL1    PIC  X(11).
               05  W15-ALL1    PIC  X(11).
               05  W15-BAL2    PIC  X(11).
               05  W15-ALL2    PIC  X(11).
               05  W15-BAL3    PIC  X(11).
               05  W15-ALL3    PIC  X(11).
               05  W15-BAL4    PIC  X(11).
               05  W15-ALL4    PIC  X(11).
               05  W15-BAL5    PIC  X(11).
               05  W15-ALL5    PIC  X(11).
               05  W15-BAL6    PIC  X(11).
               05  W15-ALL6    PIC  X(11).
               05  W15-BAL7    PIC  X(11).
               05  W15-ALL7    PIC  X(11).
               05  W15-BAL8    PIC  X(11).
               05  W15-ALL8    PIC  X(11).
               05  W15-BAL9    PIC  X(11).
               05  W15-ALL9    PIC  X(11).
               05  W15-BAL10   PIC  X(11).
               05  W15-ALL10   PIC  X(11).
               05  W15-BAL11   PIC  X(11).
               05  W15-ALL11   PIC  X(11).
               05  W15-BAL12   PIC  X(11).
               05  W15-ALL12   PIC  X(11).
               05  W15-BAL13   PIC  X(11).
               05  W15-ALL13   PIC  X(11).
               05  W15-BAL14   PIC  X(11).
               05  W15-ALL14   PIC  X(11).
               05  W15-BAL15   PIC  X(11).
               05  W15-ALL15   PIC  X(11).

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
          03  W95-COMP        PIC  X(40).
          03  W95-ADD1        PIC  X(30).
          03  W95-ADD2        PIC  X(30).
          03  W95-ADD3        PIC  X(30).
          03  W95-ADD4        PIC  X(30).
           03  W95-PC.
              05  W95-POST    PIC  9(04).
          03  W95-TEL        PIC  X(14).
           03  W95-V1.
              05  W95-T       PIC  Z(04)9.99.
           03  W95-V2.
              05  W95-B       PIC  Z(06)9.99-.
           03  W95-D.
              05  W95-DTE     PIC  Z9/99/9999.
    03  W95-TIME.
        05  W95-HH      PIC  99.
        05  FILLER      PIC  X(01) VALUE ":".
        05  W95-MM      PIC  99.
        05  FILLER      PIC  X(01) VALUE ":".
        05  W95-SS      PIC  99.

       01  W100-EDIT.
           03  W100-LBAL       PIC Z(06)9.99-.
           03  W100-LARR       PIC Z(06)9.99.
           03  W100-BAL        PIC Z(08)9.99-.
           03  W100-CUR        PIC Z(08)9.99-.
           03  W100-30         PIC Z(08)9.99-.
           03  W100-60         PIC Z(08)9.99-.
           03  W100-90         PIC Z(08)9.99-.
           03  W100-120        PIC Z(08)9.99-.
           03  W100-INT        PIC Z(08)9.99-.
           03  W100-CRL        PIC Z(06)9.
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

       01  S26-QNT.
          03        COLUMN 51 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(04)9.99 USING W10-QUANT AUTO.

       01  S26-PRC.
          03        COLUMN 60 FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC Z(05)9.99 USING W10-SELL AUTO.

       01  S28.
          03  LINE 25 COLUMN 65 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
                                 PIC ZZZ9 USING WS-SALKEY AUTO.

       01  S30.
           03  LINE  2 COLUMN 36 FOREGROUND-COLOR 15
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
    03        COLUMN 10 BACKGROUND-COLOR 5
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

       01  S31B.
           03  LINE  2 COLUMN 32 FOREGROUND-COLOR 15
                                 VALUE "CONVERT QUOTATION".

       01  S36.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(06) USING W80-ACNO AUTO.

       01  S40.
    03  LINE  8 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "Ú".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14
                                 PIC X(50) USING WS-SRT-LINE.
    03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "¿".
          03  LINE  9 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     PIC  X(18) FROM WS-SPACE.
    03        COLUMN 34 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "CLIENT DETAILS".
    03        COLUMN 48 BACKGROUND-COLOR 2
     PIC  X(18) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
          03  LINE 10 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "Name    >".
    03        COLUMN 65 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "<".
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
          03  LINE 11 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "Address >".
    03        COLUMN 55 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "<".
    03        COLUMN 56 BACKGROUND-COLOR 2
     PIC  X(10) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
          03  LINE 12 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
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
     FOREGROUND-COLOR 14 VALUE "³".
          03  LINE 13 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
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
     FOREGROUND-COLOR 14 VALUE "³".
          03  LINE 14 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
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
     FOREGROUND-COLOR 14 VALUE "³".
          03  LINE 15 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
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
          03        COLUMN 66 FOREGROUND-COLOR 14 VALUE "³".
          03  LINE 16 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "Tel.    >".
    03        COLUMN 39 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "<".
    03        COLUMN 40 BACKGROUND-COLOR 2
     PIC  X(26) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
          03  LINE 17 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "Id.No.  >".
    03        COLUMN 39 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
                                 VALUE "<".
    03        COLUMN 40 BACKGROUND-COLOR 2
     PIC  X(26) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
    03  LINE 18 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "À".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14
                                 PIC X(50) USING WS-SRT-LINE.
    03        COLUMN 66 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "Ù".

       01  S41.
    03  LINE 10 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 15
                                 PIC X(40) USING W40-NM1 AUTO.
    03  LINE 11 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD1 AUTO.
    03  LINE 12 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD2 AUTO.
    03  LINE 13 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD3 AUTO.
    03  LINE 14 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 15
                                 PIC X(30) USING W40-AD4 AUTO.
    03  LINE 15 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 15
                                 PIC X(04) USING W40-PC AUTO.
    03  LINE 16 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 15
                                 PIC X(14) USING W40-HTEL AUTO.
    03  LINE 17 COLUMN 25 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 15
                                 PIC X(14) USING W40-ID AUTO.

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
            PERFORM BF000.
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

       AA50-LOOKUP     SECTION.
       AA50O.
             MOVE "O"            TO WS-LOOK.
       AA50.
            PERFORM SAVE-SCREEN.
             MOVE 4              TO W44-FUNCTION.
            PERFORM SCREEN-CONTENTS.
            PERFORM SORDER-LOOKUP.
            PERFORM RESTORE-SCREEN.
            MOVE "S" TO WS-LOOK.
       AA99.
             EXIT.

       COPY FUNCTION.SCR.

       COPY ERROR.SCR.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       AA400              SECTION.
       AA401.
            DISPLAY "F1" AT 2501
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " Quote no./ "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " Account no./ "
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F4" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
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

       COPY STOCK.RD.

       COPY SYSUSE.RD.

      /
       AD000-WRITE             SECTION.

       COPY CONTROL.WR.

       COPY PARAM.WR.

       COPY SALE.WR.

       COPY SORDER.WR.

       COPY STOCK.WR.

       COPY SORDER.LUP.

       COPY APAC.HLP.

      /
      *    THIS ROUTINE GENERATES THE RECOVERY FILE RECORDS
      *
       AY000           SECTION.
       AY05.
             MOVE 05             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
           IF WS-ACTION = 0 OR 2
               PERFORM READ-STOCK-LOCK THRU READ-STOCK-EXIT.
    IF WS-ACTION = 9
        MOVE 0   TO REC-TYPE
    ELSE
        MOVE WS-ACTION  TO REC-TYPE.
             MOVE STK-RECORD1    TO REC-STOCKF.
             GO TO AY50.

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
                        WITH FOREGROUND-COLOR 14
                        WS-STATUS WITH FOREGROUND-COLOR 15
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
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   C O D E
      *
       CA000         SECTION.
       CA00.
             MOVE 14             TO W50-LENGTH.
       CA05.
             MOVE W10-ITEM       TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-13       TO W10-ITEM
               GO TO CA05.
      CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
            MOVE W50-ITEM  TO W10-ITEM.
       CA06.
             MOVE W50-ITEM       TO STK-CODE.
       CA10.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 0
               PERFORM CA500.
       CA99.
             EXIT.
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G
      *           A L T E R N A T E   C O D E
      *
       CA100         SECTION.
       CA101.
             MOVE 10             TO W50-LENGTH.
       CA105.
             MOVE W10-ACODE      TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-9        TO W10-ACODE
               GO TO CA105.
      CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
            MOVE W50-ACODE  TO STK-ACODE W10-ACODE.
       CA110.
             PERFORM START-AT-ALT-CODE THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO CA119.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 0
               PERFORM CA500.
       CA119.
             EXIT.
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
      *
      *    ****   G E T   S T O C K   R E C O R D   U S I N G   X R E F
      *
       CA200         SECTION.
       CA201.
             MOVE 14             TO W50-LENGTH.
       CA205.
             MOVE W10-CODE2      TO W50-DATA.
           IF W50-1 = SPACE
               MOVE W50-13       TO W10-CODE2
               GO TO CA205.
      CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
            MOVE W50-ITEM  TO W10-CODE2.
       CA206.
             MOVE W50-ITEM       TO STK-XREF.
       CA210.
             PERFORM START-AT-STOCK-XREF THRU READ-STOCK-EXIT.
           IF WS-F-ERROR = 22
               GO TO CA299.
             PERFORM READ-STOCK-NEXT THRU READ-STOCK-EXIT.
             PERFORM CA500.
       CA299.
             EXIT.
      *
      *    ****    Calculate the inclusive prices
      *
       CA500         SECTION.
       CA505.
      *    ****    A V E R A G E   C O S T
    IF STK-QUANT < 0.001
        MOVE STK-COST  TO STK-AVGE.
          IF STK-WSALE = ZERO
               IF STK-CASH NOT = ZERO
                   MOVE STK-CASH TO STK-WSALE
               ELSE
                   MOVE STK-SELL TO STK-WSALE.
           IF STK-CASH = ZERO
               MOVE STK-SELL     TO STK-CASH.
             MOVE STK-CASH       TO W10-VCASH
             MOVE STK-SELL       TO W10-VSELL
             MOVE STK-WSALE      TO W10-VWSALE.
             MOVE ZERO           TO W05-RATE.
           IF STK-TAX > 0
               MOVE W05-VAT (STK-TAX)
                                 TO W05-RATE.
             COMPUTE W10-VCASH ROUNDED
                   = W10-VCASH + (W10-VCASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED
                   = W10-VWSALE + (W10-VWSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED
                   = W10-VSELL + (W10-VSELL * W05-RTE).
       CA599.
             EXIT.
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
        MOVE SOR-DESC  TO SAL-MSER
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
      MOVE ZERO   TO SOR-QNT SOR-COST SOR-INCTAX
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
       BB050               SECTION.
       BB50.
      MOVE WS-EXPP TO PIC-DETAIL2.
             MOVE ZERO           TO PIC-LN.
             WRITE PIC-L2 BEFORE 0 LINES.
             MOVE W95-COMP       TO PIC-DETAIL2.
             MOVE 2              TO PIC-LN.
             WRITE PIC-L2 BEFORE 2 LINES.
             MOVE SPACES         TO PIC-DETAIL2.
             MOVE "PICKING SLIP - REFERENCE NO:"
                                 TO PIC-H1.
             MOVE W90-PS         TO PIC-REF.
             MOVE ZERO           TO PIC-LN.
             WRITE PIC-L2 BEFORE 0 LINES.
             MOVE 2              TO PIC-LN.
             WRITE PIC-L2 BEFORE 2 LINES.
           IF W90-PICK > 01
               MOVE SPACES       TO PIC-DETAIL2
               MOVE "REPLACE PREVIOUS PICKING SLIP"
                                 TO PIC-H1
               MOVE 2            TO PIC-LN
               WRITE PIC-L2 BEFORE 2 LINES.
       BB60.
      MOVE WS-ECAN TO PIC-DETAIL2.
             MOVE ZERO           TO PIC-LN.
             WRITE PIC-L2 BEFORE 0 LINES.
             MOVE SPACES         TO PIC-DETAIL2.
             MOVE "Account :"    TO PIC-AC.
             MOVE W80-ACNO       TO PIC-ACNO.
             MOVE W40-NAME       TO PIC-NAME.
             MOVE "Date:"        TO PIC-DATE.
             MOVE W12-TODAY      TO PIC-DTE.
             MOVE 1              TO PIC-LN.
             WRITE PIC-L2 BEFORE 1 LINES.
             MOVE SPACES         TO PIC-DETAIL2.
             MOVE " Contact:"    TO PIC-CNTCT.
             MOVE W40-CONTACT    TO PIC-CONTACT.
             MOVE "Tel :"        TO PIC-AC.
             MOVE W40-TEL        TO PIC-TEL.
             MOVE "Time:"        TO PIC-DATE.
             ACCEPT W25-TIME FROM TIME.
             MOVE W25-HH         TO W95-HH.
             MOVE W25-MM         TO W95-MM.
             MOVE W25-SS         TO W95-SS.
             MOVE W95-TIME       TO PIC-TIME.
             MOVE 2              TO PIC-LN.
             WRITE PIC-L2 BEFORE 2 LINES.
             MOVE SPACES         TO PIC-DETAIL2.
             MOVE ALL "="        TO PIC-DETAIL1.
             MOVE 2              TO PIC-LN.
             WRITE PIC-L2 BEFORE 2 LINES.
             MOVE SPACES         TO PIC-DETAIL2.
             MOVE "ITEM CODE"    TO PIC-H2.
             MOVE "DESCRIPTION"  TO PIC-H3.
             MOVE "QUANTITY"     TO PIC-H4.
             MOVE "SUPP/RMK"     TO PIC-H5.
             MOVE 2              TO PIC-LN.
             WRITE PIC-L2 BEFORE 2 LINES.
             MOVE SPACES         TO PIC-DETAIL2.
       BB65.
             EXIT.
      /
       BB100               SECTION.
       BB100-MARG.
             MOVE ZERO TO WS-ERROR.
           IF W90-TAX = SPACES
               IF STK-TAX NOT = ZERO
                   COMPUTE W10-SPAPRC = (W10-SPAPRC /
                      (100.0000 + W05-RATE)) * 100.
           IF (WS-PRC = "R") AND (STK-SELL NOT > W10-SPAPRC)
               GO TO BB110
           ELSE
           IF (WS-PRC = "C") AND (STK-CASH NOT > W10-SPAPRC)
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
              MOVE 1   TO WS-ERROR
              GO TO BB110.

       BB106.
      DISPLAY SPACE AT 2551
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
            DISPLAY "Enter password" AT 2551
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
            ACCEPT W40-CSHPASS AT 2567
      WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
          AUTO NO-ECHO.
             MOVE 6 TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-CSHPASS
                                BY VALUE WS-LENGTH
                                RETURNING WS-STATUS.
            MOVE 1 TO WS-LENGTH.
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
      *    ****    C O N V E R T   Q U O T E   T O   O R D E R
      *
       BF000         SECTION 53.
       BF00.
      *
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW         TO W85-PASS.
       BF05.
      *
      *    ****    Q U O T A T I O N   S C R E E N
      *
             PERFORM ERASE-SCREEN.
             DISPLAY S30.
      *            C H A N G E   S C R E E N   H E A D I N G
             DISPLAY S31B.
             MOVE 8              TO LIN.
             PERFORM BF115 UNTIL LIN > 18.
             MOVE W12-TODAY      TO W90-DTE.
            DISPLAY W90-DTE AT 0570
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
            MOVE ZERO   TO W90-BDT W90-LDISC W90-NONTAX
              W90-BAL W90-SALES W90-EXCTOT
        W90-CUR W90-PMETH W40-STATUS.
             MOVE 1              TO WS-FIRST WS-LAST.
            MOVE SPACES  TO W90-TAX W90-INSTRUCT.
      MOVE 1   TO WS-S9.

       BF08.
            MOVE ZERO   TO W90-VAT(WS-S9) W90-TAXVAL(WS-S9).
    IF WS-S9 < 12
        ADD 1   TO WS-S9
        GO TO BF08.
      MOVE ZERO   TO W10-SORD.

       BF10.
             PERFORM AA400.
             ACCEPT S30A AT 0510.
    IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY
                     MOVE ZERO   TO W10-SORD
                     GO TO BF199
                 WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "O"  TO WS-ETYPE
   WHEN F3-KEY
                     MOVE "A"    TO WS-ETYPE
   WHEN F4-KEY
       MOVE "T"  TO WS-ETYPE
                 WHEN OTHER
                     CALL X"E5"
                     GO TO BF10
               END-EVALUATE
              PERFORM AA50O THRU AA50
               DISPLAY S30A AT 0510
               IF W10-SORD = ZERO
                   GO TO BF10.
            PERFORM CLEAR-L50.
           IF W10-SORD = ZERO
               GO TO BF199.
             MOVE W10-SORD       TO W90-REF.

       BF12.
             PERFORM GET-SORDER.
           IF WS-ERROR = 1
        MOVE WS-ERR2  TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BF10.
           IF WS-ERROR = 2
        MOVE "Not a quotation"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BF10.
           IF WS-ERROR = 3
        MOVE "Quote corrupted"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BF10.
           IF WS-ERROR = 4
        MOVE "Invalid a/c no"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BF10.
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
      DISPLAY W40-TEL AT 0466
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      DISPLAY W40-CONTACT AT 0528
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      MOVE 1   TO WS-FIRST WS-SALKEY.
             SUBTRACT 1          FROM WS-LAST.

       BF15.
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
          IF W90-TAX = SPACES
               GO TO BF20.
      MOVE 1   TO WS-S1.
      MOVE ZERO   TO W25-TOTAL.

       BF18.
          IF W90-TAXVAL(WS-S1) NOT = ZERO
              MOVE W05-VAT(WS-S1)
           TO W05-RATE
              COMPUTE W25-TOTAL ROUNDED = W25-TOTAL +
        (W90-TAXVAL(WS-S1) * W05-RTE).
    IF WS-S1 < 6
        ADD 1   TO WS-S1
        GO TO BF18.
            MOVE W25-TOTAL  TO W100-BAL.
            ADD W25-TOTAL  TO W90-TOTAL.
            DISPLAY W100-BAL AT 2215
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.

       BF20.
            MOVE W90-TOTAL  TO W100-BAL.
            DISPLAY W100-BAL AT 2315
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      *
      *    ****    D I S P L A Y   Q U O T E   D E T A I L S
      *
       BF25.
             MOVE 8              TO LIN.
            PERFORM BF115 UNTIL LIN > 18.
             MOVE 8              TO LIN.
             MOVE 1              TO WS-LINES WS-FIRST.

       BF30.
             MOVE WS-FIRST TO WS-SALKEY.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
       BF35.
          IF WS-SALKEY > WS-LAST
               GO TO BF45.
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
     DISPLAY (LIN, 5) "(I)" WITH FOREGROUND-COLOR 14
               ELSE
     DISPLAY (LIN, 5) "(R)" WITH FOREGROUND-COLOR 15
               END-IF
              GO TO BF40.
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
      BACKGROUND-COLOR 5.
      *
      *    ****    I S   T H E   S C R E E N   F U L L
      *
       BF40.
           IF LIN < 18
               ADD 1             TO LIN WS-SALKEY WS-LINES
               GO TO BF35.

       BF45.
             EXIT.

       BF50.
            PERFORM CLEAR-L50.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
           IF WS-LINES < 11
               IF WS-FIRST = 1
           MOVE "'C'onvert quote - 'E'xit"
     TO WS-ERR-MES
               ELSE
           MOVE "'C'onvert quote - 'P'revious screen - 'E'xit"
     TO WS-ERR-MES
                   MOVE 2        TO WS-LINES
           ELSE
           IF WS-SALKEY < WS-LAST
               IF WS-FIRST = 1
           MOVE "'C'onvert quote - 'N'ext screen - 'E'xit"
     TO WS-ERR-MES
                   MOVE 1        TO WS-LINES
               ELSE
           MOVE "'C'onvert - 'N'ext,'P'revious screen,'E'xit"
     TO WS-ERR-MES
                   MOVE 3        TO WS-LINES
           ELSE
               MOVE 2            TO WS-LINES
              MOVE "'C'onvert quote - 'P'revious screen - 'E'xit"
     TO WS-ERR-MES.
       BF55.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "C" OR "E" OR "N" OR "P".
           IF WS-OPTION = "E"
               GO TO BF05.
           IF WS-OPTION = "C"
               GO TO BF65.
      *
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *
           IF WS-LINES = 2 OR 3
               IF WS-OPTION = "P"
                   PERFORM BF60
                   SUBTRACT 11   FROM WS-FIRST
                   GO TO BF30.
      *
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *
           IF WS-LINES = 1 OR 3
               IF WS-OPTION = "N"
                   PERFORM BF60
                   ADD 11        TO WS-FIRST
                   GO TO BF30.
             GO TO BF55.
       BF60.
      *
      *    ****    C L E A R   S C R E E N
      *
             MOVE 8              TO LIN.
             PERFORM BF115 UNTIL LIN > 18.
             MOVE 8              TO LIN.
             MOVE 1              TO WS-LINES.
      *
      *    ****    C O N V E T   Q U O T E    T O   O R D E R
      *
       BF65.
             MOVE "Y"            TO WS-OPTION.
            PERFORM CLEAR-L50.

      *    ****    C H E C K   A R R E A R S   &   C R E D I T   L I M I T

           IF (DEB-60 > ZERO) OR (DEB-90 > ZERO) OR (DEB-120 > ZERO)
              MOVE "Arrears > 30 days - Override Y/N"
     TO WS-ERR-MES
        MOVE SPACE  TO WS-OPTION
        PERFORM OPT-MESSAGE TEST AFTER
         UNTIL WS-OPTION = "Y" OR "N"
        IF WS-OPTION = "Y"
                   MOVE ZERO     TO WS-ERROR
                   PERFORM BB106
                   PERFORM BB108
                   IF WS-ERROR = 1
                       MOVE "N"  TO WS-OPTION.
          IF WS-OPTION = "N"
               GO TO BF05.
           IF DEB-CRL > ZERO
               IF (DEB-BAL + W90-TOTAL) > DEB-CRL
           MOVE "Credit limit exceded - Override Y/N"
     TO WS-ERR-MES
     MOVE SPACE  TO WS-OPTION
     MOVE DEB-CRL  TO W100-CRL
     MOVE DEB-BAL TO W100-BAL
     DISPLAY "Balance: " AT 2540
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       W100-BAL WITH BACKGROUND-COLOR 3
       FOREGROUND-COLOR 7 HIGHLIGHT
      " Limit: "
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       W100-CRL WITH BACKGROUND-COLOR 3
       FOREGROUND-COLOR 7 HIGHLIGHT
     PERFORM OPT-MESSAGE TEST AFTER
      UNTIL WS-OPTION = "Y" OR "N"
           IF WS-OPTION = "Y"
               MOVE ZERO TO WS-ERROR
               PERFORM BB106
               PERFORM BB108
               IF WS-ERROR = 1
            MOVE "N" TO WS-OPTION.
            PERFORM CLEAR-L50.
          IF WS-OPTION = "N"
               GO TO BF05.
             DISPLAY "Converting Quote" AT 2520 WITH
             BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
             MOVE ZERO           TO WS-SALKEY.
            PERFORM AY60 THRU AY999.

       BF70.
      *
      *    ****    U P D A T E   O R D E R   F I L E
      *
             MOVE W90-REF        TO SOR-REF.
             MOVE 100            TO SOR-SQN.
             PERFORM READ-SORDER-LOCK THRU READ-SORDER-EXIT.
             MOVE "O"            TO SOR-DOC.
      PERFORM REWRITE-SORDER-UNLOCK THRU WRITE-SORDER-EXIT.
      PERFORM SAVE-SCREEN-3.
      MOVE 12   TO SHADE-ROW.
      MOVE 20   TO SHADE-COL.
      MOVE 42   TO SHADE-WIDTH.
      MOVE 2   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
        AT 1118 WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
       "¿" WITH FOREGROUND-COLOR 5 HIGHLIGHT
         BACKGROUND-COLOR 5.
      DISPLAY "³" AT 1218
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
       " Updating Stock Record:                   "
        WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 5
       "³" WITH FOREGROUND-COLOR 5 HIGHLIGHT
         BACKGROUND-COLOR 5.
      DISPLAY "À" AT 1318
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR 5 HIGHLIGHT
      BACKGROUND-COLOR 5.
      PERFORM SCREEN-SHADOW.

       BF72.
      PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
      *
      *    ****   I F E N D O F   F I L E  -   E X I T
      *
    IF WS-F-ERROR = 20
        GO TO BF75.
      *
      *    ****   I F T R A I L E R R E C O R D   -  U P D A T E
      *    T O T A L S.
      *

      *QWERTY
    IF SOR-TYPE = 9

        GO TO BF75.
      *
      *    ****   I F N O T D E T A I L   R E C O R D   ( S T O C K
      *    / I N V E N T O R Y )   -   R E A D N E X T
      *
    IF NOT (SOR-TYPE = 1)
        GO TO BF72.
      *
      *    ****    U P D A T E  P I C K I N G  S L I P   Q T Y
      *     O N  S T O C K   R E C O R D
      *
      MOVE SOR-EXT-ITEM  TO STK-CODE.
      DISPLAY STK-CODE AT 1243
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      MOVE 0   TO WS-ACTION.
      PERFORM AY05 THRU AY59.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Check if sufficient quantity available. If quantity more  ³
      *    ³ than stock quantity, reduce quantity to available. Adjust ³
      *    ³ values and totals accordingly.          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
    IF SOR-QNT > STK-QUANT

      SUBTRACT SOR-QNT  FROM STK-QUANT.
      ADD SOR-QNT  TO STK-QPIC.
            PERFORM REWRITE-STOCK-UNLOCK THRU WRITE-STOCK-EXIT.
      GO TO BF72.

       BF75.
      PERFORM RESTORE-SCREEN-3.
             PERFORM AY70 THRU AY999.

       BF80.
      MOVE "Print picking slip (Y/N)?  [ ]"
     TO WS-ERR-MES.
      MOVE "Y"   TO WS-OPTION.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "Y" OR "N".
           IF WS-OPTION = "Y"
        MOVE W90-REF  TO LS-SLS-ORDNO
               MOVE "O"          TO WS-DOC
               PERFORM BF200
               MOVE "Q"          TO WS-DOC.
      CLOSE SALE.
             OPEN OUTPUT SALE.
             CLOSE SALE.
            OPEN I-O SALE.
             GO TO BF05.

       BF115.
            DISPLAY (LIN, 1) WS-INV1 WITH BACKGROUND-COLOR 0
        FOREGROUND-COLOR 3
         WS-INV78 WITH FOREGROUND-COLOR 3
         WS-INV80 WITH BACKGROUND-COLOR 0
         FOREGROUND-COLOR 3.
            ADD 1   TO LIN.
       BF199.
             EXIT.
      /
      *    ****    P I C K I N G   S L I P S
      *
       BF200         SECTION 53.
       BF205.
      PERFORM CLEAR-L50.
           IF WS-PGADV = "Y"
        OPEN OUTPUT PRNDSK
        CALL X"91" USING X91-RES X91-FUN PRNDSK
        MOVE WS-10CPI  TO REP-DETAIL1
        WRITE REP-L1 BEFORE 0 LINES
        MOVE SPACES  TO REP-DETAIL1.
      MOVE LS-SLS-ORDNO  TO W90-REF W10-SORD.
             PERFORM GET-SORDER.
           IF WS-ERROR NOT = ZERO
        MOVE "Database corrupted"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BF399.
      *
      *    ****    R E A D   D E T A I L S   A N D   P R I N T   S L I P
      *
       BF265.
      PERFORM CLEAR-L50.
             DISPLAY "Printing Picking Slip" AT 2502 WITH
              BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
             MOVE ZERO           TO WS-SALKEY.
             PERFORM AY60 THRU AY999.
      *
      *    ****    R E A D   O R D E R   F I L E   A N D   P R I N T
      *
       BF270.
            MOVE W90-REF  TO SOR-REF.
             MOVE ZERO           TO SOR-SQN.
             PERFORM START-AT-ORDER-NO THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
        MOVE "Database corrupted"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO BF399.

       BF275.
             PERFORM READ-SORDER-NEXT THRU READ-SORDER-EXIT.
           IF WS-F-ERROR = 20
               GO TO BF300.
           IF SOR-REF NOT = W90-REF
               GO TO BF300.
           IF SOR-TYPE = 0
               ADD 1             TO W90-PICK
               PERFORM BB050
               GO TO BF275.
           IF SOR-TYPE = 9
               GO TO BF300.
           IF SOR-TYPE NOT = 1
               IF SOR-TYPE = 2 OR 5
                   GO TO BF275
               ELSE
                   MOVE SOR-DESC TO PIC-DESC
                   PERFORM BF280
                   GO TO BF275.
             MOVE SOR-ITM        TO PIC-ITM1 PRT-ITEM.
             MOVE SOR-DESC       TO PIC-DESC.
            PERFORM READ-SPARTS THRU READ-SPARTS-EXIT.
           IF WS-F-ERROR = 21
               MOVE SOR-QNT      TO PIC-QNT
               PERFORM BF280
               GO TO BF275.
             MOVE SOR-QNT        TO PIC-QTY.
       BF280.
    IF LINAGE-COUNTER OF PRNDSK < WS-PRN-LENGTH
               MOVE 1            TO PIC-LN
               WRITE PIC-L2 BEFORE 1 LINES
           ELSE
               MOVE "PAGE"       TO PIC-PG
               WRITE PIC-L2 BEFORE PAGE
        MOVE WS-EXPP TO PIC-DETAIL2
               MOVE ZERO         TO PIC-LN
               WRITE PIC-L2 BEFORE 0 LINES
               MOVE "PICKING SLIP - CONTINUED ..."
                                 TO PIC-H1
               WRITE PIC-L2 BEFORE 0 LINES
               MOVE 2            TO PIC-LN
               WRITE PIC-L2 BEFORE 2 LINES
               PERFORM BB60 THRU BB65.
             MOVE SPACES         TO PIC-DETAIL2.
      *
      *    ****  P R O D U C T I O N   D E T A I L S
      *
       BF285.
             MOVE 1              TO WS-S6.
       BF290.
           IF PRT-CODE (WS-S6) = SPACES
               ADD 1             TO WS-S6
               GO TO BF295.
             MOVE PRT-CODE (WS-S6)
                                 TO PIC-ITM2 W10-ITEM.
             MULTIPLY PRT-QUANT (WS-S6) BY SOR-QNT
                                 GIVING PIC-QNT.
             PERFORM CA000.
           IF WS-F-ERROR = 22
               ADD 1             TO WS-S6
               GO TO BF295.
             MOVE STK-DESC       TO PIC-DSCR.
             PERFORM BF280
             ADD 1 TO WS-S6.
       BF295.
           IF WS-S6 < 31
               GO TO BF290.
             GO TO BF275.
      *
      *    ****    U P D A T E   O R D E R   F I L E
      *
       BF300.
             MOVE SPACES         TO PIC-DETAIL2.
             MOVE 1              TO PIC-LN
             WRITE PIC-L1 BEFORE 1 LINES.
      MOVE ALL "="  TO PIC-DETAIL1.
      PERFORM READ-SORDER-LOCK THRU READ-SORDER-EXIT.
             ADD 1               TO SOR-PICNO.
             PERFORM REWRITE-SORDER THRU WRITE-SORDER-EXIT.
             PERFORM AY70 THRU AY999.
       BF310.
      MOVE "Advance to next page (Y/N)"
     TO WS-ERR-MES.
      MOVE "Y"   TO WS-OPTION.
      PERFORM OPT-MESSAGE TEST AFTER
       UNTIL WS-OPTION = "N" OR "Y".
             MOVE WS-OPTION      TO WS-PGADV.
           IF WS-OPTION = "N"
        IF LINAGE-COUNTER OF PRNDSK < (WS-PRN-LENGTH - 8)
                   MOVE 2        TO PIC-LN
                   WRITE PIC-L2 BEFORE 2 LINES
               ELSE
                   MOVE "PAGE"   TO PIC-PG
                   WRITE PIC-L2 BEFORE PAGE
           ELSE
               MOVE "PAGE"       TO PIC-PG
               WRITE PIC-L2 BEFORE PAGE
               CLOSE PRNDSK
               PERFORM BF400.
             MOVE SPACES         TO PIC-DETAIL2.
       BF399.
             EXIT.
      /
       BF400         SECTION 53.
       BF405.
      OPEN INPUT PRNDSK.
      CALL X"91" USING X91-RES X91-FUN PRNDSK.
      OPEN OUTPUT PRNPIC.
      CALL X"91" USING X91-RES X91-FUN PRNPIC.
       BF410.
             READ PRNDSK
                  AT END
                     CLOSE PRNDSK
                           PRNPIC
                     GO TO BF499.
      MOVE PIC-DETAIL2  TO PSL-DETAIL.
           IF PIC-PG = "PAGE"
        WRITE PSL-LINE BEFORE PAGE
           ELSE
               MOVE PIC-LN       TO WS-LINES
        WRITE PSL-LINE BEFORE WS-LINES LINES.
             GO TO BF410.
       BF499.
             EXIT.
      /    *************************************************************
      *   I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 90.
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
      MOVE "DEBTOR"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-DEBTOR.
      MOVE "DEPART"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-DEPART.
      MOVE "NETWORK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-NETWORK.
      MOVE "PRNDISK"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      INSPECT AFID-PATH REPLACING FIRST "XXX"
          BY LS-USER.
      MOVE AFID-PATH  TO W02-PRNDISK.
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
      MOVE "SHARED"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SHARED.
      MOVE "SORDER"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SORDER.
      MOVE "SPARTS"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SPARTS.
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
    IF LS-USER NUMERIC
        IF LS-USE > ZERO AND < 110
     MOVE LS-USE  TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.

       ZA01A.
            ADD 2549 WS-USUB  GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF TRM-PICP (WS-S6) = 1
               MOVE "LPT1"       TO W02-PICSLIP
           ELSE
           IF TRM-PICP (WS-S6) = 2
               MOVE "LPT2"       TO W02-PICSLIP
           ELSE
           IF TRM-PICP (WS-S6) = 3
               MOVE "LPT3"       TO W02-PICSLIP
           ELSE
           IF TRM-PICP (WS-S6) = 4
               MOVE "COM1"       TO W02-PICSLIP
           ELSE
           IF TRM-PICP (WS-S6) = 5
               MOVE "COM2"       TO W02-PICSLIP
           ELSE
           IF TRM-PICP (WS-S6) = 9
        MOVE LS-USER  TO W02-USERP.
      ADD 501 TRM-PRN1 (WS-S6)
     GIVING W25-RESULT.
             DIVIDE W25-RESULT BY 2
                                 GIVING WS-PARKEY
                                 REMAINDER WS-S6.
             ADD 1               TO WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE TRM-CONP (WS-S6)
     TO WS-COND.
             MOVE TRM-NORP (WS-S6)
     TO WS-NORM.
             MOVE TRM-EXPP (WS-S6)
     TO WS-EXPP.
             MOVE TRM-ECAN (WS-S6)
     TO WS-ECAN.
             MOVE TRM-8LPI (WS-S6)
     TO WS-8LPI.
             MOVE TRM-6LPI (WS-S6)
     TO WS-6LPI.
             MOVE TRM-10CPI (WS-S6)
     TO WS-10CPI.
             MOVE TRM-12CPI (WS-S6)
     TO WS-12CPI.
             MOVE TRM-17CPI (WS-S6)
     TO WS-17CPI.
      MOVE TRM-LENGTH (WS-S6)
     TO WS-PRN-LENGTH.
      MOVE TRM-PAGE (WS-S6)
     TO WS-PGE-LENGTH.

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
            GO TO ZA999.

       COPY ZA49.PRO.

      *
      *    ****    S E T U P   T H E   S C R E E N   G R A P H I C S
      *
       ZA55.
             MOVE 1 TO WS-S1.
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
        MOVE "SupervisorÄ"
     TO WS-WRKHD
    ELSE
        MOVE "Workstation"
     TO WS-WRKHD
        MOVE LS-USER  TO WS-WRKST.
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
        MOVE STK-CODE  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 37
              MOVE W02-SHARED  TO WS-FILE
              MOVE WS-SHARED  TO WS-KEY
    ELSE
          IF WS-F-ERROR = 64
              MOVE W02-SYSUSER  TO WS-FILE
              MOVE USE-NO  TO WS-KEY.

       COPY DISPERR.PRO.
