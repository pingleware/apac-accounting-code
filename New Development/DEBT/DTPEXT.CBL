     $SET LINKCOUNT"640" GNT"DTPEXT.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******   ********  **    **  ********   *
      *   **    **     **     **    **  **         **  **      **      *
      *   **    **     **     **    **  **          ****       **      *
      *   **    **     **     *******   *****        **        **      *
      *   **    **     **     **        **          ****       **      *
      *   **    **     **     **        **         **  **      **      *
      *   *******      **     **        ********  **    **     **      *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *   D E B T O R ' S   S A L E S   H I S T O R Y   E X T R A C T  *
      *   P R O G R A M                                                *
      *                                                                *
      *       Version 9.00.70 - March 2015                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006 - New field included in Accounts Receivable          *
      *             for recording number of outstanding Jobs           *
      *             linked to the account. Additional filler           *
      *             added for future expansion.                        *
      *                                                                *
      *  Jan 2008 - New file (DEBALT)- Debtor alternate index included *
      *             for lookups, using any word contained in the       *
      *             Debtor name.                                       *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTPEXT.
       AUTHOR.        James William Lemmon.
       DATE-WRITTEN.   OCTOBER 1998.

                COPYRIGHT NOTICE: COPYRIGHT (C) 1998 - 2016
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

       COPY "CARDEX.SL".

       COPY "DEBALT.SL".

           SELECT DEBDET  ASSIGN W02-DEBDET
                          STATUS WS-STATUS
                          ORGANIZATION LINE SEQUENTIAL.

       COPY "DEBMEM.SL".

       COPY "DEBHIS.SL".

       COPY "DEBPRF.SL".

       COPY "DEBTEX.SL".

       COPY "DEBTOR.SL".

       COPY "DEBTOT.SL".

       COPY "DEPART.SL".

       COPY "PARAM.SL".

       COPY "STOCK.SL".

       DATA DIVISION.
      *
      *          ********  ********  **        ******** 
      *          **           **     **        **
      *          **           **     **        ** 
      *          *****        **     **        *****
      *          **           **     **        **  
      *          **           **     **        **
      *          **        ********  ********  ********
      *
       FILE SECTION.

       COPY "CARDEX.FDE".

       COPY "DEBALT.FDE".

       FD  DEBDET    LABEL RECORD OMITTED.
       01  EXP-REC1.
    03  EXP-DATA     PIC  X(512).

       COPY "DEBMEM.FDE".

       COPY "DEBHIS.FDE".

       COPY "DEBPRF.FDE".

       COPY "DEBTEX.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEBTOT.FDE".

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "STOCK.FDE".

      /
      *         **         **    ******    *******    **    **
      *         **         **   **    **   **    **   **   **
      *         **    *    **   **    **   **   **    **  **
      *         **   ***   **   **    **   ******     *****
      *          ** ** ** **    **    **   **   **    **  **
      *           ***   ***     **    **   **    **   **   **
      *            *     *       ******    **    **   **    **
      *
       WORKING-STORAGE SECTION.
       77  WS-CHECK                  PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-SUB                    PIC  9(04)    COMP-5.
       77  WS-S                      PIC  9(04)    COMP-5.
       77  WS-S1                     PIC  9(04)    COMP-5.
       77  WS-S2                     PIC  9(04)    COMP-5.
       77  WS-S3                     PIC  9(04)    COMP-5.
       77  WS-S4                     PIC  9(04)    COMP-5.
       77  WS-S5                     PIC  9(04)    COMP-5.
       77  WS-S6                     PIC  9(04)    COMP-5.
       77  WS-S7                     PIC  9(04)    COMP-5.
       77  WS-S8                     PIC  9(04)    COMP-5.
       77  WS-S9                     PIC  9(04)    COMP-5.
       77  WS-IXS1                   PIC  9(04)    COMP-5.
       77  WS-ENQPOS                 PIC  9(06)    COMP-5.
       77  WS-ENQEND                 PIC  9(06)    COMP-5.
       77  WS-DEPKEY                 PIC  9(06)    COMP-5.
       77  WS-NETKEY                 PIC  9(06)    COMP-5.
       77  WS-PARKEY                 PIC  9(06)    COMP-5.
       77  WS-KEYSTR                 PIC  9(06)    COMP-5.
       77  WS-LOOK                   PIC  X(01)    VALUE "S".
       77  WS-PAGE                   PIC  9(04)    COMP-5.
       77  WS-OPTION                 PIC  X(01).
       77  WS-USE-ITM                PIC  X(01).
       77  WS-EXT-STK                PIC  X(01).
       77  WS-MTD-YTD                PIC  X(01).
       77  WS-ENQD                   PIC  X(01).
       77  WS-ENQ                    PIC  X(01).
       77  WS-EX-VAL                 PIC  X(01).
       77  WS-VIEW                   PIC  X(01).
       77  WS-C-PRICE                PIC  X(01).
       77  WS-START-MONTH            PIC  X(01).
       77  WS-SMAN                   PIC  9(02).
       77  WS-MONTH                  PIC  9(02).
       77  WS-ETYPE                  PIC  X(01).
       77  WS-TYP                    PIC  X(01).
       77  WS-ERROR                  PIC  9(01)    VALUE ZERO.
       77  WS-ERR2                   PIC  X(09)    VALUE "No Record".
       77  TODAY-DDMMYY              PIC  9(08)    COMP-5.
       77  WS-USUB                   PIC  9(04)    COMP-5.
       01  WS-DB-LINE.
           03  WS-TOP-LNE.
               05  WS-TCHR PIC X(01) OCCURS 80.
           03  WS-T-LINE REDEFINES WS-TOP-LNE.
               05  FILLER  PIC X(01).
               05  WS-H-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
           03  WS-TOP-LNE2.
               05  WS-TCH  PIC X(01) OCCURS 80.
           03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
               05  FILLER      PIC  X(01).
               05  WS-TOP-COMP PIC  X(40).
              05  FILLER      PIC  X(23).
        05  WS-WRKHD    PIC  X(11).
              05  FILLER      PIC  X(01).
        05  WS-WRKST    PIC  X(03).
              05  FILLER      PIC  X(01).
          03  WS-MID-LNE.
              05  WS-MCHR  PIC  X(80) VALUE "³                ³
      -        "                      ³          ³         ³         ³".
    03  WS-MST-LNE REDEFINES WS-MID-LNE.
        05  WS-MST1     PIC  X(01).
        05  WS-MST78    PIC  X(78).
        05  WS-MST80    PIC  X(01).
          03  WS-MD-LNE.
              05  WS-MCH   PIC  X(80) VALUE "³      ³
      -        "                      ³           ³          ³       ³".
    03  WS-MDDT-LNE REDEFINES WS-MD-LNE.
        05  WS-MDT1     PIC  X(01).
        05  WS-MDT78    PIC  X(78).
        05  WS-MDT80    PIC  X(01).
          03  WS-MID-LNE2.
        05  FILLER      PIC  X(01) VALUE "³".
        05  WS-BLNK78   PIC  X(78) VALUE ALL "°".
        05  FILLER      PIC  X(01) VALUE "³".
          03  WS-BOT-LNE.
               05  WS-BCHR PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE.
               05  FILLER  PIC X(01).
               05  WS-F-LINE
                           PIC X(78).
               05  FILLER  PIC X(01).
           03  WS-BOT-LNE2.
               05  WS-BCH  PIC X(01) OCCURS 80.
           03  WS-B-LINE REDEFINES WS-BOT-LNE2.
               05  FILLER  PIC X(01).
               05  WS-MEM-LINE.
                  07  WS-SRT-LINE
                           PIC X(50).
                  07  FILLER
                           PIC X(10).
               05  FILLER  PIC X(19).
           03  WS-MM-LINE.
               05  FILLER  PIC  X(01) VALUE X"B3".
               05  FILLER  PIC  X(60) VALUE SPACES.
               05  FILLER  PIC  X(01) VALUE X"B3".
           03  WS-HME-LINE PIC  X(60) VALUE ALL X"CD".

       COPY "WS.WS".

       01  WS-HELP.
           03  WS-MODULE             PIC  X(03)    VALUE "DTP".
           03  WS-PROG               PIC  X(03)    VALUE "SAL".

       01  WS-PARID.
          03  WS-SYS-ID              PIC  X(03).

       01  W02-FID.

           03  W02-DEBDET.
               05  W02-EXPORT        PIC X(64) VALUE "DEBTSTAT.TXT".

       01  W02-PRINTER-DETAILS.
           03  W02-PRINTER           PIC  X(12).
           03  W02-PGE-LENGTH        PIC  9(03).
           03  W02-PRN-LENGTH        PIC  9(03).
           03  W02-LINAGE            PIC  9(03).
           03  W02-PRN-STATUS        PIC  X(01) VALUE "C".
      *
      *    ****    D  =  Detail line
      *            C  =  Condensed print
      *            E  =  Expanded print
      *            H  =  Header line
      *            X  =  Cancel expanded print
      *            1  =  10 Characters per inch
      *            2  =  12 Characters per inch
      *            3  =  17 Characters per inch
      *            6  =  6 Lines per inch
      *            8  =  8 Lines per inch
      *
           03  W02-PRN-TYPE          PIC  X(01).
           03  W02-PRN-LINE          PIC  X(136).

           03  R-SL1                               REDEFINES W02-PRN-LINE.
               05  R-SLA             PIC  X(78).
               05  FILLER            PIC  X(58).

           03  R-SL2                               REDEFINES W02-PRN-LINE.
               05  FILLER            PIC  X(58).
               05  R-SLB             PIC  X(78).

           03  REP-LINE1                           REDEFINES W02-PRN-LINE.
               05  REP-DETAIL1       PIC  X(136).

           03  REP-LINE2                           REDEFINES W02-PRN-LINE.
               05  REP-DETAIL2       PIC  X(80).

           03  REP-LINE3                           REDEFINES W02-PRN-LINE.
               05  REP-CO            PIC  X(40).

           03  PRF-LINE1                           REDEFINES W02-PRN-LINE.
               05  PRF-PAGE          PIC  X(06).
               05  PRF-P-NO          PIC  Z(03)9.
               05  FILLER            PIC  X(10).
               05  PRF-COMPANY       PIC  X(40).
               05  FILLER            PIC  X(04).
               05  PRF-DATE          PIC  X(06).
               05  PRF-DTE           PIC  Z9/99/9999.

           03  PRF-LINE2                           REDEFINES W02-PRN-LINE.
               05  PRF-SP-H          PIC  X(14).
               05  PRF-SP-NO         PIC  Z9.
               05  FILLER            PIC  X(01).
               05  PRF-SP-N          PIC  X(30).

           03  PRF-LINE3                           REDEFINES W02-PRN-LINE.
               05  PRF-H1            PIC  X(07).
               05  PRF-H2            PIC  X(31).
               05  PRF-H3.
                   07  FILLER        PIC  X(01).
                   07  PRF-H3A       PIC  X(13).
                   07  PRF-H3B       PIC  X(15).
                   07  PRF-H3C       PIC  X(11).
                   07  PRF-H3D       PIC  X(06).
               05  PRF-H4.
                   07  FILLER        PIC  X(01).
                   07  PRF-H4A       PIC  X(13).
                   07  PRF-H4B       PIC  X(15).
                   07  PRF-H4C       PIC  X(11).
                   07  PRF-H4D       PIC  X(06).
               05  FILLER            PIC  X(01).
               05  PRF-H5            PIC  X(03).

           03  PRF-LINE4                           REDEFINES W02-PRN-LINE.
               05  PRF-ACNO          PIC  X(06).
               05  FILLER            PIC  X(01).
               05  PRF-NAME          PIC  X(30).
               05  FILLER            PIC  X(01).
               05  PRF-MTD-S         PIC  --------9.99.
               05  FILLER            PIC  X(01).
               05  PRF-MTD-C         PIC  --------9.99.
               05  FILLER            PIC  X(01).
               05  PRF-MTD-GP        PIC  --------9.99.
               05  FILLER            PIC  X(01).
               05  PRF-MTD-PER       PIC  --9.99.
               05  FILLER            PIC  X(01).
               05  PRF-YTD-S         PIC  --------9.99.
               05  FILLER            PIC  X(01).
               05  PRF-YTD-C         PIC  --------9.99.
               05  FILLER            PIC  X(01).
               05  PRF-YTD-GP        PIC  --------9.99.
               05  FILLER            PIC  X(01).
               05  PRF-YTD-PER       PIC  --9.99.
               05  PRF-SEQ           PIC  ZZZZ9.

       COPY "W05.VAT".

       COPY "W10.STK".

      *
      *    Date routines for accepting checking and formatting have
      *    been removed from each program and the DateCheck program
      *    will be called to handle these routines.
      *
      *    The following copy 'DateVariables' now includes the 'W12.WS'
      *    and 'W20.WS' variables and is passed to DateCheck in the CALL
      *    statement.
      *
       COPY "DateVariables".

       COPY "W15.WS".

       01  W15-ALLOCATIONS.
           03  W15-DETAIL.
               05  W15-TRAN OCCURS 15.
                   07  W15-VALUE     PIC  Z(06)9.99- BLANK WHEN ZERO.
                   07  W15-ALLOC     PIC  Z(06)9.99- BLANK WHEN ZERO.
                   07  W15-REC       PIC  X(08).
           03  W15-DISPLAY                         REDEFINES W15-DETAIL.
               05  W15-BAL1          PIC  X(11).
               05  W15-ALL1          PIC  X(11).
               05  W15-REC1          PIC  X(08).
               05  W15-BAL2          PIC  X(11).
               05  W15-ALL2          PIC  X(11).
               05  W15-REC2          PIC  X(08).
               05  W15-BAL3          PIC  X(11).
               05  W15-ALL3          PIC  X(11).
               05  W15-REC3          PIC  X(08).
               05  W15-BAL4          PIC  X(11).
               05  W15-ALL4          PIC  X(11).
               05  W15-REC4          PIC  X(08).
               05  W15-BAL5          PIC  X(11).
               05  W15-ALL5          PIC  X(11).
               05  W15-REC5          PIC  X(08).
               05  W15-BAL6          PIC  X(11).
               05  W15-ALL6          PIC  X(11).
               05  W15-REC6          PIC  X(08).
               05  W15-BAL7          PIC  X(11).
               05  W15-ALL7          PIC  X(11).
               05  W15-REC7          PIC  X(08).
               05  W15-BAL8          PIC  X(11).
               05  W15-ALL8          PIC  X(11).
               05  W15-REC8          PIC  X(08).
               05  W15-BAL9          PIC  X(11).
               05  W15-ALL9          PIC  X(11).
               05  W15-REC9          PIC  X(08).
               05  W15-BAL10         PIC  X(11).
               05  W15-ALL10         PIC  X(11).
               05  W15-REC10         PIC  X(08).
               05  W15-BAL11         PIC  X(11).
               05  W15-ALL11         PIC  X(11).
               05  W15-REC11         PIC  X(08).
               05  W15-BAL12         PIC  X(11).
               05  W15-ALL12         PIC  X(11).
               05  W15-REC12         PIC  X(08).
               05  W15-BAL13         PIC  X(11).
               05  W15-ALL13         PIC  X(11).
               05  W15-REC13         PIC  X(08).
               05  W15-BAL14         PIC  X(11).
               05  W15-ALL14         PIC  X(11).
               05  W15-REC14         PIC  X(08).
               05  W15-BAL15         PIC  X(11).
               05  W15-ALL15         PIC  X(11).
               05  W15-REC15         PIC  X(08).

       01  W25-CALCS.
           03  W25-RESULT            PIC  9(05)V99.
           03  W25-RESULT1                         REDEFINES W25-RESULT.
               05  W25-WHOLE         PIC  9(05).
               05  W25-DECIMAL       PIC  9(02).
           03  W25-RESULT2                         REDEFINES W25-RESULT.
               05  W25-KEY           PIC  9(04).
               05  W25-SUB           PIC  9(01).
               05  FILLER            PIC  9(02).
           03  W25-TOTAL             PIC S9(09)V99  COMP-3.
           03  W25-VALUE             PIC S9(09)V99  COMP-3.
           03  W25-COST              PIC S9(09)V999 COMP-3.

       01  W30-MONTHS.
           03  W30-CURR              PIC  9(02).
           03  W30-START             PIC  9(02).
           03  W30-END               PIC  9(02).
           03  W30-MONTH-NAMES.
               05  W30-ENG.
                   07  FILLER        PIC  X(09)    VALUE "January  ".
                   07  FILLER        PIC  X(09)    VALUE "February ".
                   07  FILLER        PIC  X(09)    VALUE "March   ".
                   07  FILLER        PIC  X(09)    VALUE "April    ".
                   07  FILLER        PIC  X(09)    VALUE "May      ".
                   07  FILLER        PIC  X(09)    VALUE "June     ".
                   07  FILLER        PIC  X(09)    VALUE "July     ".
                   07  FILLER        PIC  X(09)    VALUE "August   ".
                   07  FILLER        PIC  X(09)    VALUE "September".
                   07  FILLER        PIC  X(09)    VALUE "October  ".
                   07  FILLER        PIC  X(09)    VALUE "November ".
                   07  FILLER        PIC  X(09)    VALUE "December ".
           03  W30-MONTH-N                         REDEFINES W30-MONTH-NAMES
               05  W30-MONTH         PIC  X(09)    OCCURS 12.
           03  W30-START-N           PIC  X(10).
           03  W30-END-N             PIC  X(10).

       01  W32-MONTHS.
           03  W32-MONTH-NAMES.
               05  W32-ENG           PIC  X(36)    VALUE "JanFebMarAprMayJunJulAugSepOctNovDec".
           03  W32-MONTH-N                         REDEFINES W32-MONTH-NAMES.
               05  W32-MONTH         PIC  X(03)    OCCURS 12.

       01  W35-EDIT.
           03  W35-COST              PIC  Z(08)9.99- BLANK WHEN ZERO.
           03  W35-SELL              PIC  Z(08)9.99- BLANK WHEN ZERO.
           03  W35-PROF              PIC  Z(08)9.99- BLANK WHEN ZERO.
           03  W35-GP                PIC  Z(02)9.99- BLANK WHEN ZERO.

       COPY "W40.DBT".

       01  W40-BALANCES.
      *    03 W40-A-MTD              PIC S9(09)V99 COMP-3.
      *    03 W40-A-CUR              PIC S9(09)V99 COMP-3.
      *    03 W40-A-30               PIC S9(09)V99 COMP-3.
      *    03 W40-A-60               PIC S9(09)V99 COMP-3.
      *    03 W40-A-90               PIC S9(09)V99 COMP-3.
      *    03 W40-A-120              PIC S9(09)V99 COMP-3.
      *    03 W40-A-INT              PIC S9(09)V99 COMP-3.
      *    03 W40-D-MTD              PIC S9(09)V99 COMP-3.
      *    03 W40-D-CUR              PIC S9(09)V99 COMP-3.
      *    03 W40-D-30               PIC S9(09)V99 COMP-3.
      *    03 W40-D-60               PIC S9(09)V99 COMP-3.
      *    03 W40-D-90               PIC S9(09)V99 COMP-3.
      *    03 W40-D-120              PIC S9(09)V99 COMP-3.
      *    03 W40-D-INT              PIC S9(09)V99 COMP-3.
      *    03 W40-T-MTD              PIC S9(09)V99 COMP-3.
      *    03 W40-T-CUR              PIC S9(09)V99 COMP-3.
      *    03 W40-T-30               PIC S9(09)V99 COMP-3.
      *    03 W40-T-60               PIC S9(09)V99 COMP-3.
      *    03 W40-T-90               PIC S9(09)V99 COMP-3.
      *    03 W40-T-120              PIC S9(09)V99 COMP-3.
      *    03 W40-T-INT              PIC S9(09)V99 COMP-3.
           03 W40-M-COST             PIC S9(09)V99 COMP-3.
           03 W40-M-SELL             PIC S9(09)V99 COMP-3.
           03 W40-M-GP               PIC S9(09)V99 COMP-3.
           03 W40-Y-COST             PIC S9(09)V99 COMP-3.
           03 W40-Y-SELL             PIC S9(09)V99 COMP-3.
           03 W40-Y-GP               PIC S9(09)V99 COMP-3.

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE              PIC  9(02).
           03  W45-ENG               PIC  X(12).
           03  W45-AFR               PIC  X(12).
           03  W45-ACT               PIC  X(01).

       COPY "FUNCTION.WS".

       COPY "W50.WS".

       01  W55-EXPORT.
           03  W55-S1                PIC  9(04)    COMP-5.
           03  W55-EXP-DATA.
               05  W55-EXP           PIC  X(01)    OCCURS 512.

       01  W60-NME-ADD.
           03  W60-NAME.
               05  W60-NCHAR         PIC  X(01)    OCCURS 40.
           03  W60-SALESMAN                        REDEFINES W60-NAME.
               05  W60-SMAN          PIC  X(30).
               05  FILLER            PIC  X(10).
           03  W60-ADDRESS.
               05  W60-ACHAR         PIC  X(01)    OCCURS 70.
           03  W60-ADDR                            REDEFINES W60-ADDRESS.
               05  W60-ADD           PIC  X(60).
               05  FILLER            PIC  X(10).
           03  W60-ADDRESS1.
               05  W60-PAD           PIC  X(60).
               05  FILLER            PIC  X(10).

       01  W60-FIELDS.
           03  W60-S                 PIC  9(02)    COMP-5.
           03  W60-FNO               PIC  9(02)    COMP-5 OCCURS 41.

       01  W65-FIELDS.
           03  W65-S                 PIC  9(02)    COMP-5.
           03  W65-FIELD             PIC  X(22).
           03  W65-FLDS.
               05  FILLER            PIC  X(15)    VALUE "Ac Number".
               05  FILLER            PIC  X(15)    VALUE "Name".
               05  FILLER            PIC  X(15)    VALUE "Item Code".
               05  FILLER            PIC  X(15)    VALUE "January Quant".
               05  FILLER            PIC  X(15)    VALUE "February Quant".
               05  FILLER            PIC  X(15)    VALUE "March Quant".
               05  FILLER            PIC  X(15)    VALUE "April Quant".
               05  FILLER            PIC  X(15)    VALUE "May Quant".
               05  FILLER            PIC  X(15)    VALUE "June Quant".
               05  FILLER            PIC  X(15)    VALUE "July Quant".
               05  FILLER            PIC  X(15)    VALUE "August Quant".
               05  FILLER            PIC  X(15)    VALUE "September Quant".
               05  FILLER            PIC  X(15)    VALUE "October Quant".
               05  FILLER            PIC  X(15)    VALUE "November Quant".
               05  FILLER            PIC  X(15)    VALUE "December Quant".
               05  FILLER            PIC  X(15)    VALUE SPACES.
               05  FILLER            PIC  X(15)    VALUE SPACES.
               05  FILLER            PIC  X(15)    VALUE SPACES.
               05  FILLER            PIC  X(15)    VALUE SPACES.
               05  FILLER            PIC  X(15)    VALUE SPACES.
               05  FILLER            PIC  X(15)    VALUE SPACES.
               05  FILLER            PIC  X(15)    VALUE "Description".
               05  FILLER            PIC  X(15)    VALUE "Month".
               05  FILLER            PIC  X(15)    VALUE "Year".
               05  FILLER            PIC  X(15)    VALUE "Quantity".
               05  FILLER            PIC  X(15)    VALUE "Price".
               05  FILLER            PIC  X(15)    VALUE "Extended Price".
           03  W65-RFLDS                           REDEFINES W65-FLDS.
               05  W65-FLD           PIC  X(15)    OCCURS 27.

       01  W65-MONTHS.
           03  W65-MNTH-NAMES.
               05  W65-MNTHS                       OCCURS 12.
                   07  FILLER        PIC  X(02).
                   07  W65-MNTH      PIC  X(03).
           03  W65-MNTH-NMS                        REDEFINES W65-MNTH-NAMES.
               05  W65-FIRST-6       PIC  X(30).
               05  W65-LAST-6        PIC  X(30).
       01  W65-MONTHS-GP.
           03  W65-MNTH-GP.
               05  W65-GP-HEAD       PIC  X(84)    VALUE "     GP     GP     GP     GP     GP     GP     GP     GP     GP     GP     GP     GP".
               05  W65-GP                          REDEFINES W65-GP-HEAD OCCURS 12.
                   07  FILLER        PIC  X(01).
                   07  W65-MTH       PIC  X(03).
                   07  FILLER        PIC  X(03).
           03  W65-GP-NMS                          REDEFINES W65-MNTH-GP.
               05  W65-FIRST-4       PIC  X(28).
               05  W65-MID-4         PIC  X(28).
               05  W65-LAST-4        PIC  X(28).

       01  W75-KEYS.
    03  W75-S       PIC  9(02) COMP-5.
    03  W75-S1       PIC  9(02) COMP-5.
    03  W75-S2       PIC  9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
          03  W75-DNO       PIC  X(06) OCCURS 18.
    03  W75-ITEMS OCCURS 16.
        05  W75-ITEM   PIC  X(18).
        05  W75-DESC   PIC  X(29).
        05  W75-QNT    PIC S9(04)V999 COMP-3 OCCURS 12.

       01  W75-STORE.
          03  W75-SNAME      PIC  X(08).
    03  W75-SREF       PIC  X(08).
    03  W75-SACN       PIC  X(06).
    03  W75-SDTE       PIC  9(08).
    03  W75-SSM       PIC  9(02).

       01  W80-ACCOUNT.
          03  W80-ACNO        PIC  X(06).
          03  W80-TYPE        PIC  9(01).

       01  W95-STM.
           03  W95-COMP        PIC  X(40).

       01  W100-HEAD.
          03  FILLER        PIC  X(19)    VALUE
              "SALES STATISTICS -".
          03  W100-DESC       PIC  X(21).

       01  W100-EDIT.
          03  W100-LBAL       PIC  Z(06)9.99-.
          03  W100-LARR       PIC  Z(06)9.99.
    03  W100-COST       PIC  Z(06)9.99.
          03  W100-SELL       PIC  Z(06)9.99.
          03  W100-CUR        PIC  Z(08)9.99-.
          03  W100-30        PIC  Z(08)9.99-.
          03  W100-60        PIC  Z(08)9.99-.
          03  W100-90        PIC  Z(08)9.99-.
          03  W100-120        PIC  Z(08)9.99-.
          03  W100-INT        PIC  Z(08)9.99.
          03  W100-CRL        PIC  Z(06)9.
          03  W100-KEY        PIC  Z(05)9.
    03  W100-SER        PIC  ZZ9.
          03  W100-PRC        PIC  Z(06)9.99-.
          03  W100-VAL        PIC  Z(08)9.99-.
          03  W100-VAL2.
              05  W100-S6V2   PIC  Z(05)9.99-.
          03  W100-VAL3 REDEFINES W100-VAL2.
              05  W100-PRICE  PIC  X(09).
              05  FILLER      PIC  X(01).
          03  W100-VAL4 REDEFINES W100-VAL2.
              05  W100-EXT    PIC  Z(06)9.99.
    03  W100-VAL5 REDEFINES W100-VAL2.
        05  FILLER      PIC  X(02).
        05  W100-QNT    PIC  Z(06)9-.
    03  W100-VAL6 REDEFINES W100-VAL2.
        05  FILLER      PIC  X(01).
              05  W100-6V2    PIC  Z(05)9.99.
    03  W100-VAL7 REDEFINES W100-VAL2.
        05  FILLER      PIC  X(06).
        05  W100-QNT2   PIC  -(03)9.
    03  W100-VAL8 REDEFINES W100-VAL2.
        05  FILLER      PIC  X(01).
        05  W100-4V3    PIC  Z(03)9.999-.
    03  W100-VAL9 REDEFINES W100-VAL2.
        05  FILLER      PIC  X(04).
        05  W100-2V3    PIC  Z(01)9.999.
    03  W100-S9V99      PIC  -(08)9.99.
    03  W100-S3V99      PIC  -(02)9.99.
    03  W100-1.
               05  W100-N      PIC  9(01).
    03  W100-2.
               05  W100-N2     PIC  9(02).
    03  W100-4.
        05  W100-N4     PIC  9(04).
    03  W100-7.
        05  W100-NO2    PIC  Z(06)9.
    03  W100-S7.
        05  W100-NO3    PIC  -(07)9.
    03  W100-S7M REDEFINES W100-S7.
        05  W100-NO4    PIC  -(07)9.
    03  W100-S9V2.
        05  W100-V1     PIC  -(09)9.99.
    03  W100-S9V3.
        05  W100-V2     PIC  -(08)9.999.
    03  W100-S5V2.
        05  W100-V3     PIC  -(05)9.99.
    03  W100-DATE.
               05  W100-DTE    PIC  9(06).

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
      *    **        ********  **    **  **    **    ****     ******   ********
      *    **           **     ***   **  **   **    **  **   **    **  **
      *    **           **     ****  **  **  **    **    **  **        ** 
      *    **           **     ** ** **  *****     ********  **        *****
      *    **           **     **  ****  **  **    **    **  **   ***  **  
      *    **           **     **   ***  **   **   **    **  **    **  **
      *    ********  ********  **    **  **    **  **    **   ******   ********
      *
       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE.IDS".

      /
      *   ******    ******   *******   ********  ********  **     **
      *  **    **  **    **  **    **  **        **        ***    **
      *  **        **        **   **   **        **        ** *   **
      *   ******   **        ******    *****     *****     **  *  **
      *        **  **        **   **   **        **        **   * **
      *  **    **  **    **  **    **  **        **        **    ***
      *   ******    ******   **    **  ********  ********  **     **
      *
       SCREEN SECTION.

       01  MENU-INSTRUCT.
    03  LINE 25 COLUMN  2 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE "Use ".
    03        COLUMN  6 HIGHLIGHT
     BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     VALUE "".
    03        COLUMN  7 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " or ".
    03        COLUMN 11 HIGHLIGHT
     BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     VALUE "".
    03        COLUMN 12 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " to point to an item, and ".
    03        COLUMN 38 HIGHLIGHT
     BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     VALUE "<Enter>".
    03        COLUMN 45 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " to Select ".
    03        COLUMN 56 HIGHLIGHT
     BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6
     VALUE "<Esc>".
    03        COLUMN 61 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE " to Exit".

       01  S-MENU.
    03  LINE  4 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
    03        COLUMN 56 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
     VALUE "¿".
    03  LINE  5 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³     ".
    03        COLUMN 35 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "HISTORY OPTIONS".
    03        COLUMN 50 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE "      ³".
    03  LINE  6 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "1".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Extract History Period ³".
    03  LINE  7 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "2".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Extract Last 12 Months ³".
    03  LINE  8 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "3".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " View Selected Period   ³".
    03  LINE  9 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "4".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " View Last 12 Months    ³".
    03  LINE 10 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "5".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " View GP for Selected P.³".
    03  LINE 11 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "6".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " View GP for 12 Months  ³".
    03  LINE 12 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "7".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Export Selected Period ³".
    03  LINE 13 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "8".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Export last 12 Months  ³".
          03  LINE 14 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "A".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Profit for Selected A/C³".
          03  LINE 15 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "B".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Purchase Stats MTD/YTD ³".
          03  LINE 16 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "³ ".
    03        COLUMN 31 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 7
     HIGHLIGHT
     VALUE "0".
    03        COLUMN 32 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 0
     VALUE " Exit                   ³".
    03  LINE 17 COLUMN 29 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 2 HIGHLIGHT
     VALUE "À".
    03        COLUMN 30 BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
     VALUE "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S-PROG.
          03  LINE  2 COLUMN 21 FOREGROUND-COLOR 7
     HIGHLIGHT VALUE
               "RECEIVABLE - PURCHASE HISTORY STATISTICS".

       01  S-PROG2.
          03  LINE  2 COLUMN 21 FOREGROUND-COLOR 7
     HIGHLIGHT VALUE
               "RECEIVABLE - PROFIT FOR SELECTED PERIOD".

       01  S-PROG3.
          03  LINE  2 COLUMN 20 FOREGROUND-COLOR 7
     HIGHLIGHT VALUE
               "RECEIVABLE - TURNOVER FOR SELECTED PERIOD".

       01  S-PROG4.
          03  LINE  2 COLUMN 25 FOREGROUND-COLOR 7
     HIGHLIGHT VALUE
               "RECEIVABLE - STATISTICS MTD/YTD".

       01  S05D.
    03  LINE  4 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6
         VALUE "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
    03  LINE  5 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6
         VALUE "³     Purchases for Period.    ³".
    03  LINE  6 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6
         VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´".
    03  LINE  7 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6
         VALUE "³ Starting Month:              ³".
    03  LINE  8 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6
         VALUE "³ Ending Month  :              ³".
    03  LINE  9 COLUMN 25 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 6
         VALUE "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       01  S05A.
    03  LINE  7 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC 99 USING W30-START AUTO.
    03  LINE  8 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC 99 USING W30-END AUTO.

       01  S05B.
    03  LINE  7 COLUMN 44 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC 99 USING W30-START AUTO.

       01  S10D.
    02  S10D-1.
          03  LINE  2 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
               "RECEIVABLE - PURCHASE HISTORY STATISTICS".
    02  S10D-2.
    03  LINE  3 COLUMN  3 VALUE "A/C.".
    03        COLUMN 56 VALUE "Item°°°°°°°°°°°°°°°°°°°°".

       01  S10D-G.
    02  S10D-3.
          03  LINE  2 COLUMN 22 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
               "RECEIVABLE - G.P. FOR SELECTED PERIOD".
    02  S10D-4.
    03  LINE  3 COLUMN  3 VALUE "A/C.".
    03        COLUMN 56 VALUE "Item°°°°°°°°°°°°°°°°°°°°".

       01  S10D-EX.
    02  S10D-E1.
          03  LINE  2 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
               "RECEIVABLE - EXPORT SELECTIVE STATISTICS".
    02  S10D-E2.
    03  LINE  3 COLUMN  3 VALUE "A/C.".

       01  S10D-E12.
    02  S10D-E3.
          03  LINE  2 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
               "RECEIVABLE - EXPORT 12 MONTHS HISTORY".
    02  S10D-E4.
    03  LINE  3 COLUMN  3 VALUE "A/C.".

       01  S10D-P.
    02  S10D-P1.
          03  LINE  2 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
               "RECEIVABLE - PROFIT FOR SELECTED PERIOD".
    02  S10D-P2.
    03  LINE  3 COLUMN  3 VALUE "A/C.".

       01  S10D-12.
    02  S10D-12A.
          03  LINE  2 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
               "RECEIVABLE - 12 MONTHS PURHASE HISTORY".
    02  S10D-12B.
    03  LINE  3 COLUMN  3 VALUE "A/C.".
    03        COLUMN 56 VALUE "Item°°°°°°°°°°°°°°°°°°°°".

       01  S10D-12G.
    02  S10D-12C.
          03  LINE  2 COLUMN 21 FOREGROUND-COLOR 7 HIGHLIGHT VALUE
               "RECEIVABLE - 12 MONTHS G.P. HISTORY".
    02  S10D-12D.
    03  LINE  3 COLUMN  3 VALUE "A/C.".
    03        COLUMN 56 VALUE "Item°°°°°°°°°°°°°°°°°°°°".

       01  S10.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(06) USING W80-ACNO AUTO.

       01  S11.
    03  LINE  6 COLUMN 24 VALUE "Details Required:".
    03  LINE  8 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "A".
    03        COLUMN 25 VALUE "ll Items".
    03  LINE  9 COLUMN 24 FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "S".
    03        COLUMN 25 VALUE "pecific Item Code".
    03  LINE 11 COLUMN 24 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 1
     VALUE "Select ".
    03        COLUMN 31 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "A".
    03        COLUMN 32 BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
     VALUE " or ".
    03        COLUMN 36 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "S".
    03        COLUMN 37 BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
     VALUE "- press ".
    03        COLUMN 45 BACKGROUND-COLOR 3
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Esc".
    03        COLUMN 48 BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
     VALUE "ape to exit".

       01  S36-ITM.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(14)/X(03)
      USING W10-EXT-ITEM AUTO.

       01  S36-EXT.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(18) USING W10-EXT-ITEM AUTO.

       01  S36.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
      PIC X(14) USING W10-ITEM AUTO.

       01  S36-3.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(03) USING W10-I4 AUTO.

       01  S36-4.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(04) USING W10-I4 AUTO.

       01  S36-5.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(05) USING W10-I5 AUTO.

       01  S36-6.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(06) USING W10-I6 AUTO.

       01  S36-7.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(07) USING W10-I7 AUTO.

       01  S36-8.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(08) USING W10-I8 AUTO.

       01  S36-9.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(09) USING W10-I9 AUTO.

       01  S36-10.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(10) USING W10-I10 AUTO.

       01  S36-11.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(11) USING W10-I11 AUTO.

       01  S36-12.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(12) USING W10-I12 AUTO.

       01  S36-13.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(13) USING W10-I13 AUTO.

       01  S36-14.
          03  FOREGROUND-COLOR 7 HIGHLIGHT BACKGROUND-COLOR 5
           PIC Z(14) USING W10-I14 AUTO.

       01 S40.
    03  LINE 11 COLUMN 42 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC ZZZ9 USING W25-WHOLE AUTO.

       01 S41.
    03  LINE 12 COLUMN 24 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
     PIC Z9 USING WS-SMAN AUTO.

       COPY "DEBLUP.CRT".

       COPY "STKLUP.CRT".

       01  S-MOV.
           03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE
              "    Item Code              Description              Mont
      -        "h   Quantity  Av.Price".
          03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
          03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".

       01  S-HIST.
          03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE
              "    Item Code              Description          ".
    03        COLUMN 50 REVERSE-VIDEO PIC  X(30)
     USING W65-LAST-6.
          03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
          03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÏÍÍÍÍÏÍÍÍÍÏÍÍÍÍÏÍÍÍÍÏÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".

       01  S-HIST-2.
    03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE
           "    Item Code     ".
    03        COLUMN 20 REVERSE-VIDEO PIC  X(60)
     USING W65-MNTH-NAMES.
          03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍ
      -        "ÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
          03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÏÍÍÍÍÏÍÍÍ
      -        "ÍÏÍÍÍÍÏÍÍÍÍÏÍÍÍÍÏÍÍÍÍÏÍÍÍÍÏÍÍÍÍÏÍÍÍÍÏÍÍÍÍÏÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".

       01  S-GP12.
          03  LINE  4 COLUMN  2 REVERSE-VIDEO VALUE
              "    Item Code              Description          ".
    03        COLUMN 52 REVERSE-VIDEO PIC  X(30)
     USING W65-LAST-4.
          03  LINE  5 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÑÍÍÍÍÍÍÑÍÍÍÍÍÍÑÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
          03  LINE 22 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÏÍÍÍÍÍÍÏÍÍÍÍÍÍÏÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".

       01  S-PROF.
    03  LINE  4 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 7 VALUE
        "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÂÄÄÄÄÄÄÄ¿".
    03  LINE  5 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 7 VALUE
        "³    Month     ³  Total Cost ³Total Selling³   Total GP
      -        " ³  GP % ³".
    03  LINE  6 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 7 VALUE
        "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÅÄÄÄÄÄÄÄ´".
    03  LINE 19 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 7 VALUE
        "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÅÄÄÄÄÄÄÄ´".
    03  LINE 20 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 7 VALUE
        "³    TOTAL     ³             ³             ³
      -        " ³       ³".
    03  LINE 21 COLUMN 7 FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 7 VALUE
        "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄ
      -        "ÄÁÄÄÄÄÄÄÄÙ".

       01  S-TOP.
          03  LINE  4 COLUMN  2 FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7 VALUE
        "ActNo  Name                       Total Sell   Total Cos
      -        "t    GP Value    GP % ".
          03  LINE  5 COLUMN  1 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
     VALUE "ÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÑÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍ".
    03        COLUMN 80 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0 VALUE "µ".
          03  LINE 22 COLUMN  1 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
     VALUE "ÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÏÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍ".
    03        COLUMN 80 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0 VALUE "µ".

       01  SA-ACT.
          03  LINE  3 COLUMN 17 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5
           PIC  X(06) USING W80-ACNO AUTO.

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

      /
      *        *******   *******    ******    ******   ********  *******   **     **  *******    ******** 
      *        **    **  **    **  **    **  **    **  **        **    **  **     **  **    **   **
      *        **    **  **   **   **    **  **        **        **    **  **     **  **   **    **
      *        *******   ******    **    **  **        *****     **    **  **     **  ******     *****
      *        **        **   **   **    **  **        **        **    **  **     **  **   **    **
      *        **        **    **  **    **  **    **  **        **    **  **     **  **    **   **
      *        **        **    **   ******    ******   ********  *******    *******   **    **   ********
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000-MAIN        SECTION.
       AA000-INIT.
           IF LS0-DBLEV < 3
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.

       AA01.
           IF MOUSE-ATTACHED = "Y"
               MOVE 66               TO MOUSE-FUNC
               MOVE 1                TO MOUSE-PARAM
               CALL X"AF" USING MOUSE-FUNC MOUSE-PARAM.
             PERFORM ERASE-SCREEN.
             DISPLAY S-PROG.
             MOVE 5                  TO SHADE-ROW.
             MOVE 31                 TO SHADE-COL.
             MOVE 26                 TO SHADE-WIDTH.
             MOVE 13                 TO SHADE-LINES.
             DISPLAY S-MENU.
             DISPLAY MENU-INSTRUCT.
             PERFORM SCREEN-SHADOW.

       AA02.
      MOVE 5   TO TOP-ROW.
      MOVE 15   TO BOTTOM-ROW.
      MOVE 4   TO SCREEN-LIN.
      MOVE 31   TO SCREEN-COL.
      MOVE 24   TO STRING-LENGTH.
      PERFORM SAVE-ATTR.

       AA05.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       AA10.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY  PERFORM MOVE-BLOCK-UP
   WHEN DOWN-KEY  PERFORM MOVE-BLOCK-DOWN
   WHEN MOUSE-KEY  PERFORM POSITION-MOUSE
     IF MOUSE-ENTER = "Y"
         MOVE "N"  TO MOUSE-ENTER
         GO TO AA15
     END-IF
     IF MOUSE-Y = 24
         IF MOUSE-X = 5
      MOVE 5  TO KEY-CODE-1
      GO TO AA10
         ELSE
         IF MOUSE-X = 10
      MOVE 6  TO KEY-CODE-1
      GO TO AA10
         ELSE
         IF MOUSE-X > 36 AND < 44
      MOVE 0  TO KEY-CODE-1
      GO TO AA10
         ELSE
         IF MOUSE-X > 54 AND < 60
      MOVE 0  TO KEY-CODE-1
      MOVE 1  TO KEY-TYPE
      GO TO AA10
         END-IF
     END-IF
   WHEN ENTER-KEY  GO TO AA15
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO AA05
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE "0"  TO WS-OPTION
       GO TO AA18
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO AA05
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        IF (WS-OPTION < "0" OR > "8") AND
    (WS-OPTION < "A" OR > "B")
     CALL X"E5"
     GO TO AA05
        END-IF
        GO TO AA16
    ELSE
        CALL X"E5"
        GO TO AA05.

       AA15.
    IF SCREEN-LIN < 5
        CALL X"E5"
        GO TO AA05
    ELSE
    IF SCREEN-LIN = 5
        MOVE "1"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 6
        MOVE "2"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 7
        MOVE "3"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 8
        MOVE "4"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 9
        MOVE "5"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 10
        MOVE "6"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 11
        MOVE "7"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 12
        MOVE "8"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 13
        MOVE "A"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 14
        MOVE "B"   TO WS-OPTION
    ELSE
    IF SCREEN-LIN = 15
        MOVE "0"   TO WS-OPTION.
    GO TO AA18.

       AA16.
      PERFORM CLEAR-BLOCK.
    IF WS-OPTION = "0"
        MOVE 15   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "1"
        MOVE 5   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "2"
        MOVE 6   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "3"
        MOVE 7   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "4"
        MOVE 8   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "5"
        MOVE 9   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "6"
        MOVE 10   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "7"
        MOVE 11   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "8"
        MOVE 12   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "A"
        MOVE 13   TO SCREEN-LIN
    ELSE
    IF WS-OPTION = "B"
        MOVE 14   TO SCREEN-LIN.
      PERFORM SAVE-ATTR.
      PERFORM MARK-BLOCK.

       AA18.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 0   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
          IF WS-OPTION = "0"
              GO TO AA45.
           IF WS-OPTION = "1"
              PERFORM BB000
           ELSE
          IF WS-OPTION = "2"
              PERFORM BC000
           ELSE
          IF WS-OPTION = "3"
              PERFORM BA000
           ELSE
          IF WS-OPTION = "4"
              PERFORM BD000
           ELSE
          IF WS-OPTION = "5"
              PERFORM BE000
           ELSE
          IF WS-OPTION = "6"
              PERFORM BF000
           ELSE
          IF WS-OPTION = "7"
              PERFORM BG000
           ELSE
          IF WS-OPTION = "8"
              PERFORM BH000
           ELSE
          IF WS-OPTION = "A"
              PERFORM BI000
           ELSE
          IF WS-OPTION = "B"
              PERFORM BJ000.
            GO TO AA01.

       AA45.
      CLOSE CARDEX
     DEBTEX
     DEBHIS
     DEBPRF
     DEBTOT
     STOCK.

       AA49.
             EXIT PROGRAM.

       COPY "AA50DS.LUP".

      *    *************************************************************
      *    ****    G E T   T H E   E X P O R T   F I L E   I D
      *    *************************************************************
       AA100           SECTION.
       AA101.
      DISPLAY "Keyin the <pathname>/<filename> " AT 0702.
      DISPLAY "EXPORT file :" AT 0802.
             MOVE ZERO           TO WS-ERROR.
       AA105.
      ACCEPT W02-EXPORT AT 0816
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE 9  TO WS-ERROR
       GO TO AA199
   WHEN OTHER
       CALL X"E5"
       GO TO AA105
        END-EVALUATE.
      MOVE 64   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W02-EXPORT
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
             MOVE 1              TO WS-LENGTH.
      DISPLAY W02-EXPORT AT 0816
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 5 UPDATE AUTO.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
               GO TO AA105.
      PERFORM CLEAR-L50.
      OPEN OUTPUT DEBDET.
    IF WS-STATUS = "00"
      *        PERFORM AA600
               GO TO AA199.
      MOVE SPACES  TO WS-ERR-MES.
      STRING "Invalid file name" DELIMITED SIZE
      " (" DELIMITED SIZE
      WS-STATUS DELIMITED SIZE
      ")" DELIMITED SIZE
      INTO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.
      GO TO AA105.

       AA199.
             EXIT.
      *    *************************************************************
      *    ****    M O V E   F I E L D   T O   E X P O R T   A R E A
      *
      *            W50-DATA   : CONTAINS DATA TO BE INSERTED
      *            W50-LENGTH : LENGTH OF DATA
      *            W50-EXP    : FIELD TERMINATION CHARACTER
      *    *************************************************************
       AA200           SECTION.
       AA201.
           IF W50-DATA = SPACES
               GO TO AA225.
             MOVE W50-LENGTH     TO W50-S1.
      *    *************************************************************
      *    ****    D E T E R M I N E   E N D   O F   F I E L D
      *
      *            THIS LOOP EXCLUDES THE TRAILING SPACES
      *    *************************************************************
       AA205.
           IF W50-CCHAR (W50-S1) = SPACE
               SUBTRACT 1        FROM W50-S1
               GO TO AA205.
      *    *************************************************************
      *    ****    R E M O V E   L E A D I N G   S P A C E S
      *    *************************************************************
       AA210.
           IF W50-1 = SPACE
               MOVE W50-D39      TO W50-DATA
               SUBTRACT 1        FROM W50-S1
               GO TO AA210.
             MOVE W50-S1         TO W50-S2.
             MOVE 1              TO W50-S1.

       AA220.
             MOVE W50-CCHAR (W50-S1)
                                 TO W55-EXP (W55-S1).
             ADD 1               TO W50-S1
                                    W55-S1.
           IF W50-S1 NOT > W50-S2
               GO TO AA220.

       AA225.
             MOVE W50-EXP        TO W55-EXP (W55-S1).
             ADD 1               TO W55-S1.

       AA299.
             EXIT.
      *    *************************************************************
      *    ****   H E A D E R S
      *    *************************************************************
       AA600    SECTION.
       AA601.
      MOVE 1   TO W60-S.
      *    *************************************************************
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *    *************************************************************
             MOVE SPACES         TO W55-EXP-DATA.
             MOVE 1              TO W55-S1.

       AA605.
           IF W60-FNO (W60-S) = ZERO
        MOVE W55-EXP-DATA TO EXP-REC1
        PERFORM WRITE-DEBDET THRU WRITE-DEBDET-EXIT
        GO TO AA699.
             MOVE W60-FNO (W60-S)
                                 TO W50-BIN.
      MOVE 15   TO W50-LENGTH.
      MOVE W65-FLD (W50-BIN)
     TO W50-DATA.
      PERFORM AA200.
    IF W60-S < 41
               ADD 1             TO W60-S
        GO TO AA605.

       AA699.
             EXIT.
      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *     S C R E E N ,   K E Y B O A R D   &  M O U S E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³     SCREEN-SHADOW          ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:             ³
      *    ³        SHADE-ROW   - Top line of the box + 1.        ³
      *    ³        SHADE-COL   - Left line of box + 2.        ³
      *    ³        SHADE-WIDTH - Width of the box - 2.        ³
      *    ³        SHADE-LINES - Hight of box - 1.         ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³     CHECK-CORRECT          ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³       "Correct ENTER - N if incorrect"         ³
      *    ³              ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³              ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 13 (default); move another value to SLIN and PERFORM      ³
      *    ³ CHECK-SETUP THRU CHECK-EXIT.          ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³     ERROR-MESSAGE          ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display an error message with " - Press ANY key" at    ³
      *    ³ the end; use PERFORM ERROR-MESSAGE.         ³
      *    ³              ³
      *    ³ To display just the error message; use PERFORM        ³
      *    ³ ERROR-LENGTH THRU ERROR-EXIT.          ³
      *    ³              ³
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   ³
      *    ³ or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     ³
      *    ³ number to be used to SLIN and then PERFORM ERROR-DISPLAY  ³
      *    ³ THRU ERROR-EXIT.            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.CRT".

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
      *    ³   PERFORM OPT-MESSAGE.           ³
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
      *    ³ then PEFORM OPT-SETUP THRU OPT-EXIT.         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "OPTION.CRT".

       COPY "LOCKED.RC2".

       COPY "CLEAR.CRT".

      *    *************************************************************
      *    ****        D A T E  C H E C K   R O U T I N E
      *    *************************************************************
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Perform this routine with the date in W10-DATE (PIC      ³
      *    ³  99/99/9999) with the format as DD/MM/CCYY         ³
      *    ³              ³
      *    ³  On exiting - WS-ERROR will be zero (0) if the date is    ³
      *    ³  correct or one (1) if the date is incorrect.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "DATE.CHK".

       COPY "ABORT.PRN".

       COPY "PRINTUTL.AP5".

       COPY "AA300.STK".

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        R E A D  F I L E S   R O U T I N E S        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000    SECTION.

       COPY "CARDEX.RD".

       COPY "DEBALT.RD".

       COPY "DEBMEM.RD".

       COPY "DEBHIS.RD".

       COPY "DEBPRF.RD".

       COPY "DEBTEX.RD".

       COPY "DEBTOR.RD".

       COPY "DEBTOT.RD".

       COPY "DEPART.RD".

       COPY "PARAM.RD".

       COPY "STOCK.RD".

      /    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³R E W R I T E   & W R I T E   F I L E S R O U T I N E S³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AD000   SECTION.

       COPY "DEBHIS.WR".

       COPY "DEBPRF.WR".

       WRITE-DEBDET.
             WRITE EXP-REC1.
           IF WS-STATUS NOT = "00"
               MOVE 99           TO WS-F-ERROR
               PERFORM WRITE-ERROR.
      
       WRITE-DEBDET-EXIT.
             EXIT.

       COPY "DEBTEX.WR".

       COPY "DEBTOT.WR".

       COPY "DEBTOR.ALU".

       COPY "DEBTOR.ILU".

       COPY "APAC.HLP".

      /
      *  ****  E N Q U I R Y   ( P U R C H A S E   H I S T O R Y )
      *
       BA000         SECTION 50.
       BA00.
             PERFORM ERASE-SCREEN.
      DISPLAY S10D.

       BA05.
            DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " to do Account Lookup"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BA10.
            ACCEPT S10 AT 0308.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO BA15
         WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50D THRU AA59
   WHEN OTHER
       CALL X"E5"
       GO TO BA05
        END-EVALUATE
              DISPLAY S10 AT 0308
               IF W80-ACNO = SPACES
           GO TO BA05.

       BA15.
            PERFORM CLEAR-L50.
          IF W80-ACNO = SPACES
              GO TO BA999.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.

       BA20.
            PERFORM CLEAR-L50.
      *    IF W80-ACNO = "CASH"
      *        MOVE "CASH SALES" TO W40-NAME
      *        GO TO BA20-CHECK.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
              MOVE "No record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BA05.
           IF WS-ERROR = 2
              MOVE "Invalid account"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BA05.
      *
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *
            MOVE DEB-NAME  TO W40-NAME.
           IF W80-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            DISPLAY W40-NAME AT 0315
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
       BA20-CHECK.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        PERFORM CLEAR-L50
        GO TO BA05.

       BA22.
      DISPLAY S11.

       BA25.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             PERFORM CLEAR-L50
       GO TO BA05
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO BA25
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        IF NOT (WS-OPTION = "A" OR "S")
     CALL X"E5"
     GO TO BA25
        END-IF
    ELSE
        CALL X"E5"
        GO TO BA25.
      MOVE 0401   TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
      MOVE SPACES  TO W10-EXT-ITEM.
    IF W10-STCK = "N"
        IF W10-SLNGTH = 3
     MOVE ZERO  TO W10-I3
        ELSE
        IF W10-SLNGTH = 4
     MOVE ZERO  TO W10-I4
        ELSE
        IF W10-SLNGTH = 5
     MOVE ZERO  TO W10-I5
        ELSE
        IF W10-SLNGTH = 6
     MOVE ZERO  TO W10-I6
        ELSE
        IF W10-SLNGTH = 7
     MOVE ZERO  TO W10-I7
        ELSE
        IF W10-SLNGTH = 8
     MOVE ZERO  TO W10-I8
        ELSE
        IF W10-SLNGTH = 9
     MOVE ZERO  TO W10-I9
        ELSE
        IF W10-SLNGTH = 10
     MOVE ZERO  TO W10-I10
        ELSE
        IF W10-SLNGTH = 11
     MOVE ZERO  TO W10-I11
        ELSE
        IF W10-SLNGTH = 12
     MOVE ZERO  TO W10-I12
        ELSE
        IF W10-SLNGTH = 13
     MOVE ZERO  TO W10-I13
        ELSE
     MOVE ZERO  TO W10-I14.
      MOVE WS-OPTION  TO WS-ENQ.
      MOVE W12-MONTH  TO WS-MONTH.
    IF WS-ENQ = "A"
        GO TO BA45.

       BA30.
            PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     ACCEPT S36-EXT AT 0361
        ELSE
        IF WS-USE-ITM = "Y"
     ACCEPT S36-ITM AT 0361
        ELSE
           ACCEPT S36 AT 0361
        END-IF
    ELSE
    IF W10-SLNGTH = 3
        ACCEPT S36-3 AT 0361
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S36-4 AT 0361
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S36-5 AT 0361
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S36-6 AT 0361
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S36-7 AT 0361
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S36-8 AT 0361
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S36-9 AT 0361
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S36-10 AT 0361
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S36-11 AT 0361
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S36-12 AT 0361
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S36-13 AT 0361
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S36-14 AT 0361.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
             PERFORM CLEAR-L50
       GO TO BA05
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "I"  TO WS-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO WS-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO WS-ETYPE
   WHEN F5-KEY
       MOVE "X"  TO WS-ETYPE
   WHEN OTHER
       CALL X"E5"
       GO TO BA30
        END-EVALUATE
        MOVE "S"   TO WS-LOOK
              PERFORM AA50 THRU AA59
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S36-EXT AT 0361
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S36-ITM AT 0361
     ELSE
         DISPLAY S36 AT 0361
     END-IF
        ELSE
     DISPLAY S36 AT 0361.

       BA35.
          IF W10-EXT-ITEM = SPACES
              GO TO BA30.

       BA40.
             PERFORM CA000.
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     DISPLAY S36-EXT AT 0361
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S36-ITM AT 0361
        ELSE
           DISPLAY S36 AT 0361
        END-IF
    ELSE
        DISPLAY S36 AT 0361.
          IF WS-F-ERROR = 22
        MOVE "Z"   TO WS-ETYPE
              PERFORM AA50 THRU AA59
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S36-EXT AT 0361
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S36-ITM AT 0361
     ELSE
         DISPLAY S36 AT 0361
     END-IF
        ELSE
     DISPLAY S36 AT 0361
        END-IF
              IF W10-EXT-ITEM = SPACES
           GO TO BA30
               ELSE
     GO TO BA40.
      DISPLAY "Item Description" AT 0612.
      DISPLAY STK-DESC AT 0629
       WITH BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3 HIGHLIGHT.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BA30.
      GO TO BA55.

       BA45.
      MOVE "I"   TO WS-OPTION.

       BA50.
      MOVE SPACES     TO WS-ERR-MES.
            STRING "'I'tem or 'D'ate sequence  ["
      DELIMITED SIZE
      WS-OPTION DELIMITED SIZE
      "]" DELIMITED SIZE
        INTO WS-ERR-MES.
            PERFORM OPT-MESSAGE.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BA22
        END-EVALUATE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "I" OR "D")
        GO TO BA50.
      MOVE WS-OPTION  TO WS-ENQD.

       BA55.
      PERFORM MOVEMENT-LOOKUP.
      MOVE SPACES  TO W80-ACNO.
      GO TO BA00.

       BA999.
             EXIT.
      /
      *    ****    M O V E M E N T   L O O K U P
      *
       MOVEMENT-LOOKUP   SECTION 50.

       MOVE-LUP-SCREEN.
            DISPLAY S-MOV.
      INITIALIZE DXT-REC1.
    IF WS-ENQ = "S"
        MOVE W10-EXT-ITEM TO DXT-EXT-CODE.
      MOVE W80-ACNO  TO DXT-AC.
      MOVE W15-SMM  TO DXT-MNTH.
      MOVE W15-SCCYY  TO DXT-YEAR.

       MOVE-LUP-PERFORM ERASE-SCREEN.
             MOVE 0601           TO CPOS.
            PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.

       MOVE-LUP-CONTINUE.
            MOVE 6   TO CLIN.
            MOVE 1   TO W75-S.

       MOVE-LUP-TAB-LOOP.
      MOVE ZERO   TO W75-S1.

       MOVE-LUP-START-DBTX.
      MOVE SPACES  TO WS-OPTION.
    IF (WS-ENQ = "S") OR (WS-ENQD = "I")
        PERFORM START-AT-DBEX-AC THRU READ-DEBTEX-EXIT
    ELSE
              PERFORM START-AT-DBEX-AKEY THRU READ-DEBTEX-EXIT.
          IF WS-F-ERROR = 52
        GO TO MOVE-NO-RECORD.

       MOVE-GET-DEBTEX-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
        GO TO MOVE-SKIP-DBTX-READ.
            PERFORM READ-DEBTEX-NEXT THRU READ-DEBTEX-EXIT.
    IF WS-F-ERROR = 52
               SUBTRACT 1        FROM W75-S
        GO TO MOVE-FULL-SCREEN.

       MOVE-LUP-DISP-ITEM.
    IF NOT (DXT-AC = DEB-ACNO)
        GO TO MOVE-FULL-SCREEN.
    IF (WS-ENQ = "S") AND
       (W10-EXT-ITEM NOT = DXT-EXT-CODE)
        GO TO MOVE-FULL-SCREEN.

       MOVE-SKIP-DBTX-READ.
            DISPLAY DXT-EXT-CODE AT LINE CLIN COLUMN 2
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      DISPLAY DXT-DESC AT LINE CLIN COLUMN 21
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      DISPLAY DXT-MTH AT LINE CLIN COLUMN 52
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      MOVE DXT-QNT  TO W100-QNT.
      DISPLAY W100-QNT AT LINE CLIN COLUMN 62
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      COMPUTE W100-6V2 ROUNDED = DXT-TSELL / DXT-QNT.
      DISPLAY W100-6V2 AT LINE CLIN COLUMN 71
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
          IF CLIN < 21
               ADD 1             TO CLIN W75-S
              GO TO MOVE-GET-DEBTEX-REC.
      
       MOVE-FULL-SCREEN.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
001020      DISPLAY " " AT 2303 WITH FOREGROUND-COLOR 14
             " - " WITH FOREGROUND-COLOR 7
       "PgUp" WITH FOREGROUND-COLOR 14
                     "/" WITH FOREGROUND-COLOR 7
       "PgDn" WITH FOREGROUND-COLOR 14
       " - " WITH FOREGROUND-COLOR 7
             "Esc" WITH FOREGROUND-COLOR 14
       "ape." WITH FOREGROUND-COLOR 7.

       MOVE-SAVE-FULL-SCREEN.
      PERFORM SAVE-SCREEN-2.
            MOVE 0   TO W75-S1.
             MOVE 0601           TO CPOS.

       MOVE-CRT-ATTRIB.
      MOVE 3920   TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
           W42-ATTRIB2
           W41-LENGTH
     RETURNING WS-STATUS.
            MOVE 4000   TO W41-LENGTH.

       MOVE-GET-USER-OPT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
             GO TO MOVE-ADJ-NO-KEYS
   WHEN DOWN-KEY
             GO TO MOVE-CHK-NO-KEYS
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO MOVE-GET-USER-OPT
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO MOVEMENT-LOOKUP-EXIT
   WHEN PAGE-DOWN
             GO TO MOVE-GET-NEXT-PAGE
   WHEN PAGE-UP
             GO TO MOVE-GET-PREV-PAGE
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO MOVE-GET-USER-OPT.
      CALL X"E5".
            GO TO MOVE-GET-USER-OPT.

       MOVE-GET-NEXT-PAGE.
           IF W75-S < 16
              GO TO MOVE-GET-USER-OPT.

       MOVE-NEW-PAGE.
             MOVE 0601           TO CPOS.
            PERFORM MOVE-CRT-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
             MOVE LOW-VALUES     TO W75-KEYS.
             MOVE 1              TO W75-S W75-S1.
      PERFORM MOVE-LUP-TAB-LOOP.
            GO TO MOVE-GET-DEBTEX-REC.

       MOVE-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
       MOVE "J"    TO WS-OPTION.

       MOVE-GET-PREV-LOOP.
      PERFORM READ-DEBTEX-PREV THRU READ-DEBTEX-EXIT.
    IF WS-F-ERROR = 43
        MOVE SPACE  TO WS-OPTION
        GO TO MOVE-LUP-SCREEN.
    IF NOT (DXT-AC = DEB-ACNO)
        PERFORM READ-DEBTEX-NEXT THRU READ-DEBTEX-EXIT
        GO TO MOVE-NEW-PAGE.
    IF WS-IXS1 < 32
        ADD 1   TO WS-IXS1
        GO TO MOVE-GET-PREV-LOOP.
      GO TO MOVE-NEW-PAGE.

       MOVE-NO-RECORD.
      PERFORM CLEAR-L50.
            DISPLAY "No record - Press " AT 5012
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "ANY" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " key"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO MOVEMENT-LOOKUP-EXIT.

       MOVE-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1        FROM W75-S1 CLIN.
            GO TO MOVE-HIGHLIGHT.

       MOVE-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1            TO W75-S1
              GO TO MOVE-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1             TO W75-S1 CLIN.

       MOVE-HIGHLIGHT.
            PERFORM MOVE-CRT-ATTRIB.
             DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
            GO TO MOVE-GET-USER-OPT.

       MOVE-CRT-EDGE.
            DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
          FOREGROUND-COLOR 3
       "                  ³                              ³
      -       "         ³        ³         "
        WITH FOREGROUND-COLOR 3
       "³" WITH BACKGROUND-COLOR 0
         FOREGROUND-COLOR 3.
            ADD 1   TO CLIN.

       MOVEMENT-LOOKUP-EXIT.
             EXIT.
      /
      *  ****  E X T R A C T   ( P U R C H A S E   H I S T O R Y )
      *
       BB000         SECTION 51.
       BB00.
            PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.

       BB05.
      PERFORM SAVE-SCREEN-3.
      MOVE 5   TO SHADE-ROW.
      MOVE 27   TO SHADE-COL.
      MOVE 30   TO SHADE-WIDTH.
      MOVE 5   TO SHADE-LINES.
      DISPLAY S05D.
      PERFORM SCREEN-SHADOW.
      MOVE W12-MONTH  TO W30-END W30-CURR.
    IF W12-MONTH = 12
        MOVE 1   TO W30-START
    ELSE
        ADD 1 W12-MONTH  GIVING W30-START.

       BB10.
      DISPLAY "Press " AT 5052
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
       "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
    BACKGROUND-COLOR 3
       "ape to Exit"
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
      ACCEPT S05A.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BB999
   WHEN OTHER  CALL X"E5"
       GO TO BB10
        END-EVALUATE.
    IF (W30-START < 01 OR > 12) OR
       (W30-END < 01 OR > 12)
        MOVE "Months must be 01 to 12"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BB10.
      DISPLAY W30-MONTH(W30-START) AT 0747
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      DISPLAY W30-MONTH(W30-END) AT 0847
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BB10.
      PERFORM CLEAR-L50.
      MOVE W12-TODAY  TO W15-START W15-END.
      MOVE W30-START  TO W15-SMNTH.
      MOVE W30-END  TO W15-EMNTH.
      MOVE 01   TO W15-SDAY.
      MOVE 31   TO W15-EDAY.
      *
      *    ****   I S S T A R T   M O N T H -   PREVIOUS YEAR
      *
    IF W30-START > W30-CURR
        SUBTRACT 1  FROM W15-SCCYY
        IF W30-END > W30-CURR
     SUBTRACT 1  FROM W15-ECCYY
        END-IF
    ELSE
    IF W30-END < W30-START
        SUBTRACT 1  FROM W15-SCCYY.
    IF W15-EMNTH = 04 OR 06 OR 09 OR 11
        MOVE 30   TO W15-EDAY
    ELSE
    IF W15-EMNTH = 02
      *
      *    ****    C H E C K   F O R   L E A P  Y E A R
      *
              DIVIDE W15-ECCYY BY 4 GIVING W25-RESULT
              IF W25-DECIMAL = 0
     MOVE 29  TO W15-EDAY
        ELSE
     MOVE 28  TO W15-EDAY.
      *
      *    ****    SWITCH START DATE TO YEAR/MONTH/DAY
      *
      MOVE W15-SDAY  TO W15-SDD.
      MOVE W15-SMNTH  TO W15-SMM.
      MOVE W15-SCENT  TO W15-SCC.
      MOVE W15-SYEAR  TO W15-SYY.
      *
      *    ****    SWITCH END DATE TO YEAR/MONTH/DAY
      *
      MOVE W15-EDAY  TO W15-EDD.
      MOVE W15-EMNTH  TO W15-EMM.
      MOVE W15-ECENT  TO W15-ECC.
      MOVE W15-EYEAR  TO W15-EYY.
      PERFORM RESTORE-SCREEN-3.
      CLOSE DEBTEX.
      OPEN OUTPUT DEBTEX.
      CLOSE DEBTEX.
      OPEN I-O DEBTEX.
      *
      *    ****    G E T   T H E   F I R S T   D E B T O R
      *     A C C O U N T
      *
      MOVE " !"   TO DEB-ACNO.
      PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT.
          IF WS-F-ERROR = 6
        MOVE "No Details" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BB999.
      DISPLAY "Debtor account:" AT 0912.

       BB15.
      DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR 3.
      DISPLAY "Extracting:" AT 1112.
      PERFORM PRINT-ABORT.
    IF WS-OPTION = "Y"
        GO TO BB30.
            PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
              GO TO BB30.
      DISPLAY DEB-ACNO AT 0928
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      MOVE DEB-ACNO  TO CRD-AC.
      MOVE W15-S-YMD  TO CRD-DTE.
      PERFORM START-AT-CRDX-ACNT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        GO TO BB15.

       BB25.
            PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        GO TO BB15.
    IF NOT (CRD-AC = DEB-ACNO)
        GO TO BB15.
    IF CRD-DTE > W15-E-YMD
        GO TO BB15.
    IF CRD-QNT = ZERO
        GO TO BB25.
      DISPLAY CRD-DTE AT 1128
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
       CRD-EXT-CODE AT 1138
       WITH FOREGROUND-COLOR 6 HIGHLIGHT.
      MOVE CRD-AC  TO DXT-AC.
      MOVE CRD-EXT-CODE  TO DXT-EXT-CODE.
      MOVE CRD-DTE  TO W20-DTE.
      MOVE W20-MONTH  TO DXT-MNTH.
      MOVE W20-CNYR  TO DXT-YEAR.
            READ DEBTEX WITH IGNORE LOCK
        KEY IS DXT-AKEY.
          IF NOT (WS-STAT1 = "0")
        PERFORM BB990
        MOVE CRD-QNT  TO DXT-QNT
        IF (CRD-QNT < ZERO)
     COMPUTE DXT-TCOST = (CRD-COST * CRD-QNT) * -1
     COMPUTE DXT-TSELL = (CRD-SELL * CRD-QNT) * -1
        ELSE
     MULTIPLY CRD-COST BY CRD-QNT
         GIVING DXT-TCOST
     MULTIPLY CRD-SELL BY CRD-QNT
         GIVING DXT-TSELL
        END-IF
        MOVE W30-MONTH(W20-MONTH)
     TO DXT-MTH
        MOVE W15-EMM  TO DXT-EMTH
        MOVE STK-DESC  TO DXT-DESC
        PERFORM WRITE-DEBTEX THRU WRITE-DEBTEX-EXIT
    ELSE
        ADD CRD-QNT  TO DXT-QNT
        IF (CRD-QNT < ZERO)
     COMPUTE DXT-TCOST = DXT-TCOST +
           ((CRD-COST * CRD-QNT) * -1)
     COMPUTE DXT-TSELL = DXT-TSELL +
           ((CRD-SELL * CRD-QNT) * -1)
        ELSE
     COMPUTE DXT-TCOST = DXT-TCOST + (CRD-COST * CRD-QNT)
     COMPUTE DXT-TSELL = DXT-TSELL + (CRD-SELL * CRD-QNT)
        END-IF
        PERFORM REWRITE-DEBTEX THRU WRITE-DEBTEX-EXIT.
      GO TO BB25.

       BB30.
      DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR 3.
      CLOSE DEBTEX.
      OPEN I-O DEBTEX.
      GO TO BB999.

       BB990.
      MOVE CRD-EXT-CODE  TO W10-EXT-ITEM.
      PERFORM CA000.
    IF NOT (WS-F-ERROR = ZERO)
        MOVE "ITEM NO LONGER IN STOCK"
     TO STK-DESC.

       BB999.
      EXIT.
      /
      *  ****  E X T R A C T   ( 1 2   M O N T H   H I S T O R Y )
      *
       BC000         SECTION 52.
       BC00.
            PERFORM ERASE-SCREEN.
      DISPLAY S-PROG.

       BC05.
      MOVE W12-TODAY  TO W15-START W15-END.
      MOVE 01   TO W15-SDAY.
      MOVE 31   TO W15-EDAY.
    IF W15-SMNTH = 12
        MOVE 1   TO W15-SMNTH
    ELSE
        ADD 1   TO W15-SMNTH
        SUBTRACT 1  FROM W15-SCCYY.
    IF W15-EMNTH = 04 OR 06 OR 09 OR 11
        MOVE 30   TO W15-EDAY
    ELSE
    IF W15-EMNTH = 02
      *
      *    ****    C H E C K   F O R   L E A P  Y E A R
      *
              DIVIDE W15-ECCYY BY 4 GIVING W25-RESULT
              IF W25-DECIMAL = 0
     MOVE 29  TO W15-EDAY
        ELSE
     MOVE 28  TO W15-EDAY.
      *
      *    ****    SWITCH START DATE TO YEAR/MONTH/DAY
      *
      MOVE W15-SDAY  TO W15-SDD.
      MOVE W15-SMNTH  TO W15-SMM.
      MOVE W15-SCENT  TO W15-SCC.
      MOVE W15-SYEAR  TO W15-SYY.
      *
      *    ****    SWITCH END DATE TO YEAR/MONTH/DAY
      *
      MOVE W15-EDAY  TO W15-EDD.
      MOVE W15-EMNTH  TO W15-EMM.
      MOVE W15-ECENT  TO W15-ECC.
      MOVE W15-EYEAR  TO W15-EYY.

       BC10.
      CLOSE DEBHIS.
      OPEN OUTPUT DEBHIS.
      CLOSE DEBHIS.
      OPEN I-O DEBHIS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³      C r e a t e   R E S T A R T   r e c o r d        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      MOVE X"000000000001"
     TO DHS-AC.
      MOVE SPACES  TO DHS-S-AC DHS-E-AC DHS-COMP
      *
      *    ****    G E T   T H E   F I R S T   D E B T O R
      *     A C C O U N T
      *
      MOVE " !"   TO DEB-ACNO.
      PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT.
          IF WS-F-ERROR = 6
        MOVE "No Details" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BC999.
      DISPLAY "Debtor account:" AT 0912.

       BC15.
      DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR 3.
      DISPLAY "Extracting:" AT 1112.
      PERFORM PRINT-ABORT.
    IF WS-OPTION = "Y"
        GO TO BC25.
            PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
              GO TO BC25.
      DISPLAY DEB-ACNO AT 0928
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      MOVE DEB-ACNO  TO CRD-AC.
      MOVE W15-S-YMD  TO CRD-DTE.
      PERFORM START-AT-CRDX-ACNT THRU READ-CARDEX-EXIT.
          IF WS-F-ERROR = 43
        GO TO BC15.

       BC20.
            PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        GO TO BC15.
    IF NOT (CRD-AC = DEB-ACNO)
        GO TO BC15.
    IF CRD-DTE > W15-E-YMD
        GO TO BC15.
    IF CRD-QNT = ZERO
        GO TO BC20.
      DISPLAY CRD-DTE AT 1128
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
       CRD-EXT-CODE AT 1138
       WITH FOREGROUND-COLOR 6 HIGHLIGHT.
      MOVE CRD-AC  TO DHS-AC.
      MOVE CRD-EXT-CODE  TO DHS-EXT-CODE.
      MOVE CRD-DTE  TO W20-DTE.
            READ DEBHIS WITH IGNORE LOCK
        KEY IS DHS-KEY.
          IF NOT (WS-STAT1 = "0")
        MOVE 1   TO WS-S9
        PERFORM BC990
        PERFORM BC995.
      ADD CRD-QNT  TO DHS-QNT(W20-MONTH).
    IF CRD-QNT < ZERO
        COMPUTE DHS-TCOST(W20-MONTH) = DHS-TCOST(W20-MONTH)
       + ((CRD-COST * CRD-QNT) * -1)
        COMPUTE DHS-TSELL(W20-MONTH) = DHS-TSELL(W20-MONTH)
       + ((CRD-SELL * CRD-QNT) * -1)
    ELSE
        COMPUTE DHS-TCOST(W20-MONTH) = DHS-TCOST(W20-MONTH)
         + (CRD-COST * CRD-QNT)
        COMPUTE DHS-TSELL(W20-MONTH) = DHS-TSELL(W20-MONTH)
         + (CRD-SELL * CRD-QNT).
      PERFORM REWRITE-DEBHIS THRU WRITE-DEBHIS-EXIT.
      GO TO BC20.

       BC25.
      DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR 3.
      CLOSE DEBHIS.
      OPEN I-O DEBHIS.
      GO TO BC999.

       BC990.
      MOVE CRD-EXT-CODE  TO W10-EXT-ITEM.
      PERFORM CA000.
    IF NOT WS-F-ERROR = ZERO
        MOVE "ITEM NO LONGER IN STOCK"
     TO STK-DESC.

       BC995.
      MOVE  ZERO   TO DHS-QNT(WS-S9)
        DHS-TCOST(WS-S9)
        DHS-TSELL(WS-S9).
    IF WS-S9 < 12
        ADD 1   TO WS-S9
        GO TO BC995.
      MOVE W15-SMNTH  TO DHS-START-MNTH.
      MOVE STK-DESC  TO DHS-DESC.
      PERFORM WRITE-DEBHIS THRU WRITE-DEBHIS-EXIT.

       BC999.
      EXIT.
      /
      *  ****  V I E W  ( 1 2  M O N T H   H I S T O R Y )
      *
       BD000         SECTION 53.
       BD00.
      MOVE "C"   TO WS-VIEW.
            PERFORM ERASE-SCREEN.
      MOVE SPACES  TO DHS-AC
        DHS-EXT-CODE.
      PERFORM START-AT-DBHS-AC THRU READ-DEBHIS-EXIT.
    IF WS-F-ERROR = 54
        GO TO BD100.
      PERFORM READ-DEBHIS-NEXT THRU READ-DEBHIS-EXIT.
    IF WS-F-ERROR = 54
        GO TO BD100.
      PERFORM BD80 THRU BD90.
      DISPLAY S10D-12.

       BD05.
            DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " to do Account Lookup"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BD10.
            ACCEPT S10 AT 0308.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO BD15
         WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50D THRU AA59
   WHEN OTHER
       CALL X"E5"
       GO TO BD05
        END-EVALUATE
              DISPLAY S10 AT 0308
               IF W80-ACNO = SPACES
           GO TO BD05.

       BD15.
            PERFORM CLEAR-L50.
          IF W80-ACNO = SPACES
              GO TO BD999.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.

       BD20.
            PERFORM CLEAR-L50.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
              MOVE "No record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BD05.
           IF WS-ERROR = 2
              MOVE "Invalid account"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BD05.
      *
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *
            MOVE DEB-NAME  TO W40-NAME.
           IF W80-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            DISPLAY W40-NAME AT 0315
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        PERFORM CLEAR-L50
        GO TO BD05.

       BD22.
      DISPLAY S11.

       BD25.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             PERFORM CLEAR-L50
       GO TO BD05
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO BD25
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        IF NOT (WS-OPTION = "A" OR "S")
     CALL X"E5"
     GO TO BD25
        END-IF
    ELSE
        CALL X"E5"
        GO TO BD25.
      MOVE 0401   TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
      MOVE SPACES  TO W10-EXT-ITEM.
    IF W10-STCK = "N"
        IF W10-SLNGTH = 3
     MOVE ZERO  TO W10-I3
        ELSE
        IF W10-SLNGTH = 4
     MOVE ZERO  TO W10-I4
        ELSE
        IF W10-SLNGTH = 5
     MOVE ZERO  TO W10-I5
        ELSE
        IF W10-SLNGTH = 6
     MOVE ZERO  TO W10-I6
        ELSE
        IF W10-SLNGTH = 7
     MOVE ZERO  TO W10-I7
        ELSE
        IF W10-SLNGTH = 8
     MOVE ZERO  TO W10-I8
        ELSE
        IF W10-SLNGTH = 9
     MOVE ZERO  TO W10-I9
        ELSE
        IF W10-SLNGTH = 10
     MOVE ZERO  TO W10-I10
        ELSE
        IF W10-SLNGTH = 11
     MOVE ZERO  TO W10-I11
        ELSE
        IF W10-SLNGTH = 12
     MOVE ZERO  TO W10-I12
        ELSE
        IF W10-SLNGTH = 13
     MOVE ZERO  TO W10-I13
        ELSE
     MOVE ZERO  TO W10-I14.
      MOVE WS-OPTION  TO WS-ENQ.
      MOVE W12-MONTH  TO WS-MONTH.
    IF WS-ENQ = "A"
        GO TO BD45.

       BD30.
            PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     ACCEPT S36-EXT AT 0361
        ELSE
        IF WS-USE-ITM = "Y"
     ACCEPT S36-ITM AT 0361
        ELSE
           ACCEPT S36 AT 0361
        END-IF
    ELSE
    IF W10-SLNGTH = 3
        ACCEPT S36-3 AT 0361
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S36-4 AT 0361
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S36-5 AT 0361
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S36-6 AT 0361
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S36-7 AT 0361
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S36-8 AT 0361
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S36-9 AT 0361
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S36-10 AT 0361
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S36-11 AT 0361
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S36-12 AT 0361
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S36-13 AT 0361
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S36-14 AT 0361.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
             PERFORM CLEAR-L50
       GO TO BD05
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "I"  TO WS-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO WS-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO WS-ETYPE
   WHEN F5-KEY
       MOVE "X"  TO WS-ETYPE
   WHEN OTHER
       CALL X"E5"
       GO TO BD30
        END-EVALUATE
        MOVE "S"   TO WS-LOOK
              PERFORM AA50 THRU AA59
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S36-EXT AT 0361
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S36-ITM AT 0361
     ELSE
         DISPLAY S36 AT 0361
     END-IF
        ELSE
     DISPLAY S36 AT 0361.

       BD35.
          IF W10-EXT-ITEM = SPACES
              GO TO BD30.

       BD40.
             PERFORM CA000.
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     DISPLAY S36-EXT AT 0361
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S36-ITM AT 0361
        ELSE
           DISPLAY S36 AT 0361
        END-IF
    ELSE
        DISPLAY S36 AT 0361.
          IF WS-F-ERROR = 22
        MOVE "Z"   TO WS-ETYPE
              PERFORM AA50 THRU AA59
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S36-EXT AT 0361
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S36-ITM AT 0361
     ELSE
         DISPLAY S36 AT 0361
     END-IF
        ELSE
     DISPLAY S36 AT 0361
        END-IF
              IF W10-EXT-ITEM = SPACES
           GO TO BD30
               ELSE
     GO TO BD40.
      DISPLAY "Item Description" AT 0612.
      DISPLAY STK-DESC AT 0629
       WITH BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3 HIGHLIGHT.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BD30.
      GO TO BD55.

       BD45.
      MOVE "I"   TO WS-OPTION.

       BD55.
      PERFORM HISTORY-LOOKUP.
      MOVE SPACES  TO W80-ACNO.
      GO TO BD00.

       BD80.
      MOVE DHS-START-MNTH TO WS-S7.
      MOVE 1   TO WS-S8.
      MOVE SPACES  TO W65-MONTHS.

       BD85.
      MOVE W32-MONTH(WS-S7)
     TO W65-MNTH(WS-S8).
    IF WS-S8 < 12
        ADD 1   TO WS-S7
        WS-S8
        IF WS-S7 > 12
     MOVE 1  TO WS-S7
        END-IF
        GO TO BD85.

       BD90.
      EXIT.

       BD100.
      MOVE "No details have been extracted"
     TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.

       BD999.
      EXIT.
      /
      *    ****    P U R C H A S E   H I S T O R Y   L O O K U P
      *
       HISTORY-LOOKUP   SECTION 53.

       HIST-LUP-SCREEN.
            DISPLAY S-HIST.
      INITIALIZE DHS-REC1.
    IF WS-ENQ = "S"
        MOVE W10-EXT-ITEM TO DHS-EXT-CODE.
      MOVE W80-ACNO  TO DHS-AC.

       HIST-LUP-PERFORM ERASE-SCREEN.
             MOVE 0601           TO CPOS.
            PERFORM HIST-CRT-EDGE UNTIL CLIN = 22.

       HIST-LUP-CONTINUE.
            MOVE 6   TO CLIN.
             MOVE 1              TO W75-S W75-S1.

       HIST-LUP-TAB-LOOP.
      MOVE ZERO   TO W75-QNT(W75-S1, W75-S).
    IF W75-S < 12
        ADD 1   TO W75-S
        GO TO HIST-LUP-TAB-LOOP.
      MOVE SPACES  TO W75-ITEM(W75-S1)
        W75-DESC(W75-S1).
    IF W75-S1 < 16
               ADD 1             TO W75-S1
        MOVE 1   TO W75-S
        GO TO HIST-LUP-TAB-LOOP.
      MOVE ZERO   TO W75-S1.
      MOVE 1   TO W75-S.

       HIST-LUP-START-DBTX.
      MOVE SPACES  TO WS-OPTION.
      PERFORM START-AT-DBHS-AC THRU READ-DEBHIS-EXIT.
          IF WS-F-ERROR = 54
        GO TO HIST-NO-RECORD.

       HIST-GET-DEBHIS-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
        GO TO HIST-SKIP-DBTX-READ.
            PERFORM READ-DEBHIS-NEXT THRU READ-DEBHIS-EXIT.
    IF WS-F-ERROR = 54
               SUBTRACT 1        FROM W75-S
        GO TO HIST-FULL-SCREEN.

       HIST-LUP-DISP-ITEM.
    IF NOT (DHS-AC = DEB-ACNO)
        GO TO HIST-FULL-SCREEN.
    IF (WS-ENQ = "S") AND
       (W10-EXT-ITEM NOT = DHS-EXT-CODE)
        GO TO HIST-FULL-SCREEN.

       HIST-SKIP-DBTX-READ.
      MOVE 1        TO W75-S1.
      MOVE DHS-START-MNTH
          TO W75-S2.

       HIST-STORE-QNT-LOOP.
      MOVE DHS-QNT(W75-S2)
          TO W75-QNT(W75-S, W75-S1).
    IF W75-S1 < 12
        ADD 1        TO W75-S1 W75-S2
        IF W75-S2 > 12
     SUBTRACT 12 FROM W75-S2
        END-IF
        GO TO HIST-STORE-QNT-LOOP.
      MOVE DHS-EXT-CODE TO W75-ITEM(W75-S).
      MOVE DHS-DESC     TO W75-DESC(W75-S).
            DISPLAY DHS-EXT-CODE AT LINE CLIN COLUMN 2
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
    IF WS-VIEW = "P" OR "C"
        DISPLAY W75-DESC(W75-S) AT LINE CLIN COLUMN 21
         WITH FOREGROUND-COLOR 3 HIGHLIGHT.
    IF WS-VIEW = "P"
        MOVE 1        TO W75-S1 W75-S2
        MOVE 51        TO CCOL
        PERFORM HIST-DISPLAY-6-QNT
    ELSE
    IF WS-VIEW = "C"
        MOVE 7        TO W75-S1
        MOVE 1        TO W75-S2
        MOVE 51        TO CCOL
        PERFORM HIST-DISPLAY-6-QNT
    ELSE
        MOVE 1        TO W75-S1
        MOVE 21        TO CCOL
        PERFORM HIST-DISPLAY-12-QNT.
          IF CLIN < 21
               ADD 1             TO CLIN W75-S
              GO TO HIST-GET-DEBHIS-REC.
      
       HIST-FULL-SCREEN.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
001020      DISPLAY " " AT 2303 WITH FOREGROUND-COLOR 14
             " - " WITH FOREGROUND-COLOR 7
       "PgUp" WITH FOREGROUND-COLOR 14
                     "/" WITH FOREGROUND-COLOR 7
       "PgDn" WITH FOREGROUND-COLOR 14
       " - " WITH FOREGROUND-COLOR 7
       "T" WITH FOREGROUND-COLOR 14
       "welve," WITH FOREGROUND-COLOR 7
       "P" WITH FOREGROUND-COLOR 14
       "rev," WITH FOREGROUND-COLOR 7
       "C" WITH FOREGROUND-COLOR 14
       "urrent six months - " WITH FOREGROUND-COLOR 7
             "Esc" WITH FOREGROUND-COLOR 14
       "ape." WITH FOREGROUND-COLOR 7.

       HIST-SAVE-FULL-SCREEN.
      PERFORM SAVE-SCREEN-2.
            MOVE 0   TO W75-S1.
             MOVE 0601           TO CPOS.

       HIST-CRT-ATTRIB.
      MOVE 3920   TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
           W42-ATTRIB2
           W41-LENGTH
     RETURNING WS-STATUS.
            MOVE 4000   TO W41-LENGTH.

       HIST-GET-USER-OPT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
             GO TO HIST-ADJ-NO-KEYS
   WHEN DOWN-KEY
             GO TO HIST-CHK-NO-KEYS
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO HIST-GET-USER-OPT
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO HISTORY-LOOKUP-EXIT
   WHEN PAGE-DOWN
             GO TO HIST-GET-NEXT-PAGE
   WHEN PAGE-UP
             GO TO HIST-GET-PREV-PAGE
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO HIST-GET-USER-OPT
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        GO TO HIST-CHK-VIEW.
      CALL X"E5".
            GO TO HIST-GET-USER-OPT.

       HIST-CHK-VIEW.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "P" OR "C" OR "T")
        GO TO HIST-GET-USER-OPT.
    IF WS-VIEW = WS-OPTION
        GO TO HIST-GET-USER-OPT.
      MOVE WS-OPTION    TO WS-VIEW.
    IF WS-VIEW = "P" OR "C"
        DISPLAY S-HIST
    ELSE
        DISPLAY S-HIST-2.
    IF WS-VIEW ="P"
        DISPLAY W65-FIRST-6 AT 0450 WITH REVERSE-VIDEO.
      PERFORM HIST-LUP-PERFORM ERASE-SCREEN.
      MOVE W75-S        TO WS-S9.
      MOVE 1        TO W75-S.
            MOVE 6        TO CLIN.

       HIST-CHANGE-VIEW.
      DISPLAY W75-ITEM(W75-S) AT LINE CLIN COLUMN 2
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
    IF WS-VIEW = "P" OR "C"
        DISPLAY W75-DESC(W75-S) AT LINE CLIN COLUMN 21
   WITH FOREGROUND-COLOR 3 HIGHLIGHT.
    IF WS-VIEW = "P"
        MOVE 1        TO W75-S1 W75-S2
        MOVE 51        TO CCOL
        PERFORM HIST-DISPLAY-6-QNT
    ELSE
    IF WS-VIEW = "C"
        MOVE 7        TO W75-S1
        MOVE 1        TO W75-S2
        MOVE 51        TO CCOL
        PERFORM HIST-DISPLAY-6-QNT
    ELSE
        MOVE 1        TO W75-S1
        MOVE 21        TO CCOL
        PERFORM HIST-DISPLAY-12-QNT.
    IF W75-S < WS-S9
        ADD 1        TO W75-S
      CLIN
        GO TO HIST-CHANGE-VIEW.
      GO TO HIST-FULL-SCREEN.

       HIST-DISPLAY-6-QNT.
      MOVE W75-QNT(W75-S, W75-S1)
          TO W100-QNT2.
      DISPLAY W100-QNT2 AT CPOS
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
    IF W75-S2 < 6
        ADD 1        TO W75-S1 W75-S2
        ADD 5        TO CCOL
        GO TO HIST-DISPLAY-6-QNT.

       HIST-DISPLAY-12-QNT.
      MOVE W75-QNT(W75-S, W75-S1)
          TO W100-QNT2.
      DISPLAY W100-QNT2 AT CPOS
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
    IF W75-S1 < 12
        ADD 1        TO W75-S1
        ADD 5        TO CCOL
        GO TO HIST-DISPLAY-12-QNT.

       HIST-GET-NEXT-PAGE.
           IF W75-S < 16
              GO TO HIST-GET-USER-OPT.

       HIST-NEW-PAGE.
             MOVE 0601           TO CPOS.
            PERFORM HIST-CRT-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
             MOVE LOW-VALUES     TO W75-KEYS.
             MOVE 1              TO W75-S W75-S1.
      PERFORM HIST-LUP-TAB-LOOP.
            GO TO HIST-GET-DEBHIS-REC.

       HIST-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
       MOVE "J"    TO WS-OPTION.

       HIST-GET-PREV-LOOP.
      PERFORM READ-DEBHIS-PREV THRU READ-DEBHIS-EXIT.
    IF WS-F-ERROR = 43
        MOVE SPACE  TO WS-OPTION
        GO TO HIST-LUP-SCREEN.
    IF NOT (DHS-AC = DEB-ACNO)
        PERFORM READ-DEBHIS-NEXT THRU READ-DEBHIS-EXIT
        GO TO HIST-NEW-PAGE.
    IF WS-IXS1 < 32
        ADD 1   TO WS-IXS1
        GO TO HIST-GET-PREV-LOOP.
      GO TO HIST-NEW-PAGE.

       HIST-NO-RECORD.
      PERFORM CLEAR-L50.
            DISPLAY "No record - Press " AT 5012
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "ANY" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " key"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO HISTORY-LOOKUP-EXIT.

       HIST-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1        FROM W75-S1 CLIN.
            GO TO HIST-HIGHLIGHT.

       HIST-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1            TO W75-S1
              GO TO HIST-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1             TO W75-S1 CLIN.

       HIST-HIGHLIGHT.
            PERFORM HIST-CRT-ATTRIB.
             DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
            GO TO HIST-GET-USER-OPT.

       HIST-CRT-EDGE.
    IF WS-VIEW = "P" OR "C"
              DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
          FOREGROUND-COLOR 3
       "                  ³                             ³
      -       "   ³    ³    ³    ³    ³    "
        WITH FOREGROUND-COLOR 3
       "³" WITH BACKGROUND-COLOR 0
         FOREGROUND-COLOR 3
    ELSE
        DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
          FOREGROUND-COLOR 3
       "                  ³    ³    ³    ³    ³    ³    ³
      -       "   ³    ³    ³    ³    ³    "
        WITH FOREGROUND-COLOR 3
       "³" WITH BACKGROUND-COLOR 0
         FOREGROUND-COLOR 3.
            ADD 1   TO CLIN.

       HISTORY-LOOKUP-EXIT.
             EXIT.
      /
      *  ****  V I E W  G P   F O R   S E L E C T E D  P E R I O D
      *
       BE000         SECTION 54.
       BE00.
          IF (LS0-DBLEV < 3) AND (LS0-STLEV < 9)
        MOVE "Not Authorised"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BE999.

       BE02.
            PERFORM ERASE-SCREEN.
      DISPLAY S10D-G.

       BE05.
            DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " to do Account Lookup"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BE10.
            ACCEPT S10 AT 0308.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO BE15
         WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50D THRU AA59
   WHEN OTHER
       CALL X"E5"
       GO TO BE05
        END-EVALUATE
              DISPLAY S10 AT 0308
               IF W80-ACNO = SPACES
           GO TO BE05.

       BE15.
            PERFORM CLEAR-L50.
          IF W80-ACNO = SPACES
              GO TO BE999.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.

       BE20.
            PERFORM CLEAR-L50.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
              MOVE "No record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BE05.
           IF WS-ERROR = 2
              MOVE "Invalid account"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BE05.
      *
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *
            MOVE DEB-NAME  TO W40-NAME.
           IF W80-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            DISPLAY W40-NAME AT 0315
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        PERFORM CLEAR-L50
        GO TO BE05.

       BE22.
      DISPLAY S11.

       BE25.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             PERFORM CLEAR-L50
       GO TO BE05
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO BE25
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        IF NOT (WS-OPTION = "A" OR "S")
     CALL X"E5"
     GO TO BE25
        END-IF
    ELSE
        CALL X"E5"
        GO TO BE25.
      MOVE 0401   TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
      MOVE SPACES  TO W10-EXT-ITEM.
    IF W10-STCK = "N"
        IF W10-SLNGTH = 3
     MOVE ZERO  TO W10-I3
        ELSE
        IF W10-SLNGTH = 4
     MOVE ZERO  TO W10-I4
        ELSE
        IF W10-SLNGTH = 5
     MOVE ZERO  TO W10-I5
        ELSE
        IF W10-SLNGTH = 6
     MOVE ZERO  TO W10-I6
        ELSE
        IF W10-SLNGTH = 7
     MOVE ZERO  TO W10-I7
        ELSE
        IF W10-SLNGTH = 8
     MOVE ZERO  TO W10-I8
        ELSE
        IF W10-SLNGTH = 9
     MOVE ZERO  TO W10-I9
        ELSE
        IF W10-SLNGTH = 10
     MOVE ZERO  TO W10-I10
        ELSE
        IF W10-SLNGTH = 11
     MOVE ZERO  TO W10-I11
        ELSE
        IF W10-SLNGTH = 12
     MOVE ZERO  TO W10-I12
        ELSE
        IF W10-SLNGTH = 13
     MOVE ZERO  TO W10-I13
        ELSE
     MOVE ZERO  TO W10-I14.
      MOVE WS-OPTION  TO WS-ENQ.
      MOVE W12-MONTH  TO WS-MONTH.
    IF WS-ENQ = "A"
        GO TO BE45.

       BE30.
            PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     ACCEPT S36-EXT AT 0361
        ELSE
        IF WS-USE-ITM = "Y"
     ACCEPT S36-ITM AT 0361
        ELSE
           ACCEPT S36 AT 0361
        END-IF
    ELSE
    IF W10-SLNGTH = 3
        ACCEPT S36-3 AT 0361
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S36-4 AT 0361
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S36-5 AT 0361
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S36-6 AT 0361
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S36-7 AT 0361
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S36-8 AT 0361
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S36-9 AT 0361
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S36-10 AT 0361
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S36-11 AT 0361
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S36-12 AT 0361
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S36-13 AT 0361
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S36-14 AT 0361.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
             PERFORM CLEAR-L50
       GO TO BE05
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "I"  TO WS-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO WS-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO WS-ETYPE
   WHEN F5-KEY
       MOVE "X"  TO WS-ETYPE
   WHEN OTHER
       CALL X"E5"
       GO TO BE30
        END-EVALUATE
        MOVE "S"   TO WS-LOOK
              PERFORM AA50 THRU AA59
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S36-EXT AT 0361
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S36-ITM AT 0361
     ELSE
         DISPLAY S36 AT 0361
     END-IF
        ELSE
     DISPLAY S36 AT 0361.

       BE35.
          IF W10-EXT-ITEM = SPACES
              GO TO BE30.

       BE40.
             PERFORM CA000.
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     DISPLAY S36-EXT AT 0361
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S36-ITM AT 0361
        ELSE
           DISPLAY S36 AT 0361
        END-IF
    ELSE
        DISPLAY S36 AT 0361.
          IF WS-F-ERROR = 22
        MOVE "Z"   TO WS-ETYPE
              PERFORM AA50 THRU AA59
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S36-EXT AT 0361
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S36-ITM AT 0361
     ELSE
         DISPLAY S36 AT 0361
     END-IF
        ELSE
     DISPLAY S36 AT 0361
        END-IF
              IF W10-EXT-ITEM = SPACES
           GO TO BE30
               ELSE
     GO TO BE40.
      DISPLAY "Item Description" AT 0612.
      DISPLAY STK-DESC AT 0629
       WITH BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3 HIGHLIGHT.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BE30.
      GO TO BE55.

       BE45.
      MOVE "I"   TO WS-OPTION.

       BE50.
      MOVE SPACES     TO WS-ERR-MES.
            STRING "'I'tem or 'D'ate sequence  ["
      DELIMITED SIZE
      WS-OPTION DELIMITED SIZE
      "]" DELIMITED SIZE
        INTO WS-ERR-MES.
            PERFORM OPT-MESSAGE.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BE22
        END-EVALUATE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "I" OR "D")
        GO TO BE50.
      MOVE WS-OPTION  TO WS-ENQD.

       BE55.
      PERFORM G-PROFIT-LOOKUP.
      MOVE SPACES  TO W80-ACNO.
      GO TO BE00.

       BE999.
      EXIT.
      /
      *    ****    G R O S S   P R O F I T   L O O K U P
      *
       G-PROFIT-LOOKUP   SECTION 54.

       GP-LUP-SCREEN.
            DISPLAY S-MOV.
      DISPLAY " G.P. % " AT 0472 WITH REVERSE-VIDEO.
      MOVE ZERO   TO W25-TOTAL W25-VALUE.
      INITIALIZE DXT-REC1.
    IF WS-ENQ = "S"
        MOVE W10-EXT-ITEM TO DXT-EXT-CODE.
      MOVE W80-ACNO  TO DXT-AC.
      MOVE W15-SMM  TO DXT-MNTH.
      MOVE W15-SCCYY  TO DXT-YEAR.

       GP-LUP-PERFORM ERASE-SCREEN.
             MOVE 0601           TO CPOS.
            PERFORM GP-CRT-EDGE UNTIL CLIN = 22.

       GP-LUP-CONTINUE.
            MOVE 6   TO CLIN.
            MOVE 1   TO W75-S.

       GP-LUP-TAB-LOOP.
      MOVE ZERO   TO W75-S1.

       GP-LUP-START-DBTX.
      MOVE SPACES  TO WS-OPTION.
    IF (WS-ENQ = "S") OR (WS-ENQD = "I")
        PERFORM START-AT-DBEX-AC THRU READ-DEBTEX-EXIT
    ELSE
              PERFORM START-AT-DBEX-AKEY THRU READ-DEBTEX-EXIT.
          IF WS-F-ERROR = 52
        GO TO GP-NO-RECORD.

       GP-GET-DEBTEX-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
        GO TO GP-SKIP-DBTX-READ.
            PERFORM READ-DEBTEX-NEXT THRU READ-DEBTEX-EXIT.
    IF WS-F-ERROR = 52
               SUBTRACT 1        FROM W75-S
        GO TO GP-FULL-SCREEN.

       GP-LUP-DISP-ITEM.
    IF NOT (DXT-AC = DEB-ACNO)
        GO TO GP-FULL-SCREEN.
    IF (WS-ENQ = "S") AND
       (W10-EXT-ITEM NOT = DXT-EXT-CODE)
        GO TO GP-FULL-SCREEN.

       GP-SKIP-DBTX-READ.
            DISPLAY DXT-EXT-CODE AT LINE CLIN COLUMN 2
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      DISPLAY DXT-DESC AT LINE CLIN COLUMN 21
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      DISPLAY DXT-MTH AT LINE CLIN COLUMN 52
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      MOVE DXT-QNT  TO W100-QNT.
      DISPLAY W100-QNT AT LINE CLIN COLUMN 62
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      COMPUTE W100-4V3 ROUNDED = ((DXT-TSELL - DXT-TCOST)
     / DXT-TSELL) * 100.
      ADD DXT-TCOST  TO W25-VALUE.
      ADD DXT-TSELL  TO W25-TOTAL.
      DISPLAY W100-4V3 AT LINE CLIN COLUMN 71
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
          IF CLIN < 21
               ADD 1             TO CLIN W75-S
              GO TO GP-GET-DEBTEX-REC.
      
       GP-FULL-SCREEN.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
001020      DISPLAY " " AT 2303 WITH FOREGROUND-COLOR 14
             " - " WITH FOREGROUND-COLOR 7
       "PgUp" WITH FOREGROUND-COLOR 14
                     "/" WITH FOREGROUND-COLOR 7
       "PgDn" WITH FOREGROUND-COLOR 14
       " - " WITH FOREGROUND-COLOR 7
             "Esc" WITH FOREGROUND-COLOR 14
       "ape." WITH FOREGROUND-COLOR 7.

       GP-SAVE-FULL-SCREEN.
      PERFORM SAVE-SCREEN-2.
            MOVE 0   TO W75-S1.
             MOVE 0601           TO CPOS.

       GP-CRT-ATTRIB.
      MOVE 3920   TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
           W42-ATTRIB2
           W41-LENGTH
     RETURNING WS-STATUS.
            MOVE 4000   TO W41-LENGTH.

       GP-GET-USER-OPT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
             GO TO GP-ADJ-NO-KEYS
   WHEN DOWN-KEY
             GO TO GP-CHK-NO-KEYS
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO GP-GET-USER-OPT
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO G-PROFIT-LOOKUP-EXIT
   WHEN PAGE-DOWN
             GO TO GP-GET-NEXT-PAGE
   WHEN PAGE-UP
             GO TO GP-GET-PREV-PAGE
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO GP-GET-USER-OPT.
      CALL X"E5".
            GO TO GP-GET-USER-OPT.

       GP-DISP-TOTAL.
            DISPLAY "Total GP for selection        "
        AT LINE CLIN COLUMN 21
        WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      COMPUTE W100-4V3 ROUNDED = ((W25-TOTAL - W25-VALUE)
     / W25-TOTAL) * 100.
      DISPLAY W100-4V3 AT LINE CLIN COLUMN 71
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.

       GP-GET-NEXT-PAGE.
           IF W75-S < 16
              GO TO GP-GET-USER-OPT.

       GP-NEW-PAGE.
             MOVE 0601           TO CPOS.
            PERFORM GP-CRT-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
             MOVE LOW-VALUES     TO W75-KEYS.
             MOVE 1              TO W75-S W75-S1.
      PERFORM GP-LUP-TAB-LOOP.
            GO TO GP-GET-DEBTEX-REC.

       GP-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
       MOVE "J"    TO WS-OPTION.

       GP-GET-PREV-LOOP.
      PERFORM READ-DEBTEX-PREV THRU READ-DEBTEX-EXIT.
    IF WS-F-ERROR = 43
        MOVE SPACE  TO WS-OPTION
        GO TO GP-LUP-SCREEN.
    IF NOT (DXT-AC = DEB-ACNO)
        PERFORM READ-DEBTEX-NEXT THRU READ-DEBTEX-EXIT
        GO TO GP-NEW-PAGE.
    IF WS-IXS1 < 32
        ADD 1   TO WS-IXS1
        GO TO GP-GET-PREV-LOOP.
      GO TO GP-NEW-PAGE.

       GP-NO-RECORD.
      PERFORM CLEAR-L50.
            DISPLAY "No record - Press " AT 5012
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "ANY" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " key"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO G-PROFIT-LOOKUP-EXIT.

       GP-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1        FROM W75-S1 CLIN.
            GO TO GP-HIGHLIGHT.

       GP-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1            TO W75-S1
              GO TO GP-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1             TO W75-S1 CLIN.

       GP-HIGHLIGHT.
            PERFORM GP-CRT-ATTRIB.
             DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
            GO TO GP-GET-USER-OPT.

       GP-CRT-EDGE.
            DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
          FOREGROUND-COLOR 3
       "                  ³                              ³
      -       "         ³        ³         "
        WITH FOREGROUND-COLOR 3
       "³" WITH BACKGROUND-COLOR 0
         FOREGROUND-COLOR 3.
            ADD 1   TO CLIN.

       G-PROFIT-LOOKUP-EXIT.
             EXIT.
      /
      *  ****  V I E W  G P   F O R   1 2   M O N T H S
      *
       BF000         SECTION 55.
       BF00.
          IF (LS0-DBLEV < 3) AND (LS0-STLEV < 9)
        MOVE "Not Authorised"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BF999.

       BF02.
      MOVE "C"   TO WS-VIEW.
            PERFORM ERASE-SCREEN.
      MOVE SPACES  TO DHS-AC
        DHS-EXT-CODE.
      PERFORM START-AT-DBHS-AC THRU READ-DEBHIS-EXIT.
    IF WS-F-ERROR = 54
        GO TO BF100.
      PERFORM READ-DEBHIS-NEXT THRU READ-DEBHIS-EXIT.
    IF WS-F-ERROR = 54
        GO TO BF100.
      PERFORM BF80 THRU BF90.
      DISPLAY S10D-12G.

       BF05.
            DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " to do Account Lookup"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BF10.
            ACCEPT S10 AT 0308.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO BF15
         WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50D THRU AA59
   WHEN OTHER
       CALL X"E5"
       GO TO BF05
        END-EVALUATE
              DISPLAY S10 AT 0308
               IF W80-ACNO = SPACES
           GO TO BF05.

       BF15.
            PERFORM CLEAR-L50.
          IF W80-ACNO = SPACES
              GO TO BF999.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.

       BF20.
            PERFORM CLEAR-L50.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
              MOVE "No record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BF05.
           IF WS-ERROR = 2
              MOVE "Invalid account"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BF05.
      *
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *
            MOVE DEB-NAME  TO W40-NAME.
           IF W80-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            DISPLAY W40-NAME AT 0315
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        PERFORM CLEAR-L50
        GO TO BF05.

       BF22.
      DISPLAY S11.

       BF25.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             PERFORM CLEAR-L50
       GO TO BF05
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO BF25
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        IF NOT (WS-OPTION = "A" OR "S")
     CALL X"E5"
     GO TO BF25
        END-IF
    ELSE
        CALL X"E5"
        GO TO BF25.
      MOVE 0401   TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
      MOVE SPACES  TO W10-EXT-ITEM.
    IF W10-STCK = "N"
        IF W10-SLNGTH = 3
     MOVE ZERO  TO W10-I3
        ELSE
        IF W10-SLNGTH = 4
     MOVE ZERO  TO W10-I4
        ELSE
        IF W10-SLNGTH = 5
     MOVE ZERO  TO W10-I5
        ELSE
        IF W10-SLNGTH = 6
     MOVE ZERO  TO W10-I6
        ELSE
        IF W10-SLNGTH = 7
     MOVE ZERO  TO W10-I7
        ELSE
        IF W10-SLNGTH = 8
     MOVE ZERO  TO W10-I8
        ELSE
        IF W10-SLNGTH = 9
     MOVE ZERO  TO W10-I9
        ELSE
        IF W10-SLNGTH = 10
     MOVE ZERO  TO W10-I10
        ELSE
        IF W10-SLNGTH = 11
     MOVE ZERO  TO W10-I11
        ELSE
        IF W10-SLNGTH = 12
     MOVE ZERO  TO W10-I12
        ELSE
        IF W10-SLNGTH = 13
     MOVE ZERO  TO W10-I13
        ELSE
     MOVE ZERO  TO W10-I14.
      MOVE WS-OPTION  TO WS-ENQ.
      MOVE W12-MONTH  TO WS-MONTH.
    IF WS-ENQ = "A"
        GO TO BF45.

       BF30.
            PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     ACCEPT S36-EXT AT 0361
        ELSE
        IF WS-USE-ITM = "Y"
     ACCEPT S36-ITM AT 0361
        ELSE
           ACCEPT S36 AT 0361
        END-IF
    ELSE
    IF W10-SLNGTH = 3
        ACCEPT S36-3 AT 0361
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S36-4 AT 0361
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S36-5 AT 0361
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S36-6 AT 0361
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S36-7 AT 0361
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S36-8 AT 0361
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S36-9 AT 0361
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S36-10 AT 0361
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S36-11 AT 0361
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S36-12 AT 0361
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S36-13 AT 0361
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S36-14 AT 0361.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-EXT-ITEM
             PERFORM CLEAR-L50
       GO TO BF05
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
       MOVE "I"  TO WS-ETYPE
   WHEN F3-KEY
       MOVE "A"  TO WS-ETYPE
   WHEN F4-KEY
       MOVE "D"  TO WS-ETYPE
   WHEN F5-KEY
       MOVE "X"  TO WS-ETYPE
   WHEN OTHER
       CALL X"E5"
       GO TO BF30
        END-EVALUATE
        MOVE "S"   TO WS-LOOK
              PERFORM AA50 THRU AA59
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S36-EXT AT 0361
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S36-ITM AT 0361
     ELSE
         DISPLAY S36 AT 0361
     END-IF
        ELSE
     DISPLAY S36 AT 0361.

       BF35.
          IF W10-EXT-ITEM = SPACES
              GO TO BF30.

       BF40.
             PERFORM CA000.
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
    IF W10-STCK = "A"
        IF WS-EXT-STK = "Y"
     DISPLAY S36-EXT AT 0361
        ELSE
        IF WS-USE-ITM = "Y"
     DISPLAY S36-ITM AT 0361
        ELSE
           DISPLAY S36 AT 0361
        END-IF
    ELSE
        DISPLAY S36 AT 0361.
          IF WS-F-ERROR = 22
        MOVE "Z"   TO WS-ETYPE
              PERFORM AA50 THRU AA59
      *
      *        *****   A L P H A - N U M E R I C   C O D E   *****
      *
        IF W10-STCK = "A"
     IF WS-EXT-STK = "Y"
         DISPLAY S36-EXT AT 0361
     ELSE
     IF WS-USE-ITM = "Y"
         DISPLAY S36-ITM AT 0361
     ELSE
         DISPLAY S36 AT 0361
     END-IF
        ELSE
     DISPLAY S36 AT 0361
        END-IF
              IF W10-EXT-ITEM = SPACES
           GO TO BF30
               ELSE
     GO TO BF40.
      DISPLAY "Item Description" AT 0612.
      DISPLAY STK-DESC AT 0629
       WITH BACKGROUND-COLOR 5
     FOREGROUND-COLOR 3 HIGHLIGHT.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BF30.
      GO TO BF55.

       BF45.
      MOVE "I"   TO WS-OPTION.

       BF55.
      PERFORM G-PROF12-LOOKUP.
      MOVE SPACES  TO W80-ACNO.
      GO TO BF02.

       BF80.
      MOVE DHS-START-MNTH TO WS-S7.
      MOVE 1   TO WS-S8.

       BF85.
      MOVE W32-MONTH(WS-S7)
     TO W65-MTH(WS-S8).
    IF WS-S8 < 12
        ADD 1   TO WS-S7
        WS-S8
        IF WS-S7 > 12
     MOVE 1  TO WS-S7
        END-IF
        GO TO BF85.

       BF90.
      EXIT.

       BF100.
      MOVE "No details have been extracted"
     TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.

       BF999.
      EXIT.
      /
      *    ****    P U R C H A S E   H I S T O R Y   G . P R O F
      *
       G-PROF12-LOOKUP   SECTION 55.
       GP12-LUP-SCREEN.
            DISPLAY S-GP12.
      INITIALIZE DHS-REC1.
    IF WS-ENQ = "S"
        MOVE W10-EXT-ITEM TO DHS-EXT-CODE.
      MOVE W80-ACNO  TO DHS-AC.

       GP12-LUP-PERFORM ERASE-SCREEN.
             MOVE 0601           TO CPOS.
            PERFORM GP12-CRT-EDGE UNTIL CLIN = 22.

       GP12-LUP-CONTINUE.
            MOVE 6   TO CLIN.
             MOVE 1              TO W75-S W75-S1.

       GP12-LUP-TAB-LOOP.
      MOVE ZERO   TO W75-QNT(W75-S1, W75-S).
    IF W75-S < 12
        ADD 1   TO W75-S
        GO TO GP12-LUP-TAB-LOOP.
      MOVE SPACES  TO W75-ITEM(W75-S1)
        W75-DESC(W75-S1).
    IF W75-S1 < 16
               ADD 1             TO W75-S1
        MOVE 1   TO W75-S
        GO TO GP12-LUP-TAB-LOOP.
      MOVE ZERO   TO W75-S1.
      MOVE 1   TO W75-S.

       GP12-LUP-START-DBTX.
      MOVE SPACES  TO WS-OPTION.
      PERFORM START-AT-DBHS-AC THRU READ-DEBHIS-EXIT.
          IF WS-F-ERROR = 54
        GO TO GP12-NO-RECORD.

       GP12-GET-DEBHIS-REC.
           IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
        GO TO GP12-SKIP-DBTX-READ.
            PERFORM READ-DEBHIS-NEXT THRU READ-DEBHIS-EXIT.
    IF WS-F-ERROR = 54
               SUBTRACT 1        FROM W75-S
        GO TO GP12-FULL-SCREEN.

       GP12-LUP-DISP-ITEM.
    IF NOT (DHS-AC = DEB-ACNO)
        GO TO GP12-FULL-SCREEN.
    IF (WS-ENQ = "S") AND
       (W10-EXT-ITEM NOT = DHS-EXT-CODE)
        GO TO GP12-FULL-SCREEN.

       GP12-SKIP-DBTX-READ.
      MOVE 1        TO W75-S1.
      MOVE DHS-START-MNTH
          TO W75-S2.

       GP12-STORE-GP-LOOP.
      COMPUTE W75-QNT(W75-S, W75-S1) = ((DHS-TSELL(W75-S2)
     - DHS-TCOST(W75-S2)) / DHS-TSELL(W75-S2)) * 100.
    IF W75-S1 < 12
        ADD 1        TO W75-S1 W75-S2
        IF W75-S2 > 12
     SUBTRACT 12 FROM W75-S2
        END-IF
        GO TO GP12-STORE-GP-LOOP.
      MOVE DHS-EXT-CODE TO W75-ITEM(W75-S).
      MOVE DHS-DESC     TO W75-DESC(W75-S).
            DISPLAY DHS-EXT-CODE AT LINE CLIN COLUMN 2
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      DISPLAY DHS-DESC AT LINE CLIN COLUMN 21
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      MOVE 53        TO CCOL.
    IF WS-VIEW = "F"
        MOVE 1        TO W75-S1 W75-S2
    ELSE
    IF WS-VIEW = "C"
        MOVE 9        TO W75-S1
        MOVE 1        TO W75-S2
    ELSE
        MOVE 5        TO W75-S1
        MOVE 1        TO W75-S2.
      PERFORM GP12-DISPLAY-4-GP.
          IF CLIN < 21
               ADD 1             TO CLIN W75-S
              GO TO GP12-GET-DEBHIS-REC.
      
       GP12-FULL-SCREEN.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
001020      DISPLAY "" AT 2303 WITH FOREGROUND-COLOR 14
       X"1B1A" WITH FOREGROUND-COLOR 14
             " - " WITH FOREGROUND-COLOR 7
       "PgUp" WITH FOREGROUND-COLOR 14
                     "/" WITH FOREGROUND-COLOR 7
       "PgDn" WITH FOREGROUND-COLOR 14
       " - " WITH FOREGROUND-COLOR 7
       "F" WITH FOREGROUND-COLOR 14
       "irst," WITH FOREGROUND-COLOR 7
       "M" WITH FOREGROUND-COLOR 14
       "iddle," WITH FOREGROUND-COLOR 7
       "C" WITH FOREGROUND-COLOR 14
       "urrent four months - " WITH FOREGROUND-COLOR 7
             "Esc" WITH FOREGROUND-COLOR 14
       "ape." WITH FOREGROUND-COLOR 7.

       GP12-SAVE-FULL-SCREEN.
      PERFORM SAVE-SCREEN-2.
            MOVE 0   TO W75-S1.
             MOVE 0601           TO CPOS.

       GP12-CRT-ATTRIB.
      MOVE 3920   TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
           W42-ATTRIB2
           W41-LENGTH
     RETURNING WS-STATUS.
            MOVE 4000   TO W41-LENGTH.

       GP12-GET-USER-OPT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
             GO TO GP12-ADJ-NO-KEYS
   WHEN DOWN-KEY
             GO TO GP12-CHK-NO-KEYS
   WHEN LEFT-KEY
       IF WS-VIEW = "F" OR "M"
    MOVE "F"
     TO WS-OPTION
       ELSE
    MOVE "M"
     TO WS-OPTION
       END-IF
       GO TO GP12-CHK-VIEW
   WHEN RIGHT-KEY
       IF WS-VIEW = "C" OR "M"
    MOVE "C"
     TO WS-OPTION
       ELSE
    MOVE "M"
     TO WS-OPTION
       END-IF
       GO TO GP12-CHK-VIEW
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO GP12-GET-USER-OPT
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO G-PROF12-LOOKUP-EXIT
   WHEN PAGE-DOWN
             GO TO GP12-GET-NEXT-PAGE
   WHEN PAGE-UP
             GO TO GP12-GET-PREV-PAGE
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO GP12-GET-USER-OPT
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        GO TO GP12-CHK-VIEW.
      CALL X"E5".
            GO TO GP12-GET-USER-OPT.

       GP12-CHK-VIEW.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "F" OR "M" OR "C")
        GO TO GP12-GET-USER-OPT.
    IF WS-VIEW = WS-OPTION
        GO TO GP12-GET-USER-OPT.
      MOVE WS-OPTION  TO WS-VIEW.
      DISPLAY S-GP12.
    IF WS-VIEW ="F"
        DISPLAY W65-FIRST-4 AT 0452 WITH REVERSE-VIDEO
    ELSE
    IF WS-VIEW ="M"
        DISPLAY W65-MID-4 AT 0452 WITH REVERSE-VIDEO.
      PERFORM GP12-LUP-PERFORM ERASE-SCREEN.
      MOVE W75-S   TO WS-S9.
      MOVE 1   TO W75-S.
            MOVE 6   TO CLIN.

       GP12-CHANGE-VIEW.
      DISPLAY W75-ITEM(W75-S) AT LINE CLIN COLUMN 2
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      DISPLAY W75-DESC(W75-S) AT LINE CLIN COLUMN 21
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
      MOVE 53   TO CCOL.
    IF WS-VIEW = "F"
        MOVE 1   TO W75-S1 W75-S2
    ELSE
    IF WS-VIEW = "C"
        MOVE 9   TO W75-S1
        MOVE 1   TO W75-S2
    ELSE
        MOVE 5   TO W75-S1
        MOVE 1   TO W75-S2.
      PERFORM GP12-DISPLAY-4-GP.
    IF W75-S < WS-S9
        ADD 1   TO W75-S
        CLIN
        GO TO GP12-CHANGE-VIEW.
      GO TO GP12-FULL-SCREEN.

       GP12-DISPLAY-4-GP.
      MOVE W75-QNT(W75-S, W75-S1)
     TO W100-2V3.
      DISPLAY W100-2V3 AT CPOS
       WITH FOREGROUND-COLOR 3 HIGHLIGHT.
    IF W75-S2 < 4
        ADD 1   TO W75-S1 W75-S2
        ADD 7   TO CCOL
        GO TO GP12-DISPLAY-4-GP.

       GP12-GET-NEXT-PAGE.
           IF W75-S < 16
              GO TO GP12-GET-USER-OPT.

       GP12-NEW-PAGE.
             MOVE 0601           TO CPOS.
            PERFORM GP12-CRT-EDGE UNTIL CLIN = 22.
             MOVE 6              TO CLIN.
             MOVE LOW-VALUES     TO W75-KEYS.
             MOVE 1              TO W75-S W75-S1.
      PERFORM GP12-LUP-TAB-LOOP.
            GO TO GP12-GET-DEBHIS-REC.

       GP12-GET-PREV-PAGE.
             MOVE 1              TO WS-IXS1.
      *
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *
       MOVE "J"    TO WS-OPTION.

       GP12-GET-PREV-LOOP.
      PERFORM READ-DEBHIS-PREV THRU READ-DEBHIS-EXIT.
    IF WS-F-ERROR = 43
        MOVE SPACE  TO WS-OPTION
        GO TO GP12-LUP-SCREEN.
    IF NOT (DHS-AC = DEB-ACNO)
        PERFORM READ-DEBHIS-NEXT THRU READ-DEBHIS-EXIT
        GO TO GP12-NEW-PAGE.
    IF WS-IXS1 < 32
        ADD 1   TO WS-IXS1
        GO TO GP12-GET-PREV-LOOP.
      GO TO GP12-NEW-PAGE.

       GP12-NO-RECORD.
      PERFORM CLEAR-L50.
            DISPLAY "No record - Press " AT 5012
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "ANY" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " key"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO G-PROF12-LOOKUP-EXIT.

       GP12-ADJ-NO-KEYS.
           IF W75-S1 > 1
               SUBTRACT 1        FROM W75-S1 CLIN.
            GO TO GP12-HIGHLIGHT.

       GP12-CHK-NO-KEYS.
           IF W75-S1 = ZERO
               MOVE 1            TO W75-S1
              GO TO GP12-HIGHLIGHT.
           IF W75-S1 < W75-S
               ADD 1             TO W75-S1 CLIN.

       GP12-HIGHLIGHT.
            PERFORM GP12-CRT-ATTRIB.
             DISPLAY W43-DET (CLIN) AT CPOS WITH REVERSE-VIDEO.
            GO TO GP12-GET-USER-OPT.

       GP12-CRT-EDGE.
            DISPLAY "³" AT CPOS WITH BACKGROUND-COLOR 0
          FOREGROUND-COLOR 3
       "                  ³
      -       "³      ³      ³      ³      "
        WITH FOREGROUND-COLOR 3
       "³" WITH BACKGROUND-COLOR 0
         FOREGROUND-COLOR 3.
            ADD 1   TO CLIN.

       G-PROF12-LOOKUP-EXIT.
             EXIT.
      /
      *  ****  E X P O R T   F O R   S E L E C T E D   P E R I O D
      *
       BG000         SECTION 56.
       BG00.
            PERFORM ERASE-SCREEN.
      MOVE 0   TO WS-ERROR
        WS-ENQEND.
          IF (LS0-DBLEV < 3) AND (LS0-STLEV < 9)
        MOVE "Not Authorised"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BG995.
      DISPLAY S10D-E1.

       BG05.
      MOVE "'A'll or 'S'elective per Account  [A]"
     TO WS-ERR-MES.
            MOVE "A"   TO WS-OPTION.
      *    *************************************************************
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E
      *            A C C E P T E D.
      *    *************************************************************
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "A" OR "S")
              GO TO BG05.
      MOVE WS-OPTION  TO WS-ENQD.
    IF WS-ENQD = "A"
        PERFORM BG70
        GO TO BG30.
      MOVE "A"   TO WS-ENQ.
      DISPLAY S10D-E2.

       BG10.
            DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " to do Account Lookup"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BG15.
            ACCEPT S10 AT 0308.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO BG20
         WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50D THRU AA59
   WHEN OTHER
       CALL X"E5"
       GO TO BG10
        END-EVALUATE
              DISPLAY S10 AT 0308
               IF W80-ACNO = SPACES
           GO TO BG10.

       BG20.
            PERFORM CLEAR-L50.
          IF W80-ACNO = SPACES
              GO TO BG995.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.

       BG25.
            PERFORM CLEAR-L50.
            PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
              MOVE "No record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BG10.
           IF WS-ERROR = 2
              MOVE "Invalid account"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BG10.
      *    *************************************************************
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *    *************************************************************
            MOVE DEB-NAME  TO W40-NAME.
           IF W80-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            DISPLAY W40-NAME AT 0315
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.

       BG25-CHECK.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        PERFORM CLEAR-L50
        GO TO BG10.

       BG30.
      PERFORM SAVE-SCREEN-3.
      MOVE 5   TO SHADE-ROW.
      MOVE 27   TO SHADE-COL.
      MOVE 30   TO SHADE-WIDTH.
      MOVE 5   TO SHADE-LINES.
      DISPLAY S05D.
      PERFORM SCREEN-SHADOW.
      MOVE W12-MONTH  TO W30-END W30-CURR.
    IF W12-MONTH = 12
        MOVE 1   TO W30-START
    ELSE
        ADD 1 W12-MONTH  GIVING W30-START.

       BG35.
      DISPLAY "Press " AT 5052
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
       "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
    BACKGROUND-COLOR 3
       "ape to Exit"
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
      ACCEPT S05A.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BG995
   WHEN OTHER  CALL X"E5"
       GO TO BG35
        END-EVALUATE.
    IF (W30-START < 01 OR > 12) OR
       (W30-END < 01 OR > 12)
        MOVE "Months must be 01 to 12"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BG35.
      DISPLAY W30-MONTH(W30-START) AT 0747
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      DISPLAY W30-MONTH(W30-END) AT 0847
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BG35.
      PERFORM CLEAR-L50.
      MOVE W12-TODAY  TO W15-START W15-END.
      MOVE W30-START  TO W15-SMNTH.
      MOVE W30-END  TO W15-EMNTH.
      MOVE 01   TO W15-SDAY.
      MOVE 31   TO W15-EDAY.
      *    *************************************************************
      *    ****   I S S T A R T   M O N T H -   PREVIOUS YEAR
      *    *************************************************************
    IF W30-START > W30-CURR
        SUBTRACT 1  FROM W15-SCCYY
        IF W30-END > W30-CURR
     SUBTRACT 1  FROM W15-ECCYY
        END-IF
    ELSE
    IF W30-END < W30-START
        SUBTRACT 1  FROM W15-SCCYY.
    IF W15-EMNTH = 04 OR 06 OR 09 OR 11
        MOVE 30   TO W15-EDAY
    ELSE
    IF W15-EMNTH = 02
      *    *************************************************************
      *    ****    C H E C K   F O R   L E A P  Y E A R
      *    *************************************************************
              DIVIDE W15-ECCYY BY 4 GIVING W25-RESULT
              IF W25-DECIMAL = 0
     MOVE 29  TO W15-EDAY
        ELSE
     MOVE 28  TO W15-EDAY.
      *    *************************************************************
      *    ****    SWITCH START DATE TO YEAR/MONTH/DAY
      *    *************************************************************
      MOVE W15-SDAY  TO W15-SDD.
      MOVE W15-SMNTH  TO W15-SMM.
      MOVE W15-SCENT  TO W15-SCC.
      MOVE W15-SYEAR  TO W15-SYY.
      *    *************************************************************
      *    ****    SWITCH END DATE TO YEAR/MONTH/DAY
      *    *************************************************************
      MOVE W15-EDAY  TO W15-EDD.
      MOVE W15-EMNTH  TO W15-EMM.
      MOVE W15-ECENT  TO W15-ECC.
      MOVE W15-EYEAR  TO W15-EYY.
      PERFORM RESTORE-SCREEN-3.
      CLOSE DEBTEX.
      INSPECT W02-DEBTEX REPLACING FIRST "DBEX"
     BY "DBXP".
      OPEN OUTPUT DEBTEX.
      CLOSE DEBTEX.
      OPEN I-O DEBTEX.
      *    *************************************************************
      *    ****    E X T R A C T   F O R   S E L E C T E D
      *     A C C O U N T ?
      *    *************************************************************
    IF WS-ENQD = "S"
        MOVE W80-ACNO  TO DEB-ACNO
    ELSE
      *    *************************************************************
      *    ****    G E T   T H E   F I R S T   D E B T O R
      *     A C C O U N T
      *    *************************************************************
        MOVE " !"  TO DEB-ACNO.
      PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT.
          IF WS-F-ERROR = 6
        GO TO BG992.
      DISPLAY "Debtor account:" AT 0912.

       BG40.
      DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR 3.
      DISPLAY "Extracting:" AT 1112.
      PERFORM PRINT-ABORT.
    IF WS-OPTION = "Y"
        GO TO BG50.
            PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
          IF WS-F-ERROR = 6
              GO TO BG50.
    IF WS-ENQ = "S"
        IF NOT (DEB-SMAN = WS-SMAN)
     GO TO BG40.
    IF WS-ENQD = "S"
        IF NOT (DEB-ACNO = W80-ACNO)
     GO TO BG992.
      DISPLAY DEB-ACNO AT 0928
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      MOVE DEB-ACNO  TO CRD-AC.
      MOVE W15-S-YMD  TO CRD-DTE.
      PERFORM START-AT-CRDX-ACNT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        GO TO BG40.

       BG45.
            PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        GO TO BG40.
    IF NOT (CRD-AC = DEB-ACNO)
        IF WS-ENQD = "S"
     GO TO BG50
        ELSE
     GO TO BG40.
    IF CRD-DTE > W15-E-YMD
        GO TO BG40.
    IF CRD-QNT = ZERO
        GO TO BG45.
      ADD 1   TO WS-ENQEND.
      DISPLAY CRD-DTE AT 1128
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
       CRD-EXT-CODE AT 1138
       WITH FOREGROUND-COLOR 6 HIGHLIGHT.
      MOVE CRD-AC  TO DXT-AC.
      MOVE CRD-EXT-CODE  TO DXT-EXT-CODE.
      MOVE CRD-DTE  TO W20-DTE.
      MOVE W20-MONTH  TO DXT-MNTH.
      MOVE W20-CNYR  TO DXT-YEAR.
            READ DEBTEX WITH IGNORE LOCK
        KEY IS DXT-AKEY.
          IF NOT (WS-STAT1 = "0")
        PERFORM BG990
        MOVE CRD-QNT  TO DXT-QNT
        IF (CRD-QNT < ZERO)
     COMPUTE DXT-TCOST = (CRD-COST * CRD-QNT) * -1
     COMPUTE DXT-TSELL = (CRD-SELL * CRD-QNT) * -1
        ELSE
     MULTIPLY CRD-COST BY CRD-QNT
         GIVING DXT-TCOST
     MULTIPLY CRD-SELL BY CRD-QNT
         GIVING DXT-TSELL
        END-IF
        MOVE W30-MONTH(W20-MONTH)
     TO DXT-MTH
        MOVE W15-EMM  TO DXT-EMTH
        MOVE STK-DESC  TO DXT-DESC
        PERFORM WRITE-DEBTEX THRU WRITE-DEBTEX-EXIT
    ELSE
        ADD CRD-QNT  TO DXT-QNT
        IF (CRD-QNT < ZERO)
     COMPUTE DXT-TCOST = DXT-TCOST +
           ((CRD-COST * CRD-QNT) * -1)
     COMPUTE DXT-TSELL = DXT-TSELL +
           ((CRD-SELL * CRD-QNT) * -1)
        ELSE
     COMPUTE DXT-TCOST = DXT-TCOST + (CRD-COST * CRD-QNT)
     COMPUTE DXT-TSELL = DXT-TSELL + (CRD-SELL * CRD-QNT)
        END-IF
        PERFORM REWRITE-DEBTEX THRU WRITE-DEBTEX-EXIT.
      GO TO BG45.

       BG50.
      DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR 3.
      CLOSE DEBTEX.
      OPEN I-O DEBTEX.
    IF WS-ENQEND = ZERO
        GO TO BG992.
      MOVE 0401   TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
      DISPLAY "Generating Export File" AT 0502
        WITH FOREGROUND-COLOR 7 HIGHLIGHT.
      MOVE "N"   TO WS-OPTION.
      *    *************************************************************
      *    ****   CURRENT PRICES
      *    *************************************************************
       BG52.
      MOVE "Include Current Selling Prices (Y/N)  [ ]"
     TO WS-ERR-MES.
      PERFORM OPT-MESSAGE.
    IF NOT (WS-OPTION = "Y" OR "N")
        GO TO BG52.
      MOVE WS-OPTION  TO WS-C-PRICE.
      MOVE ","   TO W50-EXP.
      MOVE 1   TO W60-S.
      *    *************************************************************
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *    *************************************************************
             MOVE SPACES         TO W55-EXP-DATA.
             MOVE 1              TO W55-S1.
      PERFORM AA100.
      *    *************************************************************
      *    ****   S T A R T   R E A D I N G   E X T R A C T E D
      *    D A T A   & C R E A T E   E X P O R T   F I L E
      *    *************************************************************
      MOVE SPACES  TO DXT-AC
        DXT-EXT-CODE.
      MOVE ZERO   TO DXT-YEAR
        DXT-MNTH.
      MOVE SPACES  TO W80-ACNO.
      PERFORM START-AT-DBEX-AKEY THRU READ-DEBTEX-EXIT.
    IF WS-F-ERROR = 52
        GO TO BG992.

       BG55.
     PERFORM READ-DEBTEX-NEXT THRU READ-DEBTEX-EXIT.
    IF WS-F-ERROR = 52
        GO TO BG65.
    IF NOT (DXT-AC = W80-ACNO)
        PERFORM BG60.
      MOVE 1   TO W60-S.
      *    *************************************************************
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *    *************************************************************
             MOVE SPACES         TO W55-EXP-DATA.
             MOVE 1              TO W55-S1.
      *    *************************************************************
      *     W50-DATA   : CONTAINS DATA TO BE INSERTED
      *            W50-LENGTH : LENGTH OF DATA
      *    *************************************************************
      MOVE DXT-EXT-CODE  TO W50-DATA.
      MOVE 18   TO W50-LENGTH.
      PERFORM AA200.
      MOVE DXT-DESC  TO W50-DATA.
      MOVE 30   TO W50-LENGTH.
      PERFORM AA200.
      MOVE DXT-MTH  TO W50-DATA.
      MOVE 9   TO W50-LENGTH.
      PERFORM AA200.
      MOVE DXT-YEAR  TO W100-N4.
      MOVE W100-4  TO W50-DATA.
      MOVE 4   TO W50-LENGTH.
      PERFORM AA200.
      MOVE DXT-QNT  TO W100-V1.
      MOVE W100-S9V2  TO W50-DATA.
      MOVE 13   TO W50-LENGTH.
      PERFORM AA200.
      COMPUTE W25-COST = (DXT-TSELL / DXT-QNT).
      MOVE W25-COST  TO W100-V2.
      MOVE W100-S9V3  TO W50-DATA.
      MOVE 13   TO W50-LENGTH.
      PERFORM AA200.
      MOVE DXT-TSELL  TO W100-V1.
      MOVE W100-S9V2  TO W50-DATA.
      MOVE 13   TO W50-LENGTH.
      PERFORM AA200.
      MOVE W55-EXP-DATA  TO EXP-REC1.
      PERFORM WRITE-DEBDET THRU WRITE-DEBDET-EXIT.
      GO TO BG55.
      *    *************************************************************
      *    ****    A C C O U N T   H E A D E R
      *    *************************************************************
       BG60.
      INITIALIZE W60-FIELDS.
      *    *************************************************************
      *    ****    ACCOUNT NUMBER & NAME
      *    *************************************************************
      MOVE 01   TO W60-FNO (1).
      MOVE 02   TO W60-FNO (2).
      PERFORM AA600.
      MOVE DXT-AC  TO W80-ACNO.
             PERFORM CA155-GET-DEBTOR.
      MOVE 1   TO W60-S.
      *    *************************************************************
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *    *************************************************************
             MOVE SPACES         TO W55-EXP-DATA.
             MOVE 1              TO W55-S1.
      MOVE DEB-ACNO  TO W50-DATA.
      MOVE 6   TO W50-LENGTH.
      PERFORM AA200.
      MOVE DEB-NAME  TO W60-NAME.
            INSPECT W60-NAME REPLACING ALL "*" BY " ".
            INSPECT W60-NAME REPLACING FIRST "!" BY " ".
      MOVE W60-NAME  TO W50-DATA.
      MOVE 40   TO W50-LENGTH.
      PERFORM AA200.
      MOVE W55-EXP-DATA  TO EXP-REC1.
      PERFORM WRITE-DEBDET THRU WRITE-DEBDET-EXIT.
      INITIALIZE W60-FIELDS.
      *    *************************************************************
      *    ****    ITEM CODE, DECRIPTION, MONTH, YEAR, QUANTITY
      *     PRICE & EXTENDED PRICE
      *    *************************************************************
      MOVE 03   TO W60-FNO (1).
      MOVE 22   TO W60-FNO (2).
      MOVE 23   TO W60-FNO (3).
      MOVE 24   TO W60-FNO (4).
      MOVE 25   TO W60-FNO (5).
      MOVE 26   TO W60-FNO (6).
      MOVE 27   TO W60-FNO (7).
      PERFORM AA600.

       BG65.
      MOVE "Export file created - Press any key"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
      CLOSE DEBTEX.
      INSPECT W02-DEBTEX REPLACING FIRST "DBXP"
     BY "DBEX".
      OPEN I-O DEBTEX.
      CLOSE DEBDET.
      GO TO BG999.

       BG70.
      MOVE "'A'll accounts or per 'S'alesperson [A]"
     TO WS-ERR-MES.
      MOVE "A"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "A" OR "S")
        GO TO BG70.
      MOVE WS-OPTION  TO WS-ENQ.
    IF WS-OPTION = "S"
        PERFORM SAVE-SCREEN-3
        PERFORM BG75 THRU BG85
        PERFORM RESTORE-SCREEN-3
        IF WS-SMAN = ZERO
     GO TO BG70.

       BG75.
      MOVE 10   TO SHADE-ROW.
      MOVE 24   TO SHADE-COL.
      MOVE 35   TO SHADE-WIDTH.
      MOVE 5   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0922
        WITH FOREGROUND-COLOR 5 HIGHLIGHT
      BACKGROUND-COLOR 5
       "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "³" AT 1022 WITH FOREGROUND-COLOR 5 HIGHLIGHT
      BACKGROUND-COLOR 5
       "S E L E C T   S A L E S P E R S O N"
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 5
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "³ " AT 1122 WITH FOREGROUND-COLOR 5 HIGHLIGHT
      BACKGROUND-COLOR 5
       "No Name                           "
        WITH FOREGROUND-COLOR 7 BACKGROUND-COLOR 5
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "³                                   " AT 1222
        WITH FOREGROUND-COLOR 5 HIGHLIGHT
      BACKGROUND-COLOR 5
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "À" AT 1322 WITH FOREGROUND-COLOR 5 HIGHLIGHT
          BACKGROUND-COLOR 5
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      MOVE ZERO   TO WS-SMAN.

       BG80.
      ACCEPT S41.
    IF WS-SMAN > 50
        GO TO BG80.
    IF WS-SMAN = ZERO
        GO TO BG85.
            ADD 100 WS-SMAN  GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-SALESMAN = SPACES
              GO TO BG80.
            DISPLAY PAR-SALESMAN AT 1227
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      MOVE PAR-SALESMAN  TO W60-SMAN.

       BG85.
      MOVE 15   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
          IF WS-OPTION = "N"
        GO TO BG80.

       BG990.
      MOVE CRD-EXT-CODE  TO W10-EXT-ITEM.
      PERFORM CA000.
    IF NOT (WS-F-ERROR = ZERO)
        MOVE "ITEM NO LONGER IN STOCK"
     TO STK-DESC.

       BG992.
      MOVE "No Details"  TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.

       BG995.
      MOVE 9   TO WS-ERROR.

       BG999.
      EXIT.
      /    *************************************************************
      *    ****   E X P O R T L A S T   1 2 M O N T H S
      *    *************************************************************
       BH000         SECTION 57.
       BH00.
             PERFORM ERASE-SCREEN.
      MOVE 0   TO WS-ERROR
        WS-ENQEND.
          IF (LS0-DBLEV < 3) AND (LS0-STLEV < 9)
        MOVE "Not Authorised"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BH999.
      DISPLAY S10D-E3.

       BH05.
      MOVE "'A'll or 'S'elective per Account  [A]"
     TO WS-ERR-MES.
            MOVE "A"   TO WS-OPTION.
      *    *************************************************************
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E
      *            A C C E P T E D.
      *    *************************************************************
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "A" OR "S")
              GO TO BH05.
      MOVE WS-OPTION  TO WS-ENQD.
    IF WS-ENQD = "A"
        PERFORM BH70
        GO TO BH30.
      MOVE "A"   TO WS-ENQ.
      DISPLAY S10D-E4.

       BH10.
            DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " to do Account Lookup"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BH15.
            ACCEPT S10 AT 0308.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO BH20
         WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50D THRU AA59
   WHEN OTHER
       CALL X"E5"
       GO TO BH10
        END-EVALUATE
              DISPLAY S10 AT 0308
               IF W80-ACNO = SPACES
           GO TO BH10.

       BH20.
            PERFORM CLEAR-L50.
          IF W80-ACNO = SPACES
              GO TO BH999.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.

       BH25.
            PERFORM CLEAR-L50.
            PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
              MOVE "No record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BH10.
           IF WS-ERROR = 2
              MOVE "Invalid account"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BH10.
      *    *************************************************************
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *    *************************************************************
            MOVE DEB-NAME  TO W40-NAME.
           IF W80-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            DISPLAY W40-NAME AT 0315
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.

       BH25-CHECK.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        PERFORM CLEAR-L50
        GO TO BH10.

       BH30.
      MOVE W12-TODAY  TO W15-START W15-END.
      MOVE 01   TO W15-SDAY.
      MOVE 31   TO W15-EDAY.
    IF W15-SMNTH = 12
        MOVE 1   TO W15-SMNTH
    ELSE
        ADD 1   TO W15-SMNTH
        SUBTRACT 1  FROM W15-SCCYY.
    IF W15-EMNTH = 04 OR 06 OR 09 OR 11
        MOVE 30   TO W15-EDAY
    ELSE
    IF W15-EMNTH = 02
      *    *************************************************************
      *    ****    C H E C K   F O R   L E A P  Y E A R
      *    *************************************************************
              DIVIDE W15-ECCYY BY 4 GIVING W25-RESULT
              IF W25-DECIMAL = 0
     MOVE 29  TO W15-EDAY
        ELSE
     MOVE 28  TO W15-EDAY.
      *    *************************************************************
      *    ****    SWITCH START DATE TO YEAR/MONTH/DAY
      *    *************************************************************
      MOVE W15-SDAY  TO W15-SDD.
      MOVE W15-SMNTH  TO W15-SMM.
      MOVE W15-SCENT  TO W15-SCC.
      MOVE W15-SYEAR  TO W15-SYY.
      *    *************************************************************
      *    ****    SWITCH END DATE TO YEAR/MONTH/DAY
      *    *************************************************************
      MOVE W15-EDAY  TO W15-EDD.
      MOVE W15-EMNTH  TO W15-EMM.
      MOVE W15-ECENT  TO W15-ECC.
      MOVE W15-EYEAR  TO W15-EYY.
      CLOSE DEBHIS.
      INSPECT W02-DEBTEX REPLACING FIRST "AR_HIST"
     BY "AR-DBXP".
      OPEN OUTPUT DEBHIS.
      CLOSE DEBHIS.
      OPEN I-O DEBHIS.
      *    *************************************************************
      *    ****    E X T R A C T   F O R   S E L E C T E D
      *     A C C O U N T ?
      *    *************************************************************
    IF WS-ENQD = "S"
        MOVE W80-ACNO  TO DEB-ACNO
    ELSE
      *    *************************************************************
      *    ****    G E T   T H E   F I R S T   D E B T O R
      *     A C C O U N T
      *    *************************************************************
        MOVE " !"  TO DEB-ACNO.
      PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT.
          IF WS-F-ERROR = 6
        GO TO BH995.
      DISPLAY "Debtor account:" AT 0912.

       BH40.
      DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR 3.
      DISPLAY "Extracting:" AT 1112.
      PERFORM PRINT-ABORT.
    IF WS-OPTION = "Y"
        GO TO BH50.
            PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
          IF WS-F-ERROR = 6
              GO TO BH50.
    IF WS-ENQ = "S"
        IF NOT (DEB-SMAN = WS-SMAN)
     GO TO BH40.
    IF WS-ENQD = "S"
        IF NOT (DEB-ACNO = W80-ACNO)
     GO TO BH995.
      DISPLAY DEB-ACNO AT 0928
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      MOVE DEB-ACNO  TO CRD-AC.
      MOVE W15-S-YMD  TO CRD-DTE.
      PERFORM START-AT-CRDX-ACNT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        GO TO BH40.

       BH45.
            PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        GO TO BH40.
    IF NOT (CRD-AC = DEB-ACNO)
        IF WS-ENQD = "S"
     GO TO BH50
        ELSE
     GO TO BH40.
    IF CRD-DTE > W15-E-YMD
        GO TO BH40.
    IF CRD-QNT = ZERO
        GO TO BH45.
      ADD 1   TO WS-ENQEND.
      DISPLAY CRD-DTE AT 1128
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
       CRD-EXT-CODE AT 1138
       WITH FOREGROUND-COLOR 6 HIGHLIGHT.
      MOVE CRD-AC  TO DHS-AC.
      MOVE CRD-EXT-CODE  TO DHS-EXT-CODE.
      MOVE CRD-DTE  TO W20-DTE.
            READ DEBHIS WITH IGNORE LOCK
         KEY IS DHS-KEY.
          IF NOT (WS-STAT1 = "0")
        MOVE 1   TO WS-S9
        PERFORM BH990
        PERFORM BH992.
      ADD CRD-QNT  TO DHS-QNT(W20-MONTH).
    IF CRD-QNT < ZERO
        COMPUTE DHS-TCOST(W20-MONTH) = DHS-TCOST(W20-MONTH)
       + ((CRD-COST * CRD-QNT) * -1)
        COMPUTE DHS-TSELL(W20-MONTH) = DHS-TSELL(W20-MONTH)
       + ((CRD-SELL * CRD-QNT) * -1)
    ELSE
        COMPUTE DHS-TCOST(W20-MONTH) = DHS-TCOST(W20-MONTH)
         + (CRD-COST * CRD-QNT)
        COMPUTE DHS-TSELL(W20-MONTH) = DHS-TSELL(W20-MONTH)
         + (CRD-SELL * CRD-QNT).
      PERFORM REWRITE-DEBHIS THRU WRITE-DEBHIS-EXIT.
      GO TO BH45.

       BH50.
      DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR 3.
      CLOSE DEBHIS.
      OPEN I-O DEBHIS.
    IF WS-ENQEND = ZERO
        GO TO BG992.
      MOVE 0401   TO CPOS.
             PERFORM ERASE-SCREEN-LOOP UNTIL CLIN > 23.
      DISPLAY "Generating Export File" AT 0502
        WITH FOREGROUND-COLOR 7 HIGHLIGHT.
      MOVE ","   TO W50-EXP.
      MOVE 1   TO W60-S.
      *    *************************************************************
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *    *************************************************************
             MOVE SPACES         TO W55-EXP-DATA.
             MOVE 1              TO W55-S1.
      PERFORM AA100.
      *    *************************************************************
      *    ****   S T A R T   R E A D I N G   E X T R A C T E D
      *    D A T A   & C R E A T E   E X P O R T   F I L E
      *    *************************************************************
      MOVE SPACES  TO DHS-AC
        DHS-EXT-CODE.
      MOVE SPACES  TO W80-ACNO.
      PERFORM START-AT-DBHS-AC THRU READ-DEBHIS-EXIT.
    IF WS-F-ERROR = 54
        GO TO BH992.

       BH55.
     PERFORM READ-DEBHIS-NEXT THRU READ-DEBHIS-EXIT.
    IF WS-F-ERROR = 54
        GO TO BH65.
    IF NOT (DHS-AC = W80-ACNO)
        PERFORM BH60.
      MOVE 1   TO W60-S.
      *    *************************************************************
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *    *************************************************************
             MOVE SPACES         TO W55-EXP-DATA.
             MOVE 1              TO W55-S1.
      *    *************************************************************
      *     W50-DATA   : CONTAINS DATA TO BE INSERTED
      *            W50-LENGTH : LENGTH OF DATA
      *    *************************************************************
      MOVE DHS-AC  TO W80-ACNO.
             PERFORM CA155-GET-DEBTOR.
      MOVE 1   TO W60-S.
      *    *************************************************************
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *    *************************************************************
             MOVE SPACES         TO W55-EXP-DATA.
             MOVE 1              TO W55-S1.
      MOVE DHS-EXT-CODE  TO W50-DATA.
      MOVE 18   TO W50-LENGTH.
      PERFORM AA200.
      MOVE DHS-DESC  TO W50-DATA.
      MOVE 30   TO W50-LENGTH.
      PERFORM AA200.
      MOVE 1   TO W65-S.

      MOVE DHS-START-MNTH TO WS-S7.
      MOVE 1   TO WS-S8.

       BH56.
      MOVE DHS-QNT(WS-S7) TO W100-V1.
      MOVE W100-S9V2  TO W50-DATA.
      MOVE 13   TO W50-LENGTH.
      PERFORM AA200.
      COMPUTE W25-COST = (DHS-TSELL(WS-S7) / DHS-QNT(WS-S7)).
      MOVE W25-COST  TO W100-V2.
      MOVE W100-S9V3  TO W50-DATA.
      MOVE 13   TO W50-LENGTH.
      PERFORM AA200.
      MOVE DHS-TSELL(WS-S7)
     TO W100-V1.
      MOVE W100-S9V2  TO W50-DATA.
      MOVE 13   TO W50-LENGTH.
      PERFORM AA200.
    IF WS-S8 < 12
        ADD 1   TO WS-S7
        WS-S8
        IF WS-S7 > 12
     MOVE 1  TO WS-S7
        END-IF
        GO TO BH56.
      MOVE W55-EXP-DATA  TO EXP-REC1.
      PERFORM WRITE-DEBDET THRU WRITE-DEBDET-EXIT.
      GO TO BH55.
      *    *************************************************************
      *    ****    A C C O U N T   H E A D E R
      *    *************************************************************
       BH60.
      INITIALIZE W60-FIELDS.
      *    *************************************************************
      *    ****    ACCOUNT NUMBER & NAME
      *    *************************************************************
      MOVE 01   TO W60-FNO (1).
      MOVE 02   TO W60-FNO (2).
      PERFORM AA600.
      MOVE DHS-AC  TO W80-ACNO.
             PERFORM CA155-GET-DEBTOR.
      MOVE 1   TO W60-S.
      *    *************************************************************
      *    ****    C L E A R   E X P O R T   D A T A   A R E A
      *    *************************************************************
             MOVE SPACES         TO W55-EXP-DATA.
             MOVE 1              TO W55-S1.
      MOVE DEB-ACNO  TO W50-DATA.
      MOVE 6   TO W50-LENGTH.
      PERFORM AA200.
      MOVE DEB-NAME  TO W60-NAME.
            INSPECT W60-NAME REPLACING ALL "*" BY " ".
            INSPECT W60-NAME REPLACING FIRST "!" BY " ".
      MOVE W60-NAME  TO W50-DATA.
      MOVE 40   TO W50-LENGTH.
      PERFORM AA200.
      MOVE W55-EXP-DATA  TO EXP-REC1.
      PERFORM WRITE-DEBDET THRU WRITE-DEBDET-EXIT.
      INITIALIZE W60-FIELDS.
      *    *************************************************************
      *    ****    ITEM CODE, DECRIPTION, 12 MONTHS QUANTITY
      *     PRICE & EXTENDED PRICE
      *    *************************************************************
      MOVE 03   TO W60-FNO (1).
      MOVE 22   TO W60-FNO (2).
      MOVE DHS-START-MNTH TO WS-S7.
      ADD 3   TO WS-S7.
      MOVE 3   TO WS-S8.
      PERFORM BH62.

       BH62.
      MOVE WS-S7   TO W60-FNO(WS-S8).
      ADD 1   TO WS-S8.
      MOVE 26   TO W60-FNO(WS-S8).
      ADD 1   TO WS-S8.
      MOVE 27   TO W60-FNO(WS-S8).
    IF WS-S8 < 38
        ADD 1   TO WS-S7
        WS-S8
        IF WS-S7 > 15
     MOVE 4  TO WS-S7
        END-IF
        GO TO BH62.
      PERFORM AA600.

       BH65.
      MOVE "Export file created - Press any key"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
      CLOSE DEBDET.
      CLOSE DEBHIS.
      INSPECT W02-DEBTEX REPLACING FIRST "AR_DBXP"
     BY "AR-HIST".
      OPEN I-O DEBHIS.
      GO TO BH999.

       BH70.
      MOVE "'A'll accounts or per 'S'alesperson [A]"
     TO WS-ERR-MES.
      MOVE "A"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "A" OR "S")
        GO TO BH70.
      MOVE WS-OPTION  TO WS-ENQ.
    IF WS-OPTION = "S"
        PERFORM SAVE-SCREEN-3
        PERFORM BH75 THRU BH85
        PERFORM RESTORE-SCREEN-3
        IF WS-SMAN = ZERO
     GO TO BH70.

       BH75.
      MOVE 10   TO SHADE-ROW.
      MOVE 24   TO SHADE-COL.
      MOVE 35   TO SHADE-WIDTH.
      MOVE 5   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0922
        WITH FOREGROUND-COLOR 5 HIGHLIGHT
      BACKGROUND-COLOR 5
       "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "³" AT 1022 WITH FOREGROUND-COLOR 5 HIGHLIGHT
      BACKGROUND-COLOR 5
       "S E L E C T   S A L E S P E R S O N"
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 5
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "³ " AT 1122 WITH FOREGROUND-COLOR 5 HIGHLIGHT
      BACKGROUND-COLOR 5
       "No Name                           "
        WITH FOREGROUND-COLOR 7 BACKGROUND-COLOR 5
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "³                                   " AT 1222
        WITH FOREGROUND-COLOR 5 HIGHLIGHT
      BACKGROUND-COLOR 5
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "À" AT 1322 WITH FOREGROUND-COLOR 5 HIGHLIGHT
          BACKGROUND-COLOR 5
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      MOVE ZERO   TO WS-SMAN.

       BH80.
      ACCEPT S41.
    IF WS-SMAN > 50
        GO TO BH80.
    IF WS-SMAN = ZERO
        GO TO BH85.
            ADD 100 WS-SMAN  GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-SALESMAN = SPACES
              GO TO BH80.
            DISPLAY PAR-SALESMAN AT 1227
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      MOVE PAR-SALESMAN  TO W60-SMAN.

       BH85.
      MOVE 15   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
          IF WS-OPTION = "N"
        GO TO BH80.


       BH990.
      MOVE CRD-EXT-CODE  TO W10-EXT-ITEM.
      PERFORM CA000.
    IF NOT (WS-F-ERROR = ZERO)
        MOVE "ITEM NO LONGER IN STOCK"
     TO STK-DESC.

       BH992.
      MOVE  ZERO   TO DHS-QNT(WS-S9)
        DHS-TCOST(WS-S9)
        DHS-TSELL(WS-S9).
    IF WS-S9 < 12
        ADD 1   TO WS-S9
        GO TO BH992.
      MOVE W15-SMNTH  TO DHS-START-MNTH.
      MOVE STK-DESC  TO DHS-DESC.
      PERFORM WRITE-DEBHIS THRU WRITE-DEBHIS-EXIT.

       BH995.
      MOVE "No Details"  TO WS-ERR-MES.
            PERFORM ERROR-MESSAGE.

       BH998.
      MOVE 9   TO WS-ERROR.

       BH999.
      EXIT.
      /    *************************************************************
      *    **  P R O F I T   F O R   S P E C I F I E D  A C C O U N T
      *        F O R   T H E   S E L E C T E D  P E R I O D
      *    *************************************************************
       BI000         SECTION 58.
       BI00.
            PERFORM ERASE-SCREEN.
      DISPLAY S-PROG2.

       BI05.
      PERFORM SAVE-SCREEN-3.
      MOVE 5   TO SHADE-ROW.
      MOVE 27   TO SHADE-COL.
      MOVE 30   TO SHADE-WIDTH.
      MOVE 5   TO SHADE-LINES.
      DISPLAY S05D.
      PERFORM SCREEN-SHADOW.
      MOVE W12-MONTH  TO W30-START W30-END.

       BI10.
      DISPLAY "Press " AT 5052
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
       "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
    BACKGROUND-COLOR 3
       "ape to Exit"
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
      ACCEPT S05A.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BI999
   WHEN OTHER  CALL X"E5"
       GO TO BI10
        END-EVALUATE.
    IF (W30-START < 01 OR > 12) OR
       (W30-END < 01 OR > 12)
        MOVE "Months must be 01 to 12"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BI10.
      DISPLAY W30-MONTH(W30-START) AT 0747
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      DISPLAY W30-MONTH(W30-END) AT 0847
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BI10.
      PERFORM CLEAR-L50.
      MOVE W12-TODAY  TO W15-START W15-END.
      MOVE W30-START  TO W15-SMNTH.
      MOVE W30-END  TO W15-EMNTH.
      MOVE 01   TO W15-SDAY.
      MOVE 31   TO W15-EDAY.
      *    *************************************************************
      *    ****   I S S T A R T   M O N T H -   PREVIOUS YEAR
      *    *************************************************************
    IF W30-END < W30-START
        SUBTRACT 1  FROM W15-SCCYY.
    IF W15-EMNTH = 04 OR 06 OR 09 OR 11
        MOVE 30   TO W15-EDAY
    ELSE
    IF W15-EMNTH = 02
      *    *************************************************************
      *    ****    C H E C K   F O R   L E A P  Y E A R
      *    *************************************************************
              DIVIDE W15-ECCYY BY 4 GIVING W25-RESULT
              IF W25-DECIMAL = 0
     MOVE 29  TO W15-EDAY
        ELSE
     MOVE 28  TO W15-EDAY.
      *    *************************************************************
      *    ****    SWITCH START DATE TO YEAR/MONTH/DAY
      *    *************************************************************
      MOVE W15-SDAY  TO W15-SDD.
      MOVE W15-SMNTH  TO W15-SMM.
      MOVE W15-SCENT  TO W15-SCC.
      MOVE W15-SYEAR  TO W15-SYY.
      *    *************************************************************
      *    ****    SWITCH END DATE TO YEAR/MONTH/DAY
      *    *************************************************************
      MOVE W15-EDAY  TO W15-EDD.
      MOVE W15-EMNTH  TO W15-EMM.
      MOVE W15-ECENT  TO W15-ECC.
      MOVE W15-EYEAR  TO W15-EYY.
      PERFORM RESTORE-SCREEN-3.

       BI15.
            PERFORM ERASE-SCREEN.
      DISPLAY S10D-P.

       BI20.
            DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             " to do Account Lookup"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.

       BI25.
            ACCEPT S10 AT 0308.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO BI30
         WHEN F1-KEY
       PERFORM HELP-ROUTINE
   WHEN F2-KEY
             PERFORM AA50D THRU AA59
   WHEN OTHER
       CALL X"E5"
       GO TO BI20
        END-EVALUATE
              DISPLAY S10 AT 0308
               IF W80-ACNO = SPACES
           GO TO BI20.

       BI30.
            PERFORM CLEAR-L50.
          IF W80-ACNO = SPACES
              GO TO BI999.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.

       BI35.
            PERFORM CLEAR-L50.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
              MOVE "No record"  TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BI20.
           IF WS-ERROR = 2
              MOVE "Invalid account"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BI20.
      *    *************************************************************
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *    *************************************************************
            MOVE DEB-NAME  TO W40-NAME.
           IF W80-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
            DISPLAY W40-NAME AT 0315
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        PERFORM CLEAR-L50
        GO TO BI20.

       BI40.
      MOVE W80-ACNO  TO CRD-AC.
      MOVE W15-S-YMD  TO CRD-DTE.
      PERFORM START-AT-CRDX-ACNT THRU READ-CARDEX-EXIT.
          IF WS-F-ERROR = 43
        MOVE "No details for period requested"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BI999.
      CLOSE DEBTOT.
      OPEN OUTPUT DEBTOT.
      CLOSE DEBTOT.
      OPEN I-O DEBTOT.
      DISPLAY "Extracting Data:" AT 1112.
      MOVE SPACES  TO TOT-AC   TOT-MTH.
      MOVE ZERO   TO TOT-YEAR TOT-TCOST
        TOT-MNTH TOT-TSELL
        TOT-EMTH.

       BI45.
            PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        GO TO BI50.
    IF NOT (CRD-AC = W80-ACNO)
        GO TO BI50.
    IF CRD-DTE < W15-S-YMD
        GO TO BI45.
    IF CRD-DTE > W15-E-YMD
        GO TO BI50.
      DISPLAY CRD-AC AT 1129
       WITH FOREGROUND-COLOR 7 HIGHLIGHT
       CRD-DTE AT 1136
       WITH FOREGROUND-COLOR 6 HIGHLIGHT.
      MOVE CRD-AC  TO TOT-AC.
      MOVE CRD-DTE  TO W20-DTE.
      MOVE W20-CNYR  TO TOT-YEAR.
      MOVE W20-MONTH  TO TOT-MNTH.
      PERFORM READ-DEBTOT THRU READ-DEBTOT-EXIT.
    IF WS-F-ERROR = "55"
        MOVE W30-MONTH(TOT-MNTH)
     TO TOT-MTH
        MOVE W15-EMM  TO TOT-EMTH
        IF (CRD-QNT < ZERO)
     COMPUTE TOT-TCOST = (CRD-COST * CRD-QNT) * -1
     COMPUTE TOT-TSELL = (CRD-SELL * CRD-QNT) * -1
        ELSE
     MULTIPLY CRD-COST BY CRD-QNT
         GIVING TOT-TCOST
     MULTIPLY CRD-SELL BY CRD-QNT
         GIVING TOT-TSELL
        END-IF
        PERFORM WRITE-DEBTOT THRU WRITE-DEBTOT-EXIT
    ELSE
        IF (CRD-QNT < ZERO)
     COMPUTE TOT-TCOST = TOT-TCOST +
         ((CRD-COST * CRD-QNT) * -1)
     COMPUTE TOT-TSELL = TOT-TSELL +
         ((CRD-SELL * CRD-QNT) * -1)
        ELSE
     COMPUTE TOT-TCOST = TOT-TCOST + (CRD-COST * CRD-QNT)
     COMPUTE TOT-TSELL = TOT-TSELL + (CRD-SELL * CRD-QNT)
        END-IF
        PERFORM REWRITE-DEBTOT THRU WRITE-DEBTOT-EXIT.
      GO TO BI45.

       BI50.
      DISPLAY WS-BLNK78 AT 1102 WITH FOREGROUND-COLOR 3.
      CLOSE DEBTOT.
      OPEN I-O DEBTOT.
      DISPLAY S-PROF.
      MOVE 0707   TO CPOS.
      PERFORM BI60-CRT-EDGE UNTIL CLIN = 19.
      MOVE W80-ACNO  TO TOT-AC.
      MOVE ZERO   TO TOT-YEAR  TOT-MNTH
        W25-VALUE W25-TOTAL.
      PERFORM START-AT-DEBTOT-AC THRU READ-DEBTOT-EXIT.
          IF WS-F-ERROR = 55
        GO TO BI65.
      MOVE 0700   TO CPOS.

       BI55.
      PERFORM READ-DEBTOT-NEXT THRU READ-DEBTOT-EXIT.
          IF WS-F-ERROR = 55
        GO TO BI65.
      MOVE TOT-TCOST  TO W35-COST.
      MOVE TOT-TSELL  TO W35-SELL.
      SUBTRACT TOT-TCOST FROM TOT-TSELL
     GIVING W10-VSELL.
      COMPUTE W10-ADJUST ROUNDED = (W10-VSELL / TOT-TSELL) *
        100.000.
      MOVE W10-VSELL  TO W35-PROF.
      MOVE W10-ADJUST  TO W35-GP.
      ADD TOT-TCOST  TO W25-VALUE.
      ADD TOT-TSELL  TO W25-TOTAL.
      MOVE 08   TO CCOL.
      DISPLAY TOT-MTH AT CPOS
       WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
      MOVE 18   TO CCOL.
      DISPLAY TOT-YEAR AT CPOS
       WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
      MOVE 23   TO CCOL.
      DISPLAY W35-COST AT CPOS
       WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
      MOVE 37   TO CCOL.
      DISPLAY W35-SELL AT CPOS
       WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
      MOVE 51   TO CCOL.
      DISPLAY W35-PROF AT CPOS
       WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
      MOVE 65   TO CCOL.
      DISPLAY W35-GP AT CPOS
       WITH BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
      ADD 1   TO CLIN.
      GO TO BI55.

       BI60-CRT-EDGE.
      DISPLAY "³              ³             ³             ³
      -       "       ³       ³" AT CPOS
        WITH BACKGROUND-COLOR 7
      FOREGROUND-COLOR 7 HIGHLIGHT.
            ADD 1   TO CLIN.

       BI65.
      MOVE 2023   TO CPOS.
      MOVE W25-VALUE  TO W35-COST.
      MOVE W25-TOTAL  TO W35-SELL.
      SUBTRACT W25-VALUE FROM W25-TOTAL
     GIVING W10-VSELL.
      COMPUTE W10-ADJUST ROUNDED = (W10-VSELL / W25-TOTAL) *
        100.000.
      MOVE W10-VSELL  TO W35-PROF.
      MOVE W10-ADJUST  TO W35-GP.
      DISPLAY W35-COST AT CPOS
       WITH BACKGROUND-COLOR 7
     FOREGROUND-COLOR 5.
      MOVE 37   TO CCOL.
      DISPLAY W35-SELL AT CPOS
       WITH BACKGROUND-COLOR 7
     FOREGROUND-COLOR 5.
      MOVE 51   TO CCOL.
      DISPLAY W35-PROF AT CPOS
       WITH BACKGROUND-COLOR 7
     FOREGROUND-COLOR 5.
      MOVE 65   TO CCOL.
      DISPLAY W35-GP AT CPOS
       WITH BACKGROUND-COLOR 7
     FOREGROUND-COLOR 5.

       BI70.
      DISPLAY "Press any key to continue" AT 2322
        WITH FOREGROUND-COLOR 6 HIGHLIGHT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BI75.
            MOVE "Change 'A'ccount,'P'eriod or 'E'xit"
     TO WS-ERR-MES.
      MOVE "A"   TO WS-OPTION.
            PERFORM OPT-MESSAGE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "A" OR "E" OR "P")
        GO TO BI75.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BI999
        END-EVALUATE.
    IF WS-OPTION = "A"
        GO TO BI15.
    IF WS-OPTION = "P"
        GO TO BI00.

       BI999.
      EXIT.
      /    *************************************************************
      *    **  P U R C H A S E  S T A T I S T I C S   M T D   O R
      *        Y T D   I N   D E C E N D I N G  V A L U E   O R
      *        G . P .  V A L U E   S E Q U E N C E
      *    *************************************************************
       BJ000         SECTION 59.
       BJ00.
            PERFORM ERASE-SCREEN.
      DISPLAY S-PROG4.

       BJ05.
      PERFORM SAVE-SCREEN-3.
      MOVE 5   TO SHADE-ROW.
      MOVE 27   TO SHADE-COL.
      MOVE 30   TO SHADE-WIDTH.
      MOVE 4   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 0425
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 6.
      DISPLAY "³    Purchases MTD and YTD.    ³" AT 0525
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 6.
      DISPLAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" AT 0625
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 6.
      DISPLAY "³ Starting Month:              ³" AT 0725
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 6.
      DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 0825
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 6.
      PERFORM SCREEN-SHADOW.
      MOVE W12-MONTH  TO W30-START W30-END.
    IF W30-START = 12
        MOVE 1   TO W30-START
    ELSE
        ADD 1   TO W30-START.
      DISPLAY "This routine extracts details from the starting mo
      -       "nth up to and" AT 1108
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "including the current date. The details are accumu
      -       "lated for the" AT 1208
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY "current month as MTD and for the period as YTD.
      -       "             " AT 1308
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.

       BJ10.
      DISPLAY "Press " AT 5052
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
       "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
    BACKGROUND-COLOR 3
       "ape to Exit"
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
      ACCEPT S05B.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BJ999
   WHEN OTHER  CALL X"E5"
       GO TO BJ10
        END-EVALUATE.
    IF W30-START < 01 OR > 12
        MOVE "Month must be 01 to 12"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
              GO TO BJ10.
      DISPLAY W30-MONTH(W30-START) AT 0747
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      PERFORM CHECK-CORRECT.
          IF WS-OPTION = "N"
        GO TO BJ10.
      PERFORM CLEAR-L50.
      MOVE W12-TODAY  TO W15-START W15-END.
      MOVE W30-START  TO W15-SMNTH.
      MOVE 01   TO W15-SDAY.
      *    *************************************************************
      *    ****   I S S T A R T   M O N T H -   PREVIOUS YEAR
      *    *************************************************************
    IF W30-END < W30-START
        SUBTRACT 1  FROM W15-SCCYY.
      *    *************************************************************
      *    ****    SWITCH START DATE TO YEAR/MONTH/DAY
      *    *************************************************************
      MOVE W15-SDAY  TO W15-SDD.
      MOVE W15-SMNTH  TO W15-SMM.
      MOVE W15-SCENT  TO W15-SCC.
      MOVE W15-SYEAR  TO W15-SYY.
      *    *************************************************************
      *    ****    SWITCH END DATE TO YEAR/MONTH/DAY
      *    *************************************************************
      MOVE W15-EDAY  TO W15-EDD.
      MOVE W15-EMNTH  TO W15-EMM.
      MOVE W15-ECENT  TO W15-ECC.
      MOVE W15-EYEAR  TO W15-EYY.
      MOVE "    !"  TO DBP-AC.
      PERFORM READ-DEBPRF THRU READ-DEBPRF-EXIT.
      *    *************************************************************
      *    ****    H A V E   T H E S E  D E T A I L S  B E E N
      *     E X T R A C T E D
      *    *************************************************************
    IF (DBP-START = W15-S-YMD) AND
       (DBP-END = W15-E-YMD)
        GO TO BJ35.
      *    *************************************************************
      *    ****    C R E A T E  H E A D E R   R E C O R D   O N
      *     E X T R A C T   F I L E
      *    *************************************************************
       BJ15.
      PERFORM RESTORE-SCREEN-3.
      CLOSE DEBPRF.
      OPEN OUTPUT DEBPRF.
      CLOSE DEBPRF.
      OPEN I-O DEBPRF.
      INITIALIZE DBP-REC1
      MOVE "    !"  TO DBP-AC.
      MOVE W15-S-YMD  TO DBP-START.
      MOVE W15-E-YMD  TO DBP-END.
      PERFORM WRITE-DEBPRF THRU WRITE-DEBPRF-EXIT.
      *    *************************************************************
      *    ****    G E T   T H E   F I R S T   D E B T O R
      *     A C C O U N T
      *    *************************************************************
      MOVE " !"   TO DEB-ACNO.
      PERFORM START-AT-DEBT-AC THRU READ-DEBTOR-EXIT.
          IF WS-F-ERROR = 6
        MOVE "No Details" TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BJ999.
             DISPLAY "Debtor account:" AT 1412.

       BJ20.
      DISPLAY WS-BLNK78 AT 1602 WITH FOREGROUND-COLOR 3.
      DISPLAY "Extracting:" AT 1612.
            PERFORM READ-DEBTOR-NEXT THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6
              GO TO BJ38.
      DISPLAY DEB-ACNO AT 1428
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      MOVE DEB-ACNO  TO CRD-AC.
      MOVE W15-S-YMD  TO CRD-DTE.
      PERFORM START-AT-CRDX-ACNT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        GO TO BJ20.
      MOVE DEB-ACNO  TO DBP-AC.
      MOVE DEB-NAME  TO DBP-NAME.
      MOVE DEB-SMAN  TO DBP-SM.
      MOVE ZERO   TO DBP-M-GP
        DBP-M-TCOST
        DBP-M-TSELL
        DBP-GP
        DBP-TCOST
        DBP-TSELL.

       BJ25.
      PERFORM READ-CARDEX-NEXT THRU READ-CARDEX-EXIT.
    IF WS-F-ERROR = 43
        GO TO BJ30.
    IF NOT (CRD-AC = DEB-ACNO)
        GO TO BJ30.
    IF CRD-DTE > W15-E-YMD
        GO TO BJ25.
    IF (CRD-QNT < ZERO)
        COMPUTE DBP-TCOST = DBP-TCOST +
     ((CRD-COST * CRD-QNT) * -1)
        COMPUTE DBP-TSELL = DBP-TSELL +
     ((CRD-SELL * CRD-QNT) * -1)
    ELSE
        COMPUTE DBP-TCOST = DBP-TCOST +
      (CRD-COST * CRD-QNT)
        COMPUTE DBP-TSELL = DBP-TSELL +
      (CRD-SELL * CRD-QNT).
      MOVE CRD-DTE  TO W21-DATE.
    IF W21-MM = W15-EMM
        IF (CRD-QNT < ZERO)
     COMPUTE DBP-M-TCOST = DBP-M-TCOST +
           ((CRD-COST * CRD-QNT) * -1)
     COMPUTE DBP-M-TSELL = DBP-M-TSELL +
           ((CRD-SELL * CRD-QNT) * -1)
        ELSE
     COMPUTE DBP-M-TCOST = DBP-M-TCOST +
     (CRD-COST * CRD-QNT)
     COMPUTE DBP-M-TSELL = DBP-M-TSELL +
     (CRD-SELL * CRD-QNT).
      GO TO BJ25.

       BJ30.
    IF (DBP-TCOST = ZERO) AND
       (DBP-TSELL = ZERO)
        GO TO BJ20.
      SUBTRACT DBP-M-TCOST FROM DBP-M-TSELL
     GIVING DBP-M-GP.
      SUBTRACT DBP-TCOST FROM DBP-TSELL
     GIVING DBP-GP.
      PERFORM WRITE-DEBPRF THRU WRITE-DEBPRF-EXIT.
      GO TO BJ20.

       BJ35.
            MOVE "Details have been extracted - use Y/N?"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "N" OR "Y")
              GO TO BJ35.
    IF WS-OPTION = "N"
        GO TO BJ15.
      PERFORM RESTORE-SCREEN-3.

       BJ38.
      MOVE "'D'isplay or 'P'rinter format [D]"
     TO WS-ERR-MES.
      MOVE "D"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "D" OR "P")
        GO TO BJ38.
      MOVE WS-OPTION  TO WS-ENQD.
      MOVE "Y"   TO WS-MTD-YTD.
      *    IF WS-ENQD = "P"
      *        GO TO BJ40.

       BJ40.
      MOVE 10   TO SHADE-ROW.
      MOVE 24   TO SHADE-COL.
      MOVE 35   TO SHADE-WIDTH.
      MOVE 5   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0922
        WITH FOREGROUND-COLOR 5 HIGHLIGHT
      BACKGROUND-COLOR 5
       "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "³ " AT 1022 WITH FOREGROUND-COLOR 5 HIGHLIGHT
           BACKGROUND-COLOR 5
       "R E P O R T I N G   O P T I O N S"
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 5
       " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "³ " AT 1122 WITH FOREGROUND-COLOR 5 HIGHLIGHT
           BACKGROUND-COLOR 5
       "A" WITH FOREGROUND-COLOR 7 HIGHLIGHT
         BACKGROUND-COLOR 5
       "ll extracted accounts           "
        WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 5
       " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "³ " AT 1222 WITH FOREGROUND-COLOR 5 HIGHLIGHT
           BACKGROUND-COLOR 5
       "S" WITH FOREGROUND-COLOR 7 HIGHLIGHT
         BACKGROUND-COLOR 5
       "elected - top 10/20/50/100/etc  "
        WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 5
       " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "³ " AT 1322 WITH FOREGROUND-COLOR 5 HIGHLIGHT
           BACKGROUND-COLOR 5
       "Select option required ("
        WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 5
       "A" WITH FOREGROUND-COLOR 7 HIGHLIGHT
         BACKGROUND-COLOR 5
       "/" WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 5
       "S" WITH FOREGROUND-COLOR 7 HIGHLIGHT
         BACKGROUND-COLOR 5
       ")     " WITH FOREGROUND-COLOR 3 BACKGROUND-COLOR 5
       " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "À" AT 1422 WITH FOREGROUND-COLOR 5 HIGHLIGHT
          BACKGROUND-COLOR 5
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      PERFORM SCREEN-SHADOW.
      MOVE "S"   TO WS-OPTION.
      DISPLAY "Press " AT 5052
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
       "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
    BACKGROUND-COLOR 3
       "ape to Exit"
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.

       BJ45.
      ACCEPT WS-OPTION AT 1353
      WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BJ999
   WHEN OTHER  CALL X"E5"
       GO TO BJ45
        END-EVALUATE.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "A" OR "S")
        GO TO BJ45.
      PERFORM RESTORE-SCREEN-3.
      MOVE WS-OPTION  TO WS-VIEW.
    IF WS-OPTION = "A"
        GO TO BJ55.
      MOVE 11   TO SHADE-ROW.
      MOVE 26   TO SHADE-COL.
      MOVE 31   TO SHADE-WIDTH.
      MOVE 2   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1024
        WITH FOREGROUND-COLOR 5 HIGHLIGHT
      BACKGROUND-COLOR 5
       "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "³ " AT 1124 WITH FOREGROUND-COLOR 5 HIGHLIGHT
           BACKGROUND-COLOR 5
       "Report/View top      accounts"
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 5
       " ³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "À" AT 1224 WITH FOREGROUND-COLOR 5 HIGHLIGHT
          BACKGROUND-COLOR 5
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      PERFORM SCREEN-SHADOW.
      MOVE ZERO   TO W25-RESULT.
      DISPLAY "Press " AT 5052
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3
       "Esc" WITH FOREGROUND-COLOR 6 HIGHLIGHT
    BACKGROUND-COLOR 3
       "ape to Exit"
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.

       BJ50.
      ACCEPT S40.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BJ999
   WHEN OTHER  CALL X"E5"
       GO TO BJ50
        END-EVALUATE.
    IF W25-WHOLE = ZERO
        GO TO BJ50.
      MOVE W25-WHOLE  TO WS-S.
      MOVE ZERO   TO WS-S1.
      PERFORM RESTORE-SCREEN-3.

       BJ55.
      PERFORM BJ125.

       BJ60.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *   **  *   *   * *   *  *  **  *   *   *     * *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *   *  **   *     *   *  *  *  **   *   *     * *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** * *
      *
    IF WS-ENQD = "P"
        PERFORM OPEN-PRINTER
        PERFORM BJ155
    ELSE
        DISPLAY S-TOP
        MOVE 0601  TO CPOS
        PERFORM BJ100-CRT-EDGE UNTIL CLIN = 22.
      MOVE 999999999.99  TO DBP-TSELL.
      MOVE ZERO   TO WS-S1.
    IF WS-ENQ = "A"
        PERFORM START-AT-DBPR-Y-SELL THRU READ-DEBPRF-EXIT
    ELSE
        MOVE WS-SMAN  TO DBP-SM
        PERFORM START-AT-DBPR-SM-SELL THRU READ-DEBPRF-EXIT
        DISPLAY "Sales person: " AT 0302
   WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
   WS-SMAN WITH FOREGROUND-COLOR 7 HIGHLIGHT
         BACKGROUND-COLOR 7
         " " WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 7
   W60-SMAN WITH FOREGROUND-COLOR 7 HIGHLIGHT
          BACKGROUND-COLOR 7.
    IF WS-F-ERROR = 56
        MOVE "No data on file"
     TO WS-ERR-MES
              PERFORM ERROR-MESSAGE
        GO TO BJ999.

       BJ65.
      MOVE 0601   TO CPOS.

       BJ70.
    IF WS-OPTION = "J"
               MOVE SPACE        TO WS-OPTION
        GO TO BJ75.
      *    *************************************************************
      *    ****    C H E C K   I F   R E Q U E S T E D
      *     S E L E C T I O N   H A S   B E E N
      *     D I S P L A Y E D
      *    *************************************************************
    IF NOT (WS-VIEW = "A")
        IF WS-S1 = WS-S
     GO TO BJ80.
      PERFORM READ-DEBPRF-PREV THRU READ-DEBPRF-EXIT.
    IF WS-ENQ = "S"
        IF NOT (DBP-SM = WS-SMAN)
     GO TO BJ145.
    IF WS-F-ERROR = 56
        GO TO BJ145.
    IF DBP-AC = "    !"
        GO TO BJ70.
      ADD 1   TO WS-S1.
    IF WS-ENQD = "P"
        GO TO BJ165.

       BJ75.
      DISPLAY DBP-AC AT LINE CLIN COLUMN 2
       WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      DISPLAY DBP-SNAME AT LINE CLIN COLUMN 9
       WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      MOVE DBP-TSELL  TO W100-S9V99.
      DISPLAY W100-S9V99 AT LINE CLIN COLUMN 35
       WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      MOVE DBP-TCOST  TO W100-S9V99.
      DISPLAY W100-S9V99 AT LINE CLIN COLUMN 48
       WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      MOVE DBP-GP  TO W100-S9V99.
      DISPLAY W100-S9V99 AT LINE CLIN COLUMN 61
       WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
      COMPUTE W100-S3V99 = (DBP-GP / DBP-TSELL) * 100.000.
      DISPLAY W100-S3V99 AT LINE CLIN COLUMN 74
       WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
          IF CLIN < 21
              ADD 1   TO CLIN
              GO TO BJ70.
      
       BJ80.
      DISPLAY WS-BLNK78 AT 2302 WITH FOREGROUND-COLOR 3.
            DISPLAY "PgUp" AT 2303 WITH FOREGROUND-COLOR 14
                     "/" WITH FOREGROUND-COLOR 7
       "PgDn" WITH FOREGROUND-COLOR 14
       " - " WITH FOREGROUND-COLOR 7
             "Esc" WITH FOREGROUND-COLOR 14
       "ape." WITH FOREGROUND-COLOR 7.

       BJ85.
      PERFORM SAVE-SCREEN-2.
            MOVE 0   TO W75-S1.
             MOVE 0601           TO CPOS.

       BJ90.
      MOVE 3920   TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
           W42-ATTRIB2
           W41-LENGTH
     RETURNING WS-STATUS.
            MOVE 4000   TO W41-LENGTH.

       BJ95.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
      PERFORM CLEAR-L50.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       PERFORM RESTORE-SCREEN-3
       GO TO BJ150
   WHEN PAGE-DOWN
             GO TO BJ105
   WHEN PAGE-UP
             GO TO BJ115
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO BJ95.
      CALL X"E5".
            GO TO BJ95.

       BJ100-CRT-EDGE.
      DISPLAY "³" AT CPOS WITH FOREGROUND-COLOR 7 HIGHLIGHT
          BACKGROUND-COLOR 0
       "      ³                         ³            ³
      -       "        ³            ³      "
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7
       "³" WITH FOREGROUND-COLOR 7 HIGHLIGHT
         BACKGROUND-COLOR 0.
            ADD 1   TO CLIN.

       BJ105.
          IF CLIN < 21
              GO TO BJ75.

       BJ110.
            MOVE 0601   TO CPOS.
      PERFORM BJ100-CRT-EDGE UNTIL CLIN = 22.
            GO TO BJ65.

       BJ115.
             MOVE 1              TO WS-IXS1.
      *    *************************************************************
      *    ****    SET OPTION TO BYPASS THE READ NEXT
      *    *************************************************************
       MOVE "J"    TO WS-OPTION.

       BJ120.
      PERFORM READ-DEBPRF-NEXT THRU READ-DEBPRF-EXIT.
     IF WS-F-ERROR = 56
        MOVE SPACE  TO WS-OPTION
              MOVE 0602  TO CPOS
        GO TO BJ110.
    IF WS-ENQ = "S"
        IF NOT (DBP-SM = WS-SMAN)
     MOVE SPACE  TO WS-OPTION
           MOVE 0602  TO CPOS
     GO TO BJ110.
     IF WS-IXS1 < 36
  ADD 1   TO WS-IXS1
  SUBTRACT 1  FROM WS-S1
  END-IF
  GO TO BJ120.
      GO TO BJ110.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ PERFORMED from BJ55 to determine routine required and if  ³
      *    ³ necessary, get the Salesperson details.         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BJ125.
      MOVE "'A'll accounts or per 'S'alesperson [A]"
     TO WS-ERR-MES.
      MOVE "A"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "A" OR "S")
        GO TO BJ125.
      MOVE WS-OPTION  TO WS-ENQ.
      MOVE ZERO   TO W40-M-COST W40-M-SELL
        W40-M-GP W40-Y-COST
        W40-Y-SELL W40-Y-GP.
    IF WS-OPTION = "S"
        PERFORM BJ130 THRU BJ140
        IF WS-SMAN = ZERO
     GO TO BJ125.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ PERFORMED from BJ125 to get the Salesperson details.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       BJ130.
      MOVE 10   TO SHADE-ROW.
      MOVE 24   TO SHADE-COL.
      MOVE 35   TO SHADE-WIDTH.
      MOVE 5   TO SHADE-LINES.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 0922
        WITH FOREGROUND-COLOR 5 HIGHLIGHT
      BACKGROUND-COLOR 5
       "¿" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "³" AT 1022 WITH FOREGROUND-COLOR 5 HIGHLIGHT
      BACKGROUND-COLOR 5
       "S E L E C T   S A L E S P E R S O N"
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 5
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "³ " AT 1122 WITH FOREGROUND-COLOR 5 HIGHLIGHT
      BACKGROUND-COLOR 5
       "No Name                           "
        WITH FOREGROUND-COLOR 7 BACKGROUND-COLOR 5
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "³                                   " AT 1222
        WITH FOREGROUND-COLOR 5 HIGHLIGHT
      BACKGROUND-COLOR 5
       "³" WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      DISPLAY "À" AT 1322 WITH FOREGROUND-COLOR 5 HIGHLIGHT
          BACKGROUND-COLOR 5
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 5.
      MOVE ZERO   TO WS-SMAN.

       BJ135.
      ACCEPT S41.
    IF WS-SMAN > 50
        GO TO BJ135.
    IF WS-SMAN = ZERO
        GO TO BJ140.
            ADD 100 WS-SMAN  GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-SALESMAN = SPACES
              GO TO BJ135.
            DISPLAY PAR-SALESMAN AT 1227
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.
      MOVE PAR-SALESMAN  TO W60-SMAN.

       BJ140.
      MOVE 15   TO SLIN.
      PERFORM CHECK-SETUP THRU CHECK-EXIT.
          IF WS-OPTION = "N"
        GO TO BJ135.

       BJ145.
      DISPLAY "End - Press ANY key" AT 5012
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 6 HIGHLIGHT.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.

       BJ150.
      PERFORM RESTORE-SCREEN-3.
    IF WS-ENQ = "A"
        GO TO BJ995.
      MOVE "'N'ext Sales person or 'E'xit [N]"
     TO WS-ERR-MES.
      MOVE "N"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "N" OR "E")
        GO TO BJ150.
    IF WS-OPTION = "E"
        GO TO BJ995.
    IF WS-OPTION = "N"
        PERFORM BJ130 THRU BJ140
        IF WS-SMAN = ZERO
     GO TO BJ995.
      PERFORM BJ995.
      GO TO BJ60.
      *    *************************************************************
      *    ****   R E P O R T H E A D I N G
      *    *************************************************************
       BJ155.
      MOVE SPACES  TO W100-DESC.
      MOVE W30-MONTH(W30-START)
     TO W30-START-N.
      MOVE W30-MONTH(W30-END)
     TO W30-END-N.
      STRING W30-START-N DELIMITED BY " "
      " to " DELIMITED SIZE
      W30-END-N DELIMITED BY " "
      INTO W100-DESC.
       BJ155-EXIT.
      EXIT.
      *    *************************************************************
      *    ****   1 0 C P I
      *    *************************************************************
       BJ160.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE "1"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "P"   TO WS-COMMAND.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE SPACES  TO REP-DETAIL1.
            MOVE "PAGE:"  TO PRF-PAGE.
            MOVE "DATE:"  TO PRF-DATE.
            ADD 1   TO WS-PAGE.
            MOVE W12-TODAY  TO PRF-DTE.
            MOVE WS-PAGE  TO PRF-P-NO.
            MOVE W100-HEAD  TO PRF-COMPANY.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL1.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE "E"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
      MOVE "H"   TO W02-PRN-TYPE.
            MOVE W95-COMP  TO REP-CO.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
             MOVE SPACES TO REP-DETAIL1.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        MOVE "X"   TO W02-PRN-TYPE
        PERFORM CALL-PRINTUTL.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE "H"   TO W02-PRN-TYPE.
    IF WS-ENQ = "S"
        MOVE "Sales Person:"
     TO PRF-SP-H
        MOVE WS-SMAN  TO PRF-SP-NO
        MOVE W60-SMAN  TO PRF-SP-N
        IF NOT (WS-PRINTER = "S")
     MOVE 0  TO WS-ADVANCE
     PERFORM CALL-PRINTUTL
        END-IF
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
        MOVE SPACES  TO REP-DETAIL1.
      MOVE "________________Month-to-date________________"
     TO PRF-H3.
      MOVE "________________Year-to-date_________________"
     TO PRF-H4.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL1.
      MOVE "Act No"  TO PRF-H1.
      MOVE "Name"  TO PRF-H2.
      MOVE "Total Sell"  TO PRF-H3A PRF-H4A.
      MOVE "Total Cost"  TO PRF-H3B PRF-H4B.
      MOVE "GP Value"  TO PRF-H3C PRF-H4C.
      MOVE "GP %"  TO PRF-H3D PRF-H4D.
      MOVE "Seq"   TO PRF-H5.
    IF NOT (WS-PRINTER = "S")
        MOVE 0   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL.
      MOVE 1   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      MOVE SPACES  TO REP-DETAIL1.
      MOVE "D"   TO W02-PRN-TYPE.

       BJ165.
    IF WS-PAGE = ZERO
        PERFORM BJ160.
      MOVE DBP-AC  TO PRF-ACNO.
      MOVE DBP-NAME  TO PRF-NAME.
      MOVE DBP-M-TSELL  TO PRF-MTD-S.
      ADD DBP-M-TSELL  TO W40-M-SELL.
      MOVE DBP-M-TCOST  TO PRF-MTD-C.
      ADD DBP-M-TCOST  TO W40-M-COST.
      MOVE DBP-M-GP  TO PRF-MTD-GP.
      ADD DBP-M-GP  TO W40-M-GP.
      COMPUTE PRF-MTD-PER = (DBP-M-GP / DBP-M-TSELL) * 100.000.
      MOVE DBP-TSELL  TO PRF-YTD-S.
      ADD DBP-TSELL  TO W40-Y-SELL.
      MOVE DBP-TCOST  TO PRF-YTD-C.
      ADD DBP-TCOST  TO W40-Y-COST.
      MOVE DBP-GP  TO PRF-YTD-GP.
      ADD DBP-GP   TO W40-Y-GP.
      COMPUTE PRF-YTD-PER = (DBP-GP / DBP-TSELL) * 100.000.
      MOVE WS-S1   TO PRF-SEQ.
          IF W02-LINAGE < (W02-PRN-LENGTH - 4)
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
           ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM BJ160.
            MOVE SPACES  TO REP-DETAIL1.
      PERFORM PRINT-ABORT.
    IF (WS-OPTION = "Y") OR (WS-COMMAND = "Z")
              MOVE "**** PRINT ABORTED BY OPERATOR ****"
                                 TO REP-DETAIL1
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              GO TO BJ995.

       BJ170.
      PERFORM READ-DEBPRF-PREV THRU READ-DEBPRF-EXIT.
    IF WS-ENQ = "S"
        IF NOT (DBP-SM = WS-SMAN)
     PERFORM BJ180
     GO TO BJ175.
    IF WS-F-ERROR = 56
        PERFORM BJ180
        GO TO BJ175.
    IF DBP-AC = "    !"
        GO TO BJ170.
      ADD 1   TO WS-S1.
      GO TO BJ165.

       BJ175.
      MOVE "E"   TO WS-COMMAND.
      MOVE 99   TO WS-ADVANCE.
      PERFORM CALL-PRINTUTL.
      PERFORM RESTORE-SCREEN-3.
      GO TO BJ145.

       BJ180.
            MOVE SPACES  TO REP-DETAIL1.
          IF W02-LINAGE < (W02-PRN-LENGTH - 5)
        MOVE 1   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
          ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM BJ160.
    IF WS-ENQ = "A"
        MOVE "******"  TO PRF-ACNO
        MOVE "TOTAL FOR PERIOD ******"
     TO PRF-NAME
    ELSE
        STRING "Tot-" DELIMITED SIZE
        WS-SMAN DELIMITED SIZE
     INTO PRF-ACNO
        MOVE W60-SMAN  TO PRF-NAME
    END-IF.
      MOVE W40-M-SELL  TO PRF-MTD-S.
      MOVE W40-M-COST  TO PRF-MTD-C.
      MOVE W40-M-GP  TO PRF-MTD-GP.
      COMPUTE PRF-MTD-PER = (W40-M-GP / W40-M-SELL) * 100.000.
      MOVE W40-Y-SELL  TO PRF-YTD-S.
      MOVE W40-Y-COST  TO PRF-YTD-C.
      MOVE W40-Y-GP  TO PRF-YTD-GP.
      COMPUTE PRF-YTD-PER = (W40-Y-GP / W40-Y-SELL) * 100.000.
          IF W02-LINAGE < (W02-PRN-LENGTH - 5)
        MOVE 2   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
          ELSE
        MOVE 99   TO WS-ADVANCE
        PERFORM CALL-PRINTUTL
              PERFORM BJ160
    END-IF.
            MOVE SPACES  TO REP-DETAIL1.
      MOVE ZERO   TO W40-M-COST W40-M-SELL
        W40-M-GP   W40-Y-COST
        W40-Y-SELL W40-Y-GP.

       BJ995.
    IF WS-ENQD = "P"
        MOVE "C"   TO WS-COMMAND
        PERFORM CALL-PRINTUTL.
      MOVE ZERO   TO WS-PAGE.

       BJ999.
      EXIT.
      *    *************************************************************
      *    ****   G E T   S T O C K   R E C O R D   U S I N G C O D E
      *    *************************************************************
       CA000     SECTION 60.
       CA00.
            MOVE 18  TO W50-LENGTH.

       CA05.
    IF WS-EXT-STK = "Y"
              MOVE W10-EXT-ITEM TO W50-DATA
    ELSE
        MOVE W10-ITEM  TO W50-DATA
        MOVE W10-ITM  TO W50-ITM.
    IF W50-1 = SPACE
        IF WS-EXT-STK = "Y"
     MOVE W50-17  TO W10-EXT-ITEM
        ELSE
     MOVE W50-13  TO W10-ITEM
        END-IF
               GO TO CA05.
      CALL "CBL_TOUPPER" USING W50-DATA
                                BY VALUE W50-LENGTH
                                RETURNING W50-STATUS.
            MOVE W50-EXT-ITEM TO W10-EXT-ITEM.

       CA06.
            MOVE W50-EXT-ITEM  TO STK-CODE.

       CA10.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.

       CA99.
             EXIT.
      *    *************************************************************
      *    ****    Read debtor record - using account number.
      *    *************************************************************
       CA155-GET-DEBTOR  SECTION 60.
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
       CA165-NAME  SECTION 60.
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
       CA175-ADDRESS  SECTION 60.
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
      /    *************************************************************
      *    ****    I N I T I A L I Z E   P R O G R A M
      *    *************************************************************
       ZA000-INIT        SECTION 80.
       ZA000-OPEN.
             PERFORM ZA55 THRU ZA60.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY W12-TODAY.
      MOVE LS-USUB  TO WS-USUB.
            PERFORM ERASE-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-VAT-REG  TO W05-VATNO.
    IF NOT (PAR-STOCK = "A" OR "N")
        MOVE "A"   TO PAR-STOCK.
      MOVE PAR-STOCK  TO W10-STCK.
      MOVE PAR-USE-ITM  TO WS-USE-ITM.
      MOVE PAR-EXT-STK  TO WS-EXT-STK.
    IF PAR-STOCK = "N"
        MOVE PAR-SLNGTH  TO W10-SLNGTH
    ELSE
        MOVE 14   TO W10-SLNGTH.
    IF W10-SLNGTH < 3
        MOVE 3   TO W10-SLNGTH
    ELSE
    IF W10-SLNGTH > 14
        MOVE 14   TO W10-SLNGTH.
    IF LS-USER NUMERIC
        IF LS-USE > ZERO AND < 110
     MOVE LS-USE  TO WS-USUB
                   GO TO ZA01A.
             MOVE 110            TO WS-USUB.

       ZA01A.
             ADD 2549 WS-USUB    GIVING W25-RESULT.
             MOVE W25-KEY        TO WS-PARKEY.
             ADD 1 W25-SUB       GIVING WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.

       ZA02.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY    TO W95-COMP.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
            MOVE 5   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
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
            OPEN I-O CARDEX.
    IF WS-STATUS = "05"
        CLOSE CARDEX
        OPEN OUTPUT CARDEX
        CLOSE CARDEX
        OPEN I-O CARDEX.
          IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
           GO TO ZA49
               ELSE
        IF IDX-CORRUPT
     MOVE 43  TO WS-F-ERROR
     GO TO ZA50.
           IF WS-STATUS NOT = "00"
              MOVE 43   TO WS-F-ERROR
        IF WS-STATUS = "39"
           GO TO ZA50
        ELSE
           PERFORM OPEN-ERROR.
            OPEN I-O STOCK.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 22       TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 22           TO WS-F-ERROR
               PERFORM OPEN-ERROR.
            OPEN I-O DEBTEX.
    IF WS-STATUS = "05"
        CLOSE DEBTEX
        OPEN OUTPUT DEBTEX
        CLOSE DEBTEX
        OPEN I-O DEBTEX.
          IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
           GO TO ZA49
               ELSE
        IF IDX-CORRUPT
     CLOSE DEBTEX
     OPEN OUTPUT DEBTEX
     CLOSE DEBTEX
     OPEN I-O DEBTEX.
          IF WS-STATUS NOT = "00"
              MOVE 52   TO WS-F-ERROR
        IF WS-STATUS = "39"
           CLOSE DEBTEX
     OPEN OUTPUT DEBTEX
     CLOSE DEBTEX
     OPEN I-O DEBTEX
        ELSE
           PERFORM OPEN-ERROR.
            OPEN I-O DEBHIS.
    IF WS-STATUS = "05"
        CLOSE DEBHIS
        OPEN OUTPUT DEBHIS
        MOVE X"000000000001"
     TO DHS-AC
        MOVE SPACES  TO DHS-EXT-CODE
        PERFORM WRITE-DEBHIS THRU WRITE-DEBHIS-EXIT
        CLOSE DEBHIS
        OPEN I-O DEBHIS.
          IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
           GO TO ZA49
               ELSE
        IF IDX-CORRUPT
     CLOSE DEBHIS
     OPEN OUTPUT DEBHIS
     CLOSE DEBHIS
     OPEN I-O DEBHIS.
          IF WS-STATUS NOT = "00"
              MOVE 54   TO WS-F-ERROR
        IF WS-STATUS = "39"
           CLOSE DEBHIS
     OPEN OUTPUT DEBHIS
     CLOSE DEBHIS
     OPEN I-O DEBHIS
        ELSE
           PERFORM OPEN-ERROR.
            OPEN I-O DEBPRF.
    IF WS-STATUS = "05"
        CLOSE DEBPRF
        OPEN OUTPUT DEBPRF
        CLOSE DEBPRF
        OPEN I-O DEBPRF
        INITIALIZE DBP-REC1
        MOVE "    !"  TO DBP-AC
        MOVE ZERO  TO DBP-START
        DBP-END
        PERFORM WRITE-DEBPRF THRU WRITE-DEBPRF-EXIT.
          IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
           GO TO ZA49
               ELSE
        IF IDX-CORRUPT
     CLOSE DEBPRF
     OPEN OUTPUT DEBPRF
     CLOSE DEBPRF
     OPEN I-O DEBPRF.
          IF WS-STATUS NOT = "00"
              MOVE 56   TO WS-F-ERROR
        IF WS-STATUS = "39"
           CLOSE DEBPRF
     OPEN OUTPUT DEBPRF
     CLOSE DEBPRF
     OPEN I-O DEBPRF
        ELSE
           PERFORM OPEN-ERROR.
      *      OPEN I-O DEBTOT.
      *    IF WS-STATUS = "05"
      *        CLOSE DEBTOT
        OPEN OUTPUT DEBTOT.
      *        CLOSE DEBTOT
      *        OPEN I-O DEBTOT.
      *    IF RUNTIME-ERROR
      *  IF FLE-LOCKED
      *      GO TO ZA200
      *  ELSE
      *  IF FLE-LIMIT
      *     GO TO ZA49
      *  ELSE
      *        IF IDX-CORRUPT
      *     CLOSE DEBTOT
      *     OPEN OUTPUT DEBTOT
      *     CLOSE DEBTOT
      *     OPEN I-O DEBTOT.
      *    IF WS-STATUS NOT = "00"
      *        MOVE 55   TO WS-F-ERROR
      *        IF WS-STATUS = "39"
      *     CLOSE DEBTOT
      *     OPEN OUTPUT DEBTOT
      *     CLOSE DEBTOT
      *     OPEN I-O DEBTOT
      *        ELSE
      *     PERFORM OPEN-ERROR.
            GO TO ZA999-EXIT.
      *    *************************************************************
      *    ****   F I L E   L I M I T E X C E E D E D
      *    (Config.sys Dos/Win/Windows 95/98/NT/2000)
      *    (Config.nt Windows XP)
      *    (System.ini Windows.me)
      *    *************************************************************
       ZA49.
            DISPLAY "Too many files OPEN" AT 0708
                      WITH FOREGROUND-COLOR 14.
            DISPLAY "Check the FILES option in CONFIG.SYS (Dos,Windows
      -       "3/95/98/NT/2000)" AT 0908.
      DISPLAY "CONFIG.NT  (Windows XP)" AT 1034.
      DISPLAY "SYSTEM.INI (Windows ME)" AT 1134.
            DISPLAY "Press any key to continue" AT 1308.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
            GO TO ZA205.

       ZA50.
          IF WS-F-ERROR = 22
              MOVE "STOCK File"
     TO WS-FILE
           ELSE
          IF WS-F-ERROR = 43
              MOVE "CARDEX file"
     TO WS-FILE.
            DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
       ZA51.
             EXIT PROGRAM.

       ZA55.
             MOVE 1              TO WS-S1.

       ZA60.
             MOVE WS-G1          TO WS-TCHR (WS-S1) WS-BCHR (WS-S1).
             MOVE WS-G8          TO WS-TCH  (WS-S1) WS-BCH  (WS-S1).
           IF WS-S1 < 80
               ADD 1             TO WS-S1
               GO TO ZA60.
             MOVE WS-G9          TO WS-TCH  (1).
             MOVE WS-G10         TO WS-TCH  (80).
             MOVE WS-G11         TO WS-BCH  (1).
             MOVE WS-G12         TO WS-BCH  (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G2          TO WS-TCHR (8)  WS-TCHR (49)
                                    WS-TCHR (61) WS-TCHR (72).
             MOVE WS-G4          TO WS-BCHR (8)  WS-BCHR (49)
                                    WS-BCHR (61) WS-BCHR (72).
             MOVE WS-G13         TO WS-TCHR (80) WS-BCHR (80).
             MOVE WS-G14         TO WS-TCHR (1)  WS-BCHR (1).
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
       ZA200.
             DISPLAY "Files locked - Try later" AT 2312
                      WITH FOREGROUND-COLOR 14
                     " " WS-STATUS WITH FOREGROUND-COLOR 15.
            DISPLAY "Press any key" AT 5012
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
            ACCEPT WS-OPTION AT 5026
      WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 15 AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999-EXIT.
             EXIT.
      /    *************************************************************
      *    ****    I / O   E R R O R   M E S S A G E S
      *    *************************************************************
       ZB000-ERROR      SECTION 81.

       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR 14.
             GO TO DISPLAY-FILE-NAME.

       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR 14.
             GO TO DISPLAY-FILE-NAME.

       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR 14.
             GO TO DISPLAY-FILE-NAME.
       I-O-ERROR-EXIT.
             EXIT.

       DISPLAY-FILE-NAME.
          IF WS-F-ERROR = 6
               MOVE W02-DEBTOR TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 7
               MOVE W02-DEPART TO WS-FILE
               MOVE WS-DEPKEY TO WS-KEY
          ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID TO WS-FILE
               MOVE WS-PARKEY TO WS-KEY
          ELSE
           IF WS-F-ERROR = 22
               MOVE W02-STOCKF   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE STK-CODE  TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 27
               MOVE W02-DEBMEM   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE DME-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 43
              MOVE W02-CARDEX  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE CRD-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 47
              MOVE W02-DEBALT  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE DAL-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 52
              MOVE W02-DEBTEX  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE DXT-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 54
              MOVE W02-DEBHIS  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE DHS-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 55
              MOVE W02-DEBTOT  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE TOT-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 56
              MOVE W02-DEBPRF  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE DBP-AC  TO WS-KEYX.
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
