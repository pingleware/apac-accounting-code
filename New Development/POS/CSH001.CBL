      $set LINKCOUNT"512"
      ******************************************************************
      *                                                                *
      *   ******     ******    **    **     ****       ****      **    *
      *  **    **   **    **   **    **    **  **     **  **    ***    *
      *  **         **         **    **   **    **   **    **    **    *
      *  **          ******    ********   **    **   **    **    **    *
      *  **               **   **    **   **    **   **    **    **    *
      *  **    **   **    **   **    **    **  **     **  **     **    *
      *   ******     ******    **    **     ****       ****     ****   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *     ( A P A C )   C A S H   S A L E S P R O G R A M        *
      *                                                                *
      *     VERSION 8.14.03 - May 2010           *
      *               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CSH001.
       AUTHOR.         J W LEMMON  (APAC).
       DATE-WRITTEN.   JUNE 1990.

     COPYRIGHT NOTICE: COPYRIGHT (C) 1990 - 2010
         by James William Lemmon.
           (Id No. 4412165050082).

     All rights reserved.

     e-mail jwlemmon@gmail.com.

       SECURITY.
     This program is free software; you can redistribute
     it and/or modify it under the terms of the GNU General
     Public License as published by the Free Software
     Foundation; either version 2 of the License, or (at
     your option) any later version.

     This program is distributed in the hope that it will
     be useful, but WITHOUT ANY WARRANTY; without even the
     implied warranty of MERCHANTABILITY or FITNESS FOR A
     PARTICULAR PURPOSE. See the GNU General Public License
     for more details.

     You should have received a copy of the GNU General
     Public License along with this program; if not, write
     to the Free Software Foundation, Inc., 59 Temple Place
     - Suite 330, Boston, MA 02111-1307, USA.


       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               CURSOR IS CSTART
               CONSOLE IS CRT
         CRT STATUS IS KEY-STATUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY AUDIT.SL.

       COPY CARDEX.SL.

       COPY CHEQUE.SL.

       COPY CONTROL.SL.

       COPY DEPART.SL.

       COPY LEDTRF.SL.

       COPY PARAM.SL.

       COPY RECOVER.SL.

       COPY SALE.SL.

       COPY SHARED.SL.

       COPY STOCK.SL.

       COPY STKMEM.SL.

       COPY TXTRAN.SL.

           SELECT CSHDRW  ASSIGN W02-CSHDRW
                          ORGANIZATION SEQUENTIAL
                          ACCESS SEQUENTIAL.
           SELECT CSHSLE  ASSIGN DISK
                          ORGANIZATION LINE SEQUENTIAL
                          ACCESS SEQUENTIAL.
          SELECT CUSDSP  ASSIGN W02-CUSDSP
                          ORGANIZATION SEQUENTIAL
                          ACCESS SEQUENTIAL.

      /
       DATA DIVISION.
       FILE SECTION.

       COPY APACFIDS.FDE.

       COPY AUDIT.FDE.

       COPY CARDEX.FDE.

       COPY CHEQUE.FD.

       COPY CONTROL.FDE.

       COPY DEPART.FDE.

       COPY LEDTRF.FDE.

       COPY PARAM.FDE.

       COPY RECOVER.FD.

       COPY SALE.FD.

       COPY SHARED.FDE.

       COPY STOCK.FDE.

       COPY STKMEM.FDE.

       COPY TXTRAN.FDE.

       FD  CSHDRW    LABEL RECORD OMITTED.
       01  CSH-REC.
           03  CSH-OPEN        PIC  X(06).
       FD  CSHSLE    LABEL RECORD STANDARD
                     VALUE OF FILE-ID W02-PRINTER.
       01  CSH-L.
           03  CSH-DATA.
        05  CSH-LINE PIC  X(40).
        05  FILLER   PIC  X(40).
       FD  CUSDSP    LABEL RECORD OMITTED.
       01  CUS-REC.
      *    03  CUS-CON        PIC  X(01).
      *    03  CUS-INST        PIC  X(02).
    03  CUS-LINE        PIC  X(40).
      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK    PIC X(18)  VALUE
      "aeWlimemnomLalismJ".
       77  WS-DISC    PIC 9(02)V99 COMP-3.
       77  WS-TDISC    PIC 9(02)V99 COMP-3.
       77  WS-SUB    PIC 9(04)  COMP-5.
       77  WS-SUPER    PIC 9(02)  COMP-5.
       77  WS-S1    PIC 9(04)  COMP-5.
       77  WS-S2    PIC 9(04)  COMP-5.
       77  WS-S3    PIC 9(04)  COMP-5.
       77  WS-S4    PIC 9(04)  COMP-5.
       77  WS-S5    PIC 9(04)  COMP-5.
       77  WS-S6    PIC 9(04)  COMP-5.
       77  WS-S7    PIC 9(04)  COMP-5.
       77  WS-S8    PIC 9(04)  COMP-5.
       77  WS-S9    PIC 9(04)  COMP-5.
       77  WS-IXS    PIC 9(04)  COMP-5.
       77  WS-IXS1    PIC 9(04)  COMP-5.
       77  WS-IXS2    PIC 9(04)  COMP-5.
       77  WS-IXS3    PIC 9(04)  COMP-5.
       77  WS-IXS4    PIC 9(04)  COMP-5.
       77  WS-STKKEY    PIC 9(04)  COMP-5.
       77  WS-PARKEY    PIC 9(04)  COMP-5.
       77  WS-NETKEY    PIC 9(04)  COMP-5.
       77  WS-AUDKEY    PIC 9(04)  COMP-5.
       77  WS-SALSTR    PIC 9(04)  COMP-5.
       77  WS-SALKEY    PIC 9(04)  COMP-5.
       77  WS-RECKEY    PIC 9(04)  COMP-5.
       77  WS-RECOVER    PIC 9(04)  COMP-5.
       77  WS-SHARED    PIC 9(04)  COMP-5 VALUE 1.
       77  WS-TRANS        PIC  9(06)    COMP-5 VALUE 1.
       77  WS-STOCK    PIC X(01).
       77  WS-KEY1    PIC 9(04)  COMP-5.
       77  WS-ITM    PIC X(14).
       77  WS-ITM1    PIC X(14).
       77  WS-START    PIC 9(08).
       77  WS-END    PIC 9(08).
       77  WS-TIME    PIC 9(08).
       77  WS-TME2    PIC 9(08).
       77  WS-PORT    PIC 9(05) VALUE 259.
      * 1004.
       77  WS-DRAW1    PIC 9(05) VALUE 260.
       77  WS-OPEN    PIC X(01) VALUE X"01".
       77  WS-CLOSE    PIC X(01) VALUE X"00".
       77  WS-PAR    PIC 9(01) VALUE 0.
       77  WS-INO    PIC 9(02) VALUE 0.
       77  WS-LINES    PIC 9(02) VALUE 0.
       77  WS-FIRST    PIC 9(03) VALUE 0.
       77  WS-SALPOS    PIC 9(03) VALUE 0.
       77  WS-LAST    PIC 9(03) VALUE 0.
       77  WS-PAGE    PIC 9(04)  COMP-5.
       77  WS-OPTION    PIC X(01).
       77  WS-CARDEX    PIC X(01).
       77  WS-PRC-IND    PIC X(01).
       77  WS-BNK-IND    PIC X(01).
       77  WS-SLIP-P    PIC X(01).
       77  WS-PRNINV    PIC X(01).
       77  WS-VAT-GST      PIC  X(01) VALUE "V".
       77  WS-SKIP         PIC  X(01) VALUE " ".
       77  WS-LOOK         PIC  X(01) VALUE "S".
       77  WS-ETYPE        PIC  X(01) VALUE "I".
       77  WS-PROMPT    PIC X(01).
       77  WS-WS    PIC X(01).
       77  WS-USE-CASES    PIC X(01).
       77  WS-USE-PACKS    PIC X(01).
       77  WS-USE-ITM    PIC X(01).
       77  WS-EXT-STK    PIC X(01).
       77  WS-NUM    PIC Z9.
       77  WS-ERROR    PIC 9(01).
       77  PRG-NAME2    PIC X(12) VALUE SPACES.
       77  WS-COMP    PIC 9(01).
       77  WS-INDI    PIC 9(01) VALUE ZERO.
       77  WS-UPDATE    PIC 9(01) VALUE ZERO.
       77  WS-CONREC    PIC 9(01) VALUE ZERO.
       77  WS-HEAD    PIC X(01).
       77  WS-TYPE    PIC X(01).
       77  WS-AGE    PIC X(01).
       77  WS-PRC    PIC X(01).
       77  WS-BATCH    PIC 9(05).
       77  WS-PER    PIC Z(02)9.99.
       77  WS-CR    PIC X(01).
       77  WS-GSTRES    PIC S9(07)V9(08) COMP-3.
       77  TODAY-DDMMYY    PIC 9(08) COMP-5.
       77  WS-USUB    PIC 9(04) COMP-5.
       77  WS-DRAW    PIC 9(02).
       77  WS-CSHCODE    PIC X(06).
      /
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
    03  WS-MINV-LINE REDEFINES WS-MID-LINE.
        05  WS-MIV1     PIC  X(01).
        05  WS-MIV78    PIC  X(78).
        05  WS-MIV80    PIC  X(01).
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
        03  WS-MM-LINE.
        05  FILLER      PIC  X(01) VALUE "³".
        05  WS-SPACE    PIC  X(60) VALUE SPACES.
        05  FILLER      PIC  X(01) VALUE "³".
    03  WS-HME-LINE PIC X(60) VALUE ALL "Í".

       COPY WS.WS.

       01  WS-HELP.
    03  WS-MODULE       PIC  X(03) VALUE "CSH".
    03  WS-PROG        PIC  X(03) VALUE "001".

       COPY LEDTRF.WS.

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W02-FID.

       COPY APACFIDS.ID.

       COPY AUDIT.ID.

       COPY CARDEX.ID.

       COPY CHEQUE.ID.

       COPY CONTROL.ID.

       COPY DEPART.ID.

       COPY LEDTRF.ID.

       COPY PARAM.ID.

       COPY RECOVER.ID.

       COPY SALE.ID.

       COPY SHARED.ID.

       COPY STOCK.ID.

       COPY STKMEM.ID.

       COPY TXTRAN.ID.

       01  W02-FILE-IDS.
    03  W02-PRINTER     PIC  X(12) VALUE "LPT1".
    03  W02-WS REDEFINES W02-PRINTER.
        05  W02-PROG    PIC  X(07).
        05  W02-PUSER   PIC  X(05).
    03  W02-CSHDRW      PIC X(04) VALUE "COM1".
    03  W02-CUSDSP      PIC X(04) VALUE "COM1".

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
        05  FILLER      PIC  X(01) VALUE SPACES.
               05  W15-DOCTYP  PIC  X(18) VALUE SPACES.
               05  W15-ECAN4   PIC  X(01).
           03  W15-L2.
      *  05  W15-ESC2 PIC  X(01).
      *  05  W15-DCAN PIC  X(01).
        05  FILLER      PIC  X(04) VALUE SPACES.
               05  W15-ADD     PIC  X(32) VALUE SPACES.
           03  W15-L3.
              05  FILLER      PIC  X(19) VALUE "Tel. No./Nr.     :".
               05  W15-TEL     PIC  X(18) VALUE SPACES.
           03  W15-L3A.
        05  FILLER      PIC  X(19) VALUE "VAT/BTW Reg. Nr. :".
               05  W15-GNO     PIC  X(14) VALUE SPACES.
          03  W15-L4.
              05  FILLER      PIC  X(19) VALUE "Date/Datum       :".
              05  W15-DATE    PIC  Z9/99/9999.
          03  W15-L4A.
              05  FILLER      PIC  X(19) VALUE "Time/Tyd         :".
              05  W15-TIME    PIC  X(08).
           03  W15-L5.
              05  FILLER      PIC  X(19) VALUE "Assist           :".
              05  W15-SMAN    PIC  X(21).
           03  W15-L6.
               05  FILLER      PIC  X(20) VALUE "Rec. No./Kwit. Nr.:".
               05  W15-RECNO   PIC  X(08).
           03  W15-L7.
               05  FILLER      PIC  X(20) VALUE "Inv. No./Fak. Nr. :".
               05  W15-INVNO   PIC  X(08).
           03  W15-L9.
      *  05  W15-ESC3 PIC  X(01).
      *  05  W15-DBLE2 PIC  X(01).
               05  W15-EXPP2   PIC  X(01).
              05  FILLER      PIC  X(18) VALUE "  THANK YOU/DANKIE".
               05  W15-ECAN2   PIC  X(01).
      *  05  W15-ESC4 PIC  X(01).
      *  05  W15-DCAN2 PIC  X(01).
          03  W15-L11.
        05  W15-DETAIL.
              07  W15-DESC    PIC  X(28).
              07  FILLER      PIC  X(01) VALUE SPACE.
              07  W15-PRICE   PIC  Z(06)9.99-.
          03  W15-L11A REDEFINES W15-L11.
              05  W15-ITM     PIC  X(14).
              05  FILLER      PIC  X(01).
              05  W15-QUANT   PIC  ZZZ9.99.
        05  FILLER      PIC  X(18).
          03  W15-L12 REDEFINES W15-L11.
               05  W15-DISCOUNT.
                   07  W15-DISC
                PIC  X(23).
                   07  W15-DSC PIC  Z9.99.
     07  W15-BLK PIC  X(01).
              05  FILLER      PIC  X(11).
    03  W15-L12A REDEFINES W15-L11.
        05  FILLER      PIC  X(15).
        05  W15-AS1     PIC  X(02).
        05  W15-CSHCODE PIC  X(06).
        05  W15-AS2     PIC  X(02).
        05  FILLER      PIC  X(15).
    03  W15-CUSDSP REDEFINES W15-L11.
        05  W15-CUSDESC.
     07  W15-HD1 PIC  X(10).
     07  W15-DUE PIC  Z(06)9.99.
        05  W15-CUS2.
     07  W15-HD2 PIC  X(10).
     07  W15-CHG PIC  Z(06)9.99.
           03  W15-L13.
              05  W15-REMK    PIC X(29).
               05  W15-VAL     PIC Z(06)9.99-.

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

       COPY W70DEBT.IWS.

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
           03  W75-SKEY       PIC 9(06) COMP-5 OCCURS 18.
           03  W75-ITEMS OCCURS 18.
               05  W75-CODE   PIC X(14).
               05  W75-SELL   PIC 9(07)V99 COMP-3.
               05  W75-VSELL  PIC 9(07)V99 COMP-3.
               05  W75-CASH   PIC 9(07)V99 COMP-3.
               05  W75-VCASH  PIC 9(07)V99 COMP-3.
               05  W75-WSALE  PIC 9(07)V99 COMP-3.
               05  W75-VWSALE PIC 9(07)V99 COMP-3.

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
    03  W85-CUSDISP     PIC  X(01).
    03  W85-CUSPORT     PIC  9(01).
    03  W85-KICK        PIC  X(05).
    03  W85-P-KICK REDEFINES W85-KICK.
        05  W85-KC1     PIC  X(01)    COMP-X.
        05  W85-KC2     PIC  X(01)    COMP-X.
        05  W85-KC3     PIC  X(01)    COMP-X.
        05  W85-KC4     PIC  X(01)    COMP-X.
        05  W85-KC5     PIC  X(01)    COMP-X.
    03  W85-KTCHPRN     PIC  X(01).
    03  W85-KCHPORT     PIC  9(01).
          03  FILLER        PIC  X(01).
    03  W85-USE-3       PIC  X(01).
    03  W85-SCALE-TYPE  PIC  X(01).
    03  W85-SCALE       PIC  X(01).
    03  W85-SCALPORT    PIC  9(01).
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
          03  W90-SPEED       PIC S9(07)V99 COMP-3.
          03  W90-VOUCH       PIC S9(07)V99 COMP-3.
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
          03  W95-COMP.
        05  W95-DSP-L1  PIC  X(20).
        05  W95-DSP-L2  PIC  X(20).
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
    03  W100-MEASURE.
        05  W100-LENGTH PIC  ZZZ9.9999.
        05  W100-BREADTH PIC ZZZ9.9999.
        05  W100-NUMBER PIC  ZZZ9.

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

       01  S05.
          03  LINE 12 COLUMN 12 VALUE "Enter Password".
       01  S06.
          03  LINE 12 COLUMN 28 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0 PIC  X(06)
     USING W40-CSHPASS AUTO SECURE.
       01  S07.
    03  LINE  1 COLUMN 64 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 0
     VALUE "Cash Drawer: ".
    03        COLUMN 77 FOREGROUND-COLOR 6 HIGHLIGHT
     BACKGROUND-COLOR 0
     PIC  Z9 FROM WS-DRAW.
           03  LINE  2 COLUMN 35 FOREGROUND-COLOR 15 VALUE "CASH SALE".
          03  LINE  3 COLUMN  2 VALUE "Salesperson".
    03        COLUMN 14 BACKGROUND-COLOR 5
     PIC  X(02) FROM WS-SPACE.
          03  LINE  4 COLUMN  2 VALUE "Ref. No".
    03        COLUMN 10 BACKGROUND-COLOR 0
     PIC  X(08) FROM WS-SPACE.
          03  LINE  5 COLUMN  2 REVERSE-VIDEO VALUE "Item Code      I".
          03        COLUMN 18 REVERSE-VIDEO VALUE "TM Description  ".
           03          COLUMN 33 REVERSE-VIDEO VALUE "                ".
           03          COLUMN 49 REVERSE-VIDEO VALUE "  Quantity ".
           03          COLUMN 60 REVERSE-VIDEO VALUE "Unit Price  ".
           03          COLUMN 72 REVERSE-VIDEO VALUE "Extended".
          03  LINE  6 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
          03  LINE 19 COLUMN  1 BACKGROUND-COLOR 0 VALUE "Æ".
          03        COLUMN  2 VALUE "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ
      -        "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍ".
    03        COLUMN 80 BACKGROUND-COLOR 0 VALUE "µ".
          03  LINE 20 COLUMN 3  VALUE "Sub Total ".
          03  LINE 21 COLUMN 3  VALUE "Discount  ".
          03  LINE 22 COLUMN 3  VALUE "T A X -VAT".
           03  LINE 23 COLUMN 3  VALUE "AMOUNT DUE".
       01  S09.
           03  LINE  1 COLUMN 50 VALUE "(".
           03          COLUMN 51 BLINK VALUE "REVERSAL".
           03          COLUMN 59 VALUE ")".

       01  S10.
           03  LINE 10 COLUMN 48 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 14
                                 PIC  X(24) USING W10-NAME AUTO.
           03  LINE 11 COLUMN 48 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 14
                                 PIC  X(14) USING W10-TEL AUTO.
           03  LINE 12 COLUMN 48 BACKGROUND-COLOR 7
                                 FOREGROUND-COLOR 14
                                 PIC  99/99/99 USING W22-DTE1 AUTO.

       01  S12.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
     PIC X(14) USING W10-ITEM AUTO.

       01  S12-3.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(03) USING W10-I4 AUTO.

       01  S12-4.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(04) USING W10-I4 AUTO.

       01  S12-5.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(05) USING W10-I5 AUTO.

       01  S12-6.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(06) USING W10-I6 AUTO.

       01  S12-7.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(07) USING W10-I7 AUTO.

       01  S12-8.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(08) USING W10-I8 AUTO.

       01  S12-9.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(09) USING W10-I9 AUTO.

       01  S12-10.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(10) USING W10-I10 AUTO.

       01  S12-11.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(11) USING W10-I11 AUTO.

       01  S12-12.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(12) USING W10-I12 AUTO.

       01  S12-13.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(13) USING W10-I13 AUTO.

       01  S12-14.
          03        COLUMN  2 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(14) USING W10-I14 AUTO.

       COPY STKLUP.CRT.

       01  S26-QNT.
          03        COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
                                 PIC Z(04)9.99 USING W10-QUANT AUTO.

       01  S26-WEIGHT.
          03        COLUMN 51 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0
           PIC Z(03)9.999 USING W10-QUANT AUTO.

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

       01  S26-CASH.
          03  LINE 15 COLUMN 63 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
           PIC Z(06)9.99- USING W90-CASH AUTO.

       01  S26-CHQ.
          03  LINE 16 COLUMN 63 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
           PIC Z(06)9.99- USING W90-CHQ AUTO.

       01  S26-CARD.
          03  LINE 17 COLUMN 63 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
           PIC Z(06)9.99- USING W90-CARD AUTO.

       01  S26-SPEED.
          03  LINE 18 COLUMN 63 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
           PIC Z(06)9.99- USING W90-SPEED AUTO.

       01  S26-VOUCH.
          03  LINE 19 COLUMN 63 FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 7
           PIC Z(06)9.99- USING W90-VOUCH AUTO.

       COPY S99.CRT.

       01  S-MEASURE.
    03  LINE  4 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "Ú".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14
     PIC X(19) USING WS-SRT-LINE.
    03        COLUMN 35 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "¿".
          03  LINE  5 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     PIC  X(04) FROM WS-SPACE.
    03        COLUMN 20 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14
     VALUE "MEASUREMENTS".
    03        COLUMN 32 BACKGROUND-COLOR 2
     PIC  X(03) FROM WS-SPACE.
          03        COLUMN 35 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
          03  LINE  6 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Length  >".
    03        COLUMN 34 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
          03        COLUMN 35 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
          03  LINE  7 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Breadth >".
    03        COLUMN 34 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
                                 VALUE "<".
          03        COLUMN 35 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
          03  LINE  8 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 6 HIGHLIGHT
     VALUE "Pieces  >    <".
          03        COLUMN 30 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14
     VALUE "     ³".
    03  LINE  9 COLUMN 15 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "À".
    03        COLUMN 16 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14
     PIC X(19) USING WS-SRT-LINE.
    03        COLUMN 35 BACKGROUND-COLOR 2
     FOREGROUND-COLOR 14 VALUE "Ù".

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

       COPY ERROR.CRT.

       COPY OPT.CRT.

      /
       PROCEDURE DIVISION
   USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY.
       AA000    SECTION.
       AA000-INIT.
          IF LS0-SALEV < 1
        MOVE "Not Authorised"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO AA49.
            PERFORM ZA000-INIT.
      PERFORM DA000.
    IF W85-CUSDISP = "Y"
        CLOSE CUSDSP.
      CLOSE CSHSLE
     CHEQUE
     RECOVER
     SALE.
       AA49.
             EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50.
            PERFORM SAVE-SCREEN.
             MOVE 4              TO W44-FUNCTION.
            PERFORM SCREEN-CONTENTS.
            PERFORM STOCK-LOOKUP.
            PERFORM RESTORE-SCREEN.
            MOVE "S" TO WS-LOOK.
       AA99.
             EXIT.

       COPY FUNCTION.CRT.

       COPY OPTION.CRT.

       COPY LOCKED.RC2.

       COPY CLEAR.CRT.

       COPY DATE.CHK.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       AA300              SECTION.
       AA301.
            DISPLAY "F1" AT 2502
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "F2" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             "=Item/"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F3" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             "=Alt/"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F4" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             "=Desc/"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F5" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             "=X/Ref Lookup,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F6" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             "=Order,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F7" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Prompts,"
             "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "ape"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1.
       AA305.
             EXIT.

      /
      *    ****    READ FILES ROUTINES
      *
       AC000-READ              SECTION.

       COPY AUDIT.RD.

       COPY CHEQUE.RD.

       COPY CONTROL.RD.

       COPY DEPART.RD.

       COPY LEDTRF.RD.

       COPY PARAM.RD.

       COPY SALE.RD.

       COPY SHARED.RD.

       COPY STOCK.RD.

       COPY STKMEM.RD.

       COPY TXTRAN.RD.

      /
       AD000-WRITE             SECTION.

       COPY AUDIT.WR.

       COPY CARDEX.WR.

       COPY CHEQUE.WR.

       COPY CONTROL.WR.

       COPY DEPART.WR.

       COPY LEDTRF.WR.

       COPY PARAM.WR.

       COPY SALE.WR.

       COPY SHARED.WR.

       COPY STOCK.WR.

       COPY TXTRAN.WR.

       COPY CASHDRAW.OPN.

       COPY STOCK.LUP.

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
       AY05.
             MOVE 05             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE ZERO           TO REC-TYPE.
            PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             MOVE STK-RECORD1    TO REC-STOCKF.
             GO TO AY50.
       AY16.
             MOVE 16             TO REC-FILE.
             MOVE WS-AUDKEY      TO REC-KEY.
             MOVE AUD-REC1       TO REC-AUDITF.
             GO TO AY50.
       AY19.
             MOVE 19             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE TAX-RECORD1    TO REC-TXTRAN.
             GO TO AY50.
       AY36.
             MOVE 36             TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE.
             MOVE CHQ-RECORD1    TO REC-CHEQUE.
             GO TO AY50.
       AY37.
            MOVE 37   TO REC-FILE.
            MOVE WS-SHARED  TO REC-KEY.
            MOVE SHR-RECORD  TO REC-NETWORK.
             GO TO AY50.
       AY38.
             MOVE 38             TO REC-FILE.
             MOVE WS-DEPKEY      TO REC-KEY.
            PERFORM READ-DEPART THRU READ-DEPART-EXIT.
             MOVE DPT-RECORD     TO REC-DEPART.
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
       AY43.
            MOVE 43   TO REC-FILE.
             MOVE ZERO           TO REC-KEY.
             MOVE WS-ACTION      TO REC-TYPE
            MOVE CRD-REC1  TO REC-CARDEX.
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
       BB60.
      PERFORM RESTORE-SCREEN-2.
       BB69.
             EXIT.

      /
       BG00-TRAN               SECTION.
       BG80.
             MOVE W70-AUDIT      TO WS-AUDKEY.
             MOVE LOW-VALUES     TO AUD-REC1.
             PERFORM AY16 THRU AY59.
             MOVE STK-CODE       TO AUD-CODE.
             MOVE 2              TO AUD-TYPE.
             MOVE WS-DRAW        TO AUD-SUB.
             PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
             ADD 1               TO W70-AUDIT.
             MOVE 1              TO WS-AUDKEY.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
             PERFORM AY16 THRU AY59.
             MOVE 1              TO AUD-REP2.
             PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
       BG85.
             EXIT.

       COPY CA000.STK.

       COPY CA100.STK.

       COPY CA200.STK.

       COPY CA500.STK.

       COPY LEDTRF.UPD.

      /
       DA000         SECTION 50.
       DA00.
      *
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW         TO W85-PASS.
            ADD 150 WS-DRAW  GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-RECORD151  TO W85-RECORD151.

       DA05.
    IF W85-CUSDISP = "Y"
      *        MOVE X"1B"  TO CUS-CON
      *        MOVE "QA"  TO CUS-INST
        MOVE W95-COMP  TO CUS-LINE
        WRITE CUS-REC.
      *        MOVE "QB"  TO CUS-INST
      *        MOVE W95-DSP-L2  TO CUS-LINE
      *        WRITE CUS-REC.
            PERFORM ERASE-SCREEN.
            DISPLAY S07.
            MOVE 7   TO LIN.
             PERFORM DA48 UNTIL LIN > 18.
            MOVE ZERO   TO W90-BDT W90-NONTAX W90-LDISC
                                    W90-BAL W90-TAXVAL W90-SALES
                                    W90-CUR W90-EXCTOT W10-SMAN.
            MOVE 1   TO WS-FIRST WS-SALPOS WS-LAST.
             MOVE SPACES         TO W90-TAX.
             MOVE SPACES         TO CSH-DATA.

       DA06.
      DISPLAY "F1" AT 2502
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Help,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F7" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "=Prompts,"
       "F8" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             "=Refund,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
             "F9" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
             "=Petty Cash,"
        WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
       "Esc" WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
       "ape to EXIT"
        WITH FOREGROUND-COLOR 1 BACKGROUND-COLOR 3.
            ACCEPT W10-SMAN AT 0314
            WITH FOREGROUND-COLOR 7 HIGHLIGHT
    BACKGROUND-COLOR 5 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE "E"  TO LS-INSTR
       GO TO DA150
   WHEN F1-KEY
       PERFORM HELP-ROUTINE
       GO TO DA06
   WHEN F7-KEY
       PERFORM DA300P
       GO TO DA06
   WHEN F8-KEY
       MOVE "R"  TO LS-INSTR
       GO TO DA150
   WHEN F9-KEY
       MOVE "P"  TO LS-INSTR
       GO TO DA150
   WHEN OTHER
       CALL X"E5"
       GO TO DA06
        END-EVALUATE.
    IF W10-SMAN = 99
        GO TO DA150.
           IF W10-SMAN > 50
               GO TO DA06.
      PERFORM CLEAR-L50.
           IF W10-SMAN = ZERO
               GO TO DA07.
            ADD 100 W10-SMAN  GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-SALESMAN = SPACES
               GO TO DA06.
            DISPLAY PAR-SALESMAN AT 0317
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 5.

       DA07.
            MOVE PAR-SALESMAN  TO W15-SMAN.
            PERFORM CLEAR-L50.

       DA10.
      MOVE "R"   TO WS-OPTION.
    IF WS-PROMPT = "Y"
              MOVE "'R'etail,'C'ash or 'W'holesale price  [R]"
     TO WS-ERR-MES
        PERFORM OPT-MESSAGE.
      MOVE WS-OPTION  TO WS-PRC.
          IF NOT (WS-PRC = "C" OR "R" OR "W")
              GO TO DA10.
    IF WS-PRC = "W"
              DISPLAY "Enter password" AT 2551
         WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
              ACCEPT W40-CSHPASS AT 2567
        WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
            AUTO NO-ECHO
              MOVE 6   TO WS-LENGTH
        CALL "CBL_TOUPPER" USING W40-CSHPASS
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
              MOVE 1   TO WS-LENGTH
        IF NOT (W40-CSHPASS = W85-SUPER (1) OR
         W85-SUPER (9) OR "SELOHW")
     GO TO DA10.
            PERFORM CLEAR-L50.

       DA20.
            MOVE 7   TO LIN.
            MOVE ZERO   TO W90-DB.

       DA25.
             MOVE SPACES         TO W10-DESC W10-ITEM.
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
            MOVE ZERO   TO W10-SELL WS-SUPER.
             MOVE 1              TO W10-QUANT.
       DA30.
             PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
    IF W10-STCK = "A"
              ACCEPT S12 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 3
        ACCEPT S12-3 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 4
        ACCEPT S12-4 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 5
        ACCEPT S12-5 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 6
        ACCEPT S12-6 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 7
        ACCEPT S12-7 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 8
        ACCEPT S12-8 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 9
        ACCEPT S12-9 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 10
        ACCEPT S12-10 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 11
        ACCEPT S12-11 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 12
        ACCEPT S12-12 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 13
        ACCEPT S12-13 AT LINE LIN
    ELSE
    IF W10-SLNGTH = 14
        ACCEPT S12-14 AT LINE LIN.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             DISPLAY (LIN, 2) WS-MIV78
         WITH FOREGROUND-COLOR 3
       BACKGROUND-COLOR 1
       MOVE SPACES TO W10-ITEM
       SUBTRACT 1  FROM WS-LAST
       GO TO DA49
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
   WHEN F6-KEY
       PERFORM YA000
       GO TO DA30
   WHEN F7-KEY
       PERFORM DA300P
       GO TO DA30
   WHEN OTHER
       CALL X"E5"
       GO TO DA30
        END-EVALUATE
              PERFORM AA50
              DISPLAY (LIN, 2) W10-ITEM
   WITH FOREGROUND-COLOR 7 HIGHLIGHT
        BACKGROUND-COLOR 0
               IF W10-ITEM = SPACES
           GO TO DA30.
          IF W10-ITEM = SPACES
               SUBTRACT 1        FROM WS-LAST
              GO TO DA49.
       DA31.
             PERFORM CA000.
           IF WS-F-ERROR = 22
        MOVE "Z"   TO WS-ETYPE
              PERFORM AA50
              DISPLAY (LIN, 2) W10-ITEM
   WITH FOREGROUND-COLOR 7 HIGHLIGHT
        BACKGROUND-COLOR 0
               IF W10-ITEM = SPACES
                   GO TO DA30
               ELSE
                   GO TO DA31.
    IF STK-IND = 4
        MOVE "Header Record"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO DA30.
            MOVE WS-LAST  TO WS-SALKEY.
             INITIALIZE SAL-RECORD1.
            MOVE WS-LAST  TO SAL-CODE.
      *
      *    ****    I T E M   N U M B E R   O N   C A S H   S L I P
      *
            DISPLAY (LIN, 17) "   "
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
            MOVE STK-CODE  TO SAL-ITEM.
            PERFORM CLEAR-L50.
      *
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *
          IF NOT (STK-IND = 3)
              MOVE STK-DESC  TO W10-DESC
              DISPLAY (LIN, 21) W10-SDSC
   WITH FOREGROUND-COLOR 3 HIGHLIGHT
        BACKGROUND-COLOR 0
        GO TO DA34.
       DA33.
            ACCEPT (LIN, 21) W10-SDSC
             WITH FOREGROUND-COLOR 7 HIGHLIGHT
     BACKGROUND-COLOR 0 UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO DA30
   WHEN OTHER
       CALL X"E5"
       GO TO DA33
        END-EVALUATE.
       DA33A.
            MOVE ZERO   TO STK-AVGE.
            DISPLAY "Index Unit cost" AT 2512
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
       DA33B.
            ACCEPT S26-AVGE AT LINE 25
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO DA33
   WHEN OTHER
       CALL X"E5"
       GO TO DA33B
        END-EVALUATE.
          IF STK-AVGE = ZERO
              MOVE "Unit cost is zero"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
        GO TO DA33B.
            PERFORM CLEAR-L50.
       DA34.
      *
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *
    IF W85-CUSDISP = "Y"
      *        MOVE X"1B"  TO CUS-CON
      *        MOVE "QA"  TO CUS-INST
        MOVE W10-DESC  TO W15-CUSDESC
        MOVE W15-CUSDESC  TO CUS-LINE
        WRITE CUS-REC.
      *        MOVE "QB"  TO CUS-INST
      *        MOVE SPACES  TO CUS-LINE
      *        WRITE CUS-REC.
            MOVE W10-ITEM  TO SAL-ITEM.
            MOVE W10-DESC  TO SAL-DESC.
            MOVE STK-AVGE  TO SAL-COST W10-COST.
      *
      *    ****    G E T   Q U A N T I T Y
      *
      MOVE ZERO   TO W10-LENGTH W10-BREADTH W10-NUMBER.
    IF WS-PROMPT = "F"
        IF W85-USE-3 = "N"
           DISPLAY S26-QNT AT LINE LIN
        ELSE
           DISPLAY S26-WEIGHT AT LINE LIN
        END-IF
        GO TO DA35-FAST.

       DA35.
      DISPLAY "F6" AT 2502
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 6 HIGHLIGHT
       " to key in measurements "
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 1
       "- Cutting size (Wood, Glass etc)"
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 7 HIGHLIGHT.
    IF W85-USE-3 = "Y"
              ACCEPT S26-WEIGHT AT LINE LIN
    ELSE
              ACCEPT S26-QNT AT LINE LIN.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             DISPLAY (LIN, 2) WS-MIV78 WITH FOREGROUND-COLOR 3
             GO TO DA25
   WHEN F6-KEY
       PERFORM BB050
             DISPLAY S26-WEIGHT AT LINE LIN
   END-EVALUATE.
    IF (W10-QUANT = ZERO)
              DISPLAY (LIN, 2) WS-MIV78 WITH FOREGROUND-COLOR 3
              GO TO DA25.

       DA35-FAST.
            PERFORM CLEAR-L50.
      *
      *    ****    G E T   S E L L I N G   P R I C E   (ACCOUNT OR CASH)
      *
           IF STK-IND < 3
               IF W90-TAX NOT = SPACES
                   IF WS-PRC = "R"
                       MOVE STK-SELL
                                 TO W10-SELL
                   ELSE
                   IF WS-PRC = "C"
                       MOVE STK-CASH
                                 TO W10-SELL
                   ELSE
                       MOVE STK-WSALE
                                 TO W10-SELL
               ELSE
                   IF WS-PRC = "R"
                       MOVE W10-VSELL
                                 TO W10-SELL
                   ELSE
           IF WS-PRC = "C"
                       MOVE W10-VCASH
                                 TO W10-SELL
                   ELSE
                       MOVE W10-VWSALE
                                 TO W10-SELL.
    IF WS-PROMPT = "F"
              DISPLAY S26-PRC AT LINE LIN
        GO TO DA35A-FAST.

       DA35A.
             ACCEPT S26-PRC AT LINE LIN.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO DA35
        END-EVALUATE.
            MOVE W10-SELL  TO W10-SPAPRC.
             PERFORM DA110 THRU DA120.
           IF WS-ERROR = 1
              MOVE ZERO  TO WS-ERROR
               GO TO DA35A.

       DA35A-FAST.
    IF W85-CUSDISP = "Y"
      *        MOVE X"1B"  TO CUS-CON
      *        MOVE "QA"  TO CUS-INST
        MOVE "   PRICE-"  TO W15-HD2
        MOVE W10-SELL  TO W15-CHG
        MOVE W15-CUSDSP  TO CUS-LINE
        WRITE CUS-REC.
      *        MOVE "QB"  TO CUS-INST
      *        MOVE W15-CUS2  TO CUS-LINE
      *        WRITE CUS-REC.

       DA35B.
            MOVE W10-SELL  TO SAL-SELL.
    IF WS-PRC-IND = "Y"
        IF (STK-IND > 1) OR (W10-QUANT < ZERO)
     GO TO DA40
        END-IF
    ELSE
          IF (STK-IND > 0) OR (W10-QUANT < ZERO)
              GO TO DA40.
           IF STK-QUANT = ZERO
              DISPLAY "No stock - Continue (Y/N) > <" AT 2512
         WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14
              DISPLAY "'Y' will make stock holding negative" AT 2542
         WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 1
              ACCEPT (25, 39) WS-OPTION
        WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15 AUTO
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
              PERFORM CLEAR-L50
               IF WS-OPTION = "N"
           MOVE W10-QUANT
     TO W10-REORD
                   GO TO DA25.
          IF W10-QUANT NOT > STK-QUANT
               GO TO DA40.
      DISPLAY "Negative Stock" AT 2502
        WITH BACKGROUND-COLOR 3
      FOREGROUND-COLOR 7 HIGHLIGHT.
            DISPLAY "Enter password" AT 2551
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
            ACCEPT W40-CSHPASS AT 2567
      WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
          AUTO NO-ECHO.
            MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-CSHPASS
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            MOVE 1   TO WS-LENGTH.
      IF NOT (W40-CSHPASS = W85-SUPER (1) OR
       W85-SUPER (9) OR "YTNAUQ")
   GO TO DA25.

       DA40.
      *
      *    ****    C A L C U L A T E   E X T E N D E D   V A L U E
      *
            MOVE W10-QUANT  TO SAL-QNT.
             COMPUTE W25-VALUE ROUNDED = (1.000 * W10-SELL * W10-QUANT).
            MOVE W25-VALUE  TO W15-PRICE SAL-VAL W100-EXT.
            DISPLAY (LIN, 70) W100-EXT
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
      *
      *    ****    L I N E   D I S C O U N T   ( PERCENTAGE )
      *
            MOVE ZERO   TO WS-DISC W90-CDISC.
    IF WS-PROMPT = "F"
        GO TO DA45.
            DISPLAY (25, 18) "Discount %"
       WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.

       DA42.
             ACCEPT S26-DSC AT LINE 25.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             PERFORM CLEAR-L50
       GO TO DA35A
        END-EVALUATE.
          IF WS-DISC = ZERO
               GO TO DA45.
             COMPUTE W10-SPAPRC ROUNDED = W10-SELL -
            (W10-SELL * WS-DISC / 100.00000).
      *
      *    ****    C H E C K   M A R G I N
      *
             PERFORM DA110 THRU DA120.
           IF WS-ERROR = 1
               GO TO DA42.
      PERFORM CLEAR-L50.
            MOVE WS-DISC  TO SAL-DSC.
            COMPUTE W90-CDISC = ((W25-VALUE * WS-DISC) / 100).
            MOVE W90-CDISC  TO W15-PRICE SAL-DISC.
            DISPLAY (25, 40) W15-PRICE
       WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15.
      *
      *    ****    A C C U M U L A T E   TOTAL COST / TOTAL DISCOUNT /
      *            VALUES SUBJECT TO TAX AND NON-TAX.
      *
       DA45.
          IF (STK-IND < 2) OR (STK-IND = 3)
               COMPUTE W90-BAL = W90-BAL + (W10-QUANT * W10-COST).
            ADD W90-CDISC  TO W90-LDISC.
            SUBTRACT W90-CDISC FROM W25-VALUE
     GIVING W90-VALUE.
           IF STK-TAX = 0
               ADD W90-VALUE     TO W90-NONTAX
               MOVE W90-VALUE    TO SAL-NONTAX
           ELSE
               MOVE W90-VALUE    TO SAL-TAXVAL
               ADD W90-VALUE     TO W90-TAXVAL.
           IF W90-TAX NOT = SPACES
               COMPUTE SAL-VAT ROUNDED =
                       SAL-TAXVAL * W05-RATE / 100.
      *
      *    ****    A C C U M U L A T E   T O T A L   S A L E
      *
            ADD W25-VALUE  TO W90-DB.
       DA45B.
            MOVE W90-DB  TO W90-TOTAL.
            SUBTRACT W90-LDISC  FROM W90-TOTAL.
            MOVE W90-DB  TO W15-PRICE.
            DISPLAY (20, 15) W15-PRICE
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 5.
            MULTIPLY -1 BY W90-LDISC
     GIVING W15-PRICE.
            DISPLAY (21, 15) W15-PRICE
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 5.
       DA45-AA.
            MOVE W90-TOTAL  TO W15-VAL.
            DISPLAY (23, 15) W15-VAL
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 5.
    IF W85-CUSDISP = "Y"
        MOVE "AMT DUE:-"  TO W15-HD1
        MOVE W90-TOTAL  TO W15-DUE
        MOVE W15-CUSDESC  TO CUS-LINE
        WRITE CUS-REC.

       DA45-AB.
    IF WS-PROMPT = "F"
        GO TO DA46.
      *
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E
      *            A C C E P T E D.
      *
       DA45C.
            MOVE "'A'ccept or 'C'ancel   [A]"
     TO WS-ERR-MES.
            MOVE "A"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "A" OR "C")
              GO TO DA45C.
           IF WS-OPTION = "C"
              DISPLAY (LIN, 2) WS-MIV78 WITH FOREGROUND-COLOR 3
               PERFORM DA47
               PERFORM DA45B THRU DA45-AA
               GO TO DA25.

       DA46.
             MOVE WS-SUPER       TO SAL-SUPER.
            PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
            ADD 1   TO WS-LAST.
      *
      *    ****    T E S T   F O R   C U T T I N G   S P E C S
      *
    IF W10-LENGTH > ZERO AND
       W10-BREADTH > ZERO AND
       W10-NUMBER > ZERO
             IF LIN < 18
          ADD 1   TO LIN
       ELSE
          MOVE 7  TO WS-S7
          PERFORM DA46-SHIFT
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
      *        DISPLAY (LIN, 17) "   "
      *   WITH FOREGROUND-COLOR 3 HIGHLIGHT
      *        BACKGROUND-COLOR 0
            ADD 1   TO WS-LAST.
           IF LIN < 18
              ADD 1   TO LIN
               GO TO DA25.
      PERFORM SAVE-SCREEN-2.
      MOVE 7   TO WS-S7.

       DA46-SHIFT.
      ADD 1 WS-S7  GIVING WS-S8.
      MOVE W42-LINES (WS-S8)
     TO W42-LINES (WS-S7).
      MOVE W43-LINE (WS-S8)
     TO W43-LINE (WS-S7).
    IF WS-S8 < 18
        ADD 1   TO WS-S7
        GO TO DA46-SHIFT.
      PERFORM RESTORE-SCREEN-2.
            DISPLAY (LIN, 1) WS-MIV1 WITH BACKGROUND-COLOR 0
        FOREGROUND-COLOR 3
         WS-MIV78 WITH FOREGROUND-COLOR 3
         WS-MIV80 WITH BACKGROUND-COLOR 0
         FOREGROUND-COLOR 3.
      ADD 1   TO WS-SALPOS.

       DA46-EXIT.
            GO TO DA25.

       DA47.
            SUBTRACT W25-VALUE  FROM W90-DB.
            SUBTRACT W90-CDISC  FROM W90-LDISC.
             SUBTRACT SAL-NONTAX FROM W90-NONTAX.
             SUBTRACT SAL-TAXVAL FROM W90-TAXVAL.
           IF (STK-IND < 2) OR (STK-IND = 3)
               COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).

       DA48.
            DISPLAY (LIN, 1) WS-MIV1 WITH BACKGROUND-COLOR 0
        FOREGROUND-COLOR 3
         WS-MIV78 WITH FOREGROUND-COLOR 3
         WS-MIV80 WITH BACKGROUND-COLOR 0
         FOREGROUND-COLOR 3.
            ADD 1   TO LIN.

       DA49.
           IF WS-LAST = ZERO
               GO TO DA05.
      *
      *    ****    P R O C E S S   T H E   P A Y M E N T
      *
    IF WS-PROMPT = "F"
        GO TO DA55.

       DA50.
            MOVE "'P'rocess, 'A'mend, 'C'ancel or 'M'ore items"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "P" OR "A" OR "C" OR "M")
              GO TO DA50.
           IF WS-OPTION = "M"
               ADD 1             TO WS-LAST
               GO TO DA25.
           IF WS-OPTION = "A"
              PERFORM DA200A
               GO TO DA50.
           IF WS-OPTION = "C"
               GO TO DA05.

       DA55.
            MOVE ZERO   TO WS-TDISC W90-TDISC.
           IF W90-LDISC NOT = ZERO
              MOVE W90-LDISC  TO W90-TDISC
               GO TO DA60.
            MOVE W90-DB  TO W90-DVAL.

       DA60.
             SUBTRACT W90-TDISC FROM W90-DB GIVING W90-TSELL W90-TOTAL.
             MULTIPLY -1 BY W90-TDISC GIVING W15-VAL.
            DISPLAY (21, 15) W15-VAL
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 5.
            MOVE ZERO   TO W90-TTAX W90-ATAX.
      *
      *    ****    VERSION 6.62 - ALLOW FOR SPEED-POINT
      *    - USE WINDOW FOR PAYMENT
      *
      PERFORM SAVE-SCREEN-2.

       DA62.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" AT 1043
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
       "¿"
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1143
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
       " P A Y M E N T   D E T A I L S ³"
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1243
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
       " AMOUNT DUE                    ³"
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1343
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
       " Balance                       ³"
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1443
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
       "                               ³"
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1543
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
       " Cash Received                 ³"
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1643
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
       " Cheque Received               ³"
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1743
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
       " Credit Card Slip              ³"
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1843
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
       " Speed Point Slip              ³"
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7.
      DISPLAY "³" AT 1943
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
       " Voucher (Gift)                ³"
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7.
      DISPLAY "³" AT 2043
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
       "                               ³"
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7.
      DISPLAY "³" AT 2143
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
       " C H A N G E       ³"
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7.
      DISPLAY "À" AT 2243
        WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7
       "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 7.
      MOVE 11   TO SHADE-ROW.
      MOVE 45   TO SHADE-COL.
      MOVE 31   TO SHADE-WIDTH.
      MOVE 12   TO SHADE-LINES.
      PERFORM SCREEN-SHADOW.

       DA65.
            MOVE ZERO   TO W90-DUE W90-CASH W90-VOUCH
              W90-PMT W90-CARD W90-CHANGE
        W90-CHQ W90-SPEED.
            MOVE W90-TOTAL  TO W15-VAL W90-DUE.
            DISPLAY (23, 15) W15-VAL
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 5.
            DISPLAY W15-VAL AT 1263
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 7.
      *
      *    ****    C A L C U L A T E   U N P A I D   B A L A N C E
      *
       DA71.
             COMPUTE W90-CRD = W90-DUE - W90-PMT.
            MOVE W90-CRD  TO W100-BAL.
            DISPLAY W100-BAL AT 1363
       WITH FOREGROUND-COLOR 3 HIGHLIGHT
     BACKGROUND-COLOR 7.
       DA72.
      *
      *    ****    C A S H   V A L U E
      *
             ACCEPT S26-CASH.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       PERFORM RESTORE-SCREEN-2
       GO TO DA50
        END-EVALUATE.
            MOVE W90-CASH  TO W90-PMT.
             PERFORM DA71.
           IF W90-PMT NOT < W90-DUE
               GO TO DA74.
       DA73.
      *
      *    ****    C H E Q U E   V A L U E
      *
             ACCEPT S26-CHQ.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       PERFORM RESTORE-SCREEN-2
       GO TO DA50
        END-EVALUATE.
            ADD W90-CHQ  TO W90-PMT.
             PERFORM DA71.
           IF W90-PMT NOT < W90-DUE
               GO TO DA74.
      *
      *    ****    C R E D I T   C A R D   V A L U E
      *
             ACCEPT S26-CARD.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       PERFORM RESTORE-SCREEN-2
       GO TO DA50
        END-EVALUATE.
            ADD W90-CARD  TO W90-PMT.
             PERFORM DA71.
          IF W90-PMT NOT < W90-DUE
               GO TO DA74.
      *
      *    ****    S P E E D   P O I N T   V A L U E
      *
            ACCEPT S26-SPEED.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       PERFORM RESTORE-SCREEN-2
       GO TO DA50
        END-EVALUATE.
            ADD W90-SPEED  TO W90-PMT.
             PERFORM DA71.
          IF W90-PMT NOT < W90-DUE
               GO TO DA74.
      *
      *    ****    V O U C H E R   V A L U E
      *
            ACCEPT S26-VOUCH.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       PERFORM RESTORE-SCREEN-2
       GO TO DA50
        END-EVALUATE.
            ADD W90-VOUCH  TO W90-PMT.
             PERFORM DA71.
      *
      *    ****    C H E C K   F O R   S H O R T   P A Y M E N T
      *
           IF W90-PMT < W90-DUE
               GO TO DA76.
       DA74.
      *
      *    ****    A R E   P A Y M E N T   D E T A I L S   C O R R E C T
      *
            MOVE "'C'orrect or 'R'ekey payment"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
       DA75.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "C" OR "R")
              GO TO DA74.
           IF WS-OPTION = "R"
               GO TO DA65.
    IF NOT (WS-BNK-IND = "Y")
        GO TO DA75B.
           IF W90-CHQ = ZERO
               GO TO DA75B.
      PERFORM SAVE-SCREEN-3.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
        AT 0933 WITH REVERSE-VIDEO.
      DISPLAY "³Name                                  ³"
                      AT 1033 WITH REVERSE-VIDEO.
      DISPLAY "³Telephone No.                         ³"
                      AT 1133 WITH REVERSE-VIDEO.
      DISPLAY "³Cheque Date                           ³"
                      AT 1233 WITH REVERSE-VIDEO.
      DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
        AT 1333 WITH REVERSE-VIDEO.
      MOVE 10   TO SHADE-ROW.
      MOVE 35   TO SHADE-COL.
      MOVE 38   TO SHADE-WIDTH.
      MOVE 4   TO SHADE-LINES.
      PERFORM SCREEN-SHADOW.
      MOVE SPACES  TO W10-NAME W10-TEL.
             MOVE W12-TODAY      TO W22-DTE1.
       DA75A.
             ACCEPT S10.
      PERFORM CLEAR-L50.
           IF W10-NAME = SPACES OR
              W10-TEL = SPACES OR
              W22-DTE1 = ZERO
        MOVE "Require name, telephone and date"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO DA75A.
             MOVE W22-DTE1       TO W22-DTE3
                                    W10-EDTE.
             PERFORM CHECK-DATE.
             MOVE W20-DTE        TO W22-DTE1.
             MOVE W22-DTE3       TO W20-DTE.
           IF WS-ERROR = 1
        MOVE "Invalid date"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO DA75A.
      PERFORM RESTORE-SCREEN-3.
       DA75B.
             COMPUTE W90-CHANGE = W90-PMT - W90-DUE.
            MOVE W90-CHANGE  TO W15-VAL.
            DISPLAY W15-VAL AT 2163
       WITH FOREGROUND-COLOR 0
     BACKGROUND-COLOR 7.
    IF W85-CUSDISP = "Y"
      *        MOVE X"1B"  TO CUS-CON
      *        MOVE "QA"  TO CUS-INST
        MOVE " CHANGE:-"  TO W15-HD2
        MOVE W90-CHANGE  TO W15-CHG
        MOVE W15-CUSDSP  TO CUS-LINE
        WRITE CUS-REC.
      *        MOVE "QB"  TO CUS-INST
      *        MOVE W15-CUS2  TO CUS-LINE
      *        WRITE CUS-REC.
            GO TO DA79.
       DA76.
            MOVE "Short - REKEY Payment"
     TO WS-ERR-STRING.
            PERFORM ERROR-MESSAGE.
            MOVE 20   TO LIN.
       DA78.
            GO TO DA62.

       DA79.
      MOVE WS-SLIP-P  TO WS-PRNINV.
    IF WS-PROMPT = "F"
        GO TO DA79-FAST.

       DA79-LOOP.
      MOVE WS-PRNINV  TO WS-OPTION.
      MOVE SPACES  TO WS-ERR-MES
      STRING "Print Invoice Y/N  ["
      DELIMITED SIZE
      WS-OPTION DELIMITED SIZE
      "]" DELIMITED SIZE
        INTO WS-ERR-MES.
      PERFORM OPT-MESSAGE.
      MOVE WS-OPTION  TO WS-PRNINV.
    IF NOT (WS-PRNINV = "Y" OR "N")
        GO TO DA79-LOOP.

       DA79-FAST.
      *
      *    ****    P R O C E S S   T H E   S A L E
      *
      PERFORM CLEAR-L50.
             DISPLAY "Processing Sale" AT 2520 WITH
              BACKGROUND-COLOR 3 FOREGROUND-COLOR 14 BLINK.
             MOVE 1              TO WS-SALKEY.
             PERFORM AY60 THRU AY999.
             MOVE W70-INVOICE    TO W90-REF W15-RECNO.
           IF W70-INVOICE = 99999999
               MOVE 1            TO W70-INVOICE
           ELSE
               ADD 1             TO W70-INVOICE.
            DISPLAY W90-REF AT 0410
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 0.
             PERFORM OPEN-CASH-DRAWER THRU OPEN-CASH-DRAWER-EXIT.
    IF WS-PRNINV = "Y"
        MOVE W85-SI  TO CSH-DATA
        WRITE CSH-L.
            MOVE "CASH SALE/KONTANT"
     TO W15-DOCTYP.
           IF WS-PRNINV = "N"
               GO TO DA80.
             MOVE W15-L1A        TO CSH-DATA.
             WRITE CSH-L.
             MOVE W15-L1         TO CSH-DATA.
             WRITE CSH-L.
             MOVE W15-L2         TO CSH-DATA.
             WRITE CSH-L.
             MOVE W15-L3         TO CSH-DATA.
             WRITE CSH-L.
           IF W05-VATNO NOT = SPACES
               MOVE W05-VATNO    TO W15-GNO
               MOVE W15-L3A      TO CSH-DATA
               WRITE CSH-L.
             MOVE W15-L4         TO CSH-DATA.
             WRITE CSH-L.
            ACCEPT W25-TIME FROM TIME.
             MOVE W25-HH         TO W95-HH.
             MOVE W25-MM         TO W95-MM.
             MOVE W25-SS         TO W95-SS.
      MOVE W95-TIME  TO W15-TIME.
            MOVE W15-L4A  TO CSH-DATA.
             WRITE CSH-L.
          IF W10-SMAN NOT = ZERO
               MOVE W15-L5       TO CSH-DATA
               WRITE CSH-L.
             MOVE W15-L6         TO CSH-DATA.
             WRITE CSH-L.
            MOVE ALL "-"  TO CSH-LINE.
             WRITE CSH-L.
       DA80.
      *
      *    ****    R E A D   T H E   S A L E   F I L E   A N D
      *            P R I N T   T H E   C A S H   S L I P
      *
           IF WS-SALKEY > WS-LAST
               GO TO DA95.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
           IF SAL-ITEM = SPACES
        MOVE SPACES  TO W15-L13
              MOVE SAL-DESC  TO W15-REMK
        IF WS-PRNINV = "Y"
           MOVE W15-L13  TO CSH-DATA
           WRITE CSH-L
        END-IF
        GO TO DA90.
             MOVE SAL-ITEM       TO STK-CODE.
             PERFORM AY05 THRU AY59.
      MOVE SPACES  TO W15-DETAIL.
      MOVE "CASH"  TO CRD-AC.
      MOVE ZERO   TO CRD-SEQ.
            MOVE W10-SMAN  TO CRD-SMAN.
            MOVE W90-REF  TO CRD-REF.
      MOVE STK-SUPP  TO CRD-SUPP.
      MOVE W12-T-YMD  TO CRD-DTE.
      MOVE "CASH SALE"  TO CRD-NAME.
      MOVE SPACES  TO CRD-WHS.
            MOVE SAL-ITEM  TO W15-ITM CRD-CODE.
      MOVE STK-LDG  TO CRD-DEPT.
            MOVE SAL-QNT  TO W10-QUANT W15-QUANT CRD-QNT.
    IF WS-PRNINV = "Y"
              MOVE W15-L11A  TO CSH-DATA
               WRITE CSH-L.
      MOVE SPACES  TO W15-DETAIL.
             MOVE SAL-DESC       TO W15-DESC.
             MOVE SAL-VAL        TO W15-PRICE W25-VALUE.
            MOVE SAL-COST  TO W10-COST CRD-COST.
            MOVE SAL-SELL  TO W10-SELL.
             MOVE SAL-DISC       TO W90-CDISC.
             MOVE SAL-DSC        TO WS-DISC.
            SUBTRACT W90-CDISC FROM W25-VALUE GIVING W90-VALUE.
             MOVE SAL-SUPER      TO WS-SUPER.
    IF W90-TAX = SPACES
               IF STK-TAX NOT = ZERO
                   MOVE W05-VAT (STK-TAX)
                                 TO W05-RATE
                   COMPUTE W90-VALUE ROUNDED = W90-VALUE -
                       (((W90-VALUE / (W05-RATE + 100.00000)) *
                          W05-RATE))
     COMPUTE W90-ATAX = W90-ATAX + ((W25-VALUE -
          W90-CDISC) - W90-VALUE)
                   COMPUTE W90-EXCDIS ROUNDED = W90-CDISC -
                       (((W90-CDISC / (W05-RATE + 100.00000)) *
                          W05-RATE)).
             ADD W90-VALUE       TO W90-EXCTOT.
      COMPUTE CRD-SELL ROUNDED = W90-VALUE / SAL-QNT.
    IF WS-CARDEX = "Y"
        PERFORM WRITE-CARDEX THRU WRITE-CARDEX-EXIT
              MOVE 1   TO WS-ACTION
        PERFORM AY43 THRU AY59.
    IF WS-PRNINV = "Y"
              MOVE W15-L11  TO CSH-DATA
               WRITE CSH-L.
      *
      *    ****    U P D A T E   S A L E S   A N A L Y S I S
      *
             MOVE STK-LEDG       TO WS-DEPKEY.
             PERFORM AY38 THRU AY59.
             ADD W90-VALUE       TO DPT-SALES-DAY
                                    DPT-SALES-MTD
                                    DPT-SALES-YTD.
           IF WX-INT = "Y"
               MOVE W12-T-YMD    TO WX-DATE
               MOVE STK-LEDG     TO WX-LEDG
               MOVE "CASH SALES" TO WX-NAR
               COMPUTE WX-VALUE = W90-VALUE * -1
               MOVE DPT-SALE-GL  TO WX-AC
               MOVE 04           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
      *
      *    ****    U P D A T E   C O S T - O F - S A L E S
      *
             COMPUTE W90-BCR ROUNDED = (1.000 * W10-QUANT * W10-COST).
             ADD W90-BCR         TO DPT-COST-DAY
                                    DPT-COST-MTD
                                    DPT-COST-YTD.
           IF WX-INT = "Y"
               IF W90-BCR NOT = ZERO
                   MOVE W90-BCR  TO WX-VALUE
                   MOVE DPT-COST-GL
                                 TO WX-AC
                   MOVE 08       TO WX-TYPE
                   PERFORM GL-TRF-UPDATE
                   COMPUTE WX-VALUE = W90-BCR * -1
                   MOVE DPT-PUR-GL
                                 TO WX-AC
                   MOVE 19       TO WX-TYPE
                   PERFORM GL-TRF-UPDATE.
      *
      *    ****    U P D A T E   D I S C O U N T   G I V E N
      *
             ADD W90-EXCDIS      TO DPT-DISC-DAY
                                    DPT-DISC-MTD
                                    DPT-DISC-YTD
                                    DPT-SALES-DAY
                                    DPT-SALES-MTD
                                    DPT-SALES-YTD.
             PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
             MOVE W12-T-YMD      TO STK-ACT.
      *
      *    ****    C H E C K   I F   S T O C K   C O N T R O L L E D
      *
    IF WS-PRC-IND = "Y"
              IF STK-IND > 1
           GO TO DA85
        END-IF
    ELSE
    IF STK-IND > ZERO
              GO TO DA85.
             SUBTRACT W10-QUANT  FROM STK-QUANT.
          IF STK-DISC > ZERO
               GO TO DA85.
      *
      *    ****    C H E C K   R E - O R D E R   L E V E L   A N D
      *            R E P O R T   L O W - L E V E L S
      *
           IF STK-QUANT NOT > STK-LEVEL
               MOVE 1            TO STK-DISC
               PERFORM BG80.
       DA85.
             ADD W90-BCR         TO W90-BDT STK-MTDC STK-YTDC.
             ADD W10-QUANT       TO STK-MTD STK-YTD.
             ADD W90-VALUE       TO STK-MTDV W90-SALES STK-YTDV.
             PERFORM REWRITE-STOCK THRU WRITE-STOCK-EXIT.
      *
      *    ****    L O G   T R A N S A C T I O N   T O  T H E
      *     A U D I T   F I L E
      *
            MOVE W70-AUDIT  TO WS-AUDKEY.
             MOVE LOW-VALUES     TO AUD-REC1.
             PERFORM AY16 THRU AY59.
             MOVE 6              TO AUD-TYPE.
             MOVE WS-DRAW        TO AUD-SUB.
             MOVE W90-REF        TO AUD-CSH.
             MOVE W10-SMAN       TO AUD-SMAN.
             MOVE W90-VALUE      TO AUD-SEL.
             MOVE W90-BCR        TO AUD-CST.
             MOVE STK-CODE       TO AUD-STOCK.
             MOVE WS-SUPER       TO AUD-SUPER.
             PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
             ADD 1               TO W70-AUDIT.
             MOVE 1              TO WS-AUDKEY.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
             PERFORM AY16 THRU AY59.
             MOVE 1              TO AUD-REP6.
             PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
           IF W90-CDISC = ZERO
               GO TO DA90.
      MOVE SPACES  TO W15-DETAIL.
             MOVE "DISCOUNT/AFSLAG %"
                                 TO W15-DISC.
             MOVE WS-DISC        TO W15-DSC.
             MULTIPLY -1 BY W90-CDISC
     GIVING W15-PRICE.
           IF WS-PRNINV = "Y"
               MOVE W15-L11      TO CSH-DATA
               WRITE CSH-L.

       DA90.
             MOVE SPACES         TO CSH-DATA.
             ADD 1               TO WS-SALKEY.
             GO TO DA80.

       DA95.
           IF WS-PRNINV = "Y"
              MOVE ALL "-"  TO CSH-LINE
               WRITE CSH-L.
             MOVE "Sub TOTAL/TOTAAL :"
                                 TO W15-REMK.
             MOVE W90-DB         TO W15-VAL W90-TOTAL.
           IF WS-PRNINV = "Y"
               MOVE W15-L13      TO CSH-DATA
               WRITE CSH-L.
           IF W90-TDISC NOT = ZERO
               IF W90-LDISC = ZERO
                   MOVE 298      TO WS-DEPKEY
                   PERFORM AY38 THRU AY59
                   ADD W90-TDISC TO DPT-DISC-DAY
                                    DPT-DISC-MTD
                                    DPT-DISC-YTD
                   IF WX-INT = "Y"
                       MOVE W90-EXCDIS
                                 TO WX-VALUE
                       MOVE DPT-DISC-GL
                                 TO WX-AC
                       MOVE 15   TO WX-TYPE
                       PERFORM GL-TRF-UPDATE
                   END-IF
                   PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
           IF W90-TDISC NOT = ZERO
               MOVE "DISCOUNT/AFSLAG  :"
                                 TO W15-REMK
               COMPUTE W15-VAL = W90-TDISC * -1
               IF WS-PRNINV = "Y"
                   MOVE W15-L13  TO CSH-DATA
                   WRITE CSH-L.
           IF W90-TAX NOT = SPACES
               MOVE "TAX / BELASTING  :"
                                 TO W15-REMK.
           IF W90-TAX = SPACES
               MOVE W05-VAT-R1   TO W05-RATE
               COMPUTE W90-TTAX ROUNDED = (W90-TAXVAL /
                  (100.00000 + W05-RATE)) * W05-RATE
               COMPUTE W90-TAXVAL = W90-TAXVAL - W90-TTAX.
             MOVE 9              TO WS-PARKEY.
             PERFORM AY01 THRU AY59.
             MOVE 300            TO WS-DEPKEY.
             PERFORM AY38 THRU AY59.
            ADD W90-ATAX  TO DPT-SALES-DAY
                                    DPT-SALES-MTD
                                    DPT-SALES-YTD.
    IF (WX-INT = "Y") AND (W90-ATAX NOT = ZERO)
  COMPUTE WX-VALUE = W90-ATAX * -1
                MOVE DPT-SALE-GL TO WX-AC
                MOVE 10          TO WX-TYPE
                PERFORM GL-TRF-UPDATE.
             PERFORM REWRITE-DEPART THRU WRITE-DEPART-EXIT.
             ADD W90-NONTAX      TO PAR-DNGS-DAY
                                    PAR-DNGS-MTD
                                    PAR-DNGS-YTD.
             ADD W90-TAXVAL      TO PAR-DVAT-DAY
                                    PAR-DVAT-MTD
                                    PAR-DVAT-YTD.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             MOVE W05-RATE       TO WS-PER.
           IF W90-TAX = SPACES
               GO TO DA100.
             MOVE W90-TTAX       TO W15-VAL.
           IF WS-PRNINV = "Y"
               MOVE W15-L13      TO CSH-DATA
               WRITE CSH-L.
       DA100.
             MOVE W90-BDT        TO W90-TCOST.
             MOVE 2              TO TAX-TYPE.
             MOVE 2              TO TAX-ACTYPE.
             MOVE 01             TO TAX-CODE.
             MOVE W12-T-YMD      TO TAX-DATE.
             MOVE W90-REF        TO TAX-REF.
             MOVE W90-TTAX       TO TAX-VAT.
             MOVE W90-NONTAX     TO TAX-TAXFREE.
             SUBTRACT W90-DISC FROM W90-TOTAL
                                 GIVING TAX-VALUE.
             MOVE ZERO           TO TAX-SEQ
                                    TAX-AC.
             PERFORM WRITE-TXTRAN THRU WRITE-TXTRAN-EXIT.
             MOVE 1              TO WS-ACTION.
             PERFORM AY19 THRU AY59.
             MOVE "DUE / VERSKULDIG :"
                                 TO W15-REMK.
            SUBTRACT W90-TDISC FROM W90-TOTAL
     GIVING W15-VAL.
           IF WS-PRNINV = "N"
               GO TO DA101.
             MOVE W15-L13        TO CSH-DATA
             WRITE CSH-L.
           IF W90-CASH > ZERO
              MOVE "CASH REC/KONTANT :"
                                 TO W15-REMK
               MOVE W90-CASH     TO W15-VAL
               MOVE W15-L13      TO CSH-DATA
               WRITE CSH-L.
           IF W90-CHQ > ZERO
              MOVE "CHEQUE REC/TJEK  :"
                                 TO W15-REMK
               MOVE W90-CHQ      TO W15-VAL
               MOVE W15-L13      TO CSH-DATA
               WRITE CSH-L.
          IF W90-CARD > ZERO
               MOVE "CR-CARD/KT-KAART :"
                                 TO W15-REMK
               MOVE W90-CARD     TO W15-VAL
               MOVE W15-L13      TO CSH-DATA
               WRITE CSH-L.
          IF W90-SPEED > ZERO
               MOVE "CR-CARD/KT-KAART :"
                                 TO W15-REMK
              MOVE W90-SPEED  TO W15-VAL
               MOVE W15-L13      TO CSH-DATA
               WRITE CSH-L.
          IF W90-VOUCH > ZERO
              MOVE "VOUCHER/KT-KAART :"
                                 TO W15-REMK
              MOVE W90-VOUCH  TO W15-VAL
               MOVE W15-L13      TO CSH-DATA
               WRITE CSH-L.
          IF W90-CHANGE > ZERO
               MOVE "CHANGE/KLEINGELD :"
                                 TO W15-REMK
               MOVE W90-CHANGE   TO W15-VAL
               MOVE W15-L13      TO CSH-DATA
               WRITE CSH-L.

       DA101.
           IF WS-PRNINV = "N"
               GO TO DA106.
             MOVE SPACES         TO CSH-DATA.
             WRITE CSH-L.
           IF W90-TAX = SPACES
               IF W05-VATNO NOT = SPACES
                   MOVE "VAT Inclusive / BTW Ingesluit"
                                 TO CSH-DATA
                   WRITE CSH-L
                   MOVE SPACES   TO CSH-DATA.
           IF W05-VATNO NOT = SPACES
               MOVE ALL "="      TO CSH-LINE
               WRITE CSH-L
               MOVE SPACES       TO CSH-DATA
        MOVE "   TAX INVOICE"
                                 TO W15-DOCTYP
               MOVE W15-L1A      TO CSH-DATA
               WRITE CSH-L
               MOVE SPACES       TO CSH-DATA
               MOVE "TAXABLE/BELASBARE :"
                                 TO W15-REMK
               MOVE W90-TAXVAL   TO W15-VAL
               MOVE W15-L13      TO CSH-DATA
               WRITE CSH-L
               MOVE "V A T / B T W     :"
                                 TO W15-REMK
               MOVE W90-TTAX     TO W15-VAL
               MOVE W15-L13      TO CSH-DATA
               WRITE CSH-L
               MOVE ALL "="      TO CSH-LINE
               WRITE CSH-L
               MOVE SPACES       TO CSH-DATA
               WRITE CSH-L.
             MOVE W15-L9         TO CSH-DATA.
             WRITE CSH-L.
            MOVE "    APAC accounting - (011) 462-2272"
     TO CSH-DATA.
             WRITE CSH-L.
            MOVE SPACES  TO CSH-DATA W15-DETAIL.
      MOVE "**"   TO W15-AS1 W15-AS2.
      MOVE WS-CSHCODE  TO W15-CSHCODE.
      MOVE W15-L12A  TO CSH-DATA.
            MOVE 1   TO WS-S9.
       DA105.
             WRITE CSH-L.
            MOVE SPACES  TO CSH-DATA.
           IF WS-S9 < W85-ADVANCE
               ADD 1             TO WS-S9
               GO TO DA105.
      MOVE W85-CSI  TO CSH-DATA.
             WRITE CSH-L.
             MOVE SPACES         TO CSH-DATA.
      CLOSE CSHSLE.
      OPEN OUTPUT CSHSLE.
      CALL X"91" USING X91-RES X91-FUN CSHSLE.
       DA106.
             MOVE W70-AUDIT      TO WS-AUDKEY.
             MOVE LOW-VALUES     TO AUD-REC1.
             PERFORM AY16 THRU AY59.
             MOVE 5              TO AUD-TYPE.
            MOVE WS-DRAW  TO AUD-SUB.
            MOVE W90-REF  TO AUD-CSH.
             COMPUTE AUD-SELL = W90-TSELL - W90-TTAX.
            MOVE W90-TCOST  TO AUD-COST.
            MOVE W10-SMAN  TO AUD-SMAN.
             PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
            ADD 1   TO W70-AUDIT.
            MOVE 1   TO WS-AUDKEY.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
             PERFORM AY16 THRU AY59.
            MOVE 1   TO AUD-REP5.
             PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
            MOVE W70-AUDIT  TO WS-AUDKEY.
            MOVE LOW-VALUES  TO AUD-REC1.
             PERFORM AY16 THRU AY59.
            MOVE 4   TO AUD-TYPE.
            MOVE WS-DRAW  TO AUD-SUB.
            MOVE W10-SMAN  TO AUD-AC.
            MOVE W90-REF  TO AUD-REF.
             COMPUTE AUD-VAL = (W90-TOTAL - W90-TDISC).
             PERFORM WRITE-AUDIT THRU WRITE-AUDIT-EXIT.
            ADD 1   TO W70-AUDIT.
            MOVE 1   TO WS-AUDKEY.
             PERFORM READ-AUDIT THRU READ-AUDIT-EXIT.
             PERFORM AY16 THRU AY59.
            MOVE 1   TO AUD-REP4.
             PERFORM REWRITE-AUDIT THRU WRITE-AUDIT-EXIT.
           IF W10-SMAN NOT = ZERO
              ADD 100 W10-SMAN  GIVING WS-PARKEY
               PERFORM AY01 THRU AY59
               ADD W90-TCOST     TO PAR-CT-MTD PAR-CT-YTD
               ADD W90-EXCTOT    TO PAR-SL-MTD PAR-SL-YTD
               PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             SUBTRACT W90-BAL FROM STK-TOTAL.
            ADD W90-SALES  TO STK-SMTD STK-SYTD.
            ADD 150 WS-DRAW  GIVING WS-PARKEY.
             PERFORM AY01 THRU AY59.
             ADD W90-CASH        TO PAR-CASH.
             ADD W90-CHQ         TO PAR-CHEQUES.
             ADD W90-CARD        TO PAR-CARDS.
            ADD W90-SPEED  TO PAR-SPEED.
            ADD W90-VOUCH  TO PAR-VOUCHERS.
             SUBTRACT W90-CHANGE FROM PAR-CASH.
             ADD W90-TTAX        TO PAR-VATVAL.
             ADD W90-TAXVAL      TO PAR-TAXVAL.
             COMPUTE PAR-CSH = PAR-CSH + W90-TAXVAL + W90-NONTAX.
             COMPUTE PAR-CSHMTD = PAR-CSHMTD + W90-TAXVAL + W90-NONTAX.
             COMPUTE PAR-CSHYTD = PAR-CSHYTD + W90-TAXVAL + W90-NONTAX.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
             MOVE PAR-RECORD151  TO W85-RECORD151.
            MOVE 7   TO WS-PARKEY.
             PERFORM AY01 THRU AY59.
             COMPUTE PAR-CASH-DAY  = PAR-CASH-DAY + (W90-CASH
                                     - W90-CHANGE) + W90-CHQ
         + W90-CARD + W90-VOUCH
         + W90-SPEED.
             COMPUTE PAR-CASH-MTH  = PAR-CASH-MTH + (W90-CASH
                                     - W90-CHANGE) + W90-CHQ
         + W90-CARD + W90-VOUCH
         + W90-SPEED.
             COMPUTE PAR-CASH-YEAR = PAR-CASH-YEAR + (W90-CASH
                                     - W90-CHANGE) + W90-CHQ
         + W90-CARD + W90-VOUCH
         + W90-SPEED.
           IF WX-INT = "Y"
               MOVE W12-T-YMD    TO WX-DATE
               MOVE 01           TO WX-LEDG
               MOVE "CASH SLIPS" TO WX-NAR
        COMPUTE WX-VALUE = (W90-TOTAL - W90-TDISC)
               MOVE PAR-CASH-GL  TO WX-AC
               MOVE 26           TO WX-TYPE
               PERFORM GL-TRF-UPDATE.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
            MOVE 8   TO WS-PARKEY.
             PERFORM AY01 THRU AY59.
            ADD W90-BDT  TO PAR-CSHT PAR-CMTD PAR-CYTD.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
    IF WS-BNK-IND = "Y" AND
       W90-CHQ NOT = ZERO
               INITIALIZE CHQ-RECORD2
               MOVE W90-REF      TO CHQ-TR-NUMBER
               MOVE W22-DTE1     TO CHQ-YMD
               MOVE W20-DTE      TO CHQ-DTE
               MOVE W10-NAME     TO CHQ-NAME
               MOVE W10-TEL      TO CHQ-TEL
               MOVE W90-CHQ      TO CHQ-VALUE
               PERFORM WRITE-CHEQUE THRU WRITE-CHEQUE-EXIT
               MOVE 1            TO WS-ACTION
               PERFORM AY36 THRU AY59.
             PERFORM AY70 THRU AY999.
             CLOSE PARAM AUDIT STOCK SALE NETWORK DEPART.
             OPEN OUTPUT SALE.
             CLOSE SALE.
             OPEN I-O PARAM AUDIT STOCK SALE NETWORK DEPART.
            MOVE "ENTER to continue - E to exit"
     TO WS-ERR-MES.
      MOVE SPACE   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF WS-OPTION = "E"
              GO TO DA150.
             GO TO DA05.
       DA110.
            MOVE ZERO   TO WS-ERROR.
      MOVE W10-SPAPRC  TO W10-ORDER.
    IF W90-TAX = SPACES
              IF STK-TAX NOT = ZERO
           COMPUTE W10-ORDER ROUNDED = (W10-SPAPRC /
              (100.0000 + W05-RATE)) * 100.
    IF (WS-PRC = "R") AND (STK-SELL NOT > W10-ORDER)
               GO TO DA120
           ELSE
    IF (WS-PRC = "C") AND (STK-CASH NOT > W10-ORDER)
               GO TO DA120
           ELSE
          IF STK-WSALE NOT > W10-ORDER
               GO TO DA120.
            COMPUTE W10-MARG = ((W10-ORDER - W10-COST) / W10-ORDER)
                                * 100.00.
           IF W10-MARG NOT < W85-MARG
               GO TO DA120.
       DA115.
            MOVE "Margin less than allowed - Override Y/N?"
     TO WS-ERR-MES.
      MOVE "N"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "N" OR "Y")
               GO TO DA115.
           IF WS-OPTION = "N"
              MOVE 1   TO WS-ERROR
               GO TO DA120.
       DA116.
            DISPLAY "Enter password" AT 2524
              WITH BACKGROUND-COLOR 3 FOREGROUND-COLOR 14.
            ACCEPT W40-CSHPASS AT 2539
      WITH BACKGROUND-COLOR 5 FOREGROUND-COLOR 15
          AUTO NO-ECHO.
            MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W40-CSHPASS
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            MOVE 1   TO WS-LENGTH.

       DA117.
    IF W40-CSHPASS = W85-SUPER (1)
        MOVE 1   TO WS-SUPER
        GO TO DA118.
    IF W40-CSHPASS = "NIGRAM"
               MOVE 10           TO WS-SUPER
           ELSE
    IF W40-CSHPASS = W85-SUPER (9)
        MOVE 9   TO WS-SUPER
           ELSE
              MOVE 1   TO WS-ERROR
               MOVE ZERO         TO WS-SUPER.
    IF W10-SPAPRC < W10-COST
        MOVE 1   TO WS-ERROR
  MOVE ZERO  TO WS-SUPER.

       DA118.
      PERFORM CLEAR-L50.

       DA120.
             EXIT.

       DA150.
      EXIT.
      *
      *    ****    A M E N D   S A L E S   D E T A I L S
      *
       DA200A        SECTION 50.
       DA200.
            MOVE 7   TO LIN.
             PERFORM DA48 UNTIL LIN > 18.
            MOVE 7   TO LIN.
            MOVE 1   TO WS-LINES WS-FIRST.

       DA202.
            MOVE WS-FIRST  TO WS-SALKEY WS-SALPOS.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
       DA205.
           IF WS-SALKEY > WS-LAST
              GO TO DA208.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
            DISPLAY (LIN, 2) SAL-ITEM
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 0.
            DISPLAY ( , 17) "   "
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
            DISPLAY ( , 21) SAL-SDSC
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
            MOVE SAL-QNT  TO W10-QUANT.
    IF W85-USE-3 = "N"
              DISPLAY S26-QNT AT LINE LIN
    ELSE
        DISPLAY S26-WEIGHT AT LINE LIN.
            MOVE SAL-SELL  TO W10-SELL.
             DISPLAY S26-PRC AT LINE LIN.
            MOVE SAL-VAL  TO W15-PRICE W100-EXT.
            DISPLAY ( , 70) W100-EXT
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
      *
      *    ****    I S   T H E   S C R E E N   F U L L
      *
          IF LIN < 18
              ADD 1   TO LIN WS-SALKEY WS-LINES
               GO TO DA205.

       DA208.
      MOVE 6   TO LIN.
      PERFORM CLEAR-L50.

       DA210.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
            DISPLAY " " AT 2503 WITH FOREGROUND-COLOR 14
     BACKGROUND-COLOR 3
             " highlight," WITH FOREGROUND-COLOR 1
       BACKGROUND-COLOR 3
       "PgUp" WITH FOREGROUND-COLOR 14
     BACKGROUND-COLOR 3
       "/" WITH FOREGROUND-COLOR 1
         BACKGROUND-COLOR 3
       "PgDn" WITH FOREGROUND-COLOR 14
     BACKGROUND-COLOR 3
       "," WITH FOREGROUND-COLOR 1
    BACKGROUND-COLOR 3
             "A" WITH FOREGROUND-COLOR 14
      BACKGROUND-COLOR 3
             "mend," WITH FOREGROUND-COLOR 1
           BACKGROUND-COLOR 3
             "D" WITH FOREGROUND-COLOR 14
      BACKGROUND-COLOR 3
             "elete or " WITH FOREGROUND-COLOR 1
           BACKGROUND-COLOR 3
             "Esc" WITH FOREGROUND-COLOR 14
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
       DA214.
             MOVE 3920           TO W41-LENGTH.
      CALL "CBL_WRITE_SCR_ATTRS" USING SAVE-POS
           W42-ATTRIB2
           W41-LENGTH
     RETURNING WS-STATUS.
      MOVE 4000   TO W41-LENGTH.
      *
      *    ****    G E T   U S E R   O P T I O N
      *
       DA215.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
    IF ADIS-FUNC
        EVALUATE KEY-CODE-1
   WHEN UP-KEY
       GO TO DA216
   WHEN DOWN-KEY
             GO TO DA217
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO DA215
    ELSE
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       PERFORM CLEAR-L50
       GO TO DA265
      *
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *
   WHEN PAGE-DOWN
             GO TO DA222
      *
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *
   WHEN PAGE-UP
             GO TO DA220
   WHEN OTHER  CALL X"E5"
        END-EVALUATE
        GO TO DA215
    ELSE
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
        IF (WS-OPTION = "A" OR "D") AND
    (LIN > 6)
     PERFORM CLEAR-L50
     GO TO DA224.
      CALL X"E5".
            GO TO DA215.
      *
      *    ****    U P  K E Y  ( M O V E   H I G H L I G H T  U P )
      *
       DA216.
    IF (WS-SALPOS = 1) OR (LIN < 7)
        GO TO DA218.
      SUBTRACT 1   FROM WS-SALPOS.
    IF LIN > 7
        SUBTRACT 1  FROM LIN
        GO TO DA218.
      MOVE 18   TO WS-S7.
      *
      *    ****     M O V E   T H E   D E T A I L S   D O W N
      *      O N E   L I N E
      *
       DA216-LOOP.
      SUBTRACT 1 FROM WS-S7
     GIVING WS-S8.
      MOVE W42-LINES (WS-S8)
     TO W42-LINES (WS-S7).
      MOVE W43-LINE (WS-S8)
     TO W43-LINE (WS-S7).
    IF WS-S7 > 8
        SUBTRACT 1  FROM WS-S7
        GO TO DA216-LOOP.
      MOVE WS-SALPOS  TO WS-SALKEY WS-FIRST.
      *
      *    ****    C L E A R   F I R S T   O R  L A S T
      *     D E T A I L  L I N E.
      *
       DA216-DISPLAY.
      PERFORM RESTORE-SCREEN-2.
            DISPLAY (LIN, 1) WS-MIV1 WITH BACKGROUND-COLOR 0
        FOREGROUND-COLOR 3
         WS-MIV78 WITH FOREGROUND-COLOR 3
         WS-MIV80 WITH BACKGROUND-COLOR 0
         FOREGROUND-COLOR 3.
      *
      *    ****    G E T   N E X T   O R   P R E V I O U S
      *     D E T A I L  L I N E.
      *
            PERFORM READ-SALE THRU READ-SALE-EXIT.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
          IF SAL-ITEM = SPACES
              DISPLAY (LIN, 21) SAL-SDSC
   WITH FOREGROUND-COLOR 7 HIGHLIGHT
        BACKGROUND-COLOR 0
              GO TO DA216-END-DISP.
            DISPLAY (LIN, 2) SAL-ITEM
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 0.
            DISPLAY ( , 17) "   "
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
            DISPLAY ( , 21) SAL-SDSC
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

       DA216-END-DISP.
      PERFORM SAVE-SCREEN-2.
      GO TO DA218.
      *
      *    ****    D O W N   K E Y   ( M O V E  H I G H L I G H T
      *           D O W N )
      *
       DA217.
    IF (WS-SALPOS = WS-LAST) AND (LIN > 7)
        GO TO DA218.
      *
      *    ****    WS-SALPOS CONTAINS KEY OF HIGHLIGHTED ITEM
      *
    IF LIN > 6
        ADD 1   TO WS-SALPOS.
    IF LIN < 18
        ADD 1   TO LIN
        GO TO DA218.
      MOVE 7   TO WS-S7.
      *
      *    ****     M O V E   T H E   D E T A I L S   U P   O N E
      *      L I N E
      *
       DA217-LOOP.
      ADD 1 WS-S7  GIVING WS-S8.
      MOVE W42-LINES (WS-S8)
     TO W42-LINES (WS-S7).
      MOVE W43-LINE (WS-S8)
     TO W43-LINE (WS-S7).
    IF WS-S8 < 18
        ADD 1   TO WS-S7
        GO TO DA217-LOOP.
      ADD 1   TO WS-FIRST.
      MOVE WS-SALPOS  TO WS-SALKEY.
      GO TO DA216-DISPLAY.
      *
      *    ****    H I G H L I G H T   C U R R E N T   L I N E
      *
       DA218.
            PERFORM DA214.
    IF LIN > 6
              DISPLAY (LIN, 1) W43-DET (LIN)
   WITH FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
            GO TO DA215.
      *
      *    ****    C L E A R   S C R E E N
      *
       DA219.
            MOVE 7   TO LIN.
            PERFORM DA48 UNTIL LIN > 18.
            MOVE 7   TO LIN.
             MOVE 1              TO WS-LINES.
      *
      *    ****   P R E V I O U S   S C R E E N   R E Q U I R E D
      *
       DA220.
    IF WS-FIRST = 1
        GO TO DA215.
    IF WS-FIRST > 12
        SUBTRACT 12  FROM WS-FIRST
    ELSE
        MOVE 1   TO WS-FIRST.
      PERFORM DA219.
      GO TO DA202.
      *
      *    ****    N E X T   S C R E E N   R E Q U I R E D
      *
       DA222.
    IF (WS-FIRST + 12) > WS-LAST
        GO TO DA215.
      ADD 12   TO WS-FIRST.
      PERFORM DA219.
      GO TO DA202.
      *
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *
       DA224.
          IF WS-OPTION = "A"
              GO TO DA228.
      *
      *    ****    M A K E   S U R E   T H A T   A N   I T E M
      *            M U S T   B E   D E L E T E D
      *
       DA225.
            MOVE "Are you sure Y/N?"
     TO WS-ERR-MES.
            MOVE "N"   TO WS-OPTION.
      PERFORM OPT-MESSAGE.
          IF NOT (WS-OPTION = "N" OR "Y")
              GO TO DA225.
           IF WS-OPTION = "N"
              GO TO DA210.
             MOVE "D"            TO WS-OPTION.
      *
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *
       DA228.
            MOVE WS-SALPOS  TO WS-SALKEY.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
            MOVE SAL-ITEM  TO W10-ITEM STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
            MOVE SAL-QNT  TO W10-QUANT.
            MOVE SAL-COST  TO W10-COST.
            MOVE SAL-SELL  TO W10-SELL.
            MOVE SAL-DISC  TO W90-CDISC.
            MOVE SAL-DSC  TO WS-DISC.
            MOVE SAL-VAL  TO W25-VALUE.
      *
      *    ****    R E D U C E   T O T A L S
      *
            SUBTRACT W25-VALUE  FROM W90-DB.
            SUBTRACT W90-CDISC  FROM W90-LDISC.
             SUBTRACT SAL-NONTAX FROM W90-NONTAX.
             SUBTRACT SAL-TAXVAL FROM W90-TAXVAL.
           IF (STK-IND < 2) OR (STK-IND = 3)
               COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
      *
      *    ****    I S   I T   A N   A M E N D M E N T
      *
      PERFORM DA214.
          IF WS-OPTION = "A"
               GO TO DA250.
      *
      *    ****    R E - D I S P L A Y   T O T A L S
      *
            PERFORM DA45B THRU DA45-AA.
       DA245.
      *
      *    ****    D E L E T E   I T E M   F R O M   S A L E   F I L E
      *
            MOVE WS-SALKEY  TO WS-SALSTR.
            ADD 1   TO WS-SALKEY.
           IF WS-SALKEY > WS-LAST
               SUBTRACT 1 FROM WS-LAST
               GO TO DA200.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
            MOVE WS-SALSTR  TO WS-SALKEY.
             SUBTRACT 1          FROM SAL-CODE.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
            ADD 1   TO WS-SALKEY.
             GO TO DA245.

       DA250.
      *
      *    ****    A M E N D   Q U A N T I T Y
      *
    IF W85-USE-3 = "Y"
              ACCEPT S26-WEIGHT AT LINE LIN
    ELSE
              ACCEPT S26-QNT AT LINE LIN.
            PERFORM CLEAR-L50.
           IF W10-QUANT = ZERO
              MOVE "Quantity MAY NOT be zero"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
              MOVE SAL-QNT  TO W10-QUANT
              GO TO DA250.
      *
      *    ****    R E C A L C U L A T E   V A L U E S
      *
            MULTIPLY W10-SELL BY W10-QUANT GIVING W25-VALUE.
            MOVE W25-VALUE  TO W15-PRICE W100-EXT.
            DISPLAY (LIN, 70) W100-EXT
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
      *
      *    ****    U P D A T E   S A L E   F I L E   R E C O R D
      *
            MOVE W10-QUANT  TO SAL-QNT.
            MOVE W25-VALUE  TO SAL-VAL.
           IF WS-DISC = ZERO
               GO TO DA255.
            COMPUTE W90-CDISC = (W25-VALUE * WS-DISC) / 100.
            MOVE W90-CDISC  TO W15-PRICE SAL-DISC.
      *
      *    ****    R E - C A L C U L A T E   &   D I S P L A Y
      *            T O T A L S
      *
       DA255.
            PERFORM DA45 THRU DA45-AA.

       DA260.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
             GO TO DA200.
      *
      *    ****    C L E A R   D E T A I L   L I N E S
      *
       DA265.
      PERFORM DA219.
      *
      *    ****    D I S P L A Y   L A S T   P A G E
      *
    IF WS-LAST > 10
        SUBTRACT 9 FROM WS-LAST
     GIVING WS-SALKEY
    ELSE
        MOVE 1   TO WS-SALKEY.

       DA270.
            PERFORM READ-SALE THRU READ-SALE-EXIT.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
          IF SAL-ITEM = SPACES
              DISPLAY (LIN, 21) SAL-SDSC
   WITH FOREGROUND-COLOR 7 HIGHLIGHT
        BACKGROUND-COLOR 0
              GO TO DA275.
            DISPLAY (LIN, 2) SAL-ITEM
        WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 0.
            DISPLAY ( , 17) "   "
        WITH FOREGROUND-COLOR 3 HIGHLIGHT
      BACKGROUND-COLOR 0.
            DISPLAY ( , 21) SAL-SDSC
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
       DA275.
      ADD 1   TO LIN.
          IF WS-SALKEY < WS-LAST
        ADD 1   TO WS-SALKEY
              GO TO DA270.

       DA299.
            EXIT.

       DA300P        SECTION 50.
       DA300.
      PERFORM SAVE-SCREEN.
      DISPLAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" AT 0843
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 6.
      DISPLAY "³  P R O M P T S  ³" AT 0943
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 6.
      DISPLAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" AT 1043
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 6.
      DISPLAY "³ " AT 1143
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 6
       "P" WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 6
       "rompts (Slow)  ³"
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 6.
      DISPLAY "³ " AT 1243
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 6
       "M" WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 6
       "inimum Prompts ³"
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 6.
      DISPLAY "³ " AT 1343
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 6
       "F" WITH FOREGROUND-COLOR 7 HIGHLIGHT
      BACKGROUND-COLOR 6
       "ast Capture    ³"
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 6.
      DISPLAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" AT 1443
        WITH FOREGROUND-COLOR 6 HIGHLIGHT
      BACKGROUND-COLOR 6.
      MOVE 09   TO SHADE-ROW.
      MOVE 45   TO SHADE-COL.
      MOVE 17   TO SHADE-WIDTH.
      MOVE 6   TO SHADE-LINES.
      PERFORM SCREEN-SHADOW.
      PERFORM HIDE-THE-CURSOR.

       DA305.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
    IF DATA-8BIT
        MOVE KEY-CODE-1X  TO WS-OPTION
    ELSE
        CALL X"E5" CALL X"E5" CALL X"E5"
        GO TO DA305.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "P" OR "M" OR "F")
        CALL X"E5" CALL X"E5" CALL X"E5"
        GO TO DA305.
    IF WS-OPTION = "M"
        MOVE "N"   TO WS-OPTION.
    IF WS-OPTION = "P"
        MOVE "Y"   TO WS-OPTION.
      PERFORM DISPLAY-THE-CURSOR.
      PERFORM RESTORE-SCREEN.
      MOVE WS-OPTION  TO WS-PROMPT.

       DA999.
             EXIT.
      /
       YA000        SECTION 90.
       YA00.
      MOVE "STP\STPSOR"  TO PRG-NAME2.
      PERFORM YA100.
            CLOSE RECOVER.
      MOVE "STP\ORDPRN2"  TO PRG-NAME2.
      PERFORM YA100.
            OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
            OPEN I-O RECOVER.
       YA99.
      EXIT.

       YA100        SECTION 90.
       YA101.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 0   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
      PERFORM SAVE-SCREEN-3.
      CALL PRG-NAME2
    USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY
    ON EXCEPTION
        GO TO YA110
      END-CALL.
      CANCEL PRG-NAME2.
       YA105.
      PERFORM RESTORE-SCREEN-3.
    IF MOUSE-ATTACHED = "Y"
        MOVE 66   TO MOUSE-FUNC
        MOVE 1   TO MOUSE-PARAM
        CALL X"AF" USING MOUSE-FUNC
    MOUSE-PARAM.
      GO TO YA199.
       YA110.
      MOVE SPACES  TO WS-ERR-MES.
      STRING "Program- " DELIMITED SIZE
       PRG-NAME2 DELIMITED " "
       " not on disk, press ANY key"
       DELIMITED SIZE
       INTO WS-ERR-MES.
      PERFORM ERROR-LENGTH THRU ERROR-EXIT.
      GO TO YA105.

       YA199.
      EXIT.
      /
       ZA000-INIT        SECTION 91.
       ZA000-OPEN.
      MOVE LS-COMPANY  TO WS-TOP-COMP.
    IF LS-USER = LS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
      MOVE LS-PARID  TO WS-PARID.
      MOVE LS-L-OR-N  TO W02-L-OR-N.
      MOVE WS-SYS-ID  TO W02-SYSID.
      MOVE LS-TODAY-DDMMYY
     TO TODAY-DDMMYY W12-TODAY.
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
      MOVE "AUDITF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-AUDITF.
      MOVE "CARDEX"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-CARDEX.
      MOVE "CHEQUE"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-CHEQUE.
      MOVE "DEPART"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-DEPART.
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
      MOVE "SALE"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      INSPECT AFID-PATH REPLACING FIRST "XXX"
          BY LS-USER.
      MOVE AFID-PATH  TO W02-SALE.
      MOVE "SHARED"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-SHARED.
      MOVE "STKMEM"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STKMEM.
      MOVE "STOCKF"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-STOCKF.
      MOVE "TXTRAN"  TO AFID-KEY.
      PERFORM ZA00-READ-APACFIDS THRU ZA00-READ-APACFIDS-EXIT.
      MOVE AFID-PATH  TO W02-TXTRAN.
            MOVE ZERO   TO WS-LINES.
           IF WS-DRAW = ZERO
        MOVE 40   TO WS-DRAW.
            MOVE 3   TO WS-PARKEY.
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
      MOVE PAR-VAT-REG  TO W05-VATNO.
      MOVE PAR-PROMPT  TO WS-PROMPT.
      MOVE PAR-CRDX  TO WS-CARDEX.
      MOVE PAR-PRICED-IND TO WS-PRC-IND.
      MOVE PAR-BANK-IND  TO WS-BNK-IND.
      MOVE PAR-CSHCODE  TO WS-CSHCODE.
      MOVE PAR-SLIP-P  TO WS-SLIP-P.
            OPEN OUTPUT RECOVER.
             CLOSE RECOVER.
             OPEN I-O RECOVER.
             OPEN I-O CHEQUE.
          IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49
               ELSE
               IF IDX-CORRUPT
                   MOVE 36       TO WS-F-ERROR
                   GO TO ZA50.
           IF WS-STATUS NOT = "00"
               MOVE 36           TO WS-F-ERROR
               PERFORM OPEN-ERROR.
            OPEN OUTPUT SALE.
           IF RUNTIME-ERROR
               IF FLE-LOCKED
                   GO TO ZA200
               ELSE
               IF FLE-LIMIT
                   GO TO ZA49.
           IF WS-STATUS NOT = "00"
              MOVE 19   TO WS-F-ERROR
               PERFORM OPEN-ERROR.
            CLOSE SALE.
             OPEN I-O SALE.
            MOVE 5   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-VAT-RATES  TO W05-VAT-RATES.
            MOVE 1   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-CSHEAD = SPACES
              MOVE "Setup cash slip headings"
     TO WS-ERR-STRING
              PERFORM ERROR-MESSAGE
               GO TO ZA205.
            MOVE PAR-DMY  TO W12-TODAY.
            MOVE PAR-YMD  TO W12-T-YMD.
             MOVE PAR-CSHEAD     TO W15-COMP W40-NAME.
             MOVE PAR-CSHADD     TO W15-ADD.
             MOVE PAR-TELEPHONE  TO W15-TEL.
      MOVE 6   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-WHOLESALE  TO WS-WS.
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
             ADD 150 WS-DRAW GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE W12-TODAY  TO PAR-CSHDTE W15-DATE.
            MOVE PAR-RECORD151  TO W85-RECORD151.
             PERFORM REWRITE-PARAM THRU WRITE-PARAM-EXIT.
            MOVE 5   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
            MOVE PAR-PW  TO W85-PASS.
            MOVE 32   TO WS-S1.
             DISPLAY "Waiting for printer response" AT 1431
                      WITH FOREGROUND-COLOR 14 BLINK.
       ZA05.
           IF W40-NCH (WS-S1) = SPACE
               SUBTRACT 1 FROM WS-S1
               GO TO ZA05.
           IF WS-S1 > 16
              MOVE W85-NUL  TO W15-EXPP1 W15-ECAN1
           ELSE
              MOVE W85-SO  TO W15-EXPP1
              MOVE W85-CSO  TO W15-ECAN1.
            MOVE W85-SO  TO W15-EXPP2 W15-EXPP4.
            MOVE W85-CSO  TO W15-ECAN2 W15-ECAN4.
    IF W85-CPRN = 1
        MOVE "LPT1"  TO W02-PRINTER
           ELSE
           IF W85-CPRN = 2
        MOVE "LPT2"  TO W02-PRINTER
           ELSE
           IF W85-CPRN = 3
        MOVE "LPT3"  TO W02-PRINTER
           ELSE
           IF W85-CPRN = 4
        MOVE "COM1"  TO W02-PRINTER
           ELSE
           IF W85-CPRN = 5
        MOVE "COM2"  TO W02-PRINTER
           ELSE
           IF W85-CPRN = 9
        MOVE "CSH001."  TO W02-PRINTER
        MOVE LS-USER  TO W02-PUSER.
             OPEN OUTPUT CSHSLE.
      CALL X"91" USING X91-RES X91-FUN CSHSLE.
      *
      *     O L D   S T Y L E   C A S H   D R A W E R
      *
           IF W85-DRAWER  = "F"
        IF W85-PORT = 2
                   MOVE "COM2"   TO W02-CSHDRW
               ELSE
        IF W85-PORT = 3
                   MOVE "COM3"   TO W02-CSHDRW
               ELSE
        IF W85-PORT = 4
                   MOVE "COM4"   TO W02-CSHDRW.
           IF W85-DRAWER = "F"
               OPEN OUTPUT CSHDRW
        MOVE ALL "A"  TO CSH-OPEN.
    IF W85-CUSDISP = "Y"
        IF W85-CUSPORT = 2
     MOVE "COM2"  TO W02-CUSDSP
               ELSE
        IF W85-CUSPORT = 3
     MOVE "COM3"  TO W02-CUSDSP
               ELSE
        IF W85-CUSPORT = 4
     MOVE "COM4"  TO W02-CUSDSP.
      MOVE 7   TO WS-PARKEY.
            PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF W85-CUSDISP = "Y"
        OPEN OUTPUT CUSDSP
      *        MOVE X"1B"  TO CUS-CON
      *        MOVE "QA"  TO CUS-INST
        MOVE SPACES  TO CUS-LINE.
    IF W85-CUSDISP = "Y"
        MOVE PAR-DSP-HDG  TO W95-COMP.
      GO TO ZA999.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR 14.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR 15.
             GO TO ZA205.
       ZA50.
          IF WS-F-ERROR = 36
               MOVE "PD CHEQUE file"
     TO WS-FILE.
             DISPLAY "Rebuild " AT 0812
                      WS-FILE WITH FOREGROUND-COLOR 14.
             DISPLAY "Press any key to continue" AT 1012.
             ACCEPT WS-OPTION AT 1038 WITH FOREGROUND-COLOR 15.
       ZA51.
             EXIT PROGRAM.

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
           IF WS-F-ERROR = 1
              MOVE W02-AUDITF  TO WS-FILE
              MOVE WS-AUDKEY  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 2
              MOVE W02-NETWORK  TO WS-FILE
              MOVE WS-NETKEY  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 7
              MOVE W02-DEPART  TO WS-FILE
              MOVE WS-DEPKEY  TO WS-KEY
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
           IF WS-F-ERROR = 22
               MOVE W02-STOCKF   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE STK-CODE     TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 25
               MOVE W02-STKMEM   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE SME-KEY      TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 32
               MOVE W02-TXTRAN   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE TAX-KEY  TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 36
               MOVE W02-CHEQUE   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE CHQ-TR-NUMBER
     TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 37
              MOVE W02-SHARED  TO WS-FILE
              MOVE WS-SHARED  TO WS-KEY
           ELSE
           IF WS-F-ERROR = 40
               MOVE W02-LEDTRF   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE XFR-KEY  TO WS-KEYX
          ELSE
          IF WS-F-ERROR = 43
              MOVE W02-CARDEX  TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE CRD-KEY  TO WS-KEYX.
           IF WS-STATUS = "10"
              MOVE "End of FILE"
     TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "22"
              MOVE "Duplicate record number"
     TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "23"
              MOVE "Invalid record number"
     TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "24"
              MOVE "DISK full"  TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "30"
               MOVE "DISK error" TO WS-STAT-MESSAGE
           ELSE
           IF WS-STATUS = "94"
              MOVE "FILE locked"
     TO WS-STAT-MESSAGE.
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
