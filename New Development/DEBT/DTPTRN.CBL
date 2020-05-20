      $SET LINKCOUNT"468" GNT"DTPTRN.GNT"
      ******************************************************************
      *                                                                *
      *   *******   ********  *******   ********  *******   **    **   *
      *   **    **     **     **    **     **     **    **  ***   **   *
      *   **    **     **     **    **     **     **   **   ****  **   *
      *   **    **     **     *******      **     ******    ** ** **   *
      *   **    **     **     **           **     **   **   **  ****   *
      *   **    **     **     **           **     **    **  **   ***   *
      *   *******      **     **           **     **    **  **    **   *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *      R E C U R I N G   T R A N S A C T I O N S  (CREATE)       *
      *                                                                *
      *     VERSION 9.00.20 - December 2012                            *
      *                                                                *
      * This program is still under development and is scheduled to be *
      * completed during 2011. (This is a nice to have, but not a must *
      * have or critical requirement). This program will allow for the *
      * User to setup recurring transactions for an account. Provision *
      * will be made for weekly, monthly, by-monthly, quarterly, six-  *
      * monthly and anual payments.                                    *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     DTPTRN.
       AUTHOR.         J.W.LEMMON (APAC).
       DATE-WRITTEN.   AUGUST 2002.

     COPYRIGHT NOTICE: COPYRIGHT (C) 2002 - 2016
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

       COPY "CONTROL.SL".

       COPY "DEBTOR.SL".

       COPY "DEBMEM.SL".

       COPY "DEPART.SL".

       COPY "GARTEE.SL".

       COPY "PARAM.SL".

       COPY "RECTRN.SL".

       COPY "SALE.SL".

       COPY "STOCK.SL".

       COPY "STKMEM.SL".

      /
       DATA DIVISION.
       FILE SECTION.

       COPY "CONTROL.FDE".

       COPY "DEBTOR.FDE".

       COPY "DEBMEM.FDE".

       COPY "DEPART.FDE".

       COPY "PARAM.FDE".

       COPY "RECTRN.FD".

       COPY "STOCK.FDE".

       COPY "STKMEM.FDE".

      /
       WORKING-STORAGE SECTION.
       77  WS-CHECK        PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-DISC    PIC 99V99  COMP-3.
       77  WS-TDISC        PIC  99V99    COMP-3.
       77  WS-CASH-AC      PIC  X(06).
       77  WS-SUB          PIC  9(04)    COMP-5.
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
       77  WS-DEPKEY       PIC  9(04)    COMP-5.
       77  WS-ENQPOS       PIC  9(06)    COMP-5.
       77  WS-ENQEND       PIC  9(06)    COMP-5.
       77  WS-KEYSTR       PIC  9(06)    COMP-5.
       77  WS-SALSTR       PIC  9(04)    COMP-5.
       77  WS-SALKEY       PIC  9(04)    COMP-5.
       77  WS-NETKEY       PIC  9(04)    COMP-5.
       77  WS-GAR          PIC  9(01) VALUE 0.
       77  WS-NUM          PIC  Z9.
       77  WS-ADV-RATE     PIC  9(03)V99 COMP-3.
       77  WS-ADVALOREM    PIC X(01).
       77  WS-START        PIC  9(08).
       77  WS-END          PIC  9(08).
       77  WS-PAR          PIC  9(01) VALUE 0.
       77  WS-INO          PIC  9(02) VALUE 0.
       77  WS-LINES        PIC  9(02) VALUE 0.
       77  WS-FIRST    PIC 9(03) VALUE 0.
       77  WS-LAST    PIC 9(03) VALUE 0.
       77  WS-LASTINV      PIC  9(04)    COMP-5.
       77  WS-L1           PIC  9(04)    COMP-5.
       77  WS-OPTION    PIC X(01).
       77  WS-SKIP         PIC  X(01) VALUE " ".
       77  WS-LOOK         PIC  X(01) VALUE "S".
       77  WS-ETYPE        PIC  X(01) VALUE "I".
       77  WS-ERROR        PIC  9(01).
       77  WS-CHAR1        PIC  X(01).
       77  WS-CHAR2        PIC  X(01).
       77  WS-COMP         PIC  9(01).
       77  WS-HEAD         PIC  X(01).
       77  WS-TYPE         PIC  X(01).
       77  WS-AGE          PIC  X(01).
       77  WS-PRC          PIC  X(01).
       77  WS-INPUT        PIC  X(01) VALUE "N".
       77  WS-COPY         PIC  X(01) VALUE "N".
       77  WS-NOSELL       PIC  X(01) VALUE "N".
       77  WS-PRNINV       PIC  X(01) VALUE "Y".
       77  WS-BYPASS       PIC  X(01) VALUE "Y".
       77  WS-BATCH        PIC  9(05).
       77  WS-CHK          PIC  X(33) VALUE
                                "ENTER if correct - N if incorrect".
       77  WS-A            PIC  X(19) VALUE "Any key to continue".
       77  WS-E            PIC  X(15) VALUE "**END** - ENTER".
       77  WS-ERR1         PIC  X(22) VALUE "Invalid Account Number".
       77  WS-ERR2         PIC  X(09) VALUE "No Record".
       77  WS-PER          PIC  Z(02)9.99.
       77  WS-CR           PIC  X(01).
       77  WS-TAXRES       PIC  S9(07)V9(08) COMP-3.
       77  WS-BAL          PIC  S9(09)V9(02) COMP-3.
       77  PRG-DEBT-LUP            PIC  X(12)    VALUE "DTP\DTPLOOK".

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
           03  CRT-START           PIC  9(02).
           03  CRT-END             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "DTPTRN".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - RECURING TRAN".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-WRKHD           PIC  X(11).
           03  CRT-WRKST           PIC  X(03).

      /
       01  PRG-NAME.
    03  FILLER    PIC X(08) VALUE "INV\INVF".
    03  PRG-NUMBER  PIC 9(02) VALUE ZERO.

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
              05  WS-MCHAR PIC  X(80) VALUE "³   ³              ³
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

       COPY "WS.WS".

       COPY "CHAIN.WS".

       01  WS-PARID.
          03  WS-SYS-ID       PIC  X(03).

       01  W05-TAX-RATES.
           03  W05-VAT-RATES.
               05  W05-VAT     PIC S9(03)V99 COMP-3 OCCURS 6.
           03  W05-TAX-RATES REDEFINES W05-VAT-RATES.
               05  W05-VAT-R1  PIC S9(03)V99 COMP-3.
               05  W05-VAT-R2  PIC S9(03)V99 COMP-3.
               05  W05-VAT-R3  PIC S9(03)V99 COMP-3.
               05  W05-VAT-R4  PIC S9(03)V99 COMP-3.
               05  W05-VAT-R5  PIC S9(03)V99 COMP-3.
               05  W05-VAT-R6  PIC S9(03)V99 COMP-3.
           03  W05-RATE        PIC S9(03)V99 COMP-3.
           03  W05-RTE REDEFINES W05-RATE
                               PIC S9V9(04)  COMP-3.
           03  W05-VATNO       PIC  X(14).

       01  W05-INV.
           03  W05-ENTRY       PIC  ZZZ9.
           03  FILLER          PIC  X(01) VALUE SPACES.
          03  W05-DATE        PIC  Z9/99/9999.
           03  FILLER          PIC  X(01) VALUE SPACES.
           03  W05-TYPE        PIC  X(03).
           03  FILLER          PIC  X(01) VALUE SPACES.
           03  W05-REF         PIC  X(08).
           03  FILLER          PIC  X(01) VALUE SPACES.
           03  W05-VALUE       PIC  Z(06)9.99-.

       01  W10-STOCK.
           03  W10-ITEM        PIC X(14).
           03  W10-SERNO       PIC X(10).
           03  W10-DESC.
        05  W10-SDSC    PIC  X(29).
        05  FILLER      PIC  X(01).
          03  W10-REMKS       PIC X(29).
           03  W10-IND         PIC X(01).
           03  W10-QUANT       PIC 9(07)V99  COMP-3.
           03  W10-REORD       PIC 9(07)V99  COMP-3.
           03  W10-COST        PIC 9(07)V99  COMP-3.
           03  W10-SELL        PIC 9(07)V99  COMP-3.
           03  W10-SPAPRC      PIC 9(07)V99  COMP-3.
           03  W10-LEDG        PIC 9(04)     COMP-5.
           03  W10-SMAN        PIC 9(02).
           03  W10-DISC        PIC 9(02).
           03  W10-ORDER       PIC 9(07)V99  COMP-3.
          03  W10-DATE        PIC  X(10).
          03  W10-EDTE REDEFINES W10-DATE PIC 99/99/9999.
          03  W10-AVRG        PIC 9(07)V99  COMP-3.
           03  W10-MARG        PIC 9(05)V99  COMP-3.
           03  W10-ACTIVE      PIC X(08).
           03  W10-CODE1      PIC X(14).
           03  W10-COD1 REDEFINES W10-CODE1.
               05  W10-CD1    PIC X(02).
               05  W10-CDE1   PIC X(12).
           03  W10-CODE2      PIC X(14).
           03  W10-COD2 REDEFINES W10-CODE2.
               05  W10-CD2    PIC X(02).
               05  W10-CDE2   PIC X(12).
           03  W10-ACODE      PIC X(10).
           03  W10-ACDE1 REDEFINES W10-ACODE.
               05  W10-ACRSE  PIC X(01).
               05  W10-ASFINE PIC X(02).
               05  FILLER     PIC X(07).
           03  W10-VSELL      PIC S9(07)V99  COMP-3.
           03  W10-VCASH      PIC S9(07)V99  COMP-3.
           03  W10-VWSALE     PIC S9(07)V99  COMP-3.

       01  W10-REF.
           03  W10-NUM         PIC  9(08).

       COPY "W12.WS".

       01  W15-DISCOUNT.
           03  W15-DISC        PIC S9(03)V99 COMP-3 OCCURS 10.

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

       COPY "W40.DBT".

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       COPY "W50.WS".

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
           03  W60-ITM.
               05  W60-ICHAR OCCURS 14 PIC X(01).

       01  W75-KEYS.
           03  W75-S          PIC 9(02) COMP-5.
           03  W75-S1         PIC 9(02) COMP-5.
           03  W75-S2         PIC 9(02) COMP-5.
           03  W75-KEY        PIC  9(06)    COMP-5.
           03  W75-ALLOCATED  PIC S9(09)V99 COMP-3.
           03  W75-BALANCE    PIC S9(09)V99 COMP-3.
           03  W75-DNO        PIC X(06) OCCURS 18.
           03  W75-ITEMS OCCURS 18.
               05  W75-CODE   PIC X(14).
               05  W75-SELL   PIC 9(07)V99 COMP-3.
               05  W75-VSELL  PIC 9(07)V99 COMP-3.
               05  W75-CASH   PIC 9(07)V99 COMP-3.
               05  W75-VCASH  PIC 9(07)V99 COMP-3.
               05  W75-WSALE  PIC 9(07)V99 COMP-3.
               05  W75-VWSALE PIC 9(07)V99 COMP-3.

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

       COPY "W90.DBT".

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
           03  W100-SER        PIC ZZ9.
          03  W100-PRC        PIC Z(06)9.99-.
          03  W100-VAL        PIC Z(08)9.99-.
           03  W100-V2.
               05  W100-S6V2   PIC Z(05)9.99-.
          03  W100-V3 REDEFINES W100-V2.
               05  W100-PRICE  PIC X(09).
               05  FILLER      PIC X(01).
          03  W100-V4 REDEFINES W100-V2.
               05  W100-EXT    PIC Z(06)9.99.
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

       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE.IDS".

      /
       SCREEN SECTION.

       01  S05.
           03  LINE 12 COLUMN 12 VALUE "Enter Password >      <".

       01  S06.
          03  LINE 12 COLUMN 28 FOREGROUND-COLOR Grey HIGHLIGHT
         PIC X(06) USING W40-CSHPASS AUTO SECURE.

       COPY "STKLUP.CRT".

       COPY "DEBLUP.CRT".

       01  S25.
    03  LINE  7 COLUMN 21 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT VALUE
       "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
          03  LINE  8 COLUMN 21 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT VALUE
     "³Addresses:                 ".
    03        COLUMN 49 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "  Balance  ".
          03        COLUMN 60 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC Z(08)9.99- USING DEB-BAL.
          03        COLUMN 73 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT VALUE "³".
          03  LINE  9 COLUMN 21 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³ 1 ".
    03        COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC  X(24) USING W40-ADD1.
    03        COLUMN 49 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "  MTD      ".
          03        COLUMN 60 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC Z(08)9.99- USING W100-CUR.
          03        COLUMN 73 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT VALUE "³".
          03  LINE 10 COLUMN 21 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³   ".
    03        COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC  X(24) USING W40-ADD2.
    03        COLUMN 49 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "  Current  ".
          03        COLUMN 60 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC Z(08)9.99- USING DEB-CUR.
          03        COLUMN 73 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT VALUE "³".
          03  LINE 11 COLUMN 21 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³   ".
    03        COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC  X(24) USING W40-ADD3.
    03        COLUMN 49 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "   30 Days ".
          03        COLUMN 60 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC Z(08)9.99- USING DEB-30.
          03        COLUMN 73 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT VALUE "³".
          03  LINE 12 COLUMN 21 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³   ".
    03        COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC  X(24) USING W40-ADD4.
    03        COLUMN 49 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "   60 Days ".
          03        COLUMN 60 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC Z(08)9.99- USING DEB-60.
          03        COLUMN 73 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT VALUE "³".
          03  LINE 13 COLUMN 21 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³   ".
    03        COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC  X(24) USING W40-PC1.
    03        COLUMN 49 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "   90 Days ".
          03        COLUMN 60 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC Z(08)9.99- USING DEB-90.
          03        COLUMN 73 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT VALUE "³".
          03  LINE 14 COLUMN 21 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³ 2 ".
    03        COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC  X(24) USING W40-PADD1.
    03        COLUMN 49 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "  120 Days ".
          03        COLUMN 60 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC Z(08)9.99- USING DEB-120.
          03        COLUMN 73 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT VALUE "³".
          03  LINE 15 COLUMN 21 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³   ".
    03        COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC  X(24) USING W40-PADD2.
    03        COLUMN 49 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "  Interest ".
          03        COLUMN 60 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC Z(08)9.99- USING DEB-INT.
          03        COLUMN 73 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT VALUE "³".
          03  LINE 16 COLUMN 21 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³   ".
    03        COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC  X(24) USING W40-PADD3.
    03        COLUMN 49 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "  Cr Limit ".
          03        COLUMN 60 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC Z(08)9 USING DEB-CRL.
          03        COLUMN 69 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "    ³".
    03  LINE 17 COLUMN 21 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³   ".
    03        COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC  X(24) USING W40-PADD4.
    03        COLUMN 49 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "  Last Pmt ".
    03        COLUMN 60 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC Z9/99/99 USING DEB-PMT.
          03        COLUMN 68 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "     ³".
    03  LINE 18 COLUMN 21 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³   ".
    03        COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC  X(24) USING W40-PC2.
    03        COLUMN 49 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "       Amt ".
    03        COLUMN 60 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
     PIC Z(08)9.99- USING DEB-LAST-PMT.
          03        COLUMN 73 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT VALUE "³".
    03  LINE 19 COLUMN 21 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT VALUE
       "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".

       COPY "INVOICE.CRT".

       01  S26-QNT.
          03        COLUMN 51 FOREGROUND-COLOR Cyan HIGHLIGHT
                                 PIC Z(04)9.99 USING W10-QUANT AUTO.

       01  S26-PRC.
          03        COLUMN 60 FOREGROUND-COLOR Cyan HIGHLIGHT
                                 PIC Z(05)9.99 USING W10-SELL AUTO.

       01  S26-DSC.
          03        COLUMN 30 FOREGROUND-COLOR Cyan HIGHLIGHT
                                 PIC Z9.99 USING WS-DISC AUTO.

       01  S26-TDSC.
          03        COLUMN 15 FOREGROUND-COLOR Cyan HIGHLIGHT
                                 PIC Z9.99 USING WS-TDISC AUTO.

       01  S26-AVGE.
          03        COLUMN 28 FOREGROUND-COLOR Cyan HIGHLIGHT
                                 PIC Z(04)9.99 USING STK-AVGE AUTO.

       01  S28.
          03  LINE 25 COLUMN 65 FOREGROUND-COLOR Grey HIGHLIGHT
                                 PIC ZZZ9 USING WS-SALKEY AUTO.

       01  S30.
    03  LINE  9 COLUMN 15 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "Ú".
    03        COLUMN 16 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
                                 PIC X(50) USING WS-SRT-LINE.
    03        COLUMN 66 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "¿".
          03  LINE 10 COLUMN 15 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR Green
     PIC  X(18) FROM WS-SPACE.
    03        COLUMN 34 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE "CLIENT DETAILS".
    03        COLUMN 48 BACKGROUND-COLOR Green
     PIC  X(18) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³".
          03  LINE 11 COLUMN 15 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE "Name    >".
    03        COLUMN 65 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE "<".
          03        COLUMN 66 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³".
          03  LINE 12 COLUMN 15 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE "Address >".
    03        COLUMN 55 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE "<".
    03        COLUMN 56 BACKGROUND-COLOR Green
     PIC  X(10) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³".
          03  LINE 13 COLUMN 15 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR Green
     PIC  X(08) FROM WS-SPACE.
    03        COLUMN 24 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE ">".
    03        COLUMN 55 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE "<".
    03        COLUMN 56 BACKGROUND-COLOR Green
     PIC  X(10) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³".
          03  LINE 14 COLUMN 15 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR Green
     PIC  X(08) FROM WS-SPACE.
    03        COLUMN 24 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE ">".
    03        COLUMN 55 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE "<".
    03        COLUMN 56 BACKGROUND-COLOR Green
     PIC  X(10) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³".
          03  LINE 15 COLUMN 15 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR Green
     PIC  X(08) FROM WS-SPACE.
    03        COLUMN 24 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE ">".
    03        COLUMN 55 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE "<".
    03        COLUMN 56 BACKGROUND-COLOR Green
     PIC  X(10) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³".
          03  LINE 16 COLUMN 15 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³".
    03        COLUMN 16 BACKGROUND-COLOR Green
     PIC  X(08) FROM WS-SPACE.
    03        COLUMN 24 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE ">".
    03        COLUMN 29 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
                                 VALUE "<".
    03        COLUMN 30 BACKGROUND-COLOR Green
     PIC  X(36) FROM WS-SPACE.
          03        COLUMN 66 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "³".
    03  LINE 17 COLUMN 15 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "À".
    03        COLUMN 16 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
                                 PIC X(50) USING WS-SRT-LINE.
    03        COLUMN 66 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Brown HIGHLIGHT
     VALUE "Ù".

       01  S31.
    03  LINE 11 COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(40) USING W40-NAME AUTO.
    03  LINE 12 COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(30) USING W40-ADD1 AUTO.
    03  LINE 13 COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(30) USING W40-ADD2 AUTO.
    03  LINE 14 COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(30) USING W40-ADD3 AUTO.
    03  LINE 15 COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(30) USING W40-ADD4 AUTO.
    03  LINE 16 COLUMN 25 BACKGROUND-COLOR Green
     FOREGROUND-COLOR Grey HIGHLIGHT
                                 PIC X(04) USING W40-PC1 AUTO.

       01  S36.
          03  FOREGROUND-COLOR Cyan HIGHLIGHT
      PIC X(06) USING W80-ACNO AUTO.

       01  S-MEM.
           03  LINE  6 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(01) USING WS-G9.
           03          COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(01) USING WS-G10.
           03  LINE  7 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(01) USING WS-G3.
           03          COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey VALUE "   M E M O / R E M A R K S / C O R R E S P O N D E N C E".
           03          COLUMN 78 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(01) USING WS-G3.
           03  LINE  8 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(01) USING WS-G14.
           03          COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING WS-HME-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(01) USING WS-G13.
           03  LINE  9 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 10 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 11 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 12 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 13 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 14 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 15 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 16 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 17 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 18 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 19 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 20 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(62) USING WS-MM-LINE.
           03  LINE 21 COLUMN 17 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(01) USING WS-G11.
           03          COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING WS-MEM-LINE.
           03          COLUMN 78 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(01) USING WS-G12.

       01  SA-MEM.
           03  LINE  9 COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING W200-MEM1 AUTO.
           03  LINE 10 COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING W200-MEM2 AUTO.
           03  LINE 11 COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING W200-MEM3 AUTO.
           03  LINE 12 COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING W200-MEM4 AUTO.
           03  LINE 13 COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING W200-MEM5 AUTO.
           03  LINE 14 COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING W200-MEM6 AUTO.
           03  LINE 15 COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING W200-MEM7 AUTO.
           03  LINE 16 COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING W200-MEM8 AUTO.
           03  LINE 17 COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING W200-MEM9 AUTO.
           03  LINE 18 COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING W200-MEM10 AUTO.
           03  LINE 19 COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING W200-MEM11 AUTO.
           03  LINE 20 COLUMN 18 FOREGROUND-COLOR Black BACKGROUND-COLOR Grey PIC X(60) USING W200-MEM12 AUTO.

       01  ERROR-LINE.
    03  LINE 25 COLUMN  2 BACKGROUND-COLOR WS-TEMPBG
     FOREGROUND-COLOR WS-FGRND-5
     HIGHLIGHT
     PIC  X(48) FROM WS-ERR-MES.
      /
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA000-MAIN              SECTION.
       AA000-INIT.
             PERFORM ZA000-INIT.
             PERFORM BC000.
             CLOSE SALE.
       AZ100.
             EXIT PROGRAM.

       AA50-LOOKUP     SECTION.
       AA50D.
             MOVE "D" TO WS-LOOK.
       AA50.
            PERFORM SAVE-SCREEN.
             MOVE 4              TO W44-FUNCTION.
            PERFORM SCREEN-CONTENTS.
          IF WS-LOOK = "D"
               PERFORM DEBTOR-LOOKUP
           ELSE
               PERFORM STOCK-LOOKUP.
            PERFORM RESTORE-SCREEN.
            MOVE "S" TO WS-LOOK.
       AA99.
             EXIT.

       COPY "FUNCTION.CRT".

       COPY "LOCKED.RC2".

       COPY "CLEAR.CRT".

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       AA300              SECTION.
       AA301.
            DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " Item code/ "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "F2" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " Alt code/ "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "F3" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " Description/ "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "F4" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " Cross Reference Lookup"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
       AA305.
             EXIT.
      /
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY "CONTROL.RD".

       COPY "DEBTOR.RD".

       COPY "DEBMEM.RD".

       COPY "DEPART.RD".

       COPY "GARTEE.RD".

       COPY "PARAM.RD".

       COPY "SALE.RD".

       COPY "STKMEM.RD".

       COPY "STOCK.RD".

      /
       AD000-WRITE             SECTION.

       COPY "CONTROL.WR".

       COPY "DEBTOR.WR".

       COPY "DEPART.WR".

       COPY "GARTEE.WR".

       COPY "PARAM.WR".

       COPY "SALE.WR".

       COPY "STOCK.WR".

       COPY "STOCK.LUP".

       COPY "DEBTOR.ILU".

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
              MOVE W05-VAT (WS-VAT-SUB)
                                 TO W05-RATE.
             COMPUTE W10-VCASH ROUNDED
                   = W10-VCASH + (W10-VCASH * W05-RTE).
             COMPUTE W10-VWSALE ROUNDED
                   = W10-VWSALE + (W10-VWSALE * W05-RTE).
             COMPUTE W10-VSELL ROUNDED
                   = W10-VSELL + (W10-VSELL * W05-RTE).
       CA599.
             EXIT.

      /
       BB000               SECTION.
       BB00.
      DISPLAY "Enter client details " AT 5045
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "Y" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "/" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "N" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "? > <" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             MOVE "Y"            TO WS-OPTION.
       BB05.
            ACCEPT WS-OPTION AT 5072
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N" OR "Y")
               GO TO BB05.
           IF WS-OPTION = "N"
               GO TO BB20.
      PERFORM CLEAR-L50.
      PERFORM SAVE-SCREEN.
      MOVE SPACES  TO W40-NAME W40-ADD1 W40-ADD2
                                    W40-ADD3 W40-ADD4 W40-PC1.
             MOVE 0915           TO CPOS.
       BB10.
             DISPLAY
                 "                                                    "
                 AT CPOS.
           IF CLIN < 17
               ADD 1             TO CLIN
               GO TO BB10.
      MOVE 10   TO SHADE-ROW.
      MOVE 17   TO SHADE-COL.
      MOVE 50   TO SHADE-WIDTH.
      MOVE 8   TO SHADE-LINES.
             DISPLAY S30.
      PERFORM SCREEN-SHADOW.
       BB15.
             ACCEPT S31.
            DISPLAY WS-CHK AT 5018
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 5052
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF WS-OPTION = "N"
               GO TO BB15.
       BB20.
      PERFORM CLEAR-L50.
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
        WITH FOREGROUND-COLOR Brown HIGHLIGHT
      BACKGROUND-COLOR Grey
       "            (   )" WITH FOREGROUND-COLOR Grey HIGHLIGHT
           BACKGROUND-COLOR Grey.
       BB41.
             MOVE WS-S1          TO W100-SER.
      DISPLAY W100-SER AT LINE LIN COLUMN 74
       WITH FOREGROUND-COLOR Grey HIGHLIGHT
     BACKGROUND-COLOR Grey.
       BB42.
      ACCEPT W43-SERNO(WS-S1) AT LINE LIN COLUMN 62
      WITH FOREGROUND-COLOR Black
    BACKGROUND-COLOR Grey UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W43-SERIALS
       GO TO BB45
        END-EVALUATE.
      PERFORM CLEAR-L50.
            DISPLAY WS-CHK AT 5018
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 5052
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      PERFORM CLEAR-L50.
           IF WS-OPTION = "N"
               GO TO BB42.
           IF W43-SERNO (WS-S1) = SPACES
               GO TO BB42.
             MOVE STK-CODE       TO GUA-CODE.
             MOVE W43-SERNO (WS-S1)
                                 TO GUA-SERIAL.
             PERFORM READ-GARTEE THRU READ-GARTEE-EXIT.
           IF WS-F-ERROR = ZERO
        DISPLAY "Serial number duplicated" AT 5002
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
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
       BB100               SECTION.
       BB100-MARG.
             MOVE ZERO TO WS-ERROR.
           IF W90-TAX = SPACES
               IF STK-TAX NOT = ZERO
           COMPUTE W10-SPAPRC ROUNDED = (W10-SPAPRC /
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
            DISPLAY "Margin less than allowed - Override " AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
        "Y" WITH BACKGROUND-COLOR Cyan
          FOREGROUND-COLOR Brown HIGHLIGHT
        "/" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
        "N" WITH BACKGROUND-COLOR Cyan
          FOREGROUND-COLOR Brown HIGHLIGHT.
       BB105.
            ACCEPT WS-OPTION AT 5042
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N" OR "Y")
               GO TO BB105.
           IF WS-OPTION = "N"
               MOVE 1 TO WS-ERROR
               GO TO BB110.
       BB106.
            DISPLAY "Enter password >      <" AT 5051
              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT W40-CSHPASS AT 5067
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
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
      *    ****    I N V O I C E   A N D   C A S H   S A L E
      *
       BC000      SECTION 5.
       BC00.
      *
      *    ****    S U P E R V I S O R   P A S S W O R D S
      *
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-PW         TO W85-PASS.
       BC05.
      *
      *    ****    I N V O I C E   S C R E E N
      *
             MOVE "S"                TO CRT-TYPE.
             CALL "UTP\CRTHEAD" USING CRT-HEADINGS LS-USER-ID.
             MOVE 8              TO LIN.
             PERFORM BC115 UNTIL LIN > 18.
             MOVE W12-TODAY      TO W90-DTE.
             DISPLAY W90-DTE AT 0572 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE ZERO           TO W90-BDT W90-NONTAX W90-LDISC
                                    W90-BAL W90-TAXVAL W90-SALES
                                    W90-CUR W90-EXCTOT W90-ADVTAX.
             MOVE 1              TO WS-FIRST WS-LAST.
      MOVE 1   TO WS-VAT-SUB.
             MOVE SPACES         TO W90-TAX.
             MOVE "I"            TO WS-CR.
             MOVE "N"            TO WS-INPUT.
       BC10.
      *
      *    ****    D E B T O R   A C C O U N T   N U M B E R
      *
             MOVE SPACES         TO W80-ACNO.
       BC11.
            DISPLAY "F1" AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " to do Debtor Lookup"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             ACCEPT S36 AT 0308.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W80-ACNO
       GO TO BC11A
         WHEN F1-KEY
             PERFORM AA50D THRU AA50
   WHEN OTHER
       CALL X"E5"
       GO TO BC11
        END-EVALUATE
              DISPLAY S36 AT 0308
               IF W80-ACNO = SPACES
                   GO TO BC11.
            PERFORM CLEAR-L50.
       BC11A.
          IF W80-ACNO = SPACES
               GO TO BC599.
      MOVE 6   TO WS-LENGTH.
      CALL "CBL_TOUPPER" USING W80-ACNO
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
      MOVE 1   TO WS-LENGTH.
       BC12.
            PERFORM CLEAR-L50.
             PERFORM CA155-GET-DEBTOR.
           IF WS-ERROR = 1
              DISPLAY "No record" AT 5040
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
               GO TO BC11.
           IF WS-ERROR = 2
              DISPLAY "Invalid account" AT 5040
         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
               GO TO BC11.
       BC15.
      *
      *    ****    D I S P L A Y   D E B T O R   D E T A I L S
      *
      MOVE DEB-ACNO  TO W40-ACNO.
            MOVE DEB-NAME  TO W40-NAME.
           IF W80-TYPE = 1
               INSPECT W40-NAME REPLACING ALL "*" BY " "
               INSPECT W40-NAME REPLACING FIRST "!" BY " ".
             DISPLAY W40-NAME AT 0320 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             COMPUTE W100-CUR = DEB-BAL - (DEB-CUR + DEB-30 +
                                DEB-60 + DEB-90 + DEB-120 + DEB-INT).
      PERFORM BC189 THRU BC215.
      *       DISPLAY "Press any key to continue" AT 5002
      *        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
      *      ACCEPT WS-OPTION AT 5028
      *      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      PERFORM CLEAR-L50.
      *
      *    ****    I S   T H I S   T H E   C O R R E C T   A C C O U N T
      *
             PERFORM BC18-MEMO THRU BC18E.
      PERFORM SAVE-SCREEN-2.
      MOVE 8   TO SHADE-ROW.
      MOVE 23   TO SHADE-COL.
      MOVE 51   TO SHADE-WIDTH.
      MOVE 12   TO SHADE-LINES.
            DISPLAY S25.
      PERFORM SCREEN-SHADOW.
      *
      *    ****   C A T E G O R Y   8 0   -  S U S P E N D E D   A / C
      *
           IF DEB-CAT = 80
               DISPLAY "This account has been suspended - Press any key"
   AT 5002
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
        ACCEPT WS-OPTION AT 5050
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO
               GO TO BC05.
       BC16.
            DISPLAY "C" AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "ontinue "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "R" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "ekey or "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "E" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "xit > < - Press "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "F9" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " for Memo/Remarks"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             MOVE "C"            TO WS-OPTION.
       BC18.
            ACCEPT WS-OPTION AT 5036
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
    UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN NOT F9-KEY GO TO BC19
        END-EVALUATE
    ELSE
        GO TO BC19.
       BC18-MEMO.
            PERFORM CLEAR-L50.
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
      MOVE 7   TO SHADE-ROW.
      MOVE 19   TO SHADE-COL.
      MOVE 60   TO SHADE-WIDTH.
      MOVE 15   TO SHADE-LINES.
      DISPLAY S-MEM.
      PERFORM SCREEN-SHADOW.
      DISPLAY SA-MEM.
            DISPLAY "ENTER to continue" AT 5024
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 5042
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      PERFORM RESTORE-SCREEN.
       BC18E.
             EXIT.
       BC18-E.
             GO TO BC16.
       BC19.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "C" OR "E" OR "R")
               GO TO BC18.
      PERFORM RESTORE-SCREEN-2.
       BC20.
            PERFORM CLEAR-L50.
           IF WS-OPTION = "R"
               GO TO BC05.
           IF WS-OPTION = "E"
               GO TO BC599.
             MOVE "Y"            TO WS-OPTION.
      *
      *    ****    C H E C K   A R R E A R S   &   C R E D I T   L I M I T
      *
           IF (DEB-60 > ZERO) OR (DEB-90 > ZERO) OR (DEB-120 > ZERO)
              DISPLAY "Arrears > 30 days - Override Y/N" AT 5001
         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
              ACCEPT WS-OPTION AT 5034
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
            UPDATE AUTO
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
              IF WS-OPTION = "Y"
                   MOVE ZERO     TO WS-ERROR
                   PERFORM BB106
                   PERFORM BB108
                   IF WS-ERROR = 1
                       MOVE "N"  TO WS-OPTION.
            PERFORM CLEAR-L50.
           IF WS-OPTION NOT = "Y"
               GO TO BC05.
           IF DEB-CRL > ZERO
               IF DEB-BAL > DEB-CRL
           DISPLAY "Credit limit exceded - Override Y/N" AT 5001
             WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
           ACCEPT WS-OPTION AT 5038
           WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
          UPDATE AUTO
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
              IF WS-OPTION = "Y"
                   MOVE ZERO     TO WS-ERROR
                   PERFORM BB106
                   PERFORM BB108
                   IF WS-ERROR = 1
                       MOVE "N"  TO WS-OPTION.
            PERFORM CLEAR-L50.
           IF WS-OPTION NOT = "Y"
               GO TO BC05.
             MOVE DEB-BAL        TO WS-BAL.
           IF NOT VALID-PRICE
               MOVE "R"          TO DEB-PRICE.
             MOVE DEB-PRICE      TO WS-PRC.
       BC25.
             MOVE SPACES         TO W90-ORD W90-REG.
       BC30.
A19950      PERFORM CLEAR-L50.
             ACCEPT W90-ORD AT 0368 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                                         UPDATE AUTO.
      *
      *    ****    S A L E S M A N   N U M B E R
      *
            MOVE DEB-SMAN  TO W10-SMAN.
       BC35.
             ACCEPT W10-SMAN AT 0411 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                                          UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BC05
        END-EVALUATE.
           IF W10-SMAN > 50
               GO TO BC35.
             MOVE SPACES         TO W40-ASSIST.
           IF W10-SMAN = ZERO
              GO TO BC36.
             ADD 100 W10-SMAN    GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-SALESMAN = SPACES
              DISPLAY "Invalid Salesman Number" AT 5018
         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
               GO TO BC35.
            PERFORM CLEAR-L50.
            DISPLAY PAR-SALESMAN AT 0420
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE PAR-SALESMAN   TO W40-ASSIST.
       BC36.
    IF DEB-B-TEL = SPACES
        MOVE DEB-H-TEL  TO W40-TEL
    ELSE
        MOVE DEB-B-TEL  TO W40-TEL.
     DISPLAY W40-TEL AT 0466
      WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
       BC40.
           IF DEB-INV-TYPE = "E"
               MOVE "Y"      TO WS-OPTION
           ELSE
               MOVE "N"      TO WS-OPTION.
            DISPLAY "TAX Exclusive (" AT 5020
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "Y" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "/" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "N" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             ") or e"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "X" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "port?"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
       BC42.
            ACCEPT WS-OPTION AT 5051
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
          UPDATE AUTO.
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
             MOVE WS-OPTION      TO W90-EXP.
           IF WS-OPTION = "Y"
               MOVE "E"          TO W90-EXP.
           IF WS-OPTION = "N"
               MOVE SPACES       TO W90-TAX
               MOVE "I"          TO W90-EXP
           ELSE
              MOVE W05-VATNO  TO W90-TAX.
      MOVE DEB-CONTACT  TO W90-CONTACT.
            ACCEPT W90-CONTACT AT 0529
      WITH FOREGROUND-COLOR Cyan HIGHLIGHT
    UPDATE AUTO.
       BC43.
      PERFORM CLEAR-L50.
    IF WS-ADVALOREM = "N"
        MOVE "N"   TO W90-ADV
        GO TO BC45.
      DISPLAY "Ad Valorm tax (" AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "Y" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "/" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "N" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       ")? > <"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      MOVE "N"   TO WS-OPTION.
       BC44.
            ACCEPT WS-OPTION AT 5034
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
          UPDATE AUTO.
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
            PERFORM CLEAR-L50.
       BC45.
             ACCEPT W90-REG AT 0554 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                                         UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BC43
        END-EVALUATE.
            PERFORM CLEAR-L50.
            MOVE ZERO   TO W90-DB W90-CR.
       BC50.
             MOVE 8              TO LIN.
       BC55.
             MOVE SPACES         TO W10-DESC  W10-ITEM W10-SERNO.
             MOVE ZERO           TO W10-SELL WS-SUPER.
             MOVE 1              TO W10-QUANT.
       BC60.
             PERFORM AA300.
      *
      *    ****    G E T   S T O C K   C O D E
      *
            ACCEPT W10-ITEM AT LINE LIN COLUMN 6
      WITH FOREGROUND-COLOR Cyan HIGHLIGHT UPDATE AUTO.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       MOVE SPACES TO W10-ITEM
       GO TO BC60A
   WHEN F1-KEY
       MOVE "I"  TO WS-ETYPE
   WHEN F2-KEY
       MOVE "A"  TO WS-ETYPE
   WHEN F3-KEY
       MOVE "D"  TO WS-ETYPE
   WHEN F4-KEY
       MOVE "X"  TO WS-ETYPE
   WHEN OTHER
       CALL X"E5"
       GO TO BC60
        END-EVALUATE
        MOVE "S"   TO WS-LOOK
              PERFORM AA50
              DISPLAY W10-ITEM AT LINE LIN COLUMN 6
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               IF W10-ITEM = SPACES
                   GO TO BC60.
       BC60A.
          IF W10-ITEM = SPACES
               SUBTRACT 1        FROM WS-LAST
               GO TO BC120.
       BC61.
             PERFORM CA000.
           IF WS-F-ERROR = 22
        MOVE "Z"   TO WS-ETYPE
              PERFORM AA50
              DISPLAY W10-ITEM AT LINE LIN COLUMN 6
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               IF W10-ITEM = SPACES
                   GO TO BC60
               ELSE
                   GO TO BC61.
             MOVE WS-LAST        TO WS-SALKEY.
             INITIALIZE SAL-RECORD1.
             MOVE WS-LAST        TO SAL-CODE.
      *
      *    ****    I T E M   N U M B E R   O N   I N V O I C E
      *
            DISPLAY WS-LAST AT LINE LIN COLUMN 2
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE STK-CODE       TO SAL-ITEM.
            PERFORM CLEAR-L50.
      *
      *    ****    I S   I T   A   F R E E - F O R M A T   I T E M
      *
       BC65.
           IF STK-IND = 3
               MOVE ZERO         TO STK-AVGE
              ACCEPT W10-SDSC AT LINE LIN COLUMN 21
        WITH FOREGROUND-COLOR Cyan HIGHLIGHT UPDATE AUTO
              DISPLAY "Index Unit cost" AT 5012
         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
               ACCEPT S26-AVGE AT LINE 25
           ELSE
               MOVE STK-DESC     TO W10-DESC
              DISPLAY W10-SDSC AT LINE LIN COLUMN 21
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF STK-IND = 3
               IF STK-AVGE = ZERO
           DISPLAY "Unit cost is zero" AT 5012
             WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                   GO TO BC65.
            PERFORM CLEAR-L50.
      *
      *    ****    L O G   D E T A I L S   T O   S A L E S   F I L E
      *
             MOVE W10-ITEM       TO SAL-ITEM.
             MOVE W10-DESC       TO SAL-MSER.
             MOVE STK-AVGE       TO SAL-COST W10-COST.
      *
      *    ****    G E T   Q U A N T I T Y
      *
       BC66.
             ACCEPT S26-QNT AT LINE LIN.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             DISPLAY WS-MIV78 AT LINE LIN COLUMN 2
        WITH FOREGROUND-COLOR Cyan
             GO TO BC55
   END-EVALUATE.
    IF (W10-QUANT = ZERO)
              DISPLAY WS-MIV78 AT LINE LIN COLUMN 2
         WITH FOREGROUND-COLOR Cyan
               GO TO BC55.
            PERFORM CLEAR-L50.
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
    IF NOT (PRG-NUMBER = 03)
        GO TO BC69.
      MOVE STK-COST TO W100-COST.
             MOVE STK-AVGE TO W100-AVRG.
       BC68.
           IF (W90-TAX NOT = SPACES) AND (STK-TAX > 0)
               MOVE W05-VAT (STK-TAX)
                                 TO W05-RATE
               COMPUTE WS-PER = ((W10-SELL - ((W10-SELL /
                      (100.0000 + W05-RATE)) * W05-RATE)
                       - STK-COST) / STK-COST) * 100.000 + .005
           ELSE
               COMPUTE WS-PER = ((W10-SELL - STK-COST) / STK-COST)
                                  * 100.000 + .005.
      DISPLAY "Cost: " AT 5001
                     W100-COST WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                     " Average: "
                     W100-AVRG WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                     " Markup: " WS-PER WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
       BC69.
            MOVE W10-SELL  TO W10-SPAPRC.
       BC70.
             ACCEPT S26-PRC AT LINE LIN.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
       GO TO BC66
        END-EVALUATE.
    IF NOT (PRG-NUMBER = 03)
        GO TO BC74.
      PERFORM BC68.
      DISPLAY "C" AT 5054 WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "ontinue or " WITH FOREGROUND-COLOR Grey
                     "R" WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     "ekey > <" WITH FOREGROUND-COLOR Grey.
             MOVE "C"            TO WS-OPTION.
       BC71.
      ACCEPT WS-OPTION AT 5073
                    WITH FOREGROUND-COLOR Grey HIGHLIGHT UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "C" OR "R")
        GO TO BC71.
           IF WS-OPTION = "R"
              GO TO BC70.
            PERFORM CLEAR-L50.
            MOVE W10-SELL  TO W10-SPAPRC.
       BC74.
    IF W10-SELL < W10-SPAPRC
              MOVE W10-SELL  TO W10-SPAPRC
              PERFORM BB100
              IF WS-ERROR = 1
           MOVE ZERO  TO WS-ERROR
           GO TO BC70.
            PERFORM CLEAR-L50.
       BC75.
             MOVE W10-SELL       TO SAL-SELL.
       BC76.
           IF STK-IND > 0
               GO TO BC80.
      *
      *    ****    C H E C K   S T O C K   Q U A N T I T I E S
      *
           IF STK-QUANT = ZERO
              DISPLAY "No stock - Continue (Y/N) > <" AT 5012
         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
              DISPLAY "'Y' will make stock holding negative" AT 5042
         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
              ACCEPT WS-OPTION AT 5039
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO
        CALL "CBL_TOUPPER" USING WS-OPTION
      BY VALUE WS-LENGTH
      RETURNING WS-STATUS
              PERFORM CLEAR-L50
               IF WS-OPTION = "N"
                   MOVE W10-QUANT
                                 TO W10-REORD
                   GO TO BC55.
           IF (W10-QUANT NOT > STK-QUANT) OR
              (STK-QUANT NOT > ZERO)
               GO TO BC80.
             SUBTRACT STK-QUANT FROM W10-QUANT
                                 GIVING W10-REORD.
             MOVE STK-QUANT      TO W100-BAL.
            DISPLAY "Insufficient Stock " AT 5002
              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
              W100-BAL
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
              " Accept (Enter/N) ?"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 5052
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
            PERFORM CLEAR-L50.
           IF WS-OPTION = "N"
               MOVE W10-QUANT    TO W10-REORD
               GO TO BC55.
             MOVE STK-QUANT      TO W10-QUANT.
             ACCEPT S26-QNT AT LINE LIN.
       BC80.
      *
      *    ****    C A L C U L A T E   E X T E N D E D   V A L U E
      *
             MOVE W10-QUANT      TO SAL-QNT.
             COMPUTE W25-VALUE ROUNDED = (1.000 * W10-SELL * W10-QUANT).
             MOVE W25-VALUE      TO W100-EXT SAL-VAL.
            DISPLAY W100-EXT AT LINE LIN COLUMN 70
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      *
      *    ****    L I N E   D I S C O U N T   ( PERCENTAGE )
      *
            DISPLAY "Discount %" AT 5018
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE ZERO           TO WS-DISC W90-CDISC.
           IF DEB-DISC > ZERO
               MOVE STK-LEDG     TO WS-DEPKEY
               PERFORM READ-DEPART THRU READ-DEPART-EXIT
               MOVE DPT-DSETT (DEB-DISC)
                                 TO WS-DISC.
       BC85.
             ACCEPT S26-DSC AT LINE 25.
    IF USER-FUNC
        EVALUATE KEY-CODE-1
   WHEN ESC-KEY
             PERFORM CLEAR-L50
       GO TO BC70
        END-EVALUATE.
           IF WS-DISC = ZERO
              PERFORM CLEAR-L50
               GO TO BC90.
           IF DEB-DISC > ZERO
               IF WS-DISC > DPT-DSETT (DEB-DISC)
           DISPLAY "Enter password >      <" AT 5051
             WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
           ACCEPT W40-CSHPASS AT 5067
     WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
          AUTO NO-ECHO
                   MOVE 6 TO WS-LENGTH
     CALL "CBL_TOUPPER" USING W40-CSHPASS
          BY VALUE WS-LENGTH
          RETURNING WS-STATUS
                   MOVE 1 TO WS-LENGTH
                   IF W40-CSHPASS NOT = W85-SUPER (1)
                       MOVE DPT-DSETT (DEB-DISC)
                                 TO WS-DISC.
             COMPUTE W10-SPAPRC ROUNDED = W10-SELL -
                    (W10-SELL * WS-DISC / 100.00000).
      *
      *    ****    C H E C K   M A R G I N
      *
           IF DEB-DISC > ZERO
               IF DPT-DSETT (DEB-DISC) = WS-DISC
                   GO TO BC86.
             PERFORM BB100.
           IF WS-ERROR = 1
               GO TO BC85.
       BC86.
            PERFORM CLEAR-L50.
             MOVE WS-DISC        TO SAL-DSC.
            COMPUTE W90-CDISC =
                   ((W25-VALUE * WS-DISC) / 100.00000).
            MOVE W90-CDISC  TO W100-PRC SAL-DISC.
            DISPLAY W100-PRC AT 5040
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT.
       BC90.
      *
      *    ****    A R E   T H E   D E T A I L S   C O R R E C T
      *
       BC95.
      *
      *    ****    A C C U M U L A T E   TOTAL COST / TOTAL DISCOUNT /
      *            VALUES SUBJECT TO TAX AND NON-TAX.
      *
           IF (STK-IND < 2) OR (STK-IND = 3)
               COMPUTE W90-BAL = W90-BAL + (W10-QUANT * W10-COST).
             ADD W90-CDISC       TO W90-LDISC.
             SUBTRACT W90-CDISC FROM W25-VALUE
                                 GIVING W90-VALUE.
           IF (STK-TAX = 0) OR (W90-EXP = "X")
               ADD W90-VALUE     TO W90-NONTAX
               MOVE W90-VALUE    TO SAL-NONTAX
           ELSE
               MOVE W90-VALUE    TO SAL-TAXVAL
               ADD W90-VALUE     TO W90-TAXVAL.
           IF W90-TAX NOT = SPACES
               COMPUTE SAL-VAT ROUNDED = SAL-TAXVAL * W05-RTE.
      *
      *    ****    A C C U M U L A T E   T O T A L   S A L E
      *
             ADD W25-VALUE       TO W90-DB.
       BC100.
             MOVE W90-DB         TO W90-TOTAL.
             SUBTRACT W90-LDISC  FROM W90-TOTAL.
            MOVE W90-DB  TO W100-VAL.
            DISPLAY W100-VAL AT 2015 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MULTIPLY -1 BY W90-LDISC
           GIVING W100-VAL.
            DISPLAY W100-VAL AT 2115 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      *
      *    ****    C A L C U L A T E   T A X   ( RUNNING TOTAL )
      *
           IF W90-ADV = "Y"
               COMPUTE W90-ADVTAX ROUNDED =
                      (W90-TOTAL * WS-ADV-RATE / 100.00000).
           IF W90-TAX = SPACES
               GO TO BC102.
            MOVE W05-VAT (WS-VAT-SUB)
     TO W05-RATE.
           IF W90-TAXVAL NOT = ZERO
               COMPUTE W25-TOTAL ROUNDED =
                     ((W90-TAXVAL + W90-ADVTAX) * W05-RTE)
              MOVE W25-TOTAL  TO W100-VAL
              ADD W25-TOTAL  TO W90-TOTAL
    ELSE
        MOVE ZERO  TO W100-VAL.
            DISPLAY W100-VAL AT 2215 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
       BC102.
           IF W90-ADV = "Y"
               MOVE W90-ADVTAX   TO W100-BAL
               DISPLAY W100-BAL AT 2241 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               ADD W90-ADVTAX    TO W90-TOTAL.
            MOVE W90-TOTAL  TO W100-VAL.
            DISPLAY W100-VAL AT 2315 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
       BC105.
            DISPLAY "A" AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "ccept, "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "C" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "ancel > <"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             MOVE "A"            TO WS-OPTION.
       BC110.
      *
      *    ****    M U S T   T H E   L A S T   E N T R Y   B E
      *            A C C E P T E D.
      *
            ACCEPT WS-OPTION AT 5028
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
          UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "A" OR "C")
               GO TO BC110.
            PERFORM CLEAR-L50.
           IF WS-OPTION = "C"
              DISPLAY WS-MIV78 AT LINE LIN COLUMN 2
         WITH FOREGROUND-COLOR Cyan
               PERFORM BC114
               PERFORM BC100 THRU BC102
               GO TO BC55.
       BC111.
             MOVE WS-SUPER       TO SAL-SUPER.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
             ADD 1               TO WS-LAST.
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
             MOVE WS-LAST        TO WS-SALKEY.
             PERFORM WRITE-SALE THRU WRITE-SALE-EXIT.
             ADD 1               TO WS-LAST.
       BC111D.
             ADD 1               TO WS-S1.
           IF WS-S1 NOT > WS-S2
               GO TO BC111A.
       BC111E.
      *
      *    ****    T E S T   I F   S C R E E N   F U L L
      *
           IF LIN < 18
               ADD 1             TO LIN
               GO TO BC112.
             MOVE 8              TO LIN.
             PERFORM BC115 UNTIL LIN > 18.
             GO TO BC50.
       BC112.
            DISPLAY "R" AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "emarks, "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "N" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "ext or "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "E" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "nd > <"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             MOVE WS-INPUT       TO WS-OPTION.
            ACCEPT WS-OPTION AT 5034
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
          UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "R" OR "N" OR "E")
               GO TO BC112.
            PERFORM CLEAR-L50.
             MOVE WS-OPTION      TO WS-INPUT.
           IF WS-OPTION = "N"
               GO TO BC55.
           IF WS-OPTION = "E"
               SUBTRACT 1        FROM WS-LAST
               GO TO BC120.
             MOVE SPACES         TO W10-REMKS W43-SERNO (1).
            ACCEPT (LIN, 21) W10-REMKS WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                                             UPDATE AUTO.
           IF W10-REMKS = SPACES
               GO TO BC112.
            DISPLAY WS-LAST AT LINE LIN COLUMN 2
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE WS-LAST        TO WS-SALKEY.
             INITIALIZE SAL-RECORD1.
             MOVE WS-LAST        TO SAL-CODE.
             MOVE W10-REMKS      TO SAL-MSER.
             GO TO BC111.
       BC114.
             SUBTRACT W25-VALUE  FROM W90-DB.
             SUBTRACT W90-CDISC  FROM W90-LDISC.
             SUBTRACT SAL-NONTAX FROM W90-NONTAX.
             SUBTRACT SAL-TAXVAL FROM W90-TAXVAL.
           IF (STK-IND < 2) OR (STK-IND = 3)
               COMPUTE W90-BAL = W90-BAL - (W10-COST * W10-QUANT).
       BC115.
            DISPLAY WS-MIV1 AT LINE LIN COLUMN 1
       WITH BACKGROUND-COLOR Black FOREGROUND-COLOR Cyan
       WS-MIV78 WITH FOREGROUND-COLOR Cyan
       WS-MIV80 WITH BACKGROUND-COLOR Black
       FOREGROUND-COLOR Cyan.
             ADD 1               TO LIN.
       BC120.
      *
      *    ****    W A S   T H E R E   A N   E N T R Y
      *
           IF WS-LAST = ZERO
               GO TO BC05.
      *
      *    ****    P R O C E S S   T H E   S A L E
      *
       BC125.
            PERFORM CLEAR-L50.
            DISPLAY "P" AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "rocess Invoice, "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "A" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "mend sales, "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "C" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "ancel or "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "M" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             "ore items"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
       BC130.
            ACCEPT WS-OPTION AT 5063
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "P" OR "A" OR "C" OR "M")
               GO TO BC130.
            PERFORM CLEAR-L50.
           IF WS-OPTION = "M"
               ADD 1             TO WS-LAST
               GO TO BC112.
           IF WS-OPTION = "A"
               PERFORM BC270 THRU BC335
               GO TO BC125.
           IF WS-OPTION = "C"
               GO TO BC599.
             MOVE ZERO           TO W90-STOTAL WS-TDISC W90-TDISC.
           IF W90-LDISC NOT = ZERO
               MOVE W90-LDISC    TO W90-TDISC
               GO TO BC140.
             MOVE W90-DB         TO W90-DVAL.
             GO TO BC140.
      *
      *     VERSION 6.0  - The following paragraph has been bypassed as
      *                    discount is only allowed per item at this
      *                    stage.
      *
       BC135.
      *
      *    ****    I F   N O   L I N E   D I S C O U N T   -   T H E N
      *            A L L O W   F O R   D I S C O U N T   O N   T H E
      *            I N V O I C E   T O T A L
      *
      *      ACCEPT S26-TDSC AT LINE 21.
      *      DISPLAY WS-CHK AT 5018 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
      *      ACCEPT WS-OPTION AT 5052 WITH FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      *     CALL "CBL_TOUPPER" USING WS-OPTION
      *   BY VALUE WS-LENGTH
      *   RETURNING WS-STATUS.
      *      PERFORM CLEAR-L50.
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
       BC140.
             SUBTRACT W90-TDISC FROM W90-DB
                                 GIVING W90-TSELL W90-TOTAL.
             MULTIPLY -1 BY W90-TDISC
           GIVING W100-VAL.
            DISPLAY W100-VAL AT 2115 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF W90-ADV = "Y"
               COMPUTE W90-ADVTAX ROUNDED =
                      (W90-TOTAL * WS-ADV-RATE / 100.000000).
             MOVE ZERO           TO W90-TTAX.
           IF (W90-TAX = SPACES) OR (W90-TAXVAL = ZERO)
               GO TO BC142.
            MOVE W05-VAT (WS-VAT-SUB)
     TO W05-RATE.
             COMPUTE W90-TTAX ROUNDED = ((W90-TAXVAL + W90-ADVTAX)
                                          * W05-RTE).
            MOVE W90-TTAX  TO W100-VAL.
            DISPLAY W100-VAL AT 2215 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             ADD W90-TTAX        TO W90-TOTAL.
       BC142.
           IF W90-ADV = "Y"
               MOVE W90-ADVTAX   TO W100-BAL
               DISPLAY W100-BAL AT 2241 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               ADD W90-ADVTAX    TO W90-TOTAL.
       BC185.
      *
      *    ****    M U S T   T H E   V A L U E   B E   A G E D
      *
            PERFORM CLEAR-L50.
             MOVE "M" TO WS-AGE.
      DISPLAY "Print Invoice " AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "Y" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "/" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "N" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       " > <" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             MOVE "Y"            TO WS-PRNINV.
       BC186.
            ACCEPT WS-PRNINV AT 5021
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
    UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-PRNINV
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-PRNINV = "Y" OR "N")
        GO TO BC186.
      MOVE WS-PRNINV  TO W40-PRNINV.
      MOVE WS-LAST  TO W40-LAST.
            PERFORM CLEAR-L50.
      *
      *    ****    P R O C E S S   T H E   S A L E
      *
    IF DEB-TERMS = "3"
        PERFORM BB000.
            DISPLAY "Processing Invoice" AT 5002 WITH
              BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT BLINK.
            CLOSE SALE.
             OPEN I-O SALE.
      MOVE W10-SMAN  TO W40-SMAN.
      MOVE WS-VAT-SUB  TO W40-VAT-SUB.
      MOVE WS-PRC  TO W40-PRC.
      MOVE "N"   TO W40-REPRINT.
      MOVE WS-CR   TO W40-CR.
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
      PERFORM CLEAR-L50.
      DISPLAY "Program : " AT 5002
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Blue
        PRG-NAME
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Brown
      HIGHLIGHT
       " not on disk, press "
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Blue
       "ANY" WITH BACKGROUND-COLOR Cyan
    FOREGROUND-COLOR Brown
    HIGHLIGHT
       " key to continue"
        WITH BACKGROUND-COLOR Cyan
      FOREGROUND-COLOR Blue.
      CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
       BC188.
            PERFORM CLEAR-L50.
             CLOSE SALE.
             OPEN OUTPUT SALE.
             CLOSE SALE.
             OPEN I-O SALE.
            DISPLAY "ENTER" AT 5012
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " to continue - "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
             "E" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
             " to exit"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
            ACCEPT WS-OPTION AT 5042
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF WS-OPTION = "E"
               GO TO BC599.
             GO TO BC05.
       BC189.
      *
      *    ****    T E S T   F O R   I N D I V I D U A L
      *
           IF W80-TYPE NOT = 1
               GO TO BC210.
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
       BC190.
           IF W60-NCHAR (WS-S6) NOT = SPACE
               GO TO BC195.
             SUBTRACT 1          FROM WS-S6.
           IF WS-S6 > 0
               GO TO BC190.
       BC195.
      *
      *    ****    F I R S T   I N I T I A L
      *
             MOVE 1              TO WS-S5.
             ADD 2               TO WS-S6.
             MOVE W40-NCHAR (25) TO W60-NCHAR (WS-S6).
             ADD 1               TO WS-S6.
             MOVE "."            TO W60-NCHAR (WS-S6).
           IF W40-NCHAR (49) = SPACE
               GO TO BC200.
      *
      *    ****    S E C O N D   I N I T I A L
      *
             ADD 1               TO WS-S6.
             MOVE W40-NCHAR (49) TO W60-NCHAR (WS-S6).
             ADD 1               TO WS-S6.
             MOVE "."            TO W60-NCHAR (WS-S6).
       BC200.
             ADD 2               TO WS-S6.
       BC205.
      *
      *    ****    S U R N A M E
      *
             MOVE W40-NCHAR (WS-S5)
                                 TO W60-NCHAR (WS-S6).
           IF WS-S5 < 24
               ADD 1             TO WS-S5
               ADD 1             TO WS-S6
               GO TO BC205.
             MOVE W60-NAME       TO W40-NAME.
       BC210.
             MOVE SPACES         TO W40-PC1 W40-PC2.
             MOVE DEB-ADDRESS    TO W60-ADDRESS.
             PERFORM CA175-ADDRESS.
           IF NOT (DEB-POST = ZERO OR 9999)
               MOVE DEB-POST     TO W40-PC1.
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
             MOVE DEB-PADDR      TO W60-ADDRESS1.
             PERFORM CA185-ADDRESS.
           IF NOT (DEB-PPOST = ZERO OR 9999)
               MOVE DEB-PPOST    TO W40-PC2.
           IF W40-PADD4 = SPACES
               MOVE W40-PC2      TO W40-PADD4
               MOVE SPACES       TO W40-PC2.
           IF W40-PADD3 = SPACES
               MOVE W40-PADD4    TO W40-PADD3
               MOVE SPACES       TO W40-PADD4.
           IF W40-PADD2 = SPACES
               MOVE W40-PADD3    TO W40-PADD2
               MOVE SPACES       TO W40-PADD3.
           IF W40-PADD1 = SPACES
               MOVE W40-PADD2    TO W40-PADD1
               MOVE SPACES       TO W40-PADD2.
       BC215.
        EXIT.
      *
      *    ****    A M E N D   S A L E S   D E T A I L S
      *
       BC270.
      DISPLAY "Amend " AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "H" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "eader, "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "S" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "ales details or "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "E" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "xit > <"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             MOVE "S"            TO WS-OPTION.
       BC271.
      ACCEPT WS-OPTION AT 5039
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
    UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "E" OR "H" OR "S")
               GO TO BC271.
      PERFORM CLEAR-L50.
           IF WS-OPTION = "S"
               GO TO BC272
           ELSE
           IF WS-OPTION = "E"
               GO TO BC335.
       BC271A.
      DISPLAY "Amend " AT 5002
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "O" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "rd No, "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "R" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "ef No, "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "S" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "alesman or "
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
       "E" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
       "xit > <"
        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
             MOVE SPACE          TO WS-OPTION.
       BC271B.
      ACCEPT WS-OPTION AT 5042
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
    UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
    IF NOT (WS-OPTION = "E" OR "O" OR "R" OR "S")
               GO TO BC271B.
      PERFORM CLEAR-L50.
           IF WS-OPTION = "E"
               GO TO BC335.
           IF WS-OPTION = "O"
               ACCEPT W90-ORD AT 0368
                      WITH FOREGROUND-COLOR Cyan HIGHLIGHT UPDATE AUTO
               GO TO BC271A
           ELSE
           IF WS-OPTION = "R"
               ACCEPT W90-REG AT 0554
                      WITH FOREGROUND-COLOR Cyan HIGHLIGHT UPDATE AUTO
               GO TO BC271A.
       BC271C.
             ACCEPT W10-SMAN AT 0411 WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                                          UPDATE AUTO.
           IF W10-SMAN > 50
               GO TO BC271C.
             MOVE SPACES         TO W40-ASSIST.
      MOVE W10-SMAN  TO W40-SMAN.
           IF W10-SMAN = ZERO
               DISPLAY "                              "  AT 0420
                       WITH FOREGROUND-COLOR Cyan
               GO TO BC271A.
            PERFORM CLEAR-L50.
             ADD 100 W10-SMAN    GIVING WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF PAR-SALESMAN = SPACES
              DISPLAY "Invalid Salesman Number" AT 5018
         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
               GO TO BC271C.
            PERFORM CLEAR-L50.
             DISPLAY PAR-SALESMAN AT 0420 WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE PAR-SALESMAN   TO W40-ASSIST.
             GO TO BC271A.
       BC272.
             MOVE 8              TO LIN.
             PERFORM BC115 UNTIL LIN > 18.
             MOVE 8              TO LIN.
             MOVE 1              TO WS-LINES WS-FIRST.
       BC275.
             MOVE WS-FIRST TO WS-SALKEY.
       BC280.
      *
      *    ****    H A V E   A L L   T H E   E N T R I E S   B E E N
      *            D I S P L A Y E D
      *
           IF WS-SALKEY > WS-LAST
               GO TO BC285.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
      *
      *    ****    D I S P L A Y   E N T R Y   D E T A I L S
      *
            DISPLAY SAL-CODE AT LINE LIN COLUMN 2
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
           IF SAL-ITEM = SPACES
              DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
         WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               GO TO BC282.
            DISPLAY SAL-ITEM AT LINE LIN COLUMN 6
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
            DISPLAY SAL-SDSC AT LINE LIN COLUMN 21
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             MOVE SAL-QNT        TO W10-QUANT.
             DISPLAY S26-QNT AT LINE LIN.
             MOVE SAL-SELL       TO W10-SELL.
             DISPLAY S26-PRC AT LINE LIN.
             MOVE SAL-VAL        TO W100-EXT.
            DISPLAY W100-EXT AT LINE LIN COLUMN 70
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      *
      *    ****    I S   T H E   S C R E E N   F U L L
      *
       BC282.
           IF LIN < 18
               ADD 1             TO LIN WS-SALKEY WS-LINES
               GO TO BC280.
       BC285.
      *
      *    ****    D I S P L A Y   O P T I O N S
      *
           IF WS-LINES < 11
               IF WS-FIRST = 1
           DISPLAY "A" AT 5020
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
      "mend, "
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
      "D" WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT
      "elete an Item - "
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
      "E" WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT
      "xit > <"
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
               ELSE
           DISPLAY "A" AT 5002
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
      "mend, "
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
      "D" WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT
      "elete an Item - "
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
      "P" WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT
      "revious screen - "
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
      "E" WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT
      "xit > <"
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                   MOVE 2        TO WS-LINES
           ELSE
           IF WS-SALKEY < WS-LAST
               IF WS-FIRST = 1
           DISPLAY "A" AT 5006
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
      "mend, "
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
      "D" WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT
      "elete an Item - "
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
      "N" WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT
      "ext screen - "
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
      "E" WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT
      "xit > <"
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                   MOVE 1        TO WS-LINES
               ELSE
           DISPLAY "A" AT 5004
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
      "mend, "
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
      "D" WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT
      "elete - "
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
      "N" WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT
      "ext, "
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
      "P" WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT
      "revious screen - "
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
      "E" WITH BACKGROUND-COLOR Cyan
        FOREGROUND-COLOR Brown HIGHLIGHT
      "xit > <"
       WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                   MOVE 3        TO WS-LINES
           ELSE
               MOVE 2            TO WS-LINES
              DISPLAY "A" AT 5002
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
         "mend, "
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
         "D" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
         "elete an Item - "
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
         "P" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
         "revious screen - "
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
         "E" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
         "xit > <"
   WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
       BC290.
            ACCEPT WS-OPTION AT 5050
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "A" OR "D" OR "E" OR "N" OR "P")
               GO TO BC290.
           IF WS-OPTION = "E"
               GO TO BC335.
           IF WS-OPTION = "A" OR "D"
               GO TO BC300.
      *
      *    ****    I S   P R E V I O U S   S C R E E N   R E Q U I R E D
      *
           IF WS-LINES = 2 OR 3
               IF WS-OPTION = "P"
                   PERFORM BC295
           PERFORM CLEAR-L50
                   SUBTRACT 11   FROM WS-FIRST
                   GO TO BC275.
      *
      *    ****    I S   N E X T   S C R E E N   R E Q U I R E D
      *
           IF WS-LINES = 1 OR 3
               IF WS-OPTION = "N"
                   PERFORM BC295
           PERFORM CLEAR-L50
                   ADD 11        TO WS-FIRST
                   GO TO BC275.
            GO TO BC285.
       BC295.
      *
      *    ****    C L E A R   S C R E E N
      *
             MOVE 8              TO LIN.
             PERFORM BC115 UNTIL LIN > 18.
             MOVE 8              TO LIN.
             MOVE 1              TO WS-LINES.
       BC300.
      *
      *    ****    M U S T   A N   I T E M   B E   A M E N D E D
      *
           IF WS-OPTION = "A"
               GO TO BC310.
      *
      *    ****    M A K E   S U R E   T H A T   A N   I T E M
      *            M U S T   B E   D E L E T E D
      *
            PERFORM CLEAR-L50.
            DISPLAY "Are you sure Y/N?" AT 5001
              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE "N"            TO WS-OPTION.
       BC305.
            ACCEPT WS-OPTION AT 5019
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
          UPDATE AUTO.
      CALL "CBL_TOUPPER" USING WS-OPTION
    BY VALUE WS-LENGTH
    RETURNING WS-STATUS.
          IF NOT (WS-OPTION = "N" OR "Y")
               GO TO BC305.
            PERFORM CLEAR-L50.
           IF WS-OPTION = "N"
              GO TO BC285.
             MOVE "D"            TO WS-OPTION.
       BC310.
            PERFORM CLEAR-L50.
            DISPLAY "Enter item No to Amend/Delete >  <" AT 5001
              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE ZERO           TO WS-INO.
       BC315.
      *
      *    ****    G E T   T H E   N U M B E R   O F   T H E   I T E M
      *            T H A T   M U S T   B E   A M E N D E D   O R
      *            D E L E T E D
      *
            ACCEPT WS-INO AT 5032
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT
                                        UPDATE AUTO.
           IF WS-INO = ZERO
              PERFORM CLEAR-L50
              GO TO BC285.
           IF WS-INO > WS-SALKEY
               GO TO BC315.
           IF WS-INO < WS-FIRST
               GO TO BC315.
      *
      *    ****    C A L C U L A T E   T H E   L I N E   N U M B E R
      *            O N   W H I C H   T H E   I T E M   I S
      *            D I S P L A Y E D
      *
           IF WS-INO < 12
               ADD 7 WS-INO      GIVING LIN
           ELSE
               COMPUTE LIN = WS-INO - WS-FIRST + 8.
      *
      *    ****    R E A D   T H E   D E T A I L S   F R O M   T H E
      *            S A L E   F I L E
      *
             MOVE WS-INO         TO WS-SALKEY.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
           IF SAL-ITEM = SPACES
               GO TO BC316.
             MOVE SAL-ITEM       TO W10-ITEM STK-CODE.
             PERFORM READ-STOCK THRU READ-STOCK-EXIT.
             MOVE SAL-QNT        TO W10-QUANT.
             MOVE SAL-COST       TO W10-COST.
             MOVE SAL-SELL       TO W10-SELL.
             MOVE SAL-DISC       TO W90-CDISC.
             MOVE SAL-DSC        TO WS-DISC.
             MOVE SAL-VAL        TO W25-VALUE.
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
       BC316.
           IF WS-OPTION = "A"
               GO TO BC325.
      *
      *    ****    R E - D I S P L A Y   T O T A L S
      *
           IF SAL-ITEM NOT = SPACES
               PERFORM BC100 THRU BC102.
       BC320.
      *
      *    ****    D E L E T E   I T E M   F R O M   S A L E   F I L E
      *
             MOVE WS-SALKEY      TO WS-SALSTR.
             ADD 1               TO WS-SALKEY.
           IF WS-SALKEY > WS-LAST
               SUBTRACT 1        FROM WS-LAST
               GO TO BC272.
             PERFORM READ-SALE THRU READ-SALE-EXIT.
             SUBTRACT 1          FROM SAL-CODE.
             MOVE WS-SALSTR      TO WS-SALKEY.
             PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
             ADD 1               TO WS-SALKEY.
             GO TO BC320.
       BC325.
           IF SAL-ITEM = SPACES
              ACCEPT (LIN, 21) SAL-SDSC WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                                              UPDATE AUTO
               GO TO BC332.
      *
      *    ****    A M E N D   Q U A N T I T Y
      *
             ACCEPT S26-QNT AT LINE LIN.
            PERFORM CLEAR-L50.
           IF W10-QUANT = ZERO
              DISPLAY "Quantity MAY NOT be zero" AT 5001
         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
               MOVE SAL-QNT      TO W10-QUANT
               GO TO BC325.
      *
      *    ****    R E C A L C U L A T E   V A L U E S
      *
             MULTIPLY W10-SELL BY W10-QUANT
                                 GIVING W25-VALUE.
             MOVE W25-VALUE      TO W100-EXT.
            DISPLAY W100-EXT AT LINE LIN COLUMN 70
       WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
      *
      *    ****    U P D A T E   S A L E   F I L E   R E C O R D
      *
             MOVE W10-QUANT      TO SAL-QNT.
             MOVE W25-VALUE      TO SAL-VAL.
           IF WS-DISC = ZERO
               GO TO BC330.
             COMPUTE W90-CDISC = (W25-VALUE * WS-DISC) / 100.
             MOVE W90-CDISC      TO SAL-DISC.
       BC330.
      *
      *    ****    R E - C A L C U L A T E   &   D I S P L A Y
      *            T O T A L S
      *
             PERFORM BC95 THRU BC102.
       BC332.
             PERFORM REWRITE-SALE THRU WRITE-SALE-EXIT.
             GO TO BC270.
       BC335.
             EXIT.
       BC599.
             EXIT.
      /
       ZA000-INIT        SECTION 9.
       ZA000-OPEN.
            MOVE WS-COMPANY  TO WS-TOP-COMP.
      MOVE LS-L-OR-N  TO W02-L-OR-N.

      MOVE WS-SYS-ID  TO W02-SYSID.

    IF LS-USER = WS-SYS-ID
        MOVE "SupervisorÄ"  TO WS-WRKHD
    ELSE
        MOVE "Workstation"  TO WS-WRKHD
        MOVE LS-USER    TO WS-WRKST.
            PERFORM ERASE-SCREEN.
      *
      *    ****    D O  N O T  D I S P L A Y  E R R O R
      *     M E S S A G E S
      *
      MOVE 44   TO USER-NUMBER.
      MOVE 1   TO USER-SETTING.
      CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

             COPY "FUNCTION.PRO".

       ZA00A-CONTINUE.
            MOVE 3   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      MOVE PAR-INV  TO PRG-NUMBER.
            MOVE PAR-VAT-REG  TO W05-VATNO.
           IF PAR-POS = "8"
               MOVE "N"          TO WS-BYPASS.
             OPEN OUTPUT SALE.
             CLOSE SALE.
             OPEN I-O SALE.
             MOVE 1              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-DMY        TO W12-TODAY.
             MOVE PAR-YMD        TO W12-T-YMD.
             MOVE 5              TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
    IF PAR-ADVALOREM NOT = "Y"
        MOVE "N"   TO WS-ADVALOREM
    ELSE
        MOVE "Y"   TO WS-ADVALOREM.
             MOVE PAR-ADV-RATE   TO WS-ADV-RATE.
             MOVE PAR-VAT-RATES  TO W05-VAT-RATES.
      MOVE PAR-RATE-DATE  TO WS-VAT-DATE.
            MOVE PAR-DISCOUNT  TO W15-DISCOUNT.
             GO TO ZA999.
       ZA49.
             DISPLAY "Too many files OPEN" AT 0812
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             DISPLAY "Check the FILES option in CONFIG.SYS" AT 1012.
             DISPLAY "Press any key to continue" AT 1212.
             ACCEPT WS-OPTION AT 1238 WITH FOREGROUND-COLOR Grey HIGHLIGHT.
             GO TO ZA205.
       ZA200.
             DISPLAY "Files locked - Try later" AT 2312
                      WITH FOREGROUND-COLOR Brown HIGHLIGHT
                     " " WS-STATUS WITH FOREGROUND-COLOR Grey HIGHLIGHT.
            DISPLAY "Press any key" AT 5012
              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT.
            ACCEPT WS-OPTION AT 5026
      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Grey HIGHLIGHT AUTO.
       ZA205.
             EXIT PROGRAM.
       ZA999.
             EXIT.

       I-O-ERRORS      SECTION 9.
       OPEN-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Open error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM DISPLAY-FILE-NAME.
      
       READ-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Read error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM DISPLAY-FILE-NAME.
      
       WRITE-ERROR.
             PERFORM ERASE-SCREEN.
             DISPLAY "Write error" AT 0812 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             PERFORM DISPLAY-FILE-NAME.
      

       DISPLAY-FILE-NAME.
          IF WS-F-ERROR = 2
               MOVE W02-NETWORK TO WS-FILE
               MOVE WS-NETKEY TO WS-KEY
           ELSE
           IF WS-F-ERROR = 6
               MOVE W02-DEBTOR TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE DEB-ACNO     TO WS-KEYX
           ELSE
           IF WS-F-ERROR = 7
               MOVE W02-DEPART TO WS-FILE
               MOVE WS-DEPKEY TO WS-KEY
           ELSE
           IF WS-F-ERROR = 9
               MOVE W02-GARTEE   TO WS-FILE
               MOVE ZERO         TO WS-KEY
               MOVE GUA-KEY      TO WS-KEYX
          ELSE
           IF WS-F-ERROR = 15
               MOVE WS-PARID     TO WS-FILE
               MOVE WS-PARKEY    TO WS-KEY
          ELSE
           IF WS-F-ERROR = 19
               MOVE W02-SALE TO WS-FILE
               MOVE WS-SALKEY TO WS-KEY
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
           IF WS-F-ERROR = 27
               MOVE W02-DEBMEM   TO WS-FILE
               MOVE ZERO         TO WS-KEY
        MOVE DME-KEY  TO WS-KEYX.
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
             DISPLAY "File - " AT 1012 WS-FILE WITH FOREGROUND-COLOR Cyan HIGHLIGHT.
             DISPLAY "Status " AT 1212
                      WS-STATUS WITH FOREGROUND-COLOR Cyan HIGHLIGHT
                     ": " WS-STAT-MESSAGE WITH FOREGROUND-COLOR Brown HIGHLIGHT.
           IF WS-STATUS NOT = "94"
               DISPLAY "Key    " AT 1412
                        WS-KEY WITH FOREGROUND-COLOR Cyan HIGHLIGHT
               DISPLAY "Reverse backup or contact program Support"
                        AT 1612.
               DISPLAY "Please make a note of these details" AT 1812.
             STOP RUN.
