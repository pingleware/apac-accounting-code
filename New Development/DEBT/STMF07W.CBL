      $SET LINKCOUNT"512" GNT"STMF07W.GNT"
      ******************************************************************
      *                                                                *
      *    ******   ********  **     **  *******    ****    ********   *
      *   **    **     **     ***   ***  **        **  **        **    *
      *   **           **     ** * * **  **       **    **      **     *
      *    ******      **     **  *  **  *****    **    **     **      *
      *         **     **     **     **  **       **    **     **      *
      *   **    **     **     **     **  **        **  **      **      *
      *    ******      **     **     **  **         ****       **      *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *           A C C O U N T S   R E C E I V A B L E /              *
      *                                                                *
      *       D E B T O R   S T A T E M E N T S   P R O G R A M        *
      *                                                                *
      *                   ( C O N S O L - U S B)                       *
      *                                                                *
      *       Version 9.04.01 - May 2016                               *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  May 2006     - New field included in Accounts Receivable      *
      *                 for recording number of outstanding Jobs       *
      *                 linked to the account. Additional filler       *
      *                 added for future expansion.                    *
      *                                                                *
      *  October 2006 - Amended Accounts receivable ageing from a      *
      *                 specific date to the actual date on which      *
      *                 statements and month-end is run.               *
      *                                                                *
      *  Feb 2007     - New file (DEBDEP/AR_REST.DAT) - Debtor sales   *
      *                 restrictions file has been included which      *
      *                 will allow for departments to de excluded or   *
      *                 only selected departments allowed. This will   *
      *                 allow the user the abillity to allow or bar    *
      *                 sales of selected departments. An indicator    *
      *                    03  DEB-RESTRICT              PIC  X(01).   *
      *                        88  SALES-EXCLUDED        VALUE "E".    *
      *                        88  SALES-ALLOWED         VALUE "A".    *
      *                        88  NO-SALES-RESTRICTIONS VALUE "N".    *
      *                 has been included in the Debtor Record.        *
      *                                                                *
      *  Dec 2007     - Program changed to allow the user to print     *
      *                 statements for a range of Debtor accounts.     *
      *                 The main objective is to allow statements to   *
      *                 printed on more than one work station simil-   *
      *                 taneously, allowing a shared work load to be   *
      *                 used to print large debtor files.              *
      *                 See SECTION'S XB300, XB400 AND XB500.          *
      *                                                                *
      *                 November 2014.                                 *
      *                 The above sections have been moved to program  *
      *                 DTPReadAR which is called by any program that  *
      *                 requires a range of accounts or a starting     *
      *                 account or details from a previous period.     *
      *                                                                *
      *  October 2008 - Amended Accounts receivable ageing to create   *
      *                 duplicate files of the accounts and trans-     *
      *                 actions for printing of copy satements when    *
      *                 and if required. The file names are AR_Ayymm   *
      *                 and AR_Tyymm, where yymm is replaced with      *
      *                 the year and month.                            *
      *                                                                *
      *  March 2010   - Include facility to print to a USB printer     *
      *                 via the Windows print using the relavent       *
      *                 print API's supplied with MF COBOL. An entry   *
      *                 in the APACFIDS file has been included for     *
      *                 the system to access the bitmap file that is   *
      *                 used for the statement and is accessed using   *
      *                 "STMBMP" as the key.                           *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     STMF07W.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   MARCH 2010.

                COPYRIGHT NOTICE: COPYRIGHT (C) 2010 - 2017
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

       COPY "CONTROL.SL".

       COPY "DBHIST.SL".

       COPY "DBTRAN.SL".

       COPY "DEBTOR.SL".

       COPY "DTHIST.SL".

       COPY "PARAM.SL".

       COPY "SHARED.SL".

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

       COPY "CONTROL.FDE".

       COPY "DBHIST.FDE".

       COPY "DBTCON.FDE".

       COPY "DBTRAN.FDE".

       COPY "DEBTOR.FDE".

       COPY "DTHIST.FDE".

       COPY "PARAM.FDE".

       COPY "SHARED.FDE".

      *
      *         **        **    *****    ******    **   **
      *         **        **   **   **   **   **   **  **
      *         **        **   **   **   **  **    ** **
      *         **   **   **   **   **   *****     ****
      *          ** **** **    **   **   **  **    ** **
      *           ***  ***     **   **   **   **   **  **
      *            *    *       *****    **   **   **   **
      *
       WORKING-STORAGE SECTION.
       77  WS-CHECK                PIC  X(18)         VALUE "aeWlimemnomLalismJ".
       77  WS-SUB-1                PIC  9(04)  COMP-5.
       77  WS-SUB-2                PIC  9(04)  COMP-5.
       77  WS-SUB-3                PIC  9(04)  COMP-5.
       77  WS-SUB-4                PIC  9(04)  COMP-5.
       77  WS-SUB-5                PIC  9(04)  COMP-5.
       77  WS-SUB-6                PIC  9(04)  COMP-5.
       77  WS-IXS1                 PIC  9(04)  COMP-5.
       77  WS-IXS2                 PIC  9(04)  COMP-5.
       77  WS-IXS3                 PIC  9(04)  COMP-5.
       77  WS-IXS4                 PIC  9(04)  COMP-5.
       77  WS-MSUB                 PIC  9(04)  COMP-5.
       77  WS-PARKEY               PIC  9(04)  COMP-5.
       77  WS-DEPKEY               PIC  9(04)  COMP-5.
       77  WS-NETKEY               PIC  9(06)  COMP-5.
       77  WS-SHARED               PIC  9(04)  COMP-5 VALUE 1.
       77  WS-WRKEY                PIC  9(04)  COMP-5.
       77  WS-NTP                  PIC  9(01).
       77  WS-LINES                PIC  9(02)         VALUE 34.
       77  WS-PAGE                 PIC  9(04)  COMP-5.
       77  WS-OPTION               PIC  X(01).
       77  WS-AGE                  PIC  X(01).
       77  WS-SEQ                  PIC  X(01).
       77  WS-CREDIT               PIC  X(01).
       77  WS-ZERO                 PIC  X(01).
       77  WS-RANGE                PIC  X(01).
       77  WS-NUM                  PIC  Z9.
       77  WS-ADDR                 PIC  X(01).
       77  WS-END                  PIC  X(01)         VALUE "N".
       77  WS-REF                  PIC  X(01).
       77  WS-STMT                 PIC  X(01).
           88  PREVIOUS-STATEMENT                     VALUE "P".
       77  WS-ERROR                PIC  9(01).
       77  WS-ERROR1               PIC  9(01).
       77  WS-PGE-LENGTH           PIC  9(02)         VALUE 66.
       77  WS-PRN-LENGTH           PIC  9(02)         VALUE 62.
       77  WS-EXPAND               PIC  X(01)         VALUE " ".
       77  WS-COMP                 PIC  9(01).
       77  WS-STM                  PIC  9(01).
       77  WS-TYPE                 PIC  X(01).
       77  WS-HEAD                 PIC  X(01).
       77  WS-MESS                 PIC  X(02).
       77  WS-ACNO                 PIC  X(06).
       77  WS-ASTART               PIC  X(06).
       77  WS-AEND                 PIC  X(06).
       77  WS-S1                   PIC  9(04)  COMP-5.
       77  WS-S2                   PIC  9(04)  COMP-5.
       77  WS-S3                   PIC  9(04)  COMP-5.
       77  WS-S4                   PIC  9(04)  COMP-5.
       77  WS-S5                   PIC  9(04)  COMP-5.
       77  WS-S6                   PIC  9(04)  COMP-5.
       77  WS-S7                   PIC  9(04)  COMP-5.
       77  WS-S8                   PIC  9(04)  COMP-5.
       77  WS-ERR1                 PIC  X(22)         VALUE "Invalid account number".
       77  WS-ERR2                 PIC  X(09)         VALUE "No record".
       77  WS-AC                   PIC  X(07)         VALUE "Account".
       77  PRG-DEBT-LUP            PIC  X(12)         VALUE "DTP\DTPLOOK".
       77  TODAY-DDMMYY            PIC  9(08)  COMP-5.
       77  WS-USUB                 PIC  9(04)  COMP-5.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *          C = Clear comment line (50)
      *          E = Clear lines (any line or lines between 5 and 46)
      *          H = Change heading
      *          S = Clear the screen and display headings
      *
           03  CRT-TYPE              PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START             PIC  9(02) VALUE 5.
           03  CRT-END               PIC  9(02) VALUE 46.
      *
      *    Calling Program
      *
           03  CRT-PROGRAM           PIC  X(15) VALUE "STMF07W".
      *
      *    Screen Heading
      *
           03  CRT-HEADING           PIC  X(40) VALUE "ACCOUNTS RECEIVABLE - STATEMENTS".
      *
      *    Company Name
      *
           03  CRT-COMPANY           PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD         PIC  X(11).
               05  CRT-WRKST         PIC  X(03).
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Banking details for Clients to use for payments via EFT.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  WS-BANK-DETAILS.
           03  WS-PRINT-BNK        PIC  X(01)         VALUE "N".
           03  WS-BNK-NME          PIC  X(30).
           03  WS-BRANCH.
               05  FILLER          PIC  X(13)         VALUE "Branch Code: ".
               05  WS-BR-CODE      PIC  9(06).
           03  WS-ACT-TYP          PIC  X(01).
           03  WS-BANK-ACCOUNT.
               05  WS-ACT-TYPE     PIC  X(13)         VALUE SPACES.
               05  WS-ACT-NO       PIC  X(12).
       COPY "WS.WS".

       01  WS-HELP.
           03  WS-MODULE           PIC  X(03)         VALUE "STM".
           03  WS-PROG             PIC  X(03)         VALUE "F06".

       01  W00-TRAN-CODES.
           03  W00-TRAN-TYPES                         OCCURS 100.
               05  W00-TRAN-CODE   PIC  9(02)  COMP-5.
               05  W00-ACTION      PIC  9(02)  COMP-5.
               05  W00-E-DESC      PIC  X(12).
               05  W00-A-DESC      PIC  X(12).

       01  WS-PARID.
           03  WS-SYS-ID            PIC  X(03).

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  PRINTER ROUTINES FOR USB & WINDOWS PRINTERS THAT DO NOT  ³
      *    ³  USE STANDARD RAW CODE FOR FORMATING                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      ******************************************************************
      * KEY                                                            *
      *                                                                *
      * Descriptions for all of the call-by-name routines appear       *
      * alphabetically. Each description contains the routine name and *
      * function followed by these sections (as appropriate):          *
      *                                                                *
      * SYNTAX:     Shows the CALL statement you could use to call     *
      *             the routine.                                       *
      *                                                                *
      *             The optional RETURNING clause is also shown. Every *
      *             routine returns a value showing the result of the  *
      *             operation. Unless otherwise indicated, zero        *
      *             indicates success, nonzero indicates failure. This *
      *             value is left in the data item specified in the    *
      *             RETURNING clause, in this reference, status-code.  *
      *             If this clause is omitted, the value is left in    *
      *             the special register RETURN-CODE. (If call-        *
      *             convention bit two is set, RETURN-CODE is not      *
      *             changed.)                                          *
      *                                                                *
      *             status-code must be a numeric data item capable of *
      *                         holding positive values from 0 to      *
      *                         65535; for example, PIC X(2) COMP-5.   *
      *                                                                *
      *             The name of the routine must be coded in upper     *
      *             case.                                              *
      *                                                                *
      * PARAMETERS: Describes any parameters shown in the RETURNING    *
      *             and USING clause. A parameter enclosed in brackets,*
      *             for example, [parameter1] is optional and might    *
      *             not be needed for all forms of the routine.        *
      *                                                                *
      * ON ENTRY:   Indicates which of the parameters shown are passed *
      *             on entry.                                          *
      *                                                                *
      * ON EXIT:    Indicates which of the parameters shown are        *
      *             returned on exit.                                  *
      *                                                                *
      *             Where bits of one or more bytes are referenced,    *
      *             bit 0 is the least significant (rightmost) bit.    *
      *                                                                *
      * COMMENTS:   Provides any additional information necessary for  *
      *             the successful use of the routine.                 *
      *                                                                *
      *    PC_PRINT_FILE           Print a file                        *
      *    PC_PRINTER_CLOSE        Close a printer channel             *
      *    PC_PRINTER_CONTROL      Send a printer command to a printer *
      *    PC_PRINTER_FREE_BMP     Free bitmap from memory             *
      *    PC_PRINTER_INFO         Get printer information             *
      *    PC_PRINTER_LOAD_BMP     Load bitmap into memory             *
      *    PC_PRINTER_OPEN         Open a printer channel              *
      *    PC_PRINTER_SET_COLOR    Set printer color                   *
      *    PC_PRINTER_SET_FONT     Set printer font                    *
      *    PC_PRINTER_WRITE        Write text to a printer             *
      *    PC_PRINTER_WRITE_BMP    Write bitmap to a printer           *
      *                                                                *
      *================================================================*
      *                                                                *
      *    PC_WIN_CLOSE_PRINTER    Close a printer channel             *
      *    PC_WIN_FREE_PBMP        Free bitmap from memory             *
      *    PC_WIN_LOAD_PBMP        Load bitmap into memory             *
      *    PC_WIN_OPEN_PRINTER     Open a printer channel              *
      *    PC_WIN_OPEN_PRINTER_EXT Extended open a printer channel     *
      *    PC_WIN_PRINT_FILE       Print a file                        *
      *    PC_WIN_PRINT_FILE_EXT   Extended print a file               *
      *    PC_WIN_PRINTER_CONTROL  Send a printer control              *
      *    PC_WIN_PRINTER_INFO     Get printer information             *
      *    PC_WIN_SET_PCOLOR       Set printer color                   *
      *    PC_WIN_SET_PDEFAULT     Set or reset default printer        *
      *    PC_WIN_SET_PFONT        Set printer font                    *
      *    PC_WIN_WRITE_PBMP       Write bitmap to a printer           *
      *    PC_WIN_WRITE_PRINTER    Write to a printer                  *
      *                                                                *
      * ______________________________________________________________ *

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           Parameters for PC_PRINTER_DEFAULT_NAME          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Default-Printer-Flags           pic x(04) comp-5  Value 1.
       01  Default-Printer-Name.
           03  Default-Printer-Name-Len    pic x(02) comp-5.
           03  Default-Printer-Name-Text   pic x(80).

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³          Parameters for PC_PRINTER_DEFAULT_FONT           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Default-Font-Family-Name.
           03  Default-Font-Name-Len       pic x(02) comp-5.
           03  Default-Font-Name-Text      pic x(80).
       01  Default-Font-Size               pic x(04) comp-5.
       01  Default-Font-Style              pic x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       Parameters for PC_PRINTER_DEFAULT_PROPERTIES        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Default-Properties-Flags        pic x(4) comp-5.
       01  Default-Properties.
           03 pr-len                       pic  x(2) comp-5.
           03 pr-papersize                 pic s9(4) comp-5.
           03 pr-paperlength               pic s9(4) comp-5.
           03 pr-paperwidth                pic s9(4) comp-5.
           03 pr-scale                     pic s9(4) comp-5.
           03 pr-copies                    pic s9(4) comp-5.
           03 pr-papertray                 pic s9(4) comp-5.
           03 pr-printquality              pic s9(4) comp-5.
           03 pr-color                     pic s9(4) comp-5.
           03 pr-duplex                    pic s9(4) comp-5.
           03 pr-orientation               pic s9(4) comp-5.
           03 pr-yresolution               pic s9(4) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              Parameters for PC_PRINTER_OPEN               ³ 
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Printer-Handle                  pic x(04) comp-5.
       01  Document-Title.
           03  Title-Len                   pic x(02) comp-5.
           03  Title-Text                  pic x(80).
       01  Printer-Flags                   pic x(04) comp-5.
       01  Window-Handle                   pic x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              Parameters for PC_PRINTER_WRITE              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      * ---Printer-Handle (same as above)
       01  Print-Buffer                    pic x(136).
       01  Print-Buffer-Len                pic x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³             Parameters for PC_PRINTER_LOAD_BMP            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      * ---Printer-Handle (same as above)
       01  BmpFilename                     pic x(64).
       01  BmpID                           pic x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³            Parameters for PC_PRINTER_WRITE_BMP            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      * ---Printer-Handle (same as above)
      * ---BmpID   (same as above)
       01  WriteBmpReserved                pic x(04) comp-5 value 0.
       01  BmpRow                          pic x(04) comp-5 value 1.
       01  BmpCol                          pic x(04) comp-5 value 6.
       01  BmpWidth                        pic x(04) comp-5 value 82.
       01  BmpHeigth                       pic x(04) comp-5 value 13.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³            Parameters for PC_PRINTER_WRITE_BMP            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      * ---Printer-Handle (same as above)
      * ---BmpID   (same as above)

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³            Parameters for PC_PRINTER_SET_COLOR            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      * ---Printer-Handle (same as above)
       01  fore-or-back                    pic x(2) comp-5.
       01  color-red                       pic x(2) comp-5.
       01  color-green                     pic x(2) comp-5.
       01  color-blue                      pic x(2) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³             Parameters for PC_PRINTER_CONTROL             ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      * ---Printer-Handle (same as above)
      * ---print-command:  1 - Abort printing and close printer
      *                    2 - Throw a page
      *                    3 - Flush the print buffers
      *                    4 - Start a new line
      *

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              Parameters for PC_PRINTER_CLOSE              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      * ---Printer-Handle (same as above)
      *

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                Parameters for PC_PRINT_FILE               ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Filename.
           03  Filename-Len                pic xx comp-5.
           03  Filename-Text               pic x(80).

       01  DocumentTitle.
           03  DocumentTitle-Len           pic xx comp-5.
           03  DocumentTitle-Text          pic x(15).
      * --- Printer-Flags (same as above)
      * --- Window-Handle (Same as above)

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              Parameters for PC_PRINTER_INFO               ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       78 PrinterNameBufferBegin                      value next.
       01 PrinterNameBuffer                pic x(255).
       78 PrinterNameBufferLength                     value next
          - PrinterNameBufferBegin.
       78 PrinterTypeBufferBegin                      value next.
       01 PrinterTypeBuffer                pic x(255).
       78 PrinterTypeBufferLength                     value next
          - PrinterTypeBufferBegin.
       78 PrinterDeviceBufferBegin                    value next.
       01 PrinterDeviceBuffer              pic x(255).
       78 PrinterDeviceBufferLength                   value next
          - PrinterDeviceBufferBegin.
       78 PrinterLocationBufferBegin                  value next.
       01 PrinterLocationBuffer            pic x(255).
       78 PrinterLocationBufferLength                 value next
          - PrinterLocationBufferBegin.
       78 PrinterCommentBufferBegin                   value next.
       01 PrinterCommentBuffer             pic x(255).
       78 PrinterCommentBufferLength                  value next
          - PrinterCommentBufferBegin.
       78 PrinterPaperSizeBufferBegin                 value next.
       01 PrinterPaperSizeBuffer           pic x(255).
       78 PrinterPaperSizeBufferLength                value next
          - PrinterPaperSizeBufferBegin.

       01  PrinterInformation.
           03  PrinterInformationLength    pic xxxx comp-5.
           03  DeviceContextHandle         pic xxxx comp-5.
           03  reserved                    pic xxxx comp-5.
           03  Orientation                 pic xxxx comp-5.
           03  PageRowsCurrentFont         pic xxxx comp-5.
           03  PageColsCurrentFont         pic xxxx comp-5.
           03  RowsRemainingCurrentFont    pic xxxx comp-5.
           03  MaxGraphicsHorizontalAxis   pic xxxx comp-5.
           03  MaxGraphicsVerticalAxis     pic xxxx comp-5.
           03  MinGraphicsHorizontalAxis   pic xxxx comp-5.
           03  MinGraphicsVerticalAxis     pic xxxx comp-5.
      *
      *    -------  Horizontal
      *
           03  CurrentCooridinateX         pic xxxx comp-5.
      *
      *    -------  Vertical
      *
           03  CurrentCooridinateY         pic xxxx comp-5.
           03  NumberOfCopies              pic xx   comp-5.
      *
      *    -------  Draft, Low,Medium, High, Default or DPI
      *
           03  PrintQuality                pic xx   comp-5.
      *
      *    -------  Monochrome or Color
      *
           03  Color                       pic x   comp-5.
           03  reserved                    pic x   comp-5.
           03  DriverVersionNumber         pic xx   comp-5.
           03  PrinterName.
               05  PrinterNameLength       pic xxxx comp-5 value PrinterNameBufferLength.
               05  p-PrinterNameBuffer                           pointer.
           03  PrinterType.
               05  PrinterTypeLength       pic xxxx comp-5 value PrinterTypeBufferLength.
               05  p-PrinterTypeBuffer                           pointer.
           03  PrinterDevice.
               05 PrinterDeviceLength      pic xxxx comp-5 value PrinterDeviceBufferLength.
               05 p-PrinterDeviceBuffer                          pointer.
           03  PrinterLocation.
               05 PrinterLocationLength    pic xxxx comp-5 value PrinterLocationBufferLength.
               05 p-PrinterLocationBuffer                        pointer.
           03  PrinterComment.
               05 PrinterCommentLength     pic xxxx comp-5 value PrinterCommentBufferLength.
               05 p-PrinterCommentBuffer                         pointer.
           03  PrinterPaperSize.
               05 PrinterPaperSizeLength   pic xxxx comp-5 value PrinterPaperSizeBufferLength.
               05 p-PrinterPaperSizeBuffer                       pointer.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Used to display error messages for the Print Routines.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  Operation                       pic x(30).

       01   printer-control.

           03  Default-Printer-Name.
               05  Default-Printer-Name-Len  pic x(02) comp-5.
               05  Default-Printer-Name-Text pic x(80).

           03  Font-family.
               05  Font-family-namelen     pic x(2) comp-5 value 80.
               05  Font-family-name        pic x(80).

           03  FontTypeFace        pic x(13) value "Courier New" & x"00".
           03  font-style          pic x(4) comp-5 value 0.
           03  font-size           pic x(4) comp-5 value 0.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³            Set to print ñ 5 characters per inch           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           03  font-5-CPI          pic x(4) comp-5 value 24.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³            Set to print ñ 8 characters per inch           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           03  font-8-CPI          pic x(4) comp-5 value 14.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           Set to print ñ 12 characters per inch           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           03  font-12-CPI         pic x(4) comp-5 value 10.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           Set to print ñ 15 characters per inch           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           03  font-15-CPI         pic x(4) comp-5 value 8.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           Set to print ñ 16 characters per inch           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           03  font-16-CPI         pic x(4) comp-5 value 7.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           Set to print ñ 19 characters per inch           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           03  font-19-CPI         pic x(4) comp-5 value 5.

           03  abort               pic x(4) comp-5 value 1.
           03  prn-control         pic x(4) comp-5 value 2.
           03  prn-flags           pic x(4) comp-5 value 1.
           03  prn-command         pic x(4) comp-5 value 4.
           03  prn-handle          pic x(4) comp-5.

           03  prn-open            pic x(01) VALUE "C".

           03  W02-PRINTER.
               05  W02-PRN         PIC  X(06).

           03  W02-SPOOLER.
               05  W02-WS          PIC  X(02).
               05  W02-US          PIC  X(01).
               05  W02-TERM        PIC  X(03).
               05  W02-US2         PIC  X(01).
               05  W02-REP         PIC  X(01).
               05  W02-P           PIC  X(01).
               05  W02-USER        PIC  X(03).

       01  STM-LINE.
           03  STM-DET             PIC  X(96).

       01  ST7-LINE1                                 REDEFINES STM-LINE.
          03  ST7-DET              PIC  X(96).

       01  ST7-LINE2                                 REDEFINES STM-LINE.
           03  ST7-COMP.
               05  ST7-EXCOMP      PIC  X(36).
               05  FILLER          PIC  X(36).
           03  FILLER              PIC  X(24).

       01  ST7-LINE2A                                REDEFINES STM-LINE.
           03  FILLER              PIC  X(24).
           03  ST7-HEADER          PIC  X(17).
           03  FILLER              PIC  X(55).

       01  ST7-LINE3                                 REDEFINES STM-LINE.
           03  ST7-ADD             PIC  X(30).
           03  FILLER              PIC  X(12).
           03  ST7-TEL             PIC  X(19).
           03  ST7-TNO             PIC  X(14).
           03  FILLER              PIC  X(21).

       01  ST7-LINE4                                 REDEFINES STM-LINE.
           03  ST7-PC1             PIC  X(08).
           03  ST7-AH1             PIC  X(12).
           03  ST7-AC1             PIC  X(06).
           03  FILLER              PIC  X(17).
           03  ST7-STMNT.
               05  ST7-AH2         PIC  X(06).
               05  ST7-DATE        PIC  Z9/99/9999.
               05  FILLER          PIC  X(16).
               05  ST7-AH3         PIC  X(09).
               05  ST7-PAGE        PIC  ZZZ9.
           03  ST7-REMIT                             REDEFINES ST7-STMNT.
               05  FILLER          PIC  X(13).
               05  ST7-AH2R        PIC  X(06).
               05  ST7-DATER       PIC  Z9/99/9999.
               05  FILLER          PIC  X(16).
           03  FILLER              PIC  X(08).

       01  ST7-LINE5                                 REDEFINES STM-LINE.
           03  FILLER              PIC  X(04).
           03  ST7-NAME.
               05  ST7-PC2         PIC  X(08).
               05  FILLER          PIC  X(32).
           03  FILLER              PIC  X(08).
           03  ST7-MESS            PIC  X(42).
           03  FILLER              PIC  X(02).

       01  ST7-LINE6                                 REDEFINES STM-LINE.
           03  FILLER              PIC  X(01).
           03  ST7-HD1.
               05  FILLER          PIC  X(02).
               05  ST7-TDTE        PIC  Z9/99/9999.
               05  FILLER          PIC  X(04).
               05  ST7-BFWD.
                   07  ST7-TDESC   PIC  X(12).
                   07  FILLER      PIC  X(04).
                   07  ST7-TREF    PIC  X(08).
               05  FILLER          PIC  X(04).
               05  ST7-V1.
                   07  ST7-TDBT    PIC  Z(08)9.99 BLANK WHEN ZERO.
               05  FILLER          PIC  X(04).
               05  ST7-PD          PIC  X(03).
               05  ST7-V2.
                   07  ST7-TCRD    PIC  Z(08)9.99 BLANK WHEN ZERO.
               05 ST7-CB           PIC  X(01).
               05 FILLER           PIC  X(04).
               05 ST7-V3.
                   07 ST7-TBAL     PIC  Z(08)9.99-.
               05 FILLER           PIC  X(02).

       01  ST7-LINE6-CLR                             REDEFINES STM-LINE.
           03  FILLER              PIC  X(01).
           03  ST7-CLR-DT.
               05  FILLER          PIC  X(14).
           03  ST7-CLR-DSC.
               05  FILLER          PIC  X(16).
           03  ST7-CLR-RF.
               05  FILLER          PIC  X(12).
           03  ST7-CLR-DB.
               05  FILLER          PIC  X(16).
           03  ST7-CLR-CR.
               05  FILLER          PIC  X(20).
           03  ST7-CLR-BL.
               05  FILLER          PIC  X(17).

       01  ST7-LINE7                                 REDEFINES STM-LINE.
           03  FILLER              PIC  X(15).
           03  ST7-A1              PIC  X(12).
           03  ST7-A2              PIC  X(10).
           03  ST7-A3              PIC  X(12).
           03  ST7-A4              PIC  X(12).
           03  ST7-A5              PIC  X(12).
           03  FILLER              PIC  X(23).

       01  ST7-LINE8                                 REDEFINES STM-LINE.
           03  FILLER              PIC  X(13).
           03  ST7-INT             PIC  Z(06)9.99-.
           03  FILLER              PIC  X(01).
           03  ST7-3MTHS           PIC  Z(06)9.99-.
           03  FILLER              PIC  X(01).
           03  ST7-2MTHS           PIC  Z(06)9.99-.
           03  FILLER              PIC  X(01).
           03  ST7-1MTH            PIC  Z(06)9.99-.
           03  FILLER              PIC  X(01).
           03  ST7-CUR             PIC  Z(06)9.99-.
           03  FILLER              PIC  X(01).
           03  ST7-DUE             PIC  Z(06)9.99-.
           03  FILLER              PIC  X(01).
           03  ST7-DUE2            PIC  Z(06)9.99-.


       01  W05-DEB-HISTORY.
           03  W05-DBT-HIS.
               05  W05-DBT         PIC  X(04)        VALUE "AR_T".
               05  W05-TYY         PIC  9(02).
               05  W05-TMM         PIC  9(02).
               05  FILLER          PIC  X(04)        VALUE ".DAT".
           03  W05-DEB-HIS.
               05  W05-DEB         PIC  X(04)        VALUE "AR_A".
               05  W05-DEBH-ID.
                   07  W05-DYY     PIC  9(02).
                   07  W05-DMM     PIC  9(02).
               05  W05-HIST-ID                       REDEFINES W05-DEBH-ID PIC  9(04).
               05  FILLER          PIC  X(04)        VALUE ".DAT".

       01  W05-LIST.
           03  W05-OPT             PIC  9(03).
           03  W05-TYPES                             REDEFINES W05-OPT.
               05  W05-COMP        PIC  9(01).
               05  W05-INDV        PIC  9(01).
               05  W05-INT         PIC  9(01).
      *
      *    **** Consolodated Statements Y or N
      *
           03  W05-CON-STM         PIC  X(01).
      *
      *    **** Consolodation Type A=All transactions, S=Summary
      *
           03  W05-CON-TYPE        PIC  X(01).
      *
      *    **** Statements to be printed Y or N
      *
           03  W05-PRINT-CS        PIC  X(01).

      *01  W10-SPARES.
      *    03  W10-DATE            PIC  X(10).
      *    03  W10-RDATE                             REDEFINES W10-DATE.
      *        05  W10-DTE         PIC  99/99/9999.
       01  W10-REF.
           03  W10-NUM             PIC  9(08).

       COPY "DateVariables.cpy".

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                           REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                           REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).
           03  W25-TOTAL           PIC S9(07)V99 COMP-3.
           03  W25-VALUE           PIC S9(07)V99 COMP-3.

       01  W30-MESSAGES.
           03  W30-MESS6.
               05  W30-MESS                          OCCURS 2.
                   07  W30-SMESS   PIC  X(42)        OCCURS 4.
           03  W30-MESS2                             REDEFINES W30-MESS6.
               05  W30-MESS-A                        OCCURS 2.
                   07  W30-SMES    PIC  X(42)        OCCURS 4.
          03  W30-AMESSAGES.
              05  FILLER           PIC  X(42)        VALUE "Ons het nog nie u betaling vir VERLEDE".
              05  FILLER           PIC  X(42)        VALUE "MAAND ontvang nie. Indien daar 'n rede is,".
              05  FILLER           PIC  X(42)        VALUE "word u vriendelik versoek om ons in kennis".
              05  FILLER           PIC  X(42)        VALUE "te stel.".

              05  FILLER           PIC  X(42)        VALUE "U rekening is nou TWEE MAANDE agterstallig".
              05  FILLER           PIC  X(42)        VALUE "en u word versoek om dringend aandag daar-".
              05  FILLER           PIC  X(42)        VALUE "aan te skenk".
              05  FILLER           PIC  X(42)        VALUE SPACES.

              05  FILLER           PIC  X(42)        VALUE "U rekening is nou MEER AS TWEE MAANDE".
              05  FILLER           PIC  X(42)        VALUE "agterstallig! Tensy u rekening nie op".
              05  FILLER           PIC  X(42)        VALUE "datum gebring word nie, sal GEREGTILIKE".
              05  FILLER           PIC  X(42)        VALUE " STAPPE teen u geneem word.".

              05  FILLER           PIC  X(42)        VALUE "We do not appear to have received your".
              05  FILLER           PIC  X(42)        VALUE "payment for LAST MONTH. You are kindly".
              05  FILLER           PIC  X(42)        VALUE "requested to contact us if there is a".
              05  FILLER           PIC  X(42)        VALUE "problem with your account.".

              05  FILLER           PIC  X(42)        VALUE "Your account is now TWO MONTHS in arrear".
              05  FILLER           PIC  X(42)        VALUE "and we request that you give this matter".
              05  FILLER           PIC  X(42)        VALUE "your urgent attention".
              05  FILLER           PIC  X(42)        VALUE SPACES.

              05  FILLER           PIC  X(42)        VALUE "Your account is now MORE THAN TWO MONTHS".
              05  FILLER           PIC  X(42)        VALUE "in arrear! Unless your account is brought".
              05  FILLER           PIC  X(42)        VALUE "up-to-date, LEGAL ACTION will be".
              05  FILLER           PIC  X(42)        VALUE "instituted.".

           03  W30-AMESS                             REDEFINES W30-AMESSAGES.
               05  W30-ARRM                          OCCURS 2.
                   07  W30-30D     PIC  X(168).
                   07  W30-60D     PIC  X(168).
                   07  W30-90D     PIC  X(168).
           03  W30-STM.
               05  W30-STM6.
                   07  W30-STML    PIC  X(42)        OCCURS 4.
               05  W30-STM2                          REDEFINES W30-STM6.
               05  W30-STL         PIC  X(42)        OCCURS 4.

       01  W30-AGE-DATES.
           03  W30-120             PIC  9(08)    COMP-3.
           03  W30-90              PIC  9(08)    COMP-3.
           03  W30-60              PIC  9(08)    COMP-3.
           03  W30-30              PIC  9(08)    COMP-3.
           03  W30-CUR             PIC  9(08)    COMP-3.

       01  W35-AGE-VALUES.
           03  W35-CUR             PIC S9(09)V99 COMP-3.
           03  W35-30              PIC S9(09)V99 COMP-3.
           03  W35-60              PIC S9(09)V99 COMP-3.
           03  W35-90              PIC S9(09)V99 COMP-3.
           03  W35-120             PIC S9(09)V99 COMP-3.
           03  W35-150             PIC S9(09)V99 COMP-3.
           03  W35-INT             PIC S9(09)V99 COMP-3.

       COPY "W40.DBT".

       COPY "W40.WS".

       01  W45-TRAN.
           03  W45-CODE            PIC 9(02).
           03  W45-ENG             PIC X(12).
           03  W45-AFR             PIC X(12).
           03  W45-ACT             PIC X(01).

       COPY "FUNCTION.WS".

      *COPY "W50.WS".

       01  W55-STATS.
           03  W55-H1              PIC  X(09).
           03  W55-H2              PIC  X(09).
           03  W55-H3              PIC  X(09).
           03  W55-H4              PIC  X(09).
           03  W55-H5              PIC  X(09).
           03  W55-H6              PIC  X(09).
           03  W55-MONTHS.
               05  W55-MTH1        PIC  9(02)    COMP-5.
               05  W55-MTH2        PIC  9(02)    COMP-5.
               05  W55-MTH3        PIC  9(02)    COMP-5.
               05  W55-MTH4        PIC  9(02)    COMP-5.
               05  W55-MTH5        PIC  9(02)    COMP-5.
               05  W55-MTH6        PIC  9(02)    COMP-5.
           03  W55-VALUES.
               05  W55-VAL1        PIC S9(09)V99 COMP-3.
               05  W55-VAL2        PIC S9(09)V99 COMP-3.
               05  W55-VAL3        PIC S9(09)V99 COMP-3.
               05  W55-VAL4        PIC S9(09)V99 COMP-3.
               05  W55-VAL5        PIC S9(09)V99 COMP-3.
               05  W55-VAL6        PIC S9(09)V99 COMP-3.

      *
      *    November 2014 DTPNAME and DTPADDR introduced to handle the
      *    new Debtor Record layout, so these variables are no longer
      *    required. See W40.DBT copy file.
      *
      *01  W60-NME-ADD.
      *    03  W60-NAME.
      *        05  W60-NCHAR OCCURS 40 PIC  X(01).
      *    03  W60-ADDRESS.
      *        05  W60-ACHAR OCCURS 60 PIC  X(01).

       01  W75-KEYS.
           03  W75-S               PIC  9(02)    COMP-5.
           03  W75-S1              PIC  9(02)    COMP-5.
           03  W75-DNO             PIC  X(06)    OCCURS 18.

       01  W75-TITLES.
           03  W75-DESCRIP.
               05  FILLER          PIC  X(28)    VALUE "Mr     Mnr    Mrs    Mev    ".
               05  FILLER          PIC  X(28)    VALUE "Miss   Mej    Ms     Ms     ".
               05  FILLER          PIC  X(28)    VALUE "Dr     Dr     Rev    Ds     ".
               05  FILLER          PIC  X(28)    VALUE "Prof   Prof   The HonSy Edel".
               05  FILLER          PIC  X(28)    VALUE "                            ".
           03  W75-CODES                         REDEFINES W75-DESCRIP.
               05  W75-TITLE                     OCCURS 10.
                   07  W75-ETITLE  PIC  X(07).
                   07  W75-ATITLE  PIC  X(07).

       01  W80-ACCOUNT.
           03  W80-ACNO            PIC  X(06).
           03  W80-NUMD                          REDEFINES W80-ACNO.
               05  W80-SERIAL      PIC  9(06).
           03  W80-ACNO2           PIC  X(06).
      *    03  W80-TYPE            PIC  9(01).

       COPY "W90.DBT".

       COPY "W95.STM".

       01  W100-EDIT.
           03  W100-LBAL           PIC  Z(06)9.99-.
           03  W100-LARR           PIC  Z(06)9.99-.
           03  W100-BAL            PIC  Z(08)9.99-.
           03  W100-CRL            PIC  Z(06)9.
      *
      *    **       ******  **    **  **   **    ***     *****   ******
      *    **         **    ***   **  **  **    ** **   **   **  **
      *    **         **    ****  **  ** **    **   **  **       ** 
      *    **         **    ** ** **  ****     *******  **       *****
      *    **         **    **  ****  ** **    **   **  **  ***  **  
      *    **         **    **   ***  **  **   **   **  **   **  **
      *    *******  ******  **    **  **   **  **   **   *****   ******
      *
       LINKAGE SECTION.

       COPY "CHAIN.LS".

       COPY "FILE.IDS".
      * 
      *   *****    *****   ******   ******  ******  **    **
      *  **   **  **   **  **   **  **      **      ***   **
      *  **       **       **  **   **      **      ****  **
      *   *****   **       *****    *****   *****   ** ** **
      *       **  **       **  **   **      **      **  ****
      *  **   **  **   **  **   **  **      **      **   ***
      *   *****    *****   **   **  ******  ******  **    **
      *
       SCREEN SECTION.
       01  S01.
           03  LINE  8 COLUMN 12 VALUE "Print accounts that became zero this month (".
           03          COLUMN 56 VALUE                                             "Y"    FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 57 VALUE                                              "/".
           03          COLUMN 58 VALUE                                               "N"  FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 59 VALUE                                                ")".

       01  S02.
           03  LINE 10 COLUMN 12 VALUE "Selective print (".
           03          COLUMN 29 VALUE                  "Y"   FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 30 VALUE                   "/".
           03          COLUMN 31 VALUE                    "N" FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 32 VALUE                     ")".

       01  S03.
           03  LINE 12 COLUMN 12 VALUE "Is this a restart (".
           03          COLUMN 31 VALUE                     "Y"   FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 32 VALUE                      "/". 
           03          COLUMN 33 VALUE                       "N" FOREGROUND-COLOR Brown HIGHLIGHT.
           03          COLUMN 34 VALUE                        ")".


       01  S15B.
           03  LINE  6 COLUMN 31 VALUE "PRINTING STATEMENTS" FOREGROUND-COLOR Grey HIGHLIGHT.
           03  LINE  9 COLUMN 12 VALUE "Are you sure (Y/N)?".

       01  S20-MESS.
           03  FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
               05  LINE 08 COLUMN 14 PIC  X(42) USING W30-SMES(2, 1) AUTO.
               05  LINE 09 COLUMN 14 PIC  X(42) USING W30-SMES(2, 2) AUTO.
               05  LINE 10 COLUMN 14 PIC  X(42) USING W30-SMES(2, 3) AUTO.
               05  LINE 11 COLUMN 14 PIC  X(42) USING W30-SMES(2, 4) AUTO.
               05  LINE 12 COLUMN 14 PIC  X(42) USING W30-SMES(1, 1) AUTO.
               05  LINE 13 COLUMN 14 PIC  X(42) USING W30-SMES(1, 2) AUTO.
               05  LINE 14 COLUMN 14 PIC  X(42) USING W30-SMES(1, 3) AUTO.
               05  LINE 15 COLUMN 14 PIC  X(42) USING W30-SMES(1, 4) AUTO.
      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS.
       AA00-MAIN       SECTION.
       AA00-INIT.
           IF LS-DSKDRV = "L"
               MOVE "Not available on Local drive" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO AA49.
           IF LS0-DBLEV < 2
      *
      *      Insufficient security level
      *
               MOVE X"FF"            TO WS-OPTION
               PERFORM ERROR-MESSAGE
               GO TO AA49.
             PERFORM ZA000-INIT.
             PERFORM YA000.
             PERFORM XB000.
           IF PREVIOUS-STATEMENT
               CLOSE DBHIST DTHIST.
           IF prn-open = "O"
               perform close-down-printer.

       AA49.
            EXIT PROGRAM.
      * ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * ³ The following routines are used for printing via the WINDOWS ³
      * ³ printing API's and are used for LASER and INK-JET printers   ³
      * ³ that do not allow for embedded control characters.           ³
      * ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       PRINT-DETAILS  SECTION.
             perform print-line.
      * ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * ³  Check if paper must be advanced? (WS-ADVANCE will be > 0)   ³
      * ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           if WS-ADVANCE = 99
               perform Throw-a-page
           else
           if WS-ADVANCE > 0
               perform advance-printer WS-ADVANCE times.
               
       PRINT-DETAILS-AND-CLEAR  SECTION.
             PERFORM PRINT-DETAILS.
             MOVE SPACES             TO ST7-DET.
      * ______________________________________________________________ *
      * PC_PRINTER_WRITE                                               *
      *                                                                *
      *        Writes a text string to the printer.                    *
      *                                                                *
      * SYNTAX:                                                        *
      *                                                                *
      *         call "PC_PRINTER_WRITE" using  printer-handle          *
      *               print-buffer                                     *
      *               by value  print-buff-len                         *
      *               returning status-code                            *
      *                                                                *
      * PARAMETERS:                                                    *
      *         printer-handle pic x(4) comp-5.                        *
      *         print-buffer   pic x(n).                               *
      *         print-buff-len Numeric literal or pic x(4) comp-5.     *
      *         status-code See Key                                    *
      *                                                                *
      * ON ENTRY:                                                      *
      *         printer-handle The printer handle returned when the    *
      *         printer was opened.                                    *
      *         printer-buffer The buffer from which the bytes are to  *
      *         be printed                                             *
      *         printer-buff-len  The number of bytes to print         *
      *                                                                *
      * ON EXIT:                                                       *
      *                                                                *
      *         None                                                   *
      *                                                                *
      * COMMENTS:                                                      *
      *                                                                *
      *         This routine is available in 32-bit environments only. *
      *                                                                *
      *         You can check the success of the call by examining     *
      *         RETURN-CODE.                                           *
      * ______________________________________________________________ *
       print-line   section.
             call "PC_PRINTER_WRITE" using prn-handle Print-Buffer by value Print-Buffer-Len
             end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_SET_FONT                                            *
      *                                                                *
      *         Sets the font on the printer.                          *
      *                                                                *
      * SYNTAX:                                                        *
      *                                                                *
      *         call "PC_PRINTER_SET_FONT" using     printer-handle    *
      *                                              font-family-name  *
      *                                    by value  font-size         *
      *                                    by value  font-style        *
      *                                    returning status-code       *
      *                                                                *
      * PARAMETERS:                                                    *
      *         printer-handle pic x(4) comp-5.                        *
      *         font-family-name Group item defined as:                *
      *                                name-len  pic x(2) comp-5.      *
      *                                name-text pic x().              *
      *         font-size Numeric literal or     pic x(4) comp-5.      *
      *         font-style Numeric literal or    pic x(4) comp-5.      *
      *         status-code See Key                                    *
      *                                                                *
      * ON ENTRY:                                                      *
      *         printer-handle The printer handle returned when the    *
      *                        printer was opened.                     *
      *         name-len The length of the font family name.           *
      *         name-text The family name of the font to be used,      *
      *                   such as Courier, Times and Symbol            *
      *         font-size Point size of the font                       *
      *         font-style Set of bits defining the required font      *
      *         style:                                                 *
      *               bit 3  Bold                                      *
      *               bit 2  Strikeout                                 *
      *               bit 1  Underline                                 *
      *               bit 0  Italic                                    *
      *                                                                *
      *         All remaining bits are reserved for future use and     *
      *         should not be set.                                     *
      *                                                                *
      * ON EXIT:                                                       *
      *                                                                *
      *         None                                                   *
      *                                                                *
      * COMMENTS:                                                      *
      *                                                                *
      *         This routine is available in 32-bit environments only. *
      *                                                                *
      *         You can check the success of the call by examining     *
      *         RETURN-CODE.                                           *
      * ______________________________________________________________ *
       set-printer-font   section.
             call "PC_PRINTER_SET_FONT" using prn-handle font-family by value font-size by value font-style
             end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_CONTROL                                             *
      *                                                                *
      * Sends a print command to a printer.                            *
      *                                                                *
      * SYNTAX:                                                        *
      *                                                                *
      *         call "PC_PRINTER_CONTROL" using      printer-handle    *
      *                                   by value   print-command     *
      *                                   returning  status-code       *
      *                                                                *
      * PARAMETERS:                                                    *
      *         printer-handle pic x(4) comp-5.                        *
      *         print-command Numeric literal or pic x(4) comp-5.      *
      *         status-code See Key                                    *
      *                                                                *
      * ON ENTRY:                                                      *
      *         printer-handle The printer handle returned when the    *
      *         printer was opened.                                    *
      *         print-command The command to send to the printer:      *
      *                                                                *
      *           1   Abort printing the document and close the        *
      *               printer                                          *
      *           2   Throw a page                                     *
      *           3   Flush the print buffers                          *
      *           4   Advance one line                                 *
      *                                                                *
      * ON EXIT:                                                       *
      *                                                                *
      *         None                                                   *
      *                                                                *
      * COMMENTS:                                                      *
      *                                                                *
      *         This routine is available in 32-bit environments only. *
      *                                                                *
      *         You can check the success of the call by examining     *
      *         RETURN-CODE.                                           *
      * ______________________________________________________________ *
      * ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * ³            A D V A N C E   P A P E R   1   L I N E           ³
      * ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       advance-printer    section.
             move 4                  to prn-command.
             call "PC_PRINTER_CONTROL" using    prn-handle by value prn-command
             end-call.
      * ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * ³            A D V A N C E   P A P E R   1   P A G E           ³
      * ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       Throw-a-page    section.
             move 2                  to prn-command.
             call "PC_PRINTER_CONTROL" using prn-handle by value prn-command
             end-call.
      * ______________________________________________________________ *
      * PC_PRINTER_SET_COLOR                                           *
      *                                                                *
      * Sets the color on the printer.                                 *
      *                                                                *
      * SYNTAX:                                                        *
      *                                                                *
      *         call "PC_PRINTER_SET_COLOR" using     printer-handle   *
      *                                     by value  fore-or-back     *
      *                                     by value  color-red        *
      *                                     by value  color-green      *
      *                                     by value  color-blue       *
      *                                     returning status-code      *
      *                                                                *
      * PARAMETERS:                                                    *
      *         printer-handle pic x(4) comp-5.                        *
      *         fore-or-back   pic x(2) comp-5.                        *
      *         color-red      pic x(2) comp-5.                        *
      *         color-green    pic x(2) comp-5.                        *
      *         color-blue     pic x(2) comp-5.                        *
      *         status-code See Key                                    *
      *                                                                *
      * ON ENTRY:                                                      *
      *         printer-handle The printer handle returned when the    *
      *                        printer was opened.                     *
      *         fore-or-back   Whether to set foreground or background:*
      *                                   1        foreground          *
      *                                   2        background          *
      *         color-red      Value between 0 and 255; intensity of   *
      *                                                 red            *
      *         color-green    Value between 0 and 255; intensity of   *
      *                                                 green          *
      *         color-blue     Value between 0 and 255; intensity of   *
      *                                                 blue           *
      *                                                                *
      * ON EXIT:                                                       *
      *                                                                *
      *         None                                                   *
      *                                                                *
      * COMMENTS:                                                      *
      *                                                                *
      *         This routine is available in 32-bit environments only. *
      *                                                                *
      *         You must open the printer before using this routine.   *
      *                                                                *
      *         If your printer does not support color, it is up to    *
      *         the printer device driver to interpret these values.   *
      *         For example, some perform gray-shading; others choose  *
      *         either black or white for each color.                  *
      *                                                                *
      *         You specify the color you want by combinations of      *
      *         values showing the intensity of the three primary      *
      *         colors. For example, to get the colors in the          *
      *         following table, you set the RGB values as shown:      *
      *                                                                *
      *                       Color        Red    Green   Blue         *
      *                       -----        ---    -----   ----         *
      *                       Red          255        0      0         *
      *                       Yellow       255      255      0         *
      *                       Green          0      255      0         *
      *                       Cyan           0      255    255         *
      *                       Blue           0        0    255         *
      *                       Magenta      255        0    255         *
      *                       White        255      255    255         *
      *                       Black          0        0      0         *
      *                                                                *
      *                   Some other, more unusual, colors are:        *
      *                                                                *
      *                       Dark orange  255      140      0         *
      *                       Navy blue      0        0    128         *
      *                       Violet       238      120    238         *
      *                       Beige        245      245    220         *
      *                                                                *
      * ______________________________________________________________ *
       printer-colour   section.
             call "PC_PRINTER_SET_COLOR" using   prn-handle by value fore-or-back by value color-red by value color-green by value color-blue
             end-call.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              W H I T E   B A C K G R O U N D              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       set-background-to-white   section.
             MOVE 255                TO color-red.
             MOVE 255                TO color-green.
             MOVE 255                TO color-blue.
             perform printer-colour.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              B E I G E   B A C K G R O U N D              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       set-background-to-beige   section.
             MOVE 245                TO color-red.
             MOVE 245                TO color-green.
             MOVE 220                TO color-blue.
             perform printer-colour.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        W H I T E - S M O K E   B A C K G R O U N D        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       set-background-to-white-smoke   section.
             MOVE 245                TO color-red.
             MOVE 245                TO color-green.
             MOVE 245                TO color-blue.
             perform printer-colour.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ This routine prints the transaction details and uses the  ³
      *    ³ background colour to show columns. This helps with read-  ³
      *    ³ ability and gives the document a more professional look.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       PRINT-TRANSACTION-LINE SECTION.
             perform set-background-to-beige.
             perform printer-colour.
             MOVE ST7-CLR-DT         TO Print-Buffer.
             MOVE 14                 to Print-Buffer-Len.
             perform print-line.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³               W H I T E   B A C K G R O U N D             ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             perform set-background-to-white.
             MOVE ST7-CLR-DSC        TO Print-Buffer.
             MOVE 16                 to Print-Buffer-Len.
             perform print-line.
             perform set-background-to-beige.
             MOVE ST7-CLR-RF         TO Print-Buffer.
             MOVE 12                 to Print-Buffer-Len.
             perform print-line.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³               W H I T E   B A C K G R O U N D             ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             perform set-background-to-white.
             MOVE ST7-CLR-DB         TO Print-Buffer.
             MOVE 16                 to Print-Buffer-Len.
             perform print-line.
             perform set-background-to-beige.
             MOVE ST7-CLR-CR         TO Print-Buffer.
             MOVE 20                 to Print-Buffer-Len.
             perform print-line.
             perform set-background-to-white-smoke.
             MOVE ST7-CLR-BL         TO Print-Buffer.
             MOVE 17                 to Print-Buffer-Len.
             perform print-line.
             perform advance-printer.
      *
      *************  End PRINT-TRANSACTION-LINE  *************
      * ______________________________________________________________ *
      * PC_PRINTER_CLOSE                                               *
      *                                                                *
      * Closes a previously opened printer channel.                    *
      *                                                                *
      * SYNTAX:                                                        *
      *                                                                *
      *         call "PC_PRINTER_CLOSE" using     printer-handle       *
      *                                 returning status-code          *
      *                                                                *
      * PARAMETERS:                                                    *
      *         printer-handle pic x(4) comp-5.                        *
      *         status-code See Key                                    *
      *                                                                *
      * ON ENTRY:                                                      *
      *         printer-handle The printer handle returned when the    *
      *         printer was opened.                                    *
      *                                                                *
      * ON EXIT:                                                       *
      *         None                                                   *
      *                                                                *
      * COMMENTS:                                                      *
      *                                                                *
      *         This routine is available in 32-bit environments only. *
      *                                                                *
      *         You can check the success of the call by examining     *
      *         RETURN-CODE.                                           *
      * ______________________________________________________________ *
       close-down-printer section.
             call "PC_PRINTER_CLOSE" using by reference prn-handle
             end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_LOAD_BMP                                            *
      *                                                                *
      * Loads a bitmap into memory ready for the PC_PRINTER_WRITE_BMP  *
      * function.                                                      *
      *                                                                *
      * SYNTAX:                                                        *
      *                                                                *
      *        call "PC_PRINTER_LOAD_BMP" using printer-handle         *
      *                                   by reference bmpfilename     *
      *                                   by reference bmp-id          *
      *                                   returning status-code        *
      *                                                                *
      * PARAMETERS:                                                    *
      *         printer-handle pic x(4) comp-5.                        *
      *         bmpfilename    pic x(n).                               *
      *         bmp-id         pic x(4) comp-5.                        *
      *         status-code See Key                                    *
      *                                                                *
      * ON ENTRY:                                                      *
      *         printer-handle The printer handle returned when the    *
      *                        printer was opened.                     *
      *         bmpfilename    The name of the file to be printed      *
      *                        (null or space terminated).             *
      *                                                                *
      * ON EXIT:                                                       *
      *         bmp-id      A unique identifier for the loaded bitmap. *
      *         status-code Printer status code:                       *
      *                                                                *
      *                      0  Successful                             *
      *                      3  Printer device not open                *
      *                     18  Failed to load bitmap                  *
      *                     19  Invalid bitmap id given                *
      *                                                                *
      * EXAMPLE:                                                       *
      *                                                                *
      *     call "PC_PRINTER_LOAD_BMP" using     printer-handle        *
      *                                by reference "mflogo.bmp" & x"0"*
      *                                by reference bmp-id             *
      *                                returning    printer-retcode    *
      *     end-call                                                   *
      *                                                                *
      * COMMENTS:                                                      *
      *                                                                *
      *         This routine is available in 32-bit environments only. *
      *  See also:                                                     *
      *                                                                *
      *         PC_PRINTER_FREE_BMP                                    *
      *         PC_PRINTER_WRITE_BMP                                   *
      * ______________________________________________________________ *
       print-header-BMP    section.
             call "PC_PRINTER_LOAD_BMP" using prn-handle by reference BmpFilename by reference BmpID
             end-call.

           if Return-Code not = 0
               move "PC_PRINTER_LOAD_BMP"  to Operation
               perform 9999-Display-Error
           end-if.
      * ______________________________________________________________ *
      * PC_PRINTER_WRITE_BMP                                           *
      *                                                                *
      * Write a loaded bitmap to a printer at a specified position with*
      * a given size.                                                  *
      *                                                                *
      * SYNTAX:                                                        *
      *                                                                *
      *         call "PC_PRINTER_WRITE_BMP" using     printer-handle   *
      *                                     by value  bmp-id           *
      *                                     by value  reserved         *
      *                                     by value  bmp-row          *
      *                                     by value  bmp-col          *
      *                                     by value  bmp-width        *
      *                                     by value  bmp-height       *
      *                                     returning status-code      *
      *                                                                *
      * PARAMETERS:                                                    *
      *         printer-handle pic x(4) comp-5.                        *
      *         reserved       pic x(4) comp-5 value 0.                *
      *         bmp-id         pic x(4) comp-5.                        *
      *         bmp-row        pic x(4) comp-5.                        *
      *         bmp-col        pic x(4) comp-5.                        *
      *         bmp-width      pic x(4) comp-5.                        *
      *         bmp-height     pic x(4) comp-5.                        *
      *         status-code See Key                                    *
      *                                                                *
      * ON ENTRY:                                                      *
      *         printer-handle The printer handle returned when the    *
      *                        printer was opened.                     *
      *         bmp-id    The unique identifier of the bitmap to print *
      *         bmp-row                                                *
      *         bmp-col   The position of the bitmap to print.         *
      *         bmp-width                                              *
      *         bmp-height The size of the bitmap to print.            *
      *                                                                *
      * ON EXIT:                                                       *
      *         status-code  Printer status code:                      *
      *                         0      Successful                      *
      *                         3      Printer device not open         *
      *                         4      Out of memory while printing    *
      *                         5      Disk full while spooling file   *
      *                         7      Print job aborted, no file      *
      *                                spooled to Print Manager        *
      *                        11      Write failure                   *
      *                        21      Failed to print bitmap          *
      *                                                                *
      * COMMENTS:                                                      *
      *                                                                *
      *         This routine is available in 32-bit environments only. *
      *                                                                *
      *         This routine works with PostScript and HP PCL printers.*
      *         It is not supported on HP Deskjet or dot matrix        *
      *         printers.                                              *
      *  See also:                                                     *
      *                                                                *
      *         PC_PRINTER_FREE_BMP                                    *
      *         PC_PRINTER_LOAD_BMP                                    *
      *                                                                *
      * ______________________________________________________________ *
             call "PC_PRINTER_WRITE_BMP" using prn-handle by value BmpID by value WriteBmpReserved by value BmpRow by value BmpCol by value BmpWidth by value BmpHeigth
             end-call.

           if Return-Code not = 0
               move "PC_PRINTER_WRITE_BMP" to Operation
               perform 9999-Display-Error
           end-if.
      * ______________________________________________________________ *
      * PC_PRINTER_FREE_BMP                                            *
      *                                                                *
      * Frees a bitmap from memory, releasing its image and palette    *
      * back to the operating system.                                  *
      *                                                                *
      * SYNTAX:                                                        *
      *                                                                *
      *         call "PC_PRINTER_FREE_BMP" using     printer-handle    *
      *                                    by value  bmp-id            *
      *                                    returning status-code       *
      *                                                                *
      * PARAMETERS:                                                    *
      *         printer-handle pic x(4) comp-5.                        *
      *         bmp-id         pic x(4) comp-5.                        *
      *         status-code See Key                                    *
      *                                                                *
      * ON ENTRY:                                                      *
      *         printer-handle The printer handle returned when the    *
      *                        printer was opened.                     *
      *         bmp-id The unique id of the loaded bitmap, returned    *
      *                when the bitmap was loaded.                     *
      *                                                                *
      * ON EXIT:                                                       *
      *         status-code Printer status code:                       *
      *            0        Successful                                 *
      *            3        Printer device not open                    *
      *           20        Failed to free bitmap                      *
      *                                                                *
      * EXAMPLE:                                                       *
      *                                                                *
      *         call "PC_PRINTER_FREE_BMP" using     printer-handle,   *
      *                                    by value  bmp-idlogo,       *
      *                                    returning printer-retcode   *
      *         end-call                                               *
      *                                                                *
      * COMMENTS:                                                      *
      *                                                                *
      *         This routine is available in 32-bit environments only. *
      *  See also:                                                     *
      *                                                                *
      *         PC_PRINTER_LOAD_BMP                                    *
      *         PC_PRINTER_WRITE_BMP                                   *
      * ______________________________________________________________ *
             call "PC_PRINTER_FREE_BMP" using prn-handle by value BmpID
             end-call.

           if Return-Code not = 0
               move "PC_PRINTER_FREE_BMP" to Operation
               perform 9999-Display-Error
           end-if.
      * ______________________________________________________________ *
      * PC_WIN_SET_PDEFAULT                                            *
      *                                                                *
      * Set the default printer for all PC_WIN printer routines.       *
      *                                                                *
      * SYNTAX:     call "PC_WIN_SET_PDEFAULT" using deflt-printer     *
      *             returning status-code                              *
      *                                                                *
      * PARAMETERS: deflt-printer  pic x(n).                           *
      *             status-code    See Key in the Preface              *
      *                                                                *
      * ON ENTRY:   deflt-printer A null-terminated string specifying  *
      *             the default printer. If no name is specified (that *
      *             is, the first character of the string is null)     *
      *             then the default printer is reset.                 *
      *                                                                *
      * ON EXIT:    status-code Printer status code:                   *
      *                                                                *
      *              0 Successful                                      *
      *             17 Failed to find default printer                  *
      *                                                                *
      * EXAMPLE:    call "PC_WIN_SET_PDEFAULT" using                   *
      *                  "Local PostScript Printer" & x"00"            *
      *                   returning printer-retcode                    *
      *             end-call                                           *
      *                                                                *
      * ______________________________________________________________ *

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ This routine is used to call the Debtor / Ac's Receivable ³
      *    ³ lookup program and cancel it when control is returned.    ³
      *    ³ The  relevant  error  message  will be  displayed  if  an ³
      *    ³ exception occurs. The usual  parameters are passed to the ³
      *    ³ lookup program as well W40-COMPANY which will contain the ³
      *    ³ details of the account that has been selected when control³
      *    ³ is returned to the calling program.                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AA50-LOOKUP     SECTION.
             PERFORM SAVE-SCREEN.
      *      MOVE 4                  TO W44-FUNCTION.
             PERFORM SCREEN-CONTENTS.
             CALL PRG-DEBT-LUP USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W40-COMPANY
                  ON EXCEPTION GO TO AA55
             END-CALL.
             CANCEL PRG-DEBT-LUP.
             GO TO AA58.

       AA55.
             MOVE SPACE              TO WS-ERR-MES.
             STRING "Program- " DELIMITED SIZE PRG-DEBT-LUP DELIMITED " " " not on disk, press ANY key" DELIMITED SIZE INTO WS-ERR-MES.
             PERFORM ERROR-MESSAGE.

       AA58.
             PERFORM RESTORE-SCREEN.

       AA99.
             EXIT.

      *
      *    ****   D I S P L A Y   L O O K U P   O P T I O N S
      *
       AA300     SECTION.
       AA301.
             DISPLAY "F1"                                                              AT 5002 WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                       "=Help,"                                                                WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                             "F2"                                                              WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                               " to do Account Lookup,"                                        WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                     "F9"                                      WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                       " Debtor Lookup (Any Word),"            WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue
                                                                                 "Esc"         WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Brown HIGHLIGHT
                                                                                    " to exit" WITH BACKGROUND-COLOR Cyan FOREGROUND-COLOR Blue.
      
       AA305.
             EXIT.

      *    
      *            ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      *           S C R E E N ,   K E Y B O A R D   &   M O U S E
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³      SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3       ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      SCREEN-SHADOW                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To routine is used to display a shadow down the right and ³
      *    ³ along the bottom of a pop-up box. The parameters that are ³
      *    ³ required:                                                 ³
      *    ³          SHADE-ROW   - Top line of the box.               ³
      *    ³          SHADE-COL   - Left line of box.                  ³
      *    ³          SHADE-WIDTH - Width of the box.                  ³
      *    ³          SHADE-LINES - Height of box.                     ³
      *    ÆÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ
      *    ³                      CHECK-CORRECT                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine displays a pop-up window with the message    ³
      *    ³           "Press Y if correct - N if incorrect"           ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ To start with the pop-up window higher or lower than row  ³
      *    ³ 20 (default); move another value to WS-MES-LINE.          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *            T H I S   R O U T I N E   I S   U S E D   T O
      *       D I S P L A Y   I N F O R M A T I O N   M E S S A G E S
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Move the message to WS-DSP-MES and the top line of the   ³
      *    ³  message box to WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³  default will be 20.                                      ³
      *    ³                  PERFORM DISPLAY-MESSAGE                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *           T H I S   R O U T I N E   I S   U S E D   T O
      *            D I S P L A Y   E R R O R   M E S S A G E S
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      ERROR-MESSAGE                        ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ To display the error message higher or lower (default is  ³
      *    ³ line 20) Move the line number which must be used as the   ³
      *    ³ top line to WS-MES-LINE. The message to be displayed must ³
      *    ³ be in WS-ERR-MES. PERFORM ERROR-MESSAGE.                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.CRT".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                      OPT-MESSAGE                          ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ This routine is used to allow the OPERATOR to respond to  ³
      *    ³ a request for an option, so that the correct action can   ³
      *    ³ be performed by the program. The routine will display the ³
      *    ³ message in a pop-up window and allow the OPERATOR to      ³
      *    ³ respond to the 'question'.                                ³
      *    ³                                                           ³
      *    ³ The option request must be placed in WS-OPT-MES and may   ³
      *    ³ not exceed 48 characters. The message will be centred in  ³
      *    ³ the window. An example of a message request follows:      ³
      *    ³                                                           ³
      *    ³   MOVE "Print transactions (Y/N) [ ]" TO WS-OPT-MES.      ³
      *    ³   MOVE Instruction    TO WS-INSTR.                        ³
      *    ³       [see Accptopt for instruction values]               ³
      *    ³   PERFORM OPT-MESSAGE.                                    ³
      *    ³                                                           ³
      *    ³ This would be displayed as:                               ³
      *    ³      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿   ³
      *    ³      ³          Print transactions (Y/N) [ ]          ³°° ³
      *    ³      ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ°° ³
      *    ³        °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° ³
      *    ³                                                           ³
      *    ³ The response is returned in WS-OPTION (in upper case).    ³
      *    ³                                                           ³
      *    ³ The system will display the message box with the top line ³
      *    ³ as the value of WS-MES-LINE. If WS-MES-LINE is zero the   ³
      *    ³ default will be 20.                                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "OPTION.CRT".

       COPY "LOCKED.REC".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           ERASE SCREEN FROM SPECIFIED LOCATION            ³
      *    ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
      *    ³ Uses CRT-START as starting line and increases and CRT-END ³
      *    ³ as the ending line. The screen is cleared with Column 1   ³
      *    ³ and 80 containing "³" and columns 2 to 79 containing      ³
      *    ³ spaces.                                                   ³
      *    ³                                                           ³
      *    ³  eg.                                                      ³
      *    ³   MOVE 8         TO CRT-START.                            ³
      *    ³   MOVE 28        TO CRT-END.                              ³
      *    ³   PERFORM ERASE-SCREEN.                                   ³
      *    ³                                                           ³
      *    ³ To clear the entire screen and Display a new screen with  ³
      *    ³ headings (program/date/time/company):                     ³
      *    ³                                                           ³
      *    ³    CRT-PROGRAM (calling program)                          ³
      *    ³    CRT-HEADING (Screen heading)                           ³
      *    ³    CRT-COMPANY (Company name)                             ³
      *    ³    CRT-TERMINAL (consists of two fields:                  ³
      *    ³                  CRT-WRKHD and CRT-WRKST. See CRTHEAD)    ³
      *    ³    These fields should be populated at the start of the   ³
      *    ³    program.                                               ³
      *    ³    PERFORM HEADING-AND-CLEAR-SCREEN.                      ³
      *    ³                                                           ³
      *    ³ To change the screen heading:                             ³
      *    ³    MOVE "The new heading" TO CRT-HEADING.                 ³
      *    ³    PERFORM CHANGE-HEADING.                                ³
      *    ³                                                           ³
      *    ³ To change the time on the screen:                         ³
      *    ³    PERFORM CHANGE-TIME                                    ³
      *    ³                                                           ³
      *    ³ To clear line 50:                                         ³
      *    ³    PERFORM CLEAR-L50                                      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "CLEAR.CRT".

       COPY "ABORT.PRN".

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                                                           ³
      *    ³           R E A D   F I L E S   R O U T I N E S           ³
      *    ³                                                           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       AC000-READ        SECTION.

       COPY "CONTROL.RD".

       COPY "PARAM.RD".

       COPY "DBHIST.RD".

       COPY "DBTRAN.RD".

       COPY "DEBTOR.RD1".

       COPY "DTHIST.RD".

       COPY "SHARED.RD".

       COPY "DEBTOR.LUP".

       COPY "APAC.HLP".

      *
       CA155-GET-DEBTOR        SECTION 2.
       CA155-ACCOUNT.
           IF PREVIOUS-STATEMENT
               MOVE W80-ACNO         TO DBH-ACNO
               PERFORM READ-DBHIST THRU READ-DBHIST-EXIT
               MOVE DBH-RECORD1      TO DEB-RECORD1
           ELSE
               MOVE W80-ACNO         TO DEB-ACNO
               PERFORM READ-DEBTOR THRU READ-DEBTOR-EXIT.
           IF WS-F-ERROR = 6 OR 56
               MOVE 1                TO WS-ERROR
           ELSE
           IF DEB-ACNO NOT = W80-ACNO
               MOVE 2                TO WS-ERROR
           ELSE
               MOVE 0                TO WS-ERROR
               MOVE DEB-RECORD1      TO W40-DEBTREC.              

       CA160-EXIT.
             EXIT.
      *
      *    DTPNAME and DTPADDR programs introduced November 2014 to
      *    process new Debtor Record layout for storage, display and
      *    print purposes. These routines are no longer required and
      *    will be removed on the next source code cleanup.
      *
      *CA165-NAME              SECTION 2.
      *CA165-EXTRACT.
      *     MOVE ZERO   TO WS-ERROR.
      *      UNSTRING W60-NAME DELIMITED BY "*" OR "!"
      *        INTO W40-SURNAME W40-FNAME W40-SNAME
      *        OVERFLOW
      *  MOVE 1   TO WS-ERROR.
      *CA170-EXIT.
      *      EXIT.

      *CA175-ADDRESS           SECTION 2.
      *CA175-EXTRACT.
      *      MOVE 0                  TO WS-ERROR.
      *      UNSTRING W60-ADDRESS DELIMITED BY "*" INTO W40-ADD1 W40-ADD2 W40-ADD3 W40-ADD4
      *        OVERFLOW MOVE 1       TO WS-ERROR.
      *CA180-EXIT.
      *      EXIT.
      *    *************************************************************
      *    ****    P R I N T   S T A T E M E N T S
      *    *************************************************************
       XB000        SECTION 5.
       XB00-INIT.
             PERFORM ERASE-SCREEN.
             DISPLAY S15B.
             MOVE "N"                TO WS-OPTION.
             MOVE 9                  TO WS-MES-LINE.
             MOVE 32                 TO WS-MES-COL.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
          IF WS-OPTION = "N"
              GO TO XB299-EXIT.
      *
      *  ***  ****  ***** *   *   ****  ****  *** *   * ***** ***** ****
      * *   * *   * *     **  *   *   * *   *  *  **  *   *   *     *   *
      * *   * ****  ***   * * *   ****  ****   *  * * *   *   ***   ****
      * *   * *     *     *  **   *     *   *  *  *  **   *   *     *   *
      *  ***  *     ***** *   *   *     *   * *** *   *   *   ***** *   *
      *
      * ______________________________________________________________ *
      * PC_PRINTER_OPEN                                                *
      *                                                                *
      * Opens a printer channel, giving it a document title, and       *
      * optionally displaying the dialog boxes for printer control,    *
      * font selection, and progress indication.                       *
      *                                                                *
      * SYNTAX:                                                        *
      *                                                                *
      *         call "PC_PRINTER_OPEN" using     printer-handle        *
      *                                          document-title        *
      *                                by value  flags                 *
      *                                by value  window-handle         *
      *                                returning status-code           *
      *                                                                *
      * PARAMETERS:                                                    *
      *         printer-handle                   pic x(4) comp-5.      *
      *         document-title Group item defined as:                  *
      *         title-len                        pic x(2) comp-5.      *
      *         title-text                       pic x.                *
      *         flags Numeric literal or         pic x(4) comp-5.      *
      *         window-handle Numeric literal or pic x(4) comp-5.      *
      *         status-code See Key                                    *
      *                                                                *
      * ON ENTRY:                                                      *
      *         title-len  The length of the document title.           *
      *         title-text The title of the document to be             *
      *                    printed.                                    *
      *         flags   A set of bits to define printer options:       *
      *                 bit 0 Display a dialog for defining printer    *
      *                 characteristics (cannot be used with bit 2 or  *
      *                 bit 3)                                         *
      *         bit 1   Display a font dialog box, so you can select   *
      *                 the font for the document                      *
      *         bit 2   Print in portrait orientation (cannot be used  *
      *                 with bit 0 or bit 3)                           *
      *         bit 3   Print in landscape orientation (cannot be used *
      *                 with bit 0 or bit 2)                           *
      *         bit 4   Display a progress indicator dialog box        *
      *                                                                *
      *         All remaining bits are reserved for future use and     *
      *         should not be set.                                     *
      *                                                                *
      *         window-handle  Handle of the parent window. The parent *
      *                        window is used as a reference for       *
      *                        positioning dialog boxes on the screen. *
      *                        If window-handle is 0 positioning is    *
      *                        system dependent.                       *
      *                                                                *
      * ON EXIT:                                                       *
      *          printer-handle Returns a handle to be used for        *
      *                         subsequent printer operations.         *
      *          status-code    Printer status code:                   *
      *                                                                *
      *                          0  Successful                         *
      *                          1  Could not open printer device      *
      *                          5  Failed to open file                *
      *                          9  No default printer found. Select a *
      *                             printer from Windows Control Panel.*
      *                         24  User pressed Cancel on the printer *
      *                             setup dialog or the font selection *
      *                             dialog.                            *
      *                                                                *
      * COMMENTS:                                                      *
      *                                                                *
      *         This routine is available in 32-bit environments only. *
      *                                                                *
      *         You can check the success of the call by examining     *
      *         RETURN-CODE.                                           *
      * ______________________________________________________________ *
             move 20                to title-len.
             move "STMF07W - Statements" & x"00" to title-text.
             call "PC_PRINTER_OPEN" using by reference prn-handle by reference document-title by value prn-flags by value 0
             end-call.
             MOVE SPACES             TO ST7-DET.
             MOVE "O"                TO prn-open.

       XB10.
             PERFORM ERASE-SCREEN.
             MOVE "Y"                TO WS-EXPAND.
             DISPLAY "Statement message" AT 0602.
             DISPLAY "English"           AT 0803.
             DISPLAY "Afrikaans"         AT 1203.
             MOVE SPACES             TO W30-MESS-A(1) W30-MESS-A(2).

       XB11.
             ACCEPT S20.
             PERFORM CHECK-CORRECT.
           IF WS-OPTION = "N"
               GO TO XB11.
             DISPLAY "Print keyed message on accounts < (30), (60), (90) days >  <" AT 1802.
             DISPLAY "XX = print on all statements"                                 AT 2302 WITH FOREGROUND-COLOR Brown HIGHLIGHT.
             MOVE 2                  TO WS-LENGTH.

       XB11A.
             PERFORM TEST AFTER UNTIL WS-MESS = "30" OR "60" OR "90" OR "XX"
               ACCEPT WS-MESS AT 1859 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta AUTO
               CALL "CBL_TOUPPER" USING WS-MESS BY VALUE WS-LENGTH RETURNING WS-STATUS
             END-PERFORM.
             MOVE 1                  TO WS-LENGTH.
             MOVE "O"                TO WS-INSTR.
             MOVE "Print 'I'nvoice or 'O'ther reference  [I]" TO WS-OPT-MES.
             MOVE "I"                TO WS-OPTION.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO WS-REF.

       XB12.
             MOVE 1                  TO WS-SUB-3.
             MOVE 1                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-COMPANY        TO W95-COMP.
             MOVE PAR-TELEPHONE      TO W95-TEL.
             ADD 1                   TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-ADDRESS1       TO W95-ADD1.
             MOVE PAR-ADDRESS2       TO W95-ADD2.
             MOVE PAR-ADDRESS3       TO W95-ADD3.
             MOVE PAR-ADDRESS4       TO W95-ADD4.
             MOVE PAR-POST-CDE       TO W95-POST.
           IF W95-ADD4 = SPACES
               MOVE W95-POST         TO W95-ADD4
               MOVE SPACES           TO W95-POST.
           IF W95-ADD3 = SPACES
               MOVE W95-ADD4         TO W95-ADD3
               MOVE SPACES           TO W95-ADD4.
           IF W95-ADD2 = SPACES
               MOVE W95-ADD3         TO W95-ADD2
               MOVE SPACES           TO W95-ADD3.
             PERFORM ERASE-SCREEN.
      *
      *    Print Zero Balance Accounts
      *
             DISPLAY S01.
             MOVE "N"                TO WS-OPTION.
             MOVE 8                  TO WS-MES-LINE.
             MOVE 61                 TO WS-MES-COL.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "N"
               MOVE 1                TO WS-S8
           ELSE
               MOVE ZERO             TO WS-S8.
      *
      *    Selective Print
      *
             DISPLAY S02.
             MOVE "N"                TO WS-OPTION.
             MOVE 10                 TO WS-MES-LINE.
             MOVE 34                 TO WS-MES-COL.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "Y"
               MOVE 1                TO WS-NTP
               GO TO XB125
           ELSE
               MOVE ZERO             TO WS-NTP.
      *
      *    Is it a Re-Start
      *
             DISPLAY S03.
             MOVE "N"                TO WS-OPTION.
             MOVE 12                 TO WS-MES-LINE.
             MOVE 36                 TO WS-MES-COL.
      *
      *      ACCEPT the option at the position specified above
      *
             MOVE X"FA"              TO WS-MES-CHAR(48).
             MOVE 1                  TO WS-INSTR.
             PERFORM OPT-MESSAGE.
           IF WS-OPTION = "Y"
               GO TO XB130.
               
       XB14.
             MOVE "N"                TO WS-INSTR.
             MOVE "'A'll statements or 'R'ange of statements [A]" TO WS-OPT-MES.
             MOVE "A"                TO WS-OPTION.
             PERFORM OPT-MESSAGE.
             MOVE WS-OPTION          TO WS-RANGE.
             DISPLAY "Debtor account:" AT 1612.
             MOVE 1                  TO WS-SUB-2.

       XB14-RANGE-CHECK.
           IF WS-RANGE = "R"
               MOVE "R"              TO W40-INSTR
           ELSE
               MOVE "I"              TO W40-INSTR.
             MOVE "N"                TO W40-LUP.
             CALL "DTP\DTPReadAR" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W40-COMPANY.
           IF NOT(W40-RESULT = 0)
               GO TO XB299-EXIT.
      *
      *    ****  The following tests are now in DTPReadAR and have been
      *          commented out. Will be removed after testing of the 
      *          new system has been completed
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Start reading from the first account in the file.     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *XB15-INDEX.
      *    IF PREVIOUS-STATEMENT
      *        MOVE " !"             TO DBH-NAME
      *        PERFORM START-AT-DEBH-NAME THRU READ-DBHIST-EXIT
      *    ELSE
      *        MOVE " !"             TO DEB-NAME
      *        PERFORM START-AT-DEBT-NAME THRU READ-DEBTOR-EXIT.
      *    IF WS-F-ERROR = 6 OR 56
      *        GO TO XB299-EXIT.

       XB20-LOOP.
             PERFORM PRINT-ABORT.
           IF WS-OPTION = "Y"
               MOVE "**** PRINT ABORTED BY OPERATOR ****" TO ST7-DET
               move st7-det          to Print-Buffer
               PERFORM PRINT-DETAILS-AND-CLEAR
               PERFORM Throw-a-page
               GO TO XB299-EXIT.
           IF WS-RANGE = "R"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    Check if last account in the range has been printed.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               IF WS-END = "Y"
                   GO TO XB299-EXIT.
             MOVE "N"                TO W40-INSTR.
             MOVE "P"                TO W40-DSP-OR-PRN.
             CALL "DTP\DTPReadAR" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W40-COMPANY.
           IF NOT(W40-RESULT = 0)
               GO TO XB299-EXIT.
           IF WS-RANGE = "R"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Check if this is the last account in range to be printed. ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               IF W40-ACNO = WS-AEND
                   MOVE "Y"          TO WS-END.
             DISPLAY W40-ACNO AT 1628 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.

       XB21.
           IF W40-BAL = ZERO
               IF WS-S8 = 1
                   GO TO XB100-CHECK
               ELSE
               IF (W40-DEBT = ZERO) AND (W40-CRED = ZERO)
                   GO TO XB100-CHECK.
             PERFORM XB110 THRU XB115.
             MOVE SPACES             TO W40-NAME1.
           IF W40-BAL NOT > ZERO
               MOVE ZERO             TO W90-CUR
           ELSE
               COMPUTE W90-CUR = W40-BAL - (W40-CUR + W40-30 + W40-60 + W40-90 + W40-120 + W40-INT).
      *    *************************************************************
      *    ****         T E S T   F O R   I N D I V I D U A L       ****
      *    *************************************************************
           IF W40-TYPE = 1
               MOVE "P"              TO W40-INSTR
               CALL "DTP\DTPNAME" USING W40-COMPANY LS-USER-ID
           ELSE
               MOVE W40-NAME         TO W40-DSP-NAME.
      *
      *    Extract and format Addresses for printing
      *
             MOVE "P"                TO W40-INSTR.
             CALL "DTP\DTPADDR" USING W40-COMPANY.
           IF W40-PST-ADDRESS = SPACES
               MOVE W40-STR-ADDRESS  TO W40-PST-ADDRESS.
             MOVE 0                  TO WS-PAGE.
             MOVE 1                  TO WS-ADVANCE.

       XB50-STM.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                  Setup the printer font                   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             move FontTypeFace       to Font-family-name.
             move 12                 to Font-family-namelen.
             move font-12-CPI        to font-size.
             move 0                  to font-style.
             perform set-printer-font.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                    Print header Bitmap                    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             perform print-header-BMP.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Advance printer location to past the Bitmap to be printed ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             perform advance-printer 14 times.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                Set to print ñ 5cpi and Bold               ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             move font-5-CPI         to font-size.
             move 8                  to font-style.
             perform set-printer-font.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       This heading is printed at 5cpi and centered        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE "     S T A T E M E N T / S T A A T" TO Print-Buffer.
             MOVE 44                 TO Print-Buffer-Len.
             MOVE 1                  TO WS-ADVANCE.
             PERFORM PRINT-DETAILS.
             MOVE 96                 TO Print-Buffer-Len.
             ADD 1                   TO WS-PAGE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                    Set to print ñ 12cpi                   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             move font-12-CPI        to font-size.
             move 0                  to font-style.
             perform set-printer-font.

       XB60-DEBT.
             MOVE SPACES             TO ST7-DET.
             MOVE W40-DSP-NAME       TO ST7-NAME.
             MOVE W30-STL(1)         TO ST7-MESS.
             MOVE 0                  TO WS-ERROR.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-DETAILS-AND-CLEAR.
             MOVE W40-PADD-1         TO ST7-NAME.
             MOVE W30-STL(2)         TO ST7-MESS.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-DETAILS-AND-CLEAR.
             MOVE W40-PADD-2         TO ST7-NAME.
      *
      *    DTPNAME and DTPADDR programs introduced November 2014 to
      *    process new Debtor Record layout for storage, display and
      *    print purposes. These routines are no longer required and
      *    will be removed on the next source code cleanup.
      *
      *    IF W40-ADD2 = SPACES
      *        MOVE 1                TO WS-ERROR
      *        IF NOT(W40-POST = "0000" OR "9999")
      *            MOVE W40-POST     TO ST7-PC2.
             MOVE W30-STL(3)         TO ST7-MESS.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-DETAILS-AND-CLEAR.
             MOVE W40-PADD-3         TO ST7-NAME.
      *
      *    DTPNAME and DTPADDR programs introduced November 2014 to
      *    process new Debtor Record layout for storage, display and
      *    print purposes. These routines are no longer required and
      *    will be removed on the next source code cleanup.
      *
      *    IF WS-ERROR = 0
      *        IF W40-ADD3 = SPACES
      *            MOVE 1            TO WS-ERROR
      *            IF NOT(W40-POST = "0000" OR "9999")
      *                MOVE W40-POST TO ST7-PC2.
             MOVE W30-STL(4)         TO ST7-MESS.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-DETAILS-AND-CLEAR.
             MOVE W40-PADD-4         TO ST7-NAME.
      *
      *    DTPNAME and DTPADDR programs introduced November 2014 to
      *    process new Debtor Record layout for storage, display and
      *    print purposes. These routines are no longer required and
      *    will be removed on the next source code cleanup.
      *
      *    IF WS-ERROR = 0
      *        IF W40-ADD4 = SPACES
      *            MOVE 1            TO WS-ERROR
      *            IF NOT(W40-POST = "0000" OR "9999")
      *                MOVE W40-POST TO ST7-PC2.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-DETAILS-AND-CLEAR.
      *
      *    DTPNAME and DTPADDR programs introduced November 2014 to
      *    process new Debtor Record layout for storage, display and
      *    print purposes. These routines are no longer required and
      *    will be removed on the next source code cleanup.
      *
      *    IF WS-ERROR = 0
      *       IF NOT(W40-POST = "0000" OR "9999")
             MOVE W40-PADD-PC        TO ST7-PC2.
             move st7-det            to Print-Buffer.
             move 2                  to WS-ADVANCE.
             PERFORM PRINT-DETAILS-AND-CLEAR.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The use of letter heads is not yet available in this ver- ³
      *    ³ sion and will be included later.                          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    IF NOT(WS-HEAD = "N")
             MOVE "ACCOUNT NO:"      TO ST7-AH1
             MOVE "DATE:"            TO ST7-AH2
             MOVE "PAGE NO:"         TO ST7-AH3.
             MOVE W40-ACNO           TO ST7-AC1.
             MOVE W12-TODAY          TO ST7-DATE.
             MOVE WS-PAGE            TO ST7-PAGE.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-DETAILS-AND-CLEAR.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The use of letter heads is not yet available in this ver- ³
      *    ³ sion and will be included later.          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    IF NOT(WS-HEAD = "N")
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        Switch on bold printing         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             move 8                  to font-style.
             perform set-printer-font.
             MOVE "    Date       Description     Reference       Debit            Credit           Balance" TO ST7-HD1
             move st7-det            to Print-Buffer.
             move 1                  to WS-ADVANCE.
             PERFORM PRINT-DETAILS-AND-CLEAR.

       XB75-BFWD.
             MOVE 0                  TO W25-WHOLE W90-BAL.
           IF OPEN-ITEM
               GO TO XB76.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Check if there is a brought forward balance to print.    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF W40-STM = 0
               GO TO XB80-TRAN.
           IF W40-LANG = "A"
               MOVE "REK GELEWER"    TO ST7-BFWD
           ELSE
               MOVE "A/C RENDERED"   TO ST7-BFWD.
             MOVE W40-STM            TO ST7-TBAL W90-BAL.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-TRANSACTION-LINE.
             MOVE SPACES             TO ST7-DET.
             ADD 1                   TO W25-WHOLE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ For accounts that are Brought Forward skip the recalcula- ³
      *    ³ tion of age analysis.                                     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             GO TO XB80-TRAN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Calculate the age analysis for this account (OPEN ITEM)  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       XB76.
             MOVE ZERO               TO W35-CUR W35-30 W35-60 W35-90 W35-120 W35-150 W35-INT.
           IF PREVIOUS-STATEMENT
               MOVE W40-ACNO         TO TRH-AC
               MOVE ZERO             TO TRH-DATE TRH-CODE
               MOVE SPACES           TO TRH-REF
           ELSE
               MOVE W40-ACNO         TO TRA-AC
               MOVE ZERO             TO TRA-DATE TRA-CODE
               MOVE SPACES           TO TRA-REF.
           IF PREVIOUS-STATEMENT
               PERFORM START-AT-DTRH-KEY THRU READ-DTHIST-EXIT
           ELSE
               PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5 OR 55
               GO TO XB80-TRAN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Get the next transaction for calculation of the AGE ANA- ³
      *    ³  LYSIS for this account.                                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       XB78.
           IF PREVIOUS-STATEMENT
               PERFORM READ-DTHIST-NEXT THRU READ-DTHIST-EXIT
               MOVE TRH-RECORD1      TO TRA-RECORD1
           ELSE
               PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5 OR 55) OR (W40-ACNO NOT = TRA-AC)
               GO TO XB80-TRAN.
           IF TRA-VALUE < ZERO
               IF (TRA-VALUE + TRA-ALLOCATED) = ZERO
                   GO TO XB78
               ELSE
                   ADD TRA-ALLOCATED TO TRA-VALUE
                   MOVE ZERO         TO TRA-ALLOCATED
           ELSE
               IF TRA-VALUE = TRA-PAID
                   GO TO XB78
               ELSE
                   SUBTRACT TRA-PAID FROM TRA-VALUE
                   MOVE ZERO         TO TRA-PAID.
           IF TRA-VALUE > ZERO
               IF NOT(TRA-DATE > W30-120)
                   MOVE 5            TO TRA-AGE
               ELSE
               IF NOT(TRA-DATE > W30-90)
                   MOVE 4            TO TRA-AGE
               ELSE
               IF NOT(TRA-DATE > W30-60)
                   MOVE 3            TO TRA-AGE
               ELSE
               IF NOT(TRA-DATE > W30-30)
                   MOVE 2            TO TRA-AGE
               ELSE
               IF NOT(TRA-DATE > W30-CUR)
                   MOVE 1            TO TRA-AGE
               ELSE
                   MOVE ZERO         TO TRA-AGE.
                   
           IF TRA-CODE = 17 OR 20
               ADD TRA-VALUE         TO W35-INT
           ELSE
               EVALUATE TRA-AGE
                 WHEN 5   ADD TRA-VALUE  TO W35-150

                 WHEN 4   ADD TRA-VALUE  TO W35-120

                 WHEN 3   ADD TRA-VALUE  TO W35-90

                 WHEN 2   ADD TRA-VALUE  TO W35-60

                 WHEN 1   ADD TRA-VALUE  TO W35-30

                 WHEN 0   ADD TRA-VALUE  TO W35-CUR
               END-EVALUATE.
             GO TO XB78.

       XB80-TRAN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Background colour is used to show columns, to format the  ³
      *    ³ document as the ASCII box characters can not be used with ³
      *    ³ the windows print utility. This helps with readability    ³
      *    ³ and gives the document a more professional look.          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE 2                  TO fore-or-back.
           IF PREVIOUS-STATEMENT
               MOVE W40-ACNO         TO TRH-KEY
               PERFORM START-AT-DTRH-KEY THRU READ-DTHIST-EXIT
           ELSE
               MOVE W40-ACNO         TO TRA-KEY
               PERFORM START-AT-DTRN-KEY THRU READ-DBTRAN-EXIT.
           IF WS-F-ERROR = 5 OR 55
               GO TO XB90-SEND.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Get the next transaction to print on the statement.      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       XB85-TLOOP.
           IF PREVIOUS-STATEMENT
               PERFORM READ-DTHIST-NEXT THRU READ-DTHIST-EXIT
               MOVE TRH-RECORD1      TO TRA-RECORD1
           ELSE
               PERFORM READ-DBTRAN-NEXT THRU READ-DBTRAN-EXIT.
           IF (WS-F-ERROR = 5 OR 55) OR (TRA-AC NOT = W40-ACNO)
               GO TO XB90-SEND.
           IF TRA-VALUE = ZERO
               GO TO XB85-TLOOP.
             MOVE TRA-DATE           TO W22-DTE2.
             MOVE W22-CC2            TO W22-CC3.
             MOVE W22-YY2            TO W22-YY3.
             MOVE W22-MM2            TO W22-MM3.
             MOVE W22-DD2            TO W22-DD3.
             MOVE W22-DTE3           TO ST7-TDTE.
           IF WS-REF = "O"
               IF NOT(TRA-REG = SPACES)
                   MOVE TRA-REG      TO ST7-TREF
               ELSE
                   MOVE TRA-REF      TO ST7-TREF
               END-IF
           ELSE
           IF NOT(TRA-REF = "00000000" OR SPACES)
               MOVE TRA-REF          TO ST7-TREF.
           IF W40-LANG = "A"
               MOVE W00-A-DESC(TRA-CODE) TO ST7-TDESC
           ELSE
               MOVE W00-E-DESC(TRA-CODE) TO ST7-TDESC.
           IF TRA-VALUE > ZERO
               MOVE TRA-VALUE        TO ST7-TDBT
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                   A L L O C A T I O N S                   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               IF NOT(TRA-ALLOCATED = ZERO)
                   MULTIPLY -1 BY TRA-ALLOCATED GIVING ST7-TCRD
                   MOVE " Cr["       TO ST7-PD
                   MOVE "]"          TO ST7-CB
               END-IF
           ELSE
               MOVE TRA-VALUE        TO ST7-TCRD.
             ADD TRA-VALUE           TO W90-BAL.
             MOVE W90-BAL            TO ST7-TBAL.
             PERFORM PRINT-TRANSACTION-LINE.
             MOVE SPACES             TO ST7-DET.
             ADD 1                   TO W25-WHOLE.
           IF W25-WHOLE > WS-LINES
               GO TO XB120-MULTI.
             GO TO XB85-TLOOP.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  End of the STATEMENT details (transactions space). Print ³
      *    ³  end of page details and skip to the next page and print  ³
      *    ³  the headings and carried forward balance.                ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       XB90-SEND.
            ADD 1 WS-LINES           GIVING W25-DECIMAL.
            SUBTRACT W25-WHOLE       FROM W25-DECIMAL.
          IF W25-DECIMAL > ZERO
              PERFORM PRINT-TRANSACTION-LINE W25-DECIMAL TIMES.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³          A D V A N C E   P A P E R   1   L I N E          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             perform advance-printer.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³               Set to print ñ 8 cpi and Bold               ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             move font-8-CPI         to font-size.
             move 8                  to font-style.
             perform set-printer-font.
             MOVE 66                 to Print-Buffer-Len.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              W H I T E   B A C K G R O U N D              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             perform set-background-to-white.
             MOVE "REMITTANCE ADVICE"  TO ST7-HEADER.
             MOVE 0                  TO WS-ERROR WS-ERROR1.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-DETAILS-AND-CLEAR.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                   Set to print ñ 12cpi                    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             move font-12-CPI        to font-size.
             move 0                  to font-style.
             perform set-printer-font.
             MOVE 96                 to Print-Buffer-Len.
             MOVE W95-COMP           TO ST7-NAME.
             MOVE W40-DSP-NAME       TO ST7-MESS.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-DETAILS-AND-CLEAR.
           IF WS-PRINT-BNK = "Y"
               MOVE "Banking Details" TO ST7-NAME
           ELSE
               MOVE W95-ADD1         TO ST7-NAME.
             MOVE W40-PADD-1         TO ST7-MESS.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-DETAILS-AND-CLEAR.
           IF WS-PRINT-BNK = "Y"
               MOVE WS-BNK-NME       TO ST7-NAME
           ELSE
              MOVE W95-ADD2          TO ST7-NAME.
      *       IF W95-ADD2 = SPACES
      *           MOVE 1             TO WS-ERROR1
      *           IF NOT(W95-POST = ZERO)
      *               MOVE W95-POST  TO ST7-PC2.
            MOVE W40-PADD-2          TO ST7-MESS.
      *    IF W40-ADD2 = SPACES
      *        MOVE 1                TO WS-ERROR
      *        IF NOT(W40-POST = "0000" OR "9999")
      *            MOVE W40-POST     TO ST7-MESS.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-DETAILS-AND-CLEAR.
           IF WS-PRINT-BNK = "Y"
               MOVE WS-BRANCH        TO ST7-NAME
           ELSE
               MOVE W95-ADD3         TO ST7-NAME.
      *        IF WS-ERROR1 = 0
      *            IF W95-ADD3 = SPACES
      *                MOVE 1        TO WS-ERROR1
      *                IF NOT(W95-POST = ZERO)
      *                    MOVE W95-POST TO ST7-PC2.
             MOVE W40-PADD-3         TO ST7-MESS.
      *    IF WS-ERROR = 0
      *        IF W40-PADD-3 = SPACES
      *            MOVE 1            TO WS-ERROR
      *            IF NOT(W40-POST = "0000" OR "9999")
      *                MOVE W40-POST TO ST7-MESS.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-DETAILS-AND-CLEAR.
           IF WS-PRINT-BNK = "Y"
               MOVE WS-BANK-ACCOUNT  TO ST7-NAME
           ELSE
               MOVE W95-ADD4         TO ST7-NAME.
      *        IF WS-ERROR1 = 0
      *            IF W95-ADD4 = SPACES
      *                MOVE 1        TO WS-ERROR1
      *                IF NOT(W95-POST = ZERO)
      *                    MOVE W95-POST TO ST7-PC2.
             MOVE W40-PADD-4         TO ST7-MESS.
      *    IF WS-ERROR = 0
      *        IF W40-ADD3 = SPACES
      *            MOVE 1            TO WS-ERROR
      *            IF NOT(W40-POST = "0000" OR "9999")
      *                MOVE W40-POST TO ST7-MESS.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-DETAILS-AND-CLEAR.
           IF WS-PRINT-BNK = "Y"
               MOVE SPACES           TO ST7-NAME
           ELSE
      *    IF WS-ERROR1 = 0
      *        IF NOT(W95-POST = ZERO)
               MOVE W95-POST         TO ST7-PC2.
      *    IF WS-ERROR = 0
      *        IF NOT(W40-POST = "0000" OR "9999")
             MOVE W40-PADD-PC        TO ST7-MESS.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-DETAILS-AND-CLEAR.
             MOVE "ACCOUNT NO:"      TO ST7-AH1
             MOVE "DATE:"            TO ST7-AH2R.
             MOVE W40-ACNO           TO ST7-AC1.
             MOVE W12-TODAY          TO ST7-DATER.
             move 2                  to WS-ADVANCE.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-DETAILS-AND-CLEAR.
             MOVE "INTEREST"         TO ST7-A1.
             MOVE "91+ DAYS"         TO ST7-A2.
             MOVE "61-90 DAYS"       TO ST7-A3.
             MOVE "31-60 DAYS"       TO ST7-A4.
             MOVE "01-30 DAYS"       TO ST7-A5.
             MOVE "  AMOUNT DUE"     TO ST7-V3.
             move 1                  to WS-ADVANCE.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-DETAILS-AND-CLEAR.
             MOVE W40-INT            TO ST7-INT.
             ADD W40-60 W40-90 W40-120 GIVING ST7-3MTHS.
             MOVE W40-30             TO ST7-2MTHS.
             MOVE W40-CUR            TO ST7-1MTH.
             MOVE W90-CUR            TO ST7-CUR.
      *    ELSE
      *        ADD W40-90 W40-120    GIVING ST7-INT
      *        MOVE W40-60           TO ST7-3MTHS
      *        MOVE W40-30           TO ST7-2MTHS
      *        MOVE W40-CUR          TO ST7-1MTH
      *        ADD W40-INT W90-CUR   GIVING ST7-CUR.
             MOVE W40-BAL            TO ST7-TBAL.
             move 99                 to WS-ADVANCE.
             move st7-det            to Print-Buffer.
             PERFORM PRINT-DETAILS-AND-CLEAR.

       XB100-CHECK.
           IF WS-NTP = 1
               GO TO XB125.
             GO TO XB20-LOOP.

       XB110.
           IF W40-LANG = "A"
               MOVE 1                TO WS-MSUB
           ELSE
               MOVE 2                TO WS-MSUB.
             MOVE SPACES             TO W30-STM.
           IF WS-MESS = "XX"
               GO TO XB112.
           IF (W40-90 > ZERO) OR (W40-120 > ZERO)
               MOVE W30-90D (WS-MSUB) TO W30-STM
               GO TO XB115.
           IF WS-MESS = "90"
               GO TO XB112.
           IF W40-60 > ZERO
               MOVE W30-60D(WS-MSUB) TO W30-STM
               GO TO XB115.
           IF WS-MESS = "60"
               GO TO XB112.
           IF W40-30 > ZERO
               MOVE W30-30D(WS-MSUB) TO W30-STM
              GO TO XB115.

       XB112.
             MOVE W30-MESS-A(WS-MSUB) TO W30-STM.

       XB115.
             EXIT.

       XB120-MULTI.
             PERFORM XB90-SEND.
             PERFORM XB50-STM THRU XB60-DEBT.
             MOVE ZERO               TO W25-WHOLE.
             GO TO XB85-TLOOP.

       XB125.
             MOVE 12                 TO CRT-START.
             MOVE 14                 TO CRT-END.
             PERFORM ERASE-SCREEN.
             DISPLAY "Account No:" AT 1212.
             MOVE 1224               T0 W40-CPOS.
             MOVE "U"                TO W40-INSTR.
             CALL "DTP\DTPReadAR" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W40-COMPANY.
           IF W80-ACNO = SPACES
               GO TO XB299-EXIT.
             GO TO XB21.

      *
      *    Restart Account Number
      *
       XB130.
             MOVE 12                 TO CRT-START.
             MOVE 20                 TO CRT-END.
             PERFORM ERASE-SCREEN.
             DISPLAY "Debtor account:" AT 1612.
             MOVE "S"                TO W40-INSTR.
             MOVE "N"                TO W40-LUP.
             CALL "DTP\DTPReadAR" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W40-COMPANY.
          IF W40-START-AC = SPACES
              GO TO XB200-END.

       XB135.
             MOVE "N"                TO W40-INSTR.
             MOVE "P"                TO W40-DSP-OR-PRN.
             CALL "DTP\DTPReadAR" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W40-COMPANY.
           IF NOT(W40-RESULT = 0)
               GO TO XB130.
             DISPLAY W40-ACNO AT 1428 WITH FOREGROUND-COLOR Grey HIGHLIGHT BACKGROUND-COLOR Magenta.
             GO TO XB21.

       XB200-END.

       XB299-EXIT.
            EXIT.

       YA000           SECTION 8.
       YA00.
             MOVE "P"                TO W40-INSTR
             CALL "DTP\DTPReadAR" USING LS-PARID LS-USER-ID LS0-PROGRAMS LS0-SECURITY DATA-FILE-IDS W40-COMPANY.
             MOVE W40-RESULT         TO WS-STMT.
           IF PREVIOUS-STATEMENT
               MOVE W40-TODAY        TO W12-TODAY
               MOVE W40-AGE-DATES    TO W30-AGE-DATES.

       YA999.
            EXIT.
      *    *************************************************************
      *                I N I T I A L I S E   P R O G R A M
      *    *************************************************************
       ZA000-INIT       SECTION 9.
       ZA00-OPEN.
             MOVE LS-PARID           TO WS-PARID.
             MOVE LS-TODAY-DDMMYY    TO TODAY-DDMMYY W12-TODAY.
             MOVE LS-TODAY-YYMMDD    TO W12-T-YMD.
             MOVE LS-USUB            TO WS-USUB.
             MOVE LS-COMPANY         TO CRT-COMPANY.
             MOVE LS-TERMINAL        TO CRT-TERMINAL.
             PERFORM HEADING-AND-CLEAR-SCREEN.      
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup various keys and the mouse for use with menus and  ³
      *    ³  other required user interface functions and procedures.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       COPY "FUNCTION.PRO".

             STRING W02-STMBMP DELIMITED BY " " x"00" DELIMITED SIZE INTO BmpFilename.

           IF LS-USER NUMERIC
               IF LS-USE > ZERO AND < 110
                   MOVE LS-USE       TO WS-USUB
                   GO TO ZA01A.
             MOVE 110                TO WS-USUB.

       ZA01A.
             ADD 2549 WS-USUB        GIVING W25-RESULT.
             MOVE W25-KEY            TO WS-PARKEY.
             ADD 1 W25-SUB           GIVING WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.

       ZA02.
             MOVE 6                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
           IF NOT(PAR-CON-STM = "N" OR "Y")
               MOVE "N"              TO PAR-CON-STM.
           IF NOT(PAR-CON-TYPE = "A" OR "S")
               MOVE "S"              TO PAR-CON-TYPE.
             MOVE PAR-CON-STM        TO W05-CON-STM.
             MOVE PAR-CON-TYPE       TO W05-CON-TYPE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Test if the system has been setup for USB/WINDOWS printer ³
      *    ³ and if not set default detail lines per statement to 34.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF PAR-STM-DET< 25 OR > 38
               MOVE 34               TO PAR-STM-DET
               REWRITE PAR-RECORD1.
             MOVE PAR-STM-DET        TO WS-LINES.
             MOVE 11                 TO WS-PARKEY.
             MOVE 1                  TO WS-SUB-2.

       ZA15-READ-TRANS.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE 1                  TO WS-SUB-1.

       ZA20-TRAN-LOOP.
             MOVE PAR-T-CODE(WS-SUB-1) TO W00-TRAN-CODE(WS-SUB-2).
             MOVE PAR-E-DESC(WS-SUB-1) TO W00-E-DESC(WS-SUB-2).
             MOVE PAR-A-DESC(WS-SUB-1) TO W00-A-DESC(WS-SUB-2).
             MOVE PAR-ACTION(WS-SUB-1) TO W00-ACTION(WS-SUB-2).
             ADD 1                   TO WS-SUB-2.
           IF WS-SUB-1 < 2
               ADD 1                 TO WS-SUB-1
               GO TO ZA20-TRAN-LOOP.
           IF WS-PARKEY < 60
               ADD 1                 TO WS-PARKEY
               GO TO ZA15-READ-TRANS.

       ZA25.
             MOVE 4                  TO WS-PARKEY.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
             MOVE PAR-AGE            TO WS-AGE.
      *      MOVE 1                  TO WS-PARKEY.
      *      PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *      MOVE PAR-DMY            TO W12-TODAY.
      *      MOVE PAR-YMD            TO W12-T-YMD.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The use of letter heads is not yet available in this ver- ³
      *    ³ sion and will be included later.                          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    IF PAR-STMINV = "S" OR "X"
      *        MOVE "N"   TO WS-HEAD
      *    ELSE
      *        MOVE "Y"   TO WS-HEAD.
      *    *************************************************************
      *    ****   G E T   D E B T O R   M O N T H - E N D   D A Y   ****
      *    *************************************************************
             MOVE 1                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
      *
      *    ****    S E T   U P   A G E   D A T E S
      *
             MOVE W12-T-YMD          TO W22-DTE2.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The month end dates are stored in the Control file in the ³
      *    ³ first record as Cur, 30d, 60d, 90d and 120d and the next  ³
      *    ³ routine is used since Ver 8.11.06.                        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE DEB-CUR-DTE        TO W30-CUR.
             MOVE DEB-30D-DTE        TO W30-30.
             MOVE DEB-60D-DTE        TO W30-60.
             MOVE DEB-90D-DTE        TO W30-90.
             MOVE DEB-120-DTE        TO W30-120.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Get the banking details to be used for payments via EFT.  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE 4                  TO WS-NETKEY.
             PERFORM READ-CONTROL THRU READ-CONTROL-EXIT.
             MOVE G-BANK-NAME        TO WS-BNK-NME.
             MOVE G-BANK-AC-TYP      TO WS-ACT-TYP.
             MOVE G-BANK-CDE         TO WS-BR-CODE.
      *    IF G-BNK-AC-LNGTH = 10
      *        MOVE G-BANK-AC2       TO WS-ACT-NO
      *    ELSE
               MOVE G-BANK-AC-NO     TO WS-ACT-NO.
           IF WS-ACT-TYP = "C" OR "T" OR "S" OR "R"
               MOVE "Y"              TO WS-PRINT-BNK
               EVALUATE WS-AC-TYP
                 WHEN "C"    MOVE "Cheque"       TO WS-ACT-TYPE
                 WHEN "S"    MOVE "Savings"      TO WS-ACT-TYPE
                 WHEN "R"    MOVE "Credit Card"  TO WS-ACT-TYPE
                 WHEN "T"    MOVE "Transmission" TO WS-ACT-TYPE
               END-EVALUATE.
             GO TO ZA999-EXIT.

       COPY "ZA49.PRO".

       ZA200.

       COPY "LOCKED.PRO".

       ZA205.
             EXIT PROGRAM.

       ZA999-EXIT.
             EXIT.
      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000       SECTION 9.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
             MOVE ZERO               TO WS-KEY.
             EVALUATE WS-F-ERROR
               WHEN 2          MOVE W02-NETWORK  TO WS-FILE
                               MOVE WS-NETKEY    TO WS-KEY
                     
               WHEN 5          MOVE W02-DBTRAN   TO WS-FILE
                               MOVE TRA-KEY      TO WS-KEYX

               WHEN 6          MOVE W02-DEBTOR   TO WS-FILE
                               MOVE DEB-ACNO     TO WS-KEYX
                               
               WHEN 15         MOVE WS-PARID     TO WS-FILE
                               MOVE WS-PARKEY    TO WS-KEY
                               
               WHEN 37         MOVE W02-SHARED   TO WS-FILE
                               MOVE WS-SHARED    TO WS-KEY
                               
               WHEN 55         MOVE W02-DTHIST  TO WS-FILE
                               MOVE TRH-KEY  TO WS-KEYX
                               
               WHEN 56         MOVE W02-DBHIST  TO WS-FILE
                               MOVE DBH-ACNO  TO WS-KEYX                               

               WHEN OTHER      MOVE "ERROR"      TO WS-FILE
             END-EVALUATE.

       COPY "DISPERR.PRO".

       9999-Errors     section 9.
       9999-Display-Error.

             display Operation "Error: " Return-Code.
             evaluate Return-Code
               when 0  display "Success"
               when 1  display "Could not open printer device"
               when 2  display "Invalid printer control code specified"
               when 3  display "No printer device associated with specified handle"
               when 4  display "Out of memory while printing"
               when 5  display "Failed to open file"
               when 6  display "Disk full while spooling file"
               when 7  display "Print job aborted. No job sent to print spooler"
               when 8  display "Printer information structure badly constructed"
               when 9  display "No default printer found"
               when 10 display "Error attempting to display dialog"
               when 11 display "Write failure"
               when 12 display "No fonts found usable with this printer"
               when 13 display "The font requested does not exist"
               when 14 display "User aborted print job"
               when 15 display "Reserved"
               when 16 display "Reserved"
               when 17 display "Reserved"
               when 18 display "Failed to load bitmap"
               when 19 display "Invalid bitmap id"
               when 20 display "Failed to free bitmap"
               when 21 display "Failed to print bitmap"
               when 22 display "Bad parameter"
               when 23 display "Internal error"
               when 24 display "User pressed Cancel on Dialog box"
               when other display "Unknown error"
             end-evaluate.

             stop run.
