      $SET LINKCOUNT"256" GNT "PRINTUSB.GNT"
      ******************************************************************
      *                                                                *
      *       *******    *******    ******   **    **   ********       *
      *       **    **   **    **     **     ***   **      **          *
      *       **    **   **    **     **     ** *  **      **          *
      *       *******    *******      **     ** *  **      **          *
      *       **         **    **     **     **  * **      **          *
      *       **         **    **     **     **   ***      **          *
      *       **         **    **   ******   **    **      **          *
      *                                                                *
      *     ENGLISH                                                    *
      *                                                                *
      *    P R I N T   U T I L I T Y   P R O G R A M                   *
      *                                                                *
      *       Version 9.04.05 - June 2018                              *

      *                                                                *
      ******************************************************************
      *                                                                *
      *    All code for USB/WINDOWS printing has been written in lower *
      *    case to enable it to be easiliy identified while the new    *
      *    printing routines are being implemented.                    *
      *    This will be changed to the normal standard once everything *
      *    is working correctly.                                       *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PRINTUSB.
       AUTHOR.         J W LEMMON (APAC).
       DATE-WRITTEN.   November 2009 - WIN/USB.

           This program replaces the previous program, that only allowed
           for printing to the LPT and COM ports. This program now allows
           for USB, Network or any printers that need the Windows print
           process, while still allowing for the previous printers.

           This program is used to print/spool/display report details. 
 
           The program performs the following functions:

           Open the printer.
           Open the spool file.
           Set up the necessary parameters if the report is to appear
           on the screen.
           Print a line and advance the paper as requested.
           Spool a line and pass on the advance parameters.
           Display a line of print as requested (allows for left/right).

           The calling program/s treat all reports as if they were being
           printed on a printer and are unaware of the Users request to
           spool or display the report.

      ******************************************************************
                COPYRIGHT NOTICE: COPYRIGHT (C) 2009 - 2017
                                  by James William Lemmon.
                                    (Id No. 4412165050082).

                All rights reserved.

                e-mail jwlemmon@gmail.com.
      ******************************************************************

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

       COPY "PARAM.SL".

          SELECT PRNSPL  ASSIGN W02-SPOOLER
                          ORGANIZATION LINE SEQUENTIAL.

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

       COPY "PARAM.FDE".

       FD  PRNSPL.
       01  SPL-L1.
           03  SPL-CONT.
               05  SPL-ADV     PIC  9(02).
               05  SPL-LIN     PIC  9(02).
           03  SPL-DETAIL1     PIC  X(194).

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
       77  WS-CHECK                PIC  X(18)    VALUE "aeWlimemnomLalismJ".
       77  WS-S1                   PIC  9(04)    COMP-5.
       77  WS-S2                   PIC  9(04)    COMP-5.
       77  WS-S3                   PIC  9(04)    COMP-5.
       77  WS-S4                   PIC  9(04)    COMP-5.
       77  WS-S5                   PIC  9(04)    COMP-5.
       77  WS-S6                   PIC  9(04)    COMP-5.
       77  WS-S7                   PIC  9(04)    COMP-5.
       77  WS-S8                   PIC  9(04)    COMP-5.
       77  WS-LEFT                 PIC  9(04)    COMP-5.
       77  WS-RIGHT                PIC  9(04)    COMP-5.
       77  WS-NETKEY               PIC  9(06)    COMP-5.
       77  WS-PARKEY               PIC  9(06)    COMP-5.
       77  WS-INSTR                PIC  X(01).
       77  WS-OPTION               PIC  X(01).
       77  WS-SINGLE               PIC  X(01).
       77  WS-SKIP                 PIC  X(01).
       77  WS-ERROR                PIC  9(01)    VALUE ZERO.
       77  WS-LINES                PIC  9(02)    VALUE ZERO.
       77  WS-PRN-NO               PIC  X(01)    COMP-X VALUE 9.
       77  WS-PRN-STAT             PIC  X(01)    COMP-X.
       77  TODAY-DDMMYY            PIC  9(08)    COMP-5.
       77  WS-USUB                 PIC  9(04)    COMP-5.
      *
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Used by calling program (WORKING-STORAGE) and called program ³
      *    ³ (LINKAGE SECTION) for Main screen layout and headings.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  CRT-HEADINGS.
      *
      *    TYPE:
      *       C = Clear comment line (50)
      *       E = Clear lines (any line or lines between 5 and 46)
      *       H = Change heading
      *       S = Clear the screen and display headings
      *
           03  CRT-TYPE              PIC  X(01).
      *
      *    The following two fields are used for clearing a portion of
      *    the screen. (Start line and end line)
      *
           03  CRT-START           PIC  9(02).
           03  CRT-END             PIC  9(02).
      *
      *    Calling Program
      *
           03  CRT-PROGRAM         PIC  X(15) VALUE "PRINTUSB".
      *
      *    Screen Heading
      *
           03  CRT-HEADING         PIC  X(40) VALUE "UTILITY - PRINT USB/SPOOL".
      *
      *    Company Name
      *
           03  CRT-COMPANY         PIC  X(40).
      *
      *    WorkStation / Supervisor
      *
           03  CRT-TERMINAL.
               05  CRT-WRKHD       PIC  X(11).
               05  CRT-WRKST       PIC  X(03).

       COPY "WS.WS".

       01  WS-PARID.
           03  WS-SYS-ID           PIC  X(03).

      *  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *  ³  PRINTER ROUTINES FOR USB & WINDOWS PRINTERS THAT DO NOT  ³
      *  ³  USE STANDARD RAW CODE FOR FORMATING.                     ³
      *  ³  INITIALLY ALL PARAMETERS AND LAYOUTS HAVE BEEN CODED AND ³
      *  ³  INCLUDED. ONCE THE SYSTEM IS RUNNING AND ALL REQUIRED    ³
      *  ³  OPTIONS HAVE BEEN INCLUDED, ALL UNECESSARY DEFINITIONS   ³
      *  ³  AND CODE WILL BE REMOVED.                                ³
      *  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      ******************************************************************
      * KEY                                                            *
      *                                                                *
      * Descriptions for all of the call-by-name routines appear       *
      * alphabetically. Each description contains the routine name and *
      * function followed by these sections (as appropriate):          *
      *                                                                *
      * SYNTAX:     Shows the CALL statement you could use to call the *
      *             routine.                                           *
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
      *             of holding positive values from 0 to 65535; for    *
      *             example, PIC X(2) COMP-5.                          *
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
      *    ³          Parameters for PC_PRINTER_DEFAULT_NAME           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Default-Printer-Flags   pic  x(04) comp-5  Value 1.
       01  Default-Printer-Name.
           03  Default-Printer-Name-Len  pic  x(02) comp-5.
           03  Default-Printer-Name-Text pic  x(80).

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           Parameters for PC_PRINTER_DEFAULT_FONT          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Default-Font-Family-Name.
           03  Default-Font-Name-Len  pic  x(02) comp-5.
           03  Default-Font-Name-Text pic  x(80).
       01  Default-Font-Size       pic  x(04) comp-5.
       01  Default-Font-Style      pic  x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       Parameters for PC_PRINTER_DEFAULT_PROPERTIES        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Default-Properties-Flags pic x(04) comp-5.
       01  Default-Properties.
           03 pr-len               pic  x(02) comp-5.
           03 pr-papersize         pic s9(04) comp-5.
           03 pr-paperlength       pic s9(04) comp-5.
           03 pr-paperwidth        pic s9(04) comp-5.
           03 pr-scale             pic s9(04) comp-5.
           03 pr-copies            pic s9(04) comp-5.
           03 pr-papertray         pic s9(04) comp-5.
           03 pr-printquality      pic s9(04) comp-5.
           03 pr-color             pic s9(04) comp-5.
           03 pr-duplex            pic s9(04) comp-5.
           03 pr-orientation       pic s9(04) comp-5.
           03 pr-yresolution       pic s9(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              Parameters for PC_PRINTER_OPEN               ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Printer-Handle          pic  x(04) comp-5.
       01  Document-Title.
           03  Title-Len           pic  x(02) comp-5.
           03  Title-Text          pic  x(80).
       01  Printer-Flags           pic  x(04) comp-5.
       01  Window-Handle           pic  x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³              Parameters for PC_PRINTER_WRITE              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      * ---Printer-Handle (same as above)
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        Allow for Lanscape printing on USB printers        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  Print-Buffer.
           03  Print-Cond          pic  x(194).

       01  Print-Buffer-Len        pic  x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ As there is no File Definition for the print file, the   ³
      *    ³ following layout will be used for control of the screen  ³
      *    ³ display and print line setup.                            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  Rep-line.
           03  Rep-detail          pic  x(194).
       01  R-sl1                              redefines Rep-Line.
           03  R-sla               pic  x(78).
           03  filler              pic  x(58).
       01  R-sl2                              redefines Rep-line.
           03  filler              pic  x(58).
           03  R-slb               pic  x(78).
       01  R-CHARACTERS                       REDEFINES REP-LINE.
           03  Rep-char            pic  x(01) occurs 194.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³             Parameters for PC_PRINTER_LOAD_BMP            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      * ---Printer-Handle (same as above)
       01  BmpFilename             pic  x(64).
       01  BmpID                   pic  x(04) comp-5.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³             Parameters for PC_PRINTER_WRITE_BMP           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      * ---Printer-Handle (same as above)
      * ---BmpID   (same as above)
       01  WriteBmpReserved        pic  x(04) comp-5 value 0.
       01  BmpRow                  pic  x(04) comp-5 value 1.
       01  BmpCol                  pic  x(04) comp-5 value 1.
       01  BmpWidth                pic  x(04) comp-5 value 92.
       01  BmpHeigth               pic  x(04) comp-5 value 12.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³            Parameters for PC_PRINTER_WRITE_BMP            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      * ---Printer-Handle (same as above)
      * ---BmpID   (same as above)

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³            Parameters for PC_PRINTER_SET_COLOR            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      * ---Printer-Handle (same as above)
       01  fore-or-back                    pic x(02) comp-5.
       01  color-red                       pic x(02) comp-5.
       01  color-green                     pic x(02) comp-5.
       01  color-blue                      pic x(02) comp-5.

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
      *    ³               Parameters for PC_PRINT_FILE                ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       01  Filename.
           03  Filename-Len                pic x(02) comp-5.
           03  Filename-Text               pic x(80).

       01  DocumentTitle.
           03  DocumentTitle-Len           pic x(02) comp-5.
           03  DocumentTitle-Text          pic x(15).
      * --- Printer-Flags (same as above)
      * --- Window-Handle (Same as above)

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³               Parameters for PC_PRINTER_INFO              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       78 PrinterNameBufferBegin                       value next.
       01 PrinterNameBuffer                pic x(255).
       78 PrinterNameBufferLength                      value next
          - PrinterNameBufferBegin.
       78 PrinterTypeBufferBegin                       value next.
       01 PrinterTypeBuffer                pic x(255).
       78 PrinterTypeBufferLength                      value next
          - PrinterTypeBufferBegin.
       78 PrinterDeviceBufferBegin                     value next.
       01 PrinterDeviceBuffer              pic x(255).
       78 PrinterDeviceBufferLength                    value next
          - PrinterDeviceBufferBegin.
       78 PrinterLocationBufferBegin                   value next.
       01 PrinterLocationBuffer            pic x(255).
       78 PrinterLocationBufferLength                  value next
          - PrinterLocationBufferBegin.
       78 PrinterCommentBufferBegin                    value next.
       01 PrinterCommentBuffer             pic x(255).
       78 PrinterCommentBufferLength                   value next
          - PrinterCommentBufferBegin.
       78 PrinterPaperSizeBufferBegin                  value next.
       01 PrinterPaperSizeBuffer           pic x(255).
       78 PrinterPaperSizeBufferLength                 value next
          - PrinterPaperSizeBufferBegin.

       01  PrinterInformation.
           03  PrinterInformationLength    pic x(04) comp-5.
           03  DeviceContextHandle         pic x(04) comp-5.
           03  reserved                    pic x(04) comp-5.
           03  Orientation                 pic x(04) comp-5.
           03  PageRowsCurrentFont         pic x(04) comp-5.
           03  PageColsCurrentFont         pic x(04) comp-5.
           03  RowsRemainingCurrentFont    pic x(04) comp-5.
           03  MaxGraphicsHorizontalAxis   pic x(04) comp-5.
           03  MaxGraphicsVerticalAxis     pic x(04) comp-5.
           03  MinGraphicsHorizontalAxis   pic x(04) comp-5.
           03  MinGraphicsVerticalAxis     pic x(04) comp-5.
      *
      *    -------  Horizontal
      *
           03  CurrentCooridinateX         pic x(04) comp-5.
      *
      *    -------  Vertical
      *
           03  CurrentCooridinateY         pic x(04) comp-5.

       01  PrinterInfoNotForAbove.
           03  NumberOfCopies              pic x(02) comp-5.
      *
      *    -------  Draft, Low,Medium, High, Default or DPI
      *
           03  PrintQuality                pic x(02) comp-5.
      *
      *    -------  Monochrome or Color
      *
           03  Color                       pic x     comp-5.
           03  reserved                    pic x     comp-5.
           03  DriverVersionNumber         pic x(02) comp-5.
           03  PrinterName.
               05  PrinterNameLength       pic x(04) comp-5 value PrinterNameBufferLength.
               05  p-PrinterNameBuffer                      pointer.
           03  PrinterType.
               05  PrinterTypeLength       pic x(04) comp-5 value PrinterTypeBufferLength.
               05  p-PrinterTypeBuffer                      pointer.
           03  PrinterDevice.
               05 PrinterDeviceLength      pic x(04) comp-5 value PrinterDeviceBufferLength.
               05 p-PrinterDeviceBuffer                     pointer.
           03  PrinterLocation.
               05 PrinterLocationLength    pic x(04) comp-5 value PrinterLocationBufferLength.
               05 p-PrinterLocationBuffer                   pointer.
           03  PrinterComment.
               05 PrinterCommentLength     pic x(04) comp-5 value PrinterCommentBufferLength.
               05 p-PrinterCommentBuffer                    pointer.
           03  PrinterPaperSize.
               05 PrinterPaperSizeLength   pic x(04) comp-5 value PrinterPaperSizeBufferLength.
               05 p-PrinterPaperSizeBuffer                  pointer.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Used to display error messages for the Print Routines.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       01  Operation                       pic x(30).

       01   printer-control.

           03  Default-Printer-Name.
               05  Default-Printer-Name-Len  pic x(02) comp-5.
               05  Default-Printer-Name-Text pic x(80).

           03  Font-family.
               05  Font-family-namelen pic x(02) comp-5 value 80.
               05  Font-family-name    pic x(80).

           03  FontTypeFace        pic  x(20) value spaces.
           03  FontTypeFaceC       pic  x(13) value "Courier New" & x"00".
           03  FontTypeFaceL       pic  x(18) value "Letter Gothic Std" & x"00".
           03  font-style          pic  x(04) comp-5 value 0.
           03  font-size           pic  x(04) comp-5 value 0.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³            Set to print ñ 5 characters per inch           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-5-CPI          pic  x(04) comp-5 value 24.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           Set to print ñ 5.2 characters per inch          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-5p2-CPI        pic  x(04) comp-5 value 23.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           Set to print ñ 5.5 characters per inch          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-5p5-CPI        pic  x(04) comp-5 value 22.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           Set to print ñ 5.8 characters per inch          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-5p8-CPI        pic  x(04) comp-5 value 21.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Set to print ñ 6 characters per inch: P=47; L=68      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-6-CPI          pic  x(04) comp-5 value 20.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           Set to print ñ 6.5 characters per inch          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-6p5-CPI        pic  x(04) comp-5 value 19.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Set to print ñ 6.8 characters per inch: P=52; L=75    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-6p8-CPI        pic  x(04) comp-5 value 18.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Set to print ñ 7 characters per inch: P=55; L=80      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-7-CPI          pic  x(04) comp-5 value 17.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Set to print ñ 7.5 characters per inch: P=59; L=84    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-7p5-CPI        pic  x(04) comp-5 value 16.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    Set to print ñ 8 characters per inch: P=63; L=90       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-8-CPI          pic  x(04) comp-5 value 15.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    Set to print ñ 8.5 characters per inch: P=67; L=97     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-8p5-CPI        pic  x(04) comp-5 value 14.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    Set to print ñ 9 characters per inch: P=72; L=104      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-9-CPI          pic  x(04) comp-5 value 13.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    Set to print ñ 10 characters per inch: P=78; L=113     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-10-CPI         pic  x(04) comp-5 value 12.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    Set to print ñ 11 characters per inch: P=86; L=123     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-11-CPI         pic  x(04) comp-5 value 11.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    Set to print ñ 12 characters per inch: P=94; L=135     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-12-CPI         pic  x(04) comp-5 value 10.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³   Set to print ñ 13/14 characters per inch: P=105; L=151  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-13-CPI         pic  x(04) comp-5 value 9.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    Set to print ñ 15 characters per inch: P=118; L=169    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-15-CPI         pic  x(04) comp-5 value 8.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    Set to print ñ 17 characters per inch: P=135; L=194    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-17-CPI         pic  x(04) comp-5 value 7.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    Set to print ñ 20 characters per inch: P=157; L=226    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-20-CPI         pic  x(04) comp-5 value 6.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³           Set to print ñ 24 characters per inch           ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

           03  font-24-CPI         pic  x(04) comp-5 value 5.

           03  abort               pic  x(04) comp-5 value 1.
           03  prn-control         pic  x(04) comp-5 value 2.
           03  prn-flags           pic  x(04) comp-5 value 1.
           03  prn-command         pic  x(04) comp-5 value 4.
           03  prn-handle          pic  x(04) comp-5.

           03  prn-line-size       pic  x(04) comp-5 value 0.
           03  prn-line-94         pic  x(04) comp-5 value 94.
           03  prn-line-136        pic  x(04) comp-5 value 136.
           03  prn-line-194        pic  x(04) comp-5 value 194.
      *
      *    -------    The buffer used for printing
      *               Formatted data is moved to this buffer for
      *               printing. This buffer may be redefined to
      *               represent different buffer sizes (Font size)
      *
       01  W01-Print-Detail.
      *
      *    -------    Font size 7
      *
           03  W01-Print-Line-194.
               05  W01-Print-Line-136.
                   07  W01-Detail-136.
      *
      *    -------    Font size 10
      *
                       09  W01-Print-Line-94.
                           11 W01-Detail-94  pic  x(94).
                       09  filler  pic  x(42).
                   07  filler      pic  x(58).

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

       01  W05-SCREEN.
           03  W05-PRINT-DETAIL.
               05  W05-LINE                   OCCURS 40.
                   07  W05-TYPE    PIC  X(01).
                   07  W05-DETAIL  PIC  X(136).
           03  W05-PRINT-DETAIL2              REDEFINES W05-PRINT-DETAIL.
               05  W05-LINE2                  OCCURS 40.
                   07  FILLER      PIC  X(01).
                   07  W05-SLA     PIC  X(78).
                   07  FILLER      PIC  X(58).
           03  W05-PRINT-DETAIL2              REDEFINES W05-PRINT-DETAIL.
               05  W05-LINE3                  OCCURS 40.
                   07  FILLER      PIC  X(01).
                   07  FILLER      PIC  X(58).
                   07  W05-SLB     PIC  X(78).
           03  W05-PRINT-DETAIL3              REDEFINES W05-PRINT-DETAIL.
               05  W05-LINE4                  OCCURS 40.
                   07  FILLER      PIC  X(01).
                   07  W05-CHAR    PIC  X(01) OCCURS 136.

       01  W06-SCREEN.
           03  W06-PRINT-DETAIL.
               05  W06-LINE                   OCCURS 40.
                   07  W06-CHAR    PIC  X(01) OCCURS 78.

       01  W08-EXPD.
           03  W08-CHAR            PIC  X(06).
           03  W08-RCHR                       REDEFINES W08-CHAR.
               05  W08-DEC         PIC  X(01) COMP-X OCCURS 6.

       01  W25-CALCS.
           03  W25-RESULT          PIC  9(05)V99.
           03  W25-RESULT1                       REDEFINES W25-RESULT.
               05  W25-WHOLE       PIC  9(05).
               05  W25-DECIMAL     PIC  9(02).
           03  W25-RESULT2                       REDEFINES W25-RESULT.
               05  W25-KEY         PIC  9(04).
               05  W25-SUB         PIC  9(01).
               05  FILLER          PIC  9(02).

       COPY "W40.WS".

       COPY "FUNCTION.WS".

       01  W100-STORE-DATA.
           03  W100-PRINT-DETAIL   PIC X(136).
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The redefinition below has been include as a temporary    ³
      *    ³ solution for some of the Accounts receivable report       ³
      *    ³ headings, that work with a dot matrix printer but need to ³
      *    ³ be amended to work with the Windows spooler.              ³
      *    ³ The date and page numbers which appear at the begining &  ³
      *    ³ end of the line are printed with a 17cpi font, while the  ³
      *    ³ headings between these two fields are printed with an ex- ³
      *    ³ panded font.                                              ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           03  w100-print-line               redefines w100-print-detail.
               05  w100-page.
      *          PAGE:
                   07  w100-pgh    pic  x(06).
      *          ZZZ9
                   07  w100-pgn    pic  x(04).
               05  w100-hdg        pic  x(105).
               05  w100-date.
      *          DATE:
                   07  w100-dth    pic  x(06).
      *         Z9/99/9999
                   07  w100-dte    pic  x(10).
           03  w100-print-heading.
               05  filler          pic  x(03).
      *
      *  Company/Business name and report heading.
      *
      *  CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
      *
               05  w100-rep-hdg    pic  x(62).
               05  filler          pic  x(03).
      *
      *  Special heading for Accounts Receivable.
      *
           03  w100-headings       pic  x(01).
      *
      *  Heading line has been stored.
      *
           03  W100-STORE-IND      PIC  X(01) VALUE "N".
           03  W100-PRINT-BOLD     PIC  X(01) VALUE "N".
           03  W100-HDG-IND        PIC  X(01) VALUE "N".
           03  W100-FONT-SIZE      PIC  9(04) COMP-5.
           03  W100-EXP-FONT-SIZE  PIC  9(04) COMP-5.

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

       01  LS-SCREEN-PRINT.
      *
      *    COMMAND: C = Close printer
      *             E = End of report
      *             O = Open printer
      *             P = Print a line
      *             S = Skip to new page
      *
           03  LS-COMMAND      PIC  X(01).
      *
      *    PRINTER: D = Disk
      *             S = Screen
      *             1 = Lpt1
      *             2 = Lpt2
      *             3 = Lpt3
      *             4 = Com1
      *             5 = Com2
      *             8 = USB/WINDOWS
      *
           03  LS-PRINTER.
               05  LS-PRINT-NO PIC  9(01).
      *
      *    ADVANCE:   0 = Print - No advance
      *             1-3 = Print advance lines specified
      *             4-? = Print advance 1 line, then use a loop
      *                   to advance the remaining lines.
      *              99 = Print and advance to a new page.
      *
           03  LS-ADVANCE      PIC  9(02).
      *
      *    REPORT: 1 = Picking Slips
      *            2 = Invoices/Credit Notes
      *            3 = General Reports (Stock)
      *            4 = General Reports (Creditors)
      *            5 = General Reports (Debtors)
      *            6 = General Reports (G/Ledger)
      *            7 = Statements
      *            8 = Labels
      *            9 = Quotations
      *
           03  LS-REPORT           PIC  9(01).
           03  LS-SCREEN           PIC  9(01).
           03  LS-SCREEN-LIN       PIC  9(02).
           03  LS-LINAGE           PIC  9(02).
           03  LS-PAPER            PIC  X(01).
           03  LS-ORIENTATION      PIC  X(01).

       01  L02-PRINTER-DETAILS.
           03  L02-PRINTER         PIC  X(12).
           03  L02-PGE-LENGTH      PIC  9(03).
           03  L02-PRN-LENGTH      PIC  9(03).
           03  L02-LINAGE          PIC  9(03).
           03  L02-PRN-STATUS      PIC  X(01).
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
           03  L02-PRN-TYPE        PIC  X(01).
           03  L02-PRN-LINE        PIC  X(136).
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
      *
      *       *    **** **** *****    ***  *** ****  **** 
      *       *    *    *      *     *      *  *   * *    
      *       *    ***  ***    *      ***   *  *   * ***  
      *       *    *    *      *         *  *  *   * *    
      *       **** **** *      *      ***  *** ****  **** 
      *
      *            ***  ****   ****   ***   ***  **** 
      *           *   * *      *   * *   * *   * *    
      *           *   * ***    ****  ***** *     ***  
      *           *   * *      *     *   * *  ** *    
      *            ***  *      *     *   *  ***  **** 
      *
       01  SD-LEFT.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE  5 COLUMN  2 PIC  X(78) FROM W05-SLA(1).
               05  LINE  6 COLUMN  2 PIC  X(78) FROM W05-SLA(2).
               05  LINE  7 COLUMN  2 PIC  X(78) FROM W05-SLA(3).
               05  LINE  8 COLUMN  2 PIC  X(78) FROM W05-SLA(4).
               05  LINE  9 COLUMN  2 PIC  X(78) FROM W05-SLA(5).
               05  LINE 10 COLUMN  2 PIC  X(78) FROM W05-SLA(6).
               05  LINE 11 COLUMN  2 PIC  X(78) FROM W05-SLA(7).
               05  LINE 12 COLUMN  2 PIC  X(78) FROM W05-SLA(8).
               05  LINE 13 COLUMN  2 PIC  X(78) FROM W05-SLA(9).
               05  LINE 14 COLUMN  2 PIC  X(78) FROM W05-SLA(10).
               05  LINE 15 COLUMN  2 PIC  X(78) FROM W05-SLA(11).
               05  LINE 16 COLUMN  2 PIC  X(78) FROM W05-SLA(12).
               05  LINE 17 COLUMN  2 PIC  X(78) FROM W05-SLA(13).
               05  LINE 18 COLUMN  2 PIC  X(78) FROM W05-SLA(14).
               05  LINE 19 COLUMN  2 PIC  X(78) FROM W05-SLA(15).
               05  LINE 20 COLUMN  2 PIC  X(78) FROM W05-SLA(16).
               05  LINE 21 COLUMN  2 PIC  X(78) FROM W05-SLA(17).
               05  LINE 22 COLUMN  2 PIC  X(78) FROM W05-SLA(18).
               05  LINE 23 COLUMN  2 PIC  X(78) FROM W05-SLA(19).
               05  LINE 24 COLUMN  2 PIC  X(78) FROM W05-SLA(20).
               05  LINE 25 COLUMN  2 PIC  X(78) FROM W05-SLA(21).
               05  LINE 26 COLUMN  2 PIC  X(78) FROM W05-SLA(22).
               05  LINE 27 COLUMN  2 PIC  X(78) FROM W05-SLA(23).
               05  LINE 28 COLUMN  2 PIC  X(78) FROM W05-SLA(24).
               05  LINE 29 COLUMN  2 PIC  X(78) FROM W05-SLA(25).
               05  LINE 30 COLUMN  2 PIC  X(78) FROM W05-SLA(26).
               05  LINE 31 COLUMN  2 PIC  X(78) FROM W05-SLA(27).
               05  LINE 32 COLUMN  2 PIC  X(78) FROM W05-SLA(28).
               05  LINE 33 COLUMN  2 PIC  X(78) FROM W05-SLA(29).
               05  LINE 34 COLUMN  2 PIC  X(78) FROM W05-SLA(30).
               05  LINE 35 COLUMN  2 PIC  X(78) FROM W05-SLA(31).
               05  LINE 36 COLUMN  2 PIC  X(78) FROM W05-SLA(32).
               05  LINE 37 COLUMN  2 PIC  X(78) FROM W05-SLA(33).
               05  LINE 38 COLUMN  2 PIC  X(78) FROM W05-SLA(34).
               05  LINE 39 COLUMN  2 PIC  X(78) FROM W05-SLA(35).
               05  LINE 40 COLUMN  2 PIC  X(78) FROM W05-SLA(36).
               05  LINE 41 COLUMN  2 PIC  X(78) FROM W05-SLA(37).
               05  LINE 42 COLUMN  2 PIC  X(78) FROM W05-SLA(38).
               05  LINE 43 COLUMN  2 PIC  X(78) FROM W05-SLA(39).
               05  LINE 44 COLUMN  2 PIC  X(78) FROM W05-SLA(40).
               05  LINE 45 COLUMN  1 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" FOREGROUND-COLOR Cyan BACKGROUND-COLOR Black.
      *
      *     ****  ***  ***  *   * *****    ***  *** ****  ****  
      *     *   *  *  *   * *   *   *     *      *  *   * *     
      *     ****   *  *     *****   *      ***   *  *   * ***   
      *     *   *  *  *  ** *   *   *         *  *  *   * *     
      *     *   * ***  ***  *   *   *      ***  *** ****  ****  
      *     
      *              ***  ****   ****   ***   ***  **** 
      *             *   * *      *   * *   * *   * *    
      *             *   * ***    ****  ***** *     ***  
      *             *   * *      *     *   * *  ** *    
      *              ***  *      *     *   *  ***  **** 
      *
       01  SD-RIGHT.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE  5 COLUMN  2 PIC  X(78) FROM W05-SLB(1).
               05  LINE  6 COLUMN  2 PIC  X(78) FROM W05-SLB(2).
               05  LINE  7 COLUMN  2 PIC  X(78) FROM W05-SLB(3).
               05  LINE  8 COLUMN  2 PIC  X(78) FROM W05-SLB(4).
               05  LINE  9 COLUMN  2 PIC  X(78) FROM W05-SLB(5).
               05  LINE 10 COLUMN  2 PIC  X(78) FROM W05-SLB(6).
               05  LINE 11 COLUMN  2 PIC  X(78) FROM W05-SLB(7).
               05  LINE 12 COLUMN  2 PIC  X(78) FROM W05-SLB(8).
               05  LINE 13 COLUMN  2 PIC  X(78) FROM W05-SLB(9).
               05  LINE 14 COLUMN  2 PIC  X(78) FROM W05-SLB(10).
               05  LINE 15 COLUMN  2 PIC  X(78) FROM W05-SLB(11).
               05  LINE 16 COLUMN  2 PIC  X(78) FROM W05-SLB(12).
               05  LINE 17 COLUMN  2 PIC  X(78) FROM W05-SLB(13).
               05  LINE 18 COLUMN  2 PIC  X(78) FROM W05-SLB(14).
               05  LINE 19 COLUMN  2 PIC  X(78) FROM W05-SLB(15).
               05  LINE 20 COLUMN  2 PIC  X(78) FROM W05-SLB(16).
               05  LINE 21 COLUMN  2 PIC  X(78) FROM W05-SLB(17).
               05  LINE 22 COLUMN  2 PIC  X(78) FROM W05-SLB(18).
               05  LINE 23 COLUMN  2 PIC  X(78) FROM W05-SLB(19).
               05  LINE 24 COLUMN  2 PIC  X(78) FROM W05-SLB(20).
               05  LINE 25 COLUMN  2 PIC  X(78) FROM W05-SLB(21).
               05  LINE 26 COLUMN  2 PIC  X(78) FROM W05-SLB(22).
               05  LINE 27 COLUMN  2 PIC  X(78) FROM W05-SLB(23).
               05  LINE 28 COLUMN  2 PIC  X(78) FROM W05-SLB(24).
               05  LINE 29 COLUMN  2 PIC  X(78) FROM W05-SLB(25).
               05  LINE 30 COLUMN  2 PIC  X(78) FROM W05-SLB(26).
               05  LINE 31 COLUMN  2 PIC  X(78) FROM W05-SLB(27).
               05  LINE 32 COLUMN  2 PIC  X(78) FROM W05-SLB(28).
               05  LINE 33 COLUMN  2 PIC  X(78) FROM W05-SLB(29).
               05  LINE 34 COLUMN  2 PIC  X(78) FROM W05-SLB(30).
               05  LINE 35 COLUMN  2 PIC  X(78) FROM W05-SLB(31).
               05  LINE 36 COLUMN  2 PIC  X(78) FROM W05-SLB(32).
               05  LINE 37 COLUMN  2 PIC  X(78) FROM W05-SLB(33).
               05  LINE 38 COLUMN  2 PIC  X(78) FROM W05-SLB(34).
               05  LINE 39 COLUMN  2 PIC  X(78) FROM W05-SLB(35).
               05  LINE 40 COLUMN  2 PIC  X(78) FROM W05-SLB(36).
               05  LINE 41 COLUMN  2 PIC  X(78) FROM W05-SLB(37).
               05  LINE 42 COLUMN  2 PIC  X(78) FROM W05-SLB(38).
               05  LINE 43 COLUMN  2 PIC  X(78) FROM W05-SLB(39).
               05  LINE 44 COLUMN  2 PIC  X(78) FROM W05-SLB(40).
               05  LINE 45 COLUMN  1 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" FOREGROUND-COLOR Cyan BACKGROUND-COLOR Black.
      *
      *   ****   ***  *   * ****   ***  *   *        ***   *** ****   ***  *    *   
      *   *   * *   * **  * *   * *   * ** **       *     *    *   * *   * *    *   
      *   ****  ***** * * * *   * *   * * * *  ***   ***  *    ****  *   * *    *   
      *   *   * *   * *  ** *   * *   * *   *           * *    *   * *   * *    *   
      *   *   * *   * *   * ****   ***  *   *        ***   *** *   *  ***  **** ****
      *     
      *      *    **** **** *****     ***  ****     ****  ***  ***  *   * *****  
      *      *    *    *      *      *   * *   *    *   *  *  *   * *   *   *    
      *      *    ***  ***    *      *   * ****     ****   *  *     *****   *    
      *      *    *    *      *      *   * *   *    *   *  *  *  ** *   *   *    
      *      **** **** *      *       ***  *   *    *   * ***  ***  *   *   *    
      *
       01  SD-RANDOM.
           03  BACKGROUND-COLOR Grey FOREGROUND-COLOR Black.
               05  LINE  5 COLUMN  2 PIC  X(78) FROM W06-LINE(1).
               05  LINE  6 COLUMN  2 PIC  X(78) FROM W06-LINE(2).
               05  LINE  7 COLUMN  2 PIC  X(78) FROM W06-LINE(3).
               05  LINE  8 COLUMN  2 PIC  X(78) FROM W06-LINE(4).
               05  LINE  9 COLUMN  2 PIC  X(78) FROM W06-LINE(5).
               05  LINE 10 COLUMN  2 PIC  X(78) FROM W06-LINE(6).
               05  LINE 11 COLUMN  2 PIC  X(78) FROM W06-LINE(7).
               05  LINE 12 COLUMN  2 PIC  X(78) FROM W06-LINE(8).
               05  LINE 13 COLUMN  2 PIC  X(78) FROM W06-LINE(9).
               05  LINE 14 COLUMN  2 PIC  X(78) FROM W06-LINE(10).
               05  LINE 15 COLUMN  2 PIC  X(78) FROM W06-LINE(11).
               05  LINE 16 COLUMN  2 PIC  X(78) FROM W06-LINE(12).
               05  LINE 17 COLUMN  2 PIC  X(78) FROM W06-LINE(13).
               05  LINE 18 COLUMN  2 PIC  X(78) FROM W06-LINE(14).
               05  LINE 19 COLUMN  2 PIC  X(78) FROM W06-LINE(15).
               05  LINE 20 COLUMN  2 PIC  X(78) FROM W06-LINE(16).
               05  LINE 21 COLUMN  2 PIC  X(78) FROM W06-LINE(17).
               05  LINE 22 COLUMN  2 PIC  X(78) FROM W06-LINE(18).
               05  LINE 23 COLUMN  2 PIC  X(78) FROM W06-LINE(19).
               05  LINE 24 COLUMN  2 PIC  X(78) FROM W06-LINE(20).
               05  LINE 25 COLUMN  2 PIC  X(78) FROM W06-LINE(21).
               05  LINE 26 COLUMN  2 PIC  X(78) FROM W06-LINE(22).
               05  LINE 27 COLUMN  2 PIC  X(78) FROM W06-LINE(23).
               05  LINE 28 COLUMN  2 PIC  X(78) FROM W06-LINE(24).
               05  LINE 29 COLUMN  2 PIC  X(78) FROM W06-LINE(25).
               05  LINE 30 COLUMN  2 PIC  X(78) FROM W06-LINE(26).
               05  LINE 31 COLUMN  2 PIC  X(78) FROM W06-LINE(27).
               05  LINE 32 COLUMN  2 PIC  X(78) FROM W06-LINE(28).
               05  LINE 33 COLUMN  2 PIC  X(78) FROM W06-LINE(29).
               05  LINE 34 COLUMN  2 PIC  X(78) FROM W06-LINE(30).
               05  LINE 35 COLUMN  2 PIC  X(78) FROM W06-LINE(31).
               05  LINE 36 COLUMN  2 PIC  X(78) FROM W06-LINE(32).
               05  LINE 37 COLUMN  2 PIC  X(78) FROM W06-LINE(33).
               05  LINE 38 COLUMN  2 PIC  X(78) FROM W06-LINE(34).
               05  LINE 39 COLUMN  2 PIC  X(78) FROM W06-LINE(35).
               05  LINE 40 COLUMN  2 PIC  X(78) FROM W06-LINE(36).
               05  LINE 41 COLUMN  2 PIC  X(78) FROM W06-LINE(37).
               05  LINE 42 COLUMN  2 PIC  X(78) FROM W06-LINE(38).
               05  LINE 43 COLUMN  2 PIC  X(78) FROM W06-LINE(39).
               05  LINE 44 COLUMN  2 PIC  X(78) FROM W06-LINE(40).
               05  LINE 45 COLUMN  1 VALUE "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" FOREGROUND-COLOR Cyan BACKGROUND-COLOR Black.

       01  SD-CRT-CONTROL.
           03  BACKGROUND-COLOR Blue FOREGROUND-COLOR Grey.
               05  LINE 46 COLUMN  2 VALUE "L"                                    FOREGROUND-COLOR Brown HIGHLIGHT. 
               05          COLUMN  3 VALUE   "eft, ".
               05          COLUMN  8 VALUE        "R"                             FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN  9 VALUE         "ight, ".
               05          COLUMN 15 VALUE              X"1B201A"                 FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 18 VALUE                   ",".
               05          COLUMN 19 VALUE                    "N"                 FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 20 VALUE                     "ext, ".
               05          COLUMN 25 VALUE                          "P"           FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 26 VALUE                           "age or ".
               05          COLUMN 33 VALUE                                  "Esc" FOREGROUND-COLOR Brown HIGHLIGHT.
               05          COLUMN 36 VALUE                                     "ape".
      *
      *      ******   ******    *****    *****   ******  ******   **   **  ******    ****** 
      *      **   **  **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **   **  **  **   **   **  **       **      **   **  **   **  **  **    **
      *      ******   *****    **   **  **       *****   **   **  **   **  *****     *****
      *      **       **  **   **   **  **       **      **   **  **   **  **  **    **
      *      **       **   **  **   **  **   **  **      **   **  **   **  **   **   **
      *      **       **   **   *****    *****   ******  ******    *****   **   **   ******
      *
       PROCEDURE DIVISION USING LS-PARID LS-USER-ID DATA-FILE-IDS LS-SCREEN-PRINT L02-PRINTER-DETAILS.
       AA000-MAIN      SECTION.
       AA000-INIT.
           MOVE LS-PARID             TO WS-PARID.
           MOVE LS-SCREEN-PRINT      TO WS-SCREEN-PRINT.

       AA10-OPTIONS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Must the Screen be cleared - Already been opened.     ³
      *    ³ If a USER selects the screen as output WS-COMMAND is set  ³
      *    ³ to 'I' and this test will clear the screen to a white     ³
      *    ³ background and then exit.                                 ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-COMMAND = "I"
               PERFORM OPEN-SCREEN THRU OPEN-SCREEN-EXIT
               MOVE WS-SCREEN-PRINT  TO LS-SCREEN-PRINT
               GO TO AA99.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       Printer/Screen/Disk has not been opened yet.        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF NOT(L02-PRN-STATUS = "O")
               PERFORM SAVE-SCREEN-3.
      *
      *     ****    P R O C E S S   T H E   R E Q U E S T
      *
             PERFORM PRINT-SCREEN.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³          Printer/Screen/Disk has been closed.             ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF L02-PRN-STATUS = "C"
               PERFORM RESTORE-SCREEN-3.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     Move Printer/Screen/Disk controls to linkage area.    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE WS-SCREEN-PRINT  TO LS-SCREEN-PRINT.

       AA99.
             EXIT PROGRAM.

       COPY "AA900.ALM".

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
      *    ³                        OPT-MESSAGE                        ³
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

      /
      *    ****    READ FILES ROUTINES
      *
       AC000              SECTION.

       COPY "PARAM.RD".

       PRINT-SCREEN    SECTION.
       PRINT-STATUS-TEST.
             EVALUATE WS-COMMAND
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³         O P E N   T H E   P R I N T E R   F I L E         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               WHEN "O"   IF L02-PRN-STATUS = "C"
                              PERFORM OPEN-PRINT-FILE
                          END-IF
                          GO TO PRINT-SCREEN-EXIT
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³        C L O S E   T H E   P R I N T E R   F I L E        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               WHEN "C"   IF L02-PRN-STATUS = "O"
                              PERFORM CLOSE-PRINT-FILE
                          END-IF
                          GO TO PRINT-SCREEN-EXIT
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³            S K I P   T O   A   N E W   P A G E            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               WHEN "S"   IF L02-PRN-STATUS = "O"
                              PERFORM NEW-PAGE
                          END-IF
                          GO TO PRINT-SCREEN-EXIT
             END-EVALUATE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³          D A T A   T O   B E   P R O C E S S E D          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE L02-PRN-LINE       TO Rep-detail
       W01-Print-Line-194.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    C H E C K   I F   P R I N T E R   F I L E   O P E N    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF L02-PRN-STATUS = "C"
               MOVE "Printer file not open" TO WS-ERR-MES
               PERFORM ERROR-MESSAGE
               GO TO PRINT-SCREEN-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    C H E C K   I F   P R I N T E R   O R   S C R E E N    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           IF WS-PRINTER = "S"
               PERFORM SCREEN-PRINT
           ELSE
               PERFORM LINE-PRINT-SPOOL.

       PRINT-SCREEN-EXIT.
             EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     O P E N   P R I N T E R   /   S P O O L   F I L E     ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       OPEN-PRINT-FILE SECTION 5.
       OPEN-PRINTER.
           IF L02-LINAGE = 99
               OPEN INPUT PARAM.
             MOVE "P"                TO WS-OPTION.

       OPEN-PRINTER-NO.
             MOVE SPACES          TO WS-OPT-MES.
           IF LS-PRINTER = "S"
               STRING "'P'rinter or 'D'isk  [" DELIMITED SIZE WS-OPTION DELIMITED SIZE "]" DELIMITED SIZE INTO WS-OPT-MES
               PERFORM OPT-MESSAGE TEST AFTER UNTIL WS-OPTION = "P" OR "D"
           ELSE
               STRING "'P'rinter,'S'creen or 'D'isk  [" DELIMITED SIZE WS-OPTION DELIMITED SIZE "]" DELIMITED SIZE INTO WS-OPT-MES
               PERFORM OPT-MESSAGE TEST AFTER UNTIL WS-OPTION = "P" OR "D" OR "S".
             MOVE WS-OPTION          TO WS-PRINTER.
           IF WS-OPTION = "S" OR "D"
               MOVE "Y"           TO WS-SKIP
               GO TO OPEN-PRINTER-EXIT
           END-IF.
           IF LS-USER NUMERIC
               IF LS-USE > ZERO AND < 110
                   MOVE LS-USE       TO WS-USUB
               END-IF
           ELSE
               MOVE 110              TO WS-USUB
           END-IF.
             ADD 2549 WS-USUB        GIVING W25-RESULT.
             MOVE W25-KEY            TO WS-PARKEY.
             ADD 1 W25-SUB           GIVING WS-S6.
             PERFORM READ-PARAM THRU READ-PARAM-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³            REPORT: 1 = Picking Slips                      ³
      *    ³                    2 = Invoices/Credit Notes              ³
      *    ³                    3 = General Reports (Stock)            ³
      *    ³                    4 = General Reports (Creditors)        ³
      *    ³                    5 = General Reports (Debtors)          ³
      *    ³                    6 = General Reports (G/Ledger)         ³
      *    ³                    7 = Statements                         ³
      *    ³                    8 = Labels                             ³
      *    ³                    9 = Quotations                         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             EVALUATE WS-REPORT
               WHEN 1          PERFORM OPEN-REPORT-1
               WHEN 2          PERFORM OPEN-REPORT-2
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ As printing is now using the windows drivers, the system  ³
      *    ³ will now allow for Landscape printing and the orientation ³
      *    ³ setting is now included. The detail line is now allowing  ³
      *    ³ for more characters to be printed and the number of lines ³
      *    ³ is reduced when printing landscape.                       ³
      *    ³ This program will provide the facility for the following  ³
      *    ³ reports. Over the next year the reports will be studied   ³
      *    ³ and this feature will be included where required.         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               WHEN 3 THRU 6   PERFORM OPEN-REPORT-3
      *        WHEN 4
      *        WHEN 5
      *        WHEN 6          PERFORM OPEN-REPORT-3
               WHEN 7          PERFORM OPEN-REPORT-4
               WHEN 8          PERFORM OPEN-REPORT-5
               WHEN 9          PERFORM OPEN-REPORT-6
             END-EVALUATE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The program will initially only handle USB and will later ³
      *    ³ be amended to handle the Dot Matrix Printers as well. The ³
      *    ³ following code is the original Dot Matrix Printer code.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

       OPEN-WIN-USB-PRINTER.
      *
      *                    ***  ****  ***** *   * 
      *                   *   * *   * *     **  * 
      *                   *   * ****  ***   * * * 
      *                   *   * *     *     *  ** 
      *                    ***  *     ***** *   * 
      *
      *           ****  ****  *** *   * ***** ***** ****
      *           *   * *   *  *  **  *   *   *     *   *
      *           ****  ****   *  * * *   *   ***   ****
      *           *     *   *  *  *  **   *   *     *   *
      *           *     *   * *** *   *   *   ***** *   *
      * ______________________________________________________________ *
      * PC_PRINTER_OPEN                                                *
      *                                                                *
      *   Opens a printer channel, giving it a document title, and     *
      *   optionally displaying the dialog boxes for printer control,  *
      *   font selection, and progress indication.                     *
      *                                                                *
      * SYNTAX:                                                        *
      *                                                                *
      *         call "PC_PRINTER_OPEN" using printer-handle            *
      *                                      document-title            *
      *                                by value  flags                 *
      *                                by value  window-handle         *
      *                                returning status-code           *
      *                                                                *
      * PARAMETERS:                                                    *
      *            printer-handle pic x(4) comp-5.                     *
      *            document-title Group item defined as:               *
      *                           title-len pic x(2) comp-5.           *
      *                           title-text pic x.                    *
      *            flags          Numeric literal or pic x(4) comp-5.  *
      *            window-handle  Numeric literal or pic x(4) comp-5.  *
      *            status-code    See Key                              *
      *                                                                *
      * ON ENTRY:                                                      *
      *            title-len  The length of the document title.        *
      *            title-text The title of the document to be printed. *
      *      flags A set of bits to define printer options:            *
      *            bit 0 Display a dialog for defining printer         *
      *                  characteristics (cannot be used with bit 2 or *
      *                  bit 3)                                        *
      *            bit 1 Display a font dialog box, so you can select  *
      *                  the font for the document                     *
      *            bit 2 Print in portrait orientation (cannot be used *
      *                  with bit 0 or bit 3)                          *
      *            bit 3 Print in landscape orientation (cannot be     *
      *                  used with bit 0 or bit 2)                     *
      *            bit 4 Display a progress indicator dialog box       *
      *                                                                *
      *      All remaining bits are reserved for future use and        *
      *      should not be set.                                        *
      *                                                                *
      *   window-handle  Handle of the parent window. The parent       *
      *                     window is used as a reference for          *
      *                     positioning dialog boxes on the screen.    *
      *                     If window-handle is 0 positioning is       *
      *                     system dependent.                          *
      *                                                                *
      * ON EXIT:                                                       *
      *      printer-handle Returns a handle to be used for subsequent *
      *                     printer operations.                        *
      *      status-code Printer status code:                          *
      *                                                                *
      *           0  Successful                                        *
      *           1  Could not open printer device                     *
      *           5  Failed to open file                               *
      *           9  No default printer found. Select a printer        *
      *                 from Windows Control Panel.                    *
      *          24  User pressed Cancel on the printer setup          *
      *                 or the font selection dialog.                  *
      *                                                                *
      * COMMENTS:                                                      *
      *                                                                *
      *      This routine is available in 32-bit environments only.    *
      *                                                                *
      *      You can check the success of the call by examining        *
      *      RETURN-CODE.                                              *
      * ______________________________________________________________ *
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³     REPORT: 1 = Picking Slips                             ³
      *    ³                2 = Invoices/Credit Notes                  ³
      *    ³                3 = General Reports (Stock)                ³
      *    ³                4 = General Reports (Creditors)            ³
      *    ³                5 = General Reports (Debtors)              ³
      *    ³                6 = General Reports (G/Ledger)             ³
      *    ³                7 = Statements                             ³
      *    ³                8 = Labels                                 ³
      *    ³                9 = Quotations                             ³
      *    ³                                                           ³
      *    ³  Create report name using Program name and report type.   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             EVALUATE WS-REPORT
               WHEN 1  move 23 to title-len
                       string LS-PRG-NAME delimited size "- Picking Slips" delimited size x"00" delimited size into title-text
               WHEN 2  move 17 to title-len
                       string LS-PRG-NAME delimited size "- Invoice" delimited size x"00" delimited size into title-text
               WHEN 3  move 22   to title-len
                       string LS-PRG-NAME delimited size "- Stock Report" delimited size x"00" delimited size into title-text
               WHEN 4  move 26   to title-len
                       string LS-PRG-NAME delimited size "- Creditors Report" delimited size x"00" delimited size into title-text
               WHEN 5  move 24   to title-len
                       string LS-PRG-NAME delimited size "- Debtors Report" delimited size x"00" delimited size into title-text
               WHEN 6  move 25   to title-len
                       string LS-PRG-NAME delimited size "- G-Ledger Report" delimited size x"00" delimited size into title-text
               WHEN 7  move 27   to title-len
                       string LS-PRG-NAME delimited size "- Debtor Statements" delimited size x"00" delimited size into title-text
               WHEN 8  move 22   to title-len
                       string LS-PRG-NAME delimited size "- Stock Labels" delimited size x"00" delimited size into title-text
               WHEN 9  move 20   to title-len
                       string LS-PRG-NAME delimited size "- Quotations" delimited size x"00" delimited size into title-text
             END-EVALUATE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³          Allow User to select printer to be used          ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             perform Select-printer.
           if WS-REPORT = 3 OR 4 OR 5 OR 6  
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³               Check orientatiom of Report                 ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               perform Check-printer-orientation.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Setup the printer font so as to adavance the paper for the³
      *    ³ top border.                                               ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             move FontTypeFace       to Font-family-name.
             move 12                 to Font-family-namelen.
             move font-12-CPI        to font-size.
             move 0                  to font-style.
             perform set-printer-font.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³  Setup the page and print lengths as USB/WIN printing has ³
      *    ³  no LINAGE-COUNTER and the length depends on the current  ³
      *    ³  font that is being used.                                 ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             perform Get-Printer-Info.
             move PageRowsCurrentFont to L02-PGE-LENGTH.
             compute L02-PRN-LENGTH = (PageRowsCurrentFont - 4).
             move zero               to L02-LINAGE.
      *
      *    ****  Top of page border
      *
             perform advance-printer 2 times.
      *
      *    ****  Set the printer status to "O"pen
      *
             MOVE SPACES             TO L02-PRN-LINE.
             MOVE "O"                TO L02-PRN-STATUS.
           IF L02-LINAGE = 99
               CLOSE PARAM.
             GO TO OPEN-PRINTER-EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Setup printer description or ID for disk file (printer 9) ³
      *    ³ The system allows for a printer allocation 9 which then   ³
      *    ³ writes the print file to disk. (Used for testing).        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       OPEN-REPORT-1.
           IF TRM-PICP(WS-S6) = 8
               MOVE "USB/WINDOWS"    TO W02-PRINTER
           ELSE
           IF TRM-PICP(WS-S6) = 9
               MOVE LS-USER          TO W02-USER.
             ADD 501 TRM-PRN1(WS-S6) GIVING W25-RESULT.

       OPEN-REPORT-2.
           IF TRM-INVP(WS-S6) = 8
               MOVE "USB/WINDOWS"    TO W02-PRINTER
           ELSE
           IF TRM-INVP(WS-S6) = 9
               MOVE LS-USER          TO W02-USER.
             ADD 501 TRM-PRN2(WS-S6) GIVING W25-RESULT.
       OPEN-REPORT-3.
           IF TRM-STDP(WS-S6) = 8
               MOVE "USB/WINDOWS"    TO W02-PRINTER
           ELSE
           IF TRM-STDP(WS-S6) = 9
               MOVE LS-USER          TO W02-USER.
             ADD 501 TRM-PRN3(WS-S6) GIVING W25-RESULT.

       OPEN-REPORT-4.
           IF TRM-STMP(WS-S6) = 8
               MOVE "USB/WINDOWS"    TO W02-PRINTER
           ELSE
           IF TRM-STMP(WS-S6) = 9
               MOVE LS-USER          TO W02-USER.
             ADD 501 TRM-PRN4(WS-S6) GIVING W25-RESULT.

       OPEN-REPORT-5.
           IF TRM-CSHP(WS-S6) = 8
               MOVE "USB/WINDOWS"    TO W02-PRINTER
           ELSE
           IF TRM-CSHP(WS-S6) = 9
               MOVE LS-USER          TO W02-USER.
             ADD 501 TRM-PRN5(WS-S6) GIVING W25-RESULT.

       OPEN-REPORT-6.
           IF TRM-POS(WS-S6) = 8
               MOVE "USB/WINDOWS"    TO W02-PRINTER
           ELSE
           IF TRM-POS(WS-S6) = 9
               MOVE LS-USER          TO W02-USER.
             ADD 501 TRM-PRN6(WS-S6) GIVING W25-RESULT.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Setup to redirect printing to the console and not to the  ³
      *    ³ printer or spool file.                                    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       OPEN-SCREEN.
             MOVE 1                  TO WS-S8.

       OPEN-SCREEN-LOOP.
             MOVE SPACES             TO W05-LINE(WS-S8).
           IF WS-S8 < 40
               ADD 1                 TO WS-S8
               GO TO OPEN-SCREEN-LOOP.
             MOVE ZERO               TO WS-SCREEN-LIN.
             MOVE 1                  TO WS-SCREEN WS-LEFT.
             MOVE 78                 TO WS-RIGHT.

       OPEN-SCREEN-EXIT.
             EXIT.

       OPEN-PRINTER-EXIT.
             EXIT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³       Allow User to select printer to be used        ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       Select-printer  section 5.
             move 1                  to prn-flags.
             call "PC_PRINTER_OPEN" using by reference prn-handle by reference document-title by value prn-flags by value 0
             end-call.

      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ As printing is now using the windows drivers, the system  ³
      *    ³ will now allow for Landscape printing and the orientation ³
      *    ³ setting is now included. The detail line is now allowing  ³
      *    ³ for more characters to be printed and the number of lines ³
      *    ³ is reduced when printing landscape.                       ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                Check orientatiom of Report                ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       Check-printer-orientation section 5.
           if LS-ORIENTATION = "L"
               call "PC_PRINTER_CLOSE" using by reference prn-handle
               end-call
               move 4      to prn-command
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³             Change orientatiom to Landscape               ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               move 8      to prn-flags
               call "PC_PRINTER_OPEN" using by reference prn-handle by reference document-title by value prn-flags by value 0
               end-call.

       CLOSE-PRINT-FILE  SECTION 6.
             CLOSE-PRINTER.
           IF WS-PRINTER = "S"
               GO TO CLOSE-PRINTER-STATUS
           END-IF.
             MOVE SPACES             TO Rep-detail.
             move font-10-CPI        to font-size
             perform set-printer-font
             perform close-down-printer.

       CLOSE-PRINTER-STATUS.
             MOVE "C"                TO L02-PRN-STATUS.

       CLOSE-PRINTER-EXIT.
             EXIT.
      *
      *    ****   S K I P   T O   A   N E W   P A G E
      *
       NEW-PAGE        SECTION 7.
       SKIP-TO-NEW-PAGE.
             MOVE SPACES             TO Rep-detail.
             perform Throw-a-page
           IF NOT(WS-COMMAND = "E")
               IF (L02-PGE-LENGTH > 66 AND < 88) OR (L02-PGE-LENGTH > 88)
                   perform advance-printer 2 times
               END-IF
           END-IF.

       NEW-PAGE-EXIT.
             EXIT.

       LINE-PRINT-SPOOL  SECTION 8.
       CHECK-PRINT-INFO.
           IF NOT(L02-PRN-TYPE = "D")
               if (w100-headings = "Y") OR (L02-PRN-TYPE = "H") OR (W100-STORE-IND = "Y") OR (W100-HDG = "Y")
                   perform PRINT-HEADING-LINE
                   GO TO LINE-PRINT-EXIT
               END-IF
           ELSE
               MOVE "N"              TO W100-STORE-IND W100-HDG-IND W100-headings
           END-IF.

       PRINT-A-LINE.
           IF WS-ADVANCE = 0
               PERFORM PRINT-ZERO
           ELSE
           IF WS-ADVANCE > 0 AND < 99
               PERFORM PRINT-ADVANCE
           ELSE
           IF WS-ADVANCE > 98
               PERFORM PRINT-PAGE
           ELSE
               MOVE WS-ADVANCE  TO WS-LINES
               PERFORM PRINT-ADVANCE.
             perform Get-Printer-Info
      *
      *    ****   Has number of lines per page changed due to font
      *           size being changed?
      *
           if not(PageRowsCurrentFont = L02-PGE-LENGTH)
               move PageRowsCurrentFont  to L02-PGE-LENGTH
               compute L02-PRN-LENGTH = (PageRowsCurrentFont - 4)
           else
               COMPUTE L02-LINAGE = (L02-prn-length - (RowsRemainingCurrentFont - 2))
           end-if.
             GO TO LINE-PRINT-EXIT.

       PRINT-ZERO.
             EVALUATE L02-PRN-TYPE
      * 
      *                         Set to print ñ 17cpi
      * 
               WHEN "C"    move font-17-CPI  to font-size
                           move 0            to font-style
                           perform set-printer-font
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                Check orientatiom of Report                ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
                           if LS-ORIENTATION = "L"
                               move 194      to Print-Buffer-Len
                           else
                               MOVE 135      to Print-Buffer-Len
                           end-if
               WHEN "E"    PERFORM PRINT-SET-EXPANDED-FONT
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Test current font size and then set contracted to half the³
      *    ³ size. Initially only font sizes 5, 6 and 8.5 will be test-³
      *    ³ ed as these are the fonts used on the dot matrix printers.³
      *    ³ Switch off bold printing. Future enhancements will allow  ³
      *    ³ for additional font sizes to be used on the USB/WINDOWS/  ³
      *    ³ CUPS printers.                                            ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               WHEN "X"    if font-size = font-5-CPI
                               move font-10-CPI  to font-size
                           else
                           if font-size = font-5p5-CPI
                               move font-11-CPI  to font-size
                           else
                           if font-size = font-6-CPI
                               move font-12-CPI  to font-size
      *                    else
      *                    if font-size = font-6p5-CPI
      *                        move font-13-CPI  to font-size
      *                    else
      *                    if font-size = font-7p5-CPI
      *                        move font-15-CPI  to font-size
                           else
                           if font-size = font-8p5-CPI
                               move font-17-CPI  to font-size
                           end-if
                           perform set-printer-font
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                   Set to print ñ 11cpi                    ³
      *    ³ As there is a margin on the USB/WINDOWS printers, 10cpi   ³
      *    ³ can only print 78 characters in portrait and the layouts  ³
      *    ³ use 80 characters the font will be set to 11cpi for these ³
      *    ³ printers, which will allow for 86 characters. The utility ³
      *    ³ will space the lines accordingly.                         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               WHEN "1"    move font-11-CPI  to font-size
                           move 0            to font-style
                           perform set-printer-font
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³               Check orientatiom of Report                 ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
                           if LS-ORIENTATION = "L"
                               move 123      to Print-Buffer-Len
                           else
                               MOVE 86       to Print-Buffer-Len
                           end-if
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                    Set to print ñ 12cpi                   ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               WHEN "2"    move font-12-CPI  to font-size
                           move 0            to font-style
                           perform set-printer-font
                           if LS-ORIENTATION = "L"
                               move 135      to Print-Buffer-Len
                           else
                               MOVE 96       to Print-Buffer-Len
                           end-if
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                   Set to print ñ 17cpi                    ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               WHEN "3"    move font-17-CPI  to font-size
                           move 0            to font-style
                           perform set-printer-font
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³               Check orientatiom of Report                 ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
                           if LS-ORIENTATION = "L"
                               move 194      to Print-Buffer-Len
                           else
                               MOVE 135      to Print-Buffer-Len
                           end-if
             END-EVALUATE.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ The system will ignore the lines per inch requests until  ³
      *    ³ line hight commands have been worked out. This will only  ³
      *    ³ have an effect on the day-end reports that use 8 lines    ³
      *    ³ per inch.                                                 ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      *    IF L02-PRN-TYPE = "6"
      *        MOVE WS-6LPI          TO REP-DETAIL1
      *    ELSE
      *    IF L02-PRN-TYPE = "8"
      *        MOVE WS-8LPI          TO REP-DETAIL1.
           IF WS-PRINTER = "D"
               MOVE ZERO             TO SPL-ADV
               WRITE SPL-L1
           ELSE
               move 0                to ws-advance
               perform PRINT-DETAILS.
           
       PRINT-SINGLE.
           IF WS-PRINTER = "D"
               MOVE 1                TO SPL-ADV
               WRITE SPL-L1
           ELSE
               perform PRINT-DETAILS.

       PRINT-DOUBLE.
             perform Set-bold-print.
           IF WS-PRINTER = "D"
               MOVE 2                TO SPL-ADV
               WRITE SPL-L1
           ELSE
               perform PRINT-DETAILS.

       PRINT-PAGE.
           IF WS-PRINTER = "D"
               MOVE "PAGE"           TO SPL-CONT
               WRITE SPL-L1
               MOVE ZERO             TO SPL-LIN
           ELSE
               perform PRINT-DETAILS.
           IF WS-COMMAND NOT = "E"
               IF (L02-PGE-LENGTH > 66 AND < 88) OR (L02-PGE-LENGTH > 88)
                   MOVE SPACES       TO W01-Print-Line-194 SPL-DETAIL1
           IF WS-PRINTER = "D"
               MOVE 2                TO SPL-ADV
               MOVE Rep-detail       TO SPL-DETAIL1
               WRITE SPL-L1
           ELSE
               MOVE 2                TO WS-ADVANCE
               PERFORM PRINT-DETAILS.

       PRINT-ADVANCE.
           IF WS-PRINTER = "D"
               MOVE 1                TO SPL-ADV
               WRITE SPL-L1
           ELSE
               PERFORM PRINT-DETAILS.
             MOVE SPACES             TO Rep-detail.
             SUBTRACT 1              FROM WS-LINES.
           IF WS-LINES > ZERO
               GO TO PRINT-ADVANCE.

       PRINT-SET-EXPANDED-FONT.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Test current font size and then set expanded to twice the ³
      *    ³ size and set bold print. Initially only font sizes 10, 12 ³
      *    ³ and 17 will be tested as these are the fonts used on the  ³
      *    ³ dot matrix printers. Future enhancements will allow for   ³
      *    ³ additional font sizes to be used on the USB/WINDOWS/CUPS  ³
      *    ³ printers.                                                 ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           if font-size = font-10-CPI
               move font-5-CPI       to font-size
           else
           if font-size = font-11-CPI
               move font-5p5-CPI     to font-size
           else
           if font-size = font-12-CPI
               move font-6-CPI       to font-size
      *    else
      *    if font-size = font-13-CPI
      *     move font-6p5-CPI     to font-size
      *    else
      *    if font-size = font-15-CPI
      *     move font-7p5-CPI     to font-size
           else
           if font-size = font-17-CPI
               move font-8p5-CPI     to font-size
           end-if.
             perform Set-bold-print.



       LINE-PRINT-EXIT.
             EXIT.

       PRINT-HEADING-LINE SECTION 8.
       PRINT-HDG-LINE.
             EVALUATE L02-PRN-TYPE
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³ Is this a HEADER line? If it is then check if it must be  ³
      *    ³ be printed in bold (0 line advance). Store the details    ³
      *    ³ and wait for the next call to determine if the data is the³
      *    ³ same or not. If the data is the same - PRINT BOLD!        ³
      *    ³          Set indicator that data has been stored.         ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
               WHEN "H"  IF W100-HDG-IND = "N"
                             IF WS-ADVANCE > 0
                                 PERFORM PRINT-HEADING-LINE
                                 GO TO PRINT-HDG-LINE-EXIT
                             ELSE
                                 MOVE "Y"  TO W100-HDG-IND
                             END-IF
                         END-IF
                         IF W100-STORE-IND = "Y"
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³    Has this line been received already? (Print bold)      ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
                             IF W100-PRINT-LINE = L02-PRN-LINE
                                 MOVE "Y"  TO W100-PRINT-BOLD
                             ELSE
                                 MOVE "N"  TO W100-PRINT-BOLD
                             END-IF
                             IF w100-headings = "Y"
                                 IF WS-ADVANCE > 0
                                     PERFORM PRINT-SPLIT-HEADING
                                 ELSE
                                     MOVE L02-PRN-LINE TO W100-PRINT-HEADING
                                 END-IF
                             END-IF
                         ELSE
                             MOVE L02-PRN-LINE TO W100-PRINT-DETAIL
                             MOVE "Y"        TO W100-STORE-IND
      *
      *  Check if this is the first line of the Accounts Receivable
      *  heading that needs special processing.
      *
                             if w100-pgh = "PAGE:"
                                 if w100-dth = "DATE:"
                                     if w100-hdg = spaces
                                         move "Y"  to w100-headings
                                     end-if
                                 end-if
                             end-if
                         END-IF
                         GO TO PRINT-HDG-LINE-EXIT

      *
      *
               WHEN "E"  IF w100-headings = "Y"
                             MOVE font-size      to w100-font-size
                             IF W100-FONT-SIZE = font-17-CPI
                                 move font-9-CPI to W100-EXP-FONT-SIZE
                             end-if
                         else
                             PERFORM PRINT-SET-EXPANDED-FONT
                             GO TO PRINT-HDG-LINE-EXIT
                         END-IF
             END-EVALUATE.
             
             GO TO PRINT-HDG-LINE-EXIT.


       PRINT-HDG-LINE-EXIT.
             EXIT.



       PRINT-SPLIT-HEADING SECTION 8.
       PRINT-SPLIT.
             move Print-Buffer-Len   to prn-line-size.
             MOVE w100-page          TO W01-PRINT-LINE-136.
             MOVE 12                 TO Print-Buffer-Len.
             MOVE 0                  TO WS-ADVANCE.
             PERFORM PRINT-DETAILS.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                Save the current font size.                ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             MOVE font-size          TO W100-FONT-SIZE.
           if font-size = font-17-CPI
               move font-10-CPI      to font-size
               PERFORM set-printer-font.
             MOVE W100-rep-hdg       to W01-PRINT-LINE-136.
             move 62                 to Print-Buffer-Len.
             PERFORM PRINT-DETAILS.
             move W100-FONT-SIZE     to font-size.
             PERFORM set-printer-font.
             move w100-date          to W01-PRINT-LINE-136.
             move 18                 to Print-Buffer-Len.
             move 1                  TO WS-ADVANCE.
             PERFORM PRINT-DETAILS.
             move prn-line-size      to Print-Buffer-Len.

       PRINT-SPLIT-EXIT.
             EXIT.

       SCREEN-PRINT    SECTION 9.
       SCREEN-PRINT-INIT.
             MOVE WS-SCREEN-LIN      TO WS-S8.
           IF WS-SCREEN-LIN < 40
               ADD 1                 TO WS-S8
           ELSE
               GO TO SCREEN-PRINT-SCROLL.

       DISPLAY-A-PRINT-LINE.
             MOVE L02-PRN-TYPE       TO W05-TYPE(WS-S8).
             MOVE L02-PRN-LINE       TO W05-DETAIL(WS-S8).
           IF WS-LEFT > 1 AND < 60
               PERFORM SCREEN-SHIFT-DATA THRU SCREEN-SHIFT-LOOP
           ELSE
           IF WS-SCREEN = 1
               DISPLAY SD-LEFT
           ELSE
               DISPLAY SD-RIGHT.
             GO TO SCREEN-PRINT-CHECK.

       SCREEN-SHIFT-LEFT.
           IF WS-LEFT > 1
               SUBTRACT 1            FROM WS-LEFT WS-RIGHT.
             PERFORM SCREEN-SHIFT-DATA THRU SCREEN-SHIFT-LOOP.

       SCREEN-SHIFT-RIGHT.
           IF WS-RIGHT < 136
               ADD 1                 TO WS-LEFT WS-RIGHT.
             PERFORM SCREEN-SHIFT-DATA THRU SCREEN-SHIFT-LOOP.

       SCREEN-SHIFT-DATA.
             MOVE 1                  TO WS-S1 WS-S2.
             MOVE WS-LEFT            TO WS-S3.

       SCREEN-SHIFT-LOOP.
             MOVE W05-CHAR(WS-S1, WS-S3) TO W06-CHAR(WS-S1, WS-S2).
           IF WS-S2 < 78
               ADD 1                 TO WS-S2 WS-S3
               GO TO SCREEN-SHIFT-LOOP.
           IF WS-S1 < 40
               ADD 1                 TO WS-S1
               MOVE 1                TO WS-S2
               MOVE WS-LEFT          TO WS-S3
               GO TO SCREEN-SHIFT-LOOP.
             DISPLAY SD-RANDOM.

       SCREEN-PRINT-SCROLL.
             MOVE 1                  TO WS-S7.
             MOVE 2                  TO WS-S8.

       SCREEN-PRINT-LOOP.
             MOVE W05-LINE(WS-S8)    TO W05-LINE(WS-S7).
           IF WS-S8 < 40
               ADD 1                 TO WS-S7 WS-S8
               GO TO SCREEN-PRINT-LOOP.
             GO TO DISPLAY-A-PRINT-LINE.

       SCREEN-PRINT-CHECK.
             MOVE WS-S8              TO WS-SCREEN-LIN.
             MOVE SPACES             TO L02-PRN-LINE.
           IF WS-COMMAND = "E"
               GO TO SCREEN-CONTROL.
           IF (WS-S8 = 40) AND (LS-ADVANCE = WS-ADVANCE)
               PERFORM SCREEN-CONTROL THRU SCREEN-CONTROL-EXIT
               IF (WS-COMMAND = "Z") OR (WS-ADVANCE = 1)
                   GO TO SCREEN-PRINT-EXIT.
           IF WS-ADVANCE > 1
               SUBTRACT 1            FROM WS-ADVANCE
               GO TO SCREEN-PRINT-INIT.
           IF WS-SCREEN-LIN < 40
               GO TO SCREEN-PRINT-EXIT.

       SCREEN-CONTROL.
             DISPLAY SD-CRT-CONTROL.

       SCREEN-CONTROL-OPT.
             CALL X"AF" USING GET-SINGLE-CHAR, KEY-STATUS.
           IF USER-FUNC
               EVALUATE KEY-CODE-1
                 WHEN ESC-KEY    MOVE "Z"  TO WS-COMMAND
                                 GO TO SCREEN-CONTROL-EXIT
                 WHEN PAGE-DOWN  GO TO SCREEN-CLEAR
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO SCREEN-CONTROL-OPT
               END-EVALUATE
           ELSE
           IF DATA-8BIT
               MOVE KEY-CODE-1X  TO WS-OPTION
           ELSE
           IF ADIS-FUNC
               EVALUATE KEY-CODE-1
                 WHEN LEFT-KEY   PERFORM SCREEN-SHIFT-LEFT
                                 GO TO SCREEN-CONTROL-OPT
                 WHEN RIGHT-KEY  PERFORM SCREEN-SHIFT-RIGHT
                                 GO TO SCREEN-CONTROL-OPT
                 WHEN DOWN-KEY   GO TO SCREEN-CONTROL-EXIT
                 WHEN OTHER      PERFORM AA900-ALARM
                                 GO TO SCREEN-CONTROL-OPT
               END-EVALUATE
           ELSE
               GO TO SCREEN-CONTROL-OPT.
             CALL "CBL_TOUPPER" USING WS-OPTION BY VALUE WS-LENGTH RETURNING WS-STATUS.
             EVALUATE WS-OPTION
               WHEN "L"   MOVE 1     TO WS-SCREEN WS-LEFT
                          MOVE 78    TO WS-RIGHT
               WHEN "N"   MOVE 46    TO CRT-START CRT-END
                          PERFORM ERASE-SCREEN
                          GO TO SCREEN-CONTROL-EXIT
               WHEN "P"   GO TO SCREEN-CLEAR
               WHEN "R"   MOVE 2     TO WS-SCREEN
                          MOVE 59    TO WS-LEFT
                          MOVE 136   TO WS-RIGHT
               WHEN OTHER PERFORM AA900-ALARM
                          GO TO SCREEN-CONTROL-OPT
             END-EVALUATE.
           IF WS-SCREEN = 1
               DISPLAY SD-LEFT
           ELSE
               DISPLAY SD-RIGHT.
             GO TO SCREEN-CONTROL-OPT.

       SCREEN-CLEAR.
             MOVE 1                  TO WS-S8.

       SCREEN-CLEAR-LOOP.
             MOVE SPACES             TO W05-LINE(WS-S8).
           IF WS-S8 < 40
               ADD 1                 TO WS-S8
               GO TO SCREEN-CLEAR-LOOP.
             MOVE ZERO               TO WS-SCREEN-LIN.

       SCREEN-CONTROL-EXIT.
             EXIT.

       SCREEN-PRINT-EXIT.
             EXIT.
      /
      *    ****    I / O   E R R O R   M E S S A G E S
      *
       ZB000-ERROR      SECTION 10.

       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
          IF WS-F-ERROR = 15
              MOVE WS-PARID         TO WS-FILE
              MOVE WS-PARKEY        TO WS-KEY.

       COPY "DISPERR.PRO".

      * ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * ³ The following routines are used for printing via the WINDOWS ³
      * ³ printing API's and are used for LASER and INK-JET printers   ³
      * ³ that do not allow for embedded control characters.        ³
      * ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       PRINT-DETAILS  SECTION.
             perform print-line.
      * ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * ³  Check if paper must be advanced? (WS-ADVANCE will be > 0)   ³
      * ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
           if WS-ADVANCE > 0
               perform advance-printer WS-ADVANCE times
           else
           if WS-ADVANCE = 99
               perform Throw-a-page.
      * ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * ³           A D V A N C E   P A P E R   1   L I N E            ³
      * ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       advance-printer    section.
             move 4                  to prn-command.
             call "PC_PRINTER_CONTROL" using prn-handle by value prn-command
             end-call.
      * ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      * ³           A D V A N C E   P A P E R   1   P A G E            ³
      * ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       Throw-a-page    section.
             move 2                  to prn-command.
             call "PC_PRINTER_CONTROL" using prn-handle by value prn-command
             end-call.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                  Switch on bold printing                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       Set-bold-print    section.
             move 8                  to font-style.
             perform set-printer-font.
      *    ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
      *    ³                 Switch off bold printing                  ³
      *    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
       Switch-off-bold-print section.
             move 0                  to font-style.
             perform set-printer-font.
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
      *         print-buffer pic x(n).                                 *
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
      *        This routine is available in 32-bit environments only.  *
      *                                                                *
      *        You can check the success of the call by examining      *
      *        RETURN-CODE.                                            *
      * ______________________________________________________________ *
       print-line   section.
             call "PC_PRINTER_WRITE" using prn-handle W01-Print-Line-136 by value prn-line-size
             end-call.
      * ______________________________________________________________ *
      * PC_PRINTER_SET_FONT                                            *
      *                                                                *
      *         Sets the font on the printer.                          *
      *                                                                *
      * SYNTAX:                                                        *
      *                                                                *
      *         call "PC_PRINTER_SET_FONT" using     printer-handle    *
      *               font-family-name                                 *
      *               by value  font-size                              *
      *               by value  font-style                             *
      *               returning status-code                            *
      *                                                                *
      * PARAMETERS:                                                    *
      *         printer-handle pic x(4) comp-5.                        *
      *         font-family-name Group item defined as:                *
      *                          name-len pic x(2) comp-5.             *
      *                          name-text pic x().                    *
      *         font-size Numeric literal or pic x(4) comp-5.          *
      *         font-style Numeric literal or pic x(4) comp-5.         *
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
      *         Sends a print command to a printer.                    *
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
       control-printer    section.
             call "PC_PRINTER_CONTROL" using prn-handle by value prn-command
             end-call.
      * ______________________________________________________________ *
      * PC_PRINTER_SET_COLOR                                           *
      *                                                                *
      *        Sets the color on the printer.                          *
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
      *         printer was opened.                                    *
      *         fore-or-back   Whether to set foreground or background:*
      *               1        foreground                              *
      *               2        background                              *
      *         color-red      Value between 0 and 255; intensity of   *
      *                        red                                     *
      *         color-green    Value between 0 and 255; intensity of   *
      *                        green                                   *
      *         color-blue     Value between 0 and 255; intensity of   *
      *                        blue                                    *
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
      *         Color        Red    Green   Blue                       *
      *         -----        ---    -----   ----                       *
      *         Red          255        0      0                       *
      *         Yellow       255      255      0                       *
      *         Green          0      255      0                       *
      *         Cyan           0      255    255                       *
      *         Blue           0        0    255                       *
      *         Magenta      255        0    255                       *
      *         White        255      255    255                       *
      *         Black          0        0      0                       *
      *                                                                *
      *      Some other, more unusual, colors are:                     *
      *                                                                *
      *         Dark orange  255      140      0                       *
      *         Navy blue      0        0    128                       *
      *         Violet       238      120    238                       *
      *         Beige        245      245    220                       *
      *                                                                *
      * ______________________________________________________________ *
      
       printer-colour   section.
             call "PC_PRINTER_SET_COLOR" using prn-handle by value fore-or-back by value color-red by value color-green by value color-blue
             end-call.
             
      * ______________________________________________________________ *
      * PC_PRINTER_CLOSE                                               *
      *                                                                *
      *            Closes a previously opened printer channel.         *
      *                                                                *
      * SYNTAX:                                                        *
      *                                                                *
      *            call "PC_PRINTER_CLOSE" using printer-handle        *
      *                                    returning status-code       *
      *                                                                *
      * PARAMETERS:                                                    *
      *            printer-handle  pic x(4) comp-5.                    *
      *            status-code See Key                                 *
      *                                                                *
      * ON ENTRY:                                                      *
      *            printer-handle The printer handle returned when the *
      *            printer was opened.                                 *
      *                                                                *
      * ON EXIT:                                                       *
      *            None                                                *
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
      * PC_WIN_SET_PDEFAULT                                            *
      *                                                                *
      * Set the default printer for all PC_WIN printer routines.       *
      *                                                                *
      * SYNTAX:     call "PC_WIN_SET_PDEFAULT" using deflt-printer     *
      *             returning status-code                              *
      *                                                                *
      * PARAMETERS:       deflt-printer  pic x(n).                     *
      *                   status-code    See Key in the Preface        *
      *                                                                *
      * ON ENTRY:   deflt-printer A null-terminated string specifying  *
      *             the default printer. If no name is specified (that *
      *             is, the first character of the string is null)     *
      *             then the default printer is reset.                 *
      *                                                                *
      * ON EXIT:    status-code Printer status code:                   *
      *                                                                *
      *                  0      Successful                             *
      *                 17      Failed to find default printer         *
      *                                                                *
      * EXAMPLE:    call "PC_WIN_SET_PDEFAULT" using                   *
      *                  "Local PostScript Printer" & x"00"            *
      *                   returning printer-retcode                    *
      *             end-call                                           *
      * ______________________________________________________________ *
       set-default-printer   section.

      * ______________________________________________________________ *
      * PC_PRINTER_LOAD_BMP                                            *
      *                                                                *
      * Loads a bitmap into memory ready for the PC_PRINTER_WRITE_BMP  *
      * function.                                                      *
      *                                                                *
      * SYNTAX:                                                        *
      *                                                                *
      *         call "PC_PRINTER_LOAD_BMP" using printer-handle        *
      *                                    by reference bmpfilename    *
      *                                    by reference bmp-id         *
      *                                       returning status-code    *
      *                                                                *
      * PARAMETERS:                                                    *
      *              printer-handle pic x(4) comp-5.                   *
      *              bmpfilename    pic x(n).                          *
      *              bmp-id         pic x(4) comp-5.                   *
      *              status-code See Key                               *
      *                                                                *
      * ON ENTRY:                                                      *
      *          printer-handle The printer handle returned when the   *
      *          printer was opened.                                   *
      *          bmpfilename The name of the file to be printed        *
      *          (null or space terminated).                           *
      *                                                                *
      * ON EXIT:                                                       *
      *             bmp-id  A unique identifier for the loaded         *
      *             bitmap.                                            *
      *             status-code Printer status code:                   *
      *                                                                *
      *                 0       Successful                             *
      *                 3       Printer device not open                *
      *                18       Failed to load bitmap                  *
      *                19       Invalid bitmap id given                *
      *                                                                *
      * EXAMPLE:                                                       *
      *                                                                *
      *         call "PC_PRINTER_LOAD_BMP" using    printer-handle     *
      *                               by reference "mflogo.bmp" & x"0" *
      *                               by reference  bmp-id             *
      *                                  returning  printer-retcode    *
      *         end-call                                               *
      *                                                                *
      * COMMENTS:                                                      *
      *                                                                *
      *         This routine is available in 32-bit environments only. *
      *         See also:                                              *
      *                                                                *
      *                  PC_PRINTER_FREE_BMP                           *
      *                  PC_PRINTER_WRITE_BMP                          *
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
      *                        printer-handle pic x(4) comp-5.         *
      *                        reserved       pic x(4) comp-5 value 0. *
      *                        bmp-id         pic x(4) comp-5.         *
      *                        bmp-row        pic x(4) comp-5.         *
      *                        bmp-col        pic x(4) comp-5.         *
      *                        bmp-width      pic x(4) comp-5.         *
      *                        bmp-height     pic x(4) comp-5.         *
      *                        status-code See Key                     *
      *                                                                *
      * ON ENTRY:                                                      *
      *          printer-handle The printer handle returned when the   *
      *          printer was opened.                                   *
      *          bmp-id The unique identifier of the bitmap to print   *
      *          bmp-row                                               *
      *          bmp-col The position of the bitmap to print.          *
      *          bmp-width                                             *
      *          bmp-height  The size of the bitmap to print.          *
      *                                                                *
      * ON EXIT:                                                       *
      *         status-code  Printer status code:                      *
      *               0      Successful                                *
      *               3      Printer device not open                   *
      *               4      Out of memory while printing              *
      *               5      Disk full while spooling file             *
      *               7      Print job aborted, no file spooled to     *
      *                      Print Manager                             *
      *              11      Write failure                             *
      *              21      Failed to print bitmap                    *
      *                                                                *
      * COMMENTS:                                                      *
      *                                                                *
      *         This routine is available in 32-bit environments only. *
      *                                                                *
      *         This routine works with PostScript and HP PCL printers.*
      *         It is not supported on HP Deskjet or dot matrix        *
      *         printers.                                              *
      * See also:                                                      *
      *                                                                *
      *           PC_PRINTER_FREE_BMP                                  *
      *           PC_PRINTER_LOAD_BMP                                  *
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
      *                printer-handle pic x(4) comp-5.                 *
      *                bmp-id         pic x(4) comp-5.                 *
      *                status-code    See Key                          *
      *                                                                *
      * ON ENTRY:                                                      *
      *          printer-handle The printer handle returned when the   *
      *          printer was opened.                                   *
      *          bmp-id The unique id of the loaded bitmap, returned   *
      *          when the bitmap was loaded.                           *
      *                                                                *
      * ON EXIT:                                                       *
      *          status-code Printer status code:                      *
      *                0     Successful                                *
      *                3     Printer device not open                   *
      *               20     Failed to free bitmap                     *
      *                                                                *
      * EXAMPLE:                                                       *
      *                                                                *
      *         call "PC_PRINTER_FREE_BMP" using printer-handle,       *
      *                                    by value  bmp-idlogo,       *
      *                                    returning printer-retcode   *
      *         end-call                                               *
      *                                                                *
      * COMMENTS:                                                      *
      *                                                                *
      *         This routine is available in 32-bit environments only. *
      *         See also:                                              *
      *                                                                *
      *                  PC_PRINTER_LOAD_BMP                           *
      *                  PC_PRINTER_WRITE_BMP                          *
      * ______________________________________________________________ *
      
             call "PC_PRINTER_FREE_BMP" using prn-handle by value  BmpID

           if Return-Code not = 0
               move "PC_PRINTER_FREE_BMP" to Operation
               perform 9999-Display-Error
           end-if.

      * ______________________________________________________________ *
      * PC_PRINTER_INFO                                                *
      *                                                                *
      * To allow the calling program to know when a page is full, this *
      * procedure has been introduced to replace the LINAGE-COUNTER    *
      * used by the line printers (Dot Matrix).                        *
      *                                                                *
      * SYNTAX:                                                        *
      *                                                                *
      *         call "PC_PRINTER_INFO"                                 *
      *               using by reference Printer-Handle                *
      *                     by reference PrinterInformation            *
      *         end-call                                               *
      * ______________________________________________________________ *


       Get-Printer-Info  Section.

             move length of PrinterInformation to PrinterInformationLength
             set p-PrinterNameBuffer       to address of PrinterNameBuffer
             set p-PrinterTypeBuffer       to address of PrinterTypeBuffer
             set p-PrinterDeviceBuffer     to address of PrinterDeviceBuffer
             set p-PrinterLocationBuffer   to address of PrinterLocationBuffer
             set p-PrinterCommentBuffer    to address of PrinterCommentBuffer
             set p-PrinterPaperSizeBuffer  to address of PrinterPaperSizeBuffer

             call "PC_PRINTER_INFO" using by reference prn-handle by reference PrinterInformation
             end-call

           if Return-Code not = 0
               move "PC_PRINTER_INFO"  to Operation
               perform 9999-Display-Error
           end-if.

       9999-Errors        section.
       9999-Display-Error.

           display Operation "Error: " Return-Code
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
           end-evaluate

           stop run.

      *
      *    ****    I / O   E R R O R   M E S S A G E S
      *   
       ZB000-ERROR      SECTION 9.
      
       COPY "ERRORS.PRO".

       DISPLAY-FILE-NAME.
          IF WS-F-ERROR = 15
              MOVE W02-PARAM         TO WS-FILE
              MOVE WS-PARKEY         TO WS-KEY.

       COPY "DISPERR.PRO".
