      $set LINKCOUNT"612"
       IDENTIFICATION DIVISION.
000020 PROGRAM-ID.     APACPRNT.
000030 AUTHOR.         J W LEMMON (APAC).
000040 DATE-WRITTEN.   MARCH 2009.

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

		   COPYRIGHT NOTICE: COPYRIGHT (C) 2009 - 2010
                                     by James William Lemmon.
                                       (Id No. 4412165050082).

                   All rights reserved.

                   e-mail jwlemmon@gmail.com.

000050 SECURITY.
                   This program is free software; you can redistribute
                   it and/or modify it under the terms of the GNU General
                   Public License as published by the Free Software
                   Foundation; either version 2 of the License, or (at
                   your option) any later version.

                   This program is distributed in the hope that it will
                   be useful, but WITHOUT ANY WARRANTY; without even the
                   implied warranty of MERCHANTABILITY or FITNESS FOR A
                   PARTICULAR PURPOSE.  See the GNU General Public License
                   for more details.

                   You should have received a copy of the GNU General
                   Public License along with this program; if not, write
                   to the Free Software Foundation, Inc., 59 Temple Place
                   - Suite 330, Boston, MA 02111-1307, USA.

000050 ENVIRONMENT DIVISION.
000060 CONFIGURATION SECTION.
000070 SPECIAL-NAMES.
000080                 CONSOLE IS CRT.
000090 INPUT-OUTPUT SECTION.
000100 FILE-CONTROL.

       COPY APACFIDS.SL.

000120 COPY PARAM.SL.

000200     SELECT PRNREP  ASSIGN W02-PRINTER
                          ORGANIZATION LINE SEQUENTIAL.

000200     SELECT PRNSPL  ASSIGN W02-SPOOLER
                          ORGANIZATION LINE SEQUENTIAL.

000220 DATA DIVISION.
000230 FILE SECTION.

       COPY APACFIDS.FDE.

000250 COPY PARAM.FDE.

      /
000330 FD  PRNREP    LABEL RECORD OMITTED
000420               LINAGE IS WS-PGE-LENGTH.
       01  R-SL1.
           03  R-SLA           PIC  X(78).
           03  FILLER          PIC  X(58).
       01  R-SL2.
           03  FILLER          PIC  X(58).
           03  R-SLB           PIC  X(78).
000350 01  REP-LINE1.
000360     03  REP-DETAIL1     PIC  X(136).

000330 FD  PRNSPL.
       01  SPL-L1.
           03  SPL-CONT.
               05  SPL-ADV     PIC  9(02).
               05  SPL-LIN     PIC  9(02).
000360     03  SPL-DETAIL1     PIC  X(136).

      /

       working-storage section.
       77  WS-CHECK        PIC  X(18)    VALUE
                           "aeWlimemnomLalismJ".
003260 77  WS-S1           PIC  9(04)    COMP-5.
003270 77  WS-S2           PIC  9(04)    COMP-5.
003280 77  WS-S3           PIC  9(04)    COMP-5.
003290 77  WS-S4           PIC  9(04)    COMP-5.
003300 77  WS-S5           PIC  9(04)    COMP-5.
003310 77  WS-S6           PIC  9(04)    COMP-5.
003320 77  WS-S7           PIC  9(04)    COMP-5.
003330 77  WS-S8           PIC  9(04)    COMP-5.
003330 77  WS-LEFT         PIC  9(04)    COMP-5.
003330 77  WS-RIGHT        PIC  9(04)    COMP-5.
001670 77  WS-NETKEY       PIC  9(06)    COMP-5.
001660 77  WS-PARKEY       PIC  9(06)    COMP-5.
001820 77  WS-OPTION       PIC  X(01).
       77  WS-FONT	   PIC	X(01).
001820 77  WS-SINGLE       PIC  X(01).
       77  WS-TEST	   PIC	X(01).
       88  TEST-STATEMENT		 VALUE "S".
       88  TEST-INVOICE 		 VALUE "I".
       77  WS-ERROR        PIC  9(01)    VALUE ZERO.
       77  WS-LINES        PIC  9(02)    VALUE ZERO.
002610 77  WS-LINE	   PIC	9(04)	 COMP-5.
       77  WS-DET-LINES    PIC	9(04)	 COMP-5 VALUE 31.
002620 77  WS-PAGE	   PIC	9(02)	 COMP-5 VALUE ZERO.
002570 77  WS-PGE-LENGTH   PIC	9(02)	 VALUE 66.
001870 77  WS-PRN-LENGTH   PIC  9(02)    VALUE 62.
003150 77  WS-PRN-NO	   PIC	X(01)	 COMP-X VALUE 9.
003160 77  WS-PRN-STAT     PIC  X(01)    COMP-X.
       77  TODAY-DDMMYY    PIC  9(08) COMP-5.
       77  WS-USUB         PIC  9(04) COMP-5.

      /
003050 01  WS-DB-LINE.
003080	   03  WS-TOP-LNE2.
003090	       05  WS-TCR  PIC X(80) VALUE "зддддддддддддддддддддддддддд
      - 	 "ддддддддддддддддддддддддддддддддддддддддддддддддддд©".
003100	   03  WS-TP-LINE2 REDEFINES WS-TOP-LNE2.
003110	       05  FILLER      PIC  X(01).
003120	       05  WS-TOP-COMP PIC  X(40).
003130	       05  FILLER      PIC  X(39).
003160	   03  WS-MID-LNE2.
	       05  FILLER      PIC  X(01) VALUE "Ё".
	       05  WS-BLNK78   PIC  X(78) VALUE ALL "╟".
	       05  FILLER      PIC  X(01) VALUE "Ё".

       COPY FUNCTION.WS.

      *  зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *  Ё  PRINTER ROUTINES FOR USB & WINDOWS PRINTERS THAT DO NOT  Ё
      *  Ё  USE STANDARD RAW CODE FOR FORMATING 		     Ё
      *  юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      ******************************************************************
      * KEY							       *
      * 							       *
      * Descriptions for all of the call-by-name routines appear       *
      * alphabetically. Each description contains the routine name and *
      * function followed by these sections (as appropriate):	       *
      * 							       *
      * SYNTAX:     Shows the CALL statement you could use to call     *
      * 	    the	routine.				       *
      * 							       *
      * 	    The optional RETURNING clause is also shown. Every *
      * 	    routine returns a value showing the result of the  *
      * 	    operation. Unless otherwise indicated, zero        *
      * 	    indicates success, nonzero indicates failure. This *
      * 	    value is left in the data item specified in the    *
      * 	    RETURNING clause, in this reference, status-code.  *
      * 	    If this clause is omitted, the value is left in    *
      * 	    the special register RETURN-CODE. (If call-        *
      * 	    convention bit two is set, RETURN-CODE is not      *
      * 	    changed.)					       *
      * 							       *
      * 	    status-code must be a numeric data item capable of *
      * 			holding positive values from 0 to      *
      * 			65535; for example, PIC X(2) COMP-5.   *
      * 							       *
      * 	    The name of the routine must be coded in upper     *
      * 	    case.					       *
      * 							       *
      * PARAMETERS: Describes any parameters shown in the RETURNING    *
      * 	    and USING clause. A parameter enclosed in brackets,*
      * 	    for example, [parameter1] is optional and might    *
      * 	    not be needed for all forms of the routine.        *
      * 							       *
      * ON ENTRY:   Indicates which of the parameters shown are passed *
      * 	    on entry.					       *
      * 							       *
      * ON EXIT:    Indicates which of the parameters shown are        *
      * 	    returned on exit.				       *
      * 							       *
      * 	    Where bits of one or more bytes are referenced,    *
      * 	    bit 0 is the least significant (rightmost) bit.    *
      * 							       *
      * COMMENTS:   Provides any additional information necessary for  *
      * 	    the successful use of the routine.		       *
      *								       *
      *    PC_PRINT_FILE	Print a file			       *
      *    PC_PRINTER_CLOSE	Close a printer channel 	       *
      *    PC_PRINTER_CONTROL	Send a printer command to a printer    *
      *    PC_PRINTER_FREE_BMP	Free bitmap from memory 	       *
      *    PC_PRINTER_INFO	Get printer information 	       *
      *    PC_PRINTER_LOAD_BMP	Load bitmap into memory 	       *
      *    PC_PRINTER_OPEN	Open a printer channel		       *
      *    PC_PRINTER_SET_COLOR	Set printer color		       *
      *    PC_PRINTER_SET_FONT	Set printer font		       *
      *    PC_PRINTER_WRITE	Write text to a printer 	       *
      *    PC_PRINTER_WRITE_BMP	Write bitmap to a printer	       *
      * 							       *
      *================================================================*
      * 							       *
      *    PC_WIN_CLOSE_PRINTER	   Close a printer channel	       *
      *    PC_WIN_FREE_PBMP	   Free bitmap from memory	       *
      *    PC_WIN_LOAD_PBMP	   Load bitmap into memory	       *
      *    PC_WIN_OPEN_PRINTER	   Open a printer channel	       *
      *    PC_WIN_OPEN_PRINTER_EXT Extended open a printer channel     *
      *    PC_WIN_PRINT_FILE	   Print a file 		       *
      *    PC_WIN_PRINT_FILE_EXT   Extended print a file	       *
      *    PC_WIN_PRINTER_CONTROL  Send a printer control	       *
      *    PC_WIN_PRINTER_INFO	   Get printer information	       *
      *    PC_WIN_SET_PCOLOR	   Set printer color		       *
      *    PC_WIN_SET_PDEFAULT	   Set or reset default printer        *
      *    PC_WIN_SET_PFONT	   Set printer font		       *
      *    PC_WIN_WRITE_PBMP	   Write bitmap to a printer	       *
      *    PC_WIN_WRITE_PRINTER	   Write to a printer		       *
      * 							       *
      * ______________________________________________________________ *

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	      Parameters for PC_PRINTER_DEFAULT_NAME	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       01  Default-Printer-Flags           pic x(04) comp-5  Value 1.
       01  Default-Printer-Name.
	   03  Default-Printer-Name-Len    pic x(02) comp-5.
	   03  Default-Printer-Name-Text   pic x(80).

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	      Parameters for PC_PRINTER_DEFAULT_FONT	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       01  Default-Font-Family-Name.
	   03  Default-Font-Name-Len	   pic x(02) comp-5.
	   03  Default-Font-Name-Text	   pic x(80).
       01  Default-Font-Size               pic x(04) comp-5.
       01  Default-Font-Style              pic x(04) comp-5.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	    Parameters for PC_PRINTER_DEFAULT_PROPERTIES       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       01  Default-Properties-Flags      pic x(4) comp-5.
       01  Default-Properties.
	   03 pr-len	       pic x(2) comp-5.
	   03 pr-papersize     pic s9(4) comp-5.
	   03 pr-paperlength   pic s9(4) comp-5.
	   03 pr-paperwidth    pic s9(4) comp-5.
	   03 pr-scale	       pic s9(4) comp-5.
	   03 pr-copies        pic s9(4) comp-5.
	   03 pr-papertray     pic s9(4) comp-5.
	   03 pr-printquality  pic s9(4) comp-5.
	   03 pr-color	       pic s9(4) comp-5.
	   03 pr-duplex        pic s9(4) comp-5.
	   03 pr-orientation   pic s9(4) comp-5.
	   03 pr-yresolution   pic s9(4) comp-5.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  Parameters for PC_PRINTER_OPEN	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       01  Printer-Handle     pic x(04) comp-5.
       01  Document-Title.
	   03  Title-Len      pic x(02) comp-5.
	   03  Title-Text     pic x(80).
       01  Printer-Flags      pic x(04) comp-5.
       01  Window-Handle      pic x(04) comp-5.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  Parameters for PC_PRINTER_WRITE	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

      * ---Printer-Handle (same as above)
       01  Print-Buffer       pic x(136).
       01  Print-Buffer-Len   pic x(04) comp-5.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       Parameters for PC_PRINTER_LOAD_BMP	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      * ---Printer-Handle (same as above)
       01  BmpFilename	      pic x(12) value "TRP-LH.bmp" & x"00".
       01  BmpID	      pic x(04) comp-5.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		Parameters for PC_PRINTER_WRITE_BMP	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      * ---Printer-Handle (same as above)
      * ---BmpID	  (same as above)
       01  WriteBmpReserved   pic x(04) comp-5 value 0.
       01  BmpRow	      pic x(04) comp-5 value 1.
       01  BmpCol	      pic x(04) comp-5 value 1.
       01  BmpWidth	      pic x(04) comp-5 value 92.
       01  BmpHeigth	      pic x(04) comp-5 value 12.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		Parameters for PC_PRINTER_WRITE_BMP	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      * ---Printer-Handle (same as above)
      * ---BmpID	  (same as above)

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		Parameters for PC_PRINTER_SET_COLOR	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

      * ---Printer-Handle (same as above)
       01  fore-or-back       pic x(2) comp-5.
       01  color-red	      pic x(2) comp-5.
       01  color-green	      pic x(2) comp-5.
       01  color-blue	      pic x(2) comp-5.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		 Parameters for PC_PRINTER_CONTROL	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

      * ---Printer-Handle (same as above)
      * ---print-command:  1 - Abort printing and close printer
      * 		   2 - Throw a page
      * 		   3 - Flush the print buffers
      * 		   4 - Start a new line
      *

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  Parameters for PC_PRINTER_CLOSE	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      * ---Printer-Handle (same as above)
      *

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		    Parameters for PC_PRINT_FILE	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       01  Filename.
	   03  Filename-Len		       pic xx comp-5.
	   03  Filename-Text		       pic x(80).
       01  DocumentTitle.
	   03  DocumentTitle-Len	       pic xx comp-5.
	   03  DocumentTitle-Text	       pic x(15).
      * --- Printer-Flags (same as above)
      * --- Window-Handle (Same as above)

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  Parameters for PC_PRINTER_INFO	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       78 PrinterNameBufferBegin	value next.
       01 PrinterNameBuffer         pic x(255).
       78 PrinterNameBufferLength       value next
          - PrinterNameBufferBegin.
       78 PrinterTypeBufferBegin        value next.
       01 PrinterTypeBuffer         pic x(255).
       78 PrinterTypeBufferLength       value next
          - PrinterTypeBufferBegin.
       78 PrinterDeviceBufferBegin      value next.
       01 PrinterDeviceBuffer       pic x(255).
       78 PrinterDeviceBufferLength     value next
          - PrinterDeviceBufferBegin.
       78 PrinterLocationBufferBegin    value next.
       01 PrinterLocationBuffer     pic x(255).
       78 PrinterLocationBufferLength   value next
          - PrinterLocationBufferBegin.
       78 PrinterCommentBufferBegin     value next.
       01 PrinterCommentBuffer      pic x(255).
       78 PrinterCommentBufferLength    value next
          - PrinterCommentBufferBegin.
       78 PrinterPaperSizeBufferBegin   value next.
       01 PrinterPaperSizeBuffer    pic x(255).
       78 PrinterPaperSizeBufferLength  value next
          - PrinterPaperSizeBufferBegin.

       01  PrinterInformation.
	   03  PrinterInformationLength  pic xxxx comp-5.
	   03  DeviceContextHandle	 pic xxxx comp-5.
	   03  reserved 		 pic xxxx comp-5.
	   03  Orientation		 pic xxxx comp-5.
	   03  PageRowsCurrentFont	 pic xxxx comp-5.
	   03  PageColsCurrentFont	 pic xxxx comp-5.
	   03  RowsRemainingCurrentFont  pic xxxx comp-5.
	   03  MaxGraphicsHorizontalAxis pic xxxx comp-5.
	   03  MaxGraphicsVerticalAxis	 pic xxxx comp-5.
	   03  MinGraphicsHorizontalAxis pic xxxx comp-5.
	   03  MinGraphicsVerticalAxis	 pic xxxx comp-5.
      *
      *    -------  Horizontal
      *
	   03  CurrentCooridinateX	 pic xxxx comp-5.
      *
      *    -------  Vertical
      *
	   03  CurrentCooridinateY	 pic xxxx comp-5.
	   03  NumberOfCopies		 pic xx   comp-5.
      *
      *    -------  Draft, Low,Medium, High, Default or DPI
      *
	   03  PrintQuality		 pic xx   comp-5.
      *
      *    -------  Monochrome or Color
      *
	   03  Color			 pic x	  comp-5.
	   03  reserved 		 pic x	  comp-5.
	   03  DriverVersionNumber	 pic xx   comp-5.
	   03  PrinterName.
	       05  PrinterNameLength	     pic xxxx comp-5
                   value PrinterNameBufferLength.
	       05  p-PrinterNameBuffer			   pointer.
	   03  PrinterType.
	       05  PrinterTypeLength	     pic xxxx comp-5
                   value PrinterTypeBufferLength.
	       05  p-PrinterTypeBuffer			   pointer.
	   03  PrinterDevice.
	       05 PrinterDeviceLength	    pic xxxx comp-5
                  value PrinterDeviceBufferLength.
	       05 p-PrinterDeviceBuffer 		  pointer.
	   03  PrinterLocation.
	       05 PrinterLocationLength     pic xxxx comp-5
                  value PrinterLocationBufferLength.
	       05 p-PrinterLocationBuffer		  pointer.
	   03  PrinterComment.
	       05 PrinterCommentLength	    pic xxxx comp-5
                  value PrinterCommentBufferLength.
	       05 p-PrinterCommentBuffer		  pointer.
	   03  PrinterPaperSize.
	       05 PrinterPaperSizeLength    pic xxxx comp-5
                 value PrinterPaperSizeBufferLength.
	       05 p-PrinterPaperSizeBuffer		  pointer.
      *>

      *>
       01  Operation                       pic x(30).
      *>

       01   printer-control.

	   03  Default-Printer-Name.
	       05  Default-Printer-Name-Len  pic x(02) comp-5.
	       05  Default-Printer-Name-Text pic x(80).

	   03  Font-family.
	       05  Font-family-namelen	pic x(2) comp-5 value 80.
	       05  Font-family-name	pic x(80).

	   03  FontTypeFace    pic x(20) value spaces.
	   03  FontTypeFaceC   pic x(13) value "Courier New" & x"00".
	   03  FontTypeFaceL   pic x(18)
			       value "Letter Gothic Std" & x"00".
	   03  font-style      pic x(4) comp-5 value 0.
	   03  font-size       pic x(4) comp-5 value 0.
	   03  font-size-8     pic x(4) comp-5 value 8.
	   03  font-size-9     pic x(4) comp-5 value 9.
	   03  font-size-10    pic x(4) comp-5 value 10.
	   03  font-size-11    pic x(4) comp-5 value 11.
	   03  font-size-12    pic x(4) comp-5 value 12.
	   03  font-size-13    pic x(4) comp-5 value 13.
	   03  font-size-14    pic x(4) comp-5 value 14.
	   03  font-size-15    pic x(4) comp-5 value 15.
	   03  font-size-16    pic x(4) comp-5 value 16.
	   03  font-size-17    pic x(4) comp-5 value 17.
	   03  font-size-18    pic x(4) comp-5 value 18.
	   03  abort	       pic x(4) comp-5 value 1.
           03  prn-control     pic x(4) comp-5 value 2.
	   03  prn-flags       pic x(4) comp-5 value 1.
           03  prn-command     pic x(4) comp-5 value 4.
	   03  prn-handle      pic x(4) comp-5.
	   03  prn-line-size   pic x(4) comp-5 value 0.
	   03  prn-line-94     pic x(4) comp-5 value 94.
           03  prn-line-136    pic x(4) comp-5 value 136.
      *
      *    -------    The buffer used for printing
      * 	      Formatted data is moved to this buffer for
      * 	      printing. This buffer may be redefined to
      * 	      represent different buffer sizes (Font size)
      *
       01  W01-Print-Detail.
      *
      *    -------    Font size 7
      *
	   03  W01-Print-Line-136.
	     05  W01-Detail-136.
      *
      *    -------    Font size 10
      *
	       07  W01-Print-Line-94.
		 09  W01-Detail-94 pic	x(94).
	       07  filler	   pic	x(42).

      *
      *    -------    test data layout
      *
       01  W01-PRINT-RECORD.

	   03  INV-NAME        PIC  X(40).
	   03  INV-DADD        REDEFINES INV-NAME.
	     05  INV-VATH.
	       07  INV-PC1     PIC  X(04).
	       07  FILLER      PIC  X(03).
	     05  INV-VATNO.
	       07  FILLER      PIC  X(33).
	   03  FILLER	       PIC  X(06).
	   03  INV-H3	       PIC  X(08).
	   03  INV-DADD2.
	     05  INV-PC2       PIC  X(30).
	   03  FILLER	       PIC  X(04).
	   03  INV-H4	       PIC  X(19).
	   03  INV-ORD.
	     05  INV-TERMS.
	       07  FILLER      PIC  X(05).
	       07  INV-AC.
		 09  FILLER    PIC  X(03).
		 09  INV-PGE   PIC  ZZ9.
	     05  FILLER        PIC  X(01).
	   03  FILLER	       PIC  X(01).
	   03  FILLER	       PIC  X(12).
       01  W01-PRINT-ITEM      REDEFINES W01-PRINT-RECORD.

	   03  INV-H5.
      *			       (20)
	     05  INV-ITM-MES.
	       07  FILLER      PIC  X(01).
001780	       07  INV-EXT-ITM.
		 09  INV-ITM   PIC  X(14).
		 09  INV-SL    PIC  X(01).
		 09  INV-EXT   PIC  X(03).
	       07  FILLER      PIC  X(01).

	   03  INV-H6.
      *			       (32)
	     05  FILLER        PIC  X(01).
	     05  INV-DESC.
	       07  FILLER      PIC  X(25).
	       07  INV-H6A     PIC  X(05).
	     05  FILLER        PIC  X(01).

	   03  INV-H7.
      *			       (13)
	     05  FILLER        PIC  X(01).
	     05  INV-QNT       PIC  Z(06)9.99-.
	     05  FILLER        PIC  X(01).

	   03  INV-H8.
      *			       (13)
	     05  FILLER        PIC  X(01).
	     05  INV-PRICE     PIC  Z(06)9.99-.
	     05  FILLER        PIC  X(01).

	   03  INV-H9.
      * 		       (10)
	     05  FILLER        PIC  X(02).
	     05  INV-DSC       PIC  Z9.99.
	     05  FILLER        PIC  X(03).

	   03  INV-H10.
      *			       (15)
	     05  FILLER        PIC  X(01).
	     05  INV-EXTEND    PIC  Z(08)9.99-.
	     05  FILLER        PIC  X(01).

	   03  INV-H11.
      *			       (13)
	     05  FILLER        PIC  X(01).
	     05  INV-TAX       PIC  Z(06)9.99- BLANK WHEN ZERO.
	     05  FILLER        PIC  X(01).

	   03  INV-H12.
      *			       (16)
	     05  FILLER        PIC  X(01).
	     05  INV-VAL       PIC  Z(09)9.99-.
	     05  FILLER        PIC  X(01).
      *
       01  INV-L7 REDEFINES W01-PRINT-RECORD.
	   05  FILLER	       PIC  X(08).
	   05  INV-REMKS.
	       07  FILLER      PIC  X(10).
	       07  INV-RMKS.
		 09  FILLER    PIC  X(40).
	       07  FILLER      PIC  X(13).
	   05  FILLER	       PIC  X(04).
	   05  INV-H13	       PIC  X(12).
	   05  INV-LINE        PIC  X(45).
      *
       01  INV-L8 REDEFINES W01-PRINT-RECORD.
	   05  INV-H14A	       PIC  X(08).
	   05  INV-DTE	       PIC  Z9/99/9999.
	   05  FILLER	       PIC  X(01).
	   05  INV-INVOICE.
	       07  FILLER      PIC  X(02).
	       07  INV-CRNOTE.
		   09  FILLER  PIC  X(06).
		   09  INV-COPY
			       PIC  X(12).
		   09  FILLER  PIC  X(06).
	       07  FILLER      PIC  X(03).
	   05  INV-H14	       PIC  X(08).
	   05  INV-REF	       PIC  X(10).

002140 01  STM-LINE.
002150	   03  STM-DET	       PIC X(96).

002140 01  ST7-LINE1 REDEFINES STM-LINE.
002150	   03  ST7-DET	       PIC X(96).

002160 01  ST7-LINE2 REDEFINES STM-LINE.
002170	   03  ST7-COMP.
002180	       05  ST7-EXCOMP  PIC X(36).
002190         05  FILLER      PIC X(36).
002200     03  FILLER          PIC X(24).

002160 01  ST7-LINE2A REDEFINES STM-LINE.
002170	   03  FILLER	       PIC  X(24).
002180	   03  ST7-HEADER      PIC  X(17).
002200	   03  FILLER	       PIC  X(55).

002230 01  ST7-LINE3 REDEFINES STM-LINE.
002240	   03  ST7-ADD	       PIC  X(30).
002250	   03  FILLER	       PIC  X(12).
002750	   03  ST7-TEL	       PIC  X(19).
002760	   03  ST7-TNO	       PIC  X(14).
002220	   03  FILLER	       PIC  X(21).

002290 01  ST7-LINE4 REDEFINES STM-LINE.
002350	   03  ST7-PC1	       PIC  X(08).
	   03  ST7-AH1	       PIC  X(12).
	   03  ST7-AC1	       PIC  X(06).
	   03  FILLER	       PIC  X(17).
	   03  ST7-STMNT.
	       05  ST7-AH2     PIC  X(06).
	       05  ST7-DATE    PIC  Z9/99/9999.
	       05  FILLER      PIC  X(16).
	       05  ST7-AH3     PIC  X(09).
	       05  ST7-PAGE    PIC  ZZZ9.
	   03  ST7-REMIT REDEFINES ST7-STMNT.
	       05  FILLER      PIC  X(13).
	       05  ST7-AH2R    PIC  X(06).
	       05  ST7-DATER   PIC  Z9/99/9999.
	       05  FILLER      PIC  X(16).
	   03  FILLER	       PIC  X(08).

002290 01  ST7-LINE5 REDEFINES STM-LINE.
	   03  FILLER	       PIC  X(04).
002300	   03  ST7-NAME.
002350	       05  ST7-PC2     PIC  X(08).
	       05  FILLER      PIC  X(32).
002310	   03  FILLER	       PIC  X(08).
002320	   03  ST7-MESS        PIC  X(42).
	   03  FILLER	       PIC  X(02).

002340 01  ST7-LINE6 REDEFINES STM-LINE.
	   03  FILLER	       PIC  X(01).
	   03  ST7-HD1.
	     05	 FILLER	       PIC  X(02).
002420	     05	 ST7-TDTE      PIC  Z9/99/9999.
002430	     05	 FILLER	       PIC  X(04).
	     05	 ST7-BFWD.
002440	       07  ST7-TDESC   PIC  X(12).
	       07  FILLER      PIC  X(04).
002470	       07  ST7-TREF    PIC  X(08).
002490	     05	 FILLER	       PIC  X(04).
002500	     05	 ST7-V1.
002510	       07  ST7-TDBT    PIC  Z(08)9.99 BLANK WHEN ZERO.
	     05  FILLER        PIC  X(04).
002520	     05	 ST7-PD	       PIC  X(03).
002500	     05	 ST7-V2.
002510	       07  ST7-TCRD    PIC  Z(08)9.99 BLANK WHEN ZERO.
	     05	ST7-CB	       PIC  X(01).
002520	     05	FILLER	       PIC  X(04).
002530	     05	ST7-V3.
002540	       07 ST7-TBAL     PIC  Z(08)9.99-.
002550	     05	FILLER	       PIC  X(02).

       01  ST7-LINE6-CLR REDEFINES STM-LINE.
	   03  FILLER	       PIC  X(01).
	   03  ST7-CLR-DT.
	       05  FILLER      PIC  X(14).
	   03  ST7-CLR-DSC.
	       05  FILLER      PIC  X(16).
	   03  ST7-CLR-RF.
	       05  FILLER      PIC  X(12).
	   03  ST7-CLR-DB.
	       05  FILLER      PIC  X(16).
	   03  ST7-CLR-CR.
	       07  FILLER      PIC  X(20).
	   03  ST7-CLR-BL.
	       05  FILLER      PIC  X(17).

002690 01  ST7-LINE7 REDEFINES STM-LINE.
	   03  FILLER	       PIC  X(15).
002700	   03  ST7-A1	       PIC  X(12).
002710	   03  ST7-A2	       PIC  X(10).
002720	   03  ST7-A3	       PIC  X(12).
002720	   03  ST7-A4	       PIC  X(12).
002720	   03  ST7-A5	       PIC  X(12).
002730	   03  FILLER	       PIC  X(23).

002600 01  ST7-LINE8 REDEFINES STM-LINE.
002610	   03  FILLER	       PIC  X(13).
	   03  ST7-INT	       PIC  Z(06)9.99-.
002610     03  FILLER          PIC  X(01).
002620	   03  ST7-3MTHS       PIC  Z(06)9.99-.
002610     03  FILLER          PIC  X(01).
002620	   03  ST7-2MTHS       PIC  Z(06)9.99-.
002630     03  FILLER          PIC  X(01).
002640	   03  ST7-1MTH        PIC  Z(06)9.99-.
002650     03  FILLER          PIC  X(01).
002660	   03  ST7-CUR	       PIC  Z(06)9.99-.
002670     03  FILLER          PIC  X(01).
002680	   03  ST7-DUE	       PIC  Z(06)9.99-.
002670	   03  FILLER	       PIC  X(01).
002680	   03  ST7-DUE2        PIC  Z(06)9.99-.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё The following fields contain test data for printing of an Ё
      *    Ё Invoice - Stock details, Customer details, Calculation    Ё
      *    Ё fields etc. These working storage fields all have prefix  Ё
      *    Ё W01-.						       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
       01  W01-ITEM-CODE.
	   03  FILLER	       PIC  X(14) VALUE "EXT-ITEM-CODE-".
	   03  W01-SEQ	       PIC  9(03) VALUE 1.

       01  W01-DESCRIPTIONS.
	   03  FILLER	       PIC  X(30) VALUE
			       "BRIDGESTONE - BATTLAX BT016 CO".
	   03  FILLER	       PIC  X(30) VALUE
			       "THOR QUADRANT BOOT - RED SZ8".
	   03  FILLER	       PIC  X(30) VALUE
			       "THOR GOGGLES ALLY WRAP - RED".
	   03  FILLER	       PIC  X(30) VALUE
			       "THOR QUADRANT HELMET LACED B/R".
	   03  FILLER	       PIC  X(30) VALUE
			       "THOR MOTOCROSS STATIC ELBOW GR".
	   03  FILLER	       PIC  X(30) VALUE
			       "THOR FORCE ROOST GUARD ILLUSIO".

       01  W01-DESCRIPT	       REDEFINES W01-DESCRIPTIONS.
	   03  W01-STK-DESC    PIC  X(30) OCCURS 6.

       01  W01-PRICES.
	   03  FILLER	       PIC  9(07)V99 COMP-3 VALUE 2450.00.
	   03  FILLER	       PIC  9(07)V99 COMP-3 VALUE 1177.50.
	   03  FILLER	       PIC  9(07)V99 COMP-3 VALUE 510.00.
	   03  FILLER	       PIC  9(07)V99 COMP-3 VALUE 1256.00.
	   03  FILLER	       PIC  9(07)V99 COMP-3 VALUE 211.95.
	   03  FILLER	       PIC  9(07)V99 COMP-3 VALUE 1495.00.

       01  W01-PRICE	       REDEFINES W01-PRICES.
	   03  W01-STK-PRC     PIC  9(07)V99 COMP-3 OCCURS 6.

       01  W01-CLIENT.
	   03  W01-NAME        PIC  X(40)
			       VALUE "THE MOTORCYCLE VENUE - RIVONIA".
	   03  W01-ADD1        PIC  X(30)
			       VALUE "PO BOX 2600".
	   03  W01-ADD2        PIC  X(30)
			       VALUE "RIVONIA".
	   03  W01-ADD3        PIC  X(30)
			       VALUE "SANDTON".
	   03  W01-ADD4        PIC  X(30)
			       VALUE "2046".
	   03  W01-PC1	       PIC  X(08) VALUE SPACES.
	   03  W01-PADD1       PIC  X(30)
			       VALUE "RIVONIA MOTOR VILLAGE".
	   03  W01-PADD2       PIC  X(30)
			       VALUE "236 RIVONIA BLVRD".
	   03  W01-PADD3       PIC  X(30)
			       VALUE "RIVONIA".
	   03  W01-PADD4       PIC  X(30)
			       VALUE "SANDTON".
	   03  W01-PC2	       PIC  X(08) VALUE "2091".
	   03  w01-ACNO	       PIC  X(06) VALUE "MCV001".
	   03  W01-ORD	       PIC  X(12) VALUE "M-032010-215".
	   03  W01-TAX	       PIC  X(14) VALUE "4931255653".

       01  W01-CALCS.
	   03  W01-VAT	       PIC  9(07)V99 COMP-3.
	   03  W01-INCLUSIVE   PIC  9(07)V99 COMP-3.
	   03  W01-T-VAT       PIC  9(07)V99 COMP-3.
	   03  W01-T-EXCL      PIC  9(07)V99 COMP-3.
	   03  W01-T-INCL      PIC  9(07)V99 COMP-3.

       01  W01-TRANSACTIONS.
	   03  FILLER	       PIC  9(08)    VALUE 01022010.
	   03  FILLER	       PIC  X(12)    VALUE "Invoice".
	   03  FILLER	       PIC  X(08)    VALUE "INV-0001".
	   03  FILLER	       PIC S9(07)V99 VALUE 457.98.
	   03  FILLER	       PIC S9(07)V99 VALUE ZERO.
	   03  FILLER	       PIC S9(07)V99 VALUE 457.98.
	   03  FILLER	       PIC  9(08)    VALUE 11032010.
	   03  FILLER	       PIC  X(12)    VALUE "Invoice".
	   03  FILLER	       PIC  X(08)    VALUE "INV-0002".
	   03  FILLER	       PIC S9(07)V99 VALUE 500.00.
	   03  FILLER	       PIC S9(07)V99 VALUE 200.00.
	   03  FILLER	       PIC S9(07)V99 VALUE 757.98.
	   03  FILLER	       PIC  9(08)    VALUE 14032010.
	   03  FILLER	       PIC  X(12)    VALUE "Credit note".
	   03  FILLER	       PIC  X(08)    VALUE "C/N-0001".
	   03  FILLER	       PIC S9(07)V99 VALUE ZERO.
	   03  FILLER	       PIC S9(07)V99 VALUE -200.00.
	   03  FILLER	       PIC S9(07)V99 VALUE 757.98.

       01  W01-TRANS REDEFINES W01-TRANSACTIONS.
	   03  W01-TRAN OCCURS 3.
	       05  W01-TDTE    PIC  9(08).
	       05  W01-TDESC   PIC  X(12).
	       05  W01-TREF    PIC  X(08).
	       05  W01-TDB     PIC S9(07)V99.
	       05  W01-TCR     PIC S9(07)V99.
	       05  W01-TBAL    PIC S9(07)V99.

       01  w01-balances.
	   03  w01-int	       PIC S9(07)V99 VALUE 0.
	   03  w01-cur	       PIC S9(07)V99 VALUE 0.
	   03  w01-30	       PIC S9(07)V99 VALUE 300.00.
	   03  w01-60	       PIC S9(07)V99 VALUE 457.98.
	   03  w01-90	       PIC S9(07)V99 VALUE 0.
	   03  w01-120	       PIC S9(07)V99 VALUE 0.
	   03  w01-bal	       PIC S9(07)V99 VALUE 757.98.

       COPY WS.WS.

000290 01  WS-PARID.
000020     03  WS-SYS-ID       PIC  X(03).

       01  W02-FILE-IDS.

       COPY APACFIDS.ID.

       COPY PARAM.ID.

           03  W02-PRINTER.
               05  W02-PRN     PIC  X(06).

           03  W02-SPOOLER.
               05  W02-WS      PIC  X(02).
               05  W02-US      PIC  X(01).
               05  W02-TERM    PIC  X(03).
               05  W02-US2     PIC  X(01).
               05  W02-REP     PIC  X(01).
               05  W02-P       PIC  X(01).
               05  W02-USER    PIC  X(03).

       COPY W12.WS.

       COPY W20.WS.

001570 01  W25-CALCS.
001580	   03  W25-RESULT      PIC  9(09)V99.
001590     03  W25-RESULT1 REDEFINES W25-RESULT.
001600	       05  W25-DAYS    PIC  9(03).
001610	       05  W25-WHOLE   PIC  9(06).
001620	       05  W25-DECIMAL PIC  9(02).
	   03  W25-RESULT2 REDEFINES W25-RESULT.
	       05  FILLER      PIC  9(02).
	       05  W25-KEY     PIC  9(04).
	       05  W25-SUB     PIC  9(01).
	       05  FILLER      PIC  9(02).
001630	   03  W25-PASS.
001640	       05  W25-PASS1   PIC  9(03).
001650	       05  W25-PASS2   PIC  9(03).
001660	   03  W25-TIME        PIC  9(08).
001670     03  W25-TRED REDEFINES  W25-TIME.
001680	       05  FILLER      PIC  9(02).
001690	       05  W25-FACT    PIC  9(06).
	       05  W25-DATE REDEFINES W25-FACT.
		   07  W25-YY  PIC  9(02).
		   07  W25-MM  PIC  9(02).
		   07  W25-DD  PIC  9(02).
	   03  W25-CUR-CC      PIC  9(02) COMP-5.
	   03  W25-PRV-CC      PIC  9(02) COMP-5.
	   03  W25-NXT-CC      PIC  9(02) COMP-5.

       COPY W40.WS.

006860 01  W95-STM.
006870	   03  W95-COMP        PIC  X(40) VALUE
	       "TRP DISTRIBUTORS T/A Promotion Products".
006880	   03  W95-ADD1        PIC  X(30) VALUE
	       "Kya Sands Business Park".
006890	   03  W95-ADD2        PIC  X(30) VALUE
	       "530 Granite Road, Kya Sands.".
006900	   03  W95-ADD3        PIC  X(30) VALUE
	       "PO Box 505, Lonehill,".
006910	   03  W95-ADD4        PIC  X(30) VALUE
	       "Johannesburg.".
006920     03  W95-PC.
006930	       05  W95-POST    PIC  9(04) VALUE 2062.
006940	   03  W95-TEL	       PIC  X(14) VALUE "+2711 465-1336".
	   03  W95-FAX	       PIC  X(14) VALUE "+2711 708-3689".

       LINKAGE SECTION.

       COPY CHAIN.LS.

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
      *             1 = Default Printer
      *             2 = Allow user to select a printer
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
           03  LS-REPORT       PIC  9(01).
           03  LS-SCREEN       PIC  9(01).
           03  LS-SCREEN-LIN   PIC  9(02).
           03  LS-LINAGE       PIC  9(02).
           03  LS-PAPER        PIC  X(01).
           03  FILLER          PIC  X(01).

       01  L02-PRINTER-DETAILS.
           03  L02-PRINTER     PIC  X(12).
002570     03  L02-PGE-LENGTH  PIC  9(02).
001870     03  L02-PRN-LENGTH  PIC  9(02).
           03  L02-LINAGE      PIC  9(02).
           03  L02-PRN-STATUS  PIC  X(01).
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
           03  L02-PRN-TYPE    PIC  X(01).
           03  L02-PRN-LINE    PIC  X(136).

007190 SCREEN SECTION.

       COPY BLANK.CRT.

       COPY ERROR.CRT.

       COPY S99.CRT.

       COPY OPT.CRT.

       procedure division.
005070 AA000-MAIN	       SECTION.
005080 AA00.
	     PERFORM ZA000-INIT.
	     MOVE ZERO		 TO W01-VAT    W01-INCLUSIVE W01-T-VAT
				    W01-T-EXCL W01-T-INCL.

       AA05.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		     Setup the printer font		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move FontTypeFace	 to Font-family-name.
	     move 10		 to font-size.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		     Print normal characters		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move 0		 to font-style.
	     perform set-printer-font.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		     Print header Bitmap		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     perform print-header-BMP.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё Advance printer location to past the Bitmap to be printed Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     perform control-printer 13 times.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  Set to print Я 5cpi and Bold		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move 24		 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   This heading is printed at 5cpi and centered        Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   IF TEST-STATEMENT
	       MOVE "     S T A T E M E N T / S T A A T"
				 TO W01-Print-Line-136
	   ELSE
	       MOVE "     TAX INVOICE/BELASTING FAKTUUR"
				 TO W01-Print-Line-136.
	     MOVE 44		 TO prn-line-size.
	     perform print-line.
	     perform control-printer.
	   IF TEST-STATEMENT
	       PERFORM STATEMENT-TEST
	       STOP RUN.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  Set to print Я 8 cpi and Bold		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move 14		 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		 COPY + Reference no and then date	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE SPACES	 TO W01-PRINT-RECORD.
	     MOVE "DATE  :"	 TO INV-H14A.
	     MOVE TODAY-DDMMYY	 TO INV-DTE.
	     MOVE "REF No:"	 TO INV-H14.
026590	     MOVE "TST-123456"	 TO INV-REF.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     MOVE 66		 to prn-line-size.
	     perform print-line.
	     perform control-printer.
	     MOVE SPACES	 TO W01-PRINT-RECORD.
	     MOVE "COPY/AFSKRIF" TO INV-COPY.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     perform print-line.
	     perform control-printer.
	     MOVE SPACES	 TO W01-PRINT-RECORD.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      Set to print Я 15cpi		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move 8		 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   Clear the print buffer area		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE SPACES	 TO W01-PRINT-RECORD.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     MOVE 120		 to prn-line-size.
	     perform control-printer.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё  Name, Postal & Delivery Address, Page Number, Account    Ё
      *    Ё  number, Order Number and VAT number.		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE "DELIVERY / AFLEWRING"
				 TO INV-DADD2.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     perform print-line.
	     perform control-printer.
	     MOVE SPACES	 TO W01-PRINT-RECORD.
026330	     MOVE W01-NAME	 TO INV-NAME.
	     MOVE "PAGE / BLADSY"
				 TO INV-H4.
	     ADD 1		 TO WS-PAGE.
	     MOVE WS-PAGE	 TO INV-PGE.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     perform print-line.
	     perform control-printer.
	     MOVE SPACES	 TO W01-PRINT-RECORD.
026350	     MOVE w01-ADD1	 TO INV-NAME.
026360	     MOVE w01-PADD1	 TO INV-DADD2.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     perform print-line.
	     perform control-printer.
026380	     MOVE w01-ADD2	 TO INV-NAME.
026390	     MOVE w01-PADD2	 TO INV-DADD2.
	     MOVE "ACCOUNT / REKENING"
				 TO INV-H4.
026550	     MOVE W01-ACNO	 TO INV-AC.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     perform print-line.
	     perform control-printer.
	     MOVE SPACES	 TO W01-PRINT-RECORD.
026410	     MOVE w01-ADD3	 TO INV-NAME.
026420	     MOVE w01-PADD3	 TO INV-DADD2.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     perform print-line.
	     perform control-printer.
	     MOVE SPACES	 TO W01-PRINT-RECORD.
026440	     MOVE w01-ADD4	 TO INV-NAME.
026450	     MOVE w01-PADD4	 TO INV-DADD2.
	     MOVE "ORDER / BESTELLING"
				 TO INV-H4.
026560	     MOVE W01-ORD	 TO INV-ORD.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     perform print-line.
	     perform control-printer.
	     MOVE SPACES	 TO W01-PRINT-RECORD.
026510	     MOVE w01-PC1	 TO INV-PC1.
026510	     MOVE w01-PC2	 TO INV-PC2.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     perform print-line.
	     perform control-printer.
	     MOVE SPACES	 TO W01-PRINT-RECORD.
      *
      *    ****    C L I E N T	 V A T	 N U M B E R
      *
	   IF NOT (W01-TAX = SPACES)
	       MOVE "Vat No"	 TO INV-VATH
	       MOVE W01-TAX	 TO INV-VATNO.
      *
      *    ****    P R I N T   T E R M S   O N   T H E   I N V O I C E
      *
	     MOVE "TERMS / TERME"
				 TO INV-H4.
028730	     MOVE "7 Days/Dae"	 TO INV-TERMS.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     perform print-line.
	     perform control-printer.
	     MOVE SPACES	 TO W01-PRINT-RECORD.
	     MOVE "Assist :"	 TO INV-H3.
	     MOVE "Mathew"	 TO INV-DADD2.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     perform print-line.
	     perform control-printer 2 times.
	     MOVE SPACES	 TO W01-PRINT-RECORD.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      Set to print Я 16cpi		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move 7		 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
      *						      INV-H7 (13)
	     MOVE "   QUANTITY"	 TO INV-H7.
      *						      INV-H9 (10)
	     MOVE " DISCOUNT"	 TO INV-H9.
      *						      INV-H10 (15)
	     MOVE "   EXCLUSIVE" TO INV-H10.
      *						      INV-H12 (16)
	     MOVE "   NETT AMOUNT"
				 TO INV-H12.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     MOVE 132		 to prn-line-size.
	     perform print-line.
	     perform control-printer.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      Headings for line items		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE SPACES	 TO W01-PRINT-RECORD.
      *						      INV-H5  (20)
	     MOVE "   ITEM CODE/KODE"
				 TO INV-H5.
      *						      INV-H6  (32)
	     MOVE "     DESCRIPTION/BESKRYWING"
				 TO INV-H6.
      *						      INV-H7  (13)
	     MOVE " HOEVEELHEID" TO INV-H7.
      *						      INV-H8  (13)
	     MOVE "  PRICE/PRYS" TO INV-H8.
      *						      INV-H9  (10)
	     MOVE "  AFSLAG"	 TO INV-H9.
      *						      INV-H10 (15)
	     MOVE "   EKSKLUSIEF"
				 TO INV-H10.
      *						      INV-H11 (13)
	     MOVE "   VAT/BTW"	 TO INV-H11.
      *						      INV-H12 (16)
	     MOVE "   NETO BEDRAG"
				 TO INV-H12.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     MOVE 132		 to prn-line-size.
	     perform print-line.
	     perform control-printer.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      Switch off bold printing		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
             move 0              to font-style.
             perform set-printer-font.
	     MOVE SPACES	 TO W01-PRINT-RECORD.
	     MOVE W01-ITEM-CODE	 TO INV-EXT-ITM.
	     MOVE W01-STK-DESC(1)
				 TO INV-DESC.
	     MOVE 1		 TO INV-QNT.
	     MOVE W01-STK-PRC(1) TO INV-PRICE
				    INV-EXTEND.
	     PERFORM calculate-vat.
	     MOVE W01-VAT	 TO INV-TAX.
	     MOVE W01-INCLUSIVE  TO INV-VAL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *	   Ё Background colour is used to show columns, to format the  Ё
      *    Ё document as the ASCII box characters can not be used with Ё
      *    Ё the windows print utility. This helps with readability    Ё
      *    Ё and gives the document a more professional look.	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 2		 TO fore-or-back.
	     MOVE 0		 TO WS-S1.
	   IF WS-FONT = "C"
	       MOVE 50		 TO WS-DET-LINES
	   ELSE
	       MOVE 46		 TO WS-DET-LINES.
	     PERFORM UNTIL WS-S1 = WS-DET-LINES
		 MOVE 245	 TO color-red
		 MOVE 245	 TO color-green
		 MOVE 220	 TO color-blue
		 perform printer-colour
		 MOVE INV-H5	 TO W01-Print-Line-136
		 MOVE 20	 to prn-line-size
		 perform print-line
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  W H I T E   B A C K G R O U N D	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		 MOVE 255	 TO color-red
		 MOVE 255	 TO color-green
		 MOVE 255	 TO color-blue
		 perform printer-colour
		 MOVE INV-H6	 TO W01-Print-Line-136
		 MOVE 32	 to prn-line-size
		 perform print-line
		 MOVE 245	 TO color-red
		 MOVE 245	 TO color-green
		 MOVE 220	 TO color-blue
		 perform printer-colour
		 MOVE INV-H7	 TO W01-Print-Line-136
		 MOVE 13	 to prn-line-size
		 perform print-line
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  W H I T E   B A C K G R O U N D	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		 MOVE 255	 TO color-red
		 MOVE 255	 TO color-green
		 MOVE 255	 TO color-blue
		 perform printer-colour
		 MOVE INV-H8	 TO W01-Print-Line-136
		 MOVE 13	 to prn-line-size
		 perform print-line
		 MOVE 245	 TO color-red
		 MOVE 245	 TO color-green
		 MOVE 220	 TO color-blue
		 perform printer-colour
		 MOVE INV-H9	 TO W01-Print-Line-136
		 MOVE 10	 to prn-line-size
		 perform print-line
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  W H I T E   B A C K G R O U N D	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		 MOVE 255	 TO color-red
		 MOVE 255	 TO color-green
		 MOVE 255	 TO color-blue
		 perform printer-colour
		 MOVE INV-H10	 TO W01-Print-Line-136
		 MOVE 15	 to prn-line-size
		 perform print-line
		 MOVE 245	 TO color-red
		 MOVE 245	 TO color-green
		 MOVE 220	 TO color-blue
		 perform printer-colour
		 MOVE INV-H11	 TO W01-Print-Line-136
		 MOVE 13	 to prn-line-size
		 perform print-line
		 MOVE 245	 TO color-red
		 MOVE 220	 TO color-green
		 MOVE 245	 TO color-blue
		 perform printer-colour
		 MOVE INV-H12	 TO W01-Print-Line-136
		 MOVE 16	 to prn-line-size
		 perform print-line
		 perform control-printer
		 ADD 1		 TO WS-S1 W01-SEQ
		 IF W01-SEQ < 7
		     MOVE W01-ITEM-CODE 	 TO INV-EXT-ITM
		     MOVE W01-STK-DESC(W01-SEQ)  TO INV-DESC
		     MOVE W01-STK-PRC(W01-SEQ)	 TO INV-PRICE INV-EXTEND
		     PERFORM calculate-vat
		     MOVE W01-VAT		 TO INV-TAX
		     MOVE W01-INCLUSIVE 	 TO INV-VAL
		 ELSE
		     MOVE SPACES		 TO W01-PRINT-RECORD
	     END-PERFORM.
	     MOVE SPACES	 TO W01-PRINT-RECORD.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      Set Bold printing on.		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move 8		 to font-style.
	     perform set-printer-font.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  W H I T E   B A C K G R O U N D	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 255		 TO color-red.
	     MOVE 255		 TO color-green.
	     MOVE 255		 TO color-blue.
	     perform printer-colour.
	     MOVE all "_"	 to INV-LINE.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     MOVE 132		 to prn-line-size.
	     perform print-line.
	     perform control-printer 2 times.
	     MOVE SPACES	 TO W01-PRINT-RECORD.
	     MOVE W01-T-EXCL	 TO INV-EXTEND.
	     MOVE W01-T-VAT	 TO INV-TAX.
	     MOVE W01-T-INCL	 TO INV-VAL.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     MOVE 132		 to prn-line-size.
	     perform print-line.
	     perform control-printer.
	     MOVE all "_"	 to INV-LINE.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     MOVE 132		 to prn-line-size.
	     perform print-line.
	     perform control-printer.
	     MOVE SPACES	 TO W01-PRINT-RECORD.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      Set to print Я 15cpi		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move 8		 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
	     MOVE "This is where remarks can be included on an invoice"
				 to INV-REMKS.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     MOVE 120		 to prn-line-size.
	     perform print-line.
	     perform control-printer.
	     MOVE "The remarks are continued on this line and can be"
				 to INV-REMKS.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     perform print-line.
	     perform control-printer.
	     MOVE "used to advertise specials or for greetins etc."
				 to INV-REMKS.
	     MOVE W01-PRINT-RECORD
				 TO W01-Print-Line-136.
	     perform print-line.
	     perform control-printer.
	     perform close-down-printer.
	     stop run.

       calculate-vat	   section.
	     COMPUTE W25-RESULT ROUNDED = W01-STK-PRC(W01-SEQ) * 0.14.
	     COMPUTE W01-T-VAT = W01-T-VAT + W25-RESULT.
	     MOVE W25-RESULT	 TO W01-VAT.
	     COMPUTE W01-INCLUSIVE = W01-STK-PRC(W01-SEQ) + W25-RESULT.
	     COMPUTE W01-T-EXCL = W01-T-EXCL + W01-STK-PRC(W01-SEQ).
	     COMPUTE W01-T-INCL = W01-T-INCL + W01-INCLUSIVE.

      /    *************************************************************
      *    ****    ROUTINES TO HANDLE VARIOUS FUNCTIONS FOR THE
      * 	   S C R E E N ,   K E Y B O A R D   &	 M O U S E
      *    *************************************************************
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   SAVE-SCREEN /-2/-3  and  RESTORE-SCREEN /-2/-3      Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  SCREEN-SHADOW 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To routine is used to display a shadow down the right and Ё
      *    Ё along the bottom of a pop-up box. The parameters that are Ё
      *    Ё required:						       Ё
      *	   Ё	       SHADE-ROW   - Top line of the box + 1.	       Ё
      *	   Ё	       SHADE-COL   - Left line of box + 2.	       Ё
      *	   Ё	       SHADE-WIDTH - Width of the box - 2.	       Ё
      *	   Ё	       SHADE-LINES - Hight of box - 1.		       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  CHECK-CORRECT 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine displays a pop-up window with the message    Ё
      *    Ё	      "Correct ENTER - N if incorrect"		       Ё
      *    Ё							       Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё							       Ё
      *    Ё To start with the pop-up window higher or lower than row  Ё
      *    Ё 13 (default); move another value to SLIN and PERFORM      Ё
      *    Ё CHECK-SETUP THRU CHECK-EXIT.			       Ё
      *    фммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╣
      *    Ё			  ERROR-MESSAGE 		       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё To display an error message with " - Press ANY key" at    Ё
      *    Ё the end; use PERFORM ERROR-MESSAGE.		       Ё
      *    Ё							       Ё
      *    Ё To display just the error message; use PERFORM	       Ё
      *    Ё ERROR-LENGTH THRU ERROR-EXIT.			       Ё
      *    Ё							       Ё
      *    Ё To display the error message higher or lower (default is  Ё
      *    Ё line 13) firstly use PERFORM ERROR-SETUP THRU ERROR-POS   Ё
      *    Ё or PERFORM ERROR-LENGTH THRU ERROR-POS. Move the line     Ё
      *    Ё number to be used to SLIN and then PERFORM ERROR-DISPLAY  Ё
      *    Ё THRU ERROR-EXIT.					       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY FUNCTION.CRT.

      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё			  OPT-MESSAGE			       Ё
      *    цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
      *    Ё This routine is used to allow the OPERATOR to respond to  Ё
      *    Ё a request for an option, so that the correct action can   Ё
      *    Ё be performed by the program. The routine will display the Ё
      *    Ё message in a pop-up window and allow the OPERATOR to      Ё
      *    Ё respond to the 'question'. 			       Ё
      *    Ё							       Ё
      *    Ё The option request must be placed in WS-ERR-MES and may   Ё
      *    Ё not exceed 48 characters. The message will be centred in  Ё
      *    Ё the window. An example of a message request follows:      Ё
      *    Ё							       Ё
      *    Ё   MOVE "Print transactions (Y/N) [ ]" TO WS-ERR-MES.      Ё
      *    Ё   PERFORM OPT-MESSAGE.				       Ё
      *    Ё							       Ё
      *    Ё This would be displayed as:			       Ё
      *    Ё	здддддддддддддддддддддддддддддддддддддддддддддддд©     Ё
      *    Ё	Ё	   Print transactions (Y/N) [ ] 	 Ё╟╟   Ё
      *    Ё	юдддддддддддддддддддддддддддддддддддддддддддддддды╟╟   Ё
      *    Ё	  ╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟╟   Ё
      *    Ё							       Ё
      *    Ё The response is returned in WS-OPTION (in upper case).    Ё
      *    Ё							       Ё
      *    Ё To display the request message higher or lower (default   Ё
      *    Ё is line 13) move the line number to be used to SLIN and   Ё
      *    Ё then PERFORM OPT-SETUP THRU OPT-EXIT.		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

       COPY OPTION.CRT.

      * здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      * Ё The following routines are used for printing via the WINDOWS Ё
      * Ё printing API's and are used for LASER and INK-JET printers   Ё
      * Ё that do not allow for embedded control characters.	       Ё
      * юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	PRINT-DETAILS  SECTION.
	     perform print-line.
      * здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      * Ё  Check if paper must be advanced? (WS-ADVANCE will be > 0)   Ё
      * юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	   if WS-ADVANCE > 0
	       perform advance-printer WS-ADVANCE times
	   else
	   if WS-ADVANCE = 99
	       perform Throw-a-page.
      * здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      * Ё	     A D V A N C E   P A P E R	 1   L I N E	       Ё
      * юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	advance-printer    section.
	     move 4		 to prn-command.
             call "PC_PRINTER_CONTROL" using    prn-handle
                                       by value prn-command
	     end-call.
      * здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      * Ё	     A D V A N C E   P A P E R	 1   P A G E	       Ё
      * юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	Throw-a-page	   section.
	     move 2		 to prn-command.
	     call "PC_PRINTER_CONTROL" using	prn-handle
                                       by value prn-command
	     end-call.

       STATEMENT-TEST  SECTION.

022750	     ADD 1		 TO WS-PAGE.
	     MOVE 1		 TO WS-ADVANCE.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      Set to print Я 12cpi		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move 10		 to font-size.
	     move 0		 to font-style.
	     perform set-printer-font.
022160	     MOVE SPACES	 TO ST7-DET.
022170	     MOVE W01-NAME	 TO ST7-NAME.
023390	     move "This is the statement message area that is"
				 to st7-mess.
	     MOVE 96		 to prn-line-size.
	     move st7-det	 to W01-Print-Line-136.
	     PERFORM PRINT-DETAILS.
022210	     MOVE SPACES	 TO ST7-DET.
022220	     MOVE W01-ADD1	 TO ST7-NAME.
023390	     move "used to communicate either messages of non"
				 to st7-mess.
	     move st7-det	 to W01-Print-Line-136.
	     PERFORM PRINT-DETAILS.
022240	     MOVE SPACES	 TO ST7-DET.
022260	     MOVE W01-ADD2	 TO ST7-NAME.
023390	     move "payment of account or for specials that   "
				 to st7-mess.
	     move st7-det	 to W01-Print-Line-136.
	     PERFORM PRINT-DETAILS.
022480	     MOVE SPACES	 TO ST7-DET.
022490	     MOVE W01-ADD3	 TO ST7-NAME.
023390	     move "are available. It could also be a seasonal"
				 to st7-mess.
	     move st7-det	 to W01-Print-Line-136.
	     PERFORM PRINT-DETAILS.
022580	     MOVE SPACES	 TO ST7-DET.
022590	     MOVE W01-ADD4	 TO ST7-NAME.
	     move st7-det	 to W01-Print-Line-136.
	     PERFORM PRINT-DETAILS.
022690	     MOVE SPACES	 TO ST7-DET.
022720	     MOVE W01-PC1	 TO ST7-PC2.
	     move st7-det	 to W01-Print-Line-136.
	     move 2		 to WS-ADVANCE.
	     PERFORM PRINT-DETAILS.
022690	     MOVE SPACES	 TO ST7-DET.
	     MOVE "ACCOUNT NO:"  TO ST7-AH1.
	     MOVE "DATE:"	 TO ST7-AH2.
	     MOVE "PAGE NO:"	 TO ST7-AH3.
022600	     MOVE W01-ACNO	 TO ST7-AC1.
022190	     MOVE W12-TODAY	 TO ST7-DATE.
022760	     MOVE WS-PAGE	 TO ST7-PAGE.
	     move st7-det	 to W01-Print-Line-136.
	     PERFORM PRINT-DETAILS.
022740	     MOVE SPACES	 TO ST7-DET.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      Switch on bold printing		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move 8		 to font-style.
	     perform set-printer-font.
	     MOVE "    Date       Description     Reference       Debit
      -		  "            Credit           Balance" TO ST7-HD1
	     move st7-det	 to W01-Print-Line-136.
	     move 1		 to WS-ADVANCE.
	     PERFORM PRINT-DETAILS.
022740	     MOVE SPACES	 TO ST7-DET.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      Switch off bold printing		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move 0		 to font-style.
             perform set-printer-font.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *	   Ё Background colour is used to show columns, to format the  Ё
      *    Ё document as the ASCII box characters can not be used with Ё
      *    Ё the windows print utility. This helps with readability    Ё
      *    Ё and gives the document a more professional look.	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 2		 TO fore-or-back.
	     MOVE 1		 TO WS-S1.
	   IF WS-FONT = "C"
	       MOVE 38		 TO WS-DET-LINES
	   ELSE
	       MOVE 34		 TO WS-DET-LINES.
	     PERFORM UNTIL WS-S1 = WS-DET-LINES
		 IF WS-S1 < 4
		     MOVE W01-TDTE(WS-S1)    TO ST7-TDTE
		     MOVE W01-TDESC(WS-S1)   TO ST7-TDESC
		     MOVE W01-TREF(WS-S1)    TO ST7-TREF
		     MOVE W01-TDB(WS-S1)     TO ST7-TDBT
		     MOVE W01-TCR(WS-S1)     TO ST7-TCRD
		     MOVE W01-TBAL(WS-S1)    TO ST7-TBAL
		     IF (W01-TDB(WS-S1) NOT = 0) AND
			(W01-TCR(WS-S1) NOT = 0)
			 MOVE "Cr["  TO ST7-PD
			 MOVE "]"    TO ST7-CB
		     END-IF
		 END-IF
		 MOVE 245	 TO color-red
		 MOVE 245	 TO color-green
		 MOVE 220	 TO color-blue
		 perform printer-colour
		 MOVE ST7-CLR-DT TO W01-Print-Line-136
		 MOVE 14	 to prn-line-size
		 perform print-line
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  W H I T E   B A C K G R O U N D	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		 MOVE 255	 TO color-red
		 MOVE 255	 TO color-green
		 MOVE 255	 TO color-blue
		 perform printer-colour
		 MOVE ST7-CLR-DSC TO W01-Print-Line-136
		 MOVE 16	 to prn-line-size
		 perform print-line
		 MOVE 245	 TO color-red
		 MOVE 245	 TO color-green
		 MOVE 220	 TO color-blue
		 perform printer-colour
		 MOVE ST7-CLR-RF TO W01-Print-Line-136
		 MOVE 12	 to prn-line-size
		 perform print-line
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  W H I T E   B A C K G R O U N D	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
		 MOVE 255	 TO color-red
		 MOVE 255	 TO color-green
		 MOVE 255	 TO color-blue
		 perform printer-colour
		 MOVE ST7-CLR-DB TO W01-Print-Line-136
		 MOVE 16	 to prn-line-size
		 perform print-line
		 MOVE 245	 TO color-red
		 MOVE 245	 TO color-green
		 MOVE 220	 TO color-blue
		 perform printer-colour
		 MOVE ST7-CLR-CR TO W01-Print-Line-136
		 MOVE 20	 to prn-line-size
		 perform print-line
		 MOVE 245	 TO color-red
		 MOVE 220	 TO color-green
		 MOVE 245	 TO color-blue
		 perform printer-colour
		 MOVE ST7-CLR-BL	 TO W01-Print-Line-136
		 MOVE 17	 to prn-line-size
		 perform print-line
		 perform control-printer
		 ADD 1		 TO WS-S1
		 MOVE SPACES	 TO ST7-DET
	     END-PERFORM.
	     MOVE SPACES	 TO ST7-DET.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	      A D V A N C E   P A P E R	  1   L I N E	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     perform advance-printer.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  Set to print Я 8 cpi and Bold		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move 14		 to font-size.
	     move 8		 to font-style.
	     perform set-printer-font.
	     MOVE 66		 to prn-line-size.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		  W H I T E   B A C K G R O U N D	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 255		 TO color-red.
	     MOVE 255		 TO color-green.
	     MOVE 255		 TO color-blue.
	     perform printer-colour.
019250	     MOVE "REMITTANCE ADVICE"
				 TO ST7-HEADER.
	     move st7-det	 to W01-Print-Line-136.
	     PERFORM PRINT-DETAILS.
	     MOVE SPACES	 TO ST7-DET.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      Set to print Я 12cpi		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     move 10		 to font-size.
	     move 0		 to font-style.
	     perform set-printer-font.
	     MOVE 96		 to prn-line-size.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	      A D V A N C E   P A P E R	  1   L I N E	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     perform advance-printer.
021750	     MOVE W95-COMP	 TO ST7-NAME.
022170	     MOVE W01-NAME	 TO ST7-MESS.
	     move st7-det	 to W01-Print-Line-136.
	     PERFORM PRINT-DETAILS.
021840	     MOVE SPACES	 TO ST7-DET.
021850	     MOVE W95-ADD1	 TO ST7-NAME.
022220	     MOVE W01-ADD1	 TO ST7-MESS.
	     move st7-det	 to W01-Print-Line-136.
	     PERFORM PRINT-DETAILS.
021880	     MOVE SPACES	 TO ST7-DET.
021920	     MOVE W95-ADD2	 TO ST7-NAME.
022220	     MOVE W01-ADD2	 TO ST7-MESS.
	     move st7-det	 to W01-Print-Line-136.
	     PERFORM PRINT-DETAILS.
021880	     MOVE SPACES	 TO ST7-DET.
021970	     MOVE W95-ADD3	 TO ST7-NAME.
022220	     MOVE W01-ADD3	 TO ST7-MESS.
	     move st7-det	 to W01-Print-Line-136.
	     PERFORM PRINT-DETAILS.
021880	     MOVE SPACES	 TO ST7-DET.
022020	     MOVE W95-ADD4	 TO ST7-NAME.
022220	     MOVE W01-ADD4	 TO ST7-MESS.
	     move st7-det	 to W01-Print-Line-136.
	     PERFORM PRINT-DETAILS.
021880	     MOVE SPACES	 TO ST7-DET.
022720	     MOVE W95-POST	 TO ST7-PC2.
022720	     MOVE W01-PC1	 TO ST7-MESS.
	     move st7-det	 to W01-Print-Line-136.
	     PERFORM PRINT-DETAILS.
022240	     MOVE SPACES	 TO ST7-DET.
	     MOVE "ACCOUNT NO:"  TO ST7-AH1
	     MOVE "DATE:"	 TO ST7-AH2R.
022600	     MOVE W01-ACNO	 TO ST7-AC1.
022190	     MOVE W12-TODAY	 TO ST7-DATER.
	     move 2		 to WS-ADVANCE.
	     move st7-det	 to W01-Print-Line-136.
	     PERFORM PRINT-DETAILS.
022740	     MOVE SPACES	 TO ST7-DET.
023410	     MOVE "INTEREST"	 TO ST7-A1.
023420	     MOVE "91+ DAYS"	 TO ST7-A2.
023430	     MOVE "61-90 DAYS"	 TO ST7-A3.
023560	     MOVE "31-60 DAYS"	 TO ST7-A4.
023570	     MOVE "01-30 DAYS"	 TO ST7-A5.
	     move 1		 to WS-ADVANCE.
	     move st7-det	 to W01-Print-Line-136.
	     PERFORM PRINT-DETAILS.
023520	     MOVE SPACES	 TO ST7-DET.
023640	     MOVE w01-int	 TO ST7-INT
023470	     MOVE w01-90	 to ST7-3MTHS
023480	     MOVE w01-60	 TO ST7-2MTHS
023490	     MOVE w01-30	 TO ST7-1MTH
023630	     MOVE w01-CUR	 TO ST7-CUR
023360	     MOVE w01-BAL	 TO ST7-TBAL.
	     move 99		 to WS-ADVANCE.
	     move st7-det	 to W01-Print-Line-136.
	     PERFORM PRINT-DETAILS.
023670	     MOVE SPACES	 TO ST7-DET.
	     perform close-down-printer.

      * ______________________________________________________________ *
      * PC_PRINTER_WRITE					       *
      * 							       *
      * Writes a text string to the printer.			       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_WRITE" using	 printer-handle        *
      * 					 print-buffer	       *
      * 			       by value  print-buff-len        *
      * 			       returning status-code	       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	print-buffer	pic x(n).			       *
      * 	print-buff-len	Numeric literal or pic x(4) comp-5.    *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	printer-buffer	The buffer from which the bytes are to *
      * 			be printed			       *
      * 	printer-buff-len  The number of bytes to print	       *
      * 							       *
      * ON EXIT:						       *
      * 							       *
      * 	None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
       print-line	  section.
             call "PC_PRINTER_WRITE" using    prn-handle
					      W01-Print-Line-136
				     by value prn-line-size
             end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_SET_FONT					       *
      * 							       *
      * Sets the font on the printer.				       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_SET_FONT" using     printer-handle    *
      * 					     font-family-name  *
      * 				   by value  font-size	       *
      * 				   by value  font-style        *
      * 			   returning status-code	       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	font-family-name	Group item defined as:	       *
      * 	    name-len	pic x(2) comp-5.		       *
      * 	    name-text	pic x().			       *
      * 	font-size	Numeric literal or pic x(4) comp-5.    *
      * 	font-style	Numeric literal or pic x(4) comp-5.    *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	name-len	The length of the font family name.    *
      * 	name-text	The family name of the font to be used,*
      * 			such as Courier, Times and Symbol      *
      * 	font-size	Point size of the font		       *
      * 	font-style	Set of bits defining the required font *
      * 			style:				       *
      * 	  bit 3	 Bold					       *
      * 	  bit 2	 Strikeout				       *
      * 	  bit 1	 Underline				       *
      * 	  bit 0	 Italic 				       *
      * 							       *
      * 	All remaining bits are reserved for future use and     *
      * 	should not be set.				       *
      * 							       *
      * ON EXIT:						       *
      * 							       *
      * 	None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
	set-printer-font   section.
	     call "PC_PRINTER_SET_FONT"
				     using prn-handle
					   font-family
				     by value font-size
				     by value font-style
             end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_CONTROL					       *
      * 							       *
      * Sends a print command to a printer.			       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_CONTROL" using      printer-handle    *
      * 				  by value   print-command     *
      * 				  returning  status-code       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	print-command	Numeric literal or pic x(4) comp-5.    *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	print-command	The command to send to the printer:    *
      * 							       *
      * 	  1   Abort printing the document and close the        *
      * 	      printer					       *
      * 	  2   Throw a page				       *
      * 	  3   Flush the print buffers			       *
      * 	  4   Advance one line				       *
      * 							       *
      * ON EXIT:						       *
      * 							       *
      *		None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
	control-printer    section.
             call "PC_PRINTER_CONTROL" using    prn-handle
                                       by value prn-command
             end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_SET_COLOR					       *
      * 							       *
      * Sets the color on the printer.				       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_SET_COLOR" using     printer-handle   *
      * 				    by value  fore-or-back     *
      * 				    by value  color-red        *
      * 				    by value  color-green      *
      * 				    by value  color-blue       *
      * 				    returning status-code      *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	fore-or-back	pic x(2) comp-5.		       *
      * 	color-red	pic x(2) comp-5.		       *
      * 	color-green	pic x(2) comp-5.		       *
      * 	color-blue	pic x(2) comp-5.		       *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle The printer handle returned when the    *
      * 		       printer was opened.		       *
      * 	fore-or-back   Whether to set foreground or background:*
      * 	    1	       foreground			       *
      * 	    2	       background			       *
      * 	color-red      Value between 0 and 255; intensity of   *
      * 						red	       *
      * 	color-green    Value between 0 and 255; intensity of   *
      * 						green	       *
      * 	color-blue     Value between 0 and 255; intensity of   *
      * 						blue	       *
      * 							       *
      * ON EXIT:						       *
      * 							       *
      * 	None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You must open the printer before using this routine.   *
      * 							       *
      * 	If your printer does not support color, it is up to    *
      * 	the printer device driver to interpret these values.   *
      * 	For example, some perform gray-shading; others choose  *
      * 	either black or white for each color.		       *
      * 							       *
      * 	You specify the color you want by combinations of      *
      * 	values showing the intensity of the three primary      *
      * 	colors. For example, to get the colors in the	       *
      * 	following table, you set the RGB values as shown:      *
      * 							       *
      * 	      Color	   Red	   Green   Blue 	       *
      * 	      -----	   ---	   -----   ---- 	       *
      * 	      Red	   255	   0	   0		       *
      * 	      Yellow	   255	   255	   0		       *
      * 	      Green	   0	   255	   0		       *
      * 	      Cyan	   0	   255	   255		       *
      * 	      Blue	   0	   0	   255		       *
      * 	      Magenta	   255	   0	   255		       *
      * 	      White	   255	   255	   255		       *
      * 	      Black	   0	   0	   0		       *
      * 							       *
      * 	    Some other, more unusual, colors are:	       *
      * 							       *
      * 	      Dark orange  255	   140	   0		       *
      * 	      Navy blue	   0	   0	   128		       *
      * 	      Violet	   238	   120	   238		       *
      * 	      Beige	   245	   245	   220		       *
      * 							       *
      * ______________________________________________________________ *
	printer-colour	  section.
	     call "PC_PRINTER_SET_COLOR" using	  prn-handle
					 by value fore-or-back
					 by value color-red
					 by value color-green
					 by value color-blue
             end-call.

      * ______________________________________________________________ *
      * PC_PRINTER_CLOSE					       *
      * 							       *
      * Closes a previously opened printer channel.		       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_CLOSE" using printer-handle	       *
      * 				returning status-code	       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 							       *
      * ON EXIT:						       *
      * 	None						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
	close-down-printer section.
	     call "PC_PRINTER_CLOSE" using by reference prn-handle
	     end-call.

      * ______________________________________________________________ *
      * PC_WIN_SET_PDEFAULT					       *
      * 							       *
      * Set the default printer for all PC_WIN printer routines.       *
      * 							       *
      * SYNTAX:     call "PC_WIN_SET_PDEFAULT" using	deflt-printer  *
      * 			   returning status-code	       *
      * 							       *
      * PARAMETERS: deflt-printer  pic x(n).			       *
      * 	    status-code	   See Key in the Preface	       *
      * 							       *
      * ON ENTRY:   deflt-printer A null-terminated string specifying  *
      * 			  the default printer. If no name is   *
      * 			  specified (that is, the first        *
      * 			  character of the string is null)     *
      * 			  then the default printer is reset.   *
      * 							       *
      * ON EXIT:    status-code Printer status code:		       *
      * 							       *
      * 		0	Successful			       *
      * 	       17	Failed to find default printer	       *
      * 							       *
      * EXAMPLE:    call "PC_WIN_SET_PDEFAULT" using		       *
      * 			    "Local PostScript Printer" & x"00" *
      * 			     returning printer-retcode	       *
      * 	    end-call					       *
      * ______________________________________________________________ *


      * ______________________________________________________________ *
      * PC_PRINTER_LOAD_BMP					       *
      * 							       *
      * Loads a bitmap into memory ready for the PC_PRINTER_WRITE_BMP  *
      * function.						       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_LOAD_BMP" using	printer-handle *
      * 				   by reference bmpfilename    *
      * 				   by reference bmp-id	       *
      * 				   returning	status-code    *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	bmpfilename	pic x(n).			       *
      * 	bmp-id		pic x(4) comp-5.		       *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	bmpfilename	The name of the file to be printed     *
      * 			(null or space terminated).	       *
      * 							       *
      * ON EXIT:						       *
      * 	bmp-id		A unique identifier for the loaded     *
      * 			bitmap. 			       *
      * 	status-code	Printer status code:		       *
      * 							       *
      * 	    0		Successful			       *
      * 	    3		Printer device not open 	       *
      * 	   18		Failed to load bitmap		       *
      * 	   19		Invalid bitmap id given 	       *
      * 							       *
      * EXAMPLE:						       *
      * 							       *
      *     call "PC_PRINTER_LOAD_BMP" using	    printer-handle     *
      * 			       by reference "mflogo.bmp" & x"0"*
      * 			       by reference bmp-id	       *
      * 			       returning    printer-retcode    *
      *     end-call						       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 	See also:					       *
      * 							       *
      * 	PC_PRINTER_FREE_BMP				       *
      * 	PC_PRINTER_WRITE_BMP				       *
      * ______________________________________________________________ *
       print-header-BMP    section.

	     call "PC_PRINTER_LOAD_BMP"	using prn-handle
					by reference BmpFilename
					by reference BmpID
	     end-call.

           if Return-Code not = 0
               move "PC_PRINTER_LOAD_BMP" to Operation
               perform 9999-Display-Error
	   end-if.

      * ______________________________________________________________ *
      * PC_PRINTER_WRITE_BMP					       *
      * 							       *
      * Write a loaded bitmap to a printer at a specified position with*
      * a given size.						       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_WRITE_BMP" using     printer-handle   *
      * 				    by value  bmp-id	       *
      * 				    by value  reserved	       *
      * 				    by value  bmp-row	       *
      * 				    by value  bmp-col	       *
      * 				    by value  bmp-width        *
      * 				    by value  bmp-height       *
      * 				    returning status-code      *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	reserved	pic x(4) comp-5 value 0.	       *
      * 	bmp-id		pic x(4) comp-5.		       *
      * 	bmp-row 	pic x(4) comp-5.		       *
      * 	bmp-col 	pic x(4) comp-5.		       *
      * 	bmp-width	pic x(4) comp-5.		       *
      * 	bmp-height	pic x(4) comp-5.		       *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      * 			printer was opened.		       *
      * 	bmp-id	The unique identifier of the bitmap to print   *
      * 	bmp-row 					       *
      * 	bmp-col The position of the bitmap to print.	       *
      * 	bmp-width					       *
      * 	bmp-height  The size of the bitmap to print.	       *
      * 							       *
      * ON EXIT:						       *
      * 	status-code  Printer status code:		       *
      * 	    0	     Successful 			       *
      * 	    3	     Printer device not open		       *
      * 	    4	     Out of memory while printing	       *
      * 	    5	     Disk full while spooling file	       *
      * 	    7	     Print job aborted, no file spooled to     *
      * 		     Print Manager			       *
      * 	   11	     Write failure			       *
      * 	   21	     Failed to print bitmap		       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	This routine works with PostScript and HP PCL printers.*
      * 	It is not supported on HP Deskjet or dot matrix        *
      * 	printers.					       *
      * 	See also:					       *
      * 							       *
      * 	PC_PRINTER_FREE_BMP				       *
      * 	PC_PRINTER_LOAD_BMP				       *
      * 							       *
      * ______________________________________________________________ *
	     call "PC_PRINTER_WRITE_BMP" using prn-handle
					 by value BmpID
					 by value WriteBmpReserved
					 by value BmpRow
					 by value BmpCol
					 by value BmpWidth
					 by value BmpHeigth
	     end-call.

           if Return-Code not = 0
               move "PC_PRINTER_WRITE_BMP" to Operation
               perform 9999-Display-Error
	   end-if.

      * ______________________________________________________________ *
      * PC_PRINTER_FREE_BMP					       *
      * 							       *
      * Frees a bitmap from memory, releasing its image and palette    *
      * back to the operating system.				       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_FREE_BMP" using     printer-handle    *
      * 				   by value  bmp-id	       *
      * 				   returning status-code       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	bmp-id	pic x(4) comp-5.			       *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	printer-handle	The printer handle returned when the   *
      *				printer was opened.		       *
      * 	bmp-id	The unique id of the loaded bitmap, returned   *
      * 		when the bitmap was loaded.		       *
      * 							       *
      * ON EXIT:						       *
      * 	status-code Printer status code:		       *
      * 	    0	    Successful				       *
      * 	    3	    Printer device not open		       *
      * 	   20	    Failed to free bitmap		       *
      * 							       *
      * EXAMPLE:						       *
      * 							       *
      * 	call "PC_PRINTER_FREE_BMP" using printer-handle,       *
      * 				   by value  bmp-idlogo,       *
      * 				   returning printer-retcode   *
      * 	end-call					       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 	See also:					       *
      * 							       *
      * 	PC_PRINTER_LOAD_BMP				       *
      * 	PC_PRINTER_WRITE_BMP				       *
      * ______________________________________________________________ *
	     call "PC_PRINTER_FREE_BMP" using	prn-handle
					by value  BmpID

           if Return-Code not = 0
               move "PC_PRINTER_FREE_BMP" to Operation
               perform 9999-Display-Error
	   end-if.


       9999-Errors	       section.
       9999-Display-Error.

           display Operation "Error: " Return-Code
           evaluate Return-Code
               when 0  display "Success"
               when 1  display "Could not open printer device"
               when 2  display "Invalid printer control code specified"
               when 3  display
                   "No printer device associated with specified handle"
               when 4  display "Out of memory while printing"
               when 5  display "Failed to open file"
               when 6  display "Disk full while spooling file"
               when 7  display
                   "Print job aborted. No job sent to print spooler"
               when 8  display
                   "Printer information structure badly constructed"
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

           stop run
           .

      /    *************************************************************
      *    ****  I N I T I A L I Z E   P R O G R A M
      *    *************************************************************
052700 ZA000-INIT	       SECTION 91.
052710 ZA00.
	     ACCEPT W25-FACT FROM DATE.
	     MOVE W25-YY	 TO W12-YEAR.
	     MOVE W25-MM	 TO W12-MONTH.
	     MOVE W25-DD	 TO W12-DAY.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё T H I S   I F   S T A T E M E N T	 W A S	 U S E D   T O Ё
      *    Ё H A N D L E   T H E   M I L L E N I U M   S W I T C H     Ё
      *    Ё O V E R.						       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
      *	   IF W25-YY < 50
      *	       MOVE 20		 TO W12-CENT
      *	   ELSE
      *	       MOVE 19		 TO W12-CENT.
	     MOVE 20		 TO W12-CENT.
	     MOVE W12-TODAY	 TO TODAY-DDMMYY.
003200	     MOVE W12-DAY	 TO W12-DD.
003210	     MOVE W12-MONTH	 TO W12-MM.
	     MOVE W12-CENT	 TO W12-CC.
003220	     MOVE W12-YEAR	 TO W12-YY.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	   S e t   u p	 t h e	 F U N C T I O N   k e y s     Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 1		 TO USER-ACTION
				    USER-SETTING.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		       ESC, F1 to F10 keys		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE ZERO		 TO USER-NUMBER.
	     MOVE 11		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		    PAGE-UP and PAGE-DOWN keys		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 53		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		      UP and DOWN ARROW keys		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 2		 TO USER-ACTION.
	     MOVE 3		 TO USER-SETTING.
	     MOVE 5		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё		   A C T I V A T E   M O U S E		       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 64		 TO MOUSE-FUNC.
	     MOVE 1		 TO MOUSE-PARAM.
	     CALL X"AF" USING MOUSE-FUNC
			      MOUSE-PARAM.
	   IF MOUSE-FUNC NOT = 255
	       MOVE "Y"		 TO MOUSE-ATTACHED
	       MOVE 66		 TO MOUSE-FUNC
	       MOVE 0		 TO MOUSE-PARAM
	       CALL X"AF" USING MOUSE-FUNC
				MOUSE-PARAM
	   ELSE
	       GO TO ZA05.
      *    зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
      *    Ё	       Set MOUSE key to act as FUNCTION key	       Ё
      *    юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды
	     MOVE 3		 TO USER-ACTION.
	     MOVE 27		 TO USER-NUMBER.
	     MOVE 2		 TO USER-KEYS.
	     CALL X"AF" USING SET-BIT-PAIRS, USER-KEY-CONTROL.

       ZA05.
	     MOVE "'C'ourier or 'L'etter Gothic font C/L [ ]"
				 TO WS-ERR-MES.
	     MOVE "C"		 TO WS-OPTION.

       ZA10.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT(WS-OPTION = "C" OR "L")
	       GO TO ZA10.
	     MOVE WS-OPTION	 TO WS-FONT.
	   IF WS-FONT = "C"
	       move FontTypeFaceC
				 to FontTypeFace
	       move 12		 to Font-family-namelen
	   ELSE
	       move FontTypeFaceL
				 to FontTypeFace
	       move 18		 to Font-family-namelen.
	     MOVE "'I'nvoices or 'S'tatements Test I/S [ ]"
				 TO WS-ERR-MES.
	     MOVE "I"		 TO WS-OPTION.

       ZA15.
	     PERFORM OPT-MESSAGE.
020250	   IF NOT(WS-OPTION = "I" OR "S")
	       GO TO ZA10.
	     MOVE WS-OPTION	 TO WS-TEST.

      * ______________________________________________________________ *
      * PC_PRINTER_OPEN 					       *
      * 							       *
      * Opens a printer channel, giving it a document title, and       *
      * optionally displaying the dialog boxes for printer control,    *
      * font selection, and progress indication.		       *
      * 							       *
      * SYNTAX: 						       *
      * 							       *
      * 	call "PC_PRINTER_OPEN" using	 printer-handle        *
      * 					 document-title        *
      * 			       by value  flags		       *
      * 			       by value  window-handle	       *
      * 			       returning status-code	       *
      * 							       *
      * PARAMETERS:						       *
      * 	printer-handle	pic x(4) comp-5.		       *
      * 	document-title	Group item defined as:		       *
      * 	    title-len	pic x(2) comp-5.		       *
      * 	    title-text	pic x.				       *
      * 	    flags	Numeric literal or pic x(4) comp-5.    *
      * 	    window-handle Numeric literal or pic x(4) comp-5.  *
      * 	status-code	See Key 			       *
      * 							       *
      * ON ENTRY:						       *
      * 	title-len	The length of the document title.      *
      * 	title-text	The title of the document to be        *
      * 			printed.			       *
      * 	flags	A set of bits to define printer options:       *
      * 	 bit 0	Display a dialog for defining printer	       *
      * 		characteristics (cannot be used with bit 2 or  *
      * 		bit 3)					       *
      * 	 bit 1	Display a font dialog box, so you can select   *
      * 		the font for the document		       *
      * 	 bit 2	Print in portrait orientation (cannot be used  *
      * 		with bit 0 or bit 3)			       *
      * 	 bit 3	Print in landscape orientation (cannot be used *
      * 		with bit 0 or bit 2)			       *
      * 	 bit 4	Display a progress indicator dialog box        *
      * 							       *
      * 	All remaining bits are reserved for future use and     *
      * 	should not be set.				       *
      * 							       *
      * 	window-handle  Handle of the parent window. The parent *
      * 		       window is used as a reference for       *
      * 		       positioning dialog boxes on the screen. *
      * 		       If window-handle is 0 positioning is    *
      * 		       system dependent.		       *
      * 							       *
      * ON EXIT:						       *
      * 	printer-handle	Returns a handle to be used for        *
      * 			subsequent printer operations.	       *
      * 	status-code	Printer status code:		       *
      * 							       *
      * 	    0		Successful			       *
      * 	    1		Could not open printer device	       *
      * 	    5		Failed to open file		       *
      * 	    9		No default printer found. Select a     *
      * 			printer from Windows Control Panel.    *
      * 	   24		User pressed Cancel on the printer     *
      * 			setup dialog or the font selection     *
      * 			dialog. 			       *
      * 							       *
      * COMMENTS:						       *
      * 							       *
      * 	This routine is available in 32-bit environments only. *
      * 							       *
      * 	You can check the success of the call by examining     *
      * 	RETURN-CODE.					       *
      * ______________________________________________________________ *
	     move 17		 to title-len.
             move "Printer Info Test"
                                 to title-text.

             call "PC_PRINTER_OPEN" using by reference prn-handle
                                          by reference document-title
                                          by value prn-flags
                                          by value 0
	     end-call.

       ZA999.
	     EXIT.
