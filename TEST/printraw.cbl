      $set Ans85 Mf DefaultByte"00" constant INTCODE(0)
      *----------------------------------------------------------------*
      *                         PRINTRAW                               *
      *                                                                *
      *  This example program demonstrates how to use the Windows SDK  *
      *  Printing and Spooling functions to print a document which     *
      *  contains embedded printer ESC sequence codes. These codes can *
      *  control various formatting features of the printer.           *
      *                                                                *
      *  The Windows Print Spooler and Print Manager communicate with  *
      *  a printer through its driver program. Options such as paper   *
      *  size and line spacing are usually set using Windows SDK calls *
      *  like PrintDlg or Escape.                                      *
      *                                                                *
      *  Some older applications which have not yet been converted to  *
      *  Windows 32-bit control these printer functions by sending the *
      *  printer commands (ESC sequences) directly to the printer      *
      *  either within a separate record or file or by embedding them  *
      *  directly within the print record text.                        *
      *                                                                *
      *  You cannot send ESC sequences directly to a printer using     *
      *  standard COBOL I-O statements. The Driver will strip these    *
      *  characters out of the text before sending the page to the     *
      *  print spooler. Therefore the expected formatting will not     *
      *  occur and the page will not print properly.                   *
      *                                                                *
      *  This example demonstrates how this can be accomplished by     *
      *  setting up the printer as a Raw device so as to bypass the    *
      *  print driver and write directly to the printer itself.        *
      *                                                                *
      *  Printer ESC codes differ between different manufacturers and  *
      *  models so this method is definately not portable. This program*
      *  was written for an HP LaserJet 5L printer and uses the ESC    *
      *  codes supported by this model and other HP LaserJet printers. *
      *                                                                *
      *  You can modify this program for your own testing by changing  *
      *  the value in the printer-name parameter and printer-esc-codes *
      *  to the appropriate values for your supported printer.         *
      *----------------------------------------------------------------*
       id division.
       program-id.  printraw.
       special-names.
	   call-convention 2 is WINAPI.
       data division.
       working-storage section.
       78 PROGRAM-NAME                            value "PRINTRAW".
       78 CRLF                                    value x"0D0A".

      *** Change the following to your printer name.

       01 printer-name           pic x(256)       value "HP LaserJet 5L"
                                                        & x"00".

      *** applicable only to HP LaserJet printers.

       01 printer-esc-codes.
          05 reset-printer       pic x(2)         value x"1B45".
          05 page-size-legal     pic x(5)         value x"1B266C3341".
          05 page-size-A4        pic x(6)         value x"1B266C323641".
          05 portrait            pic x(5)         value x"1B266C304F".
          05 landscape           pic x(5)         value x"1B266C314F".
          05 6-lines-per-inch    pic x(5)         value x"1B266C3644".
          05 8-lines-per-inch    pic x(5)         value x"1B266C3844".
          05 underline-on        pic x(5)         value x"1B26643044".
          05 underline-off       pic x(4)         value x"1B266440".

       01 printer-handle         pic x(4) comp-5  value zeroes.

       01 doc-info1.
          05 doc-name-pointer             pointer.
          05 output-file-pointer          pointer.
          05 data-type-pointer            pointer.

       01 my-document            pic x(12)        value z"My Document".
       01 data-type              pic x(4)         value z"RAW".                               .

       01 bytes-to-write         pic x(4) comp-5  value zeroes.
       01 bytes-written          pic x(4) comp-5  value zeroes.
       01 printer-status         pic x(4) comp-5  value zeroes.

       01 printer-record         pic x(80)        value spaces.
       01 string-pointer         pic 9(3)         value 1.
       procedure division.
       000-begin.

      ***  If we wish to compile to .int code then cob32api.dll must be
      ***  loaded before Windows API calls can be made.

       $if INTCODE=1
           call "cob32api" end-call
       $end

      ***
           display PROGRAM-NAME ": Starting."
           perform 100-setup-print-job
           if printer-status <> 0
              perform 105-print-a-page
              perform 110-close-print-job
           end-if
           display PROGRAM-NAME ": Ending."
           stop run.

      *----------------------------------------------------------------*

       100-setup-print-job.

           perform 200-open-printer
           if printer-status <> 0
              perform 205-start-document
              if printer-status <> 0
                 perform 210-start-page
                 if printer-status <> 0
                    continue
                 else
                    display PROGRAM-NAME ": Error on Start Page."
                    perform 220-end-document
                    perform 225-close-printer
                 end-if
              else
                 display PROGRAM-NAME ": Error on Start Document."
                 perform 225-close-printer
              end-if
           else
              display PROGRAM-NAME ": Error on Open Printer."
           end-if.

      *----------------------------------------------------------------*

       105-print-a-page.

      ***  Send setup codes to printer.

           string reset-printer
                  page-size-legal
                  portrait
                  6-lines-per-inch
                  CRLF delimited by size
                  into printer-record
                     with pointer string-pointer
           end-string

           perform 230-write-to-printer
           if printer-status = 0
              exit paragraph
           end-if

           string "This is Test Line Number 1"
                  CRLF delimited by size
                  into printer-record
                     with pointer string-pointer
           end-string

           perform 230-write-to-printer
           if printer-status = 0
              exit paragraph
           end-if

           string underline-on
                  "This line should be underlined"
                  underline-off
                  CRLF delimited by size
                  into printer-record
                     with pointer string-pointer
           end-string

           perform 230-write-to-printer
           if printer-status = 0
              exit paragraph
           end-if

           string "This line should not be underlined"
                  CRLF delimited by size
                  into printer-record
                     with pointer string-pointer
           end-string

           perform 230-write-to-printer.

      *----------------------------------------------------------------*

       110-close-print-job.

           perform 215-end-page
           perform 220-end-document
           perform 225-close-printer.

      *----------------------------------------------------------------*
      *    Get a handle to the specified printer. Used in all calls.   *
      *----------------------------------------------------------------*

       200-open-printer.

	   call WINAPI "OpenPrinterA"
              using by reference printer-name
                    by reference printer-handle
                    by value     0 size 4  *> printer-defaults not used
              returning          printer-status
           end-call.

      *----------------------------------------------------------------*
      *    The StartDocPrinter function informs the print spooler that *
      *    a document is to be spooled for printing.                   *
      *----------------------------------------------------------------*

       205-start-document.

      ***  Fill in the docinfo1 structure. Sets the printer to Raw mode.

           set doc-name-pointer to address of my-document
           set output-file-pointer to null
           set data-type-pointer to address of data-type

	   call WINAPI "StartDocPrinterA"
              using by value    printer-handle
                    by value     1 size 4  *> Use docinfo1 structure
                    by reference doc-info1
              returning printer-status
           end-call.

      *----------------------------------------------------------------*
      *   The StartPagePrinter function informs the spooler that a     *
      *   page is about to be printed on the specified printer.        *
      *----------------------------------------------------------------*

       210-start-page.

	   call WINAPI "StartPagePrinter"
              using by value printer-handle
              returning      printer-status
           end-call.

      *----------------------------------------------------------------*
      *    The EndPagePrinter function indicates the end of one page   *
      *    and the beginning of the next page for the specified printer*
      *----------------------------------------------------------------*

       215-end-page.

	   call WINAPI "EndPagePrinter"
              using by value printer-handle
              returning printer-status
           end-call.

      *----------------------------------------------------------------*

       220-end-document.

	   call WINAPI "EndDocPrinter"
              using by value printer-handle
              returning printer-status
           end-call.

      *----------------------------------------------------------------*

       225-close-printer.

	   call WINAPI "ClosePrinter"
              using by value printer-handle
           end-call.

      *----------------------------------------------------------------*
      *    The WritePrinter function informs the print spooler that    *
      *    data should be written to the specified printer.            *
      *----------------------------------------------------------------*

       230-write-to-printer.

           compute bytes-to-write = string-pointer - 1

	   call WINAPI "WritePrinter"
              using by value     printer-handle
                    by reference printer-record
                    by value     bytes-to-write
                    by reference bytes-written
              returning          printer-status
           end-call

           if printer-status = 0
              display PROGRAM-NAME ": Error on Write Printer."
           else
              move spaces to printer-record
              move 1 to string-pointer
           end-if.
