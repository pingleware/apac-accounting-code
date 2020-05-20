       Identification Division.
       Program-ID. PCPrinterDefaultIntro.
       Environment Division.
       Input-Output Section.

       Data Division.
       Working-Storage Section.

      *>Parameters for PC_PRINTER_DEFAULT_NAME

       01  Default-Printer-Flags           pic x(04) comp-5  Value 1.
       01  Default-Printer-Name.
           05  Default-Printer-Name-Len    pic x(02) comp-5.
           05  Default-Printer-Name-Text   pic x(80).

      *>Parameters for PC_PRINTER_DEFAULT_FONT

       01  Default-Font-Family-Name.
           05  Default-Font-Name-Len       pic x(02) comp-5.
           05  Default-Font-Name-Text      pic x(80).
       01  Default-Font-Size               pic x(04) comp-5.
       01  Default-Font-Style              pic x(04) comp-5.

      *>Parameters for PC_PRINTER_DEFAULT_PROPERTIES

       01  Default-Properties-Flags      pic x(4) comp-5.
       01  Default-Properties.
           02 pr-len           pic x(2) comp-5.
           02 pr-papersize     pic s9(4) comp-5.
           02 pr-paperlength   pic s9(4) comp-5.
           02 pr-paperwidth    pic s9(4) comp-5.
           02 pr-scale         pic s9(4) comp-5.
           02 pr-copies        pic s9(4) comp-5.
           02 pr-papertray     pic s9(4) comp-5.
           02 pr-printquality  pic s9(4) comp-5.
           02 pr-color         pic s9(4) comp-5.
           02 pr-duplex        pic s9(4) comp-5.
           02 pr-orientation   pic s9(4) comp-5.
           02 pr-yresolution   pic s9(4) comp-5.
      *>



      *>Parameters for PC_PRINTER_OPEN

       01  Printer-Handle                  pic x(04) comp-5.
       01  Document-Title.
           05  Title-Len                   pic x(02) comp-5.
           05  Title-Text                  pic x(80).
       01  Printer-Flags                   pic x(04) comp-5.
       01  Window-Handle		   pic x(04) comp-5.

      *>

      *>Parameters for PC_PRINTER_WRITE

      *>---Printer-Handle (same as above)
       01  Print-Buffer                    pic x(80).
       01  Print-Buffer-Len                pic x(04) comp-5.
      *>

      *>Parameters for PC_PRINTER_CONTROL
      *>---Printer-Handle (same as above)
      *>---print-command:  1 - Abort printing and close printer
      *>                   2 - Throw a page
      *>                   3 - Flush the print buffers
      *>                   4 - Start a new line
      *>

      *>Parameters for PC_PRINTER_CLOSE
      *>---Printer-Handle (same as above)
      *>

      *>Parameters for PC_PRINT_FILE

       01  Filename.
           02  Filename-Len                    pic xx comp-5.
           02  Filename-Text                   pic x(80).
       01  DocumentTitle.
           02  DocumentTitle-Len               pic xx comp-5.
           02  DocumentTitle-Text              pic x(15).
      *>  Printer-Flags (same as above)
      *>  Window-Handle (Same as above)
      *>

      *>Parameters for PC_PRINTER_INFO
       78 PrinterNameBufferBegin        value next.
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
           02  PrinterInformationLength  pic xxxx comp-5.
           02  DeviceContextHandle       pic xxxx comp-5.
           02  reserved                  pic xxxx comp-5.
           02  Orientation               pic xxxx comp-5.
           02  PageRowsCurrentFont       pic xxxx comp-5.
           02  PageColsCurrentFont       pic xxxx comp-5.
           02  RowsRemainingCurrentFont  pic xxxx comp-5.
           02  MaxGraphicsHorizontalAxis pic xxxx comp-5.
           02  MaxGraphicsVerticalAxis   pic xxxx comp-5.
           02  MinGraphicsHorizontalAxis pic xxxx comp-5.
           02  MinGraphicsVerticalAxis   pic xxxx comp-5.
           02  CurrentCooridinateX       pic xxxx comp-5.          *> Horizontal
           02  CurrentCooridinateY       pic xxxx comp-5.          *> Vertical
           02  NumberOfCopies            pic xx   comp-5.
           02  PrintQuality              pic xx   comp-5.          *> Draft, Low,Medium, High, Default or DPI
           02  Color                     pic x    comp-5.          *> Monochrome or Color
           02  reserved                  pic x    comp-5.          *> Slack Byte
           02  DriverVersionNumber       pic xx   comp-5.
           02  PrinterName.
               03  PrinterNameLength         pic xxxx comp-5
                   value PrinterNameBufferLength.
               03  p-PrinterNameBuffer                     pointer.
           02  PrinterType.
               03  PrinterTypeLength         pic xxxx comp-5
                   value PrinterTypeBufferLength.
               03  p-PrinterTypeBuffer                     pointer.
           02  PrinterDevice.
               03 PrinterDeviceLength       pic xxxx comp-5
                  value PrinterDeviceBufferLength.
               03 p-PrinterDeviceBuffer                   pointer.
           02  PrinterLocation.
               03 PrinterLocationLength     pic xxxx comp-5
                  value PrinterLocationBufferLength.
               03 p-PrinterLocationBuffer                 pointer.
           02  PrinterComment.
               03 PrinterCommentLength      pic xxxx comp-5
                  value PrinterCommentBufferLength.
               03 p-PrinterCommentBuffer                  pointer.
           02  PrinterPaperSize.
               03 PrinterPaperSizeLength    pic xxxx comp-5
                 value PrinterPaperSizeBufferLength.
               03 p-PrinterPaperSizeBuffer                pointer.
      *>

      *>
       01  Operation                       pic x(30).
      *>


       Procedure Division.

      *	   perform Set-Default-Printer
      *	   perform Set-Default-Font
      *	   perform Set-Default-Properties

      *>  At this point you can do one of two things
      *>  Use the combination of PC_PRINTER_OPEN, PC_PRINTER_WRITE
      *>  and PC_PRINTER_CLOSE to process the printed material

           perform Open-Printer
           perform Print-Info
           perform Print-Text
           perform Close-Printer

      *>  Or if the printed material is already available in a file
      *>  simply use the PC_PRINT_FILE call

           perform Print-File

           stop run
           .

       Set-Default-Printer.

      *>  Move the name of the printer into the data item
      *>  terminating it by a null

           move "\\montreal-fs1\mtl-lj5" & x"00"
             to Default-Printer-Name-Text

           perform varying Default-Printer-Name-Len from 80 by -1
           until Default-Printer-Name-Text(Default-Printer-Name-Len:1)
              not = space
               continue
           end-perform

	   call "PC_PRINTER_DEFAULT_NAME"
               using by value     Default-Printer-Flags
                     by reference Default-Printer-Name
           end-call




           if Return-Code not = 0
               move "PC_PRINTER_DEFAULT_NAME" to Operation
               perform 9999-Display-Error
           end-if

           .

       Set-Default-Font.

           move "Courier" & x"00" to Default-Font-Name-Text

           perform varying Default-Font-Name-Len from 80 by -1
            until Default-Font-Name-Text(Default-Font-Name-Len:1)
                  not = space
               continue
           end-perform

           move 20 to Default-Font-Size

      *>  Move the appropriate Font Style to the Data Item
      *>  bit decimal
      *>   0     1    Italic
      *>   1     2    Underline
      *>   2     4    Strikeout
      *>   3     8    Bold
      *>
      *>  For example a value of 9 would mean a combination of
      *>  Italic and Bold

           move 11 to Default-Font-Style

           call "PC_PRINTER_DEFAULT_FONT"
              using by reference Default-Font-Family-Name
                    by value     Default-Font-Size
                    by value     Default-Font-Style
           end-call

           if Return-Code not = 0
               move "PC_PRINTER_DEFAULT_FONT" to Operation
               perform 9999-Display-Error
           end-if

           .

       Set-Default-Properties.

           Initialize Default-Properties
           move 24 to pr-len

      *>  The values for the Default-Properties-Flags are
      *>
      *>        PAPERSIZE       1
      *>        PAPERLENGTH     2
      *>        PAPERWIDTH      4
      *>        SCALE           8
      *>        COPIES          16
      *>        PAPERTRAY       32
      *>        PRINTQUALITY    64
      *>        COLOR           128
      *>        DUPLEX          256
      *>        ORIENTATION     512
      *>        YRESOLUTION     1024
      *>
      *>  To use a combination of the above flags just add the values
      *>
           move 512 to Default-Properties-Flags

      *>  The values for the orientation propertiy flag are;
      *>        Portrait       1
      *>        Landscape      2
      *>
           move 2 to pr-orientation.

           call "PC_PRINTER_DEFAULT_PROPERTIES"
                using by value     Default-Properties-Flags
                      by reference Default-Properties
           end-call

           if Return-Code not = 0
               move "PC_PRINTER_DEFAULT_PROPERTIES" to Operation
               perform 9999-Display-Error
           end-if

           .

       Open-Printer.

           move "Test Report" to Title-Text
           perform varying Title-Len from 80 by -1
                     until Title-Text(Title-Len:1) not = space
               continue
           end-perform


      *>   PC_PRINTER_OPEN Flags settings
      *>   bit decimal
      *>    0     1    Display Printer Dialog allowing printer selection
      *>               and orientation specification.
      *>    1     2    Display Font Dialog allowing default font
      *>                selection for the document.
      *>    2     4    Specify Portrait orientation. No Dialog.
      *>                (conflicts with bit 0 and bit 3).
      *>    3     8    Specify Landscape orientation. No Dialog.
      *>                 (conflicts with bit 0 and bit 2).
      *>    4    16    Display Printing progress dialog.
      *>
      *> The decimal values of the flags are additive. For example,
      *> if Landscape and a Font Dialog are requirements, 8 plus 2
      *> equals 10, should be specified as the value for the flags item.


	   move 3 to Printer-Flags
           move 0 to Printer-Handle
           move 0 to Window-Handle

           call "PC_PRINTER_OPEN" using
               by reference Printer-Handle
               by reference Document-Title
               by value     Printer-Flags
               by value     Window-Handle
           end-call

           if Return-Code not = 0
               move "PC_PRINTER_OPEN" to Operation
               perform 9999-Display-Error
           end-if

           .

       Print-Info.

           move length of PrinterInformation
            to PrinterInformationLength
           set p-PrinterNameBuffer
            to address of PrinterNameBuffer
           set p-PrinterTypeBuffer
            to address of PrinterTypeBuffer
           set p-PrinterDeviceBuffer
            to address of PrinterDeviceBuffer
           set p-PrinterLocationBuffer
            to address of PrinterLocationBuffer
           set p-PrinterCommentBuffer
            to address of PrinterCommentBuffer
           set p-PrinterPaperSizeBuffer
            to address of PrinterPaperSizeBuffer

           call "PC_PRINTER_INFO"
                using by reference Printer-Handle
                      by reference PrinterInformation
           end-call

           if Return-Code not = 0
               move "PC_PRINTER_INFO" to Operation
               perform 9999-Display-Error
           end-if

           display "Orientation            : " Orientation
           display "Rows                   : " PageRowsCurrentFont
           display "Cols                   : " PageColsCurrentFont
           display "Rows Reminaing         : " RowsRemainingCurrentFont
           display "Max horz               : " MaxGraphicsHorizontalAxis
           display "Max vert               : " MaxGraphicsVerticalAxis
           display "Min horz               : " MinGraphicsHorizontalAxis
           display "Min vert               : " MinGraphicsVerticalAxis
           display "Current horz           : " CurrentCooridinateX
           display "Current vert           : " CurrentCooridinateY
           display "Copies                 : " NumberOfCopies
           display "Quality                : " no advancing
           evaluate PrintQuality
             when 0 display "Draft"
             when 1 display "Low"
             when 2 display "Medium"
             when 3 display "High"
             when 4 display "printers default used"
             when other display PrintQuality
           end-evaluate

           display "Color                   : " no advancing

           if Color = 0
             display "Monochrome"
           else
             display "Color"
           end-if

           if PrinterNameLength = 0
             display "Printer name           : not available"
           else
             display "Printer name           : "
		 PrinterNameBuffer(1: PrinterNameLength)
             display "Printer name  size     : " PrinterNameLength
           end-if

           if PrinterTypeLength = 0
             display "Printer type           : not available"
           else
             display "Printer type           : "
		 PrinterTypeBuffer(1: PrinterTypeLength)
             display "Printer type size      : " PrinterTypeLength
           end-if

           if PrinterDeviceLength = 0
             display "Printer device         : not available"
           else
             display "Printer device(s)      : "
		 PrinterDeviceBuffer(1: PrinterDeviceLength)
             display "Printer device size    : " PrinterDeviceLength
           end-if

           if PrinterLocationLength = 0
             display "Printer location       : not available"
           else
             display "Printer location: "
		 PrinterLocationBuffer(1: PrinterLocationLength)
             display "Printer location size  : " PrinterLocationLength
           end-if

           if PrinterCommentLength = 0
             display "Printer comment        : not available"
           else
             display "Printer comment: "
		 PrinterCommentBuffer(1: PrinterCommentLength)
             display "Printer comment size   : " PrinterCommentLength
           end-if

           if PrinterPaperSizeLength = 0
             display "Printer papersize: not available"
           else
             display "Printer papersize      : "
		 PrinterPaperSizeBuffer(1: PrinterPaperSizeLength)
             display "Printer papersize size : " PrinterPaperSizeLength
           end-if

           display "Driver version         : " DriverVersionNumber

           .

       Print-Text.

           perform Start-New-Line 3 times *>skip 3 lines

           move "This is a test program for PC_PRINTER_DEFAULT_ calls"
                to Print-Buffer
           move 52 to Print-Buffer-Len

           perform Write-To-Printer *>writes Print-Buffer with no
                                    *>advancing

           .

       Start-New-Line.
      *>This is better than to print x"ODOA" using pc_printer_write

           call "PC_PRINTER_CONTROL" using
               by reference Printer-Handle
               by value     4  *> Start a new line
           end-call

           if Return-Code not = 0
               move "PC_PRINTER_CONTROL" to Operation
               perform 9999-Display-Error
           end-if

           .

       Write-To-Printer.

           call "PC_PRINTER_WRITE" using
               by reference Printer-Handle
               by reference Print-Buffer
               by value     Print-Buffer-Len
           end-call

           if Return-Code not = 0
               move "PC_PRINTER_WRITE" to Operation
               perform 9999-Display-Error
               stop run
           end-if

           .

       Close-Printer.
           call "PC_PRINTER_CLOSE" using
               by reference Printer-Handle
           end-call
           if Return-Code not = 0
               move "PC_PRINTER_CLOSE" to Operation
               perform 9999-Display-Error
           end-if
           .

       Print-File.

           move "Test Report" to DocumentTitle-Text
           perform varying DocumentTitle-Len from 80 by -1
            until DocumentTitle-Text(DocumentTitle-Len:1) not = space
               continue
           end-perform

           move "PCPrinterDefaultIntro.dat" to Filename-Text
           perform varying Filename-Len from 80 by -1
            until Filename-Text(Filename-Len:1) not = space
               continue
           end-perform


      *>   PC_PRINT_FILE Flags settings
      *>   bit decimal
      *>    0     1    Display Printer Dialog allowing printer selection
      *>               and orientation specification.
      *>    1     2    Display Font Dialog allowing default font
      *>                selection for the document.
      *>    2     4    Specify Portrait orientation. No Dialog.
      *>                (conflicts with bit 0 and bit 3).
      *>    3     8    Specify Landscape orientation. No Dialog.
      *>                 (conflicts with bit 0 and bit 2).
      *>    4    16    Display Printing progress dialog.
      *>
      *> The decimal values of the flags are additive. For example,
      *> if Landscape and a Font Dialog are requirements, 8 plus 2
      *> equals 10, should be specified as the value for the flags item.

           move 0 to Printer-Flags
           move 0 to Window-Handle

           call 'PC_PRINT_FILE'
               using by reference Filename
                     by reference DocumentTitle
                     by value     Printer-Flags
                     by value     Window-Handle


           if Return-Code not = 0
               move "PC_PRINT_FILE" to Operation
               perform 9999-Display-Error
           end-if

           .

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
