       Identification Division.
       Program-ID. PCPrinterDefaultProperties.
       Environment Division.
       Input-Output Section.

       Data Division.
       Working-Storage Section.

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
       01  Window-Handle                   pic x(04) comp-5.
      *>

      *>
      *>Parameters for PC_PRINTER_LOAD_BMP

       *>----Printer-Handle (same as above)
       01  BmpFilename                     pic x(11)
           value "mflogo.bmp" & x"0".
       01  BmpID                           pic x(04) comp-5.

      *>
      *>Parameters for PC_PRINTER_WRITE_BMP

       *>----Printer-Handle (same as above)
       *>----BmpID          (same as above)
       01  WriteBmpReserved                pic x(04) comp-5 value 0.
       01  BmpRow                          pic x(04) comp-5 value 10.
       01  BmpCol                          pic x(04) comp-5 value 5.
       01  BmpWidth                        pic x(04) comp-5 value 20.
       01  BmpHeigth                       pic x(04) comp-5 value 5.
      *>

      *>Parameters for PC_PRINTER_FREE_BMP

       *>----Printer-Handle (same as above)
       *>----BmpID          (same as above)
      *>

      *>Parameters for PC_PRINTER_CONTROL

      *>---Printer-Handle (same as above)
      *>---print-command:  1 - Abort printing and close printer
      *>                   2 - Throw a page
      *>                   3 - Flush the print buffers
      *>                   4 - Start a new line
      *>

      *>Parameters for PC_PRINTER_WRITE

      *>---Printer-Handle (same as above)
       01  Print-Buffer                    pic x(80).
       01  Print-Buffer-Len                pic x(04) comp-5.
      *>

      *>Parameters for PC_PRINTER_CLOSE

      *>---Printer-Handle (same as above)
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

       01  Operation                       pic x(30).
      *>

       Procedure Division.

      *>  The purpose of this sample is to show how can several
      *>  properties of your printer be changed at one time.
      *>  All these properties can be used individually or together
      *>  as in this case.
      *>
      *>  The properties that this example will change are;
      *>       - resolution/quality
      *>       - number of copies
      *>       - orientation
      *>       - papersize selection
      *>

           Initialize Default-Properties
           move 24 to pr-len

      *>   In the case of an HP LaserJet 5SI printer the available
      *>   resolutions are 75, 150, 300 and 600
      *>   This setting can be different from printer to printer
      *>   For this sample we will print at a resolution of 150

           move 150 to pr-printquality
           move 150 to pr-yresolution

      *>  The setting for the number of copies is simply the number
      *>  of copies of the printed material that you would like to get

           move 2 to pr-copies

      *>  The values for the orientation property flag are;
      *>
      *>        Portrait       1
      *>        Landscape      2
      *>

           move 2 to pr-orientation.

      *>  The values for the papersize property flag are;
      *>
      *>           LETTER                1.
      *>           LETTERSMALL           2.
      *>           TABLOID               3.
      *>           LEDGER                4.
      *>           LEGAL                 5.
      *>           STATEMENT             6.
      *>           EXECUTIVE             7.
      *>           A3                    8.
      *>           A4                    9.
      *>           A4SMALL              10.
      *>           A5                   11.
      *>           B4                   12.
      *>           B5                   13.
      *>           FOLIO                14.
      *>           QUARTO               15.
      *>           10X14                16.
      *>           11X17                17.
      *>           NOTE                 18.
      *>           ENV-9                19.
      *>           ENV-10               20.
      *>           ENV-11               21.
      *>           ENV-12               22.
      *>           ENV-14               23.
      *>           CSHEET               24.
      *>           DSHEET               25.
      *>           ESHEET               26.
      *>           ENV-DL               27.
      *>           ENV-C5               28.
      *>           ENV-C3               29.
      *>           ENV-C4               30.
      *>           ENV-C6               31.
      *>           ENV-C65              32.
      *>           ENV-B4               33.
      *>           ENV-B5               34.
      *>           ENV-B6               35.
      *>           ENV-ITALY            36.
      *>           ENV-MONARCH          37.
      *>           ENV-PERSONAL         38.
      *>           FANFOLD-US           39.
      *>           FANFOLD-STD-GERMAN   40.
      *>           FANFOLD-LGL-GERMAN   41.
      *>           ISO-B4               42.
      *>           JAPANESE-POSTCARD    43.
      *>           9X11                 44.
      *>           10X11                45.
      *>           15X11                46.
      *>           ENV-INVITE           47.
      *>           RESERVED-48          48.
      *>           RESERVED-49          49.
      *>           LETTER-EXTRA         50.
      *>           LEGAL-EXTRA          51.
      *>           TABLOID-EXTRA        52.
      *>           A4-EXTRA             53.
      *>           LETTER-TRANSVERSE    54.
      *>           A4-TRANSVERSE        55.
      *>           LETTER-EXTRA-TRANSVERS  56.
      *>           A-PLUS               57.
      *>           B-PLUS               58.
      *>           LETTER-PLUS          59.
      *>           A4-PLUS              60.
      *>           A5-TRANSVERSE        61.
      *>           B5-TRANSVERSE        62.
      *>           A3-EXTRA             63.
      *>           A5-EXTRA             64.
      *>           B5-EXTRA             65.
      *>           A2                   66.
      *>           A3-TRANSVERSE        67.
      *>           A3-EXTRA-TRANSVERSE  68.
      *>           LAST                 68.
      *>           USER                256.
      *>

           move 5 to pr-papersize.

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
      *>  In this case printquality(64) + yresolution(1024)
      *>  + copies(16) + orientation(512) + papersize(1) = 1617
      *>
           move 1617 to Default-Properties-Flags

           perform Call-Default-Properties
           perform Open-Printer
           perform Print-Info.
           perform Print-Bmp
           perform Close-Printer

           Stop Run

           .

       Call-Default-Properties.

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

           move 0 to Printer-Flags
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
                 PrinterNameBuffer
                 (1: PrinterNameLength)
             display "Printer name  size     : " PrinterNameLength
           end-if

           if PrinterTypeLength = 0
             display "Printer type           : not available"
           else
             display "Printer type           : "
                 PrinterTypeBuffer
                 (1: PrinterTypeLength)
             display "Printer type size      : " PrinterTypeLength
           end-if

           if PrinterDeviceLength = 0
             display "Printer device         : not available"
           else
             display "Printer device(s)      : "
                 PrinterDeviceBuffer
                 (1: PrinterDeviceLength)
             display "Printer device size    : " PrinterDeviceLength
           end-if

           if PrinterLocationLength = 0
             display "Printer location       : not available"
           else
             display "Printer location: "
                 PrinterLocationBuffer
                 (1: PrinterLocationLength)
             display "Printer location size  : " PrinterLocationLength
           end-if

           if PrinterCommentLength = 0
             display "Printer comment        : not available"
           else
             display "Printer comment: "
                 PrinterCommentBuffer
                 (1: PrinterCommentLength)
             display "Printer comment size   : " PrinterCommentLength
           end-if

           if PrinterPaperSizeLength = 0
             display "Printer papersize: not available"
           else
             display "Printer papersize      : "
                 PrinterPaperSizeBuffer
                 (1: PrinterPaperSizeLength)
             display "Printer papersize size : " PrinterPaperSizeLength
           end-if

           display "Driver version         : " DriverVersionNumber

           .

       Print-Bmp.

           call "PC_PRINTER_LOAD_BMP"  using Printer-Handle
               by reference BmpFilename
               by reference BmpID
           end-call

           if Return-Code not = 0
               move "PC_PRINTER_LOAD_BMP" to Operation
               perform 9999-Display-Error
           end-if

           call "PC_PRINTER_WRITE_BMP" using Printer-Handle
               by value BmpID
               by value WriteBmpReserved
               by value BmpRow
               by value BmpCol
               by value BmpWidth
               by value BmpHeigth
           end-call

           if Return-Code not = 0
               move "PC_PRINTER_WRITE_BMP" to Operation
               perform 9999-Display-Error
           end-if

           call "PC_PRINTER_FREE_BMP" using     Printer-Handle
                                      by value  BmpID

           if Return-Code not = 0
               move "PC_PRINTER_FREE_BMP" to Operation
               perform 9999-Display-Error
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
