      $ set Ans85 Mf NoOsvs NoVsc2   Qual NoAlter DefaultByte"00"
      $ set remove(control)
       working-storage section.
           78 programName value 'PrintI2'.
            1 PRT-INFO-1 is typedef.
            2 pi-struct-size      pic xxxx    comp-5.
            2 hdc                 pic xxxx    comp-5.
            2 hps                 pic xxxx    comp-5.
            2 orientation         pic xxxx    comp-5.
            2 rows                pic xxxx    comp-5.
            2 cols                pic xxxx    comp-5.
            2 rows-left           pic xxxx    comp-5.
            2 max-horiz           pic xxxx    comp-5.
            2 max-vert            pic xxxx    comp-5.
            2 min-horiz           pic xxxx    comp-5.
            2 min-vert            pic xxxx    comp-5.
            2 curr-horiz          pic xxxx    comp-5.
            2 curr-vert           pic xxxx    comp-5.
            2 copies              pic xx      comp-5.
            2 quality             pic xx      comp-5.
            2 color               pic x       comp-5.
            2 reserved1           pic x       comp-5.
            2 driver-ver          pic xx      comp-5.
            2 pname.
            3 cbsize              pic xxxx    comp-5.
            3 buffer                          pointer.
            2 ptype.
            3 cbsize              pic xxxx    comp-5.
            3 buffer                          pointer.
            2 pdevice.
            3 cbsize              pic xxxx    comp-5.
            3 buffer                          pointer.
            2 plocation.
            3 cbsize              pic xxxx    comp-5.
            3 buffer                          pointer.
            2 pcomment.
            3 cbsize              pic xxxx    comp-5.
            3 buffer                          pointer.
            2 ppapersize.
            3 cbsize              pic xxxx    comp-5.
            3 buffer                          pointer.
            1.
            2 document-title.
            3 title-len           pic xx      comp-5.
            3 title-text          pic x(20).
            2 font-family.
            3 font-family-namelen pic xx      comp-5  value 80.
            3 font-family-name    pic x(80).
            2 print-info          PRT-INFO-1.
            2 abort               pic xxxx    comp-5  value 1.
            2 control             pic xxxx    comp-5  value 2.
            2 flags               pic xxxx    comp-5  value 1.
      * PC_PRINTER_OPEN Options
      * bit - decimal - description
      *        value
      *  0       1      Display Print Setup [Dialog]
      *  1       2      Display Font [Dialog]
      *  2       4      Select Portrait Mode (mutually exclusive of bits 0 and 3)
      *  3       8      Select Landscape Mode (mutually exclusive of bits 0 and 2)
      *  4      16      Display Progress Indicator (not functional)
            2 handle              pic xxxx    comp-5.
            1 printer-name        pic x(255).
            1 printer-type        pic x(255).
            1 printer-device      pic x(255).
            1 printer-location    pic x(255).
            1 printer-comment     pic x(255).
            1 printer-papersize   pic x(255).
            linkage section.
            1 szPrinterName pic x(256).
            procedure division using szPrinterName.
            display programName ': Starting.'
            move 17 to title-len
            move 'Printer Info Test' to title-text
            call 'PC_PRINTER_OPEN' using by reference handle
                                         by reference document-title
                                         by value     flags
                                         by value     0 size 4
            if RETURN-CODE = zero
              move length of print-info to pi-struct-size
              set buffer of pname of print-info to address of printer-name
              move 255 to cbsize of pname of print-info
              set buffer of ptype of print-info to address of printer-type
              move 255 to cbsize of ptype of print-info
              set buffer of pdevice of print-info to address of printer-device
              move 255 to cbsize of pdevice of print-info
              set buffer of plocation of print-info to address of printer-location
              move 255 to cbsize of plocation of print-info
              set buffer of pcomment of print-info to address of printer-comment
              move 255 to cbsize of pcomment of print-info
              set buffer of ppapersize of print-info to address of printer-papersize
              move 255 to cbsize of ppapersize of print-info
              call 'PC_PRINTER_INFO' using by reference handle
                                           by reference print-info
              if RETURN-CODE = zero
                display 'Orientation   : ' orientation of print-info
                display 'Rows          : ' rows of print-info
                display 'Cols          : ' cols of print-info
                display 'Rows Left     : ' rows-left of print-info
                display 'Max horz      : ' max-horiz of print-info
                display 'Max vert      : ' max-vert of print-info
                display 'Min horz      : ' min-horiz of print-info
                display 'Min vert      : ' min-vert of print-info
                display 'Current horz  : ' curr-horiz of print-info
                display 'Current vert  : ' min-vert of print-info
                display 'Copies        : ' copies of print-info
                display 'Quality       : ' no advancing
                evaluate quality of print-info
                when 0 display 'Draft'
                when 1 display 'Low'
                when 2 display 'Medium'
                when 3 display 'High'
                when 4 display 'printers default used'
                when other display quality of print-info ' DPI'
                end-evaluate
                display 'Color         : ' no advancing
                if color of print-info equals 0
                  display 'Mono Chrome'
                else
                  display 'Color'
                end-if
                if cbsize of pname of print-info equal 0
                  display 'Printer name  : not available'
                  move 1 to RETURN-CODE
                else
                  display 'Printer name  : '
                  printer-name(1: cbsize of pname of print-info)
                  display 'Printer name  size : ' cbsize of pname of print-info
                  move printer-name(1: cbsize of pname of print-info)
                   to  szPrinterName
                  move x'00' to szPrinterName(cbsize of pname of print-info + 1:1)
                end-if
                if cbsize of ptype of print-info equal 0
                  display 'Printer type  : not available'
                else
                  display 'Printer type  : '
                  printer-type(1: cbsize of ptype of print-info)
                  display 'Printer type size : ' cbsize of ptype of print-info
                end-if
                if cbsize of pdevice of print-info equal 0
                  display 'Printer device: not available'
                else
                  display 'Printer device(s): '
                  printer-device(1: cbsize of pdevice of print-info)
                  display 'Printer device size : ' cbsize of pdevice of print-info
                end-if
                if cbsize of plocation of print-info equal 0
                  display 'Printer location: not available'
                else
                  display 'Printer location: '
                  printer-location(1: cbsize of plocation of print-info)
                  display 'Printer location size : ' cbsize of plocation of print-info
                end-if
                if cbsize of pcomment of print-info equal 0
                  display 'Printer comment: not available'
                else
                  display 'Printer comment: '
                  printer-comment(1: cbsize of pcomment of print-info)
                  display 'Printer comment size : ' cbsize of pcomment of print-info
                end-if
                if cbsize of ppapersize of print-info equal 0
                  display 'Printer papersize: not available'
                else
                  display 'Printer papersize: '
                  printer-papersize(1: cbsize of ppapersize of print-info)
                  display 'Printer papersize size : ' cbsize of ppapersize of print-info
                end-if
                display 'Driver version : 'driver-ver of print-info
              else
                display 'PC_PRINTER_INFO failed (' RETURN-CODE ')'
              end-if
            else
              display 'Printer open failed.'
              move 1 to RETURN-CODE
            end-if
            call 'PC_PRINTER_CLOSE' using by reference handle
            display programName ': Ending.'
            exit program
            stop run.

