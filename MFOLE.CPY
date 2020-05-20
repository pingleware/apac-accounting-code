      *>
      *>
      *> MFOLE.CPY
      *> Public Interface parameters/constants for OLE class library
      *>
      *> For more detailed information about structures and error
      *> information, see the OLE programmer's Reference Vol 1,Vol 2
      *> and the OLE custom control developer's kit
      *>
      *> Copyright (C) 1996-1999 Micro Focus International Ltd
      *>

      $if MFOLE-CPY NOT DEFINED

       78 MFOLE-CPY          VALUE 1.

      $if OLEWINDEF-CPY NOT DEFINED

      $if data-pointer not defined
       01 data-pointer pointer is typedef.
      $end
      $if ULONG not defined
       01 ULONG             PIC 9(9)  COMP-5 TYPEDEF.
      $end
      $if USHORT not defined
       01 USHORT            PIC 9(4)  COMP-5 TYPEDEF.
      $end
      $if UINT not defined
       01 UINT              PIC 9(9)  COMP-5 TYPEDEF.
      $end
      $if BYTE not defined
       01 BYTE              PIC x COMP-5 TYPEDEF.
      $end
      $if WORD not defined
       01 WORD              PIC s9(4) COMP-5 TYPEDEF.
      $end
      $if SHORT not defined
       01 SHORT             PIC s9(4) COMP-5 TYPEDEF.
      $end
      $if INT not defined
       01 INT               PIC S9(9) COMP-5 TYPEDEF.
      $end
      $if LONG not defined
       01 LONG              PIC S9(9) COMP-5 TYPEDEF.
      $end
      $if BOOL not defined
       01 BOOL              PIC S9(9) COMP-5 TYPEDEF.
      $end
      $if DWORD not defined
       01 DWORD             PIC 9(9)  COMP-5 TYPEDEF.
      $end
      $if UCHAR not defined
       01 UCHAR             PIC x     COMP-5 TYPEDEF.
      $end
      $if CLIPFORMAT not defined
       01 CLIPFORMAT        WORD    TYPEDEF.
      $end

      $if POINT not defined
       01  tagPOINT          is typedef.
        02 x                 usage long.
        02 y                 usage long.

       01  POINT             is typedef       usage tagpoint.
       01  PPOINT            is typedef       usage data-pointer.
       01  NPPOINT           is typedef       usage data-pointer.
       01  LPPOINT           is typedef       usage data-pointer.
      $end

      $if RECT not defined
       01  tagRECT           is typedef.
        02 1left             usage long.
        02 1top              usage long.
        02 1right            usage long.
        02 1bottom           usage long.

       01  RECT              is typedef       usage tagrect.
       01  PRECT             is typedef       usage data-pointer.
       01  NPRECT            is typedef       usage data-pointer.
       01  LPRECT            is typedef       usage data-pointer.
       01  LPCRECT           is typedef       usage data-pointer.
      $end

      $if RECTL not defined
       01  1-RECTL           is typedef.
        02 1left             usage long.
        02 1top              usage long.
        02 1right            usage long.
        02 1bottom           usage long.

       01  RECTL             is typedef       usage 1-rectl.
       01  PRECTL            is typedef       usage data-pointer.
       01  LPRECTL           is typedef       usage data-pointer.
       01  LPCRECTL          is typedef       usage data-pointer.
      $end

      $if SIZEL not defined
       01  tagSIZE           is typedef.
        02 cx                usage long.
        02 cy                usage long.

       01  2SIZE             is typedef       usage tagsize.
       01  PSIZE             is typedef       usage data-pointer.
       01  LPSIZE            is typedef       usage data-pointer.
       01  SIZEL             is typedef       usage 2size.
       01  PSIZEL            is typedef       usage data-pointer.
       01  LPSIZEL           is typedef       usage data-pointer.
      $end

      $if HWND not defined
       01 HWND is typedef       usage data-pointer.
      $end

      $if HACCEL not defined
       01 HACCEL is typedef       usage data-pointer.
      $end

      $if HANDLE not defined
       01 HANDLE is typedef       usage data-pointer.
      $end

      $if MSG not defined
       01  tagMSG            is typedef.
        02 1hwnd             usage data-pointer.
        02 1message          usage UINT.
        02 1wparam           usage UINT.
        02 1lparam           usage long.
        02 1time             usage ULONG.
        02 pt                usage point.

       01  MSG               is typedef       usage tagmsg.
       01  PMSG              is typedef       usage data-pointer.
       01  NPMSG             is typedef       usage data-pointer.
       01  LPMSG             is typedef       usage data-pointer.

      $end

      $end

      *> Container Creation Flags
       78 OLECTR-CC-Simple          VALUE h"01".
       78 OLECTR-CC-Current         VALUE h"02".
       78 OLECTR-CC-RouteMenuCmds   VALUE h"04".
       78 OLECTR-CC-MDI             VALUE h"08".

      *> Container Styles
       78 OLECTR-CS-Inside-Out      VALUE h"01".
       78 OLECTR-CS-Allow-InPlace   VALUE h"02".
       78 OLECTR-CS-Ignore-DblClk   VALUE h"04".
       78 OLECTR-CS-Show-Objects    VALUE h"08".
       78 OLECTR-CS-Use-OLE-Palette VALUE h"10".
       78 OLECTR-CS-Use-Obj-Palette VALUE h"20".
       78 OLECTR-CS-No-Auto-Select  VALUE h"40".
       78 OLECTR-CS-Use-Own-Palette VALUE h"80".
       78 OLECTR-CS-Optimize-OCX    VALUE h"100".

      *> Site Size Modes
       78 OLECTR-SM-Auto            VALUE 0.
       78 OLECTR-SM-Clip            VALUE 1.
       78 OLECTR-SM-Stretch         VALUE 2.

      *> Site Styles
       78 OLECTR-SS-Full-Inplace      VALUE h"00000001".
       78 OLECTR-SS-InPlace-NoMenu    VALUE h"00000002".
       78 OLECTR-SS-InPlace-NoToolBar VALUE h"00000004".
       78 OLECTR-SS-Inside-Out        VALUE h"00000008".
       78 OLECTR-SS-Clipping-Rect     VALUE h"00000010".
       78 OLECTR-SS-Disabled          VALUE h"80000000".

      *> Popup Menu Flags
       78 OLECTR-PM-Cascade           VALUE h"00000001".
       78 OLECTR-PM-On-Button-Up      VALUE h"00000002".

      *> Aspects (for object presentation)
       78 DVASPECT-CONTENT            VALUE 1.
       78 DVASPECT-THUMBNAIL          VALUE 2.
       78 DVASPECT-ICON               VALUE 4.
       78 DVASPECT-DOCPRINT           VALUE 8.

       01 DVTARGETDEVICE     TYPEDEF.
          03 DVTD-tdSize                  DWORD.
          03 DVTD-tdDriverNameOffset      WORD.
          03 DVTD-tdDeviceNameOffset      WORD.
          03 DVTD-tdPortNameOffset;       WORD.
          03 DVTD-tdExtDevmodeOffset      WORD.
          03 DVTD-tdData                  BYTE.

      *> User Class Types
       78 OLECTR-USERCLASS-FULL       VALUE 1.
       78 OLECTR-USERCLASS-SHORT      VALUE 2.
       78 OLECTR-USERCLASS-APPNAME    VALUE 3.

       78 OLECTR-CREATE-NEW           VALUE 1.
       78 OLECTR-CREATE-FROM-FILE     VALUE 2.
       78 OLECTR-CREATE-LINK-TO-FILE  VALUE 3.

      *> Predefined verb values (for OLEX_DoVerb() )
       78 OLEIVERB-PRIMARY                VALUE 0.
       78 OLEIVERB-SHOW                   VALUE -1.
       78 OLEIVERB-OPEN                   VALUE -2.
       78 OLEIVERB-HIDE                   VALUE -3.
       78 OLEIVERB-UIACTIVATE             VALUE -4.
       78 OLEIVERB-INPLACEACTIVATE        VALUE -5.
       78 OLEIVERB-DISCARDUNDOSTATE       VALUE -6.
       78 OLEIVERB-PROPERTIES             VALUE -7.

      *> OLE/2 Misc Status bits
       78 OLEMISC-RECOMPOSEONRESIZE          VALUE 1.
       78 OLEMISC-ONLYICONIC                 VALUE 2.
       78 OLEMISC-INSERTNOTREPLACE           VALUE 4.
       78 OLEMISC-STATIC                     VALUE 8.
       78 OLEMISC-CANTLINKINSIDE             VALUE 16.
       78 OLEMISC-CANLINKBYOLE1              VALUE 32.
       78 OLEMISC-ISLINKOBJECT               VALUE 64.
       78 OLEMISC-INSIDEOUT                  VALUE 128.
       78 OLEMISC-ACTIVATEWHENVISIBLE        VALUE 256.
       78 OLEMISC-RENDERINGISDEVINDPNT       VALUE 512.
       78 OLEMISC-INVISIBLEATRUNTIME         VALUE h"00000400".
       78 OLEMISC-ALWAYSRUN                  VALUE h"00000800".
       78 OLEMISC-ACTSLIKEBUTTON             VALUE h"00001000".
       78 OLEMISC-ACTSLIKELABEL              VALUE h"00002000".
       78 OLEMISC-NOUIACTIVATE               VALUE h"00004000".
       78 OLEMISC-ALIGNABLE                  VALUE h"00008000".
       78 OLEMISC-SIMPLEFRAME                VALUE h"00010000".
       78 OLEMISC-SETCLIENTSITEFIRST         VALUE h"00020000".
       78 OLEMISC-IMEMODE                    VALUE h"00040000".

      *> OLECLOSE enumerations
       78 OLECLOSE-SAVEIFDIRTY               VALUE 0.
       78 OLECLOSE-NOSAVE                    VALUE 1.
       78 OLECLOSE-PROMPTSAVE                VALUE 2.

      *> OLE Object types,
       78 OLECTR-OBJECT                          VALUE 1.
       78 OLECTR-STATIC                          VALUE 2.

      *> Clipboard formats
       78 CF-EMBEDDEDOBJECT               VALUE h"00000001".
       78 CF-EMBEDSOURCE                  VALUE h"00000002".
       78 CF-OBJECTDESCRIPTOR             VALUE h"00000004".
       78 CF-LINKSOURCE                   VALUE h"00000008".

      *> Insert object flags
       78 IO-INSERTABLES                  VALUE h"00000001".
       78 IO-CONTROLS                     VALUE h"00000002".
       78 IO-BROWSE                       VALUE h"00000004".

      *> Install controls flags
       78 ICF-SHOWTOGGLEBOX               VALUE h"00000001".

      *> Paste Special flags,
       78 OLEUIPASTE-PASTEONLY            VALUE 0.
       78 OLEUIPASTE-PASTE                VALUE 512.
       78 OLEUIPASTE-LINK                 VALUE 1.
       78 OLEUIPASTE-ENABLEICON           VALUE 2048.

      *> tymed's
       78 TYMED-HGLOBAL                   VALUE h"00000001".
       78 TYMED-FILE                      VALUE h"00000002".
       78 TYMED-ISTREAM                   VALUE h"00000004".
       78 TYMED-ISTORAGE                  VALUE h"00000008".
       78 TYMED-GDI                       VALUE h"00000010".
       78 TYMED-MFPICT                    VALUE h"00000020".
       78 TYMED-ENHMF                     VALUE h"00000040".
       78 TYMED-NULL                      VALUE h"00000000".

      *> modifier keys
       78 MK-LBUTTON                      VALUE h"0001".
       78 MK-RBUTTON                      VALUE h"0002".
       78 MK-SHIFT                        VALUE h"0004".
       78 MK-CONTROL                      VALUE h"0008".
       78 MK-MBUTTON                      VALUE h"0010".
       78 MK-ALT                          VALUE h"0020".

      *> Drop effect values
       78 DROPEFFECT-NONE                 VALUE h"00000000".
       78 DROPEFFECT-COPY                 VALUE h"00000001".
       78 DROPEFFECT-MOVE                 VALUE h"00000002".
       78 DROPEFFECT-LINK                 VALUE h"00000004".
       78 DROPEFFECT-SCROLL               VALUE h"80000000".

       78 OLEUI-HANDLES-USEINVERSE    VALUE h"00000001".
       78 OLEUI-HANDLES-NOBORDER      VALUE h"00000002".
       78 OLEUI-HANDLES-INSIDE        VALUE h"00000004".
       78 OLEUI-HANDLES-OUTSIDE       VALUE h"00000008".

       78 OLEUI-SHADE-FULLRECT        VALUE 1.
       78 OLEUI-SHADE-BORDERIN        VALUE 2.
       78 OLEUI-SHADE-BORDEROUT       VALUE 3.

      *> IAdviseSink Advises
       78 IAS-OnViewChange         VALUE h"01".
       78 IAS-OnDataChange         VALUE h"07".
       78 IAS-OnRename             VALUE h"02".
       78 IAS-OnSave               VALUE h"03".
       78 IAS-OnClose              VALUE h"04".
       78 IAS-OnDrop               VALUE h"05".
       78 IAS-OnOLEDrop            VALUE h"06".

      *> IOleClientSite advises
       78 OLECTR-ShowObject           VALUE h"10".
       78 OLECTR-ShowWindow           VALUE h"11".
       78 OLECTR-HideWindow           VALUE h"12".

      *> OLE2 Dialog Return Codes
       78 OLEUI-FALSE                     VALUE 0. *> Error condition
       78 OLEUI-SUCCESS                   VALUE 1. *> No error, same as OLEUI-OK
       78 OLEUI-OK                        VALUE 1. *> OK button pressed
       78 OLEUI-CANCEL                    VALUE 2. *> Cancel pressed

       78 OLEUI-ERR-STANDARDMIN           VALUE 100.
       78 OLEUI-ERR-STRUCTURENULL         VALUE 101.
       78 OLEUI-ERR-STRUCTUREINVALID      VALUE 102.
       78 OLEUI-ERR-CBSTRUCTINCORRECT     VALUE 103.
       78 OLEUI-ERR-HWNDOWNERINVALID      VALUE 104.
       78 OLEUI-ERR-LPSZCAPTIONINVALID    VALUE 105.
       78 OLEUI-ERR-LPFNHOOKINVALID       VALUE 106.
       78 OLEUI-ERR-HINSTANCEINVALID      VALUE 107.
       78 OLEUI-ERR-LPSZTEMPLATEINVALID   VALUE 108.
       78 OLEUI-ERR-HRESOURCEINVALID      VALUE 109.

       78 OLEUI-ERR-FINDTEMPLATEFAILURE   VALUE 110.
       78 OLEUI-ERR-LOADTEMPLATEFAILURE   VALUE 111.
       78 OLEUI-ERR-DIALOGFAILURE         VALUE 112.
       78 OLEUI-ERR-LOCALMEMALLOC         VALUE 113.
       78 OLEUI-ERR-GLOBALMEMALLOC        VALUE 114.
       78 OLEUI-ERR-LOADSTRING            VALUE 115.

      *> Typedefs

      *> Class Identifier (CLSID)
       01 IID         typedef.
          03 IID-Data1            PIC xxxx COMP-5.
          03 IID-Data2            PIC xx COMP-5.
          03 IID-Data3            PIC xx COMP-5.
          03 IID-Data4            PIC x OCCURS 8.

       01 CLSID         IID typedef.
       01 GUID          IID typedef.
       01 REFIID        IID typedef.

      *> FormatEtc
       01 FORMATETC     typedef.
          03 FORMATETC-cfFormat     CLIPFORMAT.
          03                        WORD.       *> Filler bytes
          03 FORMATETC-ptd          Pointer.
          03 FORMATETC-dwAspect     DWORD.
          03 FORMATETC-lindex       LONG.
          03 FORMATETC-tymed        DWORD.

      *> StgMedium
       01 STGMEDIUM     typedef.
          03 STGMEDIUM-tymed           PIC xxxx COMP-5.
          03 STGMEDIUM-HandleType.
             05 STGMEDIUM-hBitmap       PIC xxxx COMP-5.
             05 STGMEDIUM-hMetaFilePict
                REDEFINES STGMEDIUM-hBitmap PIC xxxx COMP-5.
             05 STGMEDIUM-hEnhMetaFile
                REDEFINES STGMEDIUM-hBitmap PIC xxxx COMP-5.
             05 STGMEDIUM-hGlobal
                REDEFINES STGMEDIUM-hBitmap PIC xxxx COMP-5.
             05 STGMEDIUM-lpszFileName
                REDEFINES STGMEDIUM-hBitmap PIC xxxx COMP-5.
             05 STGMEDIUM-lpIStream
                REDEFINES STGMEDIUM-hBitmap PIC xxxx COMP-5.
             05 STGMEDIUM-lpIStorage
                REDEFINES STGMEDIUM-hBitmap PIC xxxx COMP-5.
          03 STGMEDIUM-pUnkForRelease  PIC xxxx COMP-5.

       01 STATDATA  typedef.
          03 STATDATA-formatetc     FORMATETC.
          03 STATDATA-advf          DWORD.
          03 STATDATA-pAdvSink      Pointer.
          03 STATDATA-dwConnection  DWORD.

       01 LARGE-INTEGER             typedef.
          03 LowPart                DWORD.
          03 HighPart               LONG.

       01 ULARGE-INTEGER            typedef.
          03 LowPart                DWORD.
          03 HighPart               DWORD.

       01 FILETIME  typedef.
          03 FILETIME-dwLowDateTime     DWORD.
          03 FILETIME-dwHighDateTime    DWORD.

       01 STATSTG   typedef.
          03 STATSTG-pwcsName           Pointer.
          03 STATSTG-type               DWORD.
          03 STATSTG-cbSize             ULARGE-INTEGER.
          03 STATSTG-mTime              FILETIME.
          03 STATSTG-cTime              FILETIME.
          03 STATSTG-aTime              FILETIME.
          03 STATSTG-grfMode            DWORD.
          03 STATSTG-grfLocksSupported  DWORD.
          03 STATSTG-clsid              CLSID.
          03 STATSTG-grfStateBits       DWORD.
          03 STATSTG-dwStgFmt           DWORD.

      *> Stream seek modes
       78 STREAM-SEEK-SET         VALUE 0.
       78 STREAM-SEEK-CUR         VALUE 1.
       78 STREAM-SEEK-END         VALUE 2.

      *> Storage modes
       78 STGM-DIRECT             VALUE h"00000000".
       78 STGM-TRANSACTED         VALUE h"00010000".
       78 STGM-SIMPLE             VALUE h"08000000".
       78 STGM-READ               VALUE h"00000000".
       78 STGM-WRITE              VALUE h"00000001".
       78 STGM-READWRITE          VALUE h"00000002".
       78 STGM-SHARE-DENY-NONE    VALUE h"00000040".
       78 STGM-SHARE-DENY-READ    VALUE h"00000030".
       78 STGM-SHARE-DENY-WRITE   VALUE h"00000020".
       78 STGM-SHARE-EXCLUSIVE    VALUE h"00000010".
       78 STGM-PRIORITY           VALUE h"00040000".
       78 STGM-DELETEONRELEASE    VALUE h"04000000".
       78 STGM-NOSCRATCH          VALUE h"00100000".
       78 STGM-CREATE             VALUE h"00001000".
       78 STGM-CONVERT            VALUE h"00020000".
       78 STGM-FAILIFTHERE        VALUE h"00000000".

       01 SNB                     typedef Pointer.

       78 STGC-DEFAULT            VALUE 0.
       78 STGC-OVERWRITE          VALUE 1.
       78 STGC-ONLYIFCURRENT      VALUE 2.
       78 STGC-DANGEROUSLYCOMMIT  VALUE 4.

      *> Object Descriptor
       01 OLEXOBJDESC  typedef.
      *> Object's aspect
          03 OLEXOBJDESC-dwDrawAspect      PIC xxxx COMP-5.
      *> Object's extent (HIMETRIC units)
          03 OLEXOBJDESC-sizel.
             05 OLEXOBJDESC-sizel-cx       PIC s9(9) COMP-5.
             05 OLEXOBJDESC-sizel-cy       PIC s9(9) COMP-5.
      *> Offset from upper-left corner of object where drag was initiated
      *> (HIMETRIC units)
          03 OLEXOBJDESC-pointl.
             05 OLEXOBJDESC-pointl-x       PIC s9(9) COMP-5.
             05 OLEXOBJDESC-pointl-y       PIC s9(9) COMP-5.
      *> Object's status bits
          03 OLEXOBJDESC-dwStatus          PIC xxxx COMP-5.


      *> Automation,

       01 BSTR                  Pointer typedef.
       01 DISPID                LONG typedef.
       01 CURRENCYTYPE          typedef.
          03 CY-Lo              ULONG.
          03 CY-Hi              LONG.

       01 OLEBOOL               PIC s9(4) COMP-5 TYPEDEF.
       78 OLEFALSE              VALUE 0.
       78 OLETRUE               VALUE -1.

      *> VarTypes,

      *>   [V] - may appear in a VARIANT
      *>   [T] - may appear in a TYPEDESC
      *>   [P] - may appear in an OLE property set
      *>   [S] - may appear in a Safe Array

       78 VT-EMPTY           VALUE 0.       *> [V]   [P]     nothing
       78 VT-NULL            VALUE 1.       *> [V]           SQL style Null
       78 VT-I2              VALUE 2.       *> [V][T][P][S]  2 byte signed int
       78 VT-I4              VALUE 3.       *> [V][T][P][S]  4 byte signed int
       78 VT-R4              VALUE 4.       *> [V][T][P][S]  4 byte real
       78 VT-R8              VALUE 5.       *> [V][T][P][S]  8 byte real
       78 VT-CY              VALUE 6.       *> [V][T][P][S]  currency
       78 VT-DATE            VALUE 7.       *> [V][T][P][S]  date
       78 VT-BSTR            VALUE 8.       *> [V][T][P][S]  OLE Automation string
       78 VT-DISPATCH        VALUE 9.       *> [V][T]   [S]  IDispatch FAR*
       78 VT-ERROR           VALUE 10.      *> [V][T]   [S]  SCODE
       78 VT-BOOL            VALUE 11.      *> [V][T][P][S]  True=-1, False=0
       78 VT-VARIANT         VALUE 12.      *> [V][T][P][S]  VARIANT FAR*
       78 VT-UNKNOWN         VALUE 13.      *> [V][T]   [S]  IUnknown FAR*
       78 VT-I1              VALUE 16.      *>    [T]        signed char
       78 VT-UI1             VALUE 17.      *> [V][T]   [S]  unsigned char
       78 VT-UI2             VALUE 18.      *>    [T]        unsigned short
       78 VT-UI4             VALUE 19.      *>    [T]        unsigned short
       78 VT-I8              VALUE 20.      *>    [T][P]     signed 64-bit int
       78 VT-UI8             VALUE 21.      *>    [T]        unsigned 64-bit int
       78 VT-INT             VALUE 22.      *>    [T]        signed machine int
       78 VT-UINT            VALUE 23.      *>    [T]        unsigned machine int
       78 VT-VOID            VALUE 24.      *>    [T]        C style void
       78 VT-HRESULT         VALUE 25.      *>    [T]
       78 VT-PTR             VALUE 26.      *>    [T]        pointer type
       78 VT-SAFEARRAY       VALUE 27.      *>    [T]        (use VT-ARRAY in VARIANT)
       78 VT-CARRAY          VALUE 28.      *>    [T]        C style array
       78 VT-USERDEFINED     VALUE 29.      *>    [T]        user defined type
       78 VT-LPSTR           VALUE 30.      *>    [T][P]     null terminated string
       78 VT-LPWSTR          VALUE 31.      *>    [T][P]     wide null terminated string
       78 VT-FILETIME        VALUE 64.      *>       [P]     FILETIME
       78 VT-BLOB            VALUE 65.      *>       [P]     Length prefixed bytes
       78 VT-STREAM          VALUE 66.      *>       [P]     Name of the stream follows
       78 VT-STORAGE         VALUE 67.      *>       [P]     Name of the storage follows
       78 VT-STREAMED-OBJECT VALUE 68.      *>       [P]     Stream contains an object
       78 VT-STORED-OBJECT   VALUE 69.      *>       [P]     Storage contains an object
       78 VT-BLOB-OBJECT     VALUE 70.      *>       [P]     Blob contains an object
       78 VT-CF              VALUE 71.      *>       [P]     Clipboard format
       78 VT-CLSID           VALUE 72.      *>       [P]     A Class ID
       78 VT-VECTOR          VALUE h"1000". *>       [P]     simple counted array
       78 VT-ARRAY           VALUE h"2000". *> [V]           SAFEARRAY*
       78 VT-BYREF           VALUE h"4000". *> [V]
       78 VT-RESERVED        VALUE h"8000".

       01 VARTYPE            USHORT typedef.

      *> VARIANTARG,
       01 VARIANTARG         typedef.
         03 VARIANTARG-vartype             VARTYPE.
         03 VARIANTARG-reserved1           PIC xx COMP-5.
         03 VARIANTARG-reserved2           PIC xx COMP-5.
         03 VARIANTARG-reserved3           PIC xx COMP-5.
            copy "vartypes.cpy" replacing ==()== by ==VARIANTARG==.

       01 VARIANT            typedef.
         03 VARIANT-vartype             VARTYPE.
         03 VARIANT-reserved1           PIC xx COMP-5.
         03 VARIANT-reserved2           PIC xx COMP-5.
         03 VARIANT-reserved3           PIC xx COMP-5.
            copy "vartypes.cpy" replacing ==()== by ==VARIANT==.

      *> COBOL variant (much simpler version of OLE VARIANT)
       01 COBVAR       typedef.
          03 COBVAR-type          PIC xxxxxxxx.
          03 COBVAR-Integer   Redefines COBVAR-type PIC s9(9) comp-5.
          03 COBVAR-String    Redefines COBVAR-type Object Reference.
          03 COBVAR-Single    Redefines COBVAR-type COMP-1.
          03 COBVAR-Double    Redefines COBVAR-type COMP-2.
          03 COBVAR-Object    Redefines COBVAR-type Object Reference.
          03 COBVAR-Pic-X-Ptr Redefines COBVAR-type Pointer.
          03 COBVAR-Font      Redefines COBVAR-type Object Reference.
          03 COBVAR-Picture   Redefines COBVAR-type Object Reference.
          03 COBVAR-Variant   Redefines COBVAR-type Object Reference.

      *> mapping values for manipulating variants to other types
       78 MAP-NONE                      VALUE 0. *> no mapping
      *>                            map to 4 byte comp-5
       78 MAP-SIGNED-INTEGER            VALUE 1.
      *>                            Map to character array string object
       78 MAP-STRING-OBJECT             VALUE 2.
      *>                            map to single precision floating point
       78 MAP-COMP-1                    VALUE 3.
      *>                            map to double precision floating point
       78 MAP-COMP-2                    VALUE 4.
      *>                            optional value
       78 MAP-OPTIONAL                  VALUE 5.
      *>                            COBOL object
       78 MAP-COBOL-OBJECT              VALUE 6.
      *>                            An OLE object (wrapping an IDispatch)
       78 MAP-OLE-OBJECT                VALUE 7.
      *>                            null-terminated pic x field
       78 MAP-PIC-X-PTR                 VALUE 8.
      *>                            COBOL class library OleFont object
       78 MAP-FONT-OBJECT               VALUE 9.
      *>                            COBOL class library OlePicture object
       78 MAP-PICTURE-OBJECT            VALUE 10.
      *>                            COBOL class library OleVariant object
       78 MAP-VARIANT                   VALUE 11.

       01 ARGUMENT           typedef.
          03 ARGUMENT-fMapVariant       PIC xxxx COMP-5.
          03 ARGUMENT-union.
             05 ARGUMENT-VariantArg        VARIANTARG.
             05 ARGUMENT-CobVar COBVAR. *> Redefines ARGUMENT-VariantArg PIC x(8).

       01 DISPPARAMS         typedef.
      *> Pointer to VARIANTARG
         03 DISPPARAMS-rgvarg              POINTER.
      *> Pointer to array of DISPIDs.
         03 DISPPARAMS-rgdispidNamedArgs   POINTER.
         03 DISPPARAMS-cArgs               PIC xxxx COMP-5.
         03 DISPPARAMS-cNamedArgs          PIC xxxx COMP-5.

       01 SCODE             DWORD typedef.

       78 DISPATCH-METHOD                  VALUE h"01".
       78 DISPATCH-PROPERTYGET             VALUE h"02".
       78 DISPATCH-PROPERTYPUT             VALUE h"04".
       78 DISPATCH-PROPERTYPUTREF          VALUE h"08".

       01 EXCEPINFO         typedef.
         03 EXCEPINFO-wCode                USHORT.
         03 EXCEPINFO-wReserved            USHORT.
         03 EXCEPINFO-bstrSource           BSTR.
         03 EXCEPINFO-bstrDescription      BSTR.
         03 EXCEPINFO-bstrHelpFile         BSTR.
         03 EXCEPINFO-dwHelpContext        ULONG.
         03 EXCEPINFO-pvReserved           Pointer.
         03 EXCEPINFO-pfnDeferredFillIn    Procedure-Pointer.
         03 EXCEPINFO-scode                SCODE.

      *> locales
       78 LOCALE-SYSTEM-DEFAULT            VALUE 0.

       78 LOCALE-NOUSEROVERRIDE            VALUE h"80000000".

       78 VARIANT-NOVALUEPROP              VALUE 1.

      *> Type libraries/Type Info

       01 MEMBERID           LONG typedef.
       01 LCID               DWORD typedef.

       01 TYPEDESC           typedef.
          03 TYPEDESC-union.
             05 TYPEDESC-lptDesc        Pointer.
             05 TYPEDESC-lpaDesc    Redefines TYPEDESC-lptDesc Pointer.
             05 TYPEDESC-hRefType   Redefines TYPEDESC-lptDesc DWORD.
          03 TYPEDESC-varType       VARTYPE.
          03                        PIC xx COMP-5.

       01 IDLDESC            typedef.
          03 IDLDESC-bstrIDLInfo    BSTR.
          03 IDLDESC-wIDLFlags      USHORT.
          03                        PIC xx COMP-5.

       01 ELEMDESC           typedef.
          03 ELEMDESC-tDesc         TYPEDESC.
          03 ELEMDESC-idlDesc       IDLDESC.

       01 VARKIND            INT typedef.
       78 VAR-PERINSTANCE           VALUE 0.
       78 VAR-STATIC                VALUE 1.
       78 VAR-CONST                 VALUE 2.
       78 VAR-DISPATCH              VALUE 3.

      *> PARAM flags
       78 PARAMFLAG-NONE           VALUE 0.
       78 PARAMFLAG-FIN            VALUE 1.
       78 PARAMFLAG-FOUT           VALUE 2.
       78 PARAMFLAG-FLCID          VALUE 4.
       78 PARAMFLAG-FRETVAL        VALUE 8.
       78 PARAMFLAG-FOPT           VALUE 16.
       78 PARAMFLAG-FHASDEFAULT    VALUE 32.


      *> Var flags
       78 VARFLAG-FReadOnly          VALUE h"0001".
       78 VARFLAG-FSource            VALUE h"0002".
       78 VARFLAG-FBindable          VALUE h"0004".
       78 VARFLAG-FRequestEdit       VALUE h"0008".
       78 VARFLAG-FDisplayBind       VALUE h"0010".
       78 VARFLAG-FDefaultBind       VALUE h"0020".
       78 VARFLAG-FHidden            VALUE h"0040".
       78 VARFLAG-FRESTRICTED        value h"80".
       78 VARFLAG-FDEFAULTCOLLELEM   value h"100".
       78 VARFLAG-FUIDEFAULT         value h"200".
       78 VARFLAG-FNONBROWSABLE      value h"400".
       78 VARFLAG-FREPLACEABLE       value h"800".
       78 VARFLAG-FIMMEDIATEBIND     value h"1000".

       01 VARDESC            typedef.
          03 VARDESC-memid               MEMBERID.
          03 VARDESC-lpstrSchema         Pointer.
          03 VARDESC-union.
             05 VARDESC-oInst            ULONG.
             05 VARDESC-lpvarValue Redefines VARDESC-oInst Pointer.
          03 VARDESC-elemdescVar         ELEMDESC.
          03 VARDESC-wVarFlags           USHORT.
          03                             PIC xx COMP-5.
          03 VARDESC-varKind             VARKIND.

       01 FUNCKIND              INT typedef.
       78 FUNC-VIRTUAL          VALUE 0.
       78 FUNC-PUREVIRTUAL      VALUE 1.
       78 FUNC-NONVIRTUAL       VALUE 2.
       78 FUNC-STATIC           VALUE 3.
       78 FUNC-DISPATCH         VALUE 4.

       01 INVOKEKIND            INT typedef.
       78 INVOKE-FUNC           VALUE 1.
       78 INVOKE-PROPERTYGET    VALUE 2.
       78 INVOKE-PROPERTYPUT    VALUE 4.
       78 INVOKE-PROPERTYPUTREF VALUE 8.

       01 CALLCONV              INT typedef.

       01 FUNCDESC              typedef.
          03 FUNCDESC-memid              MEMBERID.
          03 FUNCDESC-lprgscode          Pointer.
          03 FUNCDESC-lprgelemdescParam  Pointer.
          03 FUNCDESC-funcKind           FUNCKIND.
          03 FUNCDESC-invKind            INVOKEKIND.
          03 FUNCDESC-callConv           CALLCONV.
          03 FUNCDESC-cParams            SHORT.
          03 FUNCDESC-cParamsOpt         SHORT.
          03 FUNCDESC-oVft               SHORT.
          03 FUNCDESC-cScodes            SHORT.
          03 FUNCDESC-elemdescFunc       ELEMDESC.
          03 FUNCDESC-wFuncFlags         USHORT.

      *> FUNCFLAGS
       78 FUNCFLAG-FRESTRICTED        value h"01".
       78 FUNCFLAG-FSOURCE            value h"02".
       78 FUNCFLAG-FBINDABLE          value h"04".
       78 FUNCFLAG-FREQUESTEDIT       value h"08".
       78 FUNCFLAG-FDISPLAYBIND       value h"10".
       78 FUNCFLAG-FDEFAULTBIND       value h"20".
       78 FUNCFLAG-FHIDDEN            value h"40".
       78 FUNCFLAG-FUSESGETLASTERROR  value h"80".
       78 FUNCFLAG-FDEFAULTCOLLELEM   value h"100".
       78 FUNCFLAG-FUIDEFAULT         value h"200".
       78 FUNCFLAG-FNONBROWSABLE      value h"400".
       78 FUNCFLAG-FREPLACEABLE       value h"800".
       78 FUNCFLAG-FIMMEDIATEBIND     value h"1000".

       01 TYPEKIND              INT typedef.
       78 TKIND-ENUM            VALUE 0.
       78 TKIND-RECORD          VALUE 1.
       78 TKIND-MODULE          VALUE 2.
       78 TKIND-INTERFACE       VALUE 3.
       78 TKIND-DISPATCH        VALUE 4.
       78 TKIND-COCLASS         VALUE 5.
       78 TKIND-ALIAS           VALUE 6.
       78 TKIND-UNION           VALUE 7.
       78 TKIND-MAX             VALUE 8.

       01 TYPEATTR              typedef.
          03 TYPEATTR-guid                GUID.
          03 TYPEATTR-lcid                LCID.
          03 TYPEATTR-dwReserved          DWORD.
          03 TYPEATTR-memidConstructor    MEMBERID.
          03 TYPEATTR-memidDestructor     MEMBERID.
          03 TYPEATTR-lpstrSchema         Pointer.
          03 TYPEATTR-cbSizeInstance      ULONG.
          03 TYPEATTR-typekind            TYPEKIND.
          03 TYPEATTR-cFuncs              USHORT.
          03 TYPEATTR-cVars               USHORT.
          03 TYPEATTR-cImplTypes          USHORT.
          03 TYPEATTR-cbSizeVft           USHORT.
          03 TYPEATTR-cbAlignment         USHORT.
          03 TYPEATTR-wTypeFlags          USHORT.
          03 TYPEATTR-wMajorVerNum        USHORT.
          03 TYPEATTR-wMinorVerNum        USHORT.
          03 TYPEATTR-tdescAlias          TYPEDESC.
          03 TYPEATTR-idldescType         IDLDESC.

      *> Impl Type Flags

          78 IMPLTYPEFLAG-FDEFAULT        Value h"01".
          78 IMPLTYPEFLAG-FSOURCE         Value h"02".
          78 IMPLTYPEFLAG-FRESTRICTED     Value h"04".
          78 IMPLTYPEFLAG-FDEFAULTVTABLE  Value h"08".

      *> Property pages
       01 CALPOLESTR              typedef.
          03 CALPOLESTR-cElems    ULONG.
          03 CALPOLESTR-pElems    Pointer OCCURS 16.

       01 CADWORD                 typedef.
          03 CADWORD-cElems       ULONG.
          03 CADWORD-pElems       Pointer.

       01 OOPSPARAMTYPEINFO       typedef.
          03 numberofParameters     PIC xxxx COMP-X.
          03 aParameter OCCURS 1 To 65535
                                    Depending On numberOfParameters.
             05 paramSize           PIC xxxx COMP-X.
             05 paramConvention     PIC x COMP-X.
             05 OPTI-paramType           PIC x COMP-X.
             05 paramDescription    PIC xx COMP-X.

      *> Custom Controls (OCX)

      $if WM-USER not defined
       78 WM-USER                        VALUE h"0400".
       78 WM-COMMAND                     value h"0111".
       78 WM-CTLCOLORBTN                 value h"0135".
       78 WM-CTLCOLOREDIT                value h"0133".
       78 WM-CTLCOLORDLG                 value h"0136".
       78 WM-CTLCOLORLISTBOX             value h"0134".
       78 WM-CTLCOLORMSGBOX              value h"0132".
       78 WM-CTLCOLORSCROLLBAR           value h"0137".
       78 WM-CTLCOLORSTATIC              value h"0138".
       78 WM-DRAWITEM                    value h"002b".
       78 WM-MEASUREITEM                 value h"002c".
       78 WM-DELETEITEM                  value h"002d".
       78 WM-VKEYTOITEM                  value h"002e".
       78 WM-CHARTOITEM                  value h"002f".
       78 WM-COMPAREITEM                 value h"0039".
       78 WM-HSCROLL                     value h"0114".
       78 WM-VSCROLL                     value h"0115".
       78 WM-PARENTNOTIFY                value h"0210".
       78 WM-NOTIFY                      value h"004e".
      $end

       78 WM-MDISETMENU                  value h"0230".

       78 OCM-BASE                       VALUE WM-USER + h"1c00".

       78 OCM-COMMAND             VALUE OCM-BASE + WM-COMMAND.
       78 OCM-CTLCOLORBTN         VALUE OCM-BASE + WM-CTLCOLORBTN.
       78 OCM-CTLCOLOREDIT        VALUE OCM-BASE + WM-CTLCOLOREDIT.
       78 OCM-CTLCOLORDLG         VALUE OCM-BASE + WM-CTLCOLORDLG.
       78 OCM-CTLCOLORLISTBOX     VALUE OCM-BASE + WM-CTLCOLORLISTBOX.
       78 OCM-CTLCOLORMSGBOX      VALUE OCM-BASE + WM-CTLCOLORMSGBOX.
       78 OCM-CTLCOLORSCROLLBAR   VALUE OCM-BASE + WM-CTLCOLORSCROLLBAR.
       78 OCM-CTLCOLORSTATIC      VALUE OCM-BASE + WM-CTLCOLORSTATIC.
       78 OCM-DRAWITEM            VALUE OCM-BASE + WM-DRAWITEM.
       78 OCM-MEASUREITEM         VALUE OCM-BASE + WM-MEASUREITEM.
       78 OCM-DELETEITEM          VALUE OCM-BASE + WM-DELETEITEM.
       78 OCM-VKEYTOITEM          VALUE OCM-BASE + WM-VKEYTOITEM.
       78 OCM-CHARTOITEM          VALUE OCM-BASE + WM-CHARTOITEM.
       78 OCM-COMPAREITEM         VALUE OCM-BASE + WM-COMPAREITEM.
       78 OCM-HSCROLL             VALUE OCM-BASE + WM-HSCROLL.
       78 OCM-VSCROLL             VALUE OCM-BASE + WM-VSCROLL.
       78 OCM-PARENTNOTIFY        VALUE OCM-BASE + WM-PARENTNOTIFY.
       78 OCM-NOTIFY              VALUE OCM-BASE + WM-NOTIFY.

       01 OLE-COLOR                      typedef.
         03 OLE-COLOR-RED               PIC x COMP-5.
         03 OLE-COLOR-SYSTEM-INDEX Redefines OLE-COLOR-RED PIC x COMP-5.
         03 OLE-COLOR-GREEN             PIC x COMP-5.
         03 OLE-COLOR-BLUE              PIC x COMP-5.
         03 OLE-COLOR-USAGE             PIC x COMP-5.
       78 OLE-COLOR-USAGE-SYSTEM         VALUE h"80".

       78 OLE-COLOR-UNDEFINED            VALUE x"FFFFFFFF".
       01 OLE-XPOS-PIXELS                LONG typedef.
       01 OLE-YPOS-PIXELS                LONG typedef.
       01 OLE-XSIZE-PIXELS               LONG typedef.
       01 OLE-YSIZE-PIXELS               LONG typedef.
       01 OLE-XPOS-HIMETRIC              LONG typedef.
       01 OLE-YPOS-HIMETRIC              LONG typedef.
       01 OLE-XSIZE-HIMETRIC             LONG typedef.
       01 OLE-YSIZE-HIMETRIC             LONG typedef.
       01 OLE-HANDLE                     UINT typedef.


      *> Data binding
       78 ONREQUESTEDIT                  VALUE 0.
       78 ONCHANGED                      VALUE 1.

      *> Custom property types
       78 TYPE-OLE                       VALUE 0.
       78 TYPE-COLOR                     VALUE 1.
       78 TYPE-FONT                      VALUE 2.
       78 TYPE-PICTURE                   VALUE 3.
       78 TYPE-INTEGER                   VALUE 4.
       78 TYPE-STRING                    VALUE 5.
       78 TYPE-BOOL                      VALUE 6.
       78 TYPE-ENUMERATION               VALUE 7.
       78 TYPE-UNDEFINED                 VALUE 99.
      *> BIT setting.
       78 TYPE-READONLY                  VALUE h"8000".

      *> OCX VarTypes VALUE (just aliases for others)
       78 VT-COLOR                       VALUE VT-I4.
       78 VT-XPOS-PIXELS                 VALUE VT-I4.
       78 VT-YPOS-PIXELS                 VALUE VT-I4.
       78 VT-XSIZE-PIXELS                VALUE VT-I4.
       78 VT-YSIZE-PIXELS                VALUE VT-I4.
       78 VT-XPOS-HIMETRIC               VALUE VT-I4.
       78 VT-YPOS-HIMETRIC               VALUE VT-I4.
       78 VT-XSIZE-HIMETRIC              VALUE VT-I4.
       78 VT-YSIZE-HIMETRIC              VALUE VT-I4.
       78 VT-TRISTATE                    VALUE VT-I2.
       78 VT-OPTEXCLUSIVE                VALUE VT-BOOL.
       78 VT-FONT                        VALUE VT-DISPATCH.
       78 VT-PICTURE                     VALUE VT-DISPATCH.

      *> Stock property DISPIDs
       78 DISPID-AUTOSIZE                VALUE -500.
       78 DISPID-BACKCOLOR               VALUE -501.
       78 DISPID-BACKSTYLE               VALUE -502.
       78 DISPID-BORDERCOLOR             VALUE -503.
       78 DISPID-BORDERSTYLE             VALUE -504.
       78 DISPID-BORDERWIDTH             VALUE -505.
       78 DISPID-DRAWMODE                VALUE -507.
       78 DISPID-DRAWSTYLE               VALUE -508.
       78 DISPID-DRAWWIDTH               VALUE -509.
       78 DISPID-FILLCOLOR               VALUE -510.
       78 DISPID-FILLSTYLE               VALUE -511.
       78 DISPID-FONT                    VALUE -512.
       78 DISPID-FORECOLOR               VALUE -513.
       78 DISPID-ENABLED                 VALUE -514.
       78 DISPID-HWND                    VALUE -515.
       78 DISPID-TABSTOP                 VALUE -516.
       78 DISPID-TEXT                    VALUE -517.
       78 DISPID-CAPTION                 VALUE -518.
       78 DISPID-BORDERVISIBLE           VALUE -519.
       78 DISPID-APPEARANCE              VALUE -520.
       78 DISPID-MOUSEPOINTER            VALUE -521.
       78 DISPID-MOUSEICON               VALUE -522.
       78 DISPID-PICTURE                 VALUE -523.
       78 DISPID-VALID                   VALUE -524.
       78 DISPID-READYSTATE              VALUE -525.

      *> Container Ambient properties DISPIDs
       78 DISPID-AMBNT-BACKCOLOR         VALUE -701.
       78 DISPID-AMBNT-DISPLAYNAME       VALUE -702.
       78 DISPID-AMBNT-FONT              VALUE -703.
       78 DISPID-AMBNT-FORECOLOR         VALUE -704.
       78 DISPID-AMBNT-LOCALEID          VALUE -705.
       78 DISPID-AMBNT-MESSAGEREFLECT    VALUE -706.
       78 DISPID-AMBNT-SCALEUNITS        VALUE -707.
       78 DISPID-AMBNT-TEXTALIGN         VALUE -708.
       78 DISPID-AMBNT-USERMODE          VALUE -709.
       78 DISPID-AMBNT-UIDEAD            VALUE -710.
       78 DISPID-AMBNT-SHOWGRABHANDLES   VALUE -711.
       78 DISPID-AMBNT-SHOWHATCHING      VALUE -712.
       78 DISPID-AMBNT-DISPLAYASDEFAULT  VALUE -713.
       78 DISPID-AMBNT-SUPPORTSMNEMONICS VALUE -714.
       78 DISPID-AMBNT-AUTOCLIP          VALUE -715.
       78 DISPID-AMBNT-PALETTE           VALUE -726.
       78 DISPID-AMBNT-TRANSFERPRIORITY  VALUE -728.
       78 DISPID-AMBNT-RIGHTTOLEFT       VALUE -732.
       78 DISPID-AMBNT-TOPTOBOTTOM       VALUE -733.

       78 DISPID-PROPERTYPUT             VALUE -3.

      *> Stock event DISPIDs

       78 DISPID-CLICK                   VALUE -600.
       78 DISPID-DBLCLICK                VALUE -601.
       78 DISPID-KEYDOWN                 VALUE -602.
       78 DISPID-KEYPRESS                VALUE -603.
       78 DISPID-KEYUP                   VALUE -604.
       78 DISPID-MOUSEDOWN               VALUE -605.
       78 DISPID-MOUSEMOVE               VALUE -606.
       78 DISPID-MOUSEUP                 VALUE -607.
       78 DISPID-ERROREVENT              VALUE -608.
       78 DISPID-READYSTATECHANGE        VALUE -609.
       78 DISPID-CLICK-VALUE             VALUE -610.
       78 DISPID-RIGHTTOLEFT             VALUE -611.
       78 DISPID-TOPTOBOTTOM             VALUE -612.

      *> Stock method DISPIDs

       78 DISPID-REFRESH                 VALUE -550.
       78 DISPID-DOCLICK                 VALUE -551.
       78 DISPID-ABOUTBOX                VALUE -552.

       78 DISPID-UNKNOWN                 VALUE -1.

       78 STOCKFONT-BIT                  VALUE h"0001".
       78 STOCKFORECOLOR-BIT             VALUE h"0002".
       78 STOCKBACKCOLOR-BIT             VALUE h"0004".
       78 STOCKCAPTION-BIT               VALUE h"0008".

      *> ReadyState enumeration
       78 READYSTATE-UNINITIALIZED       VALUE 0.
       78 READYSTATE-LOADING             VALUE 1.
       78 READYSTATE-LOADED              VALUE 2.
       78 READYSTATE-INTERACTIVE         VALUE 3.
       78 READYSTATE-COMPLETE            VALUE 4.

      *> Stock property pages
      *> {A511DFE0-7B3C-11cf-98F0-0020AF1F923F} = Font
      *> {A511DFE1-7B3C-11cf-98F0-0020AF1F923F} = Color
      *> {A511DFE2-7B3C-11cf-98F0-0020AF1F923F} = Picture

       78 FontPropertyPageCLSIDString    VALUE
                              "{A511DFE0-7B3C-11cf-98F0-0020AF1F923F}".
       78 ColorPropertyPageCLSIDString   VALUE
                              "{A511DFE1-7B3C-11cf-98F0-0020AF1F923F}".
       78 PicturePropertyPageCLSIDString VALUE
                              "{A511DFE2-7B3C-11cf-98F0-0020AF1F923F}".
       78 FontPropertyPageCLSID
                              VALUE x"E0DF11A53C7Bcf1198F00020AF1F923F".
       78 ColorPropertyPageCLSID
                              VALUE x"E1DF11A53C7Bcf1198F00020AF1F923F".
       78 PicturePropertyPageCLSID
                              VALUE x"E2DF11A53C7Bcf1198F00020AF1F923F".

       78 FONTPROPERTYPAGE-RESID       VALUE 405.
       78 COLORPROPERTYPAGE-RESID      VALUE 406.
       78 PICTUREPROPERTYPAGE-RESID    VALUE 407.

       78 PROPPAGESTATUS-DIRTY         VALUE 1.
       78 PROPPAGESTATUS-VALIDATE      VALUE 2.

       01 PAGEINFO          typedef.
          03 PAGEINFO-cb                UINT.
          03 PAGEINFO-pszTitle          Pointer.
          03 PAGEINFO-size              SIZEL.
          03 PAGEINFO-pszDocString      Pointer.
          03 PAGEINFO-pszHelpFile       Pointer.
          03 PAGEINFO-dwHelpContext     DWORD.

      *> Ole fonts
       01 TEXTMETRICOLE     typedef.
          03 tmheight            LONG.
          03 tmascent            LONG.
          03 tmdescent           LONG.
          03 tminternalleading   LONG.
          03 tmexternalleading   LONG.
          03 tmavecharwidth      LONG.
          03 tmmaxcharwidth      LONG.
          03 tmweight            LONG.
          03 tmoverhang          LONG.
          03 tmdigitizedaspectx  LONG.
          03 tmdigitizedaspecty  LONG.
          03 tmfirstchar         BYTE.
          03 tmlastchar          BYTE.
          03 tmdefaultchar       BYTE.
          03 tmbreakchar         BYTE.
          03 tmitalic            BYTE.
          03 tmunderlined        BYTE.
          03 tmstruckout         BYTE.
          03 tmpitchandfamily    BYTE.
          03 tmcharset           BYTE.

      *> Picture Descriptor
       01 PICTDESC          typedef.
          03 PICTDESC-cbSize            PIC xxxx COMP-5.
          03 PICTDESC-picType           PIC s9( 9) COMP-5.
          03 PICTDESC-metafile.
             05 PICTDESC-hMetaFile      PIC xxxx COMP-5.
             05 PICTDESC-xExt           PIC xxxx COMP-5.
             05 PICTDESC-yExt           PIC xxxx COMP-5.
          03 PICTDESC-bitmap REDEFINES PICTDESC-metaFile.
             05 PICTDESC-hBitmap        PIC xxxx COMP-5.
             05 PICTDESC-hPal           PIC xxxx COMP-5.
          03 PICTDESC-icon  REDEFINES PICTDESC-metaFile.
             05 PICTDESC-hIcon          PIC xxxx COMP-5.

      *> Picture types
       78 PICTYPE-UNKNOWN               VALUE -1.
       78 PICTYPE-NONE                  VALUE 0.
       78 PICTYPE-BITMAP                VALUE 1.
       78 PICTYPE-METAFILE              VALUE 2.
       78 PICTYPE-ICON                  VALUE 3.

      *> IIDs used by the class library
       78 IID-Null         VALUE x"00000000000000000000000000000000".
       78 IID-IUnknown     VALUE x"0000000000000000C000000000000046".
       78 IID-IClassFactory
                           VALUE x"0100000000000000C000000000000046".
       78 IID-IPicture     VALUE x"8009F87B32BF1A108BBB00AA00300CAB".
       78 IID-IPictureDisp VALUE x"8109F87B32BF1A108BBB00AA00300CAB".
       78 IID-ITypeLib     VALUE x"0204020000000000C000000000000046".
       78 IID-ITypeInfo    VALUE x"0104020000000000C000000000000046".
       78 IID-IDispatch    VALUE x"0004020000000000C000000000000046".
       78 IID-IProvideClassInfo
                           VALUE x"83B296B1B4BA1A10B69C00AA00341D07".
       78 IID-IProvideClassInfo2
                           Value x"C03ABCA6AADBCE119DE300AA004BB851".
       78 IID-IPerPropertyBrowsing
                           VALUE x"AAD36B3745381B1084ED08002B2EC713".

       78 IID-IFont        VALUE x"02E0F6BE74A81A108BBA00AA00300CAB".
       78 IID-IFontDisp    VALUE x"03E0F6BE74A81A108BBA00AA00300CAB".
       78 IID-IPropertyPage
                           VALUE x"8DB296B1B4BA1110B69C00AA00341D07".

       78 IID-IPropertyPage2
                           VALUE x"6546E401AC241B1084ED08002B2EC713".

       78 IID-IOleWindow   VALUE x"1401000000000000C000000000000046".
       78 IID-IOleInPlaceSite
                           VALUE x"1901000000000000C000000000000046".
       78 IID-IOleObject   VALUE x"1201000000000000C000000000000046".
       78 IID-IOleInPlaceUIWindow
                           VALUE x"1501000000000000C000000000000046".
       78 IID-IOleInPlaceFrame
                           VALUE x"1601000000000000C000000000000046".
       78 IID-IDataObject  VALUE x"0E01000000000000C000000000000046".
       78 IID-IStream      VALUE x"0C00000000000000C000000000000046".
       78 IID-IStorage     VALUE x"0B00000000000000C000000000000046".
       78 IID-IPersistStream VALUE x"0901000000000000C000000000000046".
       78 IID-IPersistStorage VALUE x"0a01000000000000C000000000000046".

       78 IID-IPersistStreamInit
                           VALUE x"8023D57F074E1B10AE2D08002B2EC713".
       78 IID-IOleContainer VALUE x"1B01000000000000C000000000000046".
       78 IID-IPropertyBag VALUE x"002A2755CB42CE11813500AA004BB851".
       78 IID-IPersistPropertyBag
                           VALUE x"604FD837CB42CE11813500AA004BB851".

       78 IID-ISpecifyPropertyPages
                           VALUE x"8BB296B1B4BA1A10B69C00AA00341D07".
       78 IID-IMoniker
                           VALUE x"0F00000000000000C000000000000046".
       78 IID-IDispatchEx  VALUE x"6098efa620c7d011933700a0c90dcaa9".

      *> OCX standard type GUIDs
        78 GUID-HIMETRIC   VALUE x"004350660FBE1A108BBB00AA00300CAB".
        78 GUID-COLOR      VALUE x"014350660FBE1A108BBB00AA00300CAB".
        78 GUID-XPOSPIXEL  VALUE x"024350660FBE1A108BBB00AA00300CAB".
        78 GUID-YPOSPIXEL  VALUE x"034350660FBE1A108BBB00AA00300CAB".
        78 GUID-XSIZEPIXEL VALUE x"044350660FBE1A108BBB00AA00300CAB".
        78 GUID-YSIZEPIXEL VALUE x"054350660FBE1A108BBB00AA00300CAB".
        78 GUID-XPOS       VALUE x"064350660FBE1A108BBB00AA00300CAB".
        78 GUID-YPOS       VALUE x"074350660FBE1A108BBB00AA00300CAB".
        78 GUID-XSIZE      VALUE x"084350660FBE1A108BBB00AA00300CAB".
        78 GUID-YSIZE      VALUE x"094350660FBE1A108BBB00AA00300CAB".
        78 GUID-TRISTATE   VALUE x"0A4350660FBE1A108BBB00AA00300CAB".
        78 GUID-OPTIONVALUEEXCLUSIVE
                           VALUE x"0B4350660FBE1A108BBB00AA00300CAB".
        78 GUID-CHECKVALUEEXCLUSIVE
                           VALUE x"0C4350660FBE1A108BBB00AA00300CAB".
        78 GUID-FONTNAME   VALUE x"0D4350660FBE1A108BBB00AA00300CAB".
        78 GUID-FONTSIZE   VALUE x"0E4350660FBE1A108BBB00AA00300CAB".
        78 GUID-FONTBOLD   VALUE x"0F4350660FBE1A108BBB00AA00300CAB".
        78 GUID-FONTITALIC VALUE x"104350660FBE1A108BBB00AA00300CAB".
        78 GUID-FONTUNDERSCORE
                           VALUE x"114350660FBE1A108BBB00AA00300CAB".
        78 GUID-FONTSTRIKETHROUGH
                           VALUE x"124350660FBE1A108BBB00AA00300CAB".
        78 GUID-HANDLE     VALUE x"134350660FBE1A108BBB00AA00300CAB".

      *> Standard OLE objects
        78 CLSID-StdFont   VALUE x"0352e30b918fce119de300aa004bb851".
        78 CLSID-StdPicture
                           VALUE x"0452e30b918fce119de300aa004bb851".

      *> APIs
       78 OleSetMenuDescriptor        VALUE "OleSetMenuDescriptor".
       78 OleCreateMenuDescriptor     VALUE "OleCreateMenuDescriptor".
       78 OleDestroyMenuDescriptor    VALUE "OleDestroyMenuDescriptor".
       78 OleTranslateColor           VALUE "OleTranslateColor".

       01 OLEINPLACEFRAMEINFO       typedef.
          03 OLEIPFI-cb            UINT.
          03 OLEIPFI-fMDIApp       BOOL.
          03 OLEIPFI-hwndFrame     HWND.
          03 OLEIPFI-haccel        HANDLE.
          03 OLEIPFI-cAccelEntries UINT.

       01 OLEMENUGROUPWIDTHS        typedef.
          03 OLEMGW-width          LONG OCCURS 6.

       01 BORDERWIDTHS              RECT typedef.

      *> OLE Documents
       01 DOCOBJECTINFO typedef.
          03 DOI-MiscStatus             DWORD.
          03 DOI-DefaultExtension       Pointer.
          03 DOI-DefaultExtensionString Pointer.
          03 DOI-Printable              DWORD.

      *> Object registration block
       01 OBJECTREGISTRATIONINFO typedef.
          03 ORI-dwSize           DWORD.    *> Size of block
          03 ORI-LibName          Pointer.  *> String
          03 ORI-ObjName          Pointer.  *> String
          03 ORI-ControlName      Pointer.  *> String
          03 ORI-ObjectClsId      Pointer.  *> String CLSID
          03 ORI-lVersion         LONG.
          03 ORI-dwMiscStatus     DWORD.
          03 ORI-dwbitmapID       DWORD.
          03 ORI-TypeLibraryIID   Pointer.  *> String IID
          03 ORI-EventsIID        Pointer.  *> String IID
          03 ORI-DispatchIID      Pointer.  *> String IID
          03 ORI-PropertyPages    Pointer.  *> Property Pages
          03 ORI-AutomationClass  Pointer.  *> Class name
          03 ORI-DocObjectInfo    Pointer.  *> to DOCOBJECTINFO.
          03 ORI-dwSafetyFlags    DWORD.    *> See safety flags below
          03 ORI-dwFlags          DWORD.    *> See flags below

      *> Safety Flags (for ActiveX used in browsers)

      *> Object is safe to be used in script
       78 SAFE-FOR-UNTRUSTED-CALLER     Value h"00000001".
      *> Object is safe for initializing with any given data
       78 SAFE-FOR-UNTRUSTED-DATA       Value h"00000002".

      *> Flags for ActiveX control registration
       78 NOCONTROLKEY                  Value h"00000001".

       01 CONTROLINFO   typedef.
       03 CI-cb         ULONG.
       03 CI-hAccel     HANDLE.
       03 CI-cAccel     USHORT.
       03 CI-dwFlags    DWORD.

      *> BINDINFO
       01 BINDINFO      typedef.
       03 BI-cbSize       ULONG.
       03 BI-szExtraInfo  Pointer.
       03 BI-stgmedData   STGMEDIUM.
       03 BI-grfBindInfoF DWORD.
       03 BI-dwBindVerb   DWORD.
       03 BI-szCustomVerb Pointer.
       03 BI-cbStgmedData DWORD.

      *> Bind Flags

       78 BINDF-ASYNCHRONOUS              VALUE h"1".
       78 BINDF-ASYNCSTORAGE              VALUE h"2".
       78 BINDF-NOPROGRESSIVERENDERING    VALUE h"4".
       78 BINDF-OFFLINEOPERATION          VALUE h"8".
       78 BINDF-GETNEWESTVERSION          VALUE h"10".
       78 BINDF-NOWRITECACHE              VALUE h"20".
       78 BINDF-PULLDATA                  VALUE h"80".
       78 BINDF-IGNORESECURITYPROBLEM     VALUE h"100".
       78 BINDF-RESYNCHRONIZE             VALUE h"200".
       78 BINDF-HYPERLINK                 VALUE h"400".
       78 BINDF-INLINESGETNEWESTVERSION   VALUE h"10000000".
       78 BINDF-INLINESRESYNCHRONIZE      VALUE h"20000000".
       78 BINDF-CONTAINER-NOWRITECACHE    VALUE h"40000000".

      *> Errors/Status codes

      *> Catch-all general purpose OLE Error
       78 MFOLE-OLE-ERROR                     VALUE 52.
      *> OLE Error on object creation
       78 MFOLE-OLE-CREATE-ERROR              VALUE 53.

       78 S-OK                                VALUE h"00000000".
       78 S-FALSE                             VALUE h"00000001".
       78 E-UNEXPECTED                        VALUE h"8000FFFF".
      $if WIN16 not defined
       78 E-NOTIMPL                           VALUE h"80004001".
       78 E-OUTOFMEMORY                       VALUE h"8007000E".
       78 E-INVALIDARG                        VALUE h"80070057".
       78 E-NOINTERFACE                       VALUE h"80004002".
       78 E-POINTER                           VALUE h"80004003".
       78 E-HANDLE                            VALUE h"80070006".
       78 E-ABORT                             VALUE h"80004004".
       78 E-FAIL                              VALUE h"80004005".
       78 E-ACCESSDENIED                      VALUE h"80070005".
      $else
       78 E-NOTIMPL                           VALUE h"80000001".
       78 E-OUTOFMEMORY                       VALUE h"80000002".
       78 E-INVALIDARG                        VALUE h"80000003".
       78 E-NOINTERFACE                       VALUE h"80000004".
       78 E-POINTER                           VALUE h"80000005".
       78 E-HANDLE                            VALUE h"80000006".
       78 E-ABORT                             VALUE h"80000007".
       78 E-FAIL                              VALUE h"80000008".
       78 E-ACCESSDENIED                      VALUE h"80000009".
      $end
       78 CO-E-INIT-TLS                       VALUE h"80004006".
       78 CO-E-INIT-SHARED-ALLOCATOR          VALUE h"80004007".
       78 CO-E-INIT-MEMORY-ALLOCATOR          VALUE h"80004008".
       78 CO-E-INIT-CLASS-CACHE               VALUE h"80004009".
       78 CO-E-INIT-RPC-CHANNEL               VALUE h"8000400A".
       78 CO-E-INIT-TLS-SET-CHANNEL-CONT      VALUE h"8000400B".
       78 CO-E-INIT-TLS-CHANNEL-CONTROL       VALUE h"8000400C".
       78 CO-E-INIT-UNACCEPTED-USER-ALLO      VALUE h"8000400D".
       78 CO-E-INIT-SCM-MUTEX-EXISTS          VALUE h"8000400E".
       78 CO-E-INIT-SCM-FILE-MAPPING-EXI      VALUE h"8000400F".
       78 CO-E-INIT-SCM-MAP-VIEW-OF-FILE      VALUE h"80004010".
       78 CO-E-INIT-SCM-EXEC-FAILURE          VALUE h"80004011".
       78 CO-E-INIT-ONLY-SINGLE-THREADED      VALUE h"80004012".
       78 S-OK                                VALUE h"00000000".
       78 S-FALSE                             VALUE h"00000001".
       78 OLE-E-FIRST                         VALUE h"80040000".
       78 OLE-E-LAST                          VALUE h"800400FF".
       78 OLE-S-FIRST                         VALUE h"00040000".
       78 OLE-S-LAST                          VALUE h"000400FF".
       78 OLE-E-OLEVERB                       VALUE h"80040000".
       78 OLE-E-ADVF                          VALUE h"80040001".
       78 OLE-E-ENUM-NOMORE                   VALUE h"80040002".
       78 OLE-E-ADVISENOTSUPPORTED            VALUE h"80040003".
       78 OLE-E-NOCONNECTION                  VALUE h"80040004".
       78 OLE-E-NOTRUNNING                    VALUE h"80040005".
       78 OLE-E-NOCACHE                       VALUE h"80040006".
       78 OLE-E-BLANK                         VALUE h"80040007".
       78 OLE-E-CLASSDIFF                     VALUE h"80040008".
       78 OLE-E-CANT-GETMONIKER               VALUE h"80040009".
       78 OLE-E-CANT-BINDTOSOURCE             VALUE h"8004000A".
       78 OLE-E-STATIC                        VALUE h"8004000B".
       78 OLE-E-PROMPTSAVECANCELLED           VALUE h"8004000C".
       78 OLE-E-INVALIDRECT                   VALUE h"8004000D".
       78 OLE-E-WRONGCOMPOBJ                  VALUE h"8004000E".
       78 OLE-E-INVALIDHWND                   VALUE h"8004000F".
       78 OLE-E-NOT-INPLACEACTIVE             VALUE h"80040010".
       78 OLE-E-CANTCONVERT                   VALUE h"80040011".
       78 OLE-E-NOSTORAGE                     VALUE h"80040012".
       78 DV-E-FORMATETC                      VALUE h"80040064".
       78 DV-E-DVTARGETDEVICE                 VALUE h"80040065".
       78 DV-E-STGMEDIUM                      VALUE h"80040066".
       78 DV-E-STATDATA                       VALUE h"80040067".
       78 DV-E-LINDEX                         VALUE h"80040068".
       78 DV-E-TYMED                          VALUE h"80040069".
       78 DV-E-CLIPFORMAT                     VALUE h"8004006A".
       78 DV-E-DVASPECT                       VALUE h"8004006B".
       78 DV-E-DVTARGETDEVICE-SIZE            VALUE h"8004006C".
       78 DV-E-NOIVIEWOBJECT                  VALUE h"8004006D".
       78 DRAGDROP-E-FIRST                    VALUE h"80040100".
       78 DRAGDROP-E-LAST                     VALUE h"8004010F".
       78 DRAGDROP-S-FIRST                    VALUE h"00040100".
       78 DRAGDROP-S-LAST                     VALUE h"0004010F".
       78 DRAGDROP-E-NOTREGISTERED            VALUE h"80040100".
       78 DRAGDROP-E-ALREADYREGISTERED        VALUE h"80040101".
       78 DRAGDROP-E-INVALIDHWND              VALUE h"80040102".
       78 CLASSFACTORY-E-FIRST                VALUE h"80040110".
       78 CLASSFACTORY-E-LAST                 VALUE h"8004011F".
       78 CLASSFACTORY-S-FIRST                VALUE h"00040110".
       78 CLASSFACTORY-S-LAST                 VALUE h"0004011F".
       78 CLASS-E-NOAGGREGATION               VALUE h"80040110".
       78 CLASS-E-CLASSNOTAVAILABLE           VALUE h"80040111".
       78 CLASS-E-NOTLICENSED                 VALUE h"80040112".
       78 MARSHAL-E-FIRST                     VALUE h"80040120".
       78 MARSHAL-E-LAST                      VALUE h"8004012F".
       78 MARSHAL-S-FIRST                     VALUE h"00040120".
       78 MARSHAL-S-LAST                      VALUE h"0004012F".
       78 DATA-E-FIRST                        VALUE h"80040130".
       78 DATA-E-LAST                         VALUE h"8004013F".
       78 DATA-S-FIRST                        VALUE h"00040130".
       78 DATA-S-LAST                         VALUE h"0004013F".
       78 VIEW-E-FIRST                        VALUE h"80040140".
       78 VIEW-E-LAST                         VALUE h"8004014F".
       78 VIEW-S-FIRST                        VALUE h"00040140".
       78 VIEW-S-LAST                         VALUE h"0004014F".
       78 VIEW-E-DRAW                         VALUE h"80040140".
       78 REGDB-E-FIRST                       VALUE h"80040150".
       78 REGDB-E-LAST                        VALUE h"8004015F".
       78 REGDB-S-FIRST                       VALUE h"00040150".
       78 REGDB-S-LAST                        VALUE h"0004015F".
       78 REGDB-E-READREGDB                   VALUE h"80040150".
       78 REGDB-E-WRITEREGDB                  VALUE h"80040151".
       78 REGDB-E-KEYMISSING                  VALUE h"80040152".
       78 REGDB-E-INVALIDVALUE                VALUE h"80040153".
       78 REGDB-E-CLASSNOTREG                 VALUE h"80040154".
       78 REGDB-E-IIDNOTREG                   VALUE h"80040155".
       78 CACHE-E-FIRST                       VALUE h"80040170".
       78 CACHE-E-LAST                        VALUE h"8004017F".
       78 CACHE-S-FIRST                       VALUE h"00040170".
       78 CACHE-S-LAST                        VALUE h"0004017F".
       78 CACHE-E-NOCACHE-UPDATED             VALUE h"80040170".
       78 OLEOBJ-E-FIRST                      VALUE h"80040180".
       78 OLEOBJ-E-LAST                       VALUE h"8004018F".
       78 OLEOBJ-S-FIRST                      VALUE h"00040180".
       78 OLEOBJ-S-LAST                       VALUE h"0004018F".
       78 OLEOBJ-E-NOVERBS                    VALUE h"80040180".
       78 OLEOBJ-E-INVALIDVERB                VALUE h"80040181".
       78 CLIENTSITE-E-FIRST                  VALUE h"80040190".
       78 CLIENTSITE-E-LAST                   VALUE h"8004019F".
       78 CLIENTSITE-S-FIRST                  VALUE h"00040190".
       78 CLIENTSITE-S-LAST                   VALUE h"0004019F".
       78 INPLACE-E-NOTUNDOABLE               VALUE h"800401A0".
       78 INPLACE-E-NOTOOLSPACE               VALUE h"800401A1".
       78 INPLACE-E-FIRST                     VALUE h"800401A0".
       78 INPLACE-E-LAST                      VALUE h"800401AF".
       78 INPLACE-S-FIRST                     VALUE h"000401A0".
       78 INPLACE-S-LAST                      VALUE h"000401AF".
       78 ENUM-E-FIRST                        VALUE h"800401B0".
       78 ENUM-E-LAST                         VALUE h"800401BF".
       78 ENUM-S-FIRST                        VALUE h"000401B0".
       78 ENUM-S-LAST                         VALUE h"000401BF".
       78 CONVERT10-E-FIRST                   VALUE h"800401C0".
       78 CONVERT10-E-LAST                    VALUE h"800401CF".
       78 CONVERT10-S-FIRST                   VALUE h"000401C0".
       78 CONVERT10-S-LAST                    VALUE h"000401CF".
       78 CONVERT10-E-OLESTREAM-GET           VALUE h"800401C0".
       78 CONVERT10-E-OLESTREAM-PUT           VALUE h"800401C1".
       78 CONVERT10-E-OLESTREAM-FMT           VALUE h"800401C2".
       78 CONVERT10-E-OLESTREAM-BITMAP-T      VALUE h"800401C3".
       78 CONVERT10-E-STG-FMT                 VALUE h"800401C4".
       78 CONVERT10-E-STG-NO-STD-STREAM       VALUE h"800401C5".
       78 CONVERT10-E-STG-DIB-TO-BITMAP       VALUE h"800401C6".
       78 CLIPBRD-E-FIRST                     VALUE h"800401D0".
       78 CLIPBRD-E-LAST                      VALUE h"800401DF".
       78 CLIPBRD-S-FIRST                     VALUE h"000401D0".
       78 CLIPBRD-S-LAST                      VALUE h"000401DF".
       78 CLIPBRD-E-CANT-OPEN                 VALUE h"800401D0".
       78 CLIPBRD-E-CANT-EMPTY                VALUE h"800401D1".
       78 CLIPBRD-E-CANT-SET                  VALUE h"800401D2".
       78 CLIPBRD-E-BAD-DATA                  VALUE h"800401D3".
       78 CLIPBRD-E-CANT-CLOSE                VALUE h"800401D4".
       78 MK-E-FIRST                          VALUE h"800401E0".
       78 MK-E-LAST                           VALUE h"800401EF".
       78 MK-S-FIRST                          VALUE h"000401E0".
       78 MK-S-LAST                           VALUE h"000401EF".
       78 MK-E-CONNECTMANUALLY                VALUE h"800401E0".
       78 MK-E-EXCEEDEDDEADLINE               VALUE h"800401E1".
       78 MK-E-NEEDGENERIC                    VALUE h"800401E2".
       78 MK-E-UNAVAILABLE                    VALUE h"800401E3".
       78 MK-E-SYNTAX                         VALUE h"800401E4".
       78 MK-E-NOOBJECT                       VALUE h"800401E5".
       78 MK-E-INVALIDEXTENSION               VALUE h"800401E6".
       78 MK-E-INTERMEDIATEINTERFACENOTS      VALUE h"800401E7".
       78 MK-E-NOTBINDABLE                    VALUE h"800401E8".
       78 MK-E-NOTBOUND                       VALUE h"800401E9".
       78 MK-E-CANTOPENFILE                   VALUE h"800401EA".
       78 MK-E-MUSTBOTHERUSER                 VALUE h"800401EB".
       78 MK-E-NOINVERSE                      VALUE h"800401EC".
       78 MK-E-NOSTORAGE                      VALUE h"800401ED".
       78 MK-E-NOPREFIX                       VALUE h"800401EE".
       78 MK-E-ENUMERATION-FAILED             VALUE h"800401EF".
       78 CO-E-FIRST                          VALUE h"800401F0".
       78 CO-E-LAST                           VALUE h"800401FF".
       78 CO-S-FIRST                          VALUE h"000401F0".
       78 CO-S-LAST                           VALUE h"000401FF".
       78 CO-E-NOTINITIALIZED                 VALUE h"800401F0".
       78 CO-E-ALREADYINITIALIZED             VALUE h"800401F1".
       78 CO-E-CANTDETERMINECLASS             VALUE h"800401F2".
       78 CO-E-CLASSSTRING                    VALUE h"800401F3".
       78 CO-E-IIDSTRING                      VALUE h"800401F4".
       78 CO-E-APPNOTFOUND                    VALUE h"800401F5".
       78 CO-E-APPSINGLEUSE                   VALUE h"800401F6".
       78 CO-E-ERRORINAPP                     VALUE h"800401F7".
       78 CO-E-DLLNOTFOUND                    VALUE h"800401F8".
       78 CO-E-ERRORINDLL                     VALUE h"800401F9".
       78 CO-E-WRONGOSFORAPP                  VALUE h"800401FA".
       78 CO-E-OBJNOTREG                      VALUE h"800401FB".
       78 CO-E-OBJISREG                       VALUE h"800401FC".
       78 CO-E-OBJNOTCONNECTED                VALUE h"800401FD".
       78 CO-E-APPDIDNTREG                    VALUE h"800401FE".
       78 CO-E-RELEASED                       VALUE h"800401FF".
       78 OLE-S-USEREG                        VALUE h"00040000".
       78 OLE-S-STATIC                        VALUE h"00040001".
       78 OLE-S-MAC-CLIPFORMAT                VALUE h"00040002".
       78 DRAGDROP-S-DROP                     VALUE h"00040100".
       78 DRAGDROP-S-CANCEL                   VALUE h"00040101".
       78 DRAGDROP-S-USEDEFAULTCURSORS        VALUE h"00040102".
       78 DATA-S-SAMEFORMATETC                VALUE h"00040130".
       78 VIEW-S-ALREADY-FROZEN               VALUE h"00040140".
       78 CACHE-S-FORMATETC-NOTSUPPORTED      VALUE h"00040170".
       78 CACHE-S-SAMECACHE                   VALUE h"00040171".
       78 CACHE-S-SOMECACHES-NOTUPDATED       VALUE h"00040172".
       78 OLEOBJ-S-INVALIDVERB                VALUE h"00040180".
       78 OLEOBJ-S-CANNOT-DOVERB-NOW          VALUE h"00040181".
       78 OLEOBJ-S-INVALIDHWND                VALUE h"00040182".
       78 INPLACE-S-TRUNCATED                 VALUE h"000401A0".
       78 CONVERT10-S-NO-PRESENTATION         VALUE h"000401C0".
       78 MK-S-REDUCED-TO-SELF                VALUE h"000401E2".
       78 MK-S-ME                             VALUE h"000401E4".
       78 MK-S-HIM                            VALUE h"000401E5".
       78 MK-S-US                             VALUE h"000401E6".
       78 MK-S-MONIKERALREADYREGISTERED       VALUE h"000401E7".
       78 CO-E-CLASS-CREATE-FAILED            VALUE h"80080001".
       78 CO-E-SCM-ERROR                      VALUE h"80080002".
       78 CO-E-SCM-RPC-FAILURE                VALUE h"80080003".
       78 CO-E-BAD-PATH                       VALUE h"80080004".
       78 CO-E-SERVER-EXEC-FAILURE            VALUE h"80080005".
       78 CO-E-OBJSRV-RPC-FAILURE             VALUE h"80080006".
       78 MK-E-NO-NORMALIZED                  VALUE h"80080007".
       78 CO-E-SERVER-STOPPING                VALUE h"80080008".
       78 MEM-E-INVALID-ROOT                  VALUE h"80080009".
       78 MEM-E-INVALID-LINK                  VALUE h"80080010".
       78 MEM-E-INVALID-SIZE                  VALUE h"80080011".
       78 DISP-E-UNKNOWNINTERFACE             VALUE h"80020001".
       78 DISP-E-MEMBERNOTFOUND               VALUE h"80020003".
       78 DISP-E-PARAMNOTFOUND                VALUE h"80020004".
       78 DISP-E-TYPEMISMATCH                 VALUE h"80020005".
       78 DISP-E-UNKNOWNNAME                  VALUE h"80020006".
       78 DISP-E-NONAMEDARGS                  VALUE h"80020007".
       78 DISP-E-BADVARTYPE                   VALUE h"80020008".
       78 DISP-E-EXCEPTION                    VALUE h"80020009".
       78 DISP-E-OVERFLOW                     VALUE h"8002000A".
       78 DISP-E-BADINDEX                     VALUE h"8002000B".
       78 DISP-E-UNKNOWNLCID                  VALUE h"8002000C".
       78 DISP-E-ARRAYISLOCKED                VALUE h"8002000D".
       78 DISP-E-BADPARAMCOUNT                VALUE h"8002000E".
       78 DISP-E-PARAMNOTOPTIONAL             VALUE h"8002000F".
       78 DISP-E-BADCALLEE                    VALUE h"80020010".
       78 DISP-E-NOTACOLLECTION               VALUE h"80020011".
       78 TYPE-E-BUFFERTOOSMALL               VALUE h"80028016".
       78 TYPE-E-INVDATAREAD                  VALUE h"80028018".
       78 TYPE-E-UNSUPFORMAT                  VALUE h"80028019".
       78 TYPE-E-REGISTRYACCESS               VALUE h"8002801C".
       78 TYPE-E-LIBNOTREGISTERED             VALUE h"8002801D".
       78 TYPE-E-UNDEFINEDTYPE                VALUE h"80028027".
       78 TYPE-E-QUALIFIEDNAMEDISALLOWED      VALUE h"80028028".
       78 TYPE-E-INVALIDSTATE                 VALUE h"80028029".
       78 TYPE-E-WRONGTYPEKIND                VALUE h"8002802A".
       78 TYPE-E-ELEMENTNOTFOUND              VALUE h"8002802B".
       78 TYPE-E-AMBIGUOUSNAME                VALUE h"8002802C".
       78 TYPE-E-NAMECONFLICT                 VALUE h"8002802D".
       78 TYPE-E-UNKNOWNLCID                  VALUE h"8002802E".
       78 TYPE-E-DLLFUNCTIONNOTFOUND          VALUE h"8002802F".
       78 TYPE-E-BADMODULEKIND                VALUE h"800288BD".
       78 TYPE-E-SIZETOOBIG                   VALUE h"800288C5".
       78 TYPE-E-DUPLICATEID                  VALUE h"800288C6".
       78 TYPE-E-INVALIDID                    VALUE h"800288CF".
       78 TYPE-E-TYPEMISMATCH                 VALUE h"80028CA0".
       78 TYPE-E-OUTOFBOUNDS                  VALUE h"80028CA1".
       78 TYPE-E-IOERROR                      VALUE h"80028CA2".
       78 TYPE-E-CANTCREATETMPFILE            VALUE h"80028CA3".
       78 TYPE-E-CANTLOADLIBRARY              VALUE h"80029C4A".
       78 TYPE-E-INCONSISTENTPROPFUNCS        VALUE h"80029C83".
       78 TYPE-E-CIRCULARTYPE                 VALUE h"80029C84".
       78 STG-E-INVALIDFUNCTION               VALUE h"80030001".
       78 STG-E-FILENOTFOUND                  VALUE h"80030002".
       78 STG-E-PATHNOTFOUND                  VALUE h"80030003".
       78 STG-E-TOOMANYOPENFILES              VALUE h"80030004".
       78 STG-E-ACCESSDENIED                  VALUE h"80030005".
       78 STG-E-INVALIDHANDLE                 VALUE h"80030006".
       78 STG-E-INSUFFICIENTMEMORY            VALUE h"80030008".
       78 STG-E-INVALIDPOINTER                VALUE h"80030009".
       78 STG-E-NOMOREFILES                   VALUE h"80030012".
       78 STG-E-DISKISWRITEPROTECTED          VALUE h"80030013".
       78 STG-E-SEEKERROR                     VALUE h"80030019".
       78 STG-E-WRITEFAULT                    VALUE h"8003001D".
       78 STG-E-READFAULT                     VALUE h"8003001E".
       78 STG-E-SHAREVIOLATION                VALUE h"80030020".
       78 STG-E-LOCKVIOLATION                 VALUE h"80030021".
       78 STG-E-FILEALREADYEXISTS             VALUE h"80030050".
       78 STG-E-INVALIDPARAMETER              VALUE h"80030057".
       78 STG-E-MEDIUMFULL                    VALUE h"80030070".
       78 STG-E-ABNORMALAPIEXIT               VALUE h"800300FA".
       78 STG-E-INVALIDHEADER                 VALUE h"800300FB".
       78 STG-E-INVALIDNAME                   VALUE h"800300FC".
       78 STG-E-UNKNOWN                       VALUE h"800300FD".
       78 STG-E-UNIMPLEMENTEDFUNCTION         VALUE h"800300FE".
       78 STG-E-INVALIDFLAG                   VALUE h"800300FF".
       78 STG-E-INUSE                         VALUE h"80030100".
       78 STG-E-NOTCURRENT                    VALUE h"80030101".
       78 STG-E-REVERTED                      VALUE h"80030102".
       78 STG-E-CANTSAVE                      VALUE h"80030103".
       78 STG-E-OLDFORMAT                     VALUE h"80030104".
       78 STG-E-OLDDLL                        VALUE h"80030105".
       78 STG-E-SHAREREQUIRED                 VALUE h"80030106".
       78 STG-E-NOTFILEBASEDSTORAGE           VALUE h"80030107".
       78 STG-E-EXTANTMARSHALLINGS            VALUE h"80030108".
       78 STG-S-CONVERTED                     VALUE h"00030200".
       78 RPC-E-CALL-REJECTED                 VALUE h"80010001".
       78 RPC-E-CALL-CANCELED                 VALUE h"80010002".
       78 RPC-E-CANTPOST-INSENDCALL           VALUE h"80010003".
       78 RPC-E-CANTCALLOUT-INASYNCCALL       VALUE h"80010004".
       78 RPC-E-CANTCALLOUT-INEXTERNALCA      VALUE h"80010005".
       78 RPC-E-CONNECTION-TERMINATED         VALUE h"80010006".
       78 RPC-E-SERVER-DIED                   VALUE h"80010007".
       78 RPC-E-CLIENT-DIED                   VALUE h"80010008".
       78 RPC-E-INVALID-DATAPACKET            VALUE h"80010009".
       78 RPC-E-CANTTRANSMIT-CALL             VALUE h"8001000A".
       78 RPC-E-CLIENT-CANTMARSHAL-DATA       VALUE h"8001000B".
       78 RPC-E-CLIENT-CANTUNMARSHAL-DAT      VALUE h"8001000C".
       78 RPC-E-SERVER-CANTMARSHAL-DATA       VALUE h"8001000D".
       78 RPC-E-SERVER-CANTUNMARSHAL-DAT      VALUE h"8001000E".
       78 RPC-E-INVALID-DATA                  VALUE h"8001000F".
       78 RPC-E-INVALID-PARAMETER             VALUE h"80010010".
       78 RPC-E-CANTCALLOUT-AGAIN             VALUE h"80010011".
       78 RPC-E-SERVER-DIED-DNE               VALUE h"80010012".
       78 RPC-E-SYS-CALL-FAILED               VALUE h"80010100".
       78 RPC-E-OUT-OF-RESOURCES              VALUE h"80010101".
       78 RPC-E-ATTEMPTED-MULTITHREAD         VALUE h"80010102".
       78 RPC-E-NOT-REGISTERED                VALUE h"80010103".
       78 RPC-E-FAULT                         VALUE h"80010104".
       78 RPC-E-SERVERFAULT                   VALUE h"80010105".
       78 RPC-E-CHANGED-MODE                  VALUE h"80010106".
       78 RPC-E-INVALIDMETHOD                 VALUE h"80010107".
       78 RPC-E-DISCONNECTED                  VALUE h"80010108".
       78 RPC-E-RETRY                         VALUE h"80010109".
       78 RPC-E-SERVERCALL-RETRYLATER         VALUE h"8001010A".
       78 RPC-E-SERVERCALL-REJECTED           VALUE h"8001010B".
       78 RPC-E-INVALID-CALLDATA              VALUE h"8001010C".
       78 RPC-E-CANTCALLOUT-ININPUTSYNCC      VALUE h"8001010D".
       78 RPC-E-WRONG-THREAD                  VALUE h"8001010E".
       78 RPC-E-THREAD-NOT-INIT               VALUE h"8001010F".
       78 RPC-E-UNEXPECTED                    VALUE h"8001FFFF".

      *> Control codes
       78 CLASS-E-NOTLICENSED                 VALUE h"80040112".

       78 CTL-E-GETNOTSUPPORTED               VALUE h"800A0000" + 394.
      $end
