DEFINT A-Z

'2HR8.BAS
'Written by Lloyd Chang

'This file is part of slix

'The conditions to use
'2HR8 are the same as those
'applied to the use of slix.

COLOR 7
PRINT
PRINT "2HR8 - BMP/GIF to HR8 converter"
PRINT "Written by Lloyd Chang"
PRINT
PRINT "This could take a while...  :)"
PRINT

COLOR 23
PRINT "WORKING"

HR8Header$ = "HR8"

HR8HeaderSize% = 7

RGBPaletteFile% = FREEFILE

PaletteRGB$ = SPACE$(768)


DIM RGBPaletteTranslator(255) AS INTEGER
OPEN "RGB.PAL" FOR BINARY AS #RGBPaletteFile%
GET #RGBPaletteFile%, 1, PaletteRGB$
CLOSE #RGBPaletteFile%

FILEXFile% = FREEFILE

filename$ = COMMAND$

'Open file for input so QB stops with an error if it doesn't exist.
OPEN filename$ FOR INPUT AS #FILEXFile%
CLOSE #FILEXFile%

OPEN filename$ FOR BINARY AS #FILEXFile%

FOR Count% = 1 TO LEN(filename$)
  SELECT CASE MID$(filename$, Count%, 1)
    CASE IS = "."
      Outputfile$ = LEFT$(filename$, Count%) + "HR8"
      extension% = 1
  END SELECT
NEXT Count%

SELECT CASE extension%
  CASE IS = 1
  CASE ELSE
    Outputfile$ = filename$ + ".HR8"
END SELECT

SELECT CASE LOF(FILEXFile%)
  CASE IS > 32
    HeaderTest$ = SPACE$(32)
  CASE ELSE
    HeaderTest$ = SPACE$(LOF(FILEXFile%))
END SELECT

GET #FILEXFile%, , HeaderTest$

SELECT CASE LEFT$(HeaderTest$, 2)
  CASE IS = "BM"
    HeaderSig% = 2
    HeaderSig$ = "BMPWIN"
END SELECT

SELECT CASE LEFT$(HeaderTest$, 3)
  CASE IS = "GIF"
    HeaderSig% = 3
    HeaderSig$ = "GIF"
END SELECT

SELECT CASE HeaderSig$
  CASE IS = "BMPWIN"
    Outputfile% = FREEFILE
    OPEN Outputfile$ FOR BINARY AS #Outputfile%
    header$ = SPACE$(14)
    sizing$ = SPACE$(4)
    GET #FILEXFile%, 1, header$
    GET #FILEXFile%, 15, sizing$
    bmpinfosize = CVI(sizing$)
    'bmpinfosize - Is the size of the information header for the bitmap.
    '              Different bitmap versions have variations in filetypes.
    '              40 is a standard windows 3.1 bitmap.
    '              12 is for OS/2 bitmaps
    'The next routine reads in the appropriate headers and colour tables.
    'nbits is the number of bits per pixel - i.e. number of colours
    '1 bit = 2 colours, 4 bits = 16 colours, 8 bits = 256 colours, etc.
    'the 24 bit mode does not have a palette, its colours are expressed as
    'image data

    'Design of a windows 3.1 bitmap - Taken from bmp.zip on the
    'x2ftp.oulu.fi ftp site under /pub/msdos/programming/formats
    'Specifications for a Windows 3.1 bitmap. (.BMP)
    'Email any questions/responses to me at zabudsk@ecf.utoronto.ca
    'or post to alt.lang.basic or comp.lang.basic.misc.

    '       | # of   |
    'Offset | bytes  | Function (value)
    '-------+--------+--- General Picture information starts here---------
    '  0    |   2    | (BM) - Tells us that the picture is in bmp format
    '  2    |   4    | Size of the file (without header?)
    '  6    |   2    | (0) Reserved1 - Must be zero
    '  8    |   2    | (0) Reserved2 - Must be zero
    '  10   |   4    | Number of bytes offset of the picture data
    '-------+--------+--- Information Header starts here -----------------
    '  14   |   4    | (40/12) Size of information header (Win3.1/OS2)
    '  18   |   4    | Picture width in pixels
    '  22   |   4    | Picture Height in pixels
    '  26   |   2    | (1) Number of planes, must be 1
    '  28   |   2    | Number of bits per pixel (bpp), must be 1,4,8 or 24
    '  30   |   4    | (0) Compression - 0 means no compression, 1,2 are RLEs
    '  34   |   4    | Image size in bytes
    '  38   |   4    | picture width in pels per metre
    '  42   |   4    | picture height in pels per metre
    '  46   |   4    | (0) Number of colours used in the picture, 0 means all
    '  50   |   4    | (0) Number of important colours, 0 means all
    '-------+--------+--- Palette data starts here -----------------------
    '  54   |   1    | (b) - blue intensity component, color 0 - range 0 to 255
    '  55   |   1    | (g) - green intensity component, color 0 - range 0 to 255
    '  56   |   1    | (r) - red intensity component, color 0 - range 0 to 255
    '  57   |   1    | (0) - unused
    '  58   |   1    | (b) - blue intensity component, color 0 - range 0 to 255
    '  ...  | ...    |
    '  54   | 4*2^bpp| total range of palette
    '-------+--------+--- Image data starts here -------------------------
    '54+    | width* | Bitmap data starting at lower left portion of the
    '(4*2^n)| height*| image moving from left towards right. Moving up 1
    '       | (8/bpp)| pixel when at the right hand side of the image, starting
    '       |        | from the left side again, until the top right of the
    '       |        | image is reached

    'Note that this format is slightly different for a OS/2 Bitmap.
    'The header is the same up to (but not including) bit 30-
    'The palette colour values follow at bit 30, with the form...
    '1 byte blue intensity
    '1 byte green intensity
    '1 byte red intensity
    'For each colour of the picture.
    'Bitmapped image data follows the colour tables


    'Special note: When storing 1 bit (2 colour) pictures.
    '8 horizontal pixels are packed into 1 byte. Each bit determines
    'the colour of one pixel (colour 0 or colour 1)

    '4 bit pictures (16 colours) use 2 nibbles (4 bits) for each pixel
    'thus there are 2 pixels for each byte of image data.

    '8 bit pictures use 1 byte per pixel. Each byte of image data
    'represents one of 256 colours.

    '24 bit pictures express colour values by using 3 bytes and each has a
    'value between 0 and 255. The first byte is for red, the second is for
    'green and the third is for blue. Thus (256)^3 or 2^24 of 16777216 different
    'colours.

    IF bmpinfosize = 12 THEN
       infoheader$ = SPACE$(12)
       GET #FILEXFile%, 15, infoheader$
       nbits = CVI(MID$(infoheader$, 15, 4))

       IF nbits = 1 THEN
          palet$ = SPACE$(6)
          GET #FILEXFile%, bmpinfosize + 15, palet$
       ELSEIF nbits = 4 THEN
          palet$ = SPACE$(48)
          GET #FILEXFile%, bmpinfosize + 15, palet$
       ELSEIF nbits = 8 THEN
          palet$ = SPACE$(768)
          GET #FILEXFile%, bmpinfosize + 15, palet$
       END IF
    ELSEIF bmpinfosize = 40 THEN
       infoheader$ = SPACE$(40)
       GET #FILEXFile%, 15, infoheader$
       nbits = CVI(MID$(infoheader$, 15, 4))
       IF nbits = 1 THEN
          palet$ = SPACE$(8)
          GET #FILEXFile%, bmpinfosize + 15, palet$
       ELSEIF nbits = 4 THEN
          palet$ = SPACE$(64)
          GET #FILEXFile%, bmpinfosize + 15, palet$
       ELSEIF nbits = 8 THEN
          palet$ = SPACE$(1024)
          GET #FILEXFile%, bmpinfosize + 15, palet$
       END IF
    END IF
   

    ft$ = MID$(header$, 1, 2)
    'PRINT "Type of file (Should be BM): "; ft$

    filesize& = CVL(MID$(header$, 3, 4))
    'PRINT "Size of file: "; filesize&

    r1 = CVI(MID$(header$, 7, 2))
    'PRINT "Reserved 1: "; r1

    r2 = CVI(MID$(header$, 9, 2))
    'PRINT "Reserved 2: "; r2

    offset = CVL(MID$(header$, 11, 4))
    'PRINT "Number of bytes offset from beginning: "; offset

    'PRINT

    headersize = CVL(MID$(infoheader$, 1, 4))
    'PRINT "Size of header: "; headersize

    PicWidth = CVL(MID$(infoheader$, 5, 4))
    'PRINT "Width: "; picwidth

    PicHeight = CVL(MID$(infoheader$, 9, 4))
    'PRINT "Height: "; picheight

    HeaderXSize1$ = CHR$(PicWidth \ 256)
    HeaderXSize2$ = CHR$(PicWidth MOD 256)
    HeaderYSize1$ = CHR$(PicHeight \ 256)
    HeaderYSize2$ = CHR$(PicHeight MOD 256)
    PUT #Outputfile%, 1, HR8Header$
    PUT #Outputfile%, , HeaderXSize1$
    PUT #Outputfile%, , HeaderXSize2$
    PUT #Outputfile%, , HeaderYSize1$
    PUT #Outputfile%, , HeaderYSize2$
    FileNoHeaderCount& = 0

    nplanes = CVI(MID$(infoheader$, 13, 4))
    'PRINT "Planes: "; nplanes

    'PRINT "Bits per plane: "; nbits

    'PRINT

    IF headersize = 40 THEN
       'PRINT "Compression: ";
       comptype = CVL(MID$(infoheader$, 17, 4))
       'IF comptype = 0 THEN PRINT "None"
       'IF comptype = 1 THEN PRINT "Run Length - 8 Bits"
       'IF comptype = 2 THEN PRINT "Run Length - 4 Bits"

       imagesize& = CVL(MID$(infoheader$, 21, 4))
       'PRINT "Image Size (bytes): "; imagesize&

       XSize = CVL(MID$(infoheader$, 25, 4))
       'PRINT "X size (pixels per metre): "; xsize

       YSize = CVL(MID$(infoheader$, 29, 4))
       'PRINT "Y size (pixels per metre): "; ysize

       colorsused = CVL(MID$(infoheader$, 33, 4))
       'PRINT "Number of colours used: "; colorsused

       neededcolours = CVL(MID$(infoheader$, 37, 4))
       'PRINT "Number of important colours: "; neededcolours
    END IF
    'PRINT
    'PRINT "Press Any key to continue."
    'WHILE INKEY$ = ""
    'WEND

    IF nbits = 1 THEN
       'SCREEN 11
    ELSEIF nbits = 4 THEN
       'SCREEN 12
    ELSEIF nbits = 8 OR nbits = 24 THEN
       'SCREEN 13
    END IF
    IF bmpinfosize = 40 THEN ngroups = 4
    IF bmpinfosize = 12 THEN ngroups = 3

    IF nbits = 24 THEN
       IF ngroups = 3 THEN
          FOR c = 0 TO 63
             d = c * 4
             palet$ = palet$ + CHR$(d) + CHR$(d) + CHR$(d)
             palet$ = palet$ + CHR$(d) + CHR$(d) + CHR$(d + 1)
             palet$ = palet$ + CHR$(d) + CHR$(d + 1) + CHR$(d)
             palet$ = palet$ + CHR$(d + 1) + CHR$(d) + CHR$(d)
          NEXT c
       ELSEIF ngroups = 4 THEN
          FOR c = 0 TO 63
             d = c * 4
             palet$ = palet$ + CHR$(d) + CHR$(d) + CHR$(d) + CHR$(0)
             palet$ = palet$ + CHR$(d) + CHR$(d) + CHR$(d + 1) + CHR$(0)
             palet$ = palet$ + CHR$(d) + CHR$(d + 1) + CHR$(d) + CHR$(0)
             palet$ = palet$ + CHR$(d + 1) + CHR$(d) + CHR$(d) + CHR$(0)
          NEXT c
       END IF
    END IF

    FOR x = 1 TO LEN(palet$) STEP ngroups
       zb# = INT((ASC(MID$(palet$, x, 1))) / 4)
       zg# = INT((ASC(MID$(palet$, x + 1, 1))) / 4)
       zr# = INT((ASC(MID$(palet$, x + 2, 1))) / 4)
       Blue% = zb# \ (36 / 4)
       Green% = zg# \ (84 / 4)
       Red% = zr# \ (36 / 4)
       FOR Count% = 0 TO 255
         SELECT CASE (ASC(MID$(PaletteRGB$, ((Count% * 3) + 1), 1)) \ 36)
           CASE IS = Red%
             SELECT CASE (ASC(MID$(PaletteRGB$, ((Count% * 3) + 2), 1)) \ 84)
               CASE IS = Green%
                 SELECT CASE (ASC(MID$(PaletteRGB$, ((Count% * 3) + 3), 1)) \ 36)
                   CASE IS = Blue%
                     RGBPaletteTranslator((x - 1) / ngroups) = Count%
                 END SELECT
             END SELECT
         END SELECT
       NEXT Count%
       zc# = zb# * 65536# + zg# * 256# + zr#
       cres = ASC(MID$(palet$, x + 3, 1))
       'PALETTE ((x - 1) / ngroups), zc#
    NEXT x

    IF nbits = 24 THEN
       y = PicHeight - 1
       x = 0
       dat$ = "   "
       WHILE y >= 0
          WHILE x < PicWidth
             GET 1, , dat$
             p1 = INT((ASC(MID$(dat$, 1, 1)) + ASC(MID$(dat$, 1, 1)) + ASC(MID$(dat$, 1, 1))) / 3)
             'IF x < VGAWidth% AND y < VGAHeight% THEN PSETX x, y, RGBPaletteTranslator(p1)
             Dummy$ = CHR$(RGBPaletteTranslator(p1))
             FilePutLocation& = ((CLNG(PicWidth) * PicHeight) + HR8HeaderSize%) - ((PicWidth - 1) - (FileNoHeaderCount& MOD PicWidth)) - ((FileNoHeaderCount& \ PicWidth) * CLNG(PicWidth))
             'A& = 0: CLS: ? (64000 + 7) - (319 - (A& MOD 320)) - ((A& \ 320) * 320&)
             PUT #Outputfile%, FilePutLocation&, Dummy$
             FileNoHeaderCount& = FileNoHeaderCount& + 1
             x = x + 1
          WEND
          y = y - 1
          x = 0
       WEND
    ELSEIF nbits = 8 THEN
       y = PicHeight - 1
       x = 0
       dat$ = " "
       WHILE y >= 0
          WHILE x < PicWidth
             GET 1, , dat$
             'IF x < VGAWidth% AND y < VGAHeight% THEN PSETX x, y, RGBPaletteTranslator(ASC(dat$))
             Dummy$ = CHR$(RGBPaletteTranslator(ASC(dat$)))
             FilePutLocation& = ((CLNG(PicWidth) * PicHeight) + HR8HeaderSize%) - ((PicWidth - 1) - (FileNoHeaderCount& MOD PicWidth)) - ((FileNoHeaderCount& \ PicWidth) * CLNG(PicWidth))
             PUT #Outputfile%, FilePutLocation&, Dummy$
             FileNoHeaderCount& = FileNoHeaderCount& + 1
             x = x + 1
          WEND
          y = y - 1
          x = 0
       WEND
    ELSEIF nbits = 4 THEN
       y = PicHeight - 1
       x = 0
       dat$ = " "
       WHILE y >= 0
          WHILE x < PicWidth
            GET 1, , dat$
            LOCATE 1, 1
            p1 = ASC(dat$) AND 15
            p2 = ASC(dat$) AND 240 / 16
            'IF x < VGAWidth% AND y < VGAHeight% THEN PSETX x, y, RGBPaletteTranslator(p1)
            Dummy$ = CHR$(RGBPaletteTranslator(p1))
            FilePutLocation& = ((CLNG(PicWidth) * PicHeight) + HR8HeaderSize%) - ((PicWidth - 1) - (FileNoHeaderCount& MOD PicWidth)) - ((FileNoHeaderCount& \ PicWidth) * CLNG(PicWidth))
            PUT #Outputfile%, FilePutLocation&, Dummy$
            FileNoHeaderCount& = FileNoHeaderCount& + 1
            'IF x < VGAWidth% AND y < VGAHeight% THEN PSETX x + 1, y, RGBPaletteTranslator(p2)
            Dummy$ = CHR$(RGBPaletteTranslator(p2))
            FilePutLocation& = ((CLNG(PicWidth) * PicHeight) + HR8HeaderSize%) - ((PicWidth - 1) - (FileNoHeaderCount& MOD PicWidth)) - ((FileNoHeaderCount& \ PicWidth) * CLNG(PicWidth))
            PUT #Outputfile%, FilePutLocation&, Dummy$
            FileNoHeaderCount& = FileNoHeaderCount& + 1
            x = x + 2
          WEND
          y = y - 1
          x = 0
       WEND
    ELSEIF nbits = 1 THEN
       y = PicHeight - 1
       x = 0
       dat$ = " "
       WHILE y >= 0
          WHILE x < PicWidth
            GET 1, , dat$
            FOR P = 0 TO 7
               'IF x < VGAWidth% AND y < VGAHeight% THEN PSETX x + (7 - P), y, RGBPaletteTranslator((p1 AND 2 ^ P) / 2 ^ P)
               Dummy$ = CHR$(RGBPaletteTranslator((p1 AND 2 ^ P) / 2 ^ P))
               FilePutLocation& = ((CLNG(PicWidth) * PicHeight) + HR8HeaderSize%) - ((PicWidth - 1) - (FileNoHeaderCount& MOD PicWidth)) - ((FileNoHeaderCount& \ PicWidth) * CLNG(PicWidth))
               PUT #Outputfile%, FilePutLocation&, Dummy$
               FileNoHeaderCount& = FileNoHeaderCount& + 1
               NEXT P
            x = x + 8
          WEND
          y = y - 1
          x = 0
       WEND
    END IF

    'CLOSE
  CASE IS = "GIF"
    Outputfile% = FREEFILE
    OPEN Outputfile$ FOR BINARY AS #Outputfile%
    '
    'DEGIF6.BAS - No frills GIF decoder for the VGA's 320x200x256 mode.
    'By Rich Geldreich 1993 (Public domain, use as you wish.)
    'This version should properly decode all LZW encoded images in
    'GIF image files. I've finally added GIF89a and local colormap
    'support, so it more closely follows the GIF specification. It
    'still doesn't support the entire GIF89a specification, but it'll
    'show most GIF files fine.
    'The GIF decoding speed of this program isn't great, but I'd say
    'for an all QB/PDS decoder it's not bad!
    'Note: This program does not stop decoding the GIF image after the
    'rest of the scanlines become invisible! This happens with images
    'larger than the 320x200 screen. So if the program seems to be
    'just sitting there, accessing your hard disk, don't worry...
    'It'll beep when it's done.
    'DEFINT A-Z
    'Prefix() and Suffix() hold the LZW phrase dictionary.
    'OutStack() is used as a decoding stack.
    'ShiftOut() as a power of two table used to quickly retrieve the LZW
    'multibit codes.
    DIM Prefix(4095), Suffix(4095), OutStack(4095), ShiftOut(8)

    'The following line is for the QB environment(slow).
    DIM YBase AS LONG, Powersof2(11) AS LONG, WorkCode AS LONG
    'For a little more speed, unremark the next line and remark the one
    'above, before you compile... You'll get an overflow error if the
    'following line is used in the QB environment, so change it back.
    'DIM YBase AS INTEGER, Powersof2(11) AS INTEGER, WorkCode AS INTEGER

    'Precalculate power of two tables for fast shifts.
    FOR A = 0 TO 8: ShiftOut(8 - A) = 2 ^ A: NEXT
    FOR A = 0 TO 11: Powersof2(A) = 2 ^ A: NEXT

    'Get GIF filename.
    'A$ = COMMAND$: IF A$ = "" THEN INPUT "GIF file"; A$: IF A$ = "" THEN END
    'Add GIF extension if the given filename doesn't have one.
    'FOR A = LEN(filename$) TO 1 STEP -1
    'SELECT CASE MID$(filename$, A, 1)
        'CASE "\", ":": EXIT FOR
        'CASE ".": Extension = -1: EXIT FOR
        'END SELECT
    'NEXT
    'IF Extension = 0 THEN filename$ = filename$ + ".GIF"

    'Open file for input so QB stops with an error if it doesn't exist.
    'OPEN A$ FOR INPUT AS #FILEXFile%: CLOSE #FILEXFile%
    'OPEN A$ FOR BINARY AS #FILEXFile%

    'Check to see if GIF file. Ignore GIF version number.
    A$ = "      ": GET #FILEXFile%, 1, A$
    'IF LEFT$(A$, 3) <> "GIF" THEN PRINT "Not a GIF file.": END

    'Get logical screen's X and Y resolution.
    GET #FILEXFile%, , TotalX: GET #FILEXFile%, , TotalY: GOSUB GetByte
    'Calculate number of colors and find out if a global palette exists.
    NumColors = 2 ^ ((A AND 7) + 1): NoPalette = (A AND 128) = 0
    'Retrieve background color.
    GOSUB GetByte: Background = A

    'Get aspect ratio and ignore it.
    GOSUB GetByte

    'Retrieve global palette if it exists.
    IF NoPalette = 0 THEN P$ = SPACE$(NumColors * 3): GET #FILEXFile%, , P$

    DO 'Image decode loop

    'Skip by any GIF extensions.
    '(With a few modifications this code could also fetch comments.)
    DO
        'Skip by any zeros at end of image (why must I do this? the
        'GIF spec never mentioned it)
        DO
            IF EOF(FILEXFile%) THEN GOTO AllDone 'if at end of file, exit
            GOSUB GetByte
        LOOP WHILE A = 0           'loop while byte fetched is zero

        SELECT CASE A
        CASE 44  'We've found an image descriptor!
            EXIT DO
        CASE 59  'GIF trailer, stop decoding.
            GOTO AllDone
        CASE IS <> 33
            'PRINT "Unknown GIF extension type."': END
        END SELECT
        'Skip by blocked extension data.
        GOSUB GetByte
        DO: GOSUB GetByte: A$ = SPACE$(A): GET #FILEXFile%, , A$: LOOP UNTIL A = 0
    LOOP
    'Get image's start coordinates and size.
    GET #FILEXFile%, , XStart: GET #FILEXFile%, , YStart: GET #FILEXFile%, , XLength: GET #FILEXFile%, , YLength
    HeaderXSize1$ = CHR$(XLength \ 256)
    HeaderXSize2$ = CHR$(XLength MOD 256)
    HeaderYSize1$ = CHR$(YLength \ 256)
    HeaderYSize2$ = CHR$(YLength MOD 256)
    PUT #Outputfile%, 1, HR8Header$
    PUT #Outputfile%, , HeaderXSize1$
    PUT #Outputfile%, , HeaderXSize2$
    PUT #Outputfile%, , HeaderYSize1$
    PUT #Outputfile%, , HeaderYSize2$
    XEnd = XStart + XLength: YEnd = YStart + YLength

    'Check for local colormap, and fetch it if it exists.
    GOSUB GetByte
    IF (A AND 128) THEN
        NoPalette = 0
        NumColors = 2 ^ ((A AND 7) + 1)
        P$ = SPACE$(NumColors * 3): GET #FILEXFile%, , P$
    END IF

    'Check for interlaced image.
    Interlaced = (A AND 64) > 0: PassNumber = 0: PassStep = 8

    'Get LZW starting code size.
    GOSUB GetByte

    'Calculate clear code, end of stream code, and first free LZW code.
    ClearCode = 2 ^ A
    EOSCode = ClearCode + 1
    FirstCode = ClearCode + 2: NextCode = FirstCode
    StartCodeSize = A + 1: CodeSize = StartCodeSize

    'Find maximum code for the current code size.
    StartMaxCode = 2 ^ (A + 1) - 1: MaxCode = StartMaxCode

    BitsIn = 0: BlockSize = 0: BlockPointer = 1

    x = XStart: y = YStart: YBase = y * CLNG(VGAWidth%)

    'Set screen 13 in not set yet.
    IF FirstTime = 0 THEN
        'Go to VGA mode 13 (320x200x256).
        'SCREEN 13: DEF SEG = &HA000
    END IF

    'Set palette, if there was one.
    IF NoPalette = 0 THEN
        'Use OUTs for speed.
        'OUT &H3C8, 0
        'FOR A = 1 TO NumColors * 3: OUT &H3C9, ASC(MID$(P$, A, 1)) \ 4: NEXT
        FOR A% = 0 TO (NumColors - 1)
          Blue% = ASC(MID$(P$, (A * 3) + 1, 1)) \ 36
          Green% = ASC(MID$(P$, (A * 3) + 2, 1)) \ 84
          Red% = ASC(MID$(P$, (A * 3) + 3, 1)) \ 36
          FOR Count% = 0 TO 255
            SELECT CASE (ASC(MID$(PaletteRGB$, ((Count% * 3) + 1), 1)) \ 36)
              CASE IS = Blue%
                SELECT CASE (ASC(MID$(PaletteRGB$, ((Count% * 3) + 2), 1)) \ 84)
                  CASE IS = Green%
                    SELECT CASE (ASC(MID$(PaletteRGB$, ((Count% * 3) + 3), 1)) \ 36)
                      CASE IS = Red%
                        RGBPaletteTranslator(A%) = Count%
                    END SELECT
                END SELECT
            END SELECT
          NEXT Count%
        NEXT A%
        'Save palette of image to disk.
        'OPEN "pal." FOR BINARY AS #2: PUT #2, , P$: CLOSE #2
    END IF

    'IF FirstTime = 0 THEN
      'Clear entire screen to background color. This isn't
      'done until the image's palette is set, to avoid flicker
      'on some GIFs.
        'LINE (0, 0)-(319, 199), Background, BF
        'FirstTime = -1
    'END IF

    'Decode LZW data stream to screen.
    DO
        'Retrieve one LZW code.
        GOSUB GetCode
        'Is it an end of stream code?
        IF Code <> EOSCode THEN
            'Is it a clear code? (The clear code resets the sliding
            'dictionary - it *should* be the first LZW code present in
            'the data stream.)
            IF Code = ClearCode THEN
                NextCode = FirstCode
                CodeSize = StartCodeSize
                MaxCode = StartMaxCode
                DO: GOSUB GetCode: LOOP WHILE Code = ClearCode
                IF Code = EOSCode THEN GOTO ImageDone
                LastCode = Code: LastPixel = Code
                'IF X < 320 AND y < 200 THEN POKE X + YBase, LastPixel
                'IF x < VGAWidth% AND y < VGAHeight% THEN PSETX x, y, LastPixel
                'IF x < VGAWidth% AND y < VGAHeight% THEN PSETX x, y, RGBPaletteTranslator(LastPixel)
                Dummy$ = CHR$(RGBPaletteTranslator(LastPixel))
                PUT #Outputfile%, , Dummy$
                x = x + 1: IF x = XEnd THEN GOSUB NextScanLine
            ELSE
                CurCode = Code: StackPointer = 0

                'Have we entered this code into the dictionary yet?
                IF Code >= NextCode THEN
                    IF Code > NextCode THEN GOTO AllDone 'Bad GIF if this happens.
                   'mimick last code if we haven't entered the requested
                   'code into the dictionary yet
                    CurCode = LastCode
                    OutStack(StackPointer) = LastPixel
                    StackPointer = StackPointer + 1
                END IF

                'Recursively get each character of the string.
                'Since we get the characters in reverse, "push" them
                'onto a stack so we can "pop" them off later.
                'Hint: There is another, much faster way to accomplish
                'this that doesn't involve a decoding stack at all...
                DO WHILE CurCode >= FirstCode
                    OutStack(StackPointer) = Suffix(CurCode)
                    StackPointer = StackPointer + 1
                    CurCode = Prefix(CurCode)
                LOOP

                LastPixel = CurCode
                'IF X < 320 AND y < 200 THEN POKE X + YBase, LastPixel
                'IF x < VGAWidth% AND y < VGAHeight% THEN PSETX x, y, LastPixel
                'IF x < VGAWidth% AND y < VGAHeight% THEN PSETX x, y, RGBPaletteTranslator(LastPixel)
                Dummy$ = CHR$(RGBPaletteTranslator(LastPixel))
                PUT #Outputfile%, , Dummy$
                x = x + 1: IF x = XEnd THEN GOSUB NextScanLine

                '"Pop" each character onto the display.
                FOR A = StackPointer - 1 TO 0 STEP -1
                    'IF X < 320 AND y < 200 THEN POKE X + YBase, OutStack(A)
                    'IF x < VGAWidth% AND y < VGAHeight% THEN PSETX x, y, OutStack(A)
                    'IF x < VGAWidth% AND y < VGAHeight% THEN PSETX x, y, RGBPaletteTranslator(OutStack(A))
                    Dummy$ = CHR$(RGBPaletteTranslator(OutStack(A)))
                    PUT #Outputfile%, , Dummy$
                    x = x + 1: IF x = XEnd THEN GOSUB NextScanLine
                NEXT

                'Can we put this new string into our dictionary? (Some GIF
                'encoders will wait a bit when the dictionary is full
                'before sending a clear code- this increases compression
                'because the dictionary's contents are thrown away less
                'often.)
                IF NextCode < 4096 THEN
                    'Store new string in the dictionary for later use.
                    Prefix(NextCode) = LastCode
                    Suffix(NextCode) = LastPixel
                    NextCode = NextCode + 1
                    'Time to increase the LZW code size?
                    IF (NextCode > MaxCode) AND (CodeSize < 12) THEN
                        CodeSize = CodeSize + 1
                        MaxCode = MaxCode * 2 + 1
                    END IF
                END IF
                LastCode = Code
            END IF
        END IF
    LOOP UNTIL Code = EOSCode
ImageDone:

    LOOP

AllDone:

    'Save image and palette to BSAVE file.
    'DEF SEG = &HA000
    'OUT &H3C7, 0
    'FOR a = 0 TO 767
    '    POKE a + 64000, INP(&H3C9)
    'NEXT
    'BSAVE "pic.bas", 0, 64768

    'Load images saved with the above code with this:
    'DEF SEG= &HA000
    'BLOAD "Pic.Bas"
    'OUT &H3C8, 0
    'FOR a = 0 To 767
    '     OUT &H3C9, Peek(a+ 64000)
    'NEXT

    'BEEP: DO: LOOP WHILE INKEY$ <> "": DO: LOOP UNTIL INKEY$ <> ""
    'END
    GOTO EndGIFRoutine 'Yes, I know...This is poorly structured programming


    'Slowly reads one byte from the GIF file...
GetByte: A$ = " ": GET #FILEXFile%, , A$: A = ASC(A$): RETURN

    'Moves down one scanline. If the GIF is interlaced, then the number
    'of scanlines skipped is based on the current pass.
NextScanLine:
        IF Interlaced THEN
            y = y + PassStep
            IF y >= YEnd THEN
                PassNumber = PassNumber + 1
                SELECT CASE PassNumber
                CASE 1: y = 4: PassStep = 8
                CASE 2: y = 2: PassStep = 4
                CASE 3: y = 1: PassStep = 2
                END SELECT
            END IF
        ELSE
            y = y + 1
        END IF
        x = XStart: YBase = y * CLNG(VGAWidth%)
    RETURN

    'Reads a multibit code from the data stream.
GetCode:
        WorkCode = LastChar \ ShiftOut(BitsIn)
      'Loop while more bits are needed.
        DO WHILE CodeSize > BitsIn
    'Reads a byte from the LZW data stream. Since the data stream is
    'blocked, a check is performed for the end of the current block
    'before each byte is fetched.
            IF BlockPointer > BlockSize THEN
              'Retrieve block's length
                GOSUB GetByte: BlockSize = A
                A$ = SPACE$(BlockSize): GET #FILEXFile%, , A$
                BlockPointer = 1
            END IF
          'Yuck, ASC() and MID$() aren't that fast.
            LastChar = ASC(MID$(A$, BlockPointer, 1))
            BlockPointer = BlockPointer + 1
          'Append 8 more bits to the input buffer
            WorkCode = WorkCode OR LastChar * Powersof2(BitsIn)
            BitsIn = BitsIn + 8
        LOOP
      'Take away x number of bits.
        BitsIn = BitsIn - CodeSize
      'Return code to caller.
        Code = WorkCode AND MaxCode
    RETURN
EndGIFRoutine:

END SELECT
CLOSE #FILEXFile%
COLOR 7
LOCATE (CSRLIN - 1)
PRINT STRING$(7, 32)
END

