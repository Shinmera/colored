#|
 This file is a part of Colored
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.colored)

;; constants.lisp
(docs:define-docs
  (function define-color
    "Define a new color constant.

NAME should be a symbol for the constant's name. If the symbol is not
interned in any package, the name will automatically be used as a
symbol designator in the ORG.SHIRAKUMO.ALLOY.COLORED.COLORS package.
If the package is ORG.SHIRAKUMO.ALLOY.COLORED.COLORS, then the symbol
is also automatically exported from that package.

INT should be an integer describing the full colour, as interpreted
by the CONVERSION function. By default this should be an RGB integer.

Note that once the constant is defined, its value cannot be changed.

See RGB"))

;; ops.lisp
(docs:define-docs
  (function decode
    "Constructs a colour from the given integer colour representation.

CHANNEL-SIZE should be the number of bits per channel, and CHANNELS a
list in order of the channels. The list should contain symbols naming
the channels available to the requested TYPE.

For instance, to decode a 16bpc BGRA integer, you would use
  (decode-color int 16 '(b g r a))

See COLOR (type)
See ENCODE")

  (function encode
    "Constructs an integer for the colour using the given integer colour representation.

CHANNEL-SIZE should be the number of bits per channel, and CHANNELS a
list in order of the channels. The list should contain symbols naming
channels available for the colour being encoded.

For instance, to encode a 16bpc BGRA integer, you would use
  (encode-color color 16 '(b g r a))

See COLOR (type)
See DECODE")

  (function temperature-color
    "Compute the colour for the corresponding light temperature.

The temperature is given in Kelvin and is valid in the range
[0,40'000]. Values outside this range will not yield correct colours.

Note that the range [0,1'000] is denormalised and will simply
correspond to the colour for 1'000K linearly scaled down to zero.

Daylight temperatures occur in the range [5'000,6'5000], moonlight
temperature is around 4'100K.

See COLOR (type)")

  (function red
    "Returns the red component of the colour.

If not in RGB format, is first converted to RGB.

See R")

  (function green
    "Returns the green component of the colour.

If not in RGB format, is first converted to RGB.

See G")

  (function blue
    "Returns the blue component of the colour.

If not in RGB format, is first converted to RGB.

See B")

  (function alpha
    "Returns the alpha component of the colour.

See A")

  (function hue
    "Returns the hue component of the colour in degrees [0,360[.

If not in HSL/HSV/HSI format, is first converted to HSV.

See H")

  (function saturation
    "Returns the saturation component of the colour.

If not in HSL/HSV/HSI format, is first converted to HSV.

See S")

  (function value
    "Returns the value component of the colour.

If not in HSV format, is first converted to HSV.

See V")

  (function lightness
    "Returns the lightness component of the colour.

If not in HSL format, is first converted to HSL.

See L")

  (function intensity
    "Returns the intensity component of the colour.

If not in HSI format, is first converted to HSI.

See I")

  (function cyan
    "Returns the cyan component of the colour.

If not in CMYK format, is first converted to CMYK.

See C")

  (function magenta
    "Returns the magenta component of the colour.

If not in CMYK format, is first converted to CMYK.

See M")

  (function yellow
    "Returns the yellow component of the colour.

If not in CMYK format, is first converted to CMYK.

See Y")

  (function black
    "Returns the black component of the colour.

If not in CMYK format, is first converted to CMYK.

See K")

  (function lerp
    "Linearly interpolate between two colours from the same colour space.

If the colours are of a different colour space (type), an error is
signalled.

Each of the colours' channels are interpolated separately.")

  (function gradient
    "Evaluate a colour gradient at a specific point.

Returns the linear interpolation of the two colours between the stops 
designated by X. Each stop in STOPS should be a cons of its position
along the gradient and the colour at that stop.

See LERP"))

;; type.lisp
(docs:define-docs
  (type color
    "Representation of a colour in some space.

A colour is an immutable object and can be emitted into FASLs. Colors
can and are cached aggressively thanks to their immutability.

Note that an instance returned by a constructor may or may not be EQ 
to a previously constructed color instance with the same channel 
values. To properly test equality, use COLOR= or COLOR-EQUAL.

Note that the colour channels are stored as SINGLE-FLOATs. Conversion
functions to and from integers are available as operations. The floats
are not constrained to be within [0,1] and can thus be used to encode
high dynamic range.

See CHANNELS
See A
See COLOR (function)
See COLOR=
See COLOR-EQUAL
See RGB
See HSV
See HSL
See HSI
See CMYK
See LAB")

  (function channels
    "Returns the list of available channels in the colour.

Accepts both a COLOR instance and a color type name.

See COLOR (type)")

  (function convert
    "Convert a colour to a different colour space (type).

Direct conversions between any colour space and any other is not
guaranteed to be available, and you may have to convert to a
common space such as RGB or LAB first and then to your target
colour space of choice.

Certain conversions may accept additional arguments that influence
the process and colour range.

See COLOR (type)")

  (function a
    "Returns the alpha channel component.

0 means transparent, 1 opaque.

See COLOR (type)")

  (function srgb
    "Create a new sRGB color instance.

This is distinct from RGB by being in a non-linear space.

See COLOR (type)
See RGB")

  (function rgb
    "Create a new RGB color instance.

See COLOR (type)")

  (function r
    "Returns the red channel component.

See RGB
See RED")

  (function g
    "Returns the green channel component.

See RGB
See GREEN")

  (function b
    "Returns the blue channel component.

See RGB
See BLUE")

  (function hsv
    "Create a Hue/Saturation/Value component colour.

The Hue should be in [0,360[ degrees, the other two values in the
[0,1] range.

See TO-HSV
See HUE
See SATURATION
See VALUE
See COLOR (type)")

  (function h
    "Returns the hue channel component.

see HSV
See HSL
See HSI
See HUE")

  (function s
    "Returns the saturation channel component.

see HSV
See HSL
See HSI
See SATURATION")

  (function v
    "Returns the value channel component.

See HSV
See VALUE")

  (function hsl
    "Create a Hue/Saturation/Lightness component colour.

The Hue should be in [0,360[ degrees, the other two values in the
[0,1] range.

See TO-HSL
See HUE
See LIGHTNESS
See COLOR (type)")

  (function l
    "Returns the lightness channel component.

See HSL
See LIGHTNESS")

  (function hsi
    "Create a Hue/Saturation/Intensity component colour.

The Hue should be in [0,360[ degrees, the other two values in the
[0,1] range.

See TO-HSI
See HUE
See INTENSITY
See COLOR (type)")

  (function i
    "Returns the intensity channel component.

See HSI
See INTENSITY")

  (function cmyk
    "Create a Cyan/Magenta/Yellow/Black component colour.

See COLOR (type)
See C
See M
See Y
See K")

  (function c
    "Returns the cyan channel component.

See CMYK
See CYAN")

  (function m
    "Returns the magenta channel component.

See CMYK
See MAGENTA")

  (function y
    "Returns the yellow channel component.

See CMYK
See YELLOW")

  (function k
    "Returns the black channel component.

See CMYK
See BLACK")

  (function lab
    "Create a CIE LAB colour instance.

See COLOR (type)
See L*
See A*
See B*")

  (function l*
    "Returns the lightness channel component.

See LAB")

  (function a*
    "Returns the green/red channel component.

See LAB")

  (function b*
    "Returns the blue/yellow channel component.

See LAB")

  (function xyz
    "Create a CIE XYZ colour instance.

See COLOR (type)
See X
See Y
See Z")

  (function x
    "Return the X component.

See XYZ")

  (function y
    "Return the Y component.

See XYZ")

  (function z
    "Return the Z component.

See XYZ")

  (function color=
    "Returns true if all colours match in all channels.

See COLOR (type)
See COLOR-EQUAL")

  (function color-equal
    "Returns true if all colours match in the RGB channels.

Unlike COLOR=, this ignores the alpha channel.

See COLOR (type)
See COLOR="))

;; conversion-matrices.lisp
(docs:define-docs
  (function conversion-matrix
    "Returns a linear conversion matrix for the two given colour spaces.

Signals an error if no conversion matrix to fit the from/to
spaces is known. Standard conversions are known for converting
to/from XYZ and the following:

  :ADOBE-RGB
  :APPLE-RGB
  :BEST-RGB
  :BETA-RGB
  :BRUCE-RGB
  :CIE-RGB
  :COLORMATCH-RGB
  :DON-RGB-4
  :ECI-RGB
  :EKTA-SPACE-PS5
  :NTSC-RGB
  :PAL-RGB
  :PROPHOTO-RGB
  :SMPTE-C-RGB
  :SRGB
  :WIDE-GAMUT-RGB

The conversion method may be passed to CONVERT via the keywords
:SPACE (for a name of a colour space) or :MATRIX for passing a
9-element vector directly.

See DEFINE-CONVERSION")
  
  (function define-conversion
    "Defines a new colour space conversion matrix.

The matrix should be expressed via 9 entries that form a 3x3
matrix.

See CONVERSION-MATRIX"))

;; whitepoints.lisp
(docs:define-docs
  (function whitepoint
    "Returns a reference whitepoint XYZ triplet for the requested name.

Signals an error if no whitepoint is known for the given name.
The following whitepoints are defined by default:

  :A
  :B
  :C
  :D50
  :D55
  :D60
  :D65
  :D70
  :D75
  :E
  :F1
  :F2
  :F3
  :F4
  :F5
  :F6
  :F7
  :F8
  :F9
  :F10
  :F11
  :F12
  :LED-B1
  :LED-B2
  :LED-B3
  :LED-B4
  :LED-B5
  :LED-BH1
  :LED-RGB1
  :LED-V1
  :LED-V2

The whitepoint may be passed to CONVERT via the :WHITEPOINT keyword,
as either the name of a defined whitepoint, or a vector exprsesing the
XYZ triplet.

See DEFINE-WHITEPOINT")
  
  (function define-whitepoint
    "Defines a new whitepoint.

The whitepoint should be a list of two values, the xy chromaticity
coordinates in the xyY space. They are then automatically converted
and internally stored in the XYZ format.

See WHITEPOINT
See COMPUTE-D-WHITEPOINT")
  
  (function compute-d-whitepoint
    "Computes the xyY coordinates for the CIE standard illuminant series D.

CCT must be a CCT reference temperature.

See DEFINE-WHITEPOINT"))
