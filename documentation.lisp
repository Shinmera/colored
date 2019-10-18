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
  (function decode-color
    "Constructs a colour from the given integer colour representation.

CHANNEL-SIZE should be the number of bits per channel, and CHANNELS a
list in order of the channels. The list should contain symbols naming
the channels R G B A.

For instance, to decode a 16bpc BGRA integer, you would use
  (decode-color int 16 (b g r a))

Please note that this is a macro and the arguments except for the
INTEGER must be literal.

See COLOR (type)
See ENCODE-COLOR")

  (function encode-color
    "Constructs an integer for the colour using the given integer colour representation.

CHANNEL-SIZE should be the number of bits per channel, and CHANNELS a
list in order of the channels. The list should contain symbols naming
the channels R G B A.

For instance, to encode a 16bpc BGRA integer, you would use
  (encode-color color 16 (b g r a))

Please note that this is a macro and the arguments except for the
COLOR must be literal.

See COLOR (type)
See DECODE-COLOR")
  
  (function rgb
    "Reads a colour out of an 8bpc RGB integer.

See COLOR")

  (function bgr
    "Reads a colour out of an 8bpc BGR integer.

See COLOR")

  (function argb
    "Reads a colour out of an 8bpc ARGB integer.

See COLOR")

  (function abgr
    "Reads a colour out of an 8bpc ABGR integer.

See COLOR")

  (function rgba
    "Reads a colour out of an 8bpc RGBA integer.

See COLOR")

  (function bgra
    "Reads a colour out of an 8bpc BGRA integer.

See COLOR")

  (function to-rgb
    "Encodes a colour into an 8bpc RGB integer.

See COLOR")

  (function to-bgr
    "Encodes a colour into an 8bpc BGR integer.

See COLOR")

  (function to-argb
    "Encodes a colour into an 8bpc ARGB integer.

See COLOR")

  (function to-abgr
    "Encodes a colour into an 8bpc ABGR integer.

See COLOR")

  (function to-rgba
    "Encodes a colour into an 8bpc RGBA integer.

See COLOR")

  (function to-bgra
    "Encodes a colour into an 8bpc BGRA integer.

See COLOR")

  (function hsv
    "Convert the Hue Saturation Value triplet to a colour.

The Hue should be in [0,360[ degrees, the other two values in the
[0,1] range.

See TO-HSV
See HUE
See SATURATION
See VALUE
See COLOR (type)")

  (function hsl
    "Convert the Hue Saturation Lightness triplet to a colour.

The Hue should be in [0,360[ degrees, the other two values in the
[0,1] range.

See TO-HSL
See HUE
See LIGHTNESS
See COLOR (type)")

  (function hsi
    "Convert the Hue Saturation Intensity triplet to a colour.

The Hue should be in [0,360[ degrees, the other two values in the
[0,1] range.

See TO-HSI
See HUE
See INTENSITY
See COLOR (type)")

  (function hcl
    "Convert the Hue Chroma Luma triplet to a colour.

The Hue should be in [0,360[ degrees, the other two values in the
[0,1] range.

See TO-HCL
See HUE
See CHROMA
See LUMA
See COLOR (type)")

  (function temperature-color
    "Compute the colour for the corresponding light temperature.

The temperature is given in Kelvin and is valid in the range
[0,40'000]. Values outside this range will not yield correct colours.

Note that the range [0,1'000] is denormalised and will simply
correspond to the colour for 1'000K linearly scaled down to zero.

Daylight temperatures occur in the range [5'000,6'5000], moonlight
temperature is around 4'100K.

See COLOR (type)")

  (function to-hsv
    "Return the Hue Saturation Value triplet of the colour.

Returns a list of the form (H S V).

Note that the result is unspecified if the colour is not in the
normalised range.

See HSV
See HUE
See SATURATION
See VALUE
See COLOR (type)")

  (function to-hsl
    "Return the Hue Saturation Lightness triplet of the colour.

Returns a list of the form (H S L).

Note that the result is unspecified if the colour is not in the
normalised range.

See HSV
See HUE
See LIGHTNESS
See COLOR (type)")

  (function to-hsi
    "Return the Hue Saturation Intensity triplet of the colour.

Returns a list of the form (H S I).

Note that the result is unspecified if the colour is not in the
normalised range.

See HSV
See HUE
See INTENSITY
See COLOR (type)")

  (function to-hcl
    "Return the Hue Chroma Luma triplet of the colour.

Returns a list of the form (H C L).

Note that the result is unspecified if the colour is not in the
normalised range.

See HSV
See HUE
See CHROMA
See LUMA
See COLOR (type)")

  (function red
    "Returns the red component of the colour.

See R")

  (function green
    "Returns the green component of the colour.

See G")

  (function blue
    "Returns the blue component of the colour.

See B")

  (function alpha
    "Returns the alpha component of the colour.

See A")

  (function hue
    "Returns the hue of the colour in degrees [0,360[.

Note that the result is unspecified if the colour is not in the
normalised range.

See COLOR (type)
See HSV
See HSL
See HSI
See HCL
See TO-HSV
See TO-HSL
See TO-HSI
See TO-HCL")

  (function saturation
    "Returns the saturation of the colour.

Note that this is the saturation corresponding to HSV, and will not be
correct for HSL or HSI.

Note that the result is unspecified if the colour is not in the
normalised range.

See COLOR (type)
See HSV
See TO-HSV")

  (function value
    "Returns the value of the colour.

Note that the result is unspecified if the colour is not in the
normalised range.

See COLOR (type)
See HSV
See TO-HSV")

  (function lightness
    "Returns the lightness of the colour.

Note that the result is unspecified if the colour is not in the
normalised range.

See COLOR (type)
See HSL
See TO-HSL")

  (function intensity
    "Returns the intensity of the colour.

Note that the result is unspecified if the colour is not in the
normalised range.

See COLOR (type)
See HSI
See TO-HSI")

  (function luma
    "Returns the luma of the colour.

Note that the result is unspecified if the colour is not in the
normalised range.

See COLOR (type)
See HCL
See TO-HCL")

  (function chroma
    "Returns the chroma of the colour.

Note that the result is unspecified if the colour is not in the
normalised range.

See COLOR (type)
See HCL
See TO-HCL")

  (function map-color
    "Maps the RGB channels using the given transform function.

Returns a new colour with each R G B channel mapped individually by
the transform function. The A channel is simply copied.

See COLOR (type)")

  (function gamma-adjust
    "Returns the gamma corrected color.

This is equivalent to mapping each RGB channel by (expt C gamma). If
you need to map back from gamma-space into linear-space, supply the
inverted gamma value.

See COLOR (type)
See REINHARD-MAP
See EXPOSURE-MAP")

  (function reinhard-map
    "Performs a Reinhard tone mapping.

This will also apply gamma correction. As such the input colour should
be in HDR gamma space.

See COLOR (type)
See GAMMA-ADJUST")

  (function exposure-map
    "Performs a simple exposure tone mapping.

This will also apply gamma correction. As such the input colour should
be in HDR gamma space.

See COLOR (type)
See GAMMA-ADJUST"))

;; type.lisp
(docs:define-docs
  (type color
    "Representation of an RGBA colour quadruplet.

A colour is an immutable object and can be emitted into FASLs. Colors
can and are cached aggressively thanks to their immutability.

Note that the colour channels are stored as SINGLE-FLOATs. Conversion
functions to and from integers are available as operations. The floats
are not constrained to be within [0,1] and can thus be used to encode
high dynamic range. Gamma mapping from and to normalised space is also
available as operations.

See R
See G
See B
See A
See COLOR (function)
See COLOR=")

  (function r
    "Returns the red channel component.

See COLOR (type)")

  (function g
    "Returns the green channel component.

See COLOR (type)")

  (function b
    "Returns the blue channel component.

See COLOR (type)")

  (function a
    "Returns the alpha channel component.

0 means transparent, 1 opaque.

See COLOR (type)")

  (function color
    "Create a new color instance.

Note that the returned instance may or may not be EQ to a previously
constructed color instance with the same channel values. To properly
test equality, use COLOR=.

See COLOR (type)
See COLOR=")

  (function color=
    "Returns true if all colours match in all channels.

See COLOR (type)
See COLOR-EQUAL")

  (function color-equal
    "Returns true if all colours match in the RGB channels.

Unlike COLOR=, this ignores the alpha channel.

See COLOR (type)
See COLOR="))
