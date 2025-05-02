(in-package #:org.shirakumo.alloy.colored)

(defun decode (integer &key (type 'rgb) (channel-size 8) (channels (channels type)))
  (let ((int-size (* channel-size (length channels)))
        (channel-max (1- (expt 2 channel-size))))
    (let ((args (loop for channel in (append (channels type) '(a))
                      for pos = (position channel channels :test #'string=)
                      collect (cond (pos
                                     (/ (ldb (byte channel-size (- int-size (* channel-size (1+ pos)))) integer)
                                        (float channel-max)))
                                    ((eq channel 'a) 1f0)
                                    (T 0f0)))))
      (apply type args))))

(defun encode (color &key (channel-size 8) (channels (channels color)))
  (let ((int-size (* channel-size (length channels)))
        (channel-max (1- (expt 2 channel-size))))
    (loop for channel in (append (channels color) '(a))
          for pos = (position channel channels :test #'string=)
          when pos
          sum (ash (max 0 (min channel-max (floor (* channel-max (funcall channel color)))))
                   (- int-size (* channel-size (1+ pos)))))))

(defun hue-space-rgb (h c x m alpha)
  (cond ((<= h 1) (color (+ m c) (+ m x) (+ m 0) alpha))
        ((<= h 2) (color (+ m x) (+ m c) (+ m 0) alpha))
        ((<= h 3) (color (+ m 0) (+ m c) (+ m x) alpha))
        ((<= h 4) (color (+ m 0) (+ m x) (+ m c) alpha))
        ((<= h 5) (color (+ m x) (+ m 0) (+ m c) alpha))
        ((<= h 6) (color (+ m c) (+ m 0) (+ m x) alpha))
        (T (color m m m alpha))))

(defun %hue (color max min)
  (flet ((f (x y)
           (let ((h (* 60 (+ x (/ y (- max min))))))
             (if (<= 0 h) h (+ h 360)))))
    (cond ((= (r color) (g color) (b color)) 0)
          ((= max (r color)) (f 0 (- (g color) (b color))))
          ((= max (g color)) (f 2 (- (b color) (r color))))
          (T                 (f 4 (- (r color) (g color)))))))

;; See https://en.wikipedia.org/wiki/SRGB for details
(defmethod convert ((color srgb) (_ (eql 'rgb)) &key)
  (flet ((srgb-to-linear (x)
           (if (<= x 0.04045)
               (/ x 12.92)
               (expt (/ (+ x 0.055) 1.055) 2.4))))
    (rgb (srgb-to-linear (r color))
         (srgb-to-linear (g color))
         (srgb-to-linear (b color))
         (a color))))

(defmethod convert ((color rgb) (_ (eql 'srgb)) &key)
  (flet ((linear-to-srgb (x)
           (if (<= x 0.0031308)
               (* x 12.92)
               (- (* 1.055 (expt x (/ 1.0 2.4))) 0.055))))
    (srgb (linear-to-srgb (r color))
          (linear-to-srgb (g color))
          (linear-to-srgb (b color))
          (a color))))

(defmethod convert ((color rgb) (_ (eql 'xyz)) &key (space :srgb) (matrix (conversion-matrix space 'xyz)))
  (xyz (reduce-row matrix 0 (r color) (g color) (b color))
       (reduce-row matrix 1 (r color) (g color) (b color))
       (reduce-row matrix 2 (r color) (g color) (b color))
       (a color)))

(defmethod convert ((color xyz) (_ (eql 'rgb)) &key (space :srgb) (matrix (conversion-matrix 'xyz space)))
  (rgb (reduce-row matrix 0 (x* color) (y* color) (z* color))
       (reduce-row matrix 1 (x* color) (y* color) (z* color))
       (reduce-row matrix 2 (x* color) (y* color) (z* color))
       (a color)))

(defmethod convert ((color xyz) (_ (eql 'lab)) &key (whitepoint :D65))
  (let* ((w (etypecase whitepoint
              (symbol (whitepoint whitepoint))
              (vector whitepoint)))
         (e 216/24389)
         (k 24389/27)
         (x (/ (x* color) (aref w 0)))
         (y (/ (y* color) (aref w 1)))
         (z (/ (z* color) (aref w 2))))
    (flet ((f (x)
             (if (< e x)
                 (expt x 1/3)
                 (/ (+ (* k x) 16) 116))))
      (lab (- (* 116 (f y)) 16)
           (* 500 (- (f x) (f y)))
           (* 200 (- (f y) (f z)))))))

(defmethod convert ((color lab) (_ (eql 'xyz)) &key (whitepoint :D65))
  (let* ((w (etypecase whitepoint
              (symbol (whitepoint whitepoint))
              (vector whitepoint)))
         (e 216/24389)
         (k 24389/27)
         (fy (/ (+ (l* color) 16) 116))
         (fx (+ (/ (a* color) 500) fy))
         (fz (- fy (/ (b* color) 200))))
    (flet ((f (x)
             (if (< e (expt x 3))
                 (expt x 3)
                 (/ (- (* 116 x) 16) k))))
      (xyz (* (aref w 0) (f fx))
           (* (aref w 1) (f fy))
           (* (aref w 2) (f fz))))))

(defmethod convert ((color rgb) (_ (eql 'lab)) &rest args)
  (apply #'convert (apply #'convert color 'xyz args) 'lab args))

(defmethod convert ((color lab) (_ (eql 'rgb)) &rest args)
  (apply #'convert (apply #'convert color 'xyz args) 'rgb args))

(defmethod convert ((color hsv) (_ (eql 'rgb)) &key)
  (flet ((f (n)
           (let ((k (mod (+ n (/ (h color) 60)) 6)))
             (- (v color) (* (v color) (s color) (max 0 (min 1 k (- 4 k))))))))
    (color (f 5) (f 3) (f 1) (a color))))

(defmethod convert ((color rgb) (_ (eql 'hsv)) &key)
  (let ((max (max (r color) (g color) (b color)))
        (min (min (r color) (g color) (b color))))
    (hsv (%hue color max min)
         (if (= 0 max) 0 (/ (- max min) max))
         (max (r color) (g color) (b color))
         (a color))))

(defmethod convert ((color hsl) (_ (eql 'rgb)) &key)
  (if (= (s color) 0)
      (color (l color) (l color) (l color) (a color))
      (let* ((c (* (s color) (- 1 (abs (1- (* 2 (l color)))))))
             (m (- (l color) (/ c 2)))
             (h (/ (h color) 60))
             (x (* c (- 1 (abs (1- (mod h 2)))))))
        (hue-space-rgb h c x m (a color)))))

(defmethod convert ((color rgb) (_ (eql 'hsl)) &key)
  (let ((max (max (r color) (g color) (b color)))
        (min (min (r color) (g color) (b color))))
    (hsl (%hue color max min)
         (cond ((= 0 max) 0)
               ((= 1 min) 0)
               (T         (/ (- max min) (- 1 (abs (1- (+ max min)))))))
         (/ (+ max min) 2)
         (a color))))

(defmethod convert ((color hsi) (_ (eql 'rgb)) &key)
  (let* ((h (/ (h color) 60))
         (z (- 1 (abs (1- (mod h 2)))))
         (c (/ (* 3 (i color) (s color)) (1+ z)))
         (x (* c z))
         (m (* (i color) (- 1 (s color)))))
    (hue-space-rgb h c x m (a color))))

(defmethod convert ((color rgb) (_ (eql 'hsi)) &key)
  (let ((max (max (r color) (g color) (b color)))
        (min (min (r color) (g color) (b color))))
    (hsi (%hue color max min)
         (- 1 (* 3 min))
         (/ (+ (r color) (g color) (b color)) 3)
         (a color))))

(defmethod convert ((color oklab) (_ (eql 'rgb)) &key)
  (let ((l (expt (+ (l* color) (* 0.3963377774 (a* color)) (* 0.2158037573 (b* color))) 3))
        (m (expt (- (l* color) (* 0.1055613458 (a* color)) (* 0.0638541728 (b* color))) 3))
        (s (expt (- (l* color) (* 0.0894841775 (a* color)) (* 1.2914855480 (b* color))) 3)))
    (rgb (+ (* +4.0767416621 l) (* -3.3077115913 m) (* +0.2309699292 s))
         (+ (* -1.2684380046 l) (* +2.6097574011 m) (* -0.3413193965 s))
         (+ (* -0.0041960863 l) (* -0.7034186147 m) (* +1.7076147010 s)))))

(defmethod convert ((color rgb) (_ (eql 'oklab)) &key)
  (let ((l (expt (+ (r color) (* 0.3963377774 (g color)) (* 0.2158037573 (b color))) 1/3))
        (m (expt (+ (r color) (* 0.1055613458 (g color)) (* 0.0638541728 (b color))) 1/3))
        (s (expt (+ (r color) (* 0.0894841775 (g color)) (* 1.2914855480 (b color))) 1/3)))
    (oklab (+ (* +0.2104542553 l) (* +0.7936177850 m) (* -0.0040720468 s))
           (+ (* +1.9779984951 l) (* -2.4285922050 m) (* +0.4505937099 s))
           (+ (* +0.0259040371 l) (* +0.7827717662 m) (* -0.8086757660 s)))))

(defmacro define-channel-reader (name base-type channel &optional (conversion-type base-type))
  `(defun ,name (color)
     (typecase color
       (,base-type (,channel color))
       (T (,channel (convert color ',conversion-type))))))

(defun alpha (color)
  (a color))

(defun luminance (color)
  (etypecase color
    (rgb (+ (* (r color) 0.299)
            (* (g color) 0.587)
            (* (b color) 0.114)))
    (color (luminance (convert color 'rgb)))))

(define-channel-reader red rgb r)
(define-channel-reader green rgb g)
(define-channel-reader blue rgb b)
(define-channel-reader hue hue-type h hsv)
(define-channel-reader saturation hue-type s hsv)
(define-channel-reader value hsv h)
(define-channel-reader lightness hsl l)
(define-channel-reader intensity hsi i)
(define-channel-reader cyan cmyk c)
(define-channel-reader magenta cmyk m)
(define-channel-reader yellow cmyk y)
(define-channel-reader black cmyk k)

;;; This is based on http://www.zombieprototypes.com/?p=210
(defun temperature-color (kelvin &optional (alpha 1))
  (let* ((kelvin (coerce kelvin 'double-float))
         (temp (max 1000d0 kelvin)))
    (declare (type (double-float 1d0) temp))
    (declare (optimize speed))
    (flet ((e (a b c d)
             (declare (type double-float a b c d))
             (let ((x (- (/ temp 100d0) d)))
               (coerce (* (max 0d0 (min 1d0 (/ (+ a (* b x) (* c (the double-float (log x)))) 255)))
                          (/ (min kelvin 1000d0) 1000d0))
                       'single-float))))
      (color (e 351.97690566805693d0
                0.114206453784165d0
                -40.25366309332127d0
                55d0)
             (if (< temp 6600)
                 (e -155.25485562709179d0
                    -0.44596950469579133d0
                    104.49216199393888d0
                    2d0)
                 (e 325.4494125711974d0
                    0.07943456536662342d0
                    0.07943456536662342d0
                    50d0))
             (e
              -254.76935184120902d0
              -254.76935184120902d0
              115.67994401066147d0
              10d0)
             alpha))))

(defmethod lerp (x (a rgb) (b rgb))
  (macrolet ((lerp (f)
               `(+ (* (,f a) (- 1 x)) (* (,f b) x))))
    (rgb (lerp r) (lerp g) (lerp b) (lerp a))))

(defmethod lerp (x (a color) (b color))
  (unless (eq (type-of a) (type-of b))
    (error "Cannot lerp between~%  ~a~%and~%  ~a~%as they are not of the same colour space." a b))
  ;; Slow, generic path.
  (apply (type-of a)
         (append (loop for channel in (channels a)
                       collect (+ (* (funcall channel a) (- 1 x))
                                  (* (funcall channel b) x)))
                 (list (+ (* (a a) (- 1 x)) (* (a b) x))))))

(defun gradient (x stops)
  (if (<= x 0)
      (cdr (first stops))
      (loop for prev = (first stops) then next
            for next in (rest stops)
            do (destructuring-bind (px . pc) prev
                 (destructuring-bind (nx . nc) next
                   (when (<= px x nx)
                     (return (lerp (/ (- x px) (- nx px)) pc nc)))))
            finally (return (cdr prev)))))

(defmacro define-cwise-op (name op)
  `(progn
     (defmethod ,name ((a rgb) (b rgb))
       (rgb (,op (r a) (r b))
            (,op (g a) (g b))
            (,op (b a) (b b))
            (,op (a a) (a b))))))

(define-cwise-op c* *)
(define-cwise-op c+ +)
(define-cwise-op c- -)
(define-cwise-op c/ /)
