;;; msgpack --- Msgpack implementation in Emacs -*- lexical-binding: t -*-

(require 'bindat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Encoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun msgpack--make-byte-array (seqs)
  (let ((res (make-string (reduce #'+ (mapcar #'length seqs)) 0 nil)))
    (loop with i = 0
          for seq in seqs
          do (loop for v across seq
                   when (or (< v 0)
                            (> v 255))
                   do (error "value out of range: %S" v)
                   do (setf (aref res i) v)
                   do (incf i)))
    res))

(defun msgpack--encode-nil ()
  (string #xc0))

(defun msgpack--encode-bool (value)
  (if value (string #xc3) (string #xc2)))

(defun msgpack--pack-short-int (value)
  (bindat-pack `((h byte)) `((h . ,value))))

(defun msgpack--pack-int (header type value)
  (bindat-pack `((h byte) (v ,type)) `((h . ,header) (v . ,value))))

(defun msgpack--pack-u64 (header value)
  (bindat-pack '((h byte) (high u32) (low u32))
               `((h . ,header)
                 (high . ,(ash (logand value #xffffffff00000000) -32))
                 (low . ,(logand value #xffffffff)))))

(defun msgpack--encode-int (value)
  (assert (and (typep value 'integer)))
  (if (minusp value)
      (cond ((> value (- #x80))
             (msgpack--pack-int #xd0 'u8 value))
            ((> value (- #x8000))
             (msgpack--pack-int #xd1 'u16 value))
            ((> value (- #x80000000))
             (msgpack--pack-int #xd2 'u32 value))
            ((> value (- #x8000000000000000))
             (msgpack--pack-u64 #xd3 value)))
    (cond ((<= value #x7f)
           (msgpack--pack-short-int value))
          ((<= value #xff)
           (msgpack--pack-int #xcc 'u8 value))
          ((<= value #xffff)
           (msgpack--pack-int #xcd 'u16 value))
          ((<= value #xffffffff)
           (msgpack--pack-int #xce 'u32 value))
          ((<= value #xffffffffffffffff)
           (msgpack--pack-u64 #xcf value))
          (t
           (error "value out of range: %S" value)))))

(defun msgpack--encode-float (value)
  (error "floating point not supported"))

(defun msgpack--pack-short-string (value)
  (let* ((utf8 (encode-coding-string value 'utf-8))
         (length (length utf8))
         (header (logior #xa0 length)))
    (bindat-pack `((h byte) (v str ,length)) `((h . ,header) (v . ,utf8)))))

(defun msgpack--pack-string (header type value)
  (let ((length (length value)))
    (bindat-pack `((h byte) (l ,type) (v str ,length)) `((h . ,header) (l . ,length) (v . ,value)))))

(defun msgpack--encode-string (value)
  (let* ((utf8 (encode-coding-string value 'utf-8))
         (length (length utf8)))
    (cond ((<= length 31)
           (msgpack--pack-short-string utf8))
          ((<= length #xff)
           (msgpack--pack-string #xd9 'u8 utf8))
          ((<= length #xffff)
           (msgpack--pack-string #xda 'u16 utf8))
          ((<= length #xffffffff)
           (msgpack--pack-string #xdb 'u32 utf8))
          (t
           (error "string too long. length: %S" length)))))

(defun msgpack--pack-short-array (value)
  (let* ((length (length value))
         (header (logior #x90 length))
         (objs (msgpack--make-byte-array (mapcar #'msgpack-encode-value value))))
    (bindat-pack `((h byte)
                   (v str ,(length objs)))
                 `((h . ,header)
                   (v . ,objs)))))

(defun msgpack--pack-array (header type value)
  (let ((length (length value))
        (objs (msgpack--make-byte-array (mapcar #'msgpack-encode-value value))))
    (bindat-pack `((h byte) (l ,type) (v str ,(length objs)))
                 `((h . ,header)
                   (l . ,length)
                   (v . ,objs)))))

(defun msgpack--encode-array (value)
  (let ((length (length value)))
    (cond ((<= length 15)
           (msgpack--pack-short-array value))
          ((<= length #xffff)
           (msgpack--pack-array #xdc 'u16 value))
          ((<= length #xffffffff)
           (msgpack--pack-array #xdd 'u32 value))
          (t
           (error "vector too long. length: %S" length)))))

(defun msgpack--make-byte-array-for-map (map)
  (msgpack--make-byte-array (loop for (k . v) in map
                                  append (list (msgpack-encode-value k)
                                               (msgpack-encode-value v)))))

(defun msgpack--pack-short-map (map)
  (let* ((length (length map))
         (header (logior #x80 length))
         (objs (msgpack--make-byte-array-for-map map)))
    (bindat-pack `((h byte) (v str ,(length objs))) `((h . ,header) (v . ,objs)))))

(defun msgpack--pack-map (header type map)
  (let ((length (length map))
        (objs (msgpack--make-byte-array-for-map map)))
    (bindat-pack `((h byte) (l ,type) (v str ,(length objs)))
                 `((h . ,header)
                   (l . ,length)
                   (v . ,objs)))))

(defun msgpack--encode-map (map)
  (let ((length (length map)))
    (cond ((<= length 15)
           (msgpack--pack-short-map map))
          ((<= length #xffff)
           (msgpack--pack-map #xde 'u16 map))
          ((<= length #xffffffff)
           (msgpack--pack-map #xdf 'u32 map))
          (t
           (error "map too long. length: %S" length)))))

(defun msgpack--encode-custom (value)
  (destructuring-bind (tag &rest content)
      value
    (ecase tag
      (:map (msgpack--encode-map content)))))

(defun msgpack-encode-value (value)
  (if (symbolp value)
      (ecase value
        ((nil) (msgpack--encode-nil))
        (:true (msgpack--encode-bool t))
        (:false (msgpack--encode-bool nil)))
    (etypecase value
      (null (msgpack--encode-nil))
      (integer (msgpack--encode-int value))
      (float (msgpack--encode-float value))
      (string (msgpack--encode-string value))
      (vector (msgpack--encode-array value))
      (list (msgpack--encode-custom value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Decoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun msgpack--decode-nil (offset)
  (cons nil (1+ offset)))

(defun msgpack--decode-false (offset)
  (cons :false (1+ offset)))

(defun msgpack--decode-true (offset)
  (cons :true (1+ offset)))

(defun msgpack--decode-bin (value offset int-length)
  (let* ((array-length (msgpack--decode-uint-block value (1+ offset) int-length))
         (array-offset (+ 1 offset int-length))
         (result (make-string array-length 0 nil)))
    (loop for i from 0 below array-length
          for array-pos from array-offset
          do (setf (aref result i) (aref value array-pos)))
    (cons result (+ array-offset array-length))))

(defun msgpack--decode-short-int (value offset)
  (cons (logand #x7f (aref value offset))
        (1+ offset)))

(defun msgpack--u->int (value bits)
  (if (zerop (logand (ash 1 (1- bits)) value))
      value
    (1- (- (logxor (1- (expt 2 bits)) value)))))

(defun msgpack--decode-short-neg-int (value offset)
  (cons (msgpack--u->int (logand #x1f value) 5)
        (1+ offset)))

(defun msgpack--decode-uint-block (value offset length)
  (loop with result = 0
        for i from offset below (+ offset length)
        for ch = (aref value i)
        do (setq result (logior (ash result 8) ch))
        finally (return result)))

(defun msgpack--decode-uint (value offset length)
  (cons (msgpack--decode-uint-block value (1+ offset) length) (+ 1 offset length)))

(defun msgpack--decode-int (value offset length)
  (loop with result = 0
        for i from (1+ offset) below (+ 1 offset length)
        for ch = (aref value i)
        do (setq result (logior (ash result 8) ch))
        finally (return (cons (msgpack--u->int result (* 8 length)) i))))

(defun msgpack--decode-short-map (value offset)
  (let ((length (logand #x0f (aref value offset)))
        (i (1+ offset)))
    (cons (cons :map
                (loop repeat length
                      for (key . key-offset) = (msgpack--decode-value-internal value i)
                      for (v . new-offset) = (msgpack--decode-value-internal value key-offset)
                      collect (cons key v)
                      do (setq i new-offset)))
          i)))

(defun msgpack--decode-string-utf-8 (value offset length)
  (let ((result (make-string length 0 nil)))
    (loop for i from offset below (+ length offset)
          for string-pos from 0
          do (setf (aref result string-pos) (aref value i)))
    (decode-coding-string result 'utf-8)))

(defun msgpack--decode-short-string (value offset)
  (let* ((length (logand #x1f (aref value offset))))
    (cons (msgpack--decode-string-utf-8 value (1+ offset) length) (+ 1 offset length))))

(defun msgpack--decode-array-values (value offset length)
  (let ((i offset))
    (cons (coerce (loop repeat length
                        for (v . new-offset) = (msgpack--decode-value-internal value i)
                        collect v
                        do (setq i new-offset))
                  'vector)
          i)))

(defun msgpack--decode-short-array (value offset)
  (let ((length (logand #x0f (aref value offset))))
    (msgpack--decode-array-values value (1+ offset) length)))

(defun msgpack--decode-array (value offset int-length)
  (let ((length (msgpack--decode-uint-block value (1+ offset) int-length)))
    (msgpack--decode-array-values value (+ 1 offset int-length) length)))

(defun msgpack--decode-string (value offset int-length)
  (let* ((string-length (msgpack--decode-uint-block value (1+ offset) int-length))
         (string-pos (+ 1 offset int-length)))
    (cons (msgpack--decode-string-utf-8 value string-pos string-length) (+ string-pos string-length))))

(defun msgpack--decode-value-internal (value offset)
  (let ((v (aref value offset)))
    (cond ((zerop (logand #x80 v))
           (msgpack--decode-short-int value offset))
          ((eql (logand #xe0 v) #xe)
           (msgpack--decode-short-neg-int value offset))
          ((eql (logand #xf0 v) #x80)
           (msgpack--decode-short-map value offset))
          ((eql (logand #xf0 v) #x90)
           (msgpack--decode-short-array value offset))
          ((eql (logand #xe0 v) #xa0)
           (msgpack--decode-short-string value offset))
          ((eql v #xc0)
           (msgpack--decode-nil offset))
          ((eql v #xc2)
           (msgpack--decode-false offset))
          ((eql v #xc3)
           (msgpack--decode-true offset))
          ((eql v #xc4)
           (msgpack--decode-bin value offset 1))
          ((eql v #xc5)
           (msgpack--decode-bin value offset 2))
          ((eql v #xc6)
           (msgpack--decode-bin value offset 4))
          ((eql v #xcc)
           (msgpack--decode-uint value offset 1))
          ((eql v #xcd)
           (msgpack--decode-uint value offset 2))
          ((eql v #xce)
           (msgpack--decode-uint value offset 4))
          ((eql v #xcf)
           (msgpack--decode-uint value offset 8))
          ((eql v #xd0)
           (msgpack--decode-int value offset 1))
          ((eql v #xd1)
           (msgpack--decode-int value offset 2))
          ((eql v #xd2)
           (msgpack--decode-int value offset 4))
          ((eql v #xd3)
           (msgpack--decode-int value offset 8))
          ((eql v #xdc)
           (msgpack--decode-array value offset 2))
          ((eql v #xdd)
           (msgpack--decode-array value offset 4))
          ((eql v #xd9)
           (msgpack--decode-string value offset 1))
          ((eql v #xda)
           (msgpack--decode-string value offset 2))
          ((eql v #xdb)
           (msgpack--decode-string value offset 4))
          (t
           (error "unable to decode values of type: 0x%02x" v)))))

(defun msgpack-decode-value (value)
  (car (msgpack--decode-value-internal value 0)))

(provide 'msgpack)
