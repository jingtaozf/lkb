(in-package :mt)

(defstruct (mrs)
  top index eps hcons)

(defstruct (ep)
  label pred roles)

(defstruct (role)
  feature value)

(defstruct (variable)
  type id properties)

(defstruct (property)
  feature value)

(defstruct (hcons)
  relation harg larg)
