;;; -*- Mode: LISP-*-

(def-schema goal (goal)(goal) nil)
(def-schema act (act)(act) nil)
(def-schema plan (plan)(plan) nil)
(def-schema state (state)(state) nil)

(def-sub-schema sell-schema act (ACT type (SELL)) (ACT type (SELL)) nil)
(def-sub-schema produce-schema act (ACT type (produce)) (ACT type (produce)) nil)
(def-sub-schema atrans-schema act (ACT type (ATRANS)) (ACT type (ATRANS)) nil)
(def-sub-schema mtrans-schema act (ACT type (MTRANS)) (ACT type (MTRANS)) nil)
(def-sub-schema propel-schema act (act type (propel)) (act type (propel))  nil)
