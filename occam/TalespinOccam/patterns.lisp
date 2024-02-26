(defparameter *all-cps* nil)

(def-cp
  (ACT TO (*VAR* to))
  (STATE ACTOR (*VAR* to))
  (object)(to))

(def-cp
  (ACT FROM (*VAR* from))
  (STATE ACTOR (*VAR* from)))

(def-cp
  (ACT OBJECT (PP COMPONENT-OF (*VAR* OBJECT)))
  (STATE ACTOR (*VAR* OBJECT))
  (mode) (to)(from) (object component-of))

(def-cp
  (ACT OBJECT (*VAR* OBJECT))
  (STATE ACTOR (*VAR* OBJECT))
  (mode) (to) (object)(from))

;(ACT OBJECT (PP COMPONENT-OF (*VAR* V51036))
;     FROM (*VAR* V51036))
;(STATE ACTOR (*VAR* V51036))

;(ACT OBJECT (PP COMPONENT-OF (*VAR* V51313))
;     TO (*VAR* V51313))
;(STATE ACTOR (*VAR* V51313))


;;==============================================
(def-cp
  (ACT object (*VAR* A))
  (RELATION ACTOR (*VAR* A))
  (to)
  )

(def-cp
  (ACT to (*VAR* A))
  (RELATION ACTOR (*VAR* A))
  )

(def-cp
  (ACT OBJECT (*VAR* O)
       FROM (*VAR* F))
  (RELATION ACTOR (*VAR* F)
            VAL (*VAR* O)))

(def-cp
  (ACT OBJECT (*VAR* O)
       FROM (*VAR* F))
  (RELATION ACTOR (*VAR* O)
            VAL (*VAR* F))
  (to))

(def-cp
  (ACT ACTOR (*VAR* A)
       OBJECT (*VAR* O))
  (RELATION ACTOR (*VAR* O)
            VAL (*VAR* A)))

(def-cp
  (ACT OBJECT (*VAR* O)
       TO (*VAR* T))
  (RELATION ACTOR (*VAR* O)
            VAL (*VAR* T))
  (to)
  )


;(ACT ACTOR (*VAR* A)
;     OBJECT (*VAR* A)
;     FROM (*VAR* F))
;(RELATION ACTOR (*VAR* A)
;          VAL (*VAR* F))

;(ACT ACTOR (*VAR* O)
;     OBJECT (*VAR* O)
;    TO (*VAR* T))
;(RELATION ACTOR (*VAR* O)
;          VAL (*VAR* T))


;(ACT ACTOR (*VAR* f)
;     OBJECT (*VAR* o)
;     TO (*VAR* f)
;     FROM (*VAR* f))
;(RELATION ACTOR (*VAR* o)
;          VAL (*VAR* f))


;(ACT ACTOR (*VAR* f)
;     OBJECT (*VAR* o)
;     FROM (*VAR* f))
;(RELATION ACTOR (*VAR* o)
;          VAL (*VAR* f))
