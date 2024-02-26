;                                           (convert-to-metta-file  cobweb_1 $_317642 cobweb/cobweb_1.pl cobweb/cobweb_1.metta)

  (= (features  ((numeric minPrice) (nominal tv) (nominal bar)))  True)
; /********************************************************************/
; /* cobweb_1.pro Last modification: Sun 2 Feb 1992 12:47:23 */
; /********************************************************************/
; /********************************************************************/
; /* Data set describing a set of fictitious hotels (price per room, */
; /* furnishings). The learning result should be a grouping into cheap*/
; /* and luxury hotels. */
; /********************************************************************/
; ; Description of the features as a list of (type and name)
; ; minimum price per room
; ; room with TV?                    ;
; room with bar?

;
; Description of the cases
;
; (first element: case-id (will not be used in clustering),
;
; rest: feature values in accordance to the description of features above)


  (= (case  (sheraton 250.0 y y))  True)
  (= (case  (ritz 223.0 y y))  True)
  (= (case  (kempinski 224.0 y y))  True)
  (= (case  (sonja 40.0 n y))  True)
  (= (case  (ostermann 35 n n))  True)
  (= (case  (zur_gruenen_wiese 50 y n))  True)

