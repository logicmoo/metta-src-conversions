;                                           (convert-to-metta-file  cobweb_3 $_442766 cobweb/cobweb_3.pl cobweb/cobweb_3.metta)

  (= (features  ((nominal bodycover) (nominal heartchamber) (nominal bodytemp) (nominal fertilization)))  True)
; /********************************************************************/
; /* cobweb_3.pro Last modification: Sat 29 May 1992 15:37:04 */
; /********************************************************************/
; /********************************************************************/
; /* COBWEB example data-set: Animal descriptions */
; /* From D. Fisher: 'Knowledge Acquisition Via Incremental */
; /* Conceptual Clustering', */
; /* Machine Learning 2, 1991, page 142, Table 1 */
; /********************************************************************/
; ; Description of the features (type and name)

;
; Description of the cases
;
; (first element: case-id (will not be used in clustering),
;
; rest: feature values in accordance to the description of features above)


  (= (case  (reptile cornified_skin imperfect_four unregulated internal))  True)
  (= (case  (bird feathers four regulated internal))  True)
  (= (case  (amphipian moist_skin three unregulated external))  True)
  (= (case  (fish scales two unregulated external))  True)
  (= (case  (mammal hair four regulated internal))  True)


