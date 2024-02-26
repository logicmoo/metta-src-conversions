

  !(ensure-loaded dmiles)

  !(multifile (/ file-search-path 2))
  !(dynamic (/ file-search-path 2))

  !(ensure-loaded-if-exists (library prompt))

  !(ensure-loaded (home argument-types))
  !(ensure-loaded (home bu-basics))
  !(ensure-loaded (home clause-heads))
  !(ensure-loaded (home div-utils))
  !(ensure-loaded (home environment))
  !(ensure-loaded (home evaluation))
  !(ensure-loaded (home filter))
  !(ensure-loaded (home flatten))
  !(ensure-loaded (home g1-ops))
  !(ensure-loaded (home g2-ops))
  !(ensure-loaded (home interpreter))
  !(ensure-loaded (home kb))
  !(ensure-loaded (home lgg))
  !(ensure-loaded (home show-utils))
  !(ensure-loaded (home td-basic))
  !(ensure-loaded (home tdref-it))
  !(ensure-loaded (home var-utils))
  !(ensure-loaded (home newpred))
  !(ensure-loaded (home gencon))
  !(ensure-loaded (home gencon-instances/rul.pl))
  !(ensure-loaded (home gencon-instances/constrained-clauses.pl))
  !(ensure-loaded (home gencon-instances/foil.pl))

  !((remove-all-atoms  &self 
      (:  interpreter 
        (depth_bound  $_))) (add-atom  &self (:  interpreter (depth_bound  10))))

