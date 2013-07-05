(in-package #:terpsiscottean)

(deffigure right "Step right"
    (1
     (1M
      (debug t "~&Going right!"))))
  
(deffigure left "Step left"
    (1
     (1M
      (debug t "~&Going left!"))))

(deffigure 1C-set "1C set"
  (2
   (1M
    (right :face 'in :step 'pas-de-basque)
    (left :face 'in :step 'pas-de-basque))
   (1W
    (right :face 'in :step 'pas-de-basque)
    (left :face 'in :step 'pas-de-basque))))