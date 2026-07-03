(splice
 (element
  p
  ()
  ((element code () ("x = {1, 2}")) (element code () ((splice "y")))))
 (element p () ("custom: "))
 (element
  p
  ()
  ((element
    math
    ((display . "inline"))
    ((element
      mrow
      ()
      ((element msup () ((element mi () ("a")) (element mn () ("2"))))
       (element mo () ("+"))
       (element msub () ((element mi () ("b")) (element mn () ("1"))))))))))
 (element
  p
  ()
  ("Literal survives: " (element strong () ((splice "yes"))) ".")))
