(splice
 (element
  p
  ()
  ("plain "
   (element code () ((splice " a } b ")))
   " end"
   "\n"
   "ext "
   (element code () ((splice " has }| inside ")))
   " done"
   "\n"
   "raw "
   (element code () ((splice "normal")))
   " too")))
