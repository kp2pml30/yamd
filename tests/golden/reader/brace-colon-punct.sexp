(splice
 (element
  p
  ()
  ("A brace body plus a trailing colon at line start is punctuation, not a"
   "\n"
   "body, when nothing is indented below:"))
 (element
  p
  ()
  ((element em () ((splice "note")))
   ":"
   "\n"
   "the colon stays literal and this joins the paragraph.")))
