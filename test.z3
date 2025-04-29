(declare-fun |0| () Float32)
(assert (not
 (=
  (ubv_to_int
   (bvand ((_ fp.to_sbv 32) RNA (fp.abs |0|)) ((_ int_to_bv 32) 2147483648)))
  0)))
(check-sat)
