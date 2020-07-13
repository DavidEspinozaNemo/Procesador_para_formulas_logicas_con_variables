fun simp prop = 
    aso(com(de_morgan(dis(doble_neg(idempotencia(neutro(prop)))))))
;