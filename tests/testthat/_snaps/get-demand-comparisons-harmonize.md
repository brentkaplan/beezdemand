# print.beezdemand_comparison is backend-agnostic and renders tables

    Code
      print(mk_tmb())
    Output
      Demand Parameter Comparisons (tmb backend)
      EMMs computed over: ~gender 
      Contrast type: pairwise
      P-value adjustment method: holm 
      ================================================== 
      
      Q0 (log10-scale contrasts):
            contrast estimate std.error conf.low conf.high p.value
       Female - Male    0.123     0.045    0.035     0.211   0.036

---

    Code
      print(mk_nlme())
    Output
      Demand Parameter Comparisons (nlme backend)
      EMMs computed over: ~gender 
      Contrast type: pairwise
      P-value adjustment method: holm 
      ================================================== 
      
      Q0 (log10-scale contrasts):
            contrast estimate std.error conf.low conf.high p.value
       Female - Male    0.123     0.045     0.03      0.21    0.01

