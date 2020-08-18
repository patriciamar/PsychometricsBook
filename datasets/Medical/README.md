# Medical

The `Medical` datasets consist of the responses of 2,392 subjects (750 males, 1,633 females and
9 subjects without gender specification) to admission test to a medical school. It contains 100 items.

*** 
## Datasets and their variables

  * `dataMedicaltest` - multiple-choice answers to the admission test in Biology. Possible answers were A, B, C, and D, while more anwers can be correct.
  * `dataMedicalkey` - vector of factors representing correct answers of the `dataMedicaltest` dataset.
  * `dataMedicalgraded` - ordinaly scored items of the `dataMedicaltest` dataset. Each item was graded with 0 to 4 points. Maximum of 4 points were set if all correct answers and none of incorrect answers were selected.
  * `dataMedical` - dichotomously scored items of the `dataMedicaltest` dataset.

***
## References
Martinková P., Štěpánek L., Drabinová A., Houdek J., Vejražka M., & Štuka Č. Semi-real-time analyses of item characteristics for medical school admission tests. Proceedings of the 2017 Federated Conference on Computer Science and Information Systems, M. Ganzha, L. Maciaszek, M. Paprzycki (eds). ACSIS, Vol. 11, pages 189–194, 2017. [doi: 10.15439/2017F380](http://dx.doi.org/10.15439/2017F380).
