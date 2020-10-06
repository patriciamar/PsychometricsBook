# HCI

The set of HCI datasets (McFarland et al., 2017) contains real datasets originally available in the `ShinyItemAnalysis` package (Martinková & Drabinová, 2018). The `HCItest` dataset consists of the responses of 651 students (405 males, 246 females) to Homeostasis Concept Inventory (HCI) multiple-choice test . It contains 20 items, vector of gender variable (`"0"` for males, `"1"` for females) and identificator whether students plan to major. The `HCI` dataset consists of the dichotomously scored responses, where correct answer is given in the `HCIkey` dataset. The `HCI` dataset also contains vector of gender variable and identificator whether students plan to major.

*** 
## Datasets and their variables
 * `HCI` - dichotomously scored HCI test
    + `Item1` - `Item20` = dichotomously scored items of the HCI test.
    + `gender` = gender of students, `"0"` for males, `"1"` for females.
    + `major` = criterion variable describing whether student plans to major in the life sciences.
 * `HCItest` - multiple-choice HCI test
    + `Item1` - `Item20` = multiple choice items of the HCI test.
    + `gender` = gender of students, `"0"` for males, `"1"` for females.
    + `major` = criterion variable describing whether student plans to major in the life sciences.
 * `HCIkey` - vector of factors representing correct answers of the HCItest dataset
 * `HCI_test_retest`
    + `ID` = identification of the student.
    + `test` = identification of the test, `"1"` for test, `"2"` for retest.
    + `Item1` - `Item20` = dichotomously scored items of the HCI test.
    + `total` = total test score of test/retest.

***
## References
Martinková, P., & Drabinová, A. (2018). ShinyItemAnalysis for teaching psychometrics and to enforce routine analysis of educational tests. The R Journal, 10(2), 503-515, [doi: 10.32614/RJ-2018-074](https://doi.org/10.32614/RJ-2018-074).
  
McFarland, J. L., Price, R. M., Wenderoth, M. P., Martinková, P., Cliff, W., Michael, J., ... & Wright, A. (2017). Development and validation of the homeostasis concept inventory. CBE-Life Sciences Education, 16(2), ar35, [doi: 10.1187/cbe.16-10-0305](https://doi.org/10.1187/cbe.16-10-0305).
