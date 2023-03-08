# lmeInfo

<details>

* Version: 0.3.0
* GitHub: https://github.com/jepusto/lmeInfo
* Source code: https://github.com/cran/lmeInfo
* Date/Publication: 2022-10-24 16:52:36 UTC
* Number of recursive dependencies: 91

Run `revdep_details(, "lmeInfo")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
          "treatment"), class = "factor")), class = "data.frame", row.names = c(NA, 
      570L), terms = ~session + school + case + outcome + treatment))`: missing values in object
      Backtrace:
          ▆
       1. ├─nlme::lme(...) at test-three-level-models.R:89:0
       2. └─nlme::lme.formula(...)
       3.   ├─base::do.call(model.frame, mfArgs)
       4.   ├─stats (local) `<fn>`(...)
       5.   └─stats::model.frame.default(...)
       6.     ├─stats (local) `<fn>`(`<df[,5]>`)
       7.     └─stats:::na.fail.default(`<df[,5]>`)
      
      [ FAIL 3 | WARN 0 | SKIP 13 | PASS 982 ]
      Error: Test failures
      Execution halted
    ```

