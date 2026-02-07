
 This directory contains the  tests suite for fpGUI.
 Over time the test suite will be extended to cover many parts of CoreLib
 and the GUI components (where possible).

  Reguirements
  ------------

    * FPCUnit testing framework - included with FPC
    * [disabled by default] FPTest testing framework (http://github.com/graemeg/fptest)


  Running the tests
  -----------------

    From the root of the repo, run:

    > pasbuild test


  Known errors
  ------------

    * The "EAccessViolation: Access violation" when the test runner terminates
      is known and expected. It's simply not shutting down the fpgApplication
      correctly.



