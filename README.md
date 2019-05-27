https://stackoverflow.com/questions/56322038/why-does-performance-drop-when-a-function-is-moved-to-another-module

# Building and running
```
$ ./scripts/build-exec.sh
...
Main.test
(11000000,2010)
(11000000,239)
(11000000,240)
(11000000,242)
(11000000,237)

SomeModule.test
(11000000,6376)
(11000000,4851)
(11000000,5455)
(11000000,5096)
(11000000,5206)
```
