cd ..

doxygen make\doxygen.cfg > doxygen.log

type .\warnings.log

rem pause

.\docs\index.html
