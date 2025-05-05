cd ..

doxygen make\doxygen.cfg > doxygen.log

type .\warnings.log

pause

.\docs\html\index.html
