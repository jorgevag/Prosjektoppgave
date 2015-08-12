// Wrapper for the iargc routine if not natively provided
extern int _gfortran_iargc(void);
int iargc_()
{
return _gfortran_iargc();
}
