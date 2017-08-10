AC_INIT(lfl)
AC_CONFIG_SRCDIR([src])

# find R home and set CC/CFLAGS
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

RBIN="${R_HOME}/bin/R"
CXX="`"${RBIN}" CMD config CXX11` `"${RBIN}" CMD config CXX11STD`"
CXXFLAGS=`"${RBIN}" CMD config CXX11FLAGS`
LIBS="${PKG_LIBS}"

AC_LANG(C++)
AC_PROG_CXX
AC_OPENMP

# since some systems have broken OMP libraries
# we also check that the actual package will work
ac_pkg_openmp=no
if test -n "${OPENMP_CXXFLAGS}"; then
  AC_MSG_CHECKING([whether OpenMP will work in a package])
  AC_LANG_CONFTEST(
  [AC_LANG_PROGRAM([[#include <omp.h>]], [[ return omp_get_num_threads (); ]])])
  PKG_CXXFLAGS="${OPENMP_CXXFLAGS}" PKG_LIBS="${OPENMP_CXXFLAGS}" "$RBIN" CMD SHLIB conftest.c 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && "$RBIN" --vanilla -q -e "dyn.load(paste('conftest',.Platform\$dynlib.ext,sep=''))" 1>&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD && ac_pkg_openmp=yes
  AC_MSG_RESULT([${ac_pkg_openmp}])
fi

# if ${ac_pkg_openmp} = "yes" then we have OMP, otherwise it will be "no"
if test "${ac_pkg_openmp}" = no; then
  OPENMP_CXXFLAGS=''
  # you could put AC_MSG_ERROR here is OpenMP is required
fi

AC_SUBST(OPENMP_CXXFLAGS)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
