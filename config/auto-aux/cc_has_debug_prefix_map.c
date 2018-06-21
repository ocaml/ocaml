/* Determine whether the C compiler supports -fdebug-prefix-map */

/* This file is to be preprocessed and its output examined. */
/* It is not C source code to be executed.  */
/* This helps with cross-compilation. */

#if defined(__INTEL_COMPILER)
false
#elif defined(__clang_major__) && defined(__clang_minor__)
#if __clang_major__ >= 7
true
#else
false
#endif
#elif defined(__GNUC__) && defined(__GNUC_MINOR__)
#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 3)
true
#else
false
#endif
#elif defined(__xlc__) && (__xlC__)
false
#else
false
#endif
