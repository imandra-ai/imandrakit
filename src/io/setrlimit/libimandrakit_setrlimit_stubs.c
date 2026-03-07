
#include <stdbool.h>

#if !defined(_WIN32) && !defined(_WIN64)
#include <sys/resource.h>
#endif

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#if !defined(_WIN32) && !defined(_WIN64)
int resources[7] = {RLIMIT_CORE,   RLIMIT_CPU,   RLIMIT_DATA, RLIMIT_FSIZE,
                    RLIMIT_NOFILE, RLIMIT_STACK, RLIMIT_AS};
#endif

CAMLprim value caml_imandrakit_setrlimit(value _res, value _value) {
  CAMLparam2(_res, _value);

#if !defined(_WIN32) && !defined(_WIN64)
  int resource = resources[Int_val(_res)];
  unsigned long limit = Int_val(_value);

  struct rlimit old_limits;
  if (getrlimit(resource, &old_limits) != 0)
    CAMLreturn(false);

  const struct rlimit new_limits = {
      .rlim_cur = limit < old_limits.rlim_max ?  limit : old_limits.rlim_max,
      .rlim_max = old_limits.rlim_max,
  };

  int res = setrlimit(resource, &new_limits);
  bool isok = (res == 0);
#else
  bool isok = true;
#endif

  CAMLreturn(Val_bool(isok));
}
