
#include <errno.h>
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

CAMLprim value caml_imandrakit_setrlimit(value _resource, value _cur, value _max) {
  CAMLparam3(_resource, _cur, _max);

#if !defined(_WIN32) && !defined(_WIN64)
  int resource = resources[Nativeint_val(_resource)];
  long unsigned int cur = Nativeint_val(_cur);
  long unsigned int max = Nativeint_val(_max);
  bool is_ok = true;

  struct rlimit old_limits;
  if (getrlimit(resource, &old_limits) != 0)
    CAMLreturn(false);

  long unsigned int new_max = max < old_limits.rlim_max ? max : old_limits.rlim_max;
  long unsigned int new_cur = cur < old_limits.rlim_cur ? cur : old_limits.rlim_cur;

  if (new_cur > new_max)
    new_cur = new_max;

  const struct rlimit new_limits = {
    .rlim_cur = new_cur,
    .rlim_max = new_max,
  };

  if (old_limits.rlim_cur != new_limits.rlim_cur ||
      old_limits.rlim_max != new_limits.rlim_max) {
    // FILE *f = fopen("/tmp/myfile", "w");
    // fprintf(f, "(%lu) CUR: %lu MAX: %lu\n", sizeof(rlim_t), new_limits.rlim_cur, new_limits.rlim_max);
    // fclose(f);
    is_ok = setrlimit(resource, &new_limits) == 0;
  }
#else
  bool isok = true;
#endif
  CAMLreturn(Val_bool(is_ok));
}

CAMLprim value caml_imandrakit_getrlimit(value _resource) {
  CAMLparam1(_resource);
  CAMLlocal2(r, sr);

  int resource = resources[Nativeint_val(_resource)];

  struct rlimit limits;
  if (getrlimit(resource, &limits) != 0) {
    r = caml_alloc(1, 1);
    Store_field(r, 0, errno);
  } else {
    r = caml_alloc(1, 0);
    sr = caml_alloc_tuple(2);
    Store_field(sr, 0, caml_copy_nativeint(limits.rlim_cur));
    Store_field(sr, 1, caml_copy_nativeint(limits.rlim_max));
    Store_field(r, 0, sr);
  }

  CAMLreturn(r);
}
