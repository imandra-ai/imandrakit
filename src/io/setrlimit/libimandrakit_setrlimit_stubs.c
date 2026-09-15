
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#if !defined(_WIN32) && !defined(_WIN64)
#include <sys/resource.h>
#include <unistd.h>
#endif

#if defined(__APPLE__)
#include <mach/mach.h>
#endif

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#if !defined(_WIN32) && !defined(_WIN64)
static const int resources[7] = {RLIMIT_CORE,  RLIMIT_CPU,    RLIMIT_DATA,
                                 RLIMIT_FSIZE, RLIMIT_NOFILE, RLIMIT_STACK,
                                 RLIMIT_AS};

/* The OCaml side uses -1 as a portable "unlimited" sentinel. The platform's
   RLIM_INFINITY is not always the all-ones bit pattern -- on Linux it is
   (~0UL), but on Darwin it is INT64_MAX -- so translate in both directions
   instead of letting -1 sign-extend into an out-of-range rlim_t. */
static rlim_t rlim_of_ocaml(intnat v) {
  return v == -1 ? RLIM_INFINITY : (rlim_t)v;
}

static intnat ocaml_of_rlim(rlim_t v) {
  return v == RLIM_INFINITY ? -1 : (intnat)v;
}
#endif

/* Returns 0 on success, a positive errno if setrlimit(2) failed, or a negative
   errno if the preliminary getrlimit(2) failed. */
CAMLprim value caml_imandrakit_setrlimit(value _resource, value _cur,
                                         value _max) {
  CAMLparam3(_resource, _cur, _max);

#if !defined(_WIN32) && !defined(_WIN64)
  int resource = resources[Nativeint_val(_resource)];
  rlim_t cur = rlim_of_ocaml(Nativeint_val(_cur));
  rlim_t max = rlim_of_ocaml(Nativeint_val(_max));

  struct rlimit old_limits;
  if (getrlimit(resource, &old_limits) != 0)
    CAMLreturn(Val_long(-errno));

  /* a limit is only ever lowered, never raised */
  rlim_t new_max = max < old_limits.rlim_max ? max : old_limits.rlim_max;
  rlim_t new_cur = cur < old_limits.rlim_cur ? cur : old_limits.rlim_cur;

  if (new_cur > new_max)
    new_cur = new_max;

  const struct rlimit new_limits = {
    .rlim_cur = new_cur,
    .rlim_max = new_max,
  };

  if (old_limits.rlim_cur != new_limits.rlim_cur ||
      old_limits.rlim_max != new_limits.rlim_max) {
    if (setrlimit(resource, &new_limits) != 0)
      CAMLreturn(Val_long(errno));
  }
#endif

  CAMLreturn(Val_long(0));
}

CAMLprim value caml_imandrakit_getrlimit(value _resource) {
  CAMLparam1(_resource);
  CAMLlocal2(r, sr);

#if !defined(_WIN32) && !defined(_WIN64)
  int resource = resources[Nativeint_val(_resource)];

  struct rlimit limits;
  if (getrlimit(resource, &limits) != 0) {
    r = caml_alloc(1, 1);
    Store_field(r, 0, caml_copy_nativeint(errno));
  } else {
    sr = caml_alloc_tuple(2);
    Store_field(sr, 0, caml_copy_nativeint(ocaml_of_rlim(limits.rlim_cur)));
    Store_field(sr, 1, caml_copy_nativeint(ocaml_of_rlim(limits.rlim_max)));
    r = caml_alloc(1, 0);
    Store_field(r, 0, sr);
  }
#else
  (void)_resource;
  r = caml_alloc(1, 1);
  Store_field(r, 0, caml_copy_nativeint(ENOSYS));
#endif

  CAMLreturn(r);
}

/* Address space currently reserved by this process, in bytes, or -1 if it
   cannot be determined. This is what RLIMIT_AS is measured against, and it
   includes reservations that hold no memory at all. */
CAMLprim value caml_imandrakit_address_space(value _unit) {
  CAMLparam1(_unit);
  intnat res = -1;

#if defined(__APPLE__)
  struct task_basic_info_64 info;
  mach_msg_type_number_t count = TASK_BASIC_INFO_64_COUNT;
  if (task_info(mach_task_self(), TASK_BASIC_INFO_64, (task_info_t)&info,
                &count) == KERN_SUCCESS)
    res = (intnat)info.virtual_size;
#elif defined(__linux__)
  FILE *f = fopen("/proc/self/statm", "r");
  if (f != NULL) {
    unsigned long pages = 0;
    if (fscanf(f, "%lu", &pages) == 1)
      res = (intnat)(pages * (unsigned long)sysconf(_SC_PAGESIZE));
    fclose(f);
  }
#endif

  CAMLreturn(caml_copy_nativeint(res));
}

CAMLprim value caml_imandrakit_rlimit_strerror(value _errno) {
  CAMLparam1(_errno);
  CAMLreturn(caml_copy_string(strerror((int)Nativeint_val(_errno))));
}
