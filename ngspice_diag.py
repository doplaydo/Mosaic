from cffi import FFI
from pathlib import Path
import importlib

ffi = FFI()

print("1. Loading api.h...")
inspice_mod = importlib.import_module("InSpice")
api_path = Path(inspice_mod.__file__).parent / "Spice" / "NgSpice" / "api.h"
with open(api_path) as fh:
    ffi.cdef(fh.read())
print("   OK")

print("2. dlopen libngspice...")
lib = ffi.dlopen("/usr/lib/libngspice.so")
print(f"   OK: {lib}")

print("3. Creating callbacks...")
send_char = ffi.callback('int (char *, int, void *)', lambda m, i, u: 0)
send_stat = ffi.callback('int (char *, int, void *)', lambda m, i, u: 0)
exit_cb = ffi.callback('int (int, bool, bool, int, void *)', lambda a, b, c, d, e: 0)
send_init = ffi.callback('int (pvecinfoall, int, void *)', lambda a, b, c: 0)
bg_running = ffi.callback('int (bool, int, void *)', lambda a, b, c: 0)
get_vsrc = ffi.callback('int (double *, double, char *, int, void *)', lambda a, b, c, d, e: 0)
get_isrc = ffi.callback('int (double *, double, char *, int, void *)', lambda a, b, c, d, e: 0)
print("   OK")

self_handle = ffi.new_handle(None)

print("4. Calling ngSpice_Init...")
rc = lib.ngSpice_Init(
    send_char, send_stat, exit_cb,
    ffi.NULL,  # send_data
    send_init, bg_running,
    self_handle
)
print(f"   returned: {rc}")

print("5. Calling ngSpice_Init_Sync...")
ngspice_id = ffi.new('int *', 0)
rc = lib.ngSpice_Init_Sync(get_vsrc, get_isrc, ffi.NULL, ngspice_id, self_handle)
print(f"   returned: {rc}")

print("6. Calling ngSpice_Command('version -f')...")
rc = lib.ngSpice_Command(b"version -f")
print(f"   returned: {rc}")

print("7. Calling ngSpice_Command('set nomoremode')...")
rc = lib.ngSpice_Command(b"set nomoremode")
print(f"   returned: {rc}")

print("All steps completed!")
